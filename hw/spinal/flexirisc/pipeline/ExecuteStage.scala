package flexirisc.pipeline

import flexirisc.Config
import flexirisc.arithmetic.{CarrySelectAdder, Divider, Multiplier}
import spinal.core._

import scala.language.postfixOps

case class ExMem() extends Component {
  val io = new Bundle {
    val mem_stall = in Bool()
    val mem_flush = in Bool()

    val ex_control_signals = in(ControlSignals())
    val ex_result = in Bits(32 bits)
    val ex_src2 = in Bits(32 bits)

    val mem_control_signals = out(ControlSignals())
    val mem_result = out Bits(32 bits)
    val mem_src2 = out Bits(32 bits)
  }

  val control_signals = Reg(ControlSignals()) init(DecodeStage.default_control())
  val result = Reg(Bits(32 bits)) init(0)
  val src2 = Reg(Bits(32 bits)) init(0)

  when(io.mem_flush) {
    control_signals := DecodeStage.default_control()
    result := 0
    src2 := 0
  }.elsewhen(!io.mem_stall) {
    control_signals := io.ex_control_signals
    result := io.ex_result
    src2 := io.ex_src2
  }

  io.mem_control_signals := control_signals
  io.mem_result := result
  io.mem_src2 := src2
}

case class ExecuteStage() extends Component {
  val io = new Bundle{
    val control_signals = in(ControlSignals())

    val pc = in UInt(32 bits)
    val pc_next_seq = in UInt(32 bits)
    val id_src1 = in Bits(32 bits)
    val id_src2 = in Bits(32 bits)
    val immediate = in Bits(32 bits)

    val mem_res = in Bits(32 bits)
    val wb_res = in Bits(32 bits)
    val use_mem_src1 = in Bool()
    val use_mem_src2 = in Bool()
    val use_wb_src1 = in Bool()
    val use_wb_src2 = in Bool()

    val result = out Bits(32 bits)
    val jump_enable = out Bool()
    val jump_address = out UInt(32 bits)
    val src2 = out Bits(32 bits)

    val stage_valid = out Bool()
  }

  val src1 = Bits(32 bits)
  src1 := io.id_src1
  when(io.use_mem_src1){
    src1 := io.mem_res
  }.elsewhen(io.use_wb_src1){
    src1 := io.wb_res
  }

  val src2 = Bits(32 bits)
  src2 := io.id_src2
  when(io.use_mem_src2) {
    src2 := io.mem_res
  }.elsewhen(io.use_wb_src2) {
    src2 := io.wb_res
  }
  io.src2 := src2

  val alu_lhs = io.control_signals.sel_add_lhs.mux(
    SEL_LHS.PC -> io.pc,
    SEL_LHS.SRC1 -> src1.asUInt
  )

  val alu_rhs = io.control_signals.sel_add_rhs.mux(
    SEL_RHS.IMM -> io.immediate.asUInt,
    SEL_RHS.SRC2 -> src2.asUInt
  )

  val adder = new CarrySelectAdder(32, 8)
  adder.io.lhs := alu_lhs
  adder.io.rhs := (alu_rhs.asBits ^ B(32 bits, default -> io.control_signals.is_sub)).asUInt
  adder.io.c_in := io.control_signals.is_sub

  val add_res = adder.io.res

  val comparator = new Area {
    val lhs = src1.asBits
    val rhs = io.control_signals.sel_comp_rhs.mux(
      SEL_RHS.IMM -> io.immediate.asBits,
      SEL_RHS.SRC2 -> src2.asBits
    )

    val lt = lhs.asSInt < rhs.asSInt
    val ltu = lhs.asUInt < rhs.asUInt

    val res = io.control_signals.comp_op(0) ^ io.control_signals.comp_op(2 downto 1).mux(
      0 -> (lhs === rhs),
      1 -> False,
      2 -> lt,
      3 -> ltu,
    )
  }

  val muldiv = new Area {
    val is_mul = ~io.control_signals.alu_op(2)

    val is_neg_lhs = src1.msb
    val is_neg_rhs = src2.msb
    val sgn_lhs = is_neg_lhs ? (-src1.asSInt).asUInt | src1.asUInt
    val sgn_rhs = is_neg_rhs ? (-src2.asSInt).asUInt | src2.asUInt

    val reg_rs1_1d = RegNext(io.control_signals.rs1)
    val reg_rs2_1d = RegNext(io.control_signals.rs2)
    val alu_1d = RegNext(io.control_signals.alu_op)

    // TODO IF rs1 and rs2 are same as 1d, and mul was also active 1d ago, do not start
    val rs_changed = (reg_rs1_1d =/= io.control_signals.rs1) | (reg_rs2_1d =/= io.control_signals.rs2)

    val mulArea = new Area {
      val mulUnit = new Multiplier(32)
      val mul_res = Bits(32 bits)
      val is_neg_res = False
      val sgn_res = (is_neg_res ? (-mulUnit.io.result.asSInt).asUInt | mulUnit.io.result).asBits

      mulUnit.io.start := rs_changed | !(alu_1d(1) === False & io.control_signals.alu_op(1) === False)
      mulUnit.io.enabled := True

      switch(io.control_signals.alu_op(1 downto 0)) {
        is(0) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := sgn_rhs
          is_neg_res := is_neg_lhs ^ is_neg_rhs
          mul_res := sgn_res(31 downto 0)
        }
        is(1) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := sgn_rhs
          is_neg_res := is_neg_lhs ^ is_neg_rhs
          mul_res := sgn_res(63 downto 32)
        }
        is(2) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := src2.asUInt
          is_neg_res := is_neg_lhs
          mul_res := sgn_res(63 downto 32)
        }
        is(3) {
          mulUnit.io.lhs := src1.asUInt
          mulUnit.io.rhs := src2.asUInt
          mul_res := mulUnit.io.result(63 downto 32).asBits
        }
      }

    }

    val divArea = new Area {
      val divUnit = new Divider(32)
      val div_res = Bits(32 bits)
      divUnit.io.start := rs_changed | (alu_1d(0) =/= io.control_signals.alu_op(0))

      switch(io.control_signals.alu_op(1 downto 0)){
        is(0){
          divUnit.io.lhs := sgn_lhs
          divUnit.io.rhs := sgn_rhs
          div_res := (is_neg_lhs ^ is_neg_rhs) ? (-divUnit.io.div.asSInt).asBits | divUnit.io.div
        }
        is(1) {
          divUnit.io.lhs := src1.asUInt
          divUnit.io.rhs := src2.asUInt
          div_res := divUnit.io.div
        }
        is(2) {
          divUnit.io.lhs := sgn_lhs
          divUnit.io.rhs := sgn_rhs
          div_res := (is_neg_lhs ^ is_neg_rhs) ? (-divUnit.io.rem.asSInt).asBits | divUnit.io.rem
        }
        is(3) {
          divUnit.io.lhs := src1.asUInt
          divUnit.io.rhs := src2.asUInt
          div_res := divUnit.io.rem
        }
      }

    }

    val res = is_mul ? mulArea.mul_res | divArea.div_res
    val muldiv_done = is_mul ? mulArea.mulUnit.io.done | divArea.divUnit.io.done
  }

  // TODO if jump is to the next instr, do not flush
  io.jump_enable := (io.control_signals.is_jump | (io.control_signals.is_branch & comparator.res))
  val alu_res = io.control_signals.alu_op.mux(
    0 -> add_res.asBits.resize(32 bits),
    1 -> (alu_lhs.asBits |<< alu_rhs(4 downto 0)).asBits,
    2 -> comparator.lt.asBits(32 bits),
    3 -> comparator.ltu.asBits(32 bits),
    4 -> (alu_lhs ^ alu_rhs).asBits,
    5 -> (io.control_signals.is_arth_shift ? (alu_lhs.asSInt >> alu_rhs(4 downto 0)).asBits | (alu_lhs |>> alu_rhs(4 downto 0)).asBits),
    6 -> (alu_lhs | alu_rhs).asBits,
    7 -> (alu_lhs & alu_rhs).asBits
  )

  io.result := io.control_signals.sel_alu_res.mux(
    SEL_RES.ALU -> (io.control_signals.is_muldiv ? muldiv.res | alu_res),
    SEL_RES.PC -> io.pc_next_seq.asBits
  )

  io.stage_valid := io.control_signals.sel_alu_res.mux(
    SEL_RES.ALU -> (io.control_signals.is_muldiv ? muldiv.muldiv_done | True),
    SEL_RES.PC -> True
  )

  io.jump_address := add_res
}

object ExecuteStage extends App {
  Config.spinal.generateVerilog(ExecuteStage())
}

