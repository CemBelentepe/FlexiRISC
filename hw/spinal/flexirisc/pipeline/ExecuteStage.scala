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
    val ex_result = in Bits(64 bits)
    val ex_src2 = in Bits(64 bits)

    val mem_control_signals = out(ControlSignals())
    val mem_result = out Bits(64 bits)
    val mem_src2 = out Bits(64 bits)
  }

  val control_signals = Reg(ControlSignals()) init(DecodeStage.default_control())
  val result = Reg(Bits(64 bits)) init(0)
  val src2 = Reg(Bits(64 bits)) init(0)

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

    val pc = in UInt(64 bits)
    val pc_next_seq = in UInt(64 bits)
    val id_src1 = in Bits(64 bits)
    val id_src2 = in Bits(64 bits)
    val immediate = in Bits(32 bits)

    val result = out Bits(64 bits)
    val jump_enable = out Bool()
    val jump_address = out UInt(64 bits)
    val src2 = out Bits(64 bits)

    val stage_valid = out Bool()
  }

  val immediate = io.immediate.asSInt.resize(64 bits)

  val src1 = io.id_src1
  val src2 = io.id_src2
  io.src2 := src2

  val alu_lhs = io.control_signals.sel_add_lhs.mux(
    SEL_LHS.PC -> io.pc,
    SEL_LHS.SRC1 -> src1.asUInt
  )

  val alu_rhs = io.control_signals.sel_add_rhs.mux(
    SEL_RHS.IMM -> immediate.asUInt,
    SEL_RHS.SRC2 -> src2.asUInt
  )

  val adder = new CarrySelectAdder(64, 8)
  adder.io.lhs := alu_lhs
  adder.io.rhs := (alu_rhs.asBits ^ B(64 bits, default -> io.control_signals.is_sub)).asUInt
  adder.io.c_in := io.control_signals.is_sub

  val add_res = adder.io.res

  val comparator = new Area {
    val lhs = src1.asBits
    val rhs = io.control_signals.sel_comp_rhs.mux(
      SEL_RHS.IMM -> immediate.asBits,
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

    val lhs = (io.control_signals.is_w ? src1(31 downto 0).asSInt.resize(64 bits).asBits | src1).asUInt
    val rhs = (io.control_signals.is_w ? src2(31 downto 0).asSInt.resize(64 bits).asBits | src2).asUInt

    val is_neg_lhs = lhs.msb
    val is_neg_rhs = rhs.msb
    val sgn_lhs = is_neg_lhs ? (-lhs.asSInt).asUInt | lhs
    val sgn_rhs = is_neg_rhs ? (-rhs.asSInt).asUInt | rhs

    val reg_rs1_1d = RegNext(io.control_signals.rs1)
    val reg_rs2_1d = RegNext(io.control_signals.rs2)
    val alu_1d = RegNext(io.control_signals.alu_op)

    // TODO IF rs1 and rs2 are same as 1d, and mul was also active 1d ago, do not start
    val rs_changed = (reg_rs1_1d =/= io.control_signals.rs1) | (reg_rs2_1d =/= io.control_signals.rs2)

    val mulArea = new Area {
      val mulUnit = new Multiplier(64)
      val mul_res = Bits(64 bits)
      val is_neg_res = False
      val sgn_res = (is_neg_res ? (-mulUnit.io.result.asSInt).asUInt | mulUnit.io.result).asBits

      mulUnit.io.start := rs_changed | !(alu_1d(1) === False & io.control_signals.alu_op(1) === False)
      mulUnit.io.enabled := True

      switch(io.control_signals.alu_op(1 downto 0)) {
        is(0) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := sgn_rhs
          is_neg_res := is_neg_lhs ^ is_neg_rhs
          mul_res := sgn_res(63 downto 0)
        }
        is(1) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := sgn_rhs
          is_neg_res := is_neg_lhs ^ is_neg_rhs
          mul_res := sgn_res(127 downto 64)
        }
        is(2) {
          mulUnit.io.lhs := sgn_lhs
          mulUnit.io.rhs := rhs
          is_neg_res := is_neg_lhs
          mul_res := sgn_res(127 downto 64)
        }
        is(3) {
          mulUnit.io.lhs := lhs
          mulUnit.io.rhs := rhs
          mul_res := mulUnit.io.result(127 downto 64).asBits
        }
      }

    }

    val divArea = new Area {
      val divUnit = new Divider(64)
      val div_res = Bits(64 bits)
      val reg_is_w_1d = RegNext(io.control_signals.is_w)
      divUnit.io.start := rs_changed | (alu_1d(0) =/= io.control_signals.alu_op(0)) | (reg_is_w_1d =/= io.control_signals.is_w)

      switch(io.control_signals.alu_op(1 downto 0)){
        is(0){
          divUnit.io.lhs := sgn_lhs
          divUnit.io.rhs := sgn_rhs
          div_res := (is_neg_lhs ^ is_neg_rhs) ? (-divUnit.io.div.asSInt).asBits | divUnit.io.div
        }
        is(1) {
          divUnit.io.lhs := lhs
          divUnit.io.rhs := rhs
          div_res := divUnit.io.div
        }
        is(2) {
          divUnit.io.lhs := sgn_lhs
          divUnit.io.rhs := sgn_rhs
          div_res := (is_neg_lhs ^ is_neg_rhs) ? (-divUnit.io.rem.asSInt).asBits | divUnit.io.rem
        }
        is(3) {
          divUnit.io.lhs := lhs
          divUnit.io.rhs := rhs
          div_res := divUnit.io.rem
        }
      }

    }

    val res = is_mul ? mulArea.mul_res | divArea.div_res
    val muldiv_done = is_mul ? mulArea.mulUnit.io.done | divArea.divUnit.io.done
  }

  // TODO if jump is to the next instr, do not flush
  io.jump_enable := (io.control_signals.is_jump | (io.control_signals.is_branch & comparator.res))
  val shamt = (io.control_signals.is_w ? alu_rhs(4 downto 0) | alu_rhs(5 downto 0))
  val i_alu_res = io.control_signals.alu_op.mux(
    0 -> add_res.asBits.resize(64 bits),
    1 -> (alu_lhs.asBits |<< alu_rhs(5 downto 0)).asBits,
    2 -> comparator.lt.asBits(64 bits),
    3 -> comparator.ltu.asBits(64 bits),
    4 -> (alu_lhs ^ alu_rhs).asBits,
    5 -> (io.control_signals.is_arth_shift ? (alu_lhs.asSInt >> shamt).asBits | (alu_lhs |>> shamt).asBits),
    6 -> (alu_lhs | alu_rhs).asBits,
    7 -> (alu_lhs & alu_rhs).asBits
  )

  val alu_res = io.control_signals.is_muldiv ? muldiv.res | i_alu_res
  io.result := io.control_signals.sel_alu_res.mux(
    SEL_RES.ALU -> (io.control_signals.is_w ? alu_res(31 downto 0).asSInt.resize(64 bits).asBits | alu_res),
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

