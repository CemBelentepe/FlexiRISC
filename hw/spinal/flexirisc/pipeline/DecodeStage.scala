package flexirisc.pipeline

import flexirisc.Config
import spinal.core._

import scala.language.postfixOps

object SEL_LHS extends SpinalEnum(binarySequential){
  val SRC1, PC = newElement()
}

object SEL_RHS extends SpinalEnum(binarySequential){
  val IMM, SRC2 = newElement()
}

object SEL_RES extends SpinalEnum(binarySequential){
  val ALU, PC = newElement()
}

case class ControlSignals() extends Bundle {
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val rd = UInt(5 bits)

  val sel_add_lhs = SEL_LHS()
  val sel_add_rhs = SEL_RHS()
  val alu_op = Bits(3 bits)
  val is_arth_shift = Bool()
  val is_sub = Bool()
  val is_muldiv = Bool()
  val sel_comp_rhs = SEL_RHS()
  val comp_op = Bits(3 bits)
  val is_jump = Bool()
  val is_branch = Bool()
  val sel_alu_res = SEL_RES()

  val funct3 = Bits(3 bits)

  val is_store = Bool()
  val is_load = Bool()
}


case class IdEx() extends Component {
  val io = new Bundle {
    val ex_stall = in Bool()
    val ex_flush = in Bool()

    val id2_control_signals = in(ControlSignals())
    val id2_src1 = in Bits(32 bits)
    val id2_src2 = in Bits(32 bits)
    val id2_immediate = in Bits(32 bits)
    val id2_pc = in UInt(32 bits)
    val id2_pc_next_seq = in UInt(32 bits)

    val ex_control_signals = out(ControlSignals())
    val ex_src1 = out Bits (32 bits)
    val ex_src2 = out Bits (32 bits)
    val ex_immediate = out Bits (32 bits)
    val ex_pc = out UInt(32 bits)
    val ex_pc_next_seq = out UInt (32 bits)
  }

  val control_signals = Reg(ControlSignals()) init(DecodeStage.default_control())
  val src1 = Reg(Bits (32 bits)) init(0)
  val src2 = Reg(Bits (32 bits)) init(0)
  val immediate = Reg(Bits (32 bits)) init(0)
  val pc = Reg(UInt (32 bits)) init(0)
  val pc_next_seq = Reg(UInt (32 bits)) init(0)

  when(io.ex_flush){
    control_signals := DecodeStage.default_control()
    src1 := 0
    src2 := 0
    immediate := 0
    pc := 0
    pc_next_seq := 0
  }.elsewhen(!io.ex_stall){
    control_signals := io.id2_control_signals
    src1 := io.id2_src1
    src2 := io.id2_src2
    immediate := io.id2_immediate
    pc := io.id2_pc
    pc_next_seq := io.id2_pc_next_seq
  }

  io.ex_control_signals := control_signals
  io.ex_src1 := src1
  io.ex_src2 := src2
  io.ex_immediate := immediate
  io.ex_pc := pc
  io.ex_pc_next_seq := pc_next_seq
}

case class DecodeStage() extends Component {
  val io = new Bundle{
    val opcode = in(OPCODE())
    val rs1 = in UInt(5 bits)
    val rs2 = in UInt(5 bits)
    val rd = in UInt(5 bits)
    val funct3 = in Bits(3 bits)
    val funct7 = in Bits(7 bits)

    val wb_rd = in UInt(5 bits)
    val wb_data = in Bits(32 bits)

    val control_signals = out(ControlSignals())

    val src1 = out Bits(32 bits)
    val src2 = out Bits(32 bits)
  }

  val regFile = new RegFile()
  regFile.io.rs1 := io.control_signals.rs1
  regFile.io.rs2 := io.control_signals.rs2
  regFile.io.rd := io.wb_rd
  io.src1 := regFile.io.src1
  io.src2 := regFile.io.src2
  regFile.io.write_data := io.wb_data


  io.control_signals := DecodeStage.default_control()

  switch(io.opcode){
    is(OPCODE.LUI){
      io.control_signals.rd := io.rd
      io.control_signals.sel_add_lhs := SEL_LHS.SRC1
    }

    is(OPCODE.AUIPC){
      io.control_signals.rd := io.rd
      io.control_signals.sel_add_lhs := SEL_LHS.PC
    }

    is(OPCODE.JAL){
      io.control_signals.rd := io.rd
      io.control_signals.sel_add_lhs := SEL_LHS.PC
      io.control_signals.is_jump := True
      io.control_signals.sel_alu_res := SEL_RES.PC
    }

    is(OPCODE.JALR) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rd := io.rd
      io.control_signals.sel_add_lhs := SEL_LHS.SRC1
      io.control_signals.is_jump := True
      io.control_signals.sel_alu_res := SEL_RES.PC
    }

    is(OPCODE.BRANCH) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rs2 := io.rs2
      io.control_signals.sel_add_lhs := SEL_LHS.PC
      io.control_signals.sel_comp_rhs := SEL_RHS.SRC2
      io.control_signals.is_branch := True
      io.control_signals.comp_op := io.funct3
    }

    is(OPCODE.LOAD) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rd := io.rd
      io.control_signals.is_load := True
      io.control_signals.funct3 := io.funct3
    }

    is(OPCODE.STORE) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rs2 := io.rs2
      io.control_signals.is_store := True
      io.control_signals.funct3 := io.funct3
    }

    is(OPCODE.OP_IMM) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rd := io.rd
      io.control_signals.alu_op := io.funct3
      io.control_signals.is_arth_shift := io.funct7(5)
    }

    is(OPCODE.OP) {
      io.control_signals.rs1 := io.rs1
      io.control_signals.rs2 := io.rs2
      io.control_signals.rd := io.rd
      io.control_signals.sel_add_rhs := SEL_RHS.SRC2
      io.control_signals.sel_comp_rhs := SEL_RHS.SRC2
      io.control_signals.alu_op := io.funct3
      io.control_signals.is_arth_shift := io.funct7(5)
      io.control_signals.is_sub := io.funct7(5)
      io.control_signals.is_muldiv := io.funct7(0)
    }
  }

}

object DecodeStage extends App {
  def default_control(): ControlSignals = {
    val control = ControlSignals()
    control.rs1 := 0
    control.rs2 := 0
    control.rd := 0
    control.sel_add_lhs := SEL_LHS.SRC1
    control.sel_add_rhs := SEL_RHS.IMM
    control.alu_op := 0
    control.sel_comp_rhs := SEL_RHS.IMM
    control.comp_op := 0
    control.sel_alu_res := SEL_RES.ALU
    control.is_jump := False
    control.is_branch := False
    control.is_arth_shift := False
    control.is_sub := False
    control.is_muldiv := False
    control.is_store := False
    control.is_load := False
    control.funct3 := 0

    control
  }

  Config.spinal.generateVerilog(DecodeStage())
}
