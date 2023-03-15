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

    val id_control_signals = in(ControlSignals())
    val id_src1 = in Bits(32 bits)
    val id_src2 = in Bits(32 bits)
    val id_immediate = in Bits(32 bits)
    val id_pc = in UInt(32 bits)
    val id_pc_next_seq = in UInt(32 bits)

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
    control_signals := io.id_control_signals
    src1 := io.id_src1
    src2 := io.id_src2
    immediate := io.id_immediate
    pc := io.id_pc
    pc_next_seq := io.id_pc_next_seq
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
    val instruction = in Bits(32 bits)

    val wb_rd = in UInt(5 bits)
    val wb_data = in Bits(32 bits)

    val control_signals = out(ControlSignals())

    val src1 = out Bits(32 bits)
    val src2 = out Bits(32 bits)
    val immediate = out Bits(32 bits)
  }

  val regFile = new RegFile()
  regFile.io.rs1 := io.control_signals.rs1
  regFile.io.rs2 := io.control_signals.rs2
  regFile.io.rd := io.wb_rd
  io.src1 := regFile.io.src1
  io.src2 := regFile.io.src2
  regFile.io.write_data := io.wb_data

  val i_imm = B(32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> io.instruction(20)
  )

  val s_imm = B(32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> io.instruction(7)
  )

  val b_imm = B(32 bits,
    (31 downto 12) -> io.instruction(31),
    11 -> io.instruction(7),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> False
  )

  val u_imm = B(32 bits,
    (31 downto 12) -> io.instruction(31 downto 12),
    (11 downto 0) -> False
  )

  val j_imm = B(32 bits,
    (31 downto 20) -> io.instruction(31),
    (19 downto 12) -> io.instruction(19 downto 12),
    11 -> io.instruction(20),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> False
  )

  val funct7 = io.instruction(31 downto 25)
  val rs2 = io.instruction(24 downto 20).asUInt
  val rs1 = io.instruction(19 downto 15).asUInt
  val funct3 = io.instruction(14 downto 12)
  val rd = io.instruction(11 downto 7).asUInt
  val opcode = io.instruction(6 downto 2)

  io.immediate := 0
  io.control_signals := DecodeStage.default_control()

  switch(opcode){
    // LUI
    is(B"5'b01101"){
      io.immediate := u_imm
      io.control_signals.rd := rd
      io.control_signals.sel_add_lhs := SEL_LHS.SRC1
    }

    // AUIPC
    is(B"5'b00101"){
      io.immediate := u_imm
      io.control_signals.rd := rd
      io.control_signals.sel_add_lhs := SEL_LHS.PC
    }

    // JAL
    is(B"5'b11011"){
      io.immediate := j_imm
      io.control_signals.rd := rd
      io.control_signals.sel_add_lhs := SEL_LHS.PC
      io.control_signals.is_jump := True
      io.control_signals.sel_alu_res := SEL_RES.PC
    }

    // JALR
    is(B"5'b11001") {
      io.immediate := i_imm
      io.control_signals.rs1 := rs1
      io.control_signals.rd := rd
      io.control_signals.sel_add_lhs := SEL_LHS.SRC1
      io.control_signals.is_jump := True
      io.control_signals.sel_alu_res := SEL_RES.PC
    }

    // BRANCH
    is(B"5'b11000") {
      io.immediate := b_imm
      io.control_signals.rs1 := rs1
      io.control_signals.rs2 := rs2
      io.control_signals.sel_add_lhs := SEL_LHS.PC
      io.control_signals.sel_comp_rhs := SEL_RHS.SRC2
      io.control_signals.is_branch := True
      io.control_signals.comp_op := funct3
    }

    // LOAD
    is(B"5'b00000") {
      io.immediate := i_imm
      io.control_signals.rs1 := rs1
      io.control_signals.rd := rd
      io.control_signals.is_load := True
      io.control_signals.funct3 := funct3
    }

    // Store
    is(B"5'b01000") {
      io.immediate := s_imm
      io.control_signals.rs1 := rs1
      io.control_signals.rs2 := rs2
      io.control_signals.is_store := True
      io.control_signals.funct3 := funct3
    }

    // OP-IMM
    is(B"5'b00100") {
      io.immediate := i_imm
      io.control_signals.rs1 := rs1
      io.control_signals.rd := rd
      io.control_signals.alu_op := funct3
      io.control_signals.is_arth_shift := funct7(5)
    }

    // OP
    is(B"5'b01100") {
      io.control_signals.rs1 := rs1
      io.control_signals.rs2 := rs2
      io.control_signals.rd := rd
      io.control_signals.sel_add_rhs := SEL_RHS.SRC2
      io.control_signals.sel_comp_rhs := SEL_RHS.SRC2
      io.control_signals.alu_op := funct3
      io.control_signals.is_arth_shift := funct7(5)
      io.control_signals.is_sub := funct7(5)
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
    control.is_store := False
    control.is_load := False
    control.funct3 := 0

    control
  }

  Config.spinal.generateVerilog(DecodeStage())
}
