package flexirisc.pipeline

import flexirisc.Config
import spinal.core._
import spinal.core.internals.Operator
import spinal.lib._

import scala.language.postfixOps

object OPCODE extends SpinalEnum() {
  val LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STORE, OP_IMM, OP = newElement()
}

case class Id1Id2() extends Component {
  val io = new Bundle {
    val id2_stall = in Bool ()
    val id2_flush = in Bool ()

    val id1_opcode = in(OPCODE())
    val id1_rs1 = in UInt(5 bits)
    val id1_rs2 = in UInt(5 bits)
    val id1_rd = in UInt(5 bits)
    val id1_funct3 = in Bits(3 bits)
    val id1_funct7_5 = in Bool()
    val id1_immediate = in Bits(32 bits)
    val id1_pc = in UInt(32 bits)
    val id1_pc_next_seq = in UInt(32 bits)

    val id2_opcode = out(OPCODE())
    val id2_rs1 = out UInt(5 bits)
    val id2_rs2 = out UInt(5 bits)
    val id2_rd = out UInt(5 bits)
    val id2_funct3 = out Bits(3 bits)
    val id2_funct7_5 = out Bool()
    val id2_immediate = out Bits(32 bits)
    val id2_pc = out UInt(32 bits)
    val id2_pc_next_seq = out UInt(32 bits)
  }

  val opcode = Reg(OPCODE()) init(OPCODE.OP)
  val rs1 = Reg(UInt(5 bits)) init (0)
  val rs2 = Reg(UInt(5 bits)) init(0)
  val rd = Reg(UInt(5 bits)) init(0)
  val funct3 = Reg(Bits(3 bits)) init(0)
  val funct7_5 = Reg(Bool()) init(False)
  val immediate = Reg(Bits(32 bits)) init(0)
  val pc = Reg(UInt(32 bits)) init(0)
  val pc_next_seq = Reg(UInt(32 bits)) init(0)

  when(io.id2_flush) {
    opcode := OPCODE.OP
    rs1 := 0
    rs2 := 0
    rd := 0
    funct3 := 0
    funct7_5 := False
    immediate := 0
    pc := 0
    pc_next_seq := 0
  }.elsewhen(!io.id2_stall) {
    opcode := io.id1_opcode
    rs1 := io.id1_rs1
    rs2 := io.id1_rs2
    rd := io.id1_rd
    funct3 := io.id1_funct3
    funct7_5 := io.id1_funct7_5
    immediate := io.id1_immediate
    pc := io.id1_pc
    pc_next_seq := io.id1_pc_next_seq
  }

  io.id2_opcode := opcode
  io.id2_rs1 := rs1
  io.id2_rs2 := rs2
  io.id2_rd := rd
  io.id2_funct3 := funct3
  io.id2_funct7_5 := funct7_5
  io.id2_immediate := immediate
  io.id2_pc := pc
  io.id2_pc_next_seq := pc_next_seq
}

case class PreDecodeStage() extends Component {
  val io = new Bundle {
    val instruction = in Bits (32 bits)

    val opcode = out(OPCODE())
    val rs1 = out UInt (5 bits)
    val rs2 = out UInt (5 bits)
    val rd = out UInt (5 bits)
    val funct3 = out Bits (3 bits)
    val funct7_5 = out Bool ()
    val immediate = out Bits (32 bits)

    val stall_if = out Bool()
  }

  val i_imm = B(
    32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> io.instruction(20)
  )

  val s_imm = B(
    32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> io.instruction(7)
  )

  val b_imm = B(
    32 bits,
    (31 downto 12) -> io.instruction(31),
    11 -> io.instruction(7),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> False
  )

  val u_imm = B(32 bits, (31 downto 12) -> io.instruction(31 downto 12), (11 downto 0) -> False)

  val j_imm = B(
    32 bits,
    (31 downto 20) -> io.instruction(31),
    (19 downto 12) -> io.instruction(19 downto 12),
    11 -> io.instruction(20),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> False
  )

  when(io.instruction(1 downto 0) === B"2'b11") {
    io.opcode.asData := io
      .instruction(6 downto 2)
      .mux(
        B"5'b01101" -> OPCODE.LUI.asData,
        B"5'b00101" -> OPCODE.AUIPC.asData,
        B"5'b11011" -> OPCODE.JAL.asData,
        B"5'b11001" -> OPCODE.JALR.asData,
        B"5'b11000" -> OPCODE.BRANCH.asData,
        B"5'b00000" -> OPCODE.LOAD.asData,
        B"5'b01000" -> OPCODE.STORE.asData,
        B"5'b00100" -> OPCODE.OP_IMM.asData,
        B"5'b01100" -> OPCODE.OP.asData,
        default -> OPCODE.OP.asData
      )

    io.immediate := io
      .instruction(6 downto 2)
      .mux(
        B"5'b01101" -> u_imm,
        B"5'b00101" -> u_imm,
        B"5'b11011" -> j_imm,
        B"5'b11000" -> b_imm,
        B"5'b00000" -> i_imm,
        B"5'b01000" -> s_imm,
        B"5'b00100" -> i_imm,
        default -> B(0)
      )

    io.rs1 := io.instruction(19 downto 15).asUInt
    io.rs2 := io.instruction(24 downto 20).asUInt
    io.rd := io.instruction(11 downto 7).asUInt
    io.funct3 := io.instruction(14 downto 12)
    io.funct7_5 := io.instruction(31 downto 25)(5)
    io.stall_if := False
  }.otherwise {
    // TODO Decode the RV32c here
    io.opcode := OPCODE.OP
    io.immediate := 0

    io.rs1 := 0
    io.rs2 := 0
    io.rd := 0
    io.funct3 := 0
    io.funct7_5 := False
    // TODO stall_if is dependent on the step counter
    io.stall_if := False
  }

}

object PreDecodeStage extends App {
  Config.spinal.generateVerilog(PreDecodeStage())
}
