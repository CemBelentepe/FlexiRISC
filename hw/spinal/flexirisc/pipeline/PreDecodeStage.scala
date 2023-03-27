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

    val id1_instruction_dbg = in Bits(32 bits)
    val id1_opcode = in(OPCODE())
    val id1_rs1 = in UInt (5 bits)
    val id1_rs2 = in UInt (5 bits)
    val id1_rd = in UInt (5 bits)
    val id1_funct3 = in Bits (3 bits)
    val id1_funct7 = in Bits (7 bits)
    val id1_immediate = in Bits (32 bits)
    val id1_pc = in UInt (64 bits)
    val id1_pc_next_seq = in UInt (64 bits)

    val id2_instruction_dbg = out Bits(32 bits)
    val id2_opcode = out(OPCODE())
    val id2_rs1 = out UInt (5 bits)
    val id2_rs2 = out UInt (5 bits)
    val id2_rd = out UInt (5 bits)
    val id2_funct3 = out Bits (3 bits)
    val id2_funct7 = out Bits (7 bits)
    val id2_immediate = out Bits (32 bits)
    val id2_pc = out UInt (64 bits)
    val id2_pc_next_seq = out UInt (64 bits)
  }

  val instruction_dbg = Reg(Bits(32 bits)) init (B"32'h00000013")
  val opcode = Reg(OPCODE()) init (OPCODE.OP)
  val rs1 = Reg(UInt(5 bits)) init (0)
  val rs2 = Reg(UInt(5 bits)) init (0)
  val rd = Reg(UInt(5 bits)) init (0)
  val funct3 = Reg(Bits(3 bits)) init (0)
  val funct7 = Reg(Bits(7 bits)) init (0)
  val immediate = Reg(Bits(32 bits)) init (0)
  val pc = Reg(UInt(64 bits)) init (0)
  val pc_next_seq = Reg(UInt(64 bits)) init (0)

  when(io.id2_flush) {
    instruction_dbg := B"32'h00000013"
    opcode := OPCODE.OP
    rs1 := 0
    rs2 := 0
    rd := 0
    funct3 := 0
    funct7 := 0
    immediate := 0
    pc := 0
    pc_next_seq := 0
  }.elsewhen(!io.id2_stall) {
    instruction_dbg := io.id1_instruction_dbg
    opcode := io.id1_opcode
    rs1 := io.id1_rs1
    rs2 := io.id1_rs2
    rd := io.id1_rd
    funct3 := io.id1_funct3
    funct7 := io.id1_funct7
    immediate := io.id1_immediate
    pc := io.id1_pc
    pc_next_seq := io.id1_pc_next_seq
  }

  io.id2_instruction_dbg := instruction_dbg
  io.id2_opcode := opcode
  io.id2_rs1 := rs1
  io.id2_rs2 := rs2
  io.id2_rd := rd
  io.id2_funct3 := funct3
  io.id2_funct7 := funct7
  io.id2_immediate := immediate
  io.id2_pc := pc
  io.id2_pc_next_seq := pc_next_seq
}

case class PreDecodeStage() extends Component {
  val io = new Bundle {
    val pc = in UInt(64 bits)
    val instruction = in Bits (64 bits)
    val inst_valid = in Bool()

    val opcode = out(OPCODE())
    val rs1 = out UInt(5 bits)
    val rs2 = out UInt(5 bits)
    val rd = out UInt(5 bits)
    val funct3 = out Bits(3 bits)
    val funct7 = out Bits(7 bits)
    val immediate = out Bits(32 bits)

    val stall_if = out Bool()
    val stage_valid = out Bool()

    val instruction_dbg = out Bits (32 bits)
  }

  val instruction = (!io.inst_valid) ? B"32'h00000013" | io.instruction.subdivideIn(2 slices)(io.pc(2).asUInt)
  io.instruction_dbg := instruction
  io.stage_valid := True

  // TODO stall_if is dependent on the step counter
  io.stall_if := False

  val immediateDecoder = new ImmediateDecoder
  immediateDecoder.io.instruction := instruction

  when(instruction(1 downto 0) === B"2'b11") {
    io.opcode.asData := instruction(6 downto 2)
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

    io.immediate := instruction(6 downto 2)
      .mux(
        B"5'b01101" -> immediateDecoder.io.u_imm,
        B"5'b00101" -> immediateDecoder.io.u_imm,
        B"5'b11011" -> immediateDecoder.io.j_imm,
        B"5'b11000" -> immediateDecoder.io.b_imm,
        B"5'b00000" -> immediateDecoder.io.i_imm,
        B"5'b01000" -> immediateDecoder.io.s_imm,
        B"5'b00100" -> immediateDecoder.io.i_imm,
        default -> B(0)
      )

    io.rs1 := instruction(19 downto 15).asUInt
    io.rs2 := instruction(24 downto 20).asUInt
    io.rd := instruction(11 downto 7).asUInt
    io.funct3 := instruction(14 downto 12)
    io.funct7 := instruction(31 downto 25)
  }.otherwise {
    // TODO Decode the RV32c here
    io.opcode := OPCODE.OP
    io.immediate := 0

    io.rs1 := 0
    io.rs2 := 0
    io.rd := 0
    io.funct3 := 0
    io.funct7 := 0
  }

}

case class ImmediateDecoder() extends Component {
  val io = new Bundle {
    val instruction = in Bits(32 bits)

    val i_imm = out Bits(32 bits)
    val s_imm = out Bits(32 bits)
    val b_imm = out Bits(32 bits)
    val u_imm = out Bits(32 bits)
    val j_imm = out Bits(32 bits)
  }

  io.i_imm := B(
    32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> io.instruction(20)
  )

  io.s_imm := B(
    32 bits,
    (31 downto 11) -> io.instruction(31),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> io.instruction(7)
  )

  io.b_imm := B(
    32 bits,
    (31 downto 12) -> io.instruction(31),
    11 -> io.instruction(7),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(11 downto 8),
    0 -> False
  )

  io.u_imm := B(32 bits, (31 downto 12) -> io.instruction(31 downto 12), (11 downto 0) -> False)

  io.j_imm := B(
    32 bits,
    (31 downto 20) -> io.instruction(31),
    (19 downto 12) -> io.instruction(19 downto 12),
    11 -> io.instruction(20),
    (10 downto 5) -> io.instruction(30 downto 25),
    (4 downto 1) -> io.instruction(24 downto 21),
    0 -> False
  )
}

object PreDecodeStage extends App {
  Config.spinal.generateVerilog(PreDecodeStage())
}
