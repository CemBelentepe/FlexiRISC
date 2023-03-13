package flexirisc.pipeline

import flexirisc.{Config, MemoryPort}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class IfId() extends Component {
  val io = new Bundle {
    val id_stall = in Bool()
    val id_flush = in Bool()

    val if_instruction = in Bits(32 bits)
    val if_pc = in UInt (32 bits)
    val if_pc_next_seq = in UInt(32 bits)

    val id_instruction = out Bits(32 bits)
    val id_pc = out UInt(32 bits)
    val id_pc_next_seq = out UInt(32 bits)
  }

  val instruction = Reg(Bits(32 bits)) init(B"32'h00000013")
  val pc = Reg(UInt(32 bits)) init(0)
  val pc_next_seq = Reg(UInt(32 bits)) init(0)

  when(io.id_flush){
    instruction := B"32'h00000013"
    pc := 0
    pc_next_seq := 0
  }.elsewhen(!io.id_stall){
    instruction := io.if_instruction
    pc := io.if_pc
    pc_next_seq := io.if_pc_next_seq
  }

  io.id_instruction := instruction
  io.id_pc := pc
  io.id_pc_next_seq := pc_next_seq
}

case class FetchStage() extends Component {
  val io = new Bundle {
    val jump_enable = in Bool()
    val jump_address = in UInt(32 bits)
    val halt_stage = in Bool()

    val instruction = out Bits(32 bits)
    val pc = out UInt (32 bits)
    val pc_next_seq = out UInt(32 bits)
    val stage_valid = out Bool()

    val inst_mem = master(MemoryPort(32, 32))
  }

  val pc = Reg(UInt(32 bits)) init(0)

  io.pc := pc
  io.pc_next_seq := pc + 4

  when(!io.halt_stage){
    when(io.jump_enable){
      pc := io.jump_address
    }otherwise{
      pc := io.pc_next_seq
    }
  }

  io.inst_mem.address := pc
  io.inst_mem.ready := !io.halt_stage
  io.inst_mem.payload := 0
  io.stage_valid := io.inst_mem.valid
  io.instruction := io.inst_mem.response
  io.inst_mem.mask := B"4'b0000"
}


object FetchStage extends App {
  Config.spinal.generateVerilog(FetchStage())
}
