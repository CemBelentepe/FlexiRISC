package flexirisc.pipeline

import flexirisc.{Config, MemoryPort}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class IfId1() extends Component {
  val io = new Bundle {
    val id1_stall = in Bool()
    val id1_flush = in Bool()

    val if_pc = in UInt (32 bits)
    val if_pc_next_seq = in UInt(32 bits)

    val id1_pc = out UInt(32 bits)
    val id1_pc_next_seq = out UInt(32 bits)
  }

  val pc = Reg(UInt(32 bits)) init(0)
  val pc_next_seq = Reg(UInt(32 bits)) init(0)

  when(io.id1_flush){
    pc := 0
    pc_next_seq := 0
  }.elsewhen(!io.id1_stall){
    pc := io.if_pc
    pc_next_seq := io.if_pc_next_seq
  }

  io.id1_pc := pc
  io.id1_pc_next_seq := pc_next_seq
}

case class FetchStage() extends Component {
  val io = new Bundle {
    val jump_enable = in Bool()
    val jump_address = in UInt(32 bits)
    val halt_stage = in Bool()

    val pc = out UInt (32 bits)
    val pc_next_seq = out UInt(32 bits)
    val request = out Bool()
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

  io.request := !io.halt_stage
}


object FetchStage extends App {
  Config.spinal.generateVerilog(FetchStage())
}
