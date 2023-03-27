package flexirisc.pipeline

import flexirisc.{Config, MemoryPort}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MemWb() extends Component {
  val io = new Bundle {
    val wb_stall = in Bool()
    val wb_flush = in Bool()

    val mem_control_signals = in(ControlSignals())
    val mem_result_data = in Bits(64 bits)

    val wb_control_signals = out(ControlSignals())
    val wb_result_data = out Bits (64 bits)
  }

  val control_signals = Reg(ControlSignals()) init(DecodeStage.default_control())
  val result_data = Reg(Bits (64 bits)) init(0)

  when(io.wb_flush) {
    control_signals := DecodeStage.default_control()
    result_data := 0
  }.elsewhen(!io.wb_stall) {
    control_signals := io.mem_control_signals
    result_data := io.mem_result_data
  }

  io.wb_control_signals := control_signals
  io.wb_result_data := result_data
}

case class MemoryStage() extends Component {
  val io = new Bundle {
    val control_signals = in(ControlSignals())

    val ex_result_data = in Bits(64 bits)
    val src2 = in Bits(64 bits)

    val result_data = out Bits(64 bits)
    val stage_valid = out Bool()

    val request = out Bool()
    val write_mask = out Bits(8 bits)
    val write_data = out Bits(64 bits)
  }

  io.request := io.control_signals.is_store | io.control_signals.is_load

  io.write_mask :=
    B(8 bits, default -> io.control_signals.is_store) &
    io.control_signals.funct3.mux(
    0 -> io.ex_result_data(2 downto 0).asUInt.muxList(
      for(index <- 0 until 8) yield (index, B(1 << index, 8 bits))
     ),
    1 -> io.ex_result_data(2 downto 1).asUInt.muxList(
      for(index <- 0 until 4) yield (index, B(3 << 2*index, 8 bits))
    ),
    2 -> io.ex_result_data(2).asUInt.muxList(
      for (index <- 0 until 2) yield (index, B(15 << 4 * index, 8 bits))
    ),
    3 -> B"8'b11111111",
    default -> B(0)
  )
  io.stage_valid := True

  val b = io.src2.subdivideIn(8 bits)(0)
  val h = io.src2.subdivideIn(16 bits)(0)
  val w = io.src2.subdivideIn(32 bits)(0)

  io.write_data := io.control_signals.funct3.mux(
    0-> b ## b ## b ## b ## b ## b ## b ## b,
    1 -> h ## h ## h ## h,
    2 -> w ## w,
    3 -> io.src2,
    default -> B(0)
  )

  io.result_data := io.ex_result_data
}

object MemoryStage extends App{
  Config.spinal.generateVerilog(MemoryStage())
}

