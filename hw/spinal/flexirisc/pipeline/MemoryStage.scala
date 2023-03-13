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
    val mem_load_data = in Bits(32 bits)
    val mem_result_data = in Bits(32 bits)

    val wb_control_signals = out(ControlSignals())
    val wb_load_data = out Bits (32 bits)
    val wb_result_data = out Bits (32 bits)
  }

  val control_signals = Reg(ControlSignals()) init(DecodeStage.default_control())
  val load_data = Reg(Bits (32 bits)) init(0)
  val result_data = Reg(Bits (32 bits)) init(0)

  when(io.wb_flush) {
    control_signals := DecodeStage.default_control()
    load_data := 0
    result_data := 0
  }.elsewhen(!io.wb_stall) {
    control_signals := io.mem_control_signals
    load_data := io.mem_load_data
    result_data := io.mem_result_data
  }

  io.wb_control_signals := control_signals
  io.wb_load_data := load_data
  io.wb_result_data := result_data
}

case class MemoryStage() extends Component {
  val io = new Bundle {
    val control_signals = in(ControlSignals())

    val ex_result_data = in Bits(32 bits)
    val src2 = in Bits(32 bits)

    val data_mem = master(MemoryPort(32, 32))

    val load_data = out Bits(32 bits)
    val result_data = out Bits(32 bits)
    val stage_valid = out Bool()
  }

  io.data_mem.address := io.ex_result_data.asUInt
  io.data_mem.ready := io.control_signals.is_store | io.control_signals.is_load

  io.data_mem.mask :=
    B(4 bits, default -> io.control_signals.is_store) &
    io.control_signals.funct3.mux(
    0 -> io.ex_result_data(1 downto 0).asUInt.muxList(
      for(index <- 0 until 4) yield (index, B(1 << index, 4 bits))
     ),
    1 -> io.ex_result_data(1).asUInt.muxList(
      for(index <- 0 until 2) yield (index, B(3 << 2*index, 4 bits))
    ),
    2 -> B"4'b1111",
    default -> B(0)
  )
  io.stage_valid := io.control_signals.is_load ? io.data_mem.valid | True
  io.load_data := io.data_mem.response
  io.data_mem.payload := io.src2

  io.result_data := io.ex_result_data
}

object MemoryStage extends App{
  Config.spinal.generateVerilog(MemoryStage())
}

