package flexirisc.pipeline

import flexirisc.Config
import spinal.core._

import scala.language.postfixOps

case class WriteBackStage() extends Component{
  val io = new Bundle{
    val control_signals = in(ControlSignals())

    val wb_load_data = in Bits(32 bits)
    val wb_result_data = in Bits(32 bits)

    val data = out Bits(32 bits)
    val rd = out UInt(5 bits)
  }

  io.rd := io.control_signals.rd

  when(io.control_signals.is_load) {
    io.data := io.control_signals.funct3.mux(
      B"3'b000" -> io.wb_load_data.subdivideIn(8 bits)(io.wb_result_data(1 downto 0).asUInt).asSInt.resize(32 bits).asBits,
      B"3'b001" -> io.wb_load_data.subdivideIn(16 bits)(io.wb_result_data(0).asUInt).asSInt.resize(32 bits).asBits,
      B"3'b010" -> io.wb_load_data,
      B"3'b100" -> io.wb_load_data.subdivideIn(8 bits)(io.wb_result_data(1 downto 0).asUInt).asUInt.resize(32 bits).asBits,
      B"3'b101" -> io.wb_load_data.subdivideIn(16 bits)(io.wb_result_data(0).asUInt).asUInt.resize(32 bits).asBits,
      default -> B"32'b0"
    )
  }otherwise {
    io.data := io.wb_result_data
  }

}

object WriteBackStage extends App {
  Config.spinal.generateVerilog(WriteBackStage())
}
