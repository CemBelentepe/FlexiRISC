package flexirisc.pipeline

import flexirisc.MemoryPort
import flexirisc.pipeline._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class StoreLoadUnit (addressWidth: Int, dataWidth: Int) extends Component {
  val io = new Bundle {
    val address = in UInt(addressWidth bits)
    val request = in Bool()
    val write_mask = in Bits(dataWidth/8 bits)
    val write_data = in Bits(dataWidth bits)
    val flush = in Bool()

    val data = out Bits(dataWidth bits)
    val valid = out Bool()

    val mem_port = master(MemoryPort(addressWidth, dataWidth))
  }

  val reg_address = RegNextWhen(io.address, io.request) init(0)
  val reg_data = RegNext(io.mem_port.response) init(B"32'h00000013")
  val reg_valid = RegNext(io.mem_port.valid) init(False)
  val reg_request_1d = RegNext(io.request) init(False)

  val address = io.request ? io.address | reg_address

  io.mem_port.address := address
  io.mem_port.payload := io.write_data
  io.mem_port.mask := io.write_mask
  io.mem_port.ready := True

  io.data := reg_request_1d ? io.mem_port.response | reg_data
  io.valid := reg_request_1d ? io.mem_port.valid | reg_valid

  when(io.flush) {
    reg_address := 0
    reg_data := B"32'h00000013"
    reg_valid := False
    reg_request_1d := False
  }
}
