package flexirisc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.HexTools

import scala.language.postfixOps
import java.io.File
import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class MemoryPort(addressWidth: Int, dataWidth: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val address = UInt(addressWidth bits)
  val payload = Bits(dataWidth bits)
  val response = Bits(dataWidth bits)
  val mask = Bits(dataWidth/8 bits)

  override def asMaster(): Unit = {
    out(ready, address, payload, mask)
    in(valid, response)
  }
}

case class Memory(depth: Int, dataWidth: Int, initFile: String = null) extends Component{
  val io = new Bundle {
    val port_a = slave(MemoryPort(log2Up(depth-1), dataWidth))
    val port_b = slave(MemoryPort(log2Up(depth-1), dataWidth))
  }

  val mem = Mem(Bits(dataWidth bits), depth)
  if(initFile != null) {
    def generateInitialContent = {
      val buffer = new ArrayBuffer[BigInt]
      for (line <- Source.fromFile(initFile).getLines) {
        val tokens: Array[String] = line.split("(//)").map(_.trim)
        if (tokens.length > 0 && tokens(0) != "") {
          val i = BigInt(tokens(0), 16)
          buffer.append(i)
        }
      }

      buffer
    }
    mem.initialContent = generateInitialContent.toArray
  }

  val wea = io.port_a.mask.orR
  io.port_a.response := mem.readWriteSync(
    io.port_a.address,
    io.port_a.payload,
    True,
    wea,
    io.port_a.mask
  )
  val port_a_addr_1d = RegNext(io.port_a.address)
  val wea_1d = RegNext(wea) init(True)
  io.port_a.valid := !wea_1d

  val web = io.port_b.mask.orR
  io.port_b.response := mem.readWriteSync(
    io.port_b.address,
    io.port_b.payload,
    True,
    web,
    io.port_b.mask
  )
  val port_b_addr_1d = RegNext(io.port_b.address)
  val web_1d = RegNext(web) init(True)
  io.port_b.valid := !web_1d
}

object Memory extends App {
  Config.spinal.generateVerilog(new Memory(1 << 12, 32))
}