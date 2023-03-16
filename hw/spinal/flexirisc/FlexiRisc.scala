package flexirisc

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.language.postfixOps

case class FlexiRisc(initFile: String = null) extends Component {
  val io = new Bundle{
    val data_mem_addr = out UInt(32 bits)
    val data_mem_data = out Bits(32 bits)
    val data_mem_mask = out Bits(4 bits)
    val done = out Bool()
    val debug = out Bool()
  }

  val core = new Core()
  val ram = new Memory(1 << 12, 32, initFile)

  io.data_mem_addr := core.io.data_mem.address
  io.data_mem_data := core.io.data_mem.payload
  io.data_mem_mask := core.io.data_mem.mask
  io.done := core.io.data_mem.address === U"32'h2010" && core.io.data_mem.mask.orR
  io.debug := core.io.data_mem.address === U"32'h2014" && core.io.data_mem.mask.orR

  ram.io.port_a.address <> core.io.inst_mem.address(13 downto 2)
  ram.io.port_a.mask <> core.io.inst_mem.mask
  ram.io.port_a.valid <> core.io.inst_mem.valid
  ram.io.port_a.payload <> core.io.inst_mem.payload
  ram.io.port_a.ready <> core.io.inst_mem.ready
  ram.io.port_a.response <> core.io.inst_mem.response

  ram.io.port_b.address <> core.io.data_mem.address(13 downto 2)
  ram.io.port_b.mask <> core.io.data_mem.mask
  ram.io.port_b.valid <> core.io.data_mem.valid
  ram.io.port_b.payload <> core.io.data_mem.payload
  ram.io.port_b.ready <> core.io.data_mem.ready
  ram.io.port_b.response <> core.io.data_mem.response
}

object FlexiRisc extends App{
  Config.spinal.generateVerilog(new FlexiRisc())
}

object FlexiRiscTest {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile {
      val dut = new FlexiRisc("/home/pro/Documents/GitHub/FlexiRISC/test/muldiv/muldiv.data")
      dut.io.simPublic()
      dut
    }.doSim { dut =>
      //Simulation code here
      dut.clockDomain.forkStimulus(10)
      var done = dut.io.done.toBoolean
      while(!done) {
        done = dut.io.done.toBoolean
        dut.clockDomain.waitRisingEdge()
      }

    }
  }
}