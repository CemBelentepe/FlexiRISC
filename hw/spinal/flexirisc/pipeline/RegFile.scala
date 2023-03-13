package flexirisc.pipeline

import flexirisc.Config
import spinal.core._

import scala.language.postfixOps

case class RegFile() extends Component {
  val io = new Bundle {
    val rs1 = in UInt(5 bits)
    val rs2 = in UInt(5 bits)
    val rd = in UInt(5 bits)
    val write_data = in Bits(32 bits)

    val src1 = out Bits(32 bits)
    val src2 = out Bits(32 bits)
  }


  val RegisterFileArea = new ClockingArea(
    ClockDomain(
      clock = clockDomain.clock,
      reset = clockDomain.reset,
      config = ClockDomainConfig(
        clockEdge = FALLING,
        resetKind = ASYNC,
        resetActiveLevel = HIGH
      ))) {

    val regFile = Mem(Bits(32 bits), 32)

    regFile.write(
      io.rd,
      io.write_data,
      io.rd =/= 0
    )

    io.src1 := (io.rs1 =/= 0) ? regFile.readAsync(io.rs1) | 0
    io.src2 := (io.rs2 =/= 0) ? regFile.readAsync(io.rs2) | 0
  }
}

object RegFile extends App {
  Config.spinal.generateVerilog(RegFile())
}
