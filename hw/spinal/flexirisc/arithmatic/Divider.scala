package flexirisc.arithmatic

import flexirisc.Config
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class Divider (width: Int) extends Component {
  val io = new Bundle {
    val lhs = in UInt (width bits)
    val rhs = in UInt (width bits)
    val start = in Bool()

    val div = out Bits(width bits)
    val rem = out Bits(width bits)
    val done = out Bool()
  }

  val priorityEncoder = PriorityEncoder(width)
  priorityEncoder.io.data := io.lhs.asBits

  val divReg = Reg(UInt(width bits))
  val remReg = Reg(UInt(width bits))
  val numReg = Reg(UInt(width bits))
  val bitsLeftReg = Reg(UInt(log2Up(width) bits))

  val div = io.start ? U(0) | divReg
  val rem = io.start ? U(0) | remReg
  val num = io.start ? (io.lhs << ((width - 1) - priorityEncoder.io.res)).resized | numReg
  val bitsLeft = io.start ? priorityEncoder.io.res | bitsLeftReg - 1

  io.done := !io.start && bitsLeftReg === 0

  when(io.start || bitsLeftReg =/= 0) {
    val remNext = UInt(width bits)
    val rem1 = (rem(width-2 downto 0) ## num.msb).asUInt

    val ge = rem1 >= io.rhs
    when(ge) {
      remNext := rem1 - io.rhs
    }.otherwise{
      remNext := rem1
    }

    divReg := (div(width-2 downto 0).asBits ## ge).asUInt
    remReg := remNext
    numReg := num |<< 1
    bitsLeftReg := bitsLeft
  }

  io.div := div.asBits
  io.rem := rem.asBits
}

case class PriorityEncoder(width: Int) extends Component {
  val io = new Bundle {
    val data = in Bits(width bits)

    val res = out UInt(log2Up(width) bits)
  }

  io.res := 0
  for(i <- 0 until width) {
    when(io.data(i)){
      io.res := i
    }
  }

}

object Divider extends App {
  Config.spinal.generateVerilog(new Divider(32))
}

object DividerTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .withWave
      .compile {
        val dut = new Divider(32)
        dut.io.simPublic()
        dut
      }
      .doSim { dut =>
        // Simulation code here
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()

        // Automatic Test
        var n_passed = 0
        val n_total = 1000
        for (i <- 0 until n_total) {
          val a, b = Random.nextLong().toBigInt & ((1L << 32) - 1)
          dut.io.lhs #= a
          dut.io.rhs #= b
          dut.io.start #= true
          dut.clockDomain.waitSampling(2)
          dut.io.start #= false

          while(!dut.io.done.toBoolean) {
            dut.clockDomain.waitRisingEdge()
          }

          val expected_div = a / b
          val expected_rem = a % b
          val result_div = dut.io.div.toBigInt
          val result_rem = dut.io.rem.toBigInt
          val passed = (result_div == expected_div) && (result_rem == expected_rem)

          if (!passed) {
            println(a, b, expected_div, result_div, expected_rem, result_rem)
          } else {
            n_passed += 1
          }
        }
        println("Passed: " + n_passed + "/" + n_total)
      }
  }
}
