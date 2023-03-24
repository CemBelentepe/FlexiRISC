package flexirisc

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class Divider (width: Int) extends Component {
  val io = new Bundle {
    val lhs = in UInt (width bits)
    val rhs = in UInt (width bits)

    val div = out Bits(width bits)
    val rem = out Bits(width bits)
  }

  io.div := (io.lhs / io.rhs).asBits
  io.rem := (io.lhs % io.rhs).asBits
}


object Divider extends App {
  Config.spinal.generateVerilog(new Divider(32))
}

object DividerTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .compile {
        val dut = new Divider(32)
        dut.io.simPublic()
        dut
      }
      .doSim { dut =>
        // Simulation code here
        var n_passed = 0
        val n_total = 100000
        for (i <- 0 until n_total) {
          val a, b = Random.nextLong().toBigInt & ((1L << 32) - 1)
          dut.io.lhs #= a
          dut.io.rhs #= b

          sleep(10)
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
