package flexirisc

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class Multiplier(width: Int) extends Component {
  val io = new Bundle {
    val lhs = in UInt (width bits)
    val rhs = in UInt (width bits)

    val result = out Vec(UInt (2 * width bits), log2Up(width/8) + 1)
  }

  assert(width >= 8, "Width must be greater or equal to 8")
  assert(isPow2(width), "Width must be a power of 2")

  if (width <= 8) {
    io.result(0) := io.lhs * io.rhs
  } else {
    val mul_ll = new Multiplier(width/2)
    val mul_hh = new Multiplier(width/2)
    val last_ind = log2Up(width/8)

    mul_ll.io.lhs := io.lhs.subdivideIn(2 slices)(0)
    mul_ll.io.rhs := io.rhs.subdivideIn(2 slices)(0)

    mul_hh.io.lhs := io.lhs.subdivideIn(2 slices)(1)
    mul_hh.io.rhs := io.rhs.subdivideIn(2 slices)(1)

    io.result(last_ind) := ((io.lhs.subdivideIn(2 slices)(0) * io.rhs.subdivideIn(2 slices)(1)) << (width/2)) +
      (mul_hh.io.result(last_ind-1).asBits ## mul_ll.io.result(last_ind-1).asBits).asUInt +
      ((io.lhs.subdivideIn(2 slices)(1) * io.rhs.subdivideIn(2 slices)(0)) << (width/2))

    for(i <- 0 until log2Up(width/8)){
      io.result(i) := (mul_hh.io.result(i).asBits ## mul_ll.io.result(i).asBits).asUInt
    }
  }
}

object Multiplier extends App {
  Config.spinal.generateVerilog(new Multiplier(64))
}

object MultiplierTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .compile {
        val dut = new Multiplier(32)
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
          val expected = a * b
          val result = dut.io.result(log2Up(32/8)).toBigInt
          val passed = result == expected

          if (!passed) {
            println(a, b, expected, result)
          } else {
            n_passed += 1
          }
        }
        println("Passed: " + n_passed + "/" + n_total)
      }
  }
}
