package flexirisc.arithmetic

import flexirisc.Config
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

case class Multiplier (width: Int) extends Component {
  val io = new Bundle {
    val start = in Bool()
    val enabled = in Bool()

    val lhs = in UInt(width bits)
    val rhs = in UInt(width bits)

    val result = out UInt(2*width bits)
    val done = out Bool()
  }

  val mul_up_lhs = UInt(width/2 bits)
  val mul_up_rhs = UInt(width/2 bits)
  val mul_up = mul_up_lhs * mul_up_rhs

  val mul_down_lhs = UInt(width/2 bits)
  val mul_down_rhs = UInt(width/2 bits)
  val mul_down = mul_down_lhs * mul_down_rhs

  val reg_state = Reg(UInt(2 bits))
  val state_next = UInt(2 bits)

  val reg_int1 = Reg(UInt(3*width/2 + 1 bits))
  val reg_int2 = Reg(UInt(3*width/2 + 1 bits))

  val state = io.start ? U(0) | reg_state

  val add_lhs = UInt(2*width bits)
  val add_rhs = UInt(2*width bits)
  val sum = add_lhs + add_rhs

  io.done := False
  io.result := sum

  when(io.enabled && state =/= U(2))
  {
    reg_state := state_next
  }

  mul_up_lhs := 0
  mul_up_rhs := 0

  mul_down_lhs := 0
  mul_down_rhs := 0
  switch(state) {
    is(0) {
      mul_up_lhs := io.lhs.subdivideIn(2 slices)(0)
      mul_up_rhs := io.rhs.subdivideIn(2 slices)(1)

      mul_down_lhs := io.lhs.subdivideIn(2 slices)(0)
      mul_down_rhs := io.rhs.subdivideIn(2 slices)(0)

      add_lhs := (mul_up << (width/2)).resize(2*width bits)
      add_rhs := mul_down.resize(2*width bits)

      reg_int1 := sum.resized
      state_next := U(1)
    }
    is(1) {
      mul_up_lhs := io.lhs.subdivideIn(2 slices)(1)
      mul_up_rhs := io.rhs.subdivideIn(2 slices)(1)

      mul_down_lhs := io.lhs.subdivideIn(2 slices)(1)
      mul_down_rhs := io.rhs.subdivideIn(2 slices)(0)

      add_lhs := (mul_up << (width / 2)).resize(2 * width bits)
      add_rhs := mul_down.resize(2 * width bits)

      reg_int2 := sum.resized
      state_next := U(2)
    }
    is(2) {
      add_lhs := (reg_int2 << (width / 2)).resize(2 * width bits)
      add_rhs := reg_int1.resize(2 * width bits)

      state_next := U(2)
      io.done := True
    }
    default {
      add_lhs := (reg_int2 << (width / 2)).resize(2 * width bits)
      add_rhs := reg_int1.resize(2 * width bits)

      state_next := U(2)
    }
  }


}

object Multiplier extends App {
  Config.spinal.generateVerilog(new Multiplier(32))
}


object MultiplierTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .withWave
      .compile {
        val dut = new Multiplier(32)
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
          dut.io.enabled #= true
          dut.clockDomain.waitSampling(2)
          dut.io.start #= false

          while (!dut.io.done.toBoolean) {
            dut.clockDomain.waitRisingEdge()
          }

          val expected = a * b
          val result = dut.io.result.toBigInt
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
