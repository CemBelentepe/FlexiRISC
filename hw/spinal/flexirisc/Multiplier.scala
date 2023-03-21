package flexirisc

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

object MUL_MODE extends SpinalEnum() {
  val MUL, MULH, MULHS, MULHSU = newElement()
}

case class Multiplier() extends Component {
  val io = new Bundle {
    val lhs = in Bits (32 bits)
    val rhs = in Bits (32 bits)

    val mode = in(MUL_MODE)

    val result = out Bits (64 bits)
  }

  val eff_lhs = Bits(32 bits)
  val eff_rhs = Bits(32 bits)

  switch(io.mode) {
    is(MUL_MODE.MUL) {
      eff_lhs := io.lhs
      eff_rhs := io.rhs
    }
    is(MUL_MODE.MULH) {
      eff_lhs := io.lhs(15 downto 0).asSInt.resize(32 bits).asBits
      eff_rhs := io.lhs(15 downto 0).asSInt.resize(32 bits).asBits
    }
    is(MUL_MODE.MULHS) {
      eff_lhs := io.lhs(15 downto 0).asUInt.resize(32 bits).asBits
      eff_rhs := io.lhs(15 downto 0).asUInt.resize(32 bits).asBits
    }
    is(MUL_MODE.MULHSU) {
      eff_lhs := io.lhs(15 downto 0).asSInt.resize(32 bits).asBits
      eff_rhs := io.lhs(15 downto 0).asUInt.resize(32 bits).asBits
    }
  }

  val pp_0 = Vec(Vec(Bits(16 bits), 4), 4)

  for (i <- 0 until 4) {
    val a = eff_lhs.subdivideIn(8 bits)(i).asUInt
    for (j <- 0 until 4) {
      val b = eff_rhs.subdivideIn(8 bits)(j).asUInt
      pp_0(i)(j) := (a * b).asBits
    }
  }

  val pp_1 = Vec(Vec(Bits(32 bits), 2), 2)

  for (i <- 0 until 2) {
    for (j <- 0 until 2) {
      val tr = pp_0(2 * i)(2 * j).asUInt
      val tl = pp_0(2 * i + 1)(2 * j).asUInt
      val br = pp_0(2 * i)(2 * j + 1).asUInt
      val bl = pp_0(2 * i + 1)(2 * j + 1).asUInt

      val top = (tl << 8) + tr
      val btm = (bl << 8) + br
      // val sum = (btm.asUInt + top(23 downto 8).asUInt) ## btm(7 downto 0)
      val sum = ((btm << 8) + top)
      pp_1(i)(j) := sum.asBits
    }
  }

  val pp_2 = Vec(Vec(Bits(64 bits), 1), 1)
  for (i <- 0 until 1) {
    for (j <- 0 until 1) {
      val tr = pp_1(2 * i)(2 * j).asUInt
      val tl = pp_1(2 * i + 1)(2 * j).asUInt
      val br = pp_1(2 * i)(2 * j + 1).asUInt
      val bl = pp_1(2 * i + 1)(2 * j + 1).asUInt

      val top = (tl << 16) + tr
      val btm = (bl << 16) + br
      val sum = ((btm << 16) + top)
      pp_2(i)(j) := sum.asBits
    }

    io.result := pp_2(0)(0)
  }
}

object Multiplier extends App {
  Config.spinal.generateVerilog(new Multiplier())
}

object MultiplierTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .compile {
        val dut = new Multiplier()
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
          dut.io.mode #= MUL_MODE.MUL

          sleep(10)
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
