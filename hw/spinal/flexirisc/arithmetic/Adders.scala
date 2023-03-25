package flexirisc.arithmetic

import flexirisc.Config
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

case class CarrySelectAdder(width: Int, unitSize: Int) extends Component {
  val io = new Bundle {
    val lhs = in UInt(width bits)
    val rhs = in UInt(width bits)
    val c_in = in Bool()

    val res = out UInt(width bits)
    val c_out = out Bool()
  }

  assert(width % unitSize == 0, "`width` must be a multiple of `unitSize`.")

  val lhs_arr = io.lhs.subdivideIn(unitSize bits)
  val rhs_arr = io.rhs.subdivideIn(unitSize bits)

  val res_arr = Vec(UInt(unitSize bits), width/unitSize)
  val c_arr = Vec(Bool(), width/unitSize)

  val res_0 = lhs_arr(0) +^ rhs_arr(0) + io.c_in.asUInt(unitSize bits)
  res_arr(0) := res_0(unitSize-1 downto 0)
  c_arr(0) := res_0(unitSize)

  for(i <- 1 until width/unitSize) {
    val res_0 = lhs_arr(i) +^ rhs_arr(i) + U(0, unitSize bits)
    val res_1 = lhs_arr(i) +^ rhs_arr(i) + U(1, unitSize bits)

    res_arr(i) := c_arr(i-1) ? res_1(unitSize-1 downto 0) | res_0(unitSize-1 downto 0)
    c_arr(i) := c_arr(i-1) ? res_1(unitSize) | res_0(unitSize)
  }

  io.res := res_arr.asBits.asUInt
  io.c_out := c_arr(width/unitSize - 1)
}

object CarrySelectAdder extends App {
  Config.spinal.generateVerilog(CarrySelectAdder(32, 8))
}

object AdderTest {
  def main(args: Array[String]): Unit = {
    val width = 32
    val unitSize = 8
    SimConfig.compile{
      val dut = new CarrySelectAdder(width, unitSize)
      dut.io.simPublic()
      dut
    }.doSim { dut =>
      //Simulation code here
      var n_passed = 0
      val n_total = 10000
      for(i <- 0 until n_total) {
        val a, b = Random.nextLong() & ((1L << width)-1)
        val c = Random.nextBoolean()
        dut.io.lhs #= a
        dut.io.rhs #= b
        dut.io.c_in #= c

        sleep(10)
        val expected = a + b + (if(c) 1 else 0)
        val result = dut.io.res.toBigInt + (dut.io.c_out.toBigInt << width)
        val passed = result == expected
        if(!passed){
          println(a, b, c, expected, result)
        }else {
          n_passed += 1
        }
      }
      println("Passed: " + n_passed + "/" + n_total)
    }
  }
}