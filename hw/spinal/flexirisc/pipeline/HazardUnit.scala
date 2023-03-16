package flexirisc.pipeline

import flexirisc.Config
import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class HazardUnit() extends Component {
  val io = new Bundle {
    val if_valid = in Bool()
    val id1_stall_if = in Bool()
    val id1_valid = in Bool()
    val id2_valid = in Bool()
    val ex_valid = in Bool()
    val mem_valid = in Bool()

    val id2_control_signals = in(ControlSignals())
    val ex_control_signals = in(ControlSignals())
    val mem_control_signals = in(ControlSignals())
    val wb_control_signals = in(ControlSignals())

    val take_branch = in Bool()

    val use_mem_src1 = out Bool()
    val use_mem_src2 = out Bool()
    val use_wb_src1 = out Bool()
    val use_wb_src2 = out Bool()

    val if_stall = out Bool()

    val id1_stall = out Bool()
    val id1_flush = out Bool()

    val id2_stall = out Bool()
    val id2_flush = out Bool()

    val ex_stall = out Bool()
    val ex_flush = out Bool()

    val mem_stall = out Bool()
    val mem_flush = out Bool()

    val wb_stall = out Bool()
    val wb_flush = out Bool()
  }

  // TODO check valid_1d for if and mem, if they are false, try_flush id and wb

  // Stall on data hazard
  val ex_rd_x0 = io.ex_control_signals.rd === 0
  val raw_ex_src1 = io.id2_control_signals.rs1 === io.ex_control_signals.rd && !ex_rd_x0
  val raw_ex_src2 = io.id2_control_signals.rs2 === io.ex_control_signals.rd && !ex_rd_x0

  val mem_rd_x0 = io.mem_control_signals.rd === 0
  val raw_mem_src1 = io.id2_control_signals.rs1 === io.mem_control_signals.rd && !mem_rd_x0
  val raw_mem_src2 = io.id2_control_signals.rs2 === io.mem_control_signals.rd && !mem_rd_x0

  val raw = (raw_ex_src1 | raw_ex_src2 | raw_mem_src1 | raw_mem_src2) & io.id2_valid

  val halt_if = !io.if_valid | io.id1_stall_if
  val halt_id1 = !io.id2_valid
  val halt_id2 = !io.id2_valid | raw
  val halt_ex = !io.ex_valid
  val halt_mem = !io.mem_valid
  val halt_wb = False

  val branch_flush = io.take_branch

  io.if_stall := False

  io.id1_stall := False
  io.id1_flush := False

  io.id2_stall := False
  io.id2_flush := False

  io.ex_stall := False
  io.ex_flush := False

  io.mem_stall := False
  io.mem_flush := False

  io.wb_stall := False
  io.wb_flush := False

  when(halt_wb) {
    io.if_stall := True
    io.id1_stall := True
    io.id2_stall := True
    io.ex_stall := True
    io.mem_stall := True
    io.wb_stall := True
  }.elsewhen(halt_mem) {
    io.if_stall := True
    io.id1_stall := True
    io.id2_stall := True
    io.ex_stall := True
    io.mem_stall := True
    io.wb_flush := True
  }.elsewhen(halt_ex) {
    io.if_stall := True
    io.id1_stall := True
    io.id2_stall := True
    io.ex_stall := True
    io.mem_flush := io.mem_valid
    when(!io.mem_flush) {
      io.wb_flush := True
    }
  }.elsewhen(branch_flush){
    io.id1_flush := True
    io.id2_flush := True
    io.ex_flush := True
  }.elsewhen(halt_id2) {
    io.if_stall := True
    io.id1_stall := True
    io.id2_stall := True
    io.ex_flush := io.ex_valid & !io.take_branch
    when(!io.ex_flush){
      io.mem_flush := io.mem_valid
      when(!io.mem_flush) {
        io.wb_flush := True
      }
    }
  }.elsewhen(halt_id1) {
    io.if_stall := True
    io.id1_stall := True
    io.id2_flush := io.id2_valid
    when(!io.id2_flush){
      io.ex_flush := io.ex_valid & !io.take_branch
      when(!io.ex_flush) {
        io.mem_flush := io.mem_valid
        when(!io.mem_flush) {
          io.wb_flush := True
        }
      }
    }
  }.elsewhen(halt_if) {
    io.if_stall := True
    io.id1_flush := io.id1_valid
    when(!io.id1_flush){
      io.id2_flush := io.id2_valid
      when(!io.id2_flush) {
        io.ex_flush := io.ex_valid & !io.take_branch
        when(!io.ex_flush) {
          io.mem_flush := io.mem_valid
          when(!io.mem_flush) {
            io.wb_flush := True
          }
        }
      }
    }
  }

  // TODO Enable forwarding
  // TODO If a load instruction is on pipe, it can't be forwarded from MEM
  // io.use_mem_src1 := io.ex_control_signals.rs1 === io.mem_control_signals.rd
  // io.use_mem_src2 := io.ex_control_signals.rs2 === io.mem_control_signals.rd
  // TODO Forward the WB result
  // io.use_wb_src1 := io.ex_control_signals.rs1 === io.wb_control_signals.rd
  // io.use_wb_src2 := io.ex_control_signals.rs2 === io.wb_control_signals.rd

  io.use_mem_src1 := False
  io.use_mem_src2 := False
  io.use_wb_src1 := False
  io.use_wb_src2 := False
}

object HazardUnit extends App {
  Config.spinal.generateVerilog(HazardUnit())
}