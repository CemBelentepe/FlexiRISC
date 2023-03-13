package flexirisc

import flexirisc.pipeline._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Core() extends Component {
  val io = new Bundle {
    val inst_mem = master(MemoryPort(32, 32))
    val data_mem = master(MemoryPort(32, 32))
  }

  val hazardUnit = HazardUnit()

  val ifStage = FetchStage()
  val idStage = DecodeStage()
  val exStage = ExecuteStage()
  val memStage = MemoryStage()
  val wbStage = WriteBackStage()

  val ifid = IfId()
  val idex = IdEx()
  val exmem = ExMem()
  val memwb = MemWb()

  // Hazard
  hazardUnit.io.if_valid := ifStage.io.stage_valid
  hazardUnit.io.id_valid := True
  hazardUnit.io.ex_valid := True
  hazardUnit.io.mem_valid := memStage.io.stage_valid

  hazardUnit.io.id_control_signals := idStage.io.control_signals
  hazardUnit.io.ex_control_signals := idex.io.ex_control_signals
  hazardUnit.io.mem_control_signals := exmem.io.mem_control_signals
  hazardUnit.io.wb_control_signals := memwb.io.wb_control_signals
  hazardUnit.io.take_branch := exStage.io.jump_enable

  // IF
  ifStage.io.jump_enable := exStage.io.jump_enable
  ifStage.io.jump_address := exStage.io.jump_address
  ifStage.io.halt_stage := hazardUnit.io.if_stall
  io.inst_mem <> ifStage.io.inst_mem

  ifid.io.id_stall := hazardUnit.io.id_stall
  ifid.io.id_flush := hazardUnit.io.id_flush

  ifid.io.if_instruction := ifStage.io.instruction
  ifid.io.if_pc := ifStage.io.pc
  ifid.io.if_pc_next_seq := ifStage.io.pc_next_seq

  // ID
  idStage.io.instruction := ifid.io.id_instruction
  idStage.io.wb_rd := wbStage.io.rd
  idStage.io.wb_data := wbStage.io.data

  idex.io.ex_stall := hazardUnit.io.ex_stall
  idex.io.ex_flush := hazardUnit.io.ex_flush

  idex.io.id_control_signals := idStage.io.control_signals
  idex.io.id_src1 := idStage.io.src1
  idex.io.id_src2 := idStage.io.src2
  idex.io.id_immediate := idStage.io.immediate
  idex.io.id_pc := ifid.io.id_pc
  idex.io.id_pc_next_seq := ifid.io.id_pc_next_seq

  // EX
  exStage.io.control_signals := idex.io.ex_control_signals
  exStage.io.pc := idex.io.ex_pc
  exStage.io.pc_next_seq := idex.io.ex_pc_next_seq
  exStage.io.id_src1 := idex.io.ex_src1
  exStage.io.id_src2 := idex.io.ex_src2
  exStage.io.immediate := idex.io.ex_immediate
  exStage.io.mem_res := exmem.io.mem_result
  exStage.io.wb_res := memwb.io.wb_result_data

  exStage.io.use_mem_src1 := hazardUnit.io.use_mem_src1
  exStage.io.use_mem_src2 := hazardUnit.io.use_mem_src2
  exStage.io.use_wb_src1 := hazardUnit.io.use_wb_src1
  exStage.io.use_wb_src2 := hazardUnit.io.use_wb_src2

  exmem.io.mem_flush := hazardUnit.io.mem_flush
  exmem.io.mem_stall := hazardUnit.io.mem_stall

  exmem.io.ex_control_signals := idex.io.ex_control_signals
  exmem.io.ex_result := exStage.io.result
  exmem.io.ex_src2 := exStage.io.src2

  // MEM
  memStage.io.control_signals := exmem.io.mem_control_signals
  memStage.io.ex_result_data := exmem.io.mem_result
  memStage.io.src2 := exmem.io.mem_src2
  io.data_mem <> memStage.io.data_mem

  memwb.io.wb_stall := hazardUnit.io.wb_stall
  memwb.io.wb_flush := hazardUnit.io.wb_flush

  memwb.io.mem_control_signals := exmem.io.mem_control_signals
  memwb.io.mem_load_data := memStage.io.load_data
  memwb.io.mem_result_data := memStage.io.result_data

  // WB
  wbStage.io.control_signals := memwb.io.wb_control_signals
  wbStage.io.wb_load_data := memwb.io.wb_load_data
  wbStage.io.wb_result_data := memwb.io.wb_result_data
}

object Core extends App {
  Config.spinal.generateVerilog(Core())
}