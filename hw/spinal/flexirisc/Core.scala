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
  val id1Stage = PreDecodeStage()
  val id2Stage = DecodeStage()
  val exStage = ExecuteStage()
  val memStage = MemoryStage()
  val wbStage = WriteBackStage()

  val ifLoad = StoreLoadUnit(32, 32)
  val memLoad = StoreLoadUnit(32, 32)

  val ifid1 = IfId1()
  val id1id2 = Id1Id2()
  val idex = IdEx()
  val exmem = ExMem()
  val memwb = MemWb()

  // Hazard
  hazardUnit.io.if_valid := True
  hazardUnit.io.id1_stall_if := id1Stage.io.stall_if
  hazardUnit.io.id1_valid := id1Stage.io.stage_valid
  hazardUnit.io.id2_valid := True
  hazardUnit.io.ex_valid := exStage.io.stage_valid
  hazardUnit.io.mem_valid := memStage.io.stage_valid
  hazardUnit.io.wb_valid := wbStage.io.stage_valid

  hazardUnit.io.id2_control_signals := id2Stage.io.control_signals
  hazardUnit.io.ex_control_signals := idex.io.ex_control_signals
  hazardUnit.io.mem_control_signals := exmem.io.mem_control_signals
  hazardUnit.io.wb_control_signals := memwb.io.wb_control_signals
  hazardUnit.io.take_branch := exStage.io.jump_enable

  // IF Load
  ifLoad.io.address := ifStage.io.pc
  ifLoad.io.request := ifStage.io.request
  ifLoad.io.write_mask := B"4'b0000"
  ifLoad.io.write_data := 0
  ifLoad.io.flush := hazardUnit.io.id1_flush

  // IF
  ifStage.io.jump_enable := exStage.io.jump_enable
  ifStage.io.jump_address := exStage.io.jump_address
  ifStage.io.halt_stage := hazardUnit.io.if_stall
  io.inst_mem <> ifLoad.io.mem_port

  ifid1.io.id1_stall := hazardUnit.io.id1_stall
  ifid1.io.id1_flush := hazardUnit.io.id1_flush

  ifid1.io.if_pc := ifStage.io.pc
  ifid1.io.if_pc_next_seq := ifStage.io.pc_next_seq

  // ID1
  id1Stage.io.instruction := ifLoad.io.data
  id1Stage.io.inst_valid := ifLoad.io.valid

  id1id2.io.id2_stall := hazardUnit.io.id2_stall
  id1id2.io.id2_flush := hazardUnit.io.id2_flush

  id1id2.io.id1_instruction_dbg := ifLoad.io.data
  id1id2.io.id1_opcode := id1Stage.io.opcode
  id1id2.io.id1_rs1 := id1Stage.io.rs1
  id1id2.io.id1_rs2 := id1Stage.io.rs2
  id1id2.io.id1_rd := id1Stage.io.rd
  id1id2.io.id1_funct3 := id1Stage.io.funct3
  id1id2.io.id1_funct7 := id1Stage.io.funct7
  id1id2.io.id1_immediate := id1Stage.io.immediate
  id1id2.io.id1_pc := ifid1.io.id1_pc
  id1id2.io.id1_pc_next_seq := ifid1.io.id1_pc_next_seq

  // ID2
  id2Stage.io.opcode := id1id2.io.id2_opcode
  id2Stage.io.rs1 := id1id2.io.id2_rs1
  id2Stage.io.rs2 := id1id2.io.id2_rs2
  id2Stage.io.rd := id1id2.io.id2_rd
  id2Stage.io.funct3 := id1id2.io.id2_funct3
  id2Stage.io.funct7 := id1id2.io.id2_funct7

  id2Stage.io.wb_rd := wbStage.io.rd
  id2Stage.io.wb_data := wbStage.io.data

  id2Stage.io.ex_result := exStage.io.result
  id2Stage.io.mem_result := memStage.io.result_data

  id2Stage.io.use_ex_src1 := hazardUnit.io.use_ex_src1
  id2Stage.io.use_ex_src2 := hazardUnit.io.use_ex_src2
  id2Stage.io.use_mem_src1 := hazardUnit.io.use_mem_src1
  id2Stage.io.use_mem_src2 := hazardUnit.io.use_mem_src2

  idex.io.ex_stall := hazardUnit.io.ex_stall
  idex.io.ex_flush := hazardUnit.io.ex_flush

  idex.io.id2_instruction_dbg := id1id2.io.id2_instruction_dbg
  idex.io.id2_control_signals := id2Stage.io.control_signals
  idex.io.id2_src1 := id2Stage.io.src1
  idex.io.id2_src2 := id2Stage.io.src2
  idex.io.id2_immediate := id1id2.io.id2_immediate
  idex.io.id2_pc := id1id2.io.id2_pc
  idex.io.id2_pc_next_seq := id1id2.io.id2_pc_next_seq

  // EX
  exStage.io.control_signals := idex.io.ex_control_signals
  exStage.io.pc := idex.io.ex_pc
  exStage.io.pc_next_seq := idex.io.ex_pc_next_seq
  exStage.io.id_src1 := idex.io.ex_src1
  exStage.io.id_src2 := idex.io.ex_src2
  exStage.io.immediate := idex.io.ex_immediate

  exmem.io.mem_flush := hazardUnit.io.mem_flush
  exmem.io.mem_stall := hazardUnit.io.mem_stall

  exmem.io.ex_control_signals := idex.io.ex_control_signals
  exmem.io.ex_result := exStage.io.result
  exmem.io.ex_src2 := exStage.io.src2

  // MEM
  memStage.io.control_signals := exmem.io.mem_control_signals
  memStage.io.ex_result_data := exmem.io.mem_result
  memStage.io.src2 := exmem.io.mem_src2
  io.data_mem <> memLoad.io.mem_port

  memwb.io.wb_stall := hazardUnit.io.wb_stall
  memwb.io.wb_flush := hazardUnit.io.wb_flush

  memwb.io.mem_control_signals := exmem.io.mem_control_signals
  memwb.io.mem_result_data := memStage.io.result_data

  // MEM Load
  memLoad.io.address := exmem.io.mem_result.asUInt
  memLoad.io.request := memStage.io.request
  memLoad.io.write_mask := memStage.io.write_mask
  memLoad.io.write_data := memStage.io.write_data
  memLoad.io.flush := hazardUnit.io.wb_flush

  // WB
  wbStage.io.control_signals := memwb.io.wb_control_signals
  wbStage.io.wb_load_data := memLoad.io.data
  wbStage.io.wb_load_valid := memLoad.io.valid
  wbStage.io.wb_result_data := memwb.io.wb_result_data
}

object Core extends App {
  Config.spinal.generateVerilog(Core())
}