//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// Wrapper for verilog single port write first memory with initialization
// implemented as Verilog back box.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util.HasBlackBoxResource


class RamInitSpWf(dWidth: Int, addrWidth: Int, loadFile: String) extends BlackBox(Map(
  "ADDR_WIDTH" -> addrWidth,
  "DATA_WIDTH" -> dWidth,
  "LOAD_FILE" -> loadFile)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val we = Input(Bool())
    val en = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val di = Input(UInt(dWidth.W))
    val dout = Output(UInt(dWidth.W))
  })
  addResource("/RamInitSpWf.v")
}


class RamSpWf(dWidth: Int, addrWidth: Int) extends BlackBox(Map(
  "ADDR_WIDTH" -> addrWidth,
  "DATA_WIDTH" -> dWidth)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val we = Input(Bool())
    val en = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val di = Input(UInt(dWidth.W))
    val dout = Output(UInt(dWidth.W))
  })
  addResource("/RamSpWf.v")
}


class Memory(dataSize: Int, addressSize: Int, loadfile: String) extends Module {
  //Constructor overload
  def this(dataSize: Int, addressSize: Int) = this(dataSize, addressSize, "" )

  val io = IO(new Bundle {
    val address = Input(UInt(addressSize.W))
    val dataRead = Output(UInt(dataSize.W))
    val enable = Input(Bool())
    val writeEnable = Input(Bool())
    val dataWrite = Input(UInt(dataSize.W))
  })

  if (loadfile == ""){
    val ramsSpWf = Module(new RamSpWf(dataSize, addressSize))
    ramsSpWf.io.clk := clock
    ramsSpWf.io.we := io.writeEnable
    ramsSpWf.io.en := io.enable
    ramsSpWf.io.addr := io.address
    ramsSpWf.io.di := io.dataWrite
    io.dataRead := ramsSpWf.io.dout
  } else {
    val ramsSpWf = Module(new RamInitSpWf(dataSize, addressSize, loadfile))
    ramsSpWf.io.clk := clock
    ramsSpWf.io.we := io.writeEnable
    ramsSpWf.io.en := io.enable
    ramsSpWf.io.addr := io.address
    ramsSpWf.io.di := io.dataWrite
    io.dataRead := ramsSpWf.io.dout
  }

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
