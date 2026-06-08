//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// The top of the architecture. It just contains reset synchronization and
// negation (if needed). It also make sure that reset last at
// least RESET_MIN_DURATION_CC.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class Top extends Module {
  val io = IO(new Bundle {
    //Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    //VGA Connector
    val vgaRed = Output(UInt(4.W))
    val vgaGreen = Output(UInt(4.W))
    val vgaBlue = Output(UInt(4.W))
    val Hsync = Output(Bool())
    val Vsync = Output(Bool())

    //Switches
    val  sw = Input(Vec(8, Bool()))

    //Leds
    val led = Output(Vec(8, Bool()))

    //Errors
    val missingFrameError = Output(Bool())
    val backBufferWriteError = Output(Bool())
    val viewBoxOutOfRangeError = Output(Bool())
  })

  val gameTop = Module(new GameTop())

  //Reset syncronization (and negation if needed)
  //Reset is active high in Chisel (reset = 1 means reset, reset = 0 means run)
  //Uncomment one of the following lines to select the reset mode of your FPGA pin
  val syncResetInput = ~RegNext(RegNext(RegNext(reset.asBool))) //Active low (e.g., for reset button in Nexys A7 or switches in Basys3 and Nexys A7)
  //val syncResetInput = RegNext(RegNext(RegNext(reset.asBool))) //Active high (e.g., for buttons in Basys3 and Nexys A7)

  //Make sure that reset last at least RESET_MIN_DURATION_CC
  val RESET_MIN_DURATION_CC = 5
  val pipeResetReg = Reg(Vec(RESET_MIN_DURATION_CC, Bool()))
  pipeResetReg(RESET_MIN_DURATION_CC - 1) := syncResetInput
  for (i <- 0 until RESET_MIN_DURATION_CC - 1) {
    pipeResetReg(i) := pipeResetReg(i + 1)
  }
  gameTop.reset := pipeResetReg.asUInt.orR

  // Forwards all IOs
  gameTop.io <> io

}

////////////////////////////////////////////////////////////
// An object extending App to generate the Verilog code.
////////////////////////////////////////////////////////////
object Top extends App {
  println("Processing Top, I will now generate the Verilog file!")
  emitVerilog(new Top())
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
