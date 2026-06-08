//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// This file contains the game logic. Implement yours here.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class GameLogic(SpriteNumber: Int, BackTileNumber: Int) extends Module {
  val io = IO(new Bundle {
    //Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    //Switches
    val sw = Input(Vec(8, Bool()))

    //Leds
    val led = Output(Vec(8, Bool()))

    //GraphicEngineVGA
    //Sprite control input
    val spriteXPosition = Output(Vec(SpriteNumber, SInt(11.W))) //-1024 to 1023
    val spriteYPosition = Output(Vec(SpriteNumber, SInt(10.W))) //-512 to 511
    val spriteVisible = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipHorizontal = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipVertical = Output(Vec(SpriteNumber, Bool()))

    //Viewbox control output
    val viewBoxX = Output(UInt(10.W)) //0 to 640
    val viewBoxY = Output(UInt(9.W)) //0 to 480

    //Background buffer output
    val backBufferWriteData = Output(UInt(log2Up(BackTileNumber).W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())

    //Status
    val newFrame = Input(Bool())
    val frameUpdateDone = Output(Bool())
  })

  // Setting all led outputs to zero
  // It can be done by the single expression below...
  io.led := Seq.fill(8)(false.B)

  // Or one by one...
  //io.led(0) := false.B
  //io.led(0) := false.B
  //io.led(1) := false.B
  //io.led(2) := false.B
  //io.led(3) := false.B
  //io.led(4) := false.B
  //io.led(5) := false.B
  //io.led(6) := false.B
  //io.led(7) := false.B

  // Or with a for loop.
  //for (i <- 0 until 8) {
  //  io.led(i) := false.B
  //}

  //Setting all sprite control outputs to zero
  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteVisible := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  //Setting the viewbox control outputs to zero
  io.viewBoxX := 0.U
  io.viewBoxY := 0.U

  //Setting the background buffer outputs to zero
  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  //Setting frame done to zero
  io.frameUpdateDone := false.B

  /////////////////////////////////////////////////////////////////
  // Write here your game logic
  // (you might need to change the initialization values above)
  /////////////////////////////////////////////////////////////////

  val idle :: inputCalc :: compute2 :: done :: Nil = Enum(4)
  val stateReg = RegInit(idle)

  def risingEdge(signal: Bool): Bool = signal && !RegNext(signal)
  def fallingEdge(signal: Bool):Bool = !signal && RegNext(signal)

  val btnCRising = risingEdge(btnC) && !throwing && !holding
  val btnCFalling = fallingEdge(btnC)

  val holding = RegInit(false.B)
  val throwing = RegInit(false.B)
  

  val speed = RegInit(0.U(8.W))
  val fpsReg = RegInit(0.U(8.W))
  

  val beerXReg = RegInit(32.S(11.W))
  val beerYReg = RegInit((360-32).S(11.W))
  
  val tickThrow = (fpsReg XOR 20.U) && (fpsReg XOR 40.U) && (fpsReg XOR 60.U)

  



  
  
  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := inputCalc
      }
    }

    is(inputCalc) {
      when(btnCRising){
        holding := true.B
      }
      when(btnCFalling){
        holding := false.B
        throwing := true.B
      }
      

      when(io.btnU) {
        
      }
      when(io.btnD) {
        
      }
      when(io.btnL) {
        
      }
      when(io.btnR) {
        
      }
      stateReg := compute2
    }
    is(compute2){
      when(holding && tickThrow){
        speed := speed + 1.U
      }
      when(throwing){
        speed := speed - 1.U
        beerXReg := beerXReg + speed
      }
      when(speed === 0.U){
        throwing := false.B
        beerXReg := 32.S
        beerYReg := (360-32).S

      }
      when(fpsReg === 60.U){
        fpsReg := 0.U
      }

      stateReg := done
    }
    is(done) {
      fpsReg := fpsReg + 1.U
      io.newFrame := true.B
      stateReg := idle
    }
  }



  // Just forwarding the newFrame into the frameUpdateDone with a 2 clock cycle delay
  // frameUpdateDone will need to be driven by your game logic FSMs
  io.frameUpdateDone := RegNext(RegNext(io.newFrame))

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
