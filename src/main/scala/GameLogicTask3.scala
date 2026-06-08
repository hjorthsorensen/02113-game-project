//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// This file contains the game logic. Implement yours here.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class GameLogicTask3(SpriteNumber: Int, BackTileNumber: Int) extends Module {
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

  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  //Two registers holding the sprite sprite X and Y with the sprite initial position
  val sprite0XReg = RegInit(32.S(11.W))
  val sprite0YReg = RegInit((360-32).S(10.W))

  val sprite1XReg = RegInit(500.S(11.W))
  val sprite1YReg = RegInit((180-32).S(10.W))
  val sprite2XReg = RegInit(32.S(11.W))
  val sprite2YReg = RegInit((360-32).S(10.W))

  //A registers holding the sprite horizontal flip
  val sprite0FlipHorizontalReg = RegInit(false.B)

  val sprite1FlipHorizontalReg = RegInit(false.B)
  val sprite2FlipHorizontalReg = RegInit(false.B)

  val sprite0VisibleReg = RegInit(true.B)

  //Making sprite 0 visible and sprite 5 inverse of 0
  io.spriteVisible(0) := sprite0VisibleReg
  io.spriteVisible(5) := !sprite0VisibleReg
  //Making sprite 1 visible
  io.spriteVisible(1) := true.B
  //Making sprite 2 visible
  io.spriteVisible(2) := true.B

  //Connecting resiters to the graphic engine
  // Sprite 5 is locked to sprite 0
  io.spriteXPosition(0) := sprite0XReg
  io.spriteYPosition(0) := sprite0YReg
  io.spriteXPosition(5) := sprite0XReg
  io.spriteYPosition(5) := sprite0YReg

  io.spriteXPosition(1) := sprite1XReg
  io.spriteYPosition(1) := sprite1YReg

  io.spriteXPosition(2) := sprite2XReg
  io.spriteYPosition(2) := sprite2YReg

  //Sprite 5 same flip as sprite 0
  io.spriteFlipHorizontal(0) := sprite0FlipHorizontalReg
  io.spriteFlipHorizontal(5) := sprite0FlipHorizontalReg
  
  io.spriteFlipHorizontal(1) := sprite1FlipHorizontalReg
  io.spriteFlipHorizontal(2) := sprite2FlipHorizontalReg

  //Counting frames:
  val cntReg = RegInit(0.U(8.W))//8wide = 255frames
  val fishReg = RegInit(0.U(8.W))//8wide = 255frames

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
      }
    }

    is(compute1) {
      when(io.btnD){
        when(sprite0YReg < (480 - 32 - 24).S) {
          sprite0YReg := sprite0YReg + 2.S
        }
      } .elsewhen(io.btnU){
        when(sprite0YReg > (96).S) {
          sprite0YReg := sprite0YReg - 2.S
        }
      }
      when(io.btnR) {
        when(sprite0XReg < (640 - 32 - 32).S) {
          sprite0XReg := sprite0XReg + 2.S
          sprite0FlipHorizontalReg := false.B
        }
      } .elsewhen(io.btnL){
        when(sprite0XReg > 32.S) {
          sprite0XReg := sprite0XReg - 2.S
          sprite0FlipHorizontalReg := true.B
        }
      }
      when(fishReg >= 30.U){
        sprite0VisibleReg := !sprite0VisibleReg
        fishReg := 0.U
      }
      when(cntReg >= 30.U){
        cntReg := 0.U
        sprite1FlipHorizontalReg := !sprite1FlipHorizontalReg
      }
      
      stateReg := done
    }

    is(done) {
      when(io.btnD){
        fishReg := fishReg + 1.U
      }.elsewhen(io.btnR){
        fishReg := fishReg + 1.U
      }.elsewhen(io.btnU){
        fishReg := fishReg + 1.U
      }.elsewhen(io.btnL){
        fishReg := fishReg + 1.U
      }
      cntReg := cntReg + 1.U
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }





}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
