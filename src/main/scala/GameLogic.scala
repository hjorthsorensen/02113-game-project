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

  val beerSpeed = 0.U(8.W)

  val gameLogic = Module(new PlayerMovementFSM())

  beerSpeed := gameLogic.io.beerSpeed

  gameLogic.io.btnC := io.btnC
  gameLogic.io.btnU := io.btnU
  gameLogic.io.btnL := io.btnL
  gameLogic.io.btnR := io.btnR
  gameLogic.io.btnD := io.btnD

  io.spriteXPosition(0) := gameLogic.io.spriteXPosition
  io.spriteXPosition(1) := gameLogic.io.spriteXPosition
  io.spriteXPosition(2) := gameLogic.io.spriteXPosition
  io.spriteXPosition(3) := gameLogic.io.spriteXPosition

  io.spriteYPosition(0) := gameLogic.io.spriteYPosition
  io.spriteYPosition(1) := gameLogic.io.spriteYPosition
  io.spriteYPosition(2) := gameLogic.io.spriteYPosition
  io.spriteYPosition(3) := gameLogic.io.spriteYPosition

  io.spriteFlipHorizontal(0) := gameLogic.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(1) := gameLogic.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(2) := gameLogic.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(3) := gameLogic.io.spriteFlipHorizontal

  io.spriteFlipVertical(0) := gameLogic.io.spriteFlipVertical
  io.spriteFlipVertical(1) := gameLogic.io.spriteFlipVertical
  io.spriteFlipVertical(2) := gameLogic.io.spriteFlipVertical
  io.spriteFlipVertical(3) := gameLogic.io.spriteFlipVertical

  switch(gameLogic.io.spriteAnimationFrame) {
    is (0.U) {
      io.spriteVisible(0) := true.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(3) := false.B
    }
    is (1.U) {
      io.spriteVisible(1) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(3) := false.B
    }
    is (2.U) {
      io.spriteVisible(2) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(3) := false.B

    }
    is (3.U) {
      io.spriteVisible(3) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B

    }
  }

  when(io.newFrame) {
    gameLogic.io.work := true.B
  }

  when(gameLogic.io.done) {
    io.frameUpdateDone := true.B

  }

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
