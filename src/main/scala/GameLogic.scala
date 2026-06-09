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

  val beerSpeed = WireDefault(0.S(8.W))

  val playerMovementFSM = Module(new PlayerMovementFSM())
  val beerMovement = Module(new BeerMovement())
  beerSpeed := playerMovementFSM.io.beerSpeed
  beerMovement.io.speed := beerSpeed
  playerMovementFSM.io.beerReady := beerMovement.io.beerReady
  
  val scoreFSM = Module(new ScoreFSM())
  scoreFSM.io.beerPositionX := beerMovement.io.beerXPos
  scoreFSM.io.beerPositionY := beerMovement.io.beerYPos
  scoreFSM.io.beerValid := beerMovement.io.beerValid

  val customerOnePositionX = RegInit(0.S(11.W))
  val customerOnePositionY = RegInit(0.S(10.W))
  val customerTwoPositionX = RegInit(0.S(11.W))
  val customerTwoPositionY = RegInit(0.S(10.W))

  scoreFSM.io.customerOnePositionX := customerOnePositionX
  scoreFSM.io.customerOnePositionY := customerOnePositionY
  scoreFSM.io.customerTwoPositionX := customerTwoPositionX
  scoreFSM.io.customerTwoPositionY := customerTwoPositionY

  

  playerMovementFSM.io.btnC := io.btnC
  playerMovementFSM.io.btnU := io.btnU
  playerMovementFSM.io.btnL := io.btnL
  playerMovementFSM.io.btnR := io.btnR
  playerMovementFSM.io.btnD := io.btnD

  io.spriteXPosition(0) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(1) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(2) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(3) := playerMovementFSM.io.spriteXPosition

  io.spriteYPosition(0) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(1) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(2) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(3) := playerMovementFSM.io.spriteYPosition

  io.spriteFlipHorizontal(0) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(1) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(2) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(3) := playerMovementFSM.io.spriteFlipHorizontal

  io.spriteFlipVertical(0) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(1) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(2) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(3) := playerMovementFSM.io.spriteFlipVertical

  switch(playerMovementFSM.io.spriteAnimationFrame) {
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
    playerMovementFSM.io.work := true.B
    beerMovement.io.work := true.B
    scoreFSM.io.work := true.B

    playerDoneReg := false.B
    beerDoneReg := false.B
    scoreFSMDoneReg := false.B

  }
  val playerDoneReg = RegInit(false.B)
  val beerDoneReg = RegInit(false.B)
  val scoreFSMDoneReg = RegInit(false.B)

  when(playerMovementFSM.io.done){
    playerDoneReg := true.B
  }
  when(beerMovement.io.done){
    beerDoneReg := true.B
  }
  when(scoreFSM.io.done){
    scoreFSMDoneReg := true.B
  }


  when(playerDoneReg && beerDoneReg && scoreFSMDoneReg) {
    io.frameUpdateDone := true.B
  }

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
