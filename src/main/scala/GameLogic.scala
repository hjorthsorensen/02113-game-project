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
    // Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    // Switches
    val sw = Input(Vec(8, Bool()))

    // Leds
    val led = Output(Vec(8, Bool()))

    // GraphicEngineVGA
    // Sprite control input
    val spriteXPosition = Output(Vec(SpriteNumber, SInt(11.W))) // -1024 to 1023
    val spriteYPosition = Output(Vec(SpriteNumber, SInt(10.W))) // -512 to 511
    val spriteVisible = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipHorizontal = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipVertical = Output(Vec(SpriteNumber, Bool()))

    // Viewbox control output
    val viewBoxX = Output(UInt(10.W)) // 0 to 640
    val viewBoxY = Output(UInt(9.W)) // 0 to 480

    // Background buffer output
    val backBufferWriteData = Output(UInt(log2Up(BackTileNumber).W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())

    // Status
    val newFrame = Input(Bool())
    val frameUpdateDone = Output(Bool())
  })

  // Setting all led outputs to zero
  // It can be done by the single expression below...
  io.led := Seq.fill(8)(false.B)

  // Setting all sprite control outputs to zero
  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteVisible := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  // Setting the viewbox control outputs to zero
  io.viewBoxX := 0.U
  io.viewBoxY := 0.U

  // Setting the background buffer outputs to zero
  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  // Setting frame done to zero
  io.frameUpdateDone := false.B

  /////////////////////////////////////////////////////////////////
  // Write here your game logic
  // (you might need to change the initialization values above)
  /////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////
  /////FSM modules instantiation and connections
  /////////////////////////////////////////////////////////////////////////

  val playerMovementFSM = Module(new PlayerMovementFSM())
  val beerMovement = Module(new BeerMovement())
  beerMovement.io.work := false.B
  beerMovement.io.speed := playerMovementFSM.io.beerSpeed
  beerMovement.io.beerYPosInp := playerMovementFSM.io.spriteYPosition

  playerMovementFSM.io.beerReady := beerMovement.io.beerReady


  val scoreFSM = Module(new ScoreFSM())
  scoreFSM.io.beerPositionX := beerMovement.io.beerXPos
  scoreFSM.io.beerPositionY := beerMovement.io.beerYPos
  scoreFSM.io.beerValid := beerMovement.io.beerValid
  scoreFSM.io.work := false.B
  io.led(0) := scoreFSM.io.customerOneScored
  io.led(1) := scoreFSM.io.customerTwoScored

  val spawnCustomer = Module(new SpawnCustomer())
  spawnCustomer.io.customer1Scored := scoreFSM.io.customerOneScored
  spawnCustomer.io.customer2Scored := scoreFSM.io.customerTwoScored

  spawnCustomer.io.work := false.B


  scoreFSM.io.customerOnePositionX := spawnCustomer.io.customer1PosX
  scoreFSM.io.customerOnePositionY := spawnCustomer.io.customer1PosY
  scoreFSM.io.customerTwoPositionX := spawnCustomer.io.customer2PosX
  scoreFSM.io.customerTwoPositionY := spawnCustomer.io.customer2PosY

  playerMovementFSM.io.work := false.B

  playerMovementFSM.io.btnC := io.btnC
  playerMovementFSM.io.btnU := io.btnU
  playerMovementFSM.io.btnL := io.btnL
  playerMovementFSM.io.btnR := io.btnR
  playerMovementFSM.io.btnD := io.btnD

  /////////////////////////////////////////////////////////////////////////
  /////Positional and visibility logic for sprites
  /////////////////////////////////////////////////////////////////////////

  io.spriteXPosition(0) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(1) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(2) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(3) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(4) := spawnCustomer.io.customer1PosX
  io.spriteXPosition(5) := spawnCustomer.io.customer1PosX
  io.spriteXPosition(6) := spawnCustomer.io.customer2PosX
  io.spriteXPosition(7) := spawnCustomer.io.customer2PosX
  io.spriteXPosition(8) := beerMovement.io.beerXPos


  io.spriteYPosition(0) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(1) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(2) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(3) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(4) := spawnCustomer.io.customer1PosY
  io.spriteYPosition(5) := spawnCustomer.io.customer1PosY
  io.spriteYPosition(6) := spawnCustomer.io.customer2PosY
  io.spriteYPosition(7) := spawnCustomer.io.customer2PosY
  io.spriteYPosition(8) := beerMovement.io.beerYPos


  io.spriteFlipHorizontal(0) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(1) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(2) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(3) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(4) := spawnCustomer.io.customer1Flipped
  io.spriteFlipHorizontal(5) := spawnCustomer.io.customer2Flipped

  io.spriteFlipVertical(0) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(1) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(2) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(3) := playerMovementFSM.io.spriteFlipVertical

  io.spriteVisible(4) := spawnCustomer.io.customer1IdleVisible
  io.spriteVisible(5) := spawnCustomer.io.customer1DrinkingVisible
  io.spriteVisible(6) := spawnCustomer.io.customer2IdleVisible
  io.spriteVisible(7) := spawnCustomer.io.customer2DrinkingVisible
  io.spriteVisible(8) := beerMovement.io.beerVisible


  /////////////////////////////////////////////////////////////////////////
  /////Player animation logic
  /////////////////////////////////////////////////////////////////////////
  switch(playerMovementFSM.io.spriteAnimationFrame) {
    is(0.U) {
      io.spriteVisible(0) := true.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(11) := false.B
    }
    is(1.U) {
      io.spriteVisible(1) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(11) := false.B
    }
    is(2.U) {
      io.spriteVisible(2) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(11) := false.B

    }
    is(3.U) {
      io.spriteVisible(11) := true.B
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B

    }
  }
  /////////////////////////////////////////////////////////////////////////
  /////BACKGROUND LOGIC
  /////////////////////////////////////////////////////////////////////////

  val backgroundHandler = Module(new BackgroundHandler())
  //Scoreboard FSM
  val scoreBoardFSM = Module(new ScoreBoardDisplayFSM())
  //Default assignment connections for background handler 
  io.backBufferWriteAddress := backgroundHandler.io.writeAdress
  io.backBufferWriteData := backgroundHandler.io.writeTileID
  io.backBufferWriteEnable := backgroundHandler.io.writeEnable

  //Score specefic signals - Add signals here for any additional background updates needed after the scoreboard is done
  scoreBoardFSM.io.score := scoreFSM.io.score
  scoreBoardFSM.io.work := backgroundHandler.io.scoreWork
  backgroundHandler.io.scoreDone := scoreBoardFSM.io.done

  //Default .io connections for background handler
  backgroundHandler.io.work := false.B
  backgroundHandler.io.inputAdress := 0.U
  backgroundHandler.io.inputTileID := 0.U

  //Conditionally assigns write address and tileID to the backgroundHandler
  when(scoreBoardFSM.io.writingScore) {
    backgroundHandler.io.inputAdress := scoreBoardFSM.io.writeAdress
    backgroundHandler.io.inputTileID := scoreBoardFSM.io.writeTileID
  }//add .elsewhen if you want to write other things to the background as well


  

  /////////////////////////////////////////////////////////////////////////
  /////DONE SIGNALS AND FSMD FOR FRAME UPDATE
  /////////////////////////////////////////////////////////////////////////
  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  val playerDoneReg = RegInit(false.B)
  val beerDoneReg = RegInit(false.B)
  val scoreFSMDoneReg = RegInit(false.B)
  val spawnCustomerReg = RegInit(false.B)
  val backgroundDoneReg = RegInit(false.B)

  
  switch(stateReg){
    is(idle){
      when(io.newFrame){
        stateReg := compute1
        playerMovementFSM.io.work := true.B
        beerMovement.io.work := true.B
        scoreFSM.io.work := true.B
        spawnCustomer.io.work := true.B
        backgroundHandler.io.work := true.B

        playerDoneReg := false.B
        beerDoneReg := false.B
        scoreFSMDoneReg := false.B
        spawnCustomerReg := false.B
        backgroundDoneReg := false.B
      }
    }
    is(compute1){
      when(playerMovementFSM.io.done) {
        playerDoneReg := true.B
      }
      when(beerMovement.io.done) {
        beerDoneReg := true.B
      }
      when(scoreFSM.io.done) {
        scoreFSMDoneReg := true.B
      }
      when(spawnCustomer.io.done) {
        spawnCustomerReg := true.B
      }

      when(backgroundHandler.io.done) {
        backgroundDoneReg := true.B
      }

      when(playerDoneReg && beerDoneReg && scoreFSMDoneReg && spawnCustomerReg && backgroundDoneReg) {
        stateReg := done
      }
    }
    is(done){
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
