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

  // Setting frame done to zero
  io.frameUpdateDone := false.B

  // Default assignment connections for background handler

  /////////////////////////////////////////////////////////////////
  // Write here your game logic
  // (you might need to change the initialization values above)
  /////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////
  ///// FSM modules instantiation
  /////////////////////////////////////////////////////////////////////////

  val playerMovementFSM = Module(new PlayerMovementFSM())
  val beerMovement = Module(new BeerMovement())
  val scoreFSM = Module(new ScoreFSM())
  val spawnCustomer = Module(new SpawnCustomer2(16,2))
  val backgroundHandler = Module(new BackgroundHandler())
  val scoreBoardFSM = Module(new ScoreBoardDisplayFSM())
  val returnBeerFSM = Module(new ReturnBeerFSM())
  val brokenGlassFSM = Module(new BrokenGlassDisplayFSM())
  val beerLeftFSM = Module(new beerLeftFSM())

  /////////////////////////////////////////////////////////////////////////
  ///// FSM modules connections
  /////////////////////////////////////////////////////////////////////////

  beerMovement.io.work := false.B
  scoreFSM.io.work := false.B
  spawnCustomer.io.work := false.B
  playerMovementFSM.io.work := false.B
  returnBeerFSM.io.work := false.B

  // Connecting to beer movement
  beerMovement.io.speed := playerMovementFSM.io.beerSpeed
  beerMovement.io.beerYPosInp := playerMovementFSM.io.spriteYPosition

  // Connecting to player movement
  playerMovementFSM.io.beerReady := beerMovement.io.beerReady

  playerMovementFSM.io.btnC := io.btnC
  playerMovementFSM.io.btnU := io.btnU
  playerMovementFSM.io.btnL := io.btnL
  playerMovementFSM.io.btnR := io.btnR
  playerMovementFSM.io.btnD := io.btnD

  // Connecting to score
  scoreFSM.io.beerPositionX := beerMovement.io.beerXPos
  scoreFSM.io.beerPositionY := beerMovement.io.beerYPos
  scoreFSM.io.beerValid := beerMovement.io.beerValid

  scoreFSM.io.customerOneScoredInp := spawnCustomer.io.customer1ScoreDone
  scoreFSM.io.customerTwoScoredInp := spawnCustomer.io.customer2ScoreDone
  scoreFSM.io.customerOnePositionX := spawnCustomer.io.customer1PosX
  scoreFSM.io.customerOnePositionY := spawnCustomer.io.customer1PosY
  scoreFSM.io.customerTwoPositionX := spawnCustomer.io.customer2PosX
  scoreFSM.io.customerTwoPositionY := spawnCustomer.io.customer2PosY

  scoreFSM.io.playerReadyToCatch := playerMovementFSM.io.isCatching
  scoreFSM.io.playerY := playerMovementFSM.io.spriteYPosition
  scoreFSM.io.beerEmptyY := returnBeerFSM.io.returnBeerYPos
  scoreFSM.io.beerEmptyX := returnBeerFSM.io.returnBeerXPos
  scoreFSM.io.emptyBeerValid := returnBeerFSM.io.beerReturnValid
  scoreFSM.io.beerBroken := beerMovement.io.beerBroken

  // Connecting to scoreboard
  scoreBoardFSM.io.score := scoreFSM.io.score
  scoreBoardFSM.io.work := backgroundHandler.io.scoreWork

  // Connecting beer left
  beerLeftFSM.io.score := playerMovementFSM.io.beerLeft
  beerLeftFSM.io.work  := backgroundHandler.io.beerWork

  // Connecting to brokenGlassDisplayFSM
  brokenGlassFSM.io.beerBroken := beerMovement.io.beerBroken
  brokenGlassFSM.io.work := backgroundHandler.io.brokenGlassWork
  brokenGlassFSM.io.tableID := beerMovement.io.tableID

  // Connecting to spawn customer
  spawnCustomer.io.customer1Scored := scoreFSM.io.customerOneScored
  spawnCustomer.io.customer2Scored := scoreFSM.io.customerTwoScored
  spawnCustomer.io.beerDone := beerMovement.io.beerValid
  // Connecting to return beer FSM
  returnBeerFSM.io.customer1XPos := spawnCustomer.io.customer1PosX
  returnBeerFSM.io.customer1YPos := spawnCustomer.io.customer1PosY
  returnBeerFSM.io.customer2XPos := spawnCustomer.io.customer2PosX
  returnBeerFSM.io.customer2YPos := spawnCustomer.io.customer2PosY
  returnBeerFSM.io.returnCustomer1 := spawnCustomer.io.customer1ScoreDone
  returnBeerFSM.io.returnCustomer2 := spawnCustomer.io.customer2ScoreDone
  
  
  returnBeerFSM.io.isBeerCatched := scoreFSM.io.beerCatched

  // Connecting tp background handler
  backgroundHandler.io.work := false.B
  backgroundHandler.io.inputAdress := 0.U
  backgroundHandler.io.inputTileID := 26.U

  backgroundHandler.io.scoreDone := scoreBoardFSM.io.done
  backgroundHandler.io.brokenGlassDone := brokenGlassFSM.io.done
  backgroundHandler.io.beerDone := beerLeftFSM.io.done

  io.backBufferWriteAddress := backgroundHandler.io.writeAdress
  io.backBufferWriteData := backgroundHandler.io.writeTileID
  io.backBufferWriteEnable := backgroundHandler.io.writeEnable

  // Conditionally assigns write address and tileID to the backgroundHandler
  when(scoreBoardFSM.io.writingScore) {
    backgroundHandler.io.inputAdress := scoreBoardFSM.io.writeAdress
    backgroundHandler.io.inputTileID := scoreBoardFSM.io.writeTileID
  }.elsewhen(brokenGlassFSM.io.writingBrokenGlass) {
    backgroundHandler.io.inputAdress := brokenGlassFSM.io.writeAdress
    backgroundHandler.io.inputTileID := brokenGlassFSM.io.writeTileID
  } .elsewhen(beerLeftFSM.io.writingScore) {
    backgroundHandler.io.inputAdress := beerLeftFSM.io.writeAdress
    backgroundHandler.io.inputTileID := beerLeftFSM.io.writeTileID
  }
    // add .elsewhen if you want to write other things to the background as well

  // DEBUG CONNECTION
  // io.led(0) := scoreFSM.io.customerOneScored
  // io.led(1) := scoreFSM.io.customerTwoScored

  /////////////////////////////////////////////////////////////////////////
  ///// Positional and visibility logic for sprites
  /////////////////////////////////////////////////////////////////////////

  // PLAYER
  io.spriteXPosition(0) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(1) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(2) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(3) := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(11) := playerMovementFSM.io.spriteXPosition

  io.spriteYPosition(0) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(1) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(2) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(3) := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(11) := playerMovementFSM.io.spriteYPosition

  io.spriteFlipHorizontal(0) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(1) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(2) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(3) := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(11) := playerMovementFSM.io.spriteFlipHorizontal

  io.spriteFlipVertical(0) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(1) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(2) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(3) := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(11) := playerMovementFSM.io.spriteFlipVertical

  // CUSTOMERS
  io.spriteXPosition(4) := spawnCustomer.io.customer1PosX
  io.spriteXPosition(5) := spawnCustomer.io.customer1PosX
  io.spriteXPosition(6) := spawnCustomer.io.customer2PosX
  io.spriteXPosition(7) := spawnCustomer.io.customer2PosX

  io.spriteYPosition(4) := spawnCustomer.io.customer1PosY
  io.spriteYPosition(5) := spawnCustomer.io.customer1PosY
  io.spriteYPosition(6) := spawnCustomer.io.customer2PosY
  io.spriteYPosition(7) := spawnCustomer.io.customer2PosY

  io.spriteFlipHorizontal(4) := spawnCustomer.io.customer1Flipped
  io.spriteFlipHorizontal(5) := spawnCustomer.io.customer2Flipped

  io.spriteVisible(4) := spawnCustomer.io.customer1IdleVisible
  io.spriteVisible(5) := spawnCustomer.io.customer1DrinkingVisible
  io.spriteVisible(6) := spawnCustomer.io.customer2IdleVisible
  io.spriteVisible(7) := spawnCustomer.io.customer2DrinkingVisible

  // BEER
  io.spriteXPosition(8) := beerMovement.io.beerXPos
  io.spriteXPosition(9) := returnBeerFSM.io.returnBeerXPos

  io.spriteYPosition(8) := beerMovement.io.beerYPos
  io.spriteYPosition(9) := returnBeerFSM.io.returnBeerYPos

  io.spriteVisible(8) := beerMovement.io.beerVisible
  io.spriteVisible(9) := returnBeerFSM.io.beerVisible

  /////////////////////////////////////////////////////////////////////////
  ///// Player animation logic
  /////////////////////////////////////////////////////////////////////////
  switch(playerMovementFSM.io.spriteAnimationFrame) {
    is(0.U) {
      io.spriteVisible(0) := true.B // TRUE
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(3) := false.B
      io.spriteVisible(11) := false.B
      io.led(2) := false.B

    }
    is(1.U) {
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(3) := false.B
      io.spriteVisible(11) := true.B // TRUE
    }
    is(2.U) {
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := true.B // TRUE
      io.spriteVisible(3) := false.B
      io.spriteVisible(11) := false.B

    }
    is(3.U) {
      io.spriteVisible(0) := false.B
      io.spriteVisible(1) := false.B
      io.spriteVisible(2) := false.B
      io.spriteVisible(3) := true.B // TRUE
      io.spriteVisible(11) := false.B

    }
    is(4.U) {
      io.spriteVisible(0)  := false.B
      io.spriteVisible(1)  := true.B //TRUE
      io.spriteVisible(2)  := false.B
      io.spriteVisible(3)  := false.B 
      io.spriteVisible(11) := false.B

      io.led(2) := true.B

    }
  }

  /////////////////////////////////////////////////////////////////////////
  ///// DONE SIGNALS AND FSMD FOR FRAME UPDATE
  /////////////////////////////////////////////////////////////////////////
  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  val playerDoneReg = RegInit(false.B)
  val beerDoneReg = RegInit(false.B)
  val scoreFSMDoneReg = RegInit(false.B)
  val spawnCustomerReg = RegInit(false.B)
  val backgroundDoneReg = RegInit(false.B)
  val returnBeerDoneReg = RegInit(false.B)

  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
        playerMovementFSM.io.work := true.B
        beerMovement.io.work := true.B
        scoreFSM.io.work := true.B
        spawnCustomer.io.work := true.B
        backgroundHandler.io.work := true.B
        returnBeerFSM.io.work := true.B

        playerDoneReg := false.B
        beerDoneReg := false.B
        scoreFSMDoneReg := false.B
        spawnCustomerReg := false.B
        backgroundDoneReg := false.B
        returnBeerDoneReg := false.B
      }
    }
    is(compute1) {
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

      when(returnBeerFSM.io.done) {
        returnBeerDoneReg := true.B
      }

      when(
        playerDoneReg && beerDoneReg && scoreFSMDoneReg && spawnCustomerReg && backgroundDoneReg && returnBeerDoneReg
      ) {
        stateReg := done
      }
    }
    is(done) {
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
