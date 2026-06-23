//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// This file contains the game logic. Implement yours here.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._
import Chisel.debug

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

    //Audio
    val DIN = Output(Bool()) //JA1 pin
    val BCLK = Output(Bool()) //JA2 pin
    val LRC = Output(Bool()) // JA3 pin
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

  // DEBUG RESET
  val resetIn = WireDefault(io.sw(2))
  /////////////////////////////////////////////////////////////////////////
  ///// FSM modules instantiation
  /////////////////////////////////////////////////////////////////////////

  val playerMovementFSM = Module(new PlayerMovementFSM())
  val beerMovement      = Module(new BeerMovementFSM())
  val scoreFSM          = Module(new ScoreFSM())
  val spawnCustomer     = Module(new SpawnCustomerFSM(16,2))
  val backgroundHandler = Module(new BackgroundHandlerFSM())
  val scoreBoardFSM     = Module(new ScoreBoardDisplayFSM())
  val returnBeerFSM     = Module(new ReturnBeerFSM())
  val brokenGlassFSM    = Module(new BrokenGlassDisplayFSM())
  val beerLeftFSM       = Module(new BeerLeftFSM())
  val audioGen          = Module(new AudioGenerator())
  val I2SDriver         = Module(new I2SDriver())
  val multiplierFSM     = Module(new MultiplierDisplayFSM())
  val viewBoxFSM        = Module(new AnimateViewBoxFSM())
  val menuFSM           = Module(new MenuControlFSM)
  val loadingFSM        = Module(new LoadingScreenFSM())
  val changeBarFSM      = Module(new ChangeBarFSM())
  /////////////////////////////////////////////////////////////////////////
  ///// FSM modules connections
  /////////////////////////////////////////////////////////////////////////

  beerMovement.io.work           := false.B
  scoreFSM.io.work               := false.B
  spawnCustomer.io.work          := false.B
  playerMovementFSM.io.work      := false.B
  backgroundHandler.io.work      := false.B
  returnBeerFSM.io.work          := false.B
  viewBoxFSM.io.work             := false.B
  menuFSM.io.work                := false.B
  loadingFSM.io.work             := false.B
  changeBarFSM.io.work           := false.B

  resetIn := !menuFSM.io.outOfMenu

  playerMovementFSM.io.resetIn   := resetIn
  spawnCustomer.io.resetIn       := resetIn
  scoreFSM.io.resetIn            := resetIn

  // Connecting to beer movement
  beerMovement.io.speed         := playerMovementFSM.io.beerSpeed
  beerMovement.io.beerYPosInp   := playerMovementFSM.io.spriteYPosition

  // Connecting to player movement
  playerMovementFSM.io.beerReady := beerMovement.io.beerReady

  playerMovementFSM.io.btnC     := io.btnC
  playerMovementFSM.io.btnU     := io.btnU
  playerMovementFSM.io.btnL     := io.btnL
  playerMovementFSM.io.btnR     := io.btnR
  playerMovementFSM.io.btnD     := io.btnD

  // Connecting to score
  scoreFSM.io.beerPositionX     := beerMovement.io.beerXPos
  scoreFSM.io.beerPositionY     := beerMovement.io.beerYPos
  scoreFSM.io.beerValid         := beerMovement.io.beerValid

  scoreFSM.io.customerOneScoredInp := spawnCustomer.io.customerScoreDone(0)
  scoreFSM.io.customerTwoScoredInp := spawnCustomer.io.customerScoreDone(1)
  scoreFSM.io.customerOnePositionX := spawnCustomer.io.customerPosX(0)
  scoreFSM.io.customerOnePositionY := spawnCustomer.io.customerPosY(0)
  scoreFSM.io.customerTwoPositionX := spawnCustomer.io.customerPosX(1)
  scoreFSM.io.customerTwoPositionY := spawnCustomer.io.customerPosY(1)

  scoreFSM.io.playerReadyToCatch   := playerMovementFSM.io.isCatching
  scoreFSM.io.playerY              := playerMovementFSM.io.spriteYPosition
  scoreFSM.io.beerEmptyY           := returnBeerFSM.io.returnBeerYPos
  scoreFSM.io.beerEmptyX           := returnBeerFSM.io.returnBeerXPos
  scoreFSM.io.emptyBeerValid       := returnBeerFSM.io.beerReturnValid
  scoreFSM.io.beerBroken           := beerMovement.io.beerBroken


  // Connecting to scoreboard
  scoreBoardFSM.io.score          := scoreFSM.io.score
  scoreBoardFSM.io.work           := backgroundHandler.io.scoreWork

  // Connecting beer left
  beerLeftFSM.io.score            := playerMovementFSM.io.beerLeft
  beerLeftFSM.io.work             := backgroundHandler.io.beerWork

  // Connecting to brokenGlassDisplayFSM
  brokenGlassFSM.io.beerBroken    := beerMovement.io.beerBroken
  brokenGlassFSM.io.work          := backgroundHandler.io.brokenGlassWork
  brokenGlassFSM.io.tableID       := beerMovement.io.tableID

  // Connecting to MultiplierFSM
  multiplierFSM.io.multiplier     := scoreFSM.io.currentMultiplier
  multiplierFSM.io.work           := backgroundHandler.io.multiplierWork

  //Loading assignment
  loadingFSM.io.work := backgroundHandler.io.loadingWork

  //ChangeBar
  changeBarFSM.io.work := backgroundHandler.io.changeBarWork
  changeBarFSM.io.changingBar := menuFSM.io.changingBar

  // Connecting to spawn customer
  spawnCustomer.io.customerScored(0) := scoreFSM.io.customerOneScored
  spawnCustomer.io.customerScored(1) := scoreFSM.io.customerTwoScored
  spawnCustomer.io.beerDone          := beerMovement.io.beerValid
  
  // Connecting to return beer FSM
  returnBeerFSM.io.customer1XPos     := spawnCustomer.io.customerPosX(0)
  returnBeerFSM.io.customer1YPos     := spawnCustomer.io.customerPosY(0)
  returnBeerFSM.io.customer2XPos     := spawnCustomer.io.customerPosX(1)
  returnBeerFSM.io.customer2YPos     := spawnCustomer.io.customerPosY(1)
  returnBeerFSM.io.returnCustomer1   := spawnCustomer.io.customerScoreDone(0)
  returnBeerFSM.io.returnCustomer2   := spawnCustomer.io.customerScoreDone(1)
  returnBeerFSM.io.isBeerCatched     := scoreFSM.io.beerCatched

  //connecting to audio generator
  audioGen.io.beerSpeed            := beerMovement.io.beerSpeed
  audioGen.io.sampleReady            := I2SDriver.io.sampleReady
  audioGen.io.beerBreaking := beerMovement.io.beerBroken
  audioGen.io.gameOver := menuFSM.io.gameOver
  audioGen.io.ptScoring := beerMovement.io.beerValid
  //connections to I2S driver
  I2SDriver.io.generatedAudio := audioGen.io.audioDataOut


  //connecting audio io out
  io.BCLK       := I2SDriver.io.BCLKOutput
  io.LRC        := I2SDriver.io.LRC
  io.DIN        := I2SDriver.io.DIN

  // Connecting tp background handler
  backgroundHandler.io.inputAdress         := 0.U
  backgroundHandler.io.inputTileID         := 26.U

  backgroundHandler.io.scoreDone           := scoreBoardFSM.io.done
  backgroundHandler.io.brokenGlassDone     := brokenGlassFSM.io.done
  backgroundHandler.io.beerDone            := beerLeftFSM.io.done
  backgroundHandler.io.multiplierDone      := multiplierFSM.io.done
  backgroundHandler.io.loadingDone         := loadingFSM.io.done
  backgroundHandler.io.changeBarDone       := changeBarFSM.io.done

  io.backBufferWriteAddress                := backgroundHandler.io.writeAdress
  io.backBufferWriteData                   := backgroundHandler.io.writeTileID
  io.backBufferWriteEnable                 := backgroundHandler.io.writeEnable

  // Conditionally assigns write address and tileID to the backgroundHandler
  when(scoreBoardFSM.io.writingScore) {
    backgroundHandler.io.inputAdress := scoreBoardFSM.io.writeAdress
    backgroundHandler.io.inputTileID := scoreBoardFSM.io.writeTileID
  }.elsewhen {multiplierFSM.io.writingMultiplier } {
    backgroundHandler.io.inputAdress := multiplierFSM.io.writeAdress
    backgroundHandler.io.inputTileID := multiplierFSM.io.writeTileID
  }.elsewhen(brokenGlassFSM.io.writingBrokenGlass) {
    backgroundHandler.io.inputAdress := brokenGlassFSM.io.writeAdress
    backgroundHandler.io.inputTileID := brokenGlassFSM.io.writeTileID
  }.elsewhen(beerLeftFSM.io.writingScore) {
    backgroundHandler.io.inputAdress := beerLeftFSM.io.writeAdress
    backgroundHandler.io.inputTileID := beerLeftFSM.io.writeTileID
  }.elsewhen(loadingFSM.io.writingLoading){
    backgroundHandler.io.inputAdress := loadingFSM.io.writeAdress
    backgroundHandler.io.inputTileID := loadingFSM.io.writeTileID
  }.elsewhen(changeBarFSM.io.writingBG){
    backgroundHandler.io.inputAdress := changeBarFSM.io.writeAdress
    backgroundHandler.io.inputTileID := changeBarFSM.io.writeTileID
  }
  // add .elsewhen if you want to write other things to the background as well
  
  // ViewBox connections
  viewBoxFSM.io.stageID    := menuFSM.io.stageID
  io.viewBoxX              := viewBoxFSM.io.viewBoxX
  io.viewBoxY              := viewBoxFSM.io.viewBoxY

  // Menu
  menuFSM.io.btnC          := io.btnC
  menuFSM.io.btnU          := io.btnU
  menuFSM.io.btnD          := io.btnD
  menuFSM.io.scoreDone     := scoreFSM.io.done
  menuFSM.io.beersLeft     := playerMovementFSM.io.beerLeft
  menuFSM.io.beerSpeed     := beerMovement.io.beerSpeed

  // DEBUG CONNECTION
  val debugVec = RegInit(VecInit(false.B,false.B,false.B,false.B))
  debugVec(0) := (beerMovement.io.beerSpeed =/= 0.S)
  debugVec(1) := beerMovement.io.beerBroken
  debugVec(2) := beerMovement.io.beerValid
  debugVec(3) := menuFSM.io.gameOver

  io.led(0) := debugVec(0)
  io.led(1) := debugVec(1)
  io.led(2) := debugVec(2)
  io.led(3) := debugVec(3)
  // io.led(0) := scoreFSM.io.customerOneScored
  // io.led(1) := scoreFSM.io.customerTwoScored
  // io.led(0):= Mux(beerMovement.io.speed =/= 0.S, true.B,false.B)
  // io.led(1) := playerMovementFSM.io.beerLeft === 0.U
  // io.led(1) := audioHandlerFSM.io.events === 1.U
  // io.led(1) := audioGen.io.debugEvent

  /////////////////////////////////////////////////////////////////////////
  ///// Positional and visibility logic for sprites
  /////////////////////////////////////////////////////////////////////////

  // PLAYER
  io.spriteXPosition(0)          := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(1)          := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(2)          := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(3)          := playerMovementFSM.io.spriteXPosition
  io.spriteXPosition(11)         := playerMovementFSM.io.spriteXPosition

  io.spriteYPosition(0)          := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(1)          := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(2)          := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(3)          := playerMovementFSM.io.spriteYPosition
  io.spriteYPosition(11)         := playerMovementFSM.io.spriteYPosition

  io.spriteFlipHorizontal(0)     := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(1)     := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(2)     := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(3)     := playerMovementFSM.io.spriteFlipHorizontal
  io.spriteFlipHorizontal(11)    := playerMovementFSM.io.spriteFlipHorizontal

  io.spriteFlipVertical(0)       := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(1)       := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(2)       := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(3)       := playerMovementFSM.io.spriteFlipVertical
  io.spriteFlipVertical(11)      := playerMovementFSM.io.spriteFlipVertical

  // CUSTOMERS
  io.spriteXPosition(4)          := spawnCustomer.io.customerPosX(0)
  io.spriteXPosition(5)          := spawnCustomer.io.customerPosX(0)
  io.spriteXPosition(6)          := spawnCustomer.io.customerPosX(1)
  io.spriteXPosition(7)          := spawnCustomer.io.customerPosX(1)

  io.spriteYPosition(4)          := spawnCustomer.io.customerPosY(0)
  io.spriteYPosition(5)          := spawnCustomer.io.customerPosY(0)
  io.spriteYPosition(6)          := spawnCustomer.io.customerPosY(1)
  io.spriteYPosition(7)          := spawnCustomer.io.customerPosY(1)

  io.spriteFlipHorizontal(4)     := spawnCustomer.io.customerFlipped(0)
  io.spriteFlipHorizontal(5)     := spawnCustomer.io.customerFlipped(1)

  io.spriteVisible(4)            := spawnCustomer.io.customerIdleVisible(0)
  io.spriteVisible(5)            := spawnCustomer.io.customerDrinkingVisible(0)
  io.spriteVisible(6)            := spawnCustomer.io.customerIdleVisible(1)
  io.spriteVisible(7)            := spawnCustomer.io.customerDrinkingVisible(1)

  // BEER
  io.spriteXPosition(8)          := beerMovement.io.beerXPos
  io.spriteXPosition(9)          := returnBeerFSM.io.returnBeerXPos

  io.spriteYPosition(8)          := beerMovement.io.beerYPos
  io.spriteYPosition(9)          := returnBeerFSM.io.returnBeerYPos

  io.spriteVisible(8)            := beerMovement.io.beerVisible && !resetIn
  io.spriteVisible(9)            := returnBeerFSM.io.beerVisible && !resetIn

  /////////////////////////////////////////////////////////////////////////
  ///// Player animation logic
  /////////////////////////////////////////////////////////////////////////
  switch(playerMovementFSM.io.spriteAnimationFrame) {
    is(0.U) {
      io.spriteVisible(0)   := true.B // TRUE
      io.spriteVisible(1)   := false.B
      io.spriteVisible(2)   := false.B
      io.spriteVisible(3)   := false.B
      io.spriteVisible(11)  := false.B

    }
    is(1.U) {
      io.spriteVisible(0)   := false.B
      io.spriteVisible(1)   := false.B
      io.spriteVisible(2)   := false.B
      io.spriteVisible(3)   := false.B
      io.spriteVisible(11)  := true.B // TRUE
    }
    is(2.U) {
      io.spriteVisible(0)   := false.B
      io.spriteVisible(1)   := false.B
      io.spriteVisible(2)   := true.B // TRUE
      io.spriteVisible(3)   := false.B
      io.spriteVisible(11)  := false.B

    }
    is(3.U) {
      io.spriteVisible(0)   := false.B
      io.spriteVisible(1)   := false.B
      io.spriteVisible(2)   := false.B
      io.spriteVisible(3)   := true.B // TRUE
      io.spriteVisible(11)  := false.B

    }
    is(4.U) {
      io.spriteVisible(0)   := false.B
      io.spriteVisible(1)   := true.B // TRUE
      io.spriteVisible(2)   := false.B
      io.spriteVisible(3)   := false.B
      io.spriteVisible(11)  := false.B
    }
  }

  when (!playerMovementFSM.io.spriteVisible) {
    io.spriteVisible(0)     := false.B
      io.spriteVisible(1)   := false.B // TRUE
      io.spriteVisible(2)   := false.B
      io.spriteVisible(3)   := false.B
      io.spriteVisible(11)  := false.B
  }

  /////////////////////////////////////////////////////////////////////////
  ///// DONE SIGNALS AND FSMD FOR FRAME UPDATE
  /////////////////////////////////////////////////////////////////////////
  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  //Registers for reading done signals from all the FSMD's
  val playerDoneReg        = RegInit(false.B)
  val beerDoneReg          = RegInit(false.B)
  val scoreFSMDoneReg      = RegInit(false.B)
  val spawnCustomerDoneReg = RegInit(false.B)
  val backgroundDoneReg    = RegInit(false.B)
  val returnBeerDoneReg    = RegInit(false.B)
  val viewBoxDoneReg       = RegInit(false.B)
  val menuDoneReg          = RegInit(false.B)




  //Collection of done signals
  val doneAll = (playerDoneReg && beerDoneReg && scoreFSMDoneReg && 
                spawnCustomerDoneReg && backgroundDoneReg && returnBeerDoneReg && 
                viewBoxDoneReg && menuDoneReg)


//FSMD
  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
        //Work signal for all FSMD
        playerMovementFSM.io.work := true.B
        beerMovement.io.work      := true.B
        scoreFSM.io.work          := true.B
        spawnCustomer.io.work     := true.B
        backgroundHandler.io.work := true.B
        returnBeerFSM.io.work     := true.B
        viewBoxFSM.io.work        := true.B
        menuFSM.io.work           := !menuFSM.io.outOfMenu


        //Registers all assigned false
        playerDoneReg             := false.B
        beerDoneReg               := false.B
        scoreFSMDoneReg           := false.B
        spawnCustomerDoneReg      := false.B
        backgroundDoneReg         := false.B
        returnBeerDoneReg         := false.B
        viewBoxDoneReg            := false.B
        menuDoneReg               := false.B
        
      }
    }
    is(compute1) {

      // playerMovementFSM.io.work := !playerDoneReg
      // beerMovement.io.work      := !beerDoneReg
      // scoreFSM.io.work          := !scoreFSMDoneReg
      // spawnCustomer.io.work     := !spawnCustomerReg
      // backgroundHandler.io.work := !backgroundDoneReg
      // returnBeerFSM.io.work     := !returnBeerDoneReg
      // viewBoxFSM.io.work        := !viewBoxDoneReg
      // menuFSM.io.work           := !menuDoneReg


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
        spawnCustomerDoneReg := true.B
      }

      when(backgroundHandler.io.done) {
        backgroundDoneReg := true.B
      }
      when(returnBeerFSM.io.done) {
        returnBeerDoneReg := true.B
      }
      when(viewBoxFSM.io.done){
        viewBoxDoneReg := true.B
      }
      when(menuFSM.io.done) {
        menuDoneReg := true.B
      }
      

      when(doneAll) {
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
