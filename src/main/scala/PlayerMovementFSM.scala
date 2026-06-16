//////////////////////////////////////////////////////////////////////////////
// Comments:
// This file contains the player movement logic
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class PlayerMovementFSM() extends Module {
  val io = IO(new Bundle {
    //Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    val reset = Input(Bool())

    //SpriteA
    val spriteXPosition = Output(SInt(11.W)) //-1024 to 1023
    val spriteYPosition = Output(SInt(10.W)) //-512 to 511
    val spriteVisible = Output(Bool())
    val spriteFlipHorizontal = Output(Bool())
    val spriteFlipVertical = Output(Bool())

    val beerSpeed = Output(SInt(8.W))
    val spriteAnimationFrame = Output(UInt(3.W))

    val beerLeft = Output(UInt(4.W))

    //Status
    val work = Input(Bool())
    val done = Output(Bool())
    val beerReady = Input(Bool())
    val isCatching = Output(Bool())
  })
  ///////////////////////////////////////////////////
  // REGISTERS
  //////////////////////////////////////////////////
  //FSM
  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)
  
  // SPRITES
  val spriteYReg              = RegInit(160.S(10.W))
  val spriteXReg              = RegInit(512.S(11.W))
  val spriteFlipHorizontalReg = RegInit(false.B)
  val spriteYRegOld           = RegInit(160.S(10.W))
  val animFrameReg            = RegInit(0.U(3.W))
  val spriteAnimationY        = RegInit(false.B)
  
  // BEER
  val beerSpeedReg  = RegInit(0.S(8.W))
  val throwStrength = RegInit(0.S(8.W)) //-16 to 15
  val beerReady     = RegInit(false.B)
  val beerLeftReg   = RegInit(10.U)

  // OTHER
  val btnUpPressed   = RegInit(false.B)
  val btnDownPressed = RegInit(false.B)
  val frameCount     = RegInit(0.U(2.W))
  val catchingReg    = RegInit(false.B)
  val catchCount     = RegInit(0.U(7.W))

  ////////////////////////////////////////////
  // RESET
  ///////////////////////////////////////////


  when (reset) {
    spriteYReg              = (160.S)
    spriteXReg              = (512.S)
    spriteFlipHorizontalReg = (false.B)
    spriteYRegOld           = (160.S)
    animFrameReg            = (0.U)
    spriteAnimationY        = (false.B)
  
    // BEER
    beerSpeedReg  = (0.S)
    throwStrength = (0.S) //-16 to 15
    beerReady     = (false.B)
    beerLeftReg   = (10.U)

    // OTHER
    btnUpPressed   = (false.B)
    btnDownPressed = (false.B)
    frameCount     = (0.U)
    catchingReg    = (false.B)
    catchCount     = (0.U)

    io.spriteVisible := false.B

    stateReg := done
  }

  ////////////////////////////////////////////
  //IO Connections
  ///////////////////////////////////////////
  // SPRITE
  io.spriteXPosition      := spriteXReg
  io.spriteYPosition      := spriteYReg
  io.spriteFlipHorizontal := spriteFlipHorizontalReg
  io.spriteAnimationFrame := animFrameReg
  io.spriteVisible        := true.B
  io.spriteFlipVertical   := false.B
  
  // OTHER
  io.beerSpeed  := beerSpeedReg
  io.beerLeft   := beerLeftReg
  io.done       := false.B
  io.isCatching := catchingReg
  ////////////////////////////////////////////////////////
  //FSMD switch
  ////////////////////////////////////////////////////////
  switch(stateReg) {
    is(idle) {
      when(io.work) { stateReg := compute1 }
    }

    is(compute1) {
      // --- BEER THROW CHARGING
      when (io.btnC && io.beerReady && beerReady) {
        when (frameCount === 0.U) {
          // Keep charging up to the max cap of 30
          throwStrength := Mux(throwStrength < 30.S, throwStrength + 1.S, throwStrength)
          spriteXReg := Mux(spriteXReg < 542.S, spriteXReg + 1.S, spriteXReg)
        }
        // Save Y location
        when (!spriteAnimationY) {
          spriteAnimationY := true.B
          spriteYRegOld := spriteYReg
        }
        spriteYReg := Mux(throwStrength < 30.S,Mux(frameCount === 0.U || frameCount === 1.U, spriteYReg + (throwStrength >> 3), spriteYReg - (throwStrength >> 3)), Mux(frameCount === 0.U || frameCount === 2.U, spriteYReg + (throwStrength >> 2), spriteYReg - (throwStrength >> 2)))
      }

      // --- BEER THROW RELEASE
      when (!io.btnC && frameCount === 0.U) {
        beerSpeedReg  := throwStrength
        throwStrength := 0.S           
        spriteXReg := 512.S
        when (!(throwStrength === 0.S)) {
          beerReady := false.B
        }

        // Return to grid at saved location
        when (spriteAnimationY){
          spriteAnimationY := false.B
          spriteYReg := spriteYRegOld

        }

      }

      // BEER CATCH
      when (io.btnL) {
        catchCount := 64.U
        catchingReg := true.B
      }

      catchCount := catchCount + 1.U
      when (catchCount < 64.U) {
        catchingReg := false.B
      }

      // MOVE PLAYER
      when(io.btnD && throwStrength === 0.S){
        btnDownPressed := true.B
        when(spriteYReg < (480 - 64 - 24).S && !btnDownPressed) {
          spriteYReg := spriteYReg + 64.S
        }
      } .elsewhen(io.btnU && throwStrength === 0.S){
        btnUpPressed := true.B
        when(spriteYReg > (96 + 64).S && !btnUpPressed) {
          spriteYReg := spriteYReg - 64.S
        }
      } .otherwise {
        btnUpPressed := false.B
        btnDownPressed := false.B

        when (io.btnR) {
          spriteXReg := (512 + 32).S 
        }
      }


      // ANIMATION ASSiGNMENT
      when(io.btnR) {
        // ONLY ALLOW REFILL AT TOP
        when (spriteYReg < (96 + 64 + 33).S && !beerReady && beerLeftReg > 0.U) {
          animFrameReg := 3.U
          beerReady := true.B
          beerLeftReg := beerLeftReg - 1.U
        }
      } .elsewhen(catchingReg){
        animFrameReg := 1.U
      } .elsewhen(io.btnC && beerReady) {
        animFrameReg := 2.U
      } .otherwise {
        when (catchCount < 32.U) {
          animFrameReg := 0.U
        } .otherwise {
          animFrameReg := 4.U
        }
      }

      stateReg := done
    }

    is(done) {
      frameCount := frameCount + 1.U
      io.done := true.B
      stateReg := idle
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
