//////////////////////////////////////////////////////////////////////////////
// Comments:
// This file contains the player movement logic
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class PlayerMovementFSM() extends Module {
  val io = IO(new Bundle {
    //inputs
    //buttons 
    val btnC                    = Input(Bool())
    val btnU                    = Input(Bool())
    val btnL                    = Input(Bool())
    val btnR                    = Input(Bool())
    val btnD                    = Input(Bool())
    val resetIn                 = Input(Bool())

    //status
    val work                    = Input(Bool())
    val beerReady               = Input(Bool())

    //outputs
    //Sprite
    val spriteXPosition         = Output(SInt(11.W)) //-1024 to 1023
    val spriteYPosition         = Output(SInt(10.W)) //-512 to 511
    val spriteVisible           = Output(Bool())
    val spriteFlipHorizontal    = Output(Bool())
    val spriteFlipVertical      = Output(Bool())
    val spriteAnimationFrame    = Output(UInt(3.W))


    //beer
    val beerSpeed               = Output(SInt(8.W))
    val beerLeft                = Output(UInt(4.W))
    val beerPour                = Output(Bool())
    val isCatching              = Output(Bool())

    //Status for main FSMD
    val done                    = Output(Bool())
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
  val beerLeftReg   = RegInit(15.U)

  // OTHER
  val btnUpPressed   = RegInit(false.B)
  val btnDownPressed = RegInit(false.B)
  val frameCount     = RegInit(0.U(2.W))
  val catchingReg    = RegInit(false.B)
  val catchCount     = RegInit(120.U(7.W))
  val canBeCatched   = RegInit(true.B)
  val idleFpsCount   = RegInit(0.U(7.W))



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
  io.beerPour   := false.B

  ////////////////////////////////////////////
  // RESET
  ///////////////////////////////////////////

  when (io.resetIn) {
    spriteYReg              := 160.S
    spriteXReg              := 512.S
    spriteFlipHorizontalReg := false.B
    spriteYRegOld           := 160.S
    animFrameReg            := 0.U
    spriteAnimationY        := false.B
  
    // BEER
    beerSpeedReg  := 0.S
    throwStrength := 0.S //-16 to 15
    beerReady     := false.B
    beerLeftReg   := 10.U

    // OTHER
    btnUpPressed   := false.B
    btnDownPressed := false.B
    frameCount     := 0.U
    catchingReg    := false.B
    catchCount     := 120.U
    canBeCatched   := true.B
    idleFpsCount   := 0.U

    io.spriteVisible := false.B

    stateReg := done
  }
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
      when(io.btnL) {// && canBeCatched
        catchCount := 0.U
        idleFpsCount := 0.U
        catchingReg := true.B
        // canBeCatched := false.B
      }
      when(catchCount >= 100.U){
        canBeCatched := true.B
      }
      
      when(catchingReg || (catchCount < 120.U)){
        catchCount := catchCount + 1.U
      }
      when (catchCount > 60.U) {
        catchingReg := false.B
      }

      // MOVE PLAYER
      when(io.btnD && throwStrength === 0.S){
        btnDownPressed := true.B
        when(spriteYReg < (480 - 64 - 24).S && !btnDownPressed) {
          spriteYReg := spriteYReg + 64.S
        }.elsewhen(beerLeftReg === 0.U && !btnDownPressed){
          beerLeftReg := 10.U
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
          io.beerPour := true.B
        }
      } .elsewhen(catchingReg){
        animFrameReg := 1.U
      } .elsewhen(io.btnC && beerReady) {
        animFrameReg := 2.U
      } .otherwise {
        when (idleFpsCount < 32.U) {
          animFrameReg := 0.U
        } .otherwise {
          animFrameReg := 4.U
        }
      }

      stateReg := done
    }

    is(done) {
      idleFpsCount := idleFpsCount + 1.U
      frameCount := frameCount + 1.U
      io.done := true.B
      stateReg := idle
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
