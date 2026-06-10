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

    val beerSpeed = Output(SInt(8.W))

    //GraphicEngineVGA
    val spriteXPosition = Output(SInt(11.W)) //-1024 to 1023
    val spriteYPosition = Output(SInt(10.W)) //-512 to 511
    val spriteAnimationFrame = Output(UInt(2.W))
    val spriteVisible = Output(Bool())
    val spriteFlipHorizontal = Output(Bool())
    val spriteFlipVertical = Output(Bool())

    //Status
    val work = Input(Bool())
    val done = Output(Bool())
    val beerReady = Input(Bool())
    // val beerValid = Output(Bool())
  })

  // REGISTERS
  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)
  
  val spriteYReg = RegInit(160.S(10.W))
  val spriteXReg = RegInit(500.S(11.W))
  val sprite0FlipHorizontalReg = RegInit(false.B)

  val beerSpeedReg = RegInit(0.S(8.W))
  val throwStrength = RegInit(0.S(8.W)) //-16 to 15
  val frameCount = RegInit(0.U(1.W))

  val animFrameReg = RegInit(0.U(2.W))

  val btnUpPressed = RegInit(false.B)
  val btnDownPressed = RegInit(false.B)

  val beerReady = RegInit(false.B)

  //Setting all sprite control outputs to zero
  io.spriteXPosition := spriteXReg
  io.spriteYPosition := spriteYReg
  io.spriteVisible := true.B
  io.spriteFlipHorizontal := sprite0FlipHorizontalReg
  io.spriteFlipVertical := false.B
  io.spriteAnimationFrame := animFrameReg
  io.beerSpeed := beerSpeedReg

  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.work) {
        stateReg := compute1
      }
    }

    is(compute1) {

      when (io.btnC && io.beerReady && beerReady) {
        when (frameCount === 0.U) {
          // Keep charging up to the max cap of 15
          throwStrength := Mux(throwStrength < 30.S, throwStrength + 1.S, throwStrength)
          spriteXReg := Mux(spriteXReg < 530.S, spriteXReg + 1.S, spriteXReg)
        }
        spriteYReg := Mux(frameCount === 0, spriteYReg + throwStrength, spriteYReg - throwStrength)
      }

      // 2. Handle the launch logic when the button is released (or beer stops being ready)
      // We check if we have accumulated strength to discharge
      when (!io.btnC && frameCount === 0.U) {
        beerSpeedReg  := throwStrength // Launch at full accumulated strength!
        throwStrength := 0.S           // Reset strength for the next throw
        spriteXReg := 500.S
        when (!(throwStrength === 0.S)) {
          beerReady := false.B
        }
      }

      when(io.btnD){
        btnDownPressed := true.B
        when(spriteYReg < (480 - 64 - 24).S && !btnDownPressed) {
          spriteYReg := spriteYReg + 64.S
        }
      } .elsewhen(io.btnU){
        btnUpPressed := true.B
        when(spriteYReg > (96 + 64).S && !btnUpPressed) {
          spriteYReg := spriteYReg - 64.S
        }
      } .otherwise {
        btnUpPressed := false.B
        btnDownPressed := false.B
      }

      when(io.btnR) {
        when (spriteYReg < (96 + 64 + 33).S) {
          animFrameReg := 3.U
          beerReady := true.B
        }
      } .elsewhen(io.btnL){
        animFrameReg := 1.U
      } .elsewhen(io.btnC && beerReady) {
        animFrameReg := 2.U
      } .otherwise {
        animFrameReg := 0.U
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
