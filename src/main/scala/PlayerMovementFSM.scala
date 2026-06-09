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
  
  val sprite0YReg = RegInit(180.S(10.W))
  val sprite0XReg = RegInit(500.S(11.W))
  val sprite0FlipHorizontalReg = RegInit(false.B)

  val beerSpeedReg = RegInit(0.S(8.W))
  val throwStrength = RegInit(0.S(8.W)) //-16 to 15
  val frameCount = RegInit(0.U(4.W))

  val animFrameReg = RegInit(0.U(2.W))

  //Setting all sprite control outputs to zero
  io.spriteXPosition := sprite0XReg
  io.spriteYPosition := sprite0YReg
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

      when (io.btnC) {
        when (io.beerReady && (frameCount === 0.U || frameCount === 3.U || frameCount === 6.U )) {
          // Keep charging up to the max cap of 15
          throwStrength := Mux(throwStrength < 30.S, throwStrength + 2.S, throwStrength)
          sprite0XReg := Mux(sprite0XReg < 515.S, sprite0XReg + 1.S, sprite0XReg)
        }
      }

      // 2. Handle the launch logic when the button is released (or beer stops being ready)
      // We check if we have accumulated strength to discharge
      when (!io.btnC && frameCount === 0.U) {
        beerSpeedReg  := throwStrength // Launch at full accumulated strength!
        throwStrength := 0.S           // Reset strength for the next throw
        sprite0XReg := 500.S
      }

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
        animFrameReg := 3.U
      } .elsewhen(io.btnL){
        animFrameReg := 1.U //this is currently the idle2 animation
      } .elsewhen(io.btnC) {
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

  // Just forwarding the newFrame into the frameUpdateDone with a 2 clock cycle delay
  // frameUpdateDone will need to be driven by your game logic FSMs
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
