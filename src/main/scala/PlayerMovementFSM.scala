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
    //Sprite control input
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
  })

  //Setting all sprite control outputs to zero
  io.spriteXPosition := 500.S
  io.spriteYPosition := 180.S
  io.spriteVisible := true.B
  io.spriteFlipHorizontal := false.B
  io.spriteFlipVertical := false.B
  io.spriteAnimationFrame := 0.U
  io.beerSpeed := 0.S
  //Setting frame done to zero
  io.done := false.B

  /////////////////////////////////////////////////////////////////
  // Write here your game logic
  // (you might need to change the initialization values above)
  /////////////////////////////////////////////////////////////////

  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  //Two registers holding the sprite sprite X and Y with the sprite initial position
  // val sprite0XReg = RegInit(32.S(11.W))
  val sprite0YReg = RegInit(180.S(10.W))

  //A registers holding the sprite horizontal flip
  val sprite0FlipHorizontalReg = RegInit(false.B)

  val throwStrength = RegInit(0.S(8.W)) //-16 to 15
  val frameCount = RegInit(0.U(3.W))
  //Making sprite 0 visible
  io.spriteVisible := true.B

  //Connecting resiters to the graphic engine
  io.spriteYPosition := sprite0YReg
  io.spriteFlipHorizontal := sprite0FlipHorizontalReg

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.work) {
        stateReg := compute1
      }
    }

    is(compute1) {

      when (frameCount === 0.U) {
        when (io.beerReady) {
          when (io.btnC) {
            throwStrength := Mux(throwStrength < 15.S, throwStrength + 1.S, throwStrength)
          } .elsewhen (throwStrength > 0.S) {
            io.beerSpeed := throwStrength
          }
        } .otherwise {
          throwStrength := 0.S
          io.beerSpeed := throwStrength
        }
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
        sprite0FlipHorizontalReg := false.B
      } .elsewhen(io.btnL){
        sprite0FlipHorizontalReg := true.B
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
