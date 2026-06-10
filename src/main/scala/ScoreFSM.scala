//////////////////////////
// SCORE CALCULATOR FSM //
//////////////////////////

import chisel3._
import chisel3.util._

class ScoreFSM extends Module {
  val io = IO(new Bundle {
    // Inputs
    val work = Input(Bool())
    val customerOnePositionX = Input(SInt(11.W))
    val customerOnePositionY = Input(SInt(10.W))
    val customerTwoPositionX = Input(SInt(11.W))
    val customerTwoPositionY = Input(SInt(10.W))
    val beerPositionX = Input(SInt(11.W))
    val beerPositionY = Input(SInt(10.W))
    val beerValid = Input(Bool())

    // Outputs
    val customerOneScored = Output(Bool())
    val customerTwoScored = Output(Bool())
    val done = Output(Bool())
    val score = Output(UInt(8.W))
  })

  // Registers
  val scoreReg = RegInit(0.U(8.W))
  val customerOneScoredReg = RegInit(false.B)
  val customerTwoScoredReg = RegInit(false.B)
  val distanceX = WireDefault(0.S(11.W))
  val distanceY = WireDefault(0.S(10.W))

  distanceX := io.beerPositionX - io.customerOnePositionX
  distanceY := io.beerPositionY - io.customerOnePositionY

  // State definitions
  val idle :: waitingForBeer :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  // FSM
  switch(stateReg) {
    is(idle) {
      when(io.work) {
        stateReg := waitingForBeer
      }
    }
    is(waitingForBeer) {
      stateReg := done
      when(io.beerValid) {
        // Check if the beer is at the same Y position as either customer
        when(
          (io.beerPositionX - io.customerOnePositionX) > 0.S && (io.beerPositionX - io.customerOnePositionX) < 40.S
        ) {
          // Score Calculations | Pixel pefect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX === 0.S) {
            scoreReg := scoreReg + 5.U
            customerOneScoredReg := true.B
          }.elsewhen(distanceX >= -32.S && distanceX <= 32.S) {
            scoreReg := scoreReg + 2.U
            customerOneScoredReg := true.B
          }.elsewhen(distanceX >= -64.S && distanceX <= 64.S) {
            scoreReg := scoreReg + 1.U
            customerOneScoredReg := true.B
          }
          scoreReg := scoreReg
        }

        when(
          (io.beerPositionX - io.customerTwoPositionX) > 0.S && (io.beerPositionX - io.customerTwoPositionX) < 40.S
        ) {
          val distanceX = io.customerTwoPositionX - io.beerPositionX
          // Score Calculations | Pixel pefect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX === 0.S) {
            scoreReg := scoreReg + 5.U
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX >= -32.S && distanceX <= 32.S) {
            scoreReg := scoreReg + 2.U
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX >= -64.S && distanceX <= 64.S) {
            scoreReg := scoreReg + 1.U
            customerTwoScoredReg := true.B
          }
          scoreReg := scoreReg
        }
      }

    }
    is(done) {
      stateReg := idle
    }
  }

  // Output the score
  io.score := scoreReg
  io.done := stateReg === done
  io.customerOneScored := customerOneScoredReg
  io.customerTwoScored := customerTwoScoredReg

}
