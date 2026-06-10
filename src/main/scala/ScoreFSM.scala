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

  val distanceX1 = WireDefault(0.S(11.W))
  val distanceY1 = WireDefault(0.S(10.W))
  val distanceX2 = WireDefault(0.S(11.W))
  val distanceY2 = WireDefault(0.S(10.W))

  distanceX1 := io.beerPositionX - io.customerOnePositionX
  distanceY1 := io.beerPositionY - io.customerOnePositionY
  distanceX2 := io.beerPositionX - io.customerTwoPositionX
  distanceY2 := io.beerPositionY - io.customerTwoPositionY

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
        when(distanceY1 >= 0.S && distanceY1 < 40.S) {
          // Score Calculations | Pixel perfect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX1 === 0.S) {
            scoreReg := scoreReg + 5.U
            customerOneScoredReg := true.B
          }.elsewhen(distanceX1 >= -32.S && distanceX1 <= 32.S) {
            scoreReg := scoreReg + 2.U
            customerOneScoredReg := true.B
          }.elsewhen(distanceX1 >= -64.S && distanceX1 <= 64.S) {
            scoreReg := scoreReg + 1.U
            customerOneScoredReg := true.B
          }
        }

        when(distanceY2 >= 0.S && distanceY2 < 40.S) {
          // Score Calculations | Pixel perfect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX2 === 0.S) {
            scoreReg := scoreReg + 5.U
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX2 >= -32.S && distanceX2 <= 32.S) {
            scoreReg := scoreReg + 2.U
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX2 >= -64.S && distanceX2 <= 64.S) {
            scoreReg := scoreReg + 1.U
            customerTwoScoredReg := true.B
          }
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
