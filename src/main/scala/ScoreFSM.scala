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
    val onePointLED = Output(Bool())
    val twoPointsLED = Output(Bool())
    val customerOneScored = Output(Bool())
    val customerTwoScored = Output(Bool())
    val done = Output(Bool())
    val score = Output(UInt(8.W))
  })

  // Registers
  val scoreReg = RegInit(0.U(8.W))
  val customerOneScoredReg = RegInit(false.B)
  val customerTwoScoredReg = RegInit(false.B)

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
      when(io.beerValid) {
        stateReg := done
        // Check if the beer is at the same Y position as either customer
        when(io.customerOnePositionY === io.beerPositionY) {
          val distanceX = io.customerOnePositionX - io.beerPositionX
          // Score Calculations | Withing 32 units = 2 points, withing 64 units = 1 points, otherwise 0.
          when(distanceX >= -32.S && distanceX <= 32.S) {
            scoreReg := scoreReg + 2.U
          }.elsewhen(distanceX >= -64.S && distanceX <= 64.S) {
            scoreReg := scoreReg + 1.U
          }
          customerOneScoredReg := true.B
        }

        when(io.customerTwoPositionY === io.beerPositionY) {
          val distanceX = io.customerTwoPositionX - io.beerPositionX
          // Score Calculations | Withing 32 units = 2 points, withing 64 units = 1 points, otherwise 0.
          when(distanceX >= -32.S && distanceX <= 32.S) {
            scoreReg := scoreReg + 2.U
          }.elsewhen(distanceX >= -64.S && distanceX <= 64.S) {
            scoreReg := scoreReg + 1.U
          }
          customerTwoScoredReg := true.B
        }
      }
    }
    is(done) {
      stateReg := idle
    }
  }

  // Output the score
  io.score := scoreReg
  io.onePointLED := scoreReg === 1.U
  io.twoPointsLED := scoreReg === 2.U
  io.customerOneScored := customerOneScoredReg
  io.customerTwoScored := customerTwoScoredReg
  io.done := stateReg === done
}
