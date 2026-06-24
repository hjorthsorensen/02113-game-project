//////////////////////////
// SCORE CALCULATOR FSM //
//////////////////////////

import chisel3._
import chisel3.util._

class ScoreFSM extends Module {
  val io = IO(new Bundle {
    // Inputs
    val work                    = Input(Bool())
    //Customer
    val customerOnePositionX    = Input(SInt(11.W))
    val customerOnePositionY    = Input(SInt(10.W))
    val customerTwoPositionX    = Input(SInt(11.W))
    val customerTwoPositionY    = Input(SInt(10.W))

    //Beer
    val beerPositionX           = Input(SInt(11.W))
    val beerPositionY           = Input(SInt(10.W))
    val beerEmptyY              = Input(SInt(10.W))
    val beerEmptyX              = Input(SInt(11.W))
    val emptyBeerValid          = Input(Bool())
    val beerValid               = Input(Bool())

    //Customer scoring
    val customerOneScoredInp    = Input(Bool())
    val customerTwoScoredInp    = Input(Bool())

    //Playerinput
    val playerY                 = Input(SInt(10.W))
    val playerReadyToCatch      = Input(Bool())
    val beerBroken              = Input(Bool())

    //Reset signal
    val resetIn                 = Input(Bool())

    // Outputs
    val customerOneScored       = Output(Bool())
    val customerTwoScored       = Output(Bool())
    val currentMultiplier       = Output(UInt(5.W))
    val done                    = Output(Bool())
    val score                   = Output(UInt(16.W))
    val beerCatched             = Output(Bool())


  })

  // Registers
  val scoreReg                = RegInit(0.U(16.W))
  val customerOneScoredReg    = RegInit(false.B)
  val customerTwoScoredReg    = RegInit(false.B)
  val beerCatched             = RegInit(false.B)
  val beerCatchedIdleCntReg   = RegInit(0.U(6.W))
  val scoreMultiplier         = RegInit(1.U(4.W))
  customerOneScoredReg       := false.B
  customerTwoScoredReg       := false.B


  val distanceX1 = WireDefault(0.S(11.W))
  val distanceY1 = WireDefault(0.S(10.W))
  val distanceX2 = WireDefault(0.S(11.W))
  val distanceY2 = WireDefault(0.S(10.W))

  distanceX1 := io.beerPositionX - io.customerOnePositionX
  distanceY1 := io.beerPositionY - io.customerOnePositionY
  distanceX2 := io.beerPositionX - io.customerTwoPositionX
  distanceY2 := io.beerPositionY - io.customerTwoPositionY


  val hitBoxXEmptyBeer    = (io.beerEmptyX >= (512 - 32).S) && (io.beerEmptyX <= 512.S)
  val hitBoxYEmptyBeer    = (io.playerY === io.beerEmptyY)
  val hitBoxValid         = hitBoxXEmptyBeer && hitBoxYEmptyBeer
  val beerCatchAttempt    = RegInit(false.B)


  // State definitions
  val idleState :: waitingForBeerState :: glassReturn :: doneState :: Nil = Enum(4)
  val stateReg = RegInit(idleState)

  // FSM
  switch(stateReg) {

    is(idleState) {
      when(io.work) {
        stateReg := waitingForBeerState
      }
    }
    is(waitingForBeerState) {
      stateReg := glassReturn
      when(io.beerValid && !customerOneScoredReg && !customerTwoScoredReg) {
        // Check if the beer is at the same Y position as either customer
        when(distanceY1 >= 0.S && distanceY1 < 40.S) {
          // Score Calculations | Pixel perfect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX1 === 0.S) {
            scoreReg := scoreReg + (5.U * scoreMultiplier)
            customerOneScoredReg := true.B
          }.elsewhen(distanceX1 >= -16.S && distanceX1 <= 32.S) {
            scoreReg := scoreReg + (2.U * scoreMultiplier)
            customerOneScoredReg := true.B
          }.elsewhen(distanceX1 >= -32.S && distanceX1 <= 64.S) {
            scoreReg := scoreReg + (1.U * scoreMultiplier)
            customerOneScoredReg := true.B
          }
        }
      }
      when(io.beerValid && !customerTwoScoredReg) {
        when(distanceY2 >= 0.S && distanceY2 < 40.S) {
          // Score Calculations | Pixel perfect = 5 points, within 32 units = 2 points, within 64 units = 1 points, otherwise 0.
          when(distanceX2 === 0.S) {
            scoreReg := scoreReg + (5.U * scoreMultiplier)
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX2 >= -16.S && distanceX2 <= 32.S) {
            scoreReg := scoreReg + (2.U * scoreMultiplier)
            customerTwoScoredReg := true.B
          }.elsewhen(distanceX2 >= -32.S && distanceX2 <= 64.S) {
            scoreReg := scoreReg + (1.U * scoreMultiplier)
            customerTwoScoredReg := true.B
          }
        }
      }
    }
    is(glassReturn) {
      stateReg := doneState
      when(beerCatchAttempt){
        beerCatchedIdleCntReg := beerCatchedIdleCntReg + 1.U
      }
      when(beerCatchedIdleCntReg === 30.U){
        beerCatchedIdleCntReg   := 0.U
        beerCatched             := false.B
        beerCatchAttempt        := false.B
      }

      when(
        io.emptyBeerValid && hitBoxValid && io.playerReadyToCatch && !beerCatchAttempt // Beer Caught
      ) {
        scoreReg := scoreReg + 1.U
        scoreMultiplier        := scoreMultiplier + 1.U
        beerCatched            := true.B
        beerCatchAttempt       := true.B
      }.elsewhen(
        io.emptyBeerValid && ((scoreReg - 1.U) > 0.U) && !beerCatchAttempt && hitBoxXEmptyBeer && (!hitBoxYEmptyBeer || !io.playerReadyToCatch) // Beer not caught
      ) {
        beerCatchAttempt       := true.B
        scoreReg               := scoreReg - 1.U
        scoreMultiplier        := 1.U
      }
    }
    is(doneState) {
      
      stateReg := idleState
    }
  }

  // Output the score, default io assignments
  io.score               := scoreReg
  io.done                := stateReg === doneState
  io.customerOneScored   := customerOneScoredReg
  io.customerTwoScored   := customerTwoScoredReg
  io.beerCatched         := beerCatched
  io.currentMultiplier   := scoreMultiplier
}
