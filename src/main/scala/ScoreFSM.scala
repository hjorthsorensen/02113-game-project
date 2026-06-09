//////////////////////////
// SCORE CALCULATOR FSM //
//////////////////////////




import chisel3._
import chisel3.util._


class ScoreFSM extends Module {
    val io = IO(new Bundle {
        // Inputs
        val wakeUp = Input(Bool())
        val customerPositionX = Input(SInt(11.W))
        val customerPositionY = Input(SInt(10.W))
        val beerPositionX = Input(SInt(11.W))
        val beerPositionY = Input(SInt(10.W))
        val beerValid = Input(Bool())

        // Outputs
        val onePointLED = Output(Bool())
        val twoPointsLED = Output(Bool())
        val score = Output(UInt(8.W))
    })


    // Score Calculations
    val scoreReg = RegInit(0.U(8.W))


    // State definitions
    val idle :: waitingForBeer :: done :: Nil = Enum(3)
    val stateReg = RegInit(idle)

    // FSM
    switch(stateReg) {
        is(idle) {
            when(io.wakeUp) {
                stateReg := waitingForBeer
            }
        }
        is(waitingForBeer) {
            when(io.beerValid) {
                val distanceX = io.customerPositionX - io.beerPositionX
                val distanceY = io.customerPositionY - io.beerPositionY // Beer is at 0 for first iteration
                val distance = distanceX.abs + distanceY.abs
                // Score Calculations | Withing 32 units = 2 points, withing 64 units = 1 points, otherwise 0.
                when(distanceX <= 32.S) {
                    scoreReg := 2.U
                } .elsewhen(distanceX <= 64.S) {
                    scoreReg := 1.U
                } .otherwise {
                    scoreReg := 0.U
                }
                stateReg := done
            }

        }
    }

    // Output the score
    io.score := scoreReg
    io.onePointLED := scoreReg === 1.U
    io.twoPointsLED := scoreReg === 2.U

}

