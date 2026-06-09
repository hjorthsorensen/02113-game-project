import chisel3._
import chisel3.util._

class BeerMovement extends Module{
    val io = IO(new Bundle {
        val speed = Input(SInt(8.W))
        val wake = Input(Bool())

        val beerXPos = Output(SInt(11.W))
        val beerYPos = Output(SInt(10.W))

        val beerValid = Output(Bool())
        val beerReady = Output(Bool())
        val done = Output(Bool())

    })
    val idle :: busy :: doneCalculation :: doneMovement :: Nil = Enum(3)
    val stateReg = RegInit(idle)

    val remainSpeed = RegInit(0.S(8.W))

    val beerXReg = RegInit(0.S(11.W))
    val beerYReg = RegInit(0.S(10.W))
    

    io.beerXPos := beerXReg
    io.beerYPos := beerYReg

    val doneCalc = remainSpeed === 0.S
    val inCalc = RegInit(false.B)

    io.done := false.B
    io.beerReady := false.B
    io.beerValid := false.B

    switch(stateReg){
        is(idle){
            when(io.wake){
                inCalc := true.B
                when(!inCalc){
                    remainSpeed := io.speed
                }
                stateReg := busy
            }
        }
        is(busy){
            io.beerReady := false.B
            remainSpeed := remainSpeed - 1.S
            beerXReg := beerXReg - remainSpeed
            
            
            stateReg := doneCalculation
        }            
        is(doneCalculation){
            io.done := true.B
            
        }
        is(doneMovement){
            stateReg := idle
            io.beerValid := true.B
            io.done := true.B
        }
    }
}
