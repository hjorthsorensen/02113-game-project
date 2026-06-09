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
    val idle :: busy :: doneMovement :: Nil = Enum(3)
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
                when(inCalc && doneCalc){
                    inCalc := false.B
                }
                when(!inCalc){
                    inCalc := true.B
                    remainSpeed := io.speed
                }
                stateReg := busy
            }
        }
        is(busy){
            when(!doneCalc && inCalc){
                remainSpeed := remainSpeed - 1.S
                beerXReg := beerXReg - remainSpeed
            }
            io.beerReady := false.B
            stateReg := doneMovement
        }            
        
        is(doneMovement){
            stateReg := idle
            io.done := true.B
            when(doneCalc){
                io.beerValid := true.B
            }
        }
    }
}
