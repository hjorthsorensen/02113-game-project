import chisel3._
import chisel3.util._

class BeerMovement extends Module{
    val io = IO(new Bundle {
        val speed = Input(UInt(8.W))
        val work = Input(Bool())

        val beerXPos = Output(SInt(11.W))
        val beerYPos = Output(SInt(10.W))

        val beerValid = Output(Bool())
        val beerReady = Output(Bool())
        val done = Output(Bool())

    })
    val idle :: busy :: done :: Nil = Enum(3)
    val stateReg = RegInit(idle)

    val remainSpeed = RegInit(0.U(8.W))

    val beerXReg = RegInit(0.S(11.W))
    val beerYReg = RegInit(0.S(10.W))

    io.beerXPos := beerXReg
    io.beerYPos := beerYReg

    val doneCalc = remainSpeed === 0.U

    io.done := false.B
    io.beerReady := true.B
    io.beerValid := false.B

    switch(stateReg){
        is(idle){
            when(io.work){
                remainSpeed := io.speed
                stateReg := busy
            }
        }
        is(busy){
            io.beerReady := false.B
            remainSpeed := remainSpeed - 1.S
            beerXReg := beerXReg - remainSpeed
            when(doneCalc){
                stateReg := done
            }
        }            

        is(done){
            stateReg := idle
            io.beerValid := true.B
            io.done := true.B
        }
    }
}
