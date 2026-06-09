import chisel3._
import chisel3.util._

class BeerMovement extends Module{
    val io = IO(new Bundle {
        val speed = Input(SInt(8.W))

        val work = Input(Bool())

        val beerXPos = Output(SInt(11.W))
        val beerYPos = Output(SInt(10.W))
        val beerVisible = Output(Bool())

        val beerValid = Output(Bool())
        val beerReady = Output(Bool())
        val done = Output(Bool())

    })
    val idle :: busy :: doneMovement :: Nil = Enum(3)
    val stateReg = RegInit(idle)

    val remainSpeed = RegInit(0.S(8.W))

    val beerXReg = RegInit(400.S(11.W))
    val beerYReg = RegInit(200.S(10.W))
    val beerVisibleReg = RegInit(false.B)
    
    io.beerVisible := beerVisibleReg
    io.beerXPos := beerXReg
    io.beerYPos := beerYReg

    val doneCalc = remainSpeed === 0.S
    val inCalc = RegInit(false.B)

    io.done := false.B
    val beerReadyReg = RegInit(true.B)
    io.beerReady := beerReadyReg
    io.beerValid := false.B

    val fpsReg = RegInit(0.U(8.W))
    

    switch(stateReg){
        is(idle){
            when(io.work){
                when(!inCalc && doneCalc && (fpsReg === 120.U)){
                    beerVisibleReg := false.B
                }
                when(inCalc && doneCalc){
                    inCalc := false.B
                    beerReadyReg := true.B
                }
                when(!inCalc && (io.speed =/= 0.S)){
                    fpsReg := 0.U
                    beerVisibleReg := true.B
                    beerReadyReg := false.B
                    inCalc := true.B
                    remainSpeed := io.speed
                    beerXReg := 500.S
                }
                stateReg := busy
            }
            
        }
        is(busy){
            when(!doneCalc && inCalc){
                remainSpeed := remainSpeed - 1.S
                beerXReg := beerXReg - remainSpeed
            }
            stateReg := doneMovement
        }            
        
        is(doneMovement){
            fpsReg := fpsReg + 1.U
            stateReg := idle
            io.done := true.B
            when(doneCalc && inCalc){
                io.beerValid := true.B
            }
        }
    }
}
