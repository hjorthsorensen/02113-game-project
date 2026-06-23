import chisel3._
import chisel3.util._

class MenuControlFSM extends Module {
    val io = IO(new Bundle {
        val work       = Input(Bool())
        val btnC       = Input(Bool())
        val btnU       = Input(Bool())
        val btnD       = Input(Bool())
        val scoreDone  = Input(Bool())
        val beersLeft  = Input(UInt(4.W))
        val beerSpeed  = Input(SInt(8.W))

        val stageID    = Output(UInt(2.W))
        val outOfMenu  = Output(Bool())
        val changingBar= Output(Bool())
        val done       = Output(Bool())
    })

    // GAME OVER = ikke flere Øl, øllen er kastet.

    val idle :: busy :: finished :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    val stageIDReg = RegInit(3.U(2.W))

    val outOfMenuReg  = RegInit(false.B)
    io.outOfMenu     := outOfMenuReg

    val delayReg = RegInit(0.U(8.W))
    val lockScreen = RegInit(false.B)
    io.changingBar := false.B

    when(stageIDReg === 1.U){
        io.changingBar := true.B
    }

    def fallingEdge(signal: Bool): Bool = !signal && RegNext(signal)
    val beerSpeedFallingEdge = fallingEdge(io.beerSpeed === 0.S)
    
    val gameOver = (io.beersLeft === 0.U) && beerSpeedFallingEdge

    val fps = RegInit(0.U(8.W))


    when (outOfMenuReg) {
        stateReg := finished
    }

    io.stageID  := stageIDReg
    io.done     := false.B
    //Switch to loading Not inside FSM cause FSM only active when outOfMenuReg === true.B
    when(outOfMenuReg && io.btnU && io.btnD){
        outOfMenuReg := false.B
        stageIDReg := 1.U
        lockScreen := true.B
    }
    when(outOfMenuReg && gameOver){
        outOfMenuReg := false.B
        stageIDReg := 2.U
    }


    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }
        }
        is(busy){
            when (io.btnC && !lockScreen) {
                stageIDReg := 0.U
                outOfMenuReg := true.B
            }
            when(lockScreen){
                delayReg := delayReg + 1.U
            }
            when(delayReg === 240.U){
                delayReg := 0.U
                stageIDReg := 0.U
                outOfMenuReg := true.B
            }
            stateReg := finished
        }
        is(finished){
            
            stateReg := idle
            io.done := true.B
            when(gameOver) {
                fps := fps + 1.U
            }
        }
    }
}
