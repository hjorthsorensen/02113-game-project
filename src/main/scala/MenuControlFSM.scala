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

        val stageID    = Output(UInt(2.W))
        val outOfMenu  = Output(Bool())
        val done       = Output(Bool())
    })

    // GAME OVER = ScoreDone && Ikke flere bajer

    val idle :: busy :: finished :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    val stageIDReg = RegInit(3.U(2.W))

    val outOfMenuReg  = RegInit(false.B)
    io.outOfMenu     := outOfMenuReg

    val gameOver = io.scoreDone && (io.beersLeft === 0.U)

    when (outOfMenuReg) {
        stateReg := finished
    }

    io.stageID  := stageIDReg
    io.done     := false.B
    //Switch to loading
    when(outOfMenuReg && io.btnU && io.btnD){
        outOfMenuReg := false.B
        stageIDReg := 1.U
    }


    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }
        }
        is(busy){
            when(gameOver) {
                stageIDReg := 2.U
                outOfMenuReg := true.B
            }.elsewhen (io.btnC) {
                stageIDReg := 0.U
                outOfMenuReg := true.B
            }
            stateReg := finished
        }
        is(finished){
            stateReg := idle
            io.done := true.B
        }
    }
}
