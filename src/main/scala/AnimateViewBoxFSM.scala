import chisel3._
import chisel3.util._

class AnimateViewBoxFSM extends Module {
    val io = IO(new Bundle {
        val work = Input(Bool())
        val stageID = Input(UInt(2.W))

        val viewBoxX = Output(UInt(10.W))
        val viewBoxY = Output(UInt(9.W))
        val stageIDOut = Output(Vec(2,Bool()))
        val done = Output(Bool())
    })



    //Registers
    val viewBoxXReg = RegInit(1.U(10.W))
    val viewBoxYReg = RegInit(1.U(9.W))

    io.viewBoxX := viewBoxXReg
    io.viewBoxY := viewBoxYReg
    io.done := false.B
    io.stageIDOut(0) := false.B
    io.stageIDOut(1) := false.B

    val idle :: busy :: finished :: Nil = Enum(3)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }
        }
        is(busy){
            when(io.stageID === 0.U){
                viewBoxXReg := 0.U
                viewBoxYReg := 0.U
            }.elsewhen(io.stageID === 1.U){
                io.stageIDOut(0) := true.B
                viewBoxXReg := 20.U
                viewBoxYReg := 0.U
            }.elsewhen(io.stageID === 2.U){
                io.stageIDOut(1) := true.B
                viewBoxXReg := 0.U
                viewBoxYReg := 14.U
            }.elsewhen(io.stageID === 3.U){
                io.stageIDOut(0) := true.B
                io.stageIDOut(1) := true.B
                viewBoxXReg := 20.U
                viewBoxYReg := 14.U
            }
            stateReg := finished

        }
        is(finished){
            stateReg := idle
            io.done := true.B
        }
    }
}
