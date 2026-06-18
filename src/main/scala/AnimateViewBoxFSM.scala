import chisel3._
import chisel3.util._

class AnimateViewBoxFSM extends Module {
    val io = IO(new Bundle {
        //Work signal and stageID
        val work         = Input(Bool())
        val stageID      = Input(UInt(2.W))

        //Output X- Y-value of viewBox and done signal
        val viewBoxX     = Output(UInt(10.W))
        val viewBoxY     = Output(UInt(9.W))
        
        //Done signal to backgroundHandler
        val done         = Output(Bool())
    })



    //Registers
    val viewBoxXReg   = RegInit(0.U(10.W))
    val viewBoxYReg   = RegInit(0.U(9.W))
  
    io.viewBoxX      := viewBoxXReg
    io.viewBoxY      := viewBoxYReg
    io.done          := false.B

    val idle :: busy :: finished :: Nil = Enum(3)
    val stateReg      = RegInit(idle)

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
                viewBoxXReg := (20*32).U
                viewBoxYReg := 0.U
            }.elsewhen(io.stageID === 2.U){
                viewBoxXReg := 0.U
                viewBoxYReg := (15*32).U
            }.elsewhen(io.stageID === 3.U){
                viewBoxXReg := (20*32).U
                viewBoxYReg := (15*32).U
            }
            stateReg := finished

        }
        is(finished){
            stateReg := idle
            io.done := true.B
        }
    }
}
