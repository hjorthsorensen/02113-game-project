import chisel3._
import chisel3.util._

class ScoreBoardDisplayFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        val score = Input(UInt(8.W))
        val work = Input(Bool())
        val scoreWriteDone = Input(Bool())
        
        
        //Outputs for background
        // val scoreTileAmount = Output(UInt(4.W))
        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(5.W))
        val scoreWriteEnable = Output(Bool())
        
        
        val done = Output(Bool())

    })

    val idle :: busy :: done :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    // val tile1 :: tile2 :: tile3 :: Nil = Enum(3)
    // val tileReg = RegInit(tile1)

    val scoreRightDigit = io.score % 10.U
    val scoreMiddleDigit = io.score / 10.U   
    val scoreLeftDigit = io.score / 100.U 
    val scoreIDReg = RegInit(0.U(2.W))
    val scoreWriteDoneReg = RegInit(false.B)
    scoreWriteDoneReg := io.scoreWriteDone

    io.scoreWriteEnable := false.B
    io.done := false.B
    io.writeAdress := 0.U
    io.writeTileID := 0.U

    


    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }
        }
        is(busy){
            io.scoreWriteEnable := true.B
            when(scoreIDReg === 0.U){
                when(scoreWriteDoneReg){
                    scoreIDReg := scoreIDReg + 1.U
                }
                io.writeAdress := 16.U
                io.writeTileID := scoreRightDigit + 16.U
            }.elsewhen(scoreIDReg === 1.U){
                when(scoreWriteDoneReg){
                    scoreIDReg := scoreIDReg + 1.U
                }
                io.writeAdress := 15.U
                io.writeTileID := scoreMiddleDigit + 16.U
            }.elsewhen(scoreIDReg === 2.U){
                when(scoreWriteDoneReg){
                    scoreIDReg := scoreIDReg + 1.U
                    stateReg := done
                }
                io.writeAdress := 14.U
                io.writeTileID := scoreLeftDigit + 16.U
            }
        }
        is(done){

        }
    }
    
}
