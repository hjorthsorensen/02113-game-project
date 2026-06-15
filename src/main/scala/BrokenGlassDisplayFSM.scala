import chisel3._
import chisel3.util._

class BrokenGlassDisplayFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        val work = Input(Bool())
        val beerBroken = Input(Bool())
        val tableID = Input(UInt(2.W))
        
        
        //Outputs for background
        // val scoreTileAmount = Output(UInt(4.W))
        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(5.W))
        val writingBrokenGlass = Output(Bool())
        
        val done = Output(Bool())

    })

    val idle :: busy :: finished :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    // val tile1 :: tile2 :: tile3 :: Nil = Enum(3)
    // val tileReg = RegInit(tile1)
    val brokenGlassReg = RegInit(false.B)
    val brokenGlassWriteDoneReg = RegInit(false.B)

    val table1 = (40*7+1).U
    val table2 = (40*9+1).U
    val table3 = (40*11+1).U
    val table4 = (40*13+1).U

    io.done := brokenGlassWriteDoneReg
    io.writeAdress := 0.U
    io.writeTileID := 0.U
    io.writingBrokenGlass := false.B

    // val tile1BrokenReg := RegInit(false.B)
    // val tile2BrokenReg := RegInit(false.B) 
    // val tile3BrokenReg := RegInit(false.B)
    // val tile4BrokenReg := RegInit(false.B)



    

    switch(stateReg){
        is(idle){
            brokenGlassWriteDoneReg := false.B
            when(io.work){
                when(io.beerBroken && !brokenGlassReg){
                    stateReg := busy
                    brokenGlassReg := true.B
                }.otherwise{
                    stateReg := finished
                }
            }
        }   
        is(busy){
            stateReg := finished
            io.writingBrokenGlass := true.B
            brokenGlassReg := false.B
            when(io.tableID === 0.U) { 
                io.writeAdress := table1
                io.writeTileID := 29.U
            }.elsewhen(io.tableID === 1.U) {
                io.writeAdress := table2
                io.writeTileID := 29.U
            }.elsewhen(io.tableID === 2.U) {
                io.writeAdress := table3
                io.writeTileID := 29.U
            }.elsewhen(io.tableID === 3.U) {
                io.writeAdress := table4
                io.writeTileID := 29.U
            }
        }

        is(finished){     
            brokenGlassWriteDoneReg := true.B
            stateReg := idle
        }
    }
    
}
