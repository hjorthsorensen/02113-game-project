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

    val idle :: busy :: calcIdle :: finished :: Nil = Enum(4)
    val stateReg = RegInit(idle)
    // val tile1 :: tile2 :: tile3 :: Nil = Enum(3)
    // val tileReg = RegInit(tile1)
    val brokenGlassReg = RegInit(false.B)
    val brokenGlassWriteDoneReg = RegInit(false.B)

    def risingEdge(signal: Bool): Bool = signal && !RegNext(signal)
    val table1 = (40*7+1).U
    val table2 = (40*9+1).U
    val table3 = (40*11+1).U
    val table4 = (40*13+1).U

    when(risingEdge(io.beerBroken)){
        brokenGlassReg := true.B
    }

    io.done := brokenGlassWriteDoneReg
    io.writeAdress := 0.U
    io.writeTileID := 0.U
    io.writingBrokenGlass := false.B

    val tile1BrokenReg = RegInit(false.B)
    val tile2BrokenReg = RegInit(false.B) 
    val tile3BrokenReg = RegInit(false.B)
    val tile4BrokenReg = RegInit(false.B)
    val tile1Cnt = RegInit(0.U(8.W))
    val tile2Cnt = RegInit(0.U(8.W))
    val tile3Cnt = RegInit(0.U(8.W))
    val tile4Cnt = RegInit(0.U(8.W))



    

    switch(stateReg){
        is(idle){
            brokenGlassWriteDoneReg := false.B
            when(io.work){
                when(io.beerBroken){// && !brokenGlassReg
                    stateReg := busy
                    // brokenGlassReg := true.B
                }.otherwise{
                    stateReg := calcIdle
                }
            }
        }   
        is(busy){
            stateReg := calcIdle
            io.writingBrokenGlass := true.B
            // brokenGlassReg := false.B
            when(io.tableID === 0.U && !tile1BrokenReg) { 
                io.writeAdress := table1
                io.writeTileID := 29.U
                tile1BrokenReg := true.B
            }.elsewhen(io.tableID === 1.U && !tile2BrokenReg) {
                io.writeAdress := table2
                io.writeTileID := 29.U
                tile2BrokenReg := true.B
            }.elsewhen(io.tableID === 2.U && !tile3BrokenReg) {
                io.writeAdress := table3
                io.writeTileID := 29.U
                tile3BrokenReg := true.B
            }.elsewhen(io.tableID === 3.U && !tile4BrokenReg) {
                io.writeAdress := table4
                io.writeTileID := 29.U
                tile4BrokenReg := true.B
            }

            
        }
        is(calcIdle){
            io.writingBrokenGlass := true.B
            when(tile1Cnt === 180.U){
                tile1Cnt := 0.U
                io.writeAdress := table1
                io.writeTileID := 13.U
                tile1BrokenReg := false.B
            }
            when(tile2Cnt === 180.U){
                tile2Cnt := 0.U
                io.writeAdress := table2
                io.writeTileID := 13.U
                tile1BrokenReg := false.B
            }
            when(tile3Cnt === 180.U){
                tile3Cnt := 0.U
                io.writeAdress := table3
                io.writeTileID := 13.U
                tile1BrokenReg := false.B
            }
            when(tile4Cnt === 180.U){
                tile4Cnt := 0.U
                io.writeAdress := table4
                io.writeTileID := 13.U
                tile1BrokenReg := false.B
            }
            stateReg := finished

        }

        is(finished){
            when(tile1BrokenReg){
                tile1Cnt := tile1Cnt + 1.U
            }
            when(tile2BrokenReg){
                tile2Cnt := tile2Cnt + 1.U
            }
            when(tile3BrokenReg){
                tile3Cnt := tile3Cnt + 1.U
            }
            when(tile4BrokenReg){
                tile4Cnt := tile4Cnt + 1.U
            }
            
            brokenGlassWriteDoneReg := true.B
            stateReg := idle
        }
    }
    
}
