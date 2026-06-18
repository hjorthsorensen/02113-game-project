import chisel3._
import chisel3.util._

class LoadingScreenFSM extends Module{
    val io = IO(new Bundle {
        //work signal from backgroundHandler
        val work = Input(Bool())

        //Adress and tileID to bgHandler
        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(6.W))
        val writingLoading = Output(Bool())
        
        //Done signal to bgHandler
        val done = Output(Bool())

    })
    
    val loadingStateReg    = RegInit(VecInit.fill(7)(false.B))
    val loadIconID         = (56).U
    val loadIconBackID     = (57).U
    val defaultAdressLoad  = 305.U

    //Method to determine empty or filled dot for loading screen
    def whichTileID(ID : Bool):(UInt) = {
        (
            Mux(ID, loadIconID, loadIconBackID)
        )
    }

    //Registers
    val writingTile     = RegInit(0.U(3.W))
    val fpsCount        = RegInit(0.U(7.W))

    io.writeAdress     := 0.U
    io.writeTileID     := 0.U
    io.writingLoading  := false.B
    io.done            := false.B



    val idle :: busy :: calc :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }

        }
        is(busy){
            io.writingLoading := true.B
            when(writingTile === 0.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (306).U
                io.writeTileID := whichTileID(loadingStateReg(0))
            }
            when(writingTile === 1.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (307).U
                io.writeTileID := whichTileID(loadingStateReg(1))
            }
            when(writingTile === 2.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (308).U
                io.writeTileID := whichTileID(loadingStateReg(2))
            }
            when(writingTile === 3.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (309).U
                io.writeTileID := whichTileID(loadingStateReg(3))
            }
            when(writingTile === 4.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (310).U
                io.writeTileID := whichTileID(loadingStateReg(4))
            }
            when(writingTile === 5.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (311).U
                io.writeTileID := whichTileID(loadingStateReg(5))
            }
            when(writingTile === 6.U){
                writingTile := 0.U
                stateReg := calc
                io.writeAdress := (312).U
                io.writeTileID := whichTileID(loadingStateReg(6))
            }

        }
        is(calc){
            when(fpsCount > 10.U){
                loadingStateReg(0) := true.B
            }.otherwise{
                loadingStateReg(0) := false.B
            }
            
            when(fpsCount > 20.U){
                loadingStateReg(1) := true.B
            }.otherwise{
                loadingStateReg(1) := false.B
            }
            
            when(fpsCount > 30.U){
                loadingStateReg(2) := true.B
            }.otherwise{
                loadingStateReg(2) := false.B
            }
            
            when(fpsCount > 40.U){
                loadingStateReg(3) := true.B
            }.otherwise{
                loadingStateReg(3) := false.B
            }
            
            when(fpsCount > 50.U){
                loadingStateReg(4) := true.B
            }.otherwise{
                loadingStateReg(4) := false.B
            }
            
            when(fpsCount > 60.U){
                loadingStateReg(5) := true.B
            }.otherwise{
                loadingStateReg(5) := false.B
            }

            when(fpsCount > 70.U){
                loadingStateReg(6) := true.B
            }.otherwise{
                loadingStateReg(6) := false.B
            }


            when(fpsCount > 100.U){
                fpsCount := 0.U
            }
            stateReg  := done
        }
        is(done){
            fpsCount  := fpsCount + 1.U
            io.done   := true.B
            stateReg  := idle
        }
    }
  
}
