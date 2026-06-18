import chisel3._
import chisel3.util._

class LoadingScreenFSM extends Module{
    val io = IO(new Bundle {
        val work = Input(Bool())

        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(5.W))
        val writingLoading = Output(Bool())

        val done = Output(Bool())

    })

    val loadingStateReg = RegInit(VecInit.fill(6)(false.B))
    val loadIconID = (4+16).U
    val loadIconBackID = (5+16).U
    val defaultAdressLoad = 305.U

    def whichTileID(ID : Bool):(UInt) = {
        val result = WireDefault(0.U(5.W))
        when(ID){
            result := loadIconID
        }.otherwise{
            result := loadIconBackID
        }
        (
            result
        )
    }

    //Registers
    val writingTile = RegInit(0.U(3.W))
    val fpsCount = RegInit(0.U(6.W))

    io.writeAdress := 0.U
    io.writeTileID := 0.U
    io.writingLoading := false.B
    io.done := false.B



    val idle :: busy :: calc :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
            }

        }
        is(busy){
            stateReg := calc
            io.writingLoading := true.B
            when(writingTile === 0.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (305).U
                io.writeTileID := whichTileID(loadingStateReg(0))
            }
            when(writingTile === 1.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (306).U
                io.writeTileID := whichTileID(loadingStateReg(1))
            }
            when(writingTile === 2.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (307).U
                io.writeTileID := whichTileID(loadingStateReg(2))
            }
            when(writingTile === 3.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (308).U
                io.writeTileID := whichTileID(loadingStateReg(3))
            }
            when(writingTile === 4.U){
                writingTile := writingTile + 1.U
                io.writeAdress := (309).U
                io.writeTileID := whichTileID(loadingStateReg(4))
            }
            when(writingTile === 5.U){
                writingTile := 0.U
                stateReg := done
                io.writeAdress := (310).U
                io.writeTileID := whichTileID(loadingStateReg(5))
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
            stateReg := done
        }
        is(done){
            fpsCount := fpsCount + 1.U
            io.done := true.B
        }
    }
  
}
