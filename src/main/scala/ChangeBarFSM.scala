import chisel3._
import chisel3.util._

class ChangeBarFSM extends Module{
  val io = IO(new Bundle {
        val work       = Input(Bool())
        val changingBar = Input(Bool())
        
        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(6.W))
        val writingBG = Output(Bool())

        val done       = Output(Bool())
    })

    //bar stage 0 or 1
    val barID = RegInit(false.B)

    val changeBarReg = RegInit(false.B)
    
    def risingEdge(signal: Bool): Bool = !signal && RegNext(signal)
    when(!changeBarReg && risingEdge(io.changingBar)){
        changeBarReg := true.B
    }

    //logic for handling tileID and adress
    val tileAdressInLine = RegInit(0.U(10.W))
    val offset = RegInit(40.U(10.W))
    val tileAdress = (tileAdressInLine + offset)

    val tileMapIDCounter = RegInit(0.U(5.W))
    //NewBar - change value(ID) of walls, floor to new tiles for change in scene these are random right now except default sky
    val tileIDTable1 = VecInit(Seq(
        26.U(6.W), //Default sky -- Important
        2.U(6.W), //Walls (windows)
        5.U(6.W), //Floor (Negroni)
        // 10.U(6.W), //Tables (table)
        15.U(6.W),  //Bar counter (bar counter)
        8.U(6.W), //Beer Pinup poster
        7.U(6.W), // Marlboro poster
        14.U(6.W) //Keg (keg)
    ))
    //Default bar
    val tileIDTable2 = VecInit(Seq(
        26.U(6.W), //Default sky
        1.U(6.W), //Walls (windows)
        13.U(6.W), //Floor (Negroni)
        // 10.U(6.W), //Tables (table)
        15.U(6.W),  //Bar counter (bar counter)
        8.U(6.W), //Beer Pinup poster
        7.U(6.W), // Marlboro poster
        14.U(6.W) //Keg (keg)
    ))

    val isTable = (tileAdressInLine >= 2.U && tileAdressInLine <= 15.U) && (offset === 280.U || offset === 360.U  || offset === 440.U || offset === 520.U)

    //Constants cycling between tile 0-19 and adding offset 40 to only cycle on the left half
    when(tileAdressInLine =/= 19.U){
        tileAdressInLine := tileAdressInLine + 1.U
    }.otherwise{
        tileAdressInLine := 0.U
        offset := offset + 40.U
    }
    

    //FSM and io connection
    val idle :: walls ::floor:: counter :: details :: done :: Nil = Enum(6)
    val stateReg = RegInit(idle)

    io.writeAdress := 0.U
    
    when(barID){
        io.writeTileID := tileIDTable1(tileMapIDCounter)
    }.otherwise{
        io.writeTileID := tileIDTable2(tileMapIDCounter)
    }
    
    io.writingBG := (stateReg =/= idle)
    io.done := false.B

    switch(stateReg){
        is(idle){
            when(io.work && changeBarReg){
                stateReg := walls
                tileMapIDCounter := 1.U
                tileAdressInLine := 0.U
                offset := 80.U
                barID := !barID
            }.otherwise{
                stateReg := done
            }

        }
        is(walls){
            when(!isTable){
                io.writeAdress := tileAdress
            }
            
            when(tileAdress >= 579.U){
                stateReg := floor
                tileMapIDCounter := tileMapIDCounter + 1.U
                offset := 200.U
                tileAdressInLine := 0.U
            }


        }
        is(floor){
            when (tileAdressInLine >= 1.U && tileAdressInLine <= 18.U && !isTable){
                io.writeAdress := tileAdress
            }
            when(tileAdress >= 579.U){
                stateReg := counter
                tileMapIDCounter := tileMapIDCounter + 1.U
                offset := 240.U
                tileAdressInLine := 0.U
            }

        }
        // is(table){
        //     when (isTable){
        //         io.writeAdress := tileAdress
        //     }
        //     when(tileAdress >= 579.U){
        //         stateReg := counter
        //         tileMapIDCounter := tileMapIDCounter + 1.U
        //         offset := 240.U
        //         tileAdressInLine := 0.U
        //     }

        // }
        is(counter){
            when(tileAdressInLine === 18.U){
                io.writeAdress := tileAdress
            }
            when(tileAdress >= 579.U){
                stateReg := details
                tileMapIDCounter := tileMapIDCounter + 1.U
                offset := 80.U
                tileAdressInLine := 0.U
            }

        }
        is(details){
            when(tileAdress === 123.U){
                io.writeAdress := tileAdress
                tileMapIDCounter := tileMapIDCounter + 1.U
            }
            when(tileAdress === 131.U){
                io.writeAdress := tileAdress
                tileMapIDCounter := tileMapIDCounter + 1.U
            }

            when(tileAdress === 218.U){
                io.writeAdress := tileAdress
                tileMapIDCounter := tileMapIDCounter + 1.U
            }

            when(tileAdress >= 579.U){
                stateReg := done
                tileMapIDCounter := 0.U
                changeBarReg := false.B
            }

        }
        is(done){
            io.done := true.B
            stateReg := idle
        }
    }
}
