import chisel3._
import chisel3.util._

class ReturnBeerFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        
        val work = Input(Bool())
        val isBeerCatched = Input(Bool())

        val returnCustomer1 = Input(Bool())
        val returnCustomer2 = Input(Bool())

        val customer1XPos = Input(SInt(11.W))
        val customer1YPos = Input(SInt(10.W))
        val customer2XPos = Input(SInt(11.W))
        val customer2YPos = Input(SInt(10.W))
        
        //Outputs for beer movement
        val returnBeerXPos = Output(SInt(11.W))
        val returnBeerYPos = Output(SInt(10.W))
        val beerVisible = Output(Bool())
        
        //Valid signal for beer
        val beerReturnValid = Output(Bool())
        val done = Output(Bool())

    })

    //FSMD states
    val idle :: busy :: done :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    //Registers for calculations
    val returnBeerXPosReg = RegInit(0.S(11.W))
    val returnBeerYPosReg = RegInit(0.S(10.W))
    val beerVisibleReg = RegInit(false.B)
    val beerReturnValidReg = RegInit(false.B)
    val returnBeerSpeedReg = RegInit(0.S(8.W))

    val returnCustomer1Reg = RegInit(false.B)
    val returnCustomer2Reg = RegInit(false.B)

    def risingEdge(signal: Bool): Bool = signal && !RegNext(signal)
    when(risingEdge(io.returnCustomer1)){
        returnCustomer1Reg := true.B
    }
    when(risingEdge(io.returnCustomer2)){
        returnCustomer2Reg := true.B
    }

    //.io connections and default outputs
    io.done := false.B
    io.returnBeerXPos := returnBeerXPosReg
    io.returnBeerYPos := returnBeerYPosReg
    io.beerVisible := beerVisibleReg
    io.beerReturnValid := beerReturnValidReg



    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
                when(!beerVisibleReg && returnCustomer1Reg){
                    beerVisibleReg := true.B
                    beerReturnValidReg := true.B
                    returnBeerXPosReg := io.customer1XPos
                    returnBeerYPosReg := io.customer1YPos
                    returnBeerSpeedReg := 30.S
                }.elsewhen(!beerVisibleReg && returnCustomer2Reg){
                    beerVisibleReg := true.B
                    beerReturnValidReg := true.B
                    returnBeerXPosReg := io.customer2XPos
                    returnBeerYPosReg := io.customer2YPos
                    returnBeerSpeedReg := 30.S
                }
            }
        }
        is(busy){
            when(beerVisibleReg){
                returnBeerXPosReg := returnBeerXPosReg + returnBeerSpeedReg
                returnBeerSpeedReg := returnBeerSpeedReg - 1.S
                when(returnBeerXPosReg >= 512.S || io.isBeerCatched || returnBeerSpeedReg <= 0.S){
                    beerVisibleReg := false.B
                    beerReturnValidReg := false.B
                    returnCustomer1Reg := false.B
                    returnCustomer2Reg := false.B
                }
            }
            stateReg := done
        }
        is(done){
            stateReg := idle
            io.done := true.B
        }
    }

}
