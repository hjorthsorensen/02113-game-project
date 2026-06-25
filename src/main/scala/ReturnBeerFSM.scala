import chisel3._
import chisel3.util._

class ReturnBeerFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        //Work signal
        val work               = Input(Bool())
        val isBeerCatched      = Input(Bool())

        //Which costumer is returning
        val returnCustomer1    = Input(Bool())
        val returnCustomer2    = Input(Bool())

        //Positions
        val customer1XPos      = Input(SInt(11.W))
        val customer1YPos      = Input(SInt(10.W))
        val customer2XPos      = Input(SInt(11.W))
        val customer2YPos      = Input(SInt(10.W))
        
        //Outputs for beer movement
        val returnBeerXPos     = Output(SInt(11.W))
        val returnBeerYPos     = Output(SInt(10.W))
        val beerVisible        = Output(Bool())
        
        //Valid signal for beer
        val beerReturnValid    = Output(Bool())
        //Done signal for main FSMD
        val done               = Output(Bool())

    })

    //FSMD states
    val idle :: busy :: done :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    //Registers for calculations
    val returnBeerXPosReg = RegInit(0.S(11.W))
    val returnBeerYPosReg = RegInit(0.S(10.W))

    val returnBeerX1PosPrevReg = RegInit(0.S(11.W))
    val returnBeerY1PosPrevReg = RegInit(0.S(10.W))
    val returnBeerX2PosPrevReg = RegInit(0.S(11.W))
    val returnBeerY2PosPrevReg = RegInit(0.S(10.W))

    when(io.customer1XPos =/= 0.S){
        returnBeerX1PosPrevReg := io.customer1XPos
    }
    when(io.customer2XPos =/= 0.S){
        returnBeerX2PosPrevReg := io.customer2XPos
    }
    when(io.customer1YPos =/= 0.S){
        returnBeerY1PosPrevReg := io.customer1YPos + 32.S
    }
    when(io.customer2YPos =/= 0.S){
        returnBeerY2PosPrevReg := io.customer2YPos + 32.S
    }
    


    val beerVisibleReg            = RegInit(false.B)
    val beerReturnValidReg        = RegInit(false.B)
    val returnBeerSpeedReg        = RegInit(0.S(8.W))

    val returnCustomer1RegQueue   = RegInit(false.B)
    val returnCustomer2RegQueue   = RegInit(false.B)

    val returningCustomer1        = RegInit(false.B)
    val returningCustomer2        = RegInit(false.B)
       
    val fpsReg                    = RegInit(0.U(2.W))
    val idleC                     = RegInit(0.U(8.W))



    //Adds a costumer to queue when they are done scoring.
    when(io.returnCustomer1 && !returnCustomer1RegQueue){
        returnCustomer1RegQueue := true.B
    }
    when(io.returnCustomer2 && !returnCustomer2RegQueue){
        returnCustomer2RegQueue := true.B
    }
 

    //.io connections and default outputs
    io.done                := false.B
    io.returnBeerXPos      := returnBeerXPosReg
    io.returnBeerYPos      := returnBeerYPosReg
    io.beerVisible         := beerVisibleReg
    io.beerReturnValid     := beerReturnValidReg


    switch(stateReg){
        is(idle){
            when(io.work){
                stateReg := busy
                //Default assignment of returning beer based on which costumer is returning
                //Costumer 1
                when(!beerVisibleReg && returnCustomer1RegQueue){
                    beerVisibleReg         := true.B
                    beerReturnValidReg     := true.B
                    //Conditional speed, closest is a bit slower
                    when(returnBeerX1PosPrevReg < 280.S){
                        returnBeerSpeedReg := 28.S
                    }.otherwise{
                        returnBeerSpeedReg := 20.S
                    }
                    returnBeerXPosReg      := returnBeerX1PosPrevReg
                    returnBeerYPosReg      := returnBeerY1PosPrevReg
                    returningCustomer1     := true.B
                //Costumer 2
                }.elsewhen(!beerVisibleReg && returnCustomer2RegQueue){
                    beerVisibleReg         := true.B
                    beerReturnValidReg     := true.B
                    returnBeerXPosReg      := returnBeerX2PosPrevReg
                    returnBeerYPosReg      := returnBeerY2PosPrevReg
                    //Conditional speed, closest is a bit slower
                    when(returnBeerX2PosPrevReg < 280.S){
                        returnBeerSpeedReg := 28.S
                    }.otherwise{
                        returnBeerSpeedReg := 20.S
                    }
                    returningCustomer2 := true.B
                }
            }
        }
        is(busy){
            //Only when visible: update and move beer.

            when(beerVisibleReg){
                //Beer movement
                returnBeerXPosReg := returnBeerXPosReg + returnBeerSpeedReg
                when(fpsReg === 1.U){//delay for slower speed update.
                    returnBeerSpeedReg := returnBeerSpeedReg - 1.S
                }
                returnBeerSpeedReg := returnBeerSpeedReg - 1.S

                //catch or returnbeer was not catched condition, costumer based.
                when((returnBeerXPosReg >= 512.S || io.isBeerCatched) && returningCustomer1){
                    beerVisibleReg             := false.B
                    beerReturnValidReg         := false.B
                    returnCustomer1RegQueue    := false.B
                    returningCustomer1         := false.B

                }.elsewhen((returnBeerXPosReg >= 512.S || io.isBeerCatched) && returningCustomer2){
                    beerVisibleReg             := false.B
                    beerReturnValidReg         := false.B
                    returnCustomer2RegQueue    := false.B
                    returningCustomer2         := false.B

                }

            }
            stateReg := done
        }
        is(done){
            fpsReg    := fpsReg + 1.U
            stateReg  := idle
            io.done   := true.B
        }
    }

}
