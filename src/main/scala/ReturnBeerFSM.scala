class ReturnBeerFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        
        val work = Input(Bool())
        val returnTrue = Input(Bool())
        val isBeerCatched = Input(Bool())
        val costumerXPos = Input(SInt(10.W))
        val costumerYPos = Input(SInt(10.W))
        
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
                when(!beerVisibleReg && io.returnTrue){
                    beerVisibleReg := true.B
                    beerReturnValidReg := true.B
                    returnBeerXPosReg := io.costumerXPos
                    returnBeerYPosReg := io.costumerYPos
                    returnBeerSpeedReg := 30.S
                }
                
            }
        }
        is(busy){
            when(beerVisibleReg){
                returnBeerXPosReg := returnBeerXPosReg + returnBeerSpeedReg
                returnBeerSpeedReg := returnBeerSpeedReg - 1.S
                when(returnBeerXPosReg >= 512.S || io.isBeerCatched){
                    beerVisibleReg := false.B
                    beerReturnValidReg := false.B
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
