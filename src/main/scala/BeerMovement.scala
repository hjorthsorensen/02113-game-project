import chisel3._
import chisel3.util._

class BeerMovement extends Module{
    val io = IO(new Bundle {
        //Inputs
        val speed = Input(SInt(8.W))
        val work = Input(Bool())
        val beerYPosInp = Input(SInt(10.W))
        
        //Outputs for beer movement
        val beerXPos = Output(SInt(11.W))
        val beerYPos = Output(SInt(10.W))
        val beerVisible = Output(Bool())
        
        //Ready and valid signals for beer
        val beerValid = Output(Bool())
        val beerReady = Output(Bool())
        val done = Output(Bool())

    })
    //FSMD states
    val idle :: busy :: doneMovement :: Nil = Enum(3)
    val stateReg = RegInit(idle)
    //Registers for calculations
    val fpsReg = RegInit(0.U(8.W))
    val inCalc = RegInit(false.B)
    
    //Registers for beer movement
    val remainSpeed = RegInit(0.S(8.W))
    val beerXReg = RegInit(500.S(11.W))
    val beerYReg = RegInit(200.S(10.W))
    val beerVisibleReg = RegInit(false.B)
    
    val beerReadyReg = RegInit(true.B)


    //.io connections and default outputs
    io.done := false.B
    io.beerValid := false.B

    io.beerReady := beerReadyReg
    io.beerVisible := beerVisibleReg
    io.beerXPos := beerXReg
    io.beerYPos := beerYReg

    //Calculating when the beer movement is done
    val doneCalc = remainSpeed === 0.S

    //FSMD switch
    switch(stateReg){
        is(idle){
            when(io.work){
                //When the beer has been thrown and the movement is done, make the beer invisible and set it ready for the next throw
                when(!inCalc && doneCalc && (fpsReg === 120.U)){
                    beerVisibleReg := false.B
                    beerReadyReg := true.B   
                }
                //When the beer is stationary, we are now not in calculation, and the beer is ready for the next throw
                when(inCalc && doneCalc){
                   inCalc := false.B
                }
                //Start the beer movement when the input speed is not zero, the beer is ready, and we are not currently calculating a movement

                when(!inCalc && (io.speed =/= 0.S) && beerReadyReg){
                    //Initialize the values for the beer movement based on the input speed and the sprite's Y position
                    fpsReg := 0.U
                    beerVisibleReg := true.B
                    beerReadyReg := false.B
                    inCalc := true.B
                    remainSpeed := io.speed
                    beerXReg := 500.S
                    beerYReg := io.beerYPosInp
                }
                stateReg := busy
            }
            
        }
        is(busy){
            //Update the beers position and remaining speed every frame until the movement is done
            when(!doneCalc && inCalc){
                remainSpeed := remainSpeed - 1.S
                beerXReg := beerXReg - remainSpeed
            }
            stateReg := doneMovement
        }            
        
        is(doneMovement){
            fpsReg := fpsReg + 1.U
            stateReg := idle
            io.done := true.B
            //Only send the beer valid signal for one cycle when the movement is done and we are still in calculation 
            //(to avoid multiple score increments for the same beer)
            when(doneCalc && inCalc){
                io.beerValid := true.B
            }
        }
    }
}
