import chisel3._
import chisel3.util._

class ScoreBoardDisplayFSM extends Module{
    val io = IO(new Bundle {
        //Inputs from ScoreFSM and BGHandler
        val score           = Input(UInt(16.W))
        val work            = Input(Bool())
        
        
        //Outputs for backgroundhandler
        val writeAdress    = Output(UInt(10.W))
        val writeTileID    = Output(UInt(6.W))
        val writingScore   = Output(Bool())

        //Done signal to BGHandler
        val done           = Output(Bool())

    })

    val idle :: calcDigits :: busy :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)


    val scoreReg            = RegInit(0.U(16.W))

    val RightDigitReg       = RegInit(0.U(4.W))
    val MiddleDigitReg      = RegInit(0.U(4.W))
    val LeftDigitReg        = RegInit(0.U(4.W))
    val LeftLeftDigitReg    = RegInit(0.U(4.W))

    //Double Dabble for 4 digits
   def doubleDabble(score: UInt): (UInt, UInt, UInt, UInt) = {
    val shiftRegInit = Cat(0.U(16.W), score(15, 0)) // 16bit digits + 16bit input
    var shiftReg = shiftRegInit
    
    //Repeat for 16 cycles, corresponding to the 16 times we are shifting into the bcd wire
    for (i <- 0 until 16) {
        val bcd = shiftReg(31, 16) // Wire to hold the digits
    
    //values of bcd before adding 3
        val ones          = bcd(3, 0)
        val tens          = bcd(7, 4)
        val hundreds      = bcd(11, 8)
        val thousands     = bcd(15, 12)
    //Values after adding 3
        val onesAdj       = Mux(ones >= 5.U, ones + 3.U, ones)
        val tensAdj       = Mux(tens >= 5.U, tens + 3.U, tens)
        val hundredsAdj   = Mux(hundreds >= 5.U, hundreds + 3.U, hundreds)
        val thousandsAdj  = Mux(thousands >= 5.U, thousands + 3.U, thousands)
    //Collect the wires to the new BCD
        val newBcd        = Cat(thousandsAdj, hundredsAdj, tensAdj, onesAdj)
    //Assign the shift reg the new wire of digits and the shiftReg.
        shiftReg          = Cat(newBcd, shiftReg(15, 0)) << 1
    }
    //Output for digits
    val result = shiftReg(31, 16)
    (// Return the 4 base10 digits.
        result(3, 0), // ones
        result(7, 4), // tens
        result(11, 8), // hundreds
        result(15, 12) // thousands
    )
    }
    
    //ID for which digit we are writing to
    val scoreIDReg         = RegInit(0.U(3.W))
    //waitReg for waiting for the calculation of the digits for doubleDabble
    val waitReg            = RegInit(0.U(5.W))

    //Default assignments for io
    io.done               := false.B
    io.writeAdress        := 0.U
    io.writeTileID        := 0.U
    io.writingScore       := false.B
    

    switch(stateReg){
        is(idle){
            when(io.work){
                scoreReg := io.score
                stateReg := calcDigits
            }
        }
        is(calcDigits){
            //Double Dabble algorithm for BCD conversion
            val (right, middle, left, leftLeft) = doubleDabble(scoreReg)
            RightDigitReg     := right
            MiddleDigitReg    := middle
            LeftDigitReg      := left
            LeftLeftDigitReg  := leftLeft

            //Wait 20 cycles before writing to the scoreboard to ensure the digits are calculated
            waitReg := waitReg + 1.U
            when(waitReg === 20.U){
                waitReg := 0.U
                stateReg := busy
            }
            
        }
        is(busy){
            io.writingScore := true.B
            when(scoreIDReg === 0.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 17.U
                io.writeTileID := RightDigitReg + 16.U
            }.elsewhen(scoreIDReg === 1.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 16.U
                io.writeTileID := MiddleDigitReg + 16.U
            }.elsewhen(scoreIDReg === 2.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 15.U
                io.writeTileID := LeftDigitReg + 16.U
            }.elsewhen(scoreIDReg === 3.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 14.U
                io.writeTileID := LeftLeftDigitReg + 16.U
            }.elsewhen(scoreIDReg === 4.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 850.U
                io.writeTileID := RightDigitReg + 16.U
            }.elsewhen(scoreIDReg === 5.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 849.U
                io.writeTileID := MiddleDigitReg + 16.U
            }.elsewhen(scoreIDReg === 6.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := 848.U
                io.writeTileID := LeftDigitReg + 16.U
            }.elsewhen(scoreIDReg === 7.U){
                stateReg := done
                io.writeAdress := 847.U
                io.writeTileID := LeftLeftDigitReg + 16.U
            }
        }
        is(done){
            io.done      := true.B
            scoreIDReg   := 0.U
            stateReg     := idle
        }
    }
    
}
