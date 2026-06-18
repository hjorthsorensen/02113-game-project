import chisel3._
import chisel3.util._

class BeerLeftFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        val score           = Input(UInt(8.W))
        val work            = Input(Bool())
        
        
        //Outputs to backgroundHandler
        val writeAdress     = Output(UInt(10.W))
        val writeTileID     = Output(UInt(6.W))
        val writingScore    = Output(Bool())
        
        //Done signal to backgroundHandler
        val done            = Output(Bool())
        

    })

    val idle :: calcDigits :: busy :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)
    // val tile1 :: tile2 :: tile3 :: Nil = Enum(3)
    // val tileReg = RegInit(tile1)
    val scoreReg = RegInit(0.U(8.W))

    val RightDigitReg = RegInit(0.U(4.W))
    val MiddleDigitReg = RegInit(0.U(4.W))
    // val wireScore = cat(LeftLeftDigitReg, LeftDigitReg, MiddleDigitReg, RightDigitReg,scoreReg)

   def doubleDabble(score: UInt): (UInt, UInt) = {
    val shiftRegInit = Cat(0.U(8.W), score(7, 0)) // 16bit digits + 16bit input
    var shiftReg = shiftRegInit

    for (i <- 0 until 8) {
      val bcd = shiftReg(15, 8)
    
      val ones = bcd(3, 0)
      val tens = bcd(7, 4)
    
      val onesAdj = Mux(ones >= 5.U, ones + 3.U, ones)
      val tensAdj = Mux(tens >= 5.U, tens + 3.U, tens)
    
      val newBcd = Cat(tensAdj, onesAdj)
    
      shiftReg = Cat(newBcd, shiftReg(7, 0)) << 1
    }
    val result = shiftReg(15, 8)
    (// Return the 4 base10 digits.
      result(3, 0), // ones
      result(7, 4) // tens
    )
    }
    

    val scoreIDReg = RegInit(0.U(2.W))
    // val scoreWriteDoneReg = RegInit(false.B)
    // scoreWriteDoneReg :=
    val scoreWriteDoneReg = RegInit(false.B)
    val waitReg = RegInit(0.U(5.W))


    io.done := scoreWriteDoneReg
    io.writeAdress := 0.U
    io.writeTileID := 0.U
    io.writingScore := false.B
    

    switch(stateReg){
        is(idle){
            scoreWriteDoneReg := false.B
            when(io.work){
                scoreReg := io.score
                stateReg := calcDigits
            }
        }
        is(calcDigits){
            //Double Dabble algorithm for BCD conversion
            val (right, middle) = doubleDabble(scoreReg)
            RightDigitReg := right
            MiddleDigitReg := middle

            //Wait a few cycles before writing to the score board to ensure the digits are calculated and stable.
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
                io.writeAdress := (16 + 40).U
                io.writeTileID := RightDigitReg + 16.U
            }.elsewhen(scoreIDReg === 1.U){
                scoreIDReg := scoreIDReg + 1.U
                io.writeAdress := (15 + 40).U
                io.writeTileID := MiddleDigitReg + 16.U
                stateReg := done
            }
        }
        is(done){
            io.done := true.B
            scoreWriteDoneReg := true.B
            scoreIDReg := 0.U
            stateReg := idle
        }
    }
    
}
