import chisel3._
import chisel3.util._

class ScoreBoardDisplayFSM extends Module{
    val io = IO(new Bundle {
        //Inputs
        val score = Input(UInt(16.W))
        val work = Input(Bool())
        
        
        //Outputs for background
        // val scoreTileAmount = Output(UInt(4.W))
        val writeAdress = Output(UInt(10.W))
        val writeTileID = Output(UInt(5.W))
        val writingScore = Output(Bool())
        
        val done = Output(Bool())

    })

    val idle :: calcDigits :: busy :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)
    // val tile1 :: tile2 :: tile3 :: Nil = Enum(3)
    // val tileReg = RegInit(tile1)
    val scoreReg = RegInit(0.U(16.W))

    val RightDigitReg = RegInit(0.U(4.W))
    val MiddleDigitReg = RegInit(0.U(4.W))
    val LeftDigitReg = RegInit(0.U(4.W))
    val LeftLeftDigitReg = RegInit(0.U(4.W))
    // val wireScore = cat(LeftLeftDigitReg, LeftDigitReg, MiddleDigitReg, RightDigitReg,scoreReg)

   def doubleDabble(score: UInt): (UInt, UInt, UInt, UInt) = {
    val shiftRegInit = Cat(0.U(16.W), score(15, 0)) // 16b BCD + 16b input
    var shiftReg = shiftRegInit
    
    for (i <- 0 until 16) {
      val bcd = shiftReg(31, 16)
    
      val ones      = bcd(3, 0)
      val tens      = bcd(7, 4)
      val hundreds  = bcd(11, 8)
      val thousands = bcd(15, 12)
    
      val onesAdj      = Mux(ones      >= 5.U, ones      + 3.U, ones)
      val tensAdj      = Mux(tens      >= 5.U, tens      + 3.U, tens)
      val hundredsAdj  = Mux(hundreds  >= 5.U, hundreds  + 3.U, hundreds)
      val thousandsAdj = Mux(thousands >= 5.U, thousands + 3.U, thousands)
    
      val newBcd = Cat(thousandsAdj, hundredsAdj, tensAdj, onesAdj)
    
      shiftReg = Cat(newBcd, shiftReg(15, 0)) << 1
    }
    val result = shiftReg(31, 16)
    (
      result(3, 0),    // ones
      result(7, 4),    // tens
      result(11, 8),   // hundreds
      result(15, 12)   // thousands
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
            val (right, middle, left, leftLeft) = doubleDabble(scoreReg)
            RightDigitReg := right
            MiddleDigitReg := middle
            LeftDigitReg := left
            LeftLeftDigitReg := leftLeft

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
                stateReg := done
                io.writeAdress := 14.U
                io.writeTileID := LeftLeftDigitReg + 16.U
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
