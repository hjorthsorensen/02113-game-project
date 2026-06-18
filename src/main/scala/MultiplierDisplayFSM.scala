import chisel3._
import chisel3.util._

class MultiplierDisplayFSM extends Module {
  val io = IO(new Bundle {
    val multiplier = Input(UInt(5.W))
    val work = Input(Bool())

    val writeAdress = Output(UInt(10.W))
    val writeTileID = Output(UInt(6.W))
    val writingMultiplier = Output(Bool())

    val done = Output(Bool())
  })

  val idle :: calcDigits :: busy :: doneState :: Nil = Enum(4)
  val stateReg = RegInit(idle)
  
  val multiplierReg = RegInit(0.U(8.W))

  val rightDigitReg = RegInit(0.U(4.W)) // ones
  val leftDigitReg = RegInit(0.U(4.W))  // tens

  def doubleDabble(score: UInt): (UInt, UInt) = {
    val shiftRegInit = Cat(0.U(8.W), score(7, 0)) 
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
    (
      result(3, 0), // ones
      result(7, 4)  // tens
    )
  }

  val multiplierIDReg = RegInit(0.U(2.W))
  val multiplierWriteDoneReg = RegInit(false.B)
  val waitReg = RegInit(0.U(5.W))

  io.done := multiplierWriteDoneReg
  io.writeAdress := 0.U
  io.writeTileID := 0.U
  io.writingMultiplier := false.B

  switch(stateReg) {
    is(idle) {
      multiplierWriteDoneReg := false.B
      when(io.work) {
        multiplierReg := io.multiplier
        stateReg := calcDigits
      }
    }
    is(calcDigits) {
      val (right, left) = doubleDabble(multiplierReg)
      rightDigitReg := right
      leftDigitReg := left

      waitReg := waitReg + 1.U
      when(waitReg === 20.U) {
        waitReg := 0.U
        stateReg := busy
      }
    }
    is(busy) {
      io.writingMultiplier := true.B
      when(multiplierIDReg === 0.U) {
        multiplierIDReg := multiplierIDReg + 1.U
        io.writeAdress := (59).U
        io.writeTileID := rightDigitReg + 16.U
      }.elsewhen(multiplierIDReg === 1.U) {
        multiplierIDReg := multiplierIDReg + 1.U
        io.writeAdress := (58).U
        io.writeTileID := leftDigitReg + 16.U
        stateReg := doneState 
      }
    }
    is(doneState) {
      multiplierWriteDoneReg := true.B
      multiplierIDReg := 0.U
      stateReg := idle
    }
  }
}