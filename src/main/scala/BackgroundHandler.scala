import chisel3._
import chisel3.util._

class BackgroundHandler extends Module {
  val io = IO(new Bundle {
    // Inputs

    val work = Input(Bool())
    val scoreDone = Input(Bool())
    val brokenGlassDone = Input(Bool())
    val beerDone = Input(Bool())
    val multiplierDone = Input(Bool())
    val inputAdress = Input(UInt(10.W))
    val inputTileID = Input(UInt(5.W))

    // Outputs for background
    val writeAdress = Output(UInt(10.W))
    val writeTileID = Output(UInt(5.W))
    val writeEnable = Output(Bool())

    val scoreWork = Output(Bool())
    val brokenGlassWork = Output(Bool())
    val beerWork = Output(Bool())
    val multiplierWork = Output(Bool())
    val done = Output(Bool())

  })

  val idle :: scoreBoard :: multiplier :: brokenGlass :: beerLeft :: done :: Nil =
    Enum(6)
  val stateReg = RegInit(idle)

  io.writeAdress := io.inputAdress
  io.writeTileID := io.inputTileID

  io.writeEnable := false.B
  io.scoreWork := false.B
  io.brokenGlassWork := false.B
  io.beerWork := false.B
  io.multiplierWork := false.B
  io.done := false.B

  switch(stateReg) {
    is(idle) {
      when(io.work) {
        stateReg := scoreBoard
        io.scoreWork := true.B
      }
    }
    is(scoreBoard) {
      io.writeEnable := true.B

      when(io.scoreDone) {
        stateReg := multiplier
        io.multiplierWork := true.B
      }
    }
    is(multiplier) {
      io.writeEnable := true.B

      when(io.multiplierDone) {
        stateReg := brokenGlass
        io.brokenGlassWork := true.B
      }
    }
    is(brokenGlass) {
      io.writeEnable := true.B

      when(io.brokenGlassDone) {
        stateReg := beerLeft
        io.beerWork := true.B
      }
    }
    is(beerLeft) {
      io.writeEnable := true.B

      when(io.beerDone) {
        stateReg := done
      }
    }
    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }
}
