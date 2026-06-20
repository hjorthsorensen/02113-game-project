import chisel3._
import chisel3.util._

class BackgroundHandlerFSM extends Module {
  val io = IO(new Bundle {
    // work input
    val work = Input(Bool())

    //Done signals sent by the individual FSMs which Backgroundhandler handles
    val scoreDone        = Input(Bool())
    val brokenGlassDone  = Input(Bool())
    val beerDone         = Input(Bool())
    val multiplierDone   = Input(Bool())
    val loadingDone      = Input(Bool())
    val changeBarDone    = Input(Bool())

    //Input adress and tileID
    val inputAdress     = Input(UInt(10.W))
    val inputTileID     = Input(UInt(6.W))

    // Outputs for background writing 
    val writeAdress     = Output(UInt(10.W))
    val writeTileID     = Output(UInt(6.W))
    val writeEnable     = Output(Bool())

    //Output signals to start each FSM which Backgroundhandler handles
    val scoreWork       = Output(Bool())
    val brokenGlassWork = Output(Bool())
    val beerWork        = Output(Bool())
    val multiplierWork  = Output(Bool())
    val loadingWork     = Output(Bool())
    val changeBarWork   = Output(Bool())

    //Done signal to main FSMD
    val done            = Output(Bool())

  })

  val idle :: scoreBoard :: multiplier :: brokenGlass :: beerLeft :: loading :: changeBar :: done :: Nil =
    Enum(8)
  val stateReg = RegInit(idle)

  io.writeAdress := io.inputAdress
  io.writeTileID := io.inputTileID

//Default outputs
  io.writeEnable       := false.B
  io.scoreWork         := false.B
  io.brokenGlassWork   := false.B
  io.beerWork          := false.B
  io.multiplierWork    := false.B
  io.done              := false.B
  io.loadingWork       := false.B
  io.changeBarWork     := false.B

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
        io.loadingWork := true.B
        stateReg := loading
      }
    }
    is(loading){
      io.writeEnable := true.B
      when(io.loadingDone){
        io.changeBarWork := true.B
        stateReg := changeBar
      }

    }
    is(changeBar){
      io.writeEnable := true.B
      when(io.changeBarDone){
            stateReg := done
        }

    }
    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }
}
