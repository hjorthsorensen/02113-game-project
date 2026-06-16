import chisel3._
import chisel3.util._

class AudioHandlerFSM extends Module {
  val io = IO(new Bundle {
    /*events:
            0) nothing.
            1) beer thrown
            2) points scoring
            3) beer caught by bartender
            4) beer falls off the edge
            5) beer poured
     */
    val readyNewEvent = Input(Bool())
    val events = Output(UInt(4.W))
    val beerThrown = Input(Bool())
    val pointScoring = Input(Bool())
    val beerCaught = Input(Bool())
    val beerFalling = Input(Bool())
    val beerPouring = Input(Bool())
  })

//when we are ready for a new event in the audio generator, we check all possible events.
  when(io.readyNewEvent) {

    when(io.beerThrown) {
      io.events := 1.U
    }.elsewhen(io.pointScoring) {
      io.events := 2.U
    }.elsewhen(io.beerCaught) {
      io.events := 3.U
    }.elsewhen(io.beerFalling) {
      io.events := 4.U
    }.elsewhen(io.beerPouring) {
      io.events := 5.U
    }.otherwise {
      io.events := 0.U
    }

  }

}
