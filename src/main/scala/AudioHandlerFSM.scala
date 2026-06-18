import chisel3._
import chisel3.util._


//checks to see what not work

/*
hardcode events to a certain value to ensure that it sends correctly.
if it works, it is the signals we receive that we dont hold for long enough.


hardcode audio generator to make one sound only. if it works, something in audiogen is wrong.

hardcode I2S to play a certain note on a switch flip.


maybe connect to some switches?

*/
class AudioHandlerFSM extends Module {
  val io = IO(new Bundle {

    val readyNewEvent = Input(Bool())
    val beerThrown = Input(Bool())
    val pointScoring = Input(Bool())
    val beerCaught = Input(Bool())
    val beerFalling = Input(Bool())
    val beerPouring = Input(Bool())
    val events = Output(UInt(4.W))
  })


io.events := 0.U
    /*events:
            0) nothing.
            1) beer thrown
            2) points scoring
            3) beer caught by bartender
            4) beer falls off the edge
            5) beer poured
     */
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
