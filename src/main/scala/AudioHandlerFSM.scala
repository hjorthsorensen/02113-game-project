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

/*events:
        0) nothing.
        1) beer thrown
        2) points scoring
        3) beer caught by bartender
        4) beer falls off the edge
        5) beer poured
 */

class AudioHandlerFSM extends Module {
  val io = IO(new Bundle {

    // inputs
    val work = Input(Bool())
    val readyNewEvent = Input(Bool())
    val beerThrown = Input(Bool())
    val pointScoring = Input(Bool())
    val beerCaught = Input(Bool())
    val beerFalling = Input(Bool())
    val beerPouring = Input(Bool())

    // outputs
    val events = Output(UInt(4.W))
    val done = Output(Bool())
  })

//definitions
  val event = RegInit(0.U(3.W))

  val idle :: newEvent :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

//io connections
  io.done := false.B
  io.events := event

  // statemachine
  switch(stateReg) {

    is(idle) {
      when(io.work) {
        stateReg := newEvent
      }


    }
    is(newEvent) {
        //assign event to something, in prioritized queue.
      when(io.beerThrown) {
        event := 1.U
      }.elsewhen(io.pointScoring) {
        event := 2.U
      }.elsewhen(io.beerCaught) {
        event := 3.U
      }.elsewhen(io.beerFalling) {
        event := 4.U
      }.elsewhen(io.beerPouring) {
        event := 5.U
      }.otherwise {
        event := 0.U
      }
      stateReg := done
    }
    is(done){
        io.done := true.B
    }
  }


}
