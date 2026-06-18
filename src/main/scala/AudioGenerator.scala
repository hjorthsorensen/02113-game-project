import chisel3._
import chisel3.util._

class AudioGenerator extends Module{
    val io = IO(new Bundle{
        val event = Input(UInt(4.W))
        val sampleReady = Input(Bool())
        val audioDataOut = Output(SInt(16.W))
        val readyNewEvent = Output(Bool())
        val clkOut = Output(Bool())
        /*events:
            0) nothing.
            1) beer thrown
            2) points scoring
            3) beer caught by bartender
            4) beer falls off the edge
            5) beer poured         
            */




    })
    val noteRepeats = RegInit(0.U(16.W))

    val noteSelector = RegInit(0.U(4.W))
    val tonePeriodLUT = MuxLookup(noteSelector, 0.U(12.W))(Seq(
  0.U  -> 0.U,    // Mute / Rest
  1.U  -> 42.U,   // Middle C (261.6 Hz)
  2.U  -> 37.U,   // D4       (293.7 Hz)
  3.U  -> 33.U,   // E4       (329.6 Hz)
  4.U  -> 28.U,   // G4       (392.0 Hz)
  5.U  -> 25.U    // A4       (440.0 Hz)
))

// val I2SDriver = Module (new I2SDriver())

val data = RegInit(0.S(16.W))

val tonePeriodCountReg = RegInit(0.U(8.W))
val toneFlipReg = RegInit(false.B)

    //clock divider (make in-program)


    val ToggleLimit = 2273.U

    val counterReg = RegInit(0.U(12.W))

    val clkReg = RegInit(false.B)

    when(counterReg === ToggleLimit - 1.U) {
        counterReg := 0.U
        clkReg := !clkReg // Toggle the clock
    }.otherwise {
        counterReg := counterReg + 1.U
    }

    //end of clock divider


val stateReg = RegInit(0.U(4.W))
   
//io assignments

    io.readyNewEvent := false.B
io.audioDataOut := data
io.clkOut := clkReg




//we only reassign readyNewEvent when noteRepeats is 0 and event is something other than silence.
when(noteRepeats === 0.U && io.event =/= 0.U){
        //decide what signals to drive depending on the events.
    switch(io.event){ 
        is(0.U){
            noteRepeats := 0.U //nothing happening.
        }
        is(1.U){
            noteRepeats := 22000.U // sliding the beer. should only take abt a second.
            noteSelector := 1.U
        }
        is(2.U){
            noteRepeats := 11000.U //0.5 sec (pts scoring approx half a second)
            noteSelector := 5.U
        }
        is(3.U){
            noteRepeats := 5500.U //0.25 sec (quick catch)
            noteSelector := 4.U
        }
        is(4.U){
            noteRepeats := 11000.U // approx half a second of falling time.
            noteSelector := 3.U
        }
        is(5.U){
            noteRepeats := 5500.U
            noteSelector := 2.U
        }
    }
    data := 32000.S
}

    //decrement noteRepeats if not zero
    when(noteRepeats =/= 0.U){
        when(io.sampleReady){
        noteRepeats := noteRepeats - 1.U
        tonePeriodCountReg := tonePeriodCountReg + 1.U
        io.audioDataOut := data
            when(tonePeriodCountReg === tonePeriodLUT - 1.U){
            data := -data
            tonePeriodCountReg := 0.U
    }
        }


    }.elsewhen(noteRepeats === 0.U){
        io.readyNewEvent := true.B
        noteSelector := 0.U
        io.audioDataOut := 0.S

    }


}