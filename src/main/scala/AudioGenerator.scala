import chisel3._
import chisel3.util._

class AudioGenerator extends Module{
    val io = IO(new Bundle{
        val beerSpeed = Input(SInt(8.W))

        val sampleReady = Input(Bool())
        val audioDataOut = Output(SInt(16.W))
        val debugEvent = Output(Bool())
     })

    val noteSelector = RegInit(0.U(4.W))
    val frequencyReg = RegInit(0.U(12.W))
    frequencyReg := io.beerSpeed.asUInt
    val tonePeriodLUT = MuxLookup(noteSelector, 0.U(12.W))(Seq(
  0.U  -> 0.U,    // Mute / Rest
  1.U  -> 42.U,   // Middle C (261.6 Hz)
  2.U  -> 37.U,   // D4       (293.7 Hz)
  3.U  -> 33.U,   // E4       (329.6 Hz)
  4.U  -> 28.U,   // G4       (392.0 Hz)
  5.U  -> 25.U    // A4       (440.0 Hz)
))


val data = RegInit(32000.S(16.W))

val tonePeriodCountReg = RegInit(0.U(8.W))
val toneFlipReg = RegInit(false.B)
val beerSliding = RegInit(io.beerSpeed =/= 0.S)
    //io assignments.
    io.debugEvent := beerSliding === true.B
    io.audioDataOut := data


    switch(beerSliding){
        is(false.B){
            noteSelector := 0.U
        }
        is(true.B){
            noteSelector := frequencyReg
        }
    }

    when(io.sampleReady){
        tonePeriodCountReg := tonePeriodCountReg + 1.U
        when(tonePeriodCountReg === tonePeriodLUT - 1.U){
            data := -data
            tonePeriodCountReg := 0.U
        }
    }










}