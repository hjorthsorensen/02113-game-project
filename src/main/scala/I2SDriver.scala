import chisel3._
import chisel3.util._

class I2SDriver extends Module{
    val io = IO(new Bundle{
        //inputs
        // val BCLKInput = Input(Bool())
        val generatedAudio = Input(SInt(16.W))
        
        //outputs
        val BCLKOutput = Output(Bool())
        val DIN = Output(Bool())
        val LRC = Output(Bool())
        val sampleReady = Output(Bool())
        
    })


        //clock divider (make in-program)


    val ToggleLimit = 16.U

    val counterReg = RegInit(0.U(12.W))

    val clkReg = RegInit(false.B)

    when(counterReg === ToggleLimit - 1.U) {
        counterReg := 0.U
        clkReg := !clkReg // Toggle the clock
    }.otherwise {
        counterReg := counterReg + 1.U
    }

    //end of clock divider



    val audioShiftReg = RegInit(0.S(16.W))
    val audioShiftCounterReg = RegInit(0.U(6.W))
    val bit_counter = RegInit(0.U(6.W))
    val sampleReadyReg = RegInit(false.B)
    val LRCReg = RegInit(false.B)
    val DINReg = RegInit(false.B)
    //signal to audio generator that we want new audio.
    //only high when bit_counter is 63 (we are done with the data)
    io.sampleReady := sampleReadyReg
    io.LRC := LRCReg
    io.DIN := DINReg
    val clkPrev = RegNext(clkReg)
    val risingEdge = clkReg && !clkPrev

    io.BCLKOutput := clkReg
    DINReg := audioShiftReg(15).asUInt

    when(risingEdge){
        bit_counter := bit_counter + 1.U
        //driving of bit counter, LRC choice
        when(bit_counter <= 31.U && 0.U <= bit_counter){
            LRCReg := false.B
        }.elsewhen(bit_counter <= 63.U && 32.U <= bit_counter){
            LRCReg := true.B
        }
        //when done with data, send sampleReady to AudioGenerator. otherwise, keep at 0.
        when(bit_counter === 63.U){
            sampleReadyReg := true.B
        }.otherwise{
            sampleReadyReg := false.B
        }



        //driving of shift register and DIN.
        when(bit_counter === 0.U){
            audioShiftReg := io.generatedAudio
        }.elsewhen(bit_counter >= 1.U && bit_counter <= 16.U){

            audioShiftReg := audioShiftReg << 1

        }



    }
}