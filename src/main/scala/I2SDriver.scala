import chisel3._
import chisel3.util._

class I2SDriver extends Module{
    val io = IO(new Bundle{
        val BCLKInput = Input(Bool())
        val BCLKOutput = Output(Bool())
        val DIN = Output(Bool())
        val LRC = Output(Bool())
        val sampleReady = Output(Bool())
        val generatedAudio = Input(SInt(16.W))
        
    })
    val audioShiftReg = RegInit(0.S(16.W))
    val audioShiftCounterReg = RegInit(0.U(6.W))
    val bit_counter = RegInit(0.U(6.W))
    //signal to audio generator that we want new audio.
    //only high when bit_counter is 63 (we are done with the data)
    io.sampleReady := false.B
    io.LRC := false.B
    io.DIN := false.B
    val bclkReg = RegNext(io.BCLKInput)
    val bclkPosEdge = io.BCLKInput && !bclkReg

    io.BCLKOutput := io.BCLKInput


    when(bclkPosEdge){
        bit_counter := bit_counter + 1.U
        //driving of bit counter, LRC choice
        when(bit_counter <= 31.U && 0.U <= bit_counter){
            io.LRC := false.B
        }.elsewhen(bit_counter <= 63.U && 32.U <= bit_counter){
            io.LRC := true.B
        }
        //when done with data, send sampleReady to AudioGenerator.
        when(bit_counter === 63.U){
            io.sampleReady := true.B
        }



        //driving of shift register and DIN.
        when(bit_counter === 0.U){
            audioShiftReg := io.generatedAudio
            audioShiftCounterReg := 15.U
        }.elsewhen(bit_counter >= 1.U && bit_counter <= 16.U){

            io.DIN := audioShiftReg(16.U(6.W) - bit_counter)
            //audioShiftReg := audioShiftReg << 1

        }.otherwise{
            io.DIN := false.B
        }



    }
}