import chisel3._
import chisel3.util._

class AudioGenerator extends Module{
    val io = IO(new Bundle{
        //inputs
        //audio signals in
        val beerSpeed = Input(SInt(8.W))
        val ptScoring = Input(Bool())
        val beerBreaking = Input(Bool())
        val gameOver = Input(Bool())
        //I2S input from driver
        val sampleReady = Input(Bool())
        //Outputs
        //
        val audioDataOut = Output(SInt(16.W))
        
        //debug output signals
        val debugEvent = Output(Vec(4,Bool()))
     })


//REGISTERS

    //conditional registers

// outer condition for audio; if true, noteSelector is 0.



//source is our inner condition for noteSelector, and it is decided from inputs from the other modules.
val source = RegInit(0.U(4.W))
//sources we want: beerSliding, PtScoring, beer breaks.
//source is 1.U at beerSlding, 2.U at PtScoring, 3.U at beer breaks, 4.U at game over.


        //AUDIO

    //halftime counter.
    val tonePeriodCountReg = RegInit(0.U(8.W))
    //note selector, selecting in tonePeriodLUT.
    val noteSelector = RegInit(0.U(4.W))
    //LUT of notes, calculated for 44.1khz sample rate.
    //(approximations)
    val tonePeriodLUT = MuxLookup(noteSelector, 0.U(12.W))(Seq(
  0.U  -> 0.U,    // Mute / Rest
  1.U  -> 175.U,   // Middle C  (261.6  Hz)
  2.U  -> 150.U,   // D4        (293.7  Hz)
  3.U  -> 133.U,   // E4        (329.6  Hz)
  4.U  -> 112.U,   // G4        (392.0  Hz)
  5.U  -> 100.U,    // A4       (440.0  Hz)
  6.U  -> 68.U,     // E5 (ish) (650.0  Hz)
  7.U  -> 50.U,     // A6       (880.0  Hz) 
  8.U  -> 42.U      // C6       (1050.0 Hz)
))
    

//the data to send. ~2^16.
val data = RegInit(32000.S(16.W))

    //INPUT SIGNALS

    //STUTTER
        //get a stuttery / "counting" audio effect by making a stutter condition.
        //an outer condition used both in ptscoring and in game over.

    val stutter = RegInit(false.B)
    //stutterCntReg is the counter for how long the stutter should be.
    val stutterCntReg = RegInit(0.U(6.W))
    //repeatCntReg to check how many stutters we make.
    val repeatCntReg = RegInit(0.U(4.W))
    when(stutterCntReg === 64.U){
        stutterCntReg := 0.U
        stutter := !stutter
        repeatCntReg := Mux(noteSelector === 4.U, repeatCntReg + 1.U, 0.U)

    }



    //BEERSLIDING SIGNAL REGS
//beerSliding is bool check to see whether we have
val beerSliding = RegInit(io.beerSpeed =/= 0.S)
val beerSpeed = RegInit(0.S(8.W))
beerSpeed := io.beerSpeed //io.beerSpeed is from playerMovementFSM.

    //PTSCORING SIGNAL REGS
        //get our pointScoring signal.
    val ptScoringReg = RegInit(io.ptScoring)


    //BEER BROKEN SIGNAL REGS
    val beerBroken = RegInit(io.beerBreaking)


    //GAME OVER SIGNAL REGS 
    val gameOver = RegInit(io.gameOver)

    //io assignments.
    io.audioDataOut := data
    
    //debugs
    io.debugEvent(0) := beerSliding
    io.debugEvent(1) := ptScoringReg
    io.debugEvent(2) := beerBroken
    io.debugEvent(3) := gameOver


//handling of sound should be default == 0.U. if stutter is false, play whatever signal is high.

    switch(stutter){
        is(false.B){ //if stutter is not true. only true during ptscoring and game over.

        
        switch(source){
            is(1.U){ //beerSliding
            stutterCntReg := 0.U
                noteSelector := Mux(beerSpeed.abs >= 15.S, 6.U, 3.U)
            }
            is(2.U){//ptScoring
                noteSelector := 8.U
                stutterCntReg := stutterCntReg + 1.U

            }
            is(3.U){ //beerBroken
            noteSelector := 2.U
            stutterCntReg := stutterCntReg + 1.U

            }
            is(4.U){ //gameOver
                //play a melody?
            noteSelector := 1.U
            stutterCntReg := stutterCntReg + 1.U
            }

        }
    }
    is (true.B){ //we are in ptscoring or gameOver.
        noteSelector := 0.U
        stutterCntReg := stutterCntReg + 1.U
    }
    }
    





    //sending of sample.
    when(io.sampleReady){
        tonePeriodCountReg := tonePeriodCountReg + 1.U
        when(tonePeriodCountReg === noteSelector - 1.U){
            data := -data
            tonePeriodCountReg := 0.U
        }
    }










}