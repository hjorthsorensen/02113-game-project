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
        val badThrow = Input(Bool())
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
//source is 1.U at beerSlding, 2.U at bad throw, 3.U at ptScoring, 4.U at beer breaks, 5.U at game over.


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
    val stutterCntReg = RegInit(0.U(12.W))
    //repeatCntReg to check how many stutters we make.
    val repeatCntReg = RegInit(0.U(7.W))
    when(stutterCntReg === 63.U){
        stutterCntReg := 0.U
        stutter := !stutter
        repeatCntReg := Mux(noteSelector === 5.U, repeatCntReg + 1.U, 0.U)

    }



    //BEERSLIDING SIGNAL REGS
//beerSliding is bool check to see if beer is sliding.
val beerSliding = RegInit(false.B)
beerSliding := io.beerSpeed =/= 0.S
//beerspeed is the actual speed we are sliding at.
val beerSpeed = RegInit(0.S(8.W))
beerSpeed := io.beerSpeed //io.beerSpeed is from playerMovementFSM.

    //PTSCORING SIGNAL REGS
        //get our pointScoring signal.
    val ptScoringReg = RegInit(false.B)
    ptScoringReg := io.ptScoring

    /*
    bad throw == didnt go to ptScoring.
    bad throw, in practice, doesnt work, as the ptScoring doesnt work.
    both signals from scoreFSM & spawnCustomerFSM refuse to activate while active.
    However, the structure of the signals ensure that, when ptScoring is made to work,
    both will work.
    */
    val badThrowReg = RegInit(true.B)
    badThrowReg := io.badThrow && !ptScoringReg


    //BEER BROKEN SIGNAL REGS
    val beerBroken = RegInit(false.B)
    beerBroken := io.beerBreaking


    //GAME OVER SIGNAL REGS 
    val gameOver = RegInit(false.B)
    gameOver := io.gameOver

    //io assignments.
    io.audioDataOut := data
    
    //debugs
    io.debugEvent(0) := beerSliding
    io.debugEvent(1) := ptScoringReg
    io.debugEvent(2) := beerBroken
    io.debugEvent(3) := gameOver

//source is 1.U at beerSlding, 2.U at PtScoring, 3.U at beer breaks, 4.U at game over.

    when(beerSliding){
        source :=  1.U
    }.elsewhen(badThrowReg){
        source := 2.U
    }.elsewhen(ptScoringReg){
        source := 3.U
    }.elsewhen(beerBroken){
        source := 4.U
    }.elsewhen(gameOver){
        source := 5.U
    }.otherwise{
        source := 0.U
    }

//handling of sound should be default == 0.U. if stutter is false, play whatever signal is high.



    switch(stutter){
        is(false.B){ //when 

        
        switch(source){
            is(0.U){ //stop sound if no sound should be playing.... :D
                noteSelector := 0.U
                stutterCntReg := 0.U
            }
            is(1.U){ //beerSliding
            stutterCntReg := 0.U
                when(beerSpeed >= 20.S){
                    noteSelector := 7.U
                }.elsewhen(beerSpeed >= 10.S){
                noteSelector := 6.U
                }.elsewhen(beerSpeed <= 10.S){
                    noteSelector := 5.U
                }
            }
            is(2.U){//bad throw
                noteSelector := 8.U

            }
            is(3.U){ //pointScoring!
            noteSelector := 2.U

            }
            is(4.U){ //beer broken...
            noteSelector := 2.U
            }
            is(5.U){ //game over!
                when(repeatCntReg <= 20.U){
                    noteSelector := 2.U
                }.elsewhen(repeatCntReg <= 40.U){
                    noteSelector := 5.U
                }.elsewhen(repeatCntReg <= 60.U){
                    noteSelector := 2.U
                }.otherwise{
                    //force us to stay here.
                    //probably could add condition to gameOverReg, to make source go back to 0.U instead.
                    repeatCntReg := 65.U
                    stutterCntReg := 0.U
                    noteSelector := 0.U
                }
            }

        }
    }
    is (true.B){ //stuttertime!
        noteSelector := 0.U
    }
    }
    





    //sending of sample.
    when(io.sampleReady){
        //increment stutterreg when sending audio, if stutterReg should increment.
        when(source =/= 0.U && source =/= 1.U){
        stutterCntReg := stutterCntReg + 1.U

        }
        tonePeriodCountReg := tonePeriodCountReg + 1.U
        //flip the data! to make the squarewave actually be a squarewave.
        when(tonePeriodCountReg === tonePeriodLUT - 1.U){
            data := -data
            tonePeriodCountReg := 0.U
        }
    }










}