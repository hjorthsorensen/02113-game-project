import chisel3._
import chisel3.util._


class AudioHandlerFSM extends Module{
    val io = IO (new Bundle{
                /*events:
            0) nothing.
            1) beer thrown
            2) points scoring
            3) beer caught by bartender
            4) beer falls off the edge
            5) beer poured         
            */
            val events = Output(Bool())
            val beerThrown = Input(Bool())
            val pointScoring = Input(Bool())
            val beerCaught = Input(Bool())
            val beerFalling = Input(Bool())
            val beerPouring = Input(Bool())
    })


 when(io.beerThrown){
    events
 }






}