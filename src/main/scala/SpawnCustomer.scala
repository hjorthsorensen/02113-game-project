import chisel3._
import chisel3.util._

class SpawnCustomer() extends Module {
    val io = IO(new Bundle {
        val work = Input(Bool())
        val beerDone = Input(Bool())
        val done = Output(Bool())
    })

    //customers default spawn is (0,0); position is picked in spawn state
    val customer1XReg = RegInit(0.S(11.W)) 
    val customer1YReg = RegInit(0.S(10.W)) 
    val idle :: spawn :: despawn :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when (io.work){
                stateReg := spawn
            }
        }
        is(spawn){
            
        }
    }


}