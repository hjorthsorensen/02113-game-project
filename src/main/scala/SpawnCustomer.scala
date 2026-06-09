import chisel3._
import chisel3.util._

class SpawnCustomer() extends Module {
    val io = IO(new Bundle {
        val work = Input(Bool())
        val beerDone = Input(Bool())
        val done = Output(Bool())
        val customer1PosX = Output(SInt(11.W))
        val customer1PosY = Output(SInt(11.W))
        val customer2PosX = Output(SInt(11.W))
        val customer2PosY = Output(SInt(11.W))
    })

    //customers default spawn is (0,0); position is picked in spawn state
    val customer1XReg = RegInit(0.S(11.W)) 
    val customer1YReg = RegInit(0.S(10.W)) 
    val customer1Visible = RegInit(false.B)
    
    val customer2XReg = RegInit(0.S(11.W)) 
    val customer2YReg = RegInit(0.S(10.W)) 
    val customer2Visible = RegInit(false.B)
    
    val customerToSpawn = RegInit(0.U(1.W)) //reg to decide what customer to spawn


    //io connections
    done := false.B
    io.customer1PosX := customer1XReg
    io.customer1PosY := customer1YReg
    io.customer2PosX := customer2XReg
    io.customer2PosY := customer2YReg
    



    //statemachine
    val idle :: spawn :: despawn :: done :: Nil = Enum(4)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when (io.work){
                stateReg := spawn
            }.elsewhen(io.beerDone){
                stateReg := despawn
            }
        }
        is(spawn){
            //decide customer
            //decide location
            //make visible
            //set customer pos on output
        }
        is(despawn){
            //decide customer
            //make visible
            //change customer decision
            //
        }
        is(done){
            io.done := true.B
            stateReg := idle
        }
    }


}