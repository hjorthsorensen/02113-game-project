import chisel3._
import chisel3.util._

class SpawnCustomer extends Module {
  val io = IO(new Bundle {
    val work = Input(Bool())
    // val beerDone = Input(Bool())
    val customer1Scored = Input(Bool())
    val customer2Scored = Input(Bool())
    val done = Output(Bool())
    val customer1PosX = Output(SInt(11.W))
    val customer1PosY = Output(SInt(10.W))
    val customer2PosX = Output(SInt(11.W))
    val customer2PosY = Output(SInt(10.W))
    val customer1IdleVisible = Output(Bool())
    val customer2IdleVisible = Output(Bool())
    val customer1DrinkingVisible = Output(Bool())
    val customer2DrinkingVisible = Output(Bool())
    val customer1Flipped = Output(Bool())
    val customer2Flipped = Output(Bool())
  })

  // customers default spawn is (0,0); position is picked in spawn state
  val customer1XReg = RegInit(0.S(11.W))
  val customer1YReg = RegInit(0.S(10.W))
  val customer1IdleVisible = RegInit(false.B)
  val customer1DrinkingVisible = RegInit(false.B)
  val customer1Spawned = RegInit(false.B)
  val customer1SpawnDelay = RegInit(0.U(8.W))
  val customer1AnimCycle = RegInit(0.U(7.W))

  val customer2XReg = RegInit(0.S(11.W))
  val customer2YReg = RegInit(0.S(10.W))
  val customer2IdleVisible = RegInit(false.B)
  val customer2DrinkingVisible = RegInit(false.B)
  val customer2Spawned = RegInit(false.B)
  val customer2SpawnDelay = RegInit(0.U(8.W))
  val customer2AnimCycle = RegInit(0.U(7.W))

  val customer1FlippedReg = RegInit(false.B)
  val customer2FlippedReg = RegInit(false.B)

  val customerToSpawn = RegInit(0.U(2.W))
  val customerToDespawn = RegInit(0.U(2.W))
  // reg to decide what customer to spawn

  // io connections
  io.done := false.B
  io.customer1PosX := customer1XReg
  io.customer1PosY := customer1YReg
  io.customer2PosX := customer2XReg
  io.customer2PosY := customer2YReg
  io.customer1IdleVisible := customer1IdleVisible
  io.customer1DrinkingVisible := customer1DrinkingVisible
  io.customer2IdleVisible := customer2IdleVisible
  io.customer2DrinkingVisible := customer2DrinkingVisible

  io.customer1Flipped := customer1FlippedReg
  io.customer2Flipped := customer2FlippedReg
//   customerToDespawn := Cat(io.customer2Scored,io.customer1Scored)
//   customerToSpawn := Cat(io.customer2IdleVisible,io.customer1IdleVisible)

  // statemachine
  val idle :: spawn :: despawn :: delays :: animate :: done :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        stateReg := spawn
      }
      //   .elsewhen(!(customerToDespawn === 0.U)) {
      //     stateReg := despawn
      //   }
    }

    is(spawn) {

      // if customer not spawned, and customer delay is 0, spawn customer.
      when(!customer1Spawned && (customer1SpawnDelay === 0.U)) {
        customer1XReg := 150.S
        customer1YReg := 220.S
        customer1IdleVisible := true.B
      }
      when(!customer2Spawned && (customer2SpawnDelay === 0.U)) {
        customer2XReg := 300.S
        customer2YReg := 220.S
        customer2IdleVisible := true.B
      }

      stateReg := despawn

    }
    is(despawn) {
      when(io.customer1Scored) {
        customer1XReg := 0.S
        customer1YReg := 0.S
        customer1IdleVisible := false.B
        customer1DrinkingVisible := false.B
      }.elsewhen(io.customer2Scored) {
        customer2XReg := 0.S
        customer2YReg := 0.S
        customer2IdleVisible := false.B
        customer2DrinkingVisible := false.B
      }

      stateReg := animate
    }
    is(animate) {
      when(customer1Spawned) {
        when(customer1AnimCycle === 60.U){
            customer1AnimCycle := 0.U
            customer1YReg := customer1YReg + 2.S
        }
      }

      when(customer2Spawned) {
        when(customer2AnimCycle === 60.U){
            customer2AnimCycle := 0.U
            customer2YReg := customer2YReg + 2.S
        }
      }
    stateReg := delays
    }
    is(delays) {
        customer1AnimCycle := customer1AnimCycle + 1.U
        customer2AnimCycle := customer2AnimCycle + 1.U
      when(!(customer1SpawnDelay === 0.U)) {
        customer1SpawnDelay := customer1SpawnDelay - 1.U
      }
      when(!(customer2SpawnDelay === 0.U)) {
        customer2SpawnDelay := customer2SpawnDelay - 1.U
      }
      stateReg := done
    }
    
    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
