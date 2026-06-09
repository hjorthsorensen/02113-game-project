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
  val customer1AnimCycle = RegInit(0.U(7.W))
  val customer1AnimDir = RegInit(true.B)

  val customer2XReg = RegInit(0.S(11.W))
  val customer2YReg = RegInit(0.S(10.W))
  val customer2IdleVisible = RegInit(false.B)
  val customer2DrinkingVisible = RegInit(false.B)
  val customer2Spawned = RegInit(false.B)
  val customer2AnimCycle = RegInit(0.U(7.W))
  val customer2AnimDir = RegInit(true.B)

  val customer1FlippedReg = RegInit(false.B)
  val customer2FlippedReg = RegInit(false.B)

  val customerToSpawn = RegInit(0.U(2.W))
  val customerToDespawn = RegInit(0.U(2.W))
  val customerSpawnDelay = RegInit(0.U(8.W))

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
      when(!customer1Spawned && (customerSpawnDelay === 0.U)) {
        customer1XReg := 150.S
        customer1YReg := 220.S
        customer1IdleVisible := true.B
        customer1Spawned := true.B
        customerSpawnDelay := 60.U
      }
      when(!customer2Spawned && (customerSpawnDelay === 0.U)) {
        customer2XReg := 300.S
        customer2YReg := 220.S
        customer2IdleVisible := true.B
        customer2Spawned := true.B
        customerSpawnDelay := 60.U
      }

      stateReg := despawn

    }
    is(despawn) {
      when(io.customer1Scored) {
        customer1XReg := 0.S
        customer1YReg := 0.S
        customer1IdleVisible := false.B
        customer1DrinkingVisible := false.B
        customer1Spawned := false.B
      }.elsewhen(io.customer2Scored) {
        customer2XReg := 0.S
        customer2YReg := 0.S
        customer2IdleVisible := false.B
        customer2DrinkingVisible := false.B
        customer2Spawned := false.B
      }

      stateReg := animate
    }
    is(animate) {
      when(customer1Spawned) {
        when(customer1AnimCycle > 59.U) {
          customer1AnimCycle := 0.U
          when(customer1AnimDir) {
            customer1YReg := customer1YReg - 2.S
          }.otherwise{
            customer1YReg := customer1YReg + 2.S
          }
            customer1AnimDir := !customer1AnimDir
        }
      }

      when(customer2Spawned) {
        when(customer2AnimCycle === 60.U) {
          customer2AnimCycle := 0.U
          when(customer2AnimDir){
          customer2YReg := customer2YReg - 2.S

          }.otherwise{
          customer2YReg := customer2YReg + 2.S

          }
          customer2AnimDir := !customer2AnimDir
        }
      }
      stateReg := delays
    }
    is(delays) {
      when(customer1Spawned) {
        customer1AnimCycle := customer1AnimCycle + 1.U
      }
      when(customer2Spawned) {
        customer2AnimCycle := customer2AnimCycle + 1.U
      }

      when(!(customerSpawnDelay === 0.U)) {
        customerSpawnDelay := customerSpawnDelay - 1.U
      }
      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
