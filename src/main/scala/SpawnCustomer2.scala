import chisel3._
import chisel3.util._

class SpawnCustomer2(degreeOfRandom: Int, Customers: Int) extends Module {
  val io = IO(new Bundle {
    val work = Input(Bool())
    val beerDone = Input(Bool())
    val customer1Scored = Input(Bool())
    val customer2Scored = Input(Bool())
    val done = Output(Bool())
    val customer1PosX = Output(SInt(11.W))
    val customer1PosY = Output(SInt(10.W))
    val customer1IdleVisible = Output(Bool())
    val customer1DrinkingVisible = Output(Bool())
    val customer1Flipped = Output(Bool())
    val customer1ScoreDone = Output(Bool())

    val customer2PosX = Output(SInt(11.W))
    val customer2PosY = Output(SInt(10.W))
    val customer2IdleVisible = Output(Bool())
    val customer2DrinkingVisible = Output(Bool())
    val customer2Flipped = Output(Bool())
    val customer2ScoreDone = Output(Bool())
  })

  // Customer Vec Initialisations. 
  val customerXPosVec = VecInit(0.S(11.W),0.S(11.W),0.S(11.W))
  val customerYPosVec = VecInit(0.S(11.W),0.S(11.W),0.S(11.W))
  val customerIdleVisibilityVec = VecInit(false.B,false.B,false.B)
  val customerDrinkingVisibilityVec = VecInit(false.B,false.B,false.B)
  val customerSpawnedVec = VecInit(false.B,false.B,false.B)
  val customerAnimCycleVec = VecInit(0.U(7.W),0.U(7.W),0.U(7.W))
  val customerScoreDoneVec = VecInit(false.B,false.B,false.B)
  val customerSeatYVec = VecInit(0.U(2.W),0.U(2.W),0.U(2.W))
  val customerFlippedVec = VecInit(false.B,false.B,false.B)
  val customerSpawnDelayReg = RegInit(0.U(9.W))
  val customerRandomValuesVec = VecInit(0.U(9.W))
//randomX should be between ~64 and ~450.
  customerRandomValuesVec(0) := random.LFSR(32,true.B)
  customerRandomValuesVec(1) := random.LFSR(32,true.B)
  customerRandomValuesVec(2) := random.LFSR(32,true.B)

  //common / shared regs
  val customerToSpawnReg = RegInit(0.U(2.W))
  val customerToDespawnReg = RegInit(0.U(2.W))
  val customerDrinkingDelayReg = RegInit(0.U(8.W))
  val customerDrinkingAnimCycleReg = RegInit(0.U(2.W))
  val customerBegunScoringReg = RegInit(0.U(2.W))
  val anyCustomerScored = RegInit(false.B)
  anyCustomerScored := io.customer1Scored === true.B || io.customer2Scored === true.B



  // reg to decide what customer to spawn

  // io connections
  io.done := false.B
  io.customer1PosX := customerXPosVec(0)
  io.customer1PosY := customerYPosVec(0)
  io.customer1IdleVisible := customerIdleVisibilityVec(0)
  io.customer1DrinkingVisible := customerDrinkingVisibilityVec(0)
  io.customer1ScoreDone := customerScoreDoneVec(0)
  customerScoreDoneVec(0) := false.B
  io.customer1Flipped := customerFlippedVec(0)


  io.customer2PosX := customerXPosVec(1)
  io.customer2PosY := customerYPosVec(1)
  io.customer2IdleVisible := customerIdleVisibilityVec(1)
  io.customer2DrinkingVisible := customerDrinkingVisibilityVec(1)
  io.customer2ScoreDone := customerScoreDoneVec(1)
  customerScoreDoneVec(1):= false.B
  io.customer2Flipped := customerFlippedVec(1)





  // statemachine
  val idle :: spawnFirstCustomer :: spawnSecondCustomer :: spawnThirdCustomer :: despawn :: delays :: animate :: done :: Nil = Enum(8)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        //only spawn new customers when all customers are despawned, 
        //and 
        when((customerSpawnedVec(0) === false.B && customerSpawnedVec(1) === false.B && customerSpawnedVec(2) === false.B)){
            stateReg := spawnFirstCustomer
        }.elsewhen(anyCustomerScored){
          stateReg := despawn
        }.otherwise{
          stateReg := delays
        }
      }
    }

    is(spawnFirstCustomer) {
      //all customers are despawned. start spawning customers, at random places, and show them.
      //spawning of different customers are in different states to ensure we can read their y values 
      //and place them om different tables (only if they are too close)
      customerXPosVec(0) := Mux(customerRandomValuesVec(0) >= 450.U,450.S,Mux(customerRandomValuesVec(0) <= 96.U, 96.S, customerRandomValuesVec(0)))
      customerSeatYVec(0) := customerRandomValuesVec(0)
      switch(customerSeatYVec(0)){
        is(0.U){
          customerYPosVec(0) := 192.S
        }
        is(1.U){
          customerYPosVec(0) := 256.S

        }
        is(2.U){
          customerYPosVec(0) := 320.S

        }
        is(3.U){
          customerYPosVec(0) := 384.S
        }
      }
  stateReg := spawnSecondCustomer
    }
    is(spawnSecondCustomer){
      //Check if we want to spawn second customer.
      when(Customers.U === 2.U){
        //first, get random position.
        customerXPosVec(1) := Mux(customerRandomValuesVec(1) >= 450.U,450.S,Mux(customerRandomValuesVec(1) <= 96.U, 96.S, customerRandomValuesVec(1)))
        customerSeatYVec(1) := customerRandomValuesVec(1)
      switch(customerSeatYVec(1)){
        is(0.U){
          customerYPosVec(1) := 192.S
        }
        is(1.U){
          customerYPosVec(1) := 256.S

        }
        is(2.U){
          customerYPosVec(1) := 320.S

        }
        is(3.U){
          customerYPosVec(1) := 384.S
        }
      }

      //now we check if they overlap...
      when((customerXPosVec(1) - customerXPosVec(0)).abs <= 50.S){
        when(customerYPosVec(1) === customerYPosVec(0)){
          //if already at lowest table, go up one table; otherwise go to the next table.
          customerYPosVec(1) := Mux(customerYPosVec(1) === 384.S, customerYPosVec(1) - 64.S, customerYPosVec(1) + 64.S)
        }
      }

      }
      stateReg := spawnThirdCustomer

      
    }
    is(spawnThirdCustomer){
      when(Customers.U === 3.U){
        //set x value randomly; and just ensure it is a different table than the others are at.
      customerXPosVec(1) := Mux(customerRandomValuesVec(2) >= 450.U,450.S,Mux(customerRandomValuesVec(2) <= 96.U, 96.S, customerRandomValuesVec(2)))
      }

    }
    is(despawn) {
    
    }
    is(animate) {
    }
    is(delays) {
      when(customer1SpawnedReg) {
        customer1AnimCycleReg := customer1AnimCycleReg + 1.U
      }
      when(customer2SpawnedReg) {
        customer2AnimCycleReg := customer2AnimCycleReg + 1.U
      }

      when(!(customer1SpawnDelayReg === 0.U)) {
        customer1SpawnDelayReg := customer1SpawnDelayReg - 1.U
      }
      when(!(customer2SpawnDelayReg === 0.U)) {
        customer2SpawnDelayReg := customer2SpawnDelayReg - 1.U
      }
      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
