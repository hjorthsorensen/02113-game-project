import chisel3._
import chisel3.util._

class SpawnCustomer2(degreeOfRandom: Int) extends Module {
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
  val customerSeatXVec = VecInit(0.U(4.W),0.U(4.W),0.U(4.W))
  val customerSeatYVec = VecInit(0.U(4.W),0.U(4.W),0.U(4.W))
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
  val someCustomerScored = RegInit(false.B)
  someCustomerScored := io.customer1Scored === true.B || io.customer2Scored === true.B



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
  val idle :: spawn :: despawn :: delays :: animate :: done :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        //only spawn new customers when all customers are despawned, 
        //and 
        when((customerSpawnedVec(0) === false.B && customerSpawnedVec(1) === false.B && customerSpawnedVec(2) === false.B)){
            stateReg := spawn
        }.elsewhen(customerScored)
      }
    }

    is(spawn) {

      // if customer not spawned, and customer delay is 0, spawn customer.
      when(!customer1SpawnedReg && (customer1SpawnDelayReg === 0.U)) {
        customer1SeatXReg := customer1SeatXReg + random.LFSR(degreeOfRandom,true.B)
        customer1SeatYReg := customer1SeatYReg + random.LFSR(degreeOfRandom,true.B)

            when(customer1SeatYReg === customer2SeatYReg){
                //if they are at the same seat, just wrap around and pick a new lane.
                //also move one two to the right, to make it seem more random.
                customer1SeatYReg := customer1SeatYReg + 1.U


        }
        customer1XReg := xSpawnValues(customer1SeatXReg)
        customer1YReg := ySpawnValues(customer1SeatYReg)
       customer1IdleVisibleReg := true.B
        customer1SpawnedReg := true.B
        customer1SpawnDelayReg := 240.U
        customer1AnimCycleReg := 0.U
        customer1AnimDirReg := true.B
      }
      when(!customer2SpawnedReg && (customer2SpawnDelayReg === 0.U)) {
        customer2SeatXReg := customer2SeatXReg + random.LFSR(degreeOfRandom,true.B)
        customer2SeatYReg := customer2SeatYReg + random.LFSR(degreeOfRandom,true.B)
        //     when(customer1SeatYReg === customer2SeatYReg){
        //             //same here.
        //         customer2SeatYReg := customer2SeatYReg + 1.U

        // }
        customer2XReg := xSpawnValues(customer2SeatXReg)
        customer2YReg := ySpawnValues(customer2SeatYReg)
        customer2IdleVisibleReg := true.B
        customer2SpawnedReg := true.B
        customer2SpawnDelayReg := 240.U
        customer2AnimCycleReg := 0.U
        customer2AnimDirReg := true.B
      }

      stateReg := despawn

    }
    is(despawn) {
      when(io.customer1Scored) {
        customerBegunScoringReg := 1.U
      }
      when(io.customer2Scored){
        customerBegunScoringReg := 2.U
      }
      when(customerBegunScoringReg === 1.U){
        customer1DrinkingVisibleReg := true.B
       customer1IdleVisibleReg := false.B
        //change to drinking sprite
        when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 0.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        }
        .elsewhen(customerDrinkingDelayReg === 35.U && customerDrinkingAnimCycleReg === 0.U){
            //going back to idle, and waiting for 4 frames
        customer1DrinkingVisibleReg := false.B
       customer1IdleVisibleReg := true.B
        customerDrinkingAnimCycleReg := 1.U
        customerDrinkingDelayReg := 0.U
        }
        when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U 
        }
        .elsewhen((customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U){
            customer1DrinkingVisibleReg := true.B
           customer1IdleVisibleReg := false.B
            customerDrinkingAnimCycleReg := 2.U
            customerDrinkingDelayReg := 0.U
        }
        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 2.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        }
        .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 2.U){
            customer1XReg := 0.S
            customer1YReg := 0.S
           customer1IdleVisibleReg := false.B
            customer1DrinkingVisibleReg := false.B
            customer1SpawnedReg := false.B
            customer1ScoreDoneReg := true.B
            customerBegunScoringReg := 0.U
            customerDrinkingAnimCycleReg := 0.U
            customerDrinkingDelayReg := 0.U
            customer1SpawnDelayReg := 240.U
        }






      }.elsewhen(customerBegunScoringReg === 2.U) {
        customer2DrinkingVisibleReg := true.B
       customer2IdleVisibleReg := false.B
        //change to drinking sprite
        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 0.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        }
        .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 0.U){
            //going back to idle, and waiting for 4 frames
        customer2DrinkingVisibleReg := false.B
       customer2IdleVisibleReg := true.B
        customerDrinkingAnimCycleReg := 1.U
        customerDrinkingDelayReg := 0.U
        }
        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 1.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U 
        }
        .elsewhen((customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 1.U){
            customer2DrinkingVisibleReg := true.B
           customer2IdleVisibleReg := false.B
            customerDrinkingAnimCycleReg := 2.U
            customerDrinkingDelayReg := 0.U
        }
        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 2.U){
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        }
        .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 2.U){
            customer2XReg := 0.S
            customer2YReg := 0.S
           customer2IdleVisibleReg := false.B
            customer2DrinkingVisibleReg := false.B
            customer2SpawnedReg := false.B
            customer2ScoreDoneReg := true.B
            customerBegunScoringReg := 0.U
            customerDrinkingAnimCycleReg := 0.U
            customerDrinkingDelayReg := 0.U
            customer2SpawnDelayReg := 240.U
        }
      }




        //set io.customer1ScoreDone & io.customer2ScoreDone true, when done with animation.
      stateReg := animate
    }
    is(animate) {
      when(customer1SpawnedReg) {
        when(customer1AnimCycleReg > 59.U) {
          customer1AnimCycleReg := 0.U
          when(customer1AnimDirReg) {
            customer1YReg := customer1YReg + 2.S
          }.otherwise{
            customer1YReg := customer1YReg - 2.S
          }
            customer1AnimDirReg := !customer1AnimDirReg
        }
      }

      when(customer2SpawnedReg) {
        when(customer2AnimCycleReg === 60.U) {
          customer2AnimCycleReg := 0.U
          when(customer2AnimDirReg){
          customer2YReg := customer2YReg + 2.S

          }.otherwise{
          customer2YReg := customer2YReg - 2.S

          }
          customer2AnimDirReg := !customer2AnimDirReg
        }
      }
      stateReg := delays
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
