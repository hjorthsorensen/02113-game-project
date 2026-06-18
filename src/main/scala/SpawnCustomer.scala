import chisel3._
import chisel3.util._

class SpawnCustomer(degreeOfRandom: Int, Customers: Int) extends Module {
  val io = IO(new Bundle {
    // STATUS
    val work = Input(Bool())
    val beerDone = Input(Bool())

    val customerScored = Input(Vec(Customers, Bool()))

    val resetIn = Input(Bool())

    val customerScoreDone = Output(Vec(Customers, Bool()))

    val done = Output(Bool())

    // SPRITES
    val customerPosX = Output(Vec(Customers, SInt(11.W)))
    val customerPosY = Output(Vec(Customers, SInt(10.W)))
    val customerIdleVisible = Output(Vec(Customers, Bool()))
    val customerDrinkingVisible = Output(Vec(Customers, Bool()))
    val customerFlipped = Output(Vec(Customers, Bool()))
  })

  ///////////////////////////////////////////
  //
  // REGISTERS
  //
  ///////////////////////////////////////////
  val idle :: spawn :: despawn :: delays :: animate :: done :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  // CUSTOMER VECTORS
  val customerXReg               = RegInit(VecInit.fill(Customers)(0.S(11.W)))
  val customerYReg               = RegInit(VecInit.fill(Customers)(0.S(10.W)))
  val customerIdleVisibleReg     = RegInit(VecInit.fill(Customers)(false.B))
  val customerDrinkingVisibleReg = RegInit(VecInit.fill(Customers)(false.B))
  val customerSpawnedReg         = RegInit(VecInit.fill(Customers)(false.B))
  val customerAnimCycleReg       = RegInit(VecInit.fill(Customers)(0.U(7.W)))
  val customerAnimDirReg         = RegInit(VecInit.fill(Customers)(true.B))
  val customerScoreDoneReg       = RegInit(VecInit.fill(Customers)(false.B))
  
  val customerSeatXReg           = RegInit(VecInit.fill(Customers)(1.U(3.W)))
  val customerSeatYReg           = RegInit(VecInit.fill(Customers)(2.U(2.W)))
  
  val customerSpawnDelayReg      = RegInit(VecInit.fill(Customers)(0.U(9.W)))
  val customerFlippedReg         = RegInit(VecInit.fill(Customers)(false.B))

  // SHARED REGISTERS
  val customerToSpawnReg           = RegInit(0.U(2.W))
  val customerToDespawnReg         = RegInit(0.U(2.W))
  val customerDrinkingDelayReg     = RegInit(0.U(8.W))
  val customerDrinkingAnimCycleReg = RegInit(0.U(2.W))
  val customerBegunScoringReg      = RegInit(0.U(2.W))
  val isFirstSpawn                 = RegInit(true.B)

  val noise = random.LFSR(degreeOfRandom, true.B)

  val xSpawnValues = VecInit(128.S(11.W), 200.S(11.W), 275.S(11.W), 352.S(11.W),160.S(11.W), 232.S(11.W), 307.S(11.W), 384.S(11.W))
  val ySpawnValues = VecInit(192.S(10.W), 256.S(10.W), 320.S(10.W), 384.S(11.W))

  when (isFirstSpawn) {
    isFirstSpawn := false.B
    for (i <- 0 until Customers) {
      val randX = noise(2 + i, 0 + i)
      val randY = noise(4 + i, 3 + i)

      val nextSeatX = customerSeatXReg(i) + randX
      val nextSeatY = customerSeatYReg(i) + randY
      
      customerSeatXReg(i) := nextSeatX
      if (i > 0) {
        when (nextSeatY === customerSeatYReg(i - 1)) {
          customerSeatYReg(i) := nextSeatY + 1.U
        } .otherwise {
          customerSeatYReg(i) := nextSeatY
        }
      } else {
        customerSeatYReg(i) := nextSeatY
      }
    
      customerXReg(i) := xSpawnValues(customerSeatXReg(i))
      customerYReg(i) := ySpawnValues(customerSeatYReg(i))
    }
  }
  
  /////////////////////////////////////////////////////
  //
  // RESET & IO CONNECTIONS
  //
  /////////////////////////////////////////////////////
  for (i <- 0 until Customers) {

    io.customerPosX(i) := customerXReg(i)
    io.customerPosY(i) := customerYReg(i)
    io.customerIdleVisible(i) := customerIdleVisibleReg(i)
    io.customerDrinkingVisible(i) := customerDrinkingVisibleReg(i)
    io.customerScoreDone(i) := customerScoreDoneReg(i)
    io.customerFlipped(i) := customerFlippedReg(i)
    
    customerScoreDoneReg(i) := false.B
    
    when(io.resetIn) {
      io.customerIdleVisible(i) := false.B  
      
      io.customerDrinkingVisible(i) := false.B

      stateReg := done
      isFirstSpawn := true.B
    }
  }

  io.done := false.B
  ///////////////////////////////////////////////////////
  //
  // statemachine
  //
  ///////////////////////////////////////////////////////

  switch(stateReg) {
    is(idle) {
      when(io.work) {
        stateReg := spawn
      }
    }

    is(spawn) {
      for (i <- 0 until Customers) {
        // if customer not spawned, and customer delay is 0, spawn customer.
        when((!customerSpawnedReg(i) && (customerSpawnDelayReg(i) === 0.U))) {

          val randX = noise(2 + i, 0 + i)
          val randY = noise(4 + i, 3 + i)

          val nextSeatX = customerSeatXReg(i) + randX
          val nextSeatY = customerSeatYReg(i) + randY
          
          customerSeatXReg(i) := nextSeatX

          if (i > 0) {
            when (nextSeatY === customerSeatYReg(i - 1)) {
              customerSeatYReg(i) := nextSeatY + 1.U
            } .otherwise {
              customerSeatYReg(i) := nextSeatY
            }
          } else {
            customerSeatYReg(i) := nextSeatY
          }
        

          customerXReg(i) := xSpawnValues(customerSeatXReg(i))
          customerYReg(i) := ySpawnValues(customerSeatYReg(i))
          customerIdleVisibleReg(i) := true.B
          customerSpawnedReg(i) := true.B
          customerSpawnDelayReg(i) := 240.U
          customerAnimCycleReg(i) := 0.U
          customerAnimDirReg(i) := true.B
        }
      }

      stateReg := despawn
    }

    is (despawn) {

      for (i <- 0 until Customers) {
        when(io.customerScored(i)) {
          customerBegunScoringReg := (i + 1).U
        }

        when(customerBegunScoringReg === (1 + i).U) {
          customerDrinkingVisibleReg(i) := true.B
          customerIdleVisibleReg(i) := false.B
          
          // change to drinking sprite
          when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 0.U) {
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
          } .elsewhen(customerDrinkingDelayReg === 35.U && customerDrinkingAnimCycleReg === 0.U) {
            // going back to idle, and waiting for 4 frames
            customerDrinkingVisibleReg(i) := false.B
            customerIdleVisibleReg(i) := true.B
            customerDrinkingAnimCycleReg := 1.U
            customerDrinkingDelayReg := 0.U
          }

          when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U) {
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
          } .elsewhen((customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U) {
            customerDrinkingVisibleReg(i) := true.B
            customerIdleVisibleReg(i) := false.B
            customerDrinkingAnimCycleReg := 2.U
            customerDrinkingDelayReg := 0.U
          }

          when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 2.U) {
            customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
          } .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 2.U) {
            customerXReg(i) := 0.S
            customerYReg(i) := 0.S
            customerIdleVisibleReg(i) := false.B
            customerDrinkingVisibleReg(i) := false.B
            customerSpawnedReg(i) := false.B
            customerScoreDoneReg(i) := true.B
            customerBegunScoringReg := 0.U
            customerDrinkingAnimCycleReg := 0.U
            customerDrinkingDelayReg := 0.U
            customerSpawnDelayReg(i) := 240.U
          }
        } 
      }

      // set io.customer1ScoreDone & io.customer2ScoreDone true, when done with animation.
      stateReg := animate
    }

    is(animate) {
      for (i <- 0 until Customers) {
        when(customerSpawnedReg(i)) {
          when(customerAnimCycleReg(i) > 59.U) {
            customerAnimCycleReg(i) := 0.U

            when(customerAnimDirReg(i)) {
              customerYReg(i) := customerYReg(i) + 2.S
            }.otherwise {
              customerYReg(i) := customerYReg(i) - 2.S
            }

            customerAnimDirReg(i) := !customerAnimDirReg(i)
          }
        }
      }

      stateReg := delays
    }

    is(delays) {
      for (i <- 0 until Customers) {
        when(customerSpawnedReg(i)) {
          customerAnimCycleReg(i) := customerAnimCycleReg(i) + 1.U
        }

        when(!(customerSpawnDelayReg(i) === 0.U)) {
          customerSpawnDelayReg(i) := customerSpawnDelayReg(i) - 1.U
        }
      }

      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
