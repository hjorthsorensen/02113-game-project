import chisel3._
import chisel3.util._

class SpawnCustomer2(degreeOfRandom: Int, Customers: Int) extends Module {
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

  // CUSTOMER 1
  val customerXReg               = RegInit(VecInit.fill(Customers)(0.S(11.W)))
  val customerYReg               = RegInit(VecInit.fill(Customers)(0.S(10.W)))
  val customerIdleVisibleReg     = RegInit(VecInit.fill(Customers)(false.B))
  val customerDrinkingVisibleReg = RegInit(VecInit.fill(Customers)(false.B))
  val customerSpawnedReg         = RegInit(VecInit.fill(Customers)(false.B))
  val customerAnimCycleReg       = RegInit(VecInit.fill(Customers)(0.U(7.W)))
  val customerAnimDirReg         = RegInit(VecInit.fill(Customers)(true.B))
  val customerScoreDoneReg       = RegInit(VecInit.fill(Customers)(false.B))
  val customerSeatXReg           = RegInit(VecInit.fill(Customers)(1.U(4.W)))
  val customerSeatYReg           = RegInit(VecInit.fill(Customers)(2.U(2.W)))
  val customerSpawnDelayReg      = RegInit(VecInit.fill(Customers)(0.U(9.W)))
  val customerFlippedReg         = RegInit(VecInit.fill(Customers)(false.B))

  // common / shared regs
  val customerToSpawnReg = RegInit(0.U(2.W))
  val customerToDespawnReg = RegInit(0.U(2.W))
  val customerDrinkingDelayReg = RegInit(0.U(8.W))
  val customerDrinkingAnimCycleReg = RegInit(0.U(2.W))
  val customerBegunScoringReg = RegInit(0.U(2.W))
  val xSpawnValues = VecInit(128.S, 200.S, 275.S, 352.S)
  val ySpawnValues = VecInit(192.S, 256.S, 320.S, 384.S)


  for (i <- 0 until Customers) {
    /////////////////////////////////////////////////////
    //
    // RESET
    //
    /////////////////////////////////////////////////////
    when(io.resetIn) {
      customerIdleVisibleReg(i) := (false.B)
      customerDrinkingVisibleReg(i) := (false.B)
    }.elsewhen(io.resetIn && !RegNext(io.resetIn)) {
      customerIdleVisibleReg(i) := (true.B)
      customerDrinkingVisibleReg(i) := (true.B)
    }

    /////////////////////////////////////////////////////
    //
    // io connections
    //
    /////////////////////////////////////////////////////
    io.customerPosX(i) := customerXReg(i)
    io.customerPosY(i) := customerYReg(i)
    io.customerIdleVisible(i) := customerIdleVisibleReg(i)
    io.customerDrinkingVisible(i) := customerDrinkingVisibleReg(i)
    io.customerScoreDone(i) := customerScoreDoneReg(i)
    io.customerFlipped(i) := customerFlippedReg(i)
    
    customerScoreDoneReg(i) := false.B
  }

  io.done := false.B
  ///////////////////////////////////////////////////////
  //
  // statemachine
  //
  ///////////////////////////////////////////////////////
  val idle :: spawn :: despawn :: delays :: animate :: done :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        stateReg := spawn
      }
    }

    is(spawn) {
      for (i <- 0 until Customers) {
        // if customer not spawned, and customer delay is 0, spawn customer.
        when(!customerSpawnedReg(i) && (customerSpawnDelayReg(i) === 0.U)) {
          customerSeatXReg(i) := customerSeatXReg(i) + random.LFSR(
            degreeOfRandom,
            true.B
          )
          
          customerSeatYReg(i) := customerSeatYReg(i) + random.LFSR(
            degreeOfRandom,
            true.B
          )

          when(customerSeatYReg(0) === customerSeatYReg(1)) {
            // if they are at the same seat, just wrap around and pick a new lane.
            // also move one two to the right, to make it seem more random.
            customerSeatYReg(i) := customerSeatYReg(i) + 1.U

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
      when(io.customerScored(0)) {
        customerBegunScoringReg := 1.U
      }

      when(io.customerScored(1)) {
        customerBegunScoringReg := 2.U
      }

      when(customerBegunScoringReg === 1.U) {
        customerDrinkingVisibleReg(0) := true.B
        customerIdleVisibleReg(0) := false.B
        
        // change to drinking sprite
        when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 0.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen(customerDrinkingDelayReg === 35.U && customerDrinkingAnimCycleReg === 0.U) {
          // going back to idle, and waiting for 4 frames
          customerDrinkingVisibleReg(0) := false.B
          customerIdleVisibleReg(0) := true.B
          customerDrinkingAnimCycleReg := 1.U
          customerDrinkingDelayReg := 0.U
        }

        when(!(customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen((customerDrinkingDelayReg === 35.U) && customerDrinkingAnimCycleReg === 1.U) {
          customerDrinkingVisibleReg(0) := true.B
          customerIdleVisibleReg(0) := false.B
          customerDrinkingAnimCycleReg := 2.U
          customerDrinkingDelayReg := 0.U
        }

        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 2.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 2.U) {
          customerXReg(0) := 0.S
          customerYReg(0) := 0.S
          customerIdleVisibleReg(0) := false.B
          customerDrinkingVisibleReg(0) := false.B
          customerSpawnedReg(0) := false.B
          customerScoreDoneReg(0) := true.B
          customerBegunScoringReg := 0.U
          customerDrinkingAnimCycleReg := 0.U
          customerDrinkingDelayReg := 0.U
          customerSpawnDelayReg(0) := 240.U
        }
      } 
      
      when(customerBegunScoringReg === 2.U) {
        customerDrinkingVisibleReg(1) := true.B
        customerIdleVisibleReg(1) := false.B
        
        // change to drinking sprite
        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 0.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 0.U) {
          // going back to idle, and waiting for 4 frames
          customerDrinkingVisibleReg(1) := false.B
          customerIdleVisibleReg(1) := true.B
          customerDrinkingAnimCycleReg := 1.U
          customerDrinkingDelayReg := 0.U
        }

        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 1.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen((customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 1.U) {
          customerDrinkingVisibleReg(1) := true.B
          customerIdleVisibleReg(1) := false.B
          customerDrinkingAnimCycleReg := 2.U
          customerDrinkingDelayReg := 0.U
        }

        when(!(customerDrinkingDelayReg === 45.U) && customerDrinkingAnimCycleReg === 2.U) {
          customerDrinkingDelayReg := customerDrinkingDelayReg + 1.U
        } .elsewhen(customerDrinkingDelayReg === 45.U && customerDrinkingAnimCycleReg === 2.U) {
          customerXReg(1) := 0.S
          customerYReg(1) := 0.S
          customerIdleVisibleReg(1) := false.B
          customerDrinkingVisibleReg(1) := false.B
          customerSpawnedReg(1) := false.B
          customerScoreDoneReg(1) := true.B
          customerBegunScoringReg := 0.U
          customerDrinkingAnimCycleReg := 0.U
          customerDrinkingDelayReg := 0.U
          customerSpawnDelayReg(1) := 240.U
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
