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
  val customerXReg               = Vec(Customers, RegInit(0.S(11.W)))
  val customerYReg               = Vec(Customers, RegInit(0.S(10.W)))
  val customerIdleVisibleReg     = Vec(Customers, RegInit(false.B))
  val customerDrinkingVisibleReg = Vec(Customers, RegInit(false.B))
  val customerSpawnedReg         = Vec(Customers, RegInit(false.B))
  val customerAnimCycleReg       = Vec(Customers, RegInit(0.U(7.W)))
  val customerAnimDirReg         = Vec(Customers, RegInit(true.B))
  val customerScoreDoneReg       = Vec(Customers, RegInit(false.B))
  val customerSeatXReg           = Vec(Customers, RegInit(1.U(4.W)))
  val customerSeatYReg           = Vec(Customers, RegInit(2.U(2.W)))
  val customerSpawnDelayReg      = Vec(Customers, RegInit(0.U(9.W)))
  val customerFlippedReg         = Vec(Customers, RegInit(false.B))

  // common / shared regs
  val customerToSpawnReg = RegInit(0.U(2.W))
  val customerToDespawnReg = RegInit(0.U(2.W))
  val customerDrinkingDelayReg = RegInit(0.U(8.W))
  val customerDrinkingAnimCycleReg = RegInit(0.U(2.W))
  val customerBegunScoringReg = RegInit(0.U(2.W))
  val xSpawnValues = VecInit(128.S, 200.S, 275.S, 352.S)
  val ySpawnValues = VecInit(192.S, 256.S, 320.S, 384.S)

  /////////////////////////////////////////////////////
  //
  // RESET
  //
  /////////////////////////////////////////////////////

  when(io.resetIn) {
    customerIdleVisibleReg(0) := (false.B)
    customerDrinkingVisibleReg(0) := (false.B)

    customerIdleVisibleReg(1) := (false.B)
    customerDrinkingVisibleReg(1) := (false.B)
  }.elsewhen(io.resetIn && !RegNext(io.resetIn)) {
    customerIdleVisibleReg(0) := (true.B)
    customerDrinkingVisibleReg(0) := (true.B)

    customerIdleVisibleReg(1) := (true.B)
    customerDrinkingVisibleReg(1) := (true.B)
  }

  /////////////////////////////////////////////////////
  //
  // io connections
  //
  /////////////////////////////////////////////////////
  io.done := false.B
  io.customerPosX(0) := customerXReg(0)
  io.customerPosY(0) := customerYReg(0)
  io.customerIdleVisible(0) := customerIdleVisibleReg(0)
  io.customerDrinkingVisible(0) := customerDrinkingVisibleReg(0)
  io.customerScoreDone(0) := customerScoreDoneReg(0)
  io.customerFlipped(0) := customerFlippedReg(0)

  io.customerPosX(1) := customerXReg(1)
  io.customerPosY(1) := customerYReg(1)
  io.customerIdleVisible(1) := customerIdleVisibleReg(1)
  io.customerDrinkingVisible(1) := customerDrinkingVisibleReg(1)
  io.customerScoreDone(1) := customerScoreDoneReg(1)
  io.customerFlipped(1) := customerFlippedReg(1)

  customerScoreDoneReg(0) := false.B
  customerScoreDoneReg(1) := false.B

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
      // if customer not spawned, and customer delay is 0, spawn customer.
      when(!customerSpawnedReg(0) && (customerSpawnDelayReg(0) === 0.U)) {
        customerSeatXReg(0) := customerSeatXReg(0) + random.LFSR(
          degreeOfRandom,
          true.B
        )
        
        customerSeatYReg(0) := customerSeatYReg(0) + random.LFSR(
          degreeOfRandom,
          true.B
        )

        when(customerSeatYReg(0) === customerSeatYReg(0)) {
          // if they are at the same seat, just wrap around and pick a new lane.
          // also move one two to the right, to make it seem more random.
          customerSeatYReg(0) := customerSeatYReg(0) + 1.U

        }

        customerXReg(0) := xSpawnValues(customerSeatXReg(0))
        customerYReg(0) := ySpawnValues(customerSeatYReg(0))
        customerIdleVisibleReg(0) := true.B
        customerSpawnedReg(0) := true.B
        customerSpawnDelayReg(0) := 240.U
        customerAnimCycleReg(0) := 0.U
        customerAnimDirReg(0) := true.B
      }

      when(!customerSpawnedReg(1) && (customerSpawnDelayReg(1) === 0.U)) {
        customerSeatXReg(1) := customerSeatXReg(1) + random.LFSR(
          degreeOfRandom,
          true.B
        )

        customerSeatYReg(1) := customerSeatYReg(1) + random.LFSR(
          degreeOfRandom,
          true.B
        )

        when(customerSeatYReg(0) === customerSeatYReg(1)){
          //same here.
          customerSeatYReg(1) := customerSeatYReg(1) + 1.U
        }

        customerXReg(1) := xSpawnValues(customerSeatXReg(1))
        customerYReg(1) := ySpawnValues(customerSeatYReg(1))
        customerIdleVisibleReg(1) := true.B
        customerSpawnedReg(1) := true.B
        customerSpawnDelayReg(1) := 240.U
        customerAnimCycleReg(1) := 0.U
        customerAnimDirReg(1) := true.B
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
      } .elsewhen(customerBegunScoringReg === 2.U) {
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
      when(customerSpawnedReg(0)) {
        when(customerAnimCycleReg(0) > 59.U) {
          customerAnimCycleReg(0) := 0.U

          when(customerAnimDirReg(0)) {
            customerYReg(0) := customerYReg(0) + 2.S
          }.otherwise {
            customerYReg(0) := customerYReg(0) - 2.S
          }

          customerAnimDirReg(0) := !customerAnimDirReg(0)
        }
      }

      when(customerSpawnedReg(1)) {
        when(customerAnimCycleReg(1) === 60.U) {
          customerAnimCycleReg(1) := 0.U

          when(customerAnimDirReg(1)) {
            customerYReg(1) := customerYReg(1) + 2.S
          }.otherwise {
            customerYReg(1) := customerYReg(1) - 2.S
          }

          customerAnimDirReg(1) := !customerAnimDirReg(1)
        }
      }

      stateReg := delays
    }

    is(delays) {
      when(customerSpawnedReg(0)) {
        customerAnimCycleReg(0) := customerAnimCycleReg(0) + 1.U
      }

      when(customerSpawnedReg(1)) {
        customerAnimCycleReg(1) := customerAnimCycleReg(1) + 1.U
      }

      when(!(customerSpawnDelayReg(0) === 0.U)) {
        customerSpawnDelayReg(0) := customerSpawnDelayReg(0) - 1.U
      }

      when(!(customerSpawnDelayReg(1) === 0.U)) {
        customerSpawnDelayReg(1) := customerSpawnDelayReg(1) - 1.U
      }

      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
