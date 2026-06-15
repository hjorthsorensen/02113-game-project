import chisel3._
import chisel3.util._

class SpawnCustomer(degreeOfRandom: Int, Customers: Int) extends Module {
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
  val customerXPosVec = RegInit(VecInit(0.S(11.W),0.S(11.W),0.S(11.W)))
  val customerYPosVec = RegInit(VecInit(0.S(11.W),0.S(11.W),0.S(11.W)))
  val customerIdleVisibilityVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerDrinkingVisibilityVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerSpawnedVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerAnimCycleVec = RegInit(VecInit(0.U(2.W),0.U(2.W),0.U(2.W)))
  val customerAnimDelayCycleVec = RegInit(VecInit(0.U(7.W),0.U(7.W),0.U(7.W)))
  val customerScoreDoneVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerSeatYVec = RegInit(VecInit(0.U(2.W),0.U(2.W),0.U(2.W)))
  val customerFlippedVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerSpawnDelayReg = RegInit(0.U(9.W))
  val customerRandomValuesVec = RegInit(VecInit(0.U(9.W),0.U(9.W),0.U(9.W)))
  val customerBegunScoringVec = RegInit(VecInit(false.B,false.B,false.B))
  val customerAnimDirVec = RegInit(VecInit(false.B,false.B,false.B))
//randomX should be between ~64 and ~450.
  customerRandomValuesVec(0) := random.LFSR(32,true.B)
  customerRandomValuesVec(1) := random.LFSR(32,true.B)
  customerRandomValuesVec(2) := random.LFSR(32,true.B)

  //common / shared regs
  val customerToSpawnReg = RegInit(0.U(2.W))
  val customerToDespawnReg = RegInit(0.U(2.W))
  val customerDrinkingDelayVec = RegInit(VecInit((0.U(8.W)),(0.U(8.W)),(0.U(8.W))))
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


// animation handler





// end of animation handling


  // statemachine
  val idle :: spawnFirstCustomer :: spawnSecondCustomer :: spawnThirdCustomer :: despawn :: delays :: animate :: done :: Nil = Enum(8)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        //only spawn new customers when all customers are despawned, 
        //and 
        when((customerSpawnedVec(0) === false.B && customerSpawnedVec(1) === false.B && customerSpawnedVec(2) === false.B) && customerSpawnDelayReg === 0.U){
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
      customerXPosVec(0) := Mux(customerRandomValuesVec(0) >= 450.U,450.S,Mux(customerRandomValuesVec(0) <= 96.U, 96.S, customerRandomValuesVec(0).asSInt))
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
  customerIdleVisibilityVec(0) := true.B
  customerSpawnedVec(0) := true.B
  stateReg := spawnSecondCustomer
    }
    is(spawnSecondCustomer){
      //Check if we want to spawn second customer.
      when(Customers.U === 2.U){
        //first, get random position.
        customerXPosVec(1) := Mux(customerRandomValuesVec(1) >= 450.U,450.S,Mux(customerRandomValuesVec(1) <= 96.U, 96.S, customerRandomValuesVec(1).asSInt))
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
      customerIdleVisibilityVec(1) := true.B
      customerSpawnedVec(1) := true.B
      stateReg := spawnThirdCustomer

      
    }
    is(spawnThirdCustomer){
      when(Customers.U === 3.U){
        //set x value randomly; and just ensure it is a different table than the others are at.
      customerXPosVec(2) := Mux(customerRandomValuesVec(2) >= 450.U,450.S,Mux(customerRandomValuesVec(2) <= 96.U, 96.S, customerRandomValuesVec(2).asSInt))
      customerSeatYVec(2) := customerRandomValuesVec(2)
      when(!(customerSeatYVec(2) === customerSeatYVec(1)) && !(customerSeatYVec(2) === customerSeatYVec(0))){
          switch(customerSeatYVec(2)){
        is(0.U){
          customerYPosVec(2) := 192.S
          }
        is(1.U){
          customerYPosVec(2) := 256.S

          }
        is(2.U){
          customerYPosVec(2) := 320.S

          }
        is(3.U){
          customerYPosVec(2) := 384.S
          }
        }
        //naive method of ensuring that it goes to an empty row.
        //if we implement more customers at the same time than four,
        //we need a more robust solution.
      }.otherwise{
        when(!(customerSeatYVec(0) === 0.U) && !(customerSeatYVec(1) === 0.U)){
          customerSeatYVec(2) := 0.U
          customerYPosVec(2) := 192.S
        }.elsewhen(!(customerSeatYVec(0) === 1.U) && !(customerSeatYVec(1) === 1.U)){
          customerSeatYVec(2) := 1.U
          customerYPosVec(2) := 256.S          
        }.elsewhen(!(customerSeatYVec(0) === 2.U) && !(customerSeatYVec(1) === 2.U)){
          customerSeatYVec(2) := 2.U
          customerYPosVec(2) := 320.S          
        }.elsewhen(!(customerSeatYVec(0) === 3.U) && !(customerSeatYVec(1) === 3.U)){
          customerSeatYVec(2) := 3.U
          customerYPosVec(2) := 384.S          
        }
      }
      customerIdleVisibilityVec(2) := true.B
      
      } //end of third customer

    //we dont have to go to despawn.
    //when we spawn customers, there is no possibility
    //(or it should not be possible)
    //that customers despawn.
    stateReg := animate

    }
    is(despawn) {
      when(io.customer1Scored){
        customerBegunScoringVec(0) := true.B
        customerAnimCycleVec(0) := 1.U


      }.elsewhen(io.customer2Scored){
        customerBegunScoringVec(1) := true.B
        customerAnimCycleVec(1) := 1.U

      // we currently dont have the io signals from outside to handle third customer.
      //add io.customer3Scored in FSMScores.

       }
      //.elsewhen(io.customer3Scored){
      //   customerBegunScoringVec(2) := true.B
      //    }

       stateReg := animate
    }
    // is(animate) {
    //   // customer despawn animations here
    //   //customer one despawn here
      
    // }
    is(animate){
      //first customer 
when(customerBegunScoringVec(0)  && !(customerDrinkingDelayVec(0) === 60.U) && customerAnimCycleVec(0) === 1.U) {
        customerDrinkingVisibilityVec(0) := true.B
        customerIdleVisibilityVec(0) := false.B
        customerDrinkingDelayVec(0) := customerDrinkingDelayVec(0) + 1.U
      }.elsewhen(customerBegunScoringVec(0) && customerDrinkingDelayVec(0) === 0.U && customerAnimCycleVec(0) === 1.U){
        customerDrinkingVisibilityVec(0) := false.B
        customerIdleVisibilityVec(0) := true.B
        customerDrinkingDelayVec(0) := 0.U
        customerAnimCycleVec(0) := 2.U
      }.elsewhen(customerBegunScoringVec(0)  && !(customerDrinkingDelayVec(0) === 60.U) && customerAnimCycleVec(0) === 2.U){
        customerDrinkingVisibilityVec(0) := true.B
        customerIdleVisibilityVec(0) := false.B
        customerDrinkingDelayVec(0) := customerDrinkingDelayVec(0) + 1.U
    }.elsewhen(customerBegunScoringVec(0) && customerDrinkingDelayVec(0) === 0.U && customerAnimCycleVec(0) === 2.U){
            customerXPosVec(0) := 0.S
            customerYPosVec(0) := 0.S
            customerIdleVisibilityVec(0) := false.B
            customerDrinkingVisibilityVec(0) := false.B
            customerSpawnedVec(0) := false.B
            customerScoreDoneVec(0) := true.B
            customerBegunScoringVec(0) := 0.U
            customerAnimCycleVec(0) := 0.U
            customerDrinkingDelayVec(0) := 0.U
            when(customerScoreDoneVec(0) && customerScoreDoneVec(1)){
            customerSpawnDelayReg := 240.U
            }

        //customer two despawn here
          when(customerBegunScoringVec(1)  && !(customerDrinkingDelayVec(1) === 60.U) && customerAnimCycleVec(1) === 1.U) {
        customerDrinkingVisibilityVec(1) := true.B
        customerIdleVisibilityVec(1) := false.B
        customerDrinkingDelayVec(1) := customerDrinkingDelayVec(1) + 1.U
      }.elsewhen(customerBegunScoringVec(1) && customerDrinkingDelayVec(1) === 0.U && customerAnimCycleVec(1) === 1.U){
        customerDrinkingVisibilityVec(1) := false.B
        customerIdleVisibilityVec(1) := true.B
        customerDrinkingDelayVec(1) := 0.U
        customerAnimCycleVec(1) := 2.U
      }.elsewhen(customerBegunScoringVec(1)  && !(customerDrinkingDelayVec(1) === 60.U) && customerAnimCycleVec(1) === 2.U){
        customerDrinkingVisibilityVec(1) := true.B
        customerIdleVisibilityVec(1) := false.B
        customerDrinkingDelayVec(1) := customerDrinkingDelayVec(1) + 1.U
    }.elsewhen(customerBegunScoringVec(1) && customerDrinkingDelayVec(1) === 0.U && customerAnimCycleVec(1) === 2.U){
            customerXPosVec(1) := 0.S
            customerYPosVec(1) := 0.S
            customerIdleVisibilityVec(1) := false.B
            customerDrinkingVisibilityVec(1) := false.B
            customerSpawnedVec(1) := false.B
            customerScoreDoneVec(1) := true.B
            customerBegunScoringVec(1) := 0.U
            customerAnimCycleVec(1) := 0.U
            customerDrinkingDelayVec(1) := 0.U
            when(customerScoreDoneVec(0) && customerScoreDoneVec(1)){
            customerSpawnDelayReg := 240.U
            }
          

    }
    }
    //add third customer despawn here, if we add it later...


      // animation cycle
      when(customerSpawnedVec(0)){
        when(customerAnimDelayCycleVec(0) > 59.U){
          customerAnimDelayCycleVec(0) := 0.U
          when(customerAnimDirVec(0)){
            customerYPosVec(0) := customerYPosVec(0) + 2.S

          }.otherwise{
            customerYPosVec(0) := customerYPosVec(0) - 2.S
          }
          customerAnimDirVec(0) := !customerAnimDirVec(0)
        }
      }.elsewhen(customerSpawnedVec(1)){
        when(customerAnimDelayCycleVec(1) > 59.U){
          customerAnimDelayCycleVec(1) := 0.U
          when(customerAnimDirVec(1)){
            customerYPosVec(1) := customerYPosVec(1) + 2.S

          }.otherwise{
            customerYPosVec(1) := customerYPosVec(1) - 2.S
          }
          customerAnimDirVec(1) := !customerAnimDirVec(1)
        }
      }
      stateReg := delays
    }



    is(delays) {
      when(customerSpawnedVec(0)) {
        customerAnimCycleVec(0) := customerAnimCycleVec(0) + 1.U
      }
      when(customerSpawnedVec(1)) {
        customerAnimCycleVec(1) := customerAnimCycleVec(1) + 1.U
      }
      when(customerSpawnedVec(2)) {
        customerAnimCycleVec(2) := customerAnimCycleVec(2) + 1.U
      }

      when(!(customerSpawnDelayReg === 0.U)) {
        customerSpawnDelayReg := customerSpawnDelayReg - 1.U
      }
      customerAnimDelayCycleVec(0) := customerAnimDelayCycleVec(0) + 1.U
      customerAnimDelayCycleVec(1) := customerAnimDelayCycleVec(1) + 1.U
      //customerAnimDelayCycleVec(2) := customerAnimDelayCycleVec(2) + 1.U
      stateReg := done
    }

    is(done) {
      io.done := true.B
      stateReg := idle
    }
  }

}
