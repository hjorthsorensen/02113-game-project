import chisel3._
import chisel3.util._

class SpawnCustomerNotWorkingEither(degreeOfRandom: Int, Customers: Int) extends Module {
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

//logical spawn variables
    val spawnCustomersNow = RegInit(false.B)

//logical despawn variables
    val customerScoredVec = RegInit(VecInit(false.B,false.B))
    //val anyCustomerScored = RegInit(false.B)
    anyCustomerScored := customerScoredVec(0) || customerScoredVec(1)


    val randomVal = random.LFSR(16)
    val legalYPositions = VecInit(192.S,256.S,320.S,384.S)

//FSM



val idle :: spawnCustomers :: despawn :: done :: Nil = Enum(4)
  val stateReg = RegInit(idle)

  switch(stateReg) {
    is(idle){
        when(io.work){
            when(spawnCustomersNow){
                stateReg := spawnCustomers
            }
            when(anyCustomerScored){
                stateReg := despawn
            }
        }
    }


    is(spawnCustomers){
        stateReg := done
        customerXPosVec(0) := Mux(randomVal < 80.U, 80.U, Mux(randomVal > 450.U, 450.U, randomVal))
        customerXPosVec(1) := Mux(randomVal < 80.U, 80.U, Mux(randomVal > 450.U, 450.U, randomVal))
        customerYPosVec(0)
        customerIdleVisibilityVec(0) := true.B
        customerIdleVisibilityVec(1) := true.B

        
        

    }
    is(despawn){
        stateReg := done
    }

  }



//end of program
}