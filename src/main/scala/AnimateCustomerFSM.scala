import chisel3._
import chisel3.util._

class SpawnCustomer(degreeOfRandom: Int, Customers: Int) extends Module {











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

      when(!(customerSpawnDelayReg === 0.U)) {
        customerSpawnDelayReg := customerSpawnDelayReg - 1.U
      }
      when(customerSpawnedVec(0)){
      customerAnimDelayCycleVec(0) := customerAnimDelayCycleVec(0) + 1.U
      }
      when(customerSpawnedVec(1)){
      customerAnimDelayCycleVec(1) := customerAnimDelayCycleVec(1) + 1.U
      }



}