//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// The tester for the game logic block.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class GameLogicTester extends AnyFlatSpec with ChiselScalatestTester {

  "GameLogicTester" should "pass" in {
    test(new GameLogic(16, 32)) { dut =>
      println("Running the GameLogic Tester")

      dut.clock.setTimeout(0)

      dut.io.btnR.poke(false.B)
      dut.io.btnL.poke(false.B)
      dut.io.btnU.poke(false.B)
      dut.io.btnD.poke(false.B)
      dut.io.btnC.poke(false.B)
      dut.io.newFrame.poke(false.B)
      dut.clock.step(1)

      dut.io.newFrame.poke(true.B)
      dut.clock.step(1)

      dut.io.newFrame.poke(false.B)
      dut.clock.step(998)

      println("End of GameLogicTester")
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////