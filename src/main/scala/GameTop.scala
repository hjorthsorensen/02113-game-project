//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// This is the game top. It connects all the modules, it includes the
// synchronizers and debouncers for the buttons and switches, and it
// manages the delayed reset release for the sub-modules.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chisel3.util._

class GameTop extends Module {
  val io = IO(new Bundle {
    //Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    //VGA Connector
    val vgaRed = Output(UInt(4.W))
    val vgaBlue = Output(UInt(4.W))
    val vgaGreen = Output(UInt(4.W))
    val Hsync = Output(Bool())
    val Vsync = Output(Bool())

    //Switches
    val  sw = Input(Vec(8, Bool()))

    //Leds
    val led = Output(Vec(8, Bool()))

    //Errors
    val missingFrameError = Output(Bool())
    val backBufferWriteError = Output(Bool())
    val viewBoxOutOfRangeError = Output(Bool())
  })

  val SPRITE_NUMBER = 16
  val BACK_TILE_NUMBER = 32
  val graphicEngineVGA = Module(new GraphicEngineVGA(SPRITE_NUMBER, BACK_TILE_NUMBER))

  //Uncomment one of the following lines to use the module related to the learning tasks
  //Use the module GameLogic to implement your game
  //val gameLogic = Module(new GameLogic(SPRITE_NUMBER, BACK_TILE_NUMBER))
  val gameLogic = Module(new GameLogicTask0(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask1(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask2(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask3(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask4(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask5(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask6(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask7(SPRITE_NUMBER, BACK_TILE_NUMBER))
  //val gameLogic = Module(new GameLogicTask8(SPRITE_NUMBER, BACK_TILE_NUMBER))

  //Debouncing
  val CLOCK_FREQUENCY_HZ = 100000000 //100 MHz
  val DEBOUNCE_PERIOD_US = 20000 //20 ms
  val DEBOUNCE_COUNTER_MAX = CLOCK_FREQUENCY_HZ / 1000000 * DEBOUNCE_PERIOD_US
  val debounceCounter = RegInit(0.U(log2Up(DEBOUNCE_COUNTER_MAX).W))
  val debounceSampleEn = Wire(Bool())
  when(debounceCounter === (DEBOUNCE_COUNTER_MAX - 1).U){
    debounceCounter := 0.U
    debounceSampleEn := true.B
  } .otherwise {
    debounceCounter := debounceCounter + 1.U
    debounceSampleEn := false.B
  }

  //Releasing the reset for graphicEngineVGA and gameLogic to wait for the debouncers to sample the input
  val RESET_RELEASE_PERIOD_US = 2 * DEBOUNCE_PERIOD_US //40ms
  val RESET_RELEASE_COUNTER_MAX = CLOCK_FREQUENCY_HZ / 1000000 * RESET_RELEASE_PERIOD_US
  val resetReleaseCounter = RegInit(0.U(log2Up(RESET_RELEASE_COUNTER_MAX).W))
  val delayedReset = Wire(Bool())
  when(resetReleaseCounter === (RESET_RELEASE_COUNTER_MAX - 1).U) {
    delayedReset := false.B
  }.otherwise {
    delayedReset := true.B
    resetReleaseCounter := resetReleaseCounter + 1.U
  }
  graphicEngineVGA.reset := delayedReset
  gameLogic.reset := delayedReset

  //Buttons synchronization and debouncing
  val btnCState = RegEnable(RegPipeline(io.btnC, 3, false.B), false.B, debounceSampleEn)
  val btnUState = RegEnable(RegPipeline(io.btnU, 3, false.B), false.B, debounceSampleEn)
  val btnLState = RegEnable(RegPipeline(io.btnL, 3, false.B), false.B, debounceSampleEn)
  val btnRState = RegEnable(RegPipeline(io.btnR, 3, false.B), false.B, debounceSampleEn)
  val btnDState = RegEnable(RegPipeline(io.btnD, 3, false.B), false.B, debounceSampleEn)
  gameLogic.io.btnC := btnCState
  gameLogic.io.btnU := btnUState
  gameLogic.io.btnL := btnLState
  gameLogic.io.btnR := btnRState
  gameLogic.io.btnD := btnDState

  //VGA
  io.vgaRed := graphicEngineVGA.io.vgaRed
  io.vgaGreen := graphicEngineVGA.io.vgaGreen
  io.vgaBlue := graphicEngineVGA.io.vgaBlue
  io.Hsync := graphicEngineVGA.io.Hsync
  io.Vsync := graphicEngineVGA.io.Vsync

  //Switches synchronization and debouncing
  for (i <- 0 until 8) {
    gameLogic.io.sw(i) := RegEnable(RegPipeline(io.sw(i), 3, false.B), false.B, debounceSampleEn)
  }

  //Leds
  io.led := gameLogic.io.led

  //Errors
  io.missingFrameError := graphicEngineVGA.io.missingFrameError
  io.backBufferWriteError := graphicEngineVGA.io.backBufferWriteError
  io.viewBoxOutOfRangeError := graphicEngineVGA.io.viewBoxOutOfRangeError

  //Sprite control input
  graphicEngineVGA.io.spriteXPosition := gameLogic.io.spriteXPosition
  graphicEngineVGA.io.spriteYPosition := gameLogic.io.spriteYPosition
  graphicEngineVGA.io.spriteVisible := gameLogic.io.spriteVisible
  graphicEngineVGA.io.spriteFlipHorizontal := gameLogic.io.spriteFlipHorizontal
  graphicEngineVGA.io.spriteFlipVertical := gameLogic.io.spriteFlipVertical

  //Viewbox control input
  graphicEngineVGA.io.viewBoxX := gameLogic.io.viewBoxX
  graphicEngineVGA.io.viewBoxY := gameLogic.io.viewBoxY

  //Background buffer input
  graphicEngineVGA.io.backBufferWriteData := gameLogic.io.backBufferWriteData
  graphicEngineVGA.io.backBufferWriteAddress := gameLogic.io.backBufferWriteAddress
  graphicEngineVGA.io.backBufferWriteEnable := gameLogic.io.backBufferWriteEnable

  //Status
  gameLogic.io.newFrame := graphicEngineVGA.io.newFrame
  graphicEngineVGA.io.frameUpdateDone := gameLogic.io.frameUpdateDone

}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
