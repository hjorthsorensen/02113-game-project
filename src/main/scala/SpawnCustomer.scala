import chisel3._
import chisel3.util._

class SpawnCustomer() extends Module {
  val io = IO(new Bundle {
    val work = Input(Bool())
    val beerDone = Input(Bool())
    val customer1Scored = Input(Bool())
    val customer2Scored = Input(Bool())
    val done = Output(Bool())
    val customer1PosX = Output(SInt(11.W))
    val customer1PosY = Output(SInt(11.W))
    val customer2PosX = Output(SInt(11.W))
    val customer2PosY = Output(SInt(11.W))
    val customer1Visible = Output(Bool())
    val customer2Visible = Output(Bool())
  })

  // customers default spawn is (0,0); position is picked in spawn state
  val customer1XReg = RegInit(0.S(11.W))
  val customer1YReg = RegInit(0.S(10.W))
  val customer1Visible = RegInit(false.B)

  val customer2XReg = RegInit(0.S(11.W))
  val customer2YReg = RegInit(0.S(10.W))
  val customer2Visible = RegInit(false.B)

  val customerToSpawn = RegInit(0.U(2.W))
  val customerToDespawn = RegInit(0.U(2.W))
  // reg to decide what customer to spawn

  // io connections
  io.done := false.B
  io.customer1PosX := customer1XReg
  io.customer1PosY := customer1YReg
  io.customer2PosX := customer2XReg
  io.customer2PosY := customer2YReg
  io.customer1Visible := customer1Visible
  io.customer2Visible := customer2Visible
  customerToDespawn := Cat(io.customer1Scored, io.customer2Scored)
  customerToSpawn := Cat(io.customer1Visible,io.customer2Visible)
  // statemachine
  val idle :: spawn :: despawn :: done :: Nil = Enum(4)
  val stateReg = RegInit(idle)

  switch(stateReg) {

    is(idle) {
      when(io.work) {
        stateReg := spawn
      }.elsewhen(io.beerDone) {
        stateReg := despawn
      }
      stateReg := done
    }

    is(spawn) {
      switch(customerToSpawn) {
        is(0.U) {
          customer1XReg := 150.S
          customer1YReg := 220.S
          customer1Visible := true.B
        }
        is(1.U) {
          customer2XReg := 300.S
          customer2YReg := 220.S
          customer2Visible := true.B

          is(2.U) {
            customer1XReg := 150.S
            customer1YReg := 220.S
            customer1Visible := true.B
            stateReg := done
          }
          //if both already spawned, just go to done
          is(3.U) {
            stateReg := done
          }
        }
      }
      stateReg := done

    }
    is(despawn) {
      switch(customerToDespawn) {
        is(1.U) {
          customer1XReg := 0.S
          customer1YReg := 0.S
          customer1Visible := false.B
        }
        is(2.U) {
          customer2XReg := 0.S
          customer2YReg := 0.S
          customer2Visible := false.B
        }
        is(0.U) {
          stateReg := done
        }
        is(3.U) {
          stateReg := done
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
