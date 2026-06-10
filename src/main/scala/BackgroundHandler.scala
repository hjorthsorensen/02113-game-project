class BackgroundHandler extends Module{
    val io = IO(new Bundle {
        //Inputs
        
        val work = Input(Bool())
        val scoreWriteEnable = Input(Bool())
        
        

        //Outputs for background
        val scoreAdress = Output(UInt(10.W))
        val scoreTileID = Output(UInt(5.W))
        val writeEnable = Output(Bool())

        val scoreWriteDone = Output(Bool())
        
        
        val done = Output(Bool())

    })

    val scoreTileAmountReg = RegInit(0.U(4.W))
    val scoreWriteDoneReg = RegInit(false.B)
    
    val idle :: busy :: doneMovement :: Nil = Enum(3)
    val stateReg = RegInit(idle)
}
