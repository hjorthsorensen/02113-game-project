# 02113 Digital Systems Design Game Project

## Requirements to play the game:
-Basys 3 FPGA Board  
-VGA compatible screen  
-VGA cable  
-Xilinx Vivado  
-IDE to generate files for vivado  
-SBT ([download here](https://www.scala-sbt.org/download/))  
-SCALA ([download here](https://www.scala-lang.org/download/))  
-a DFROBOT max98357a DAC + amplifier module  

## Instructions
Physical steps:  

connect the DAC module to the board. can sit in a pmod connector.  

Fork / Clone project onto your machine  
IDE steps:  

Open in IDE  
Open terminal and run 'sbt run' and await completion.  
Create project in Vivado and add:  

Design Sources:  
- Top.v  - top module
- RamSpWf.v  
- RamInitSpWf.v  

Constraints  
- GameBasys3.xdc  

ensure, that J2,G2,L2 are set to IO_BCLK, IO_DIN, IO_LRC, respectively.  

Now Generate Bitstream, open the hardware manager and program device.  

Upon reaching the main menu, press the center button to enter the game.  
