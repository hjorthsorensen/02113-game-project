set_param general.maxThreads 24

set verilog_files [glob -nocomplain *.v]

read_verilog $verilog_files

read_xdc vivado/Basys3Game/Basys3Game.srcs/constrs_1/imports/code/GameBasys3.xdc

synth_design -top Top -part xc7a35tcpg236-1

opt_design
place_design
route_design

write_bitstream -force Top.bit

exit