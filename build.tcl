set_param general.maxThreads 24

set verilog_files [glob -nocomplain *.v]

read_verilog $verilog_files

read_xdc constraints.xdc

synth_design -top Top -part xc7a35tcpg236-1

opt_design
place_design
route_design

write_bitstream -force output/Top.bit

exit