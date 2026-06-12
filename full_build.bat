@echo off

echo ===================================
echo SBT RUN
echo ===================================
call sbt run

echo ===================================
echo VIVADO
echo ===================================

vivado -mode batch -source build.tcl

echo ===================================
echo PROGRAMMING DEVICE
echo ===================================

vivado -mode batch -source program.tcl