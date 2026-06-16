#!/bin/bash

trap 'echo; echo "FAILED"; read -p "Press Enter to continue..."; exit 1' ERR

echo "==================================="
echo "SBT RUN"
echo "==================================="

sbt run

echo "==================================="
echo "VIVADO"
echo "==================================="

vivado -mode batch -source build.tcl

echo "==================================="
echo "PROGRAMMING DEVICE"
echo "==================================="

vivado -mode batch -source program.tcl

echo
echo "SUCCESS"
read -p "Press Enter to continue..."
