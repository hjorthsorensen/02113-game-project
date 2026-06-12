@echo off

echo ===================================
echo SBT RUN
echo ===================================
call sbt run
if errorlevel 1 goto fail

echo ===================================
echo VIVADO
echo ===================================

vivado -mode batch -source build.tcl
if errorlevel 1 goto fail

echo ===================================
echo PROGRAMMING DEVICE
echo ===================================

vivado -mode batch -source program.tcl
if errorlevel 1 goto fail

echo.
echo SUCCESS
pause
exit /b 0

:fail
echo.
echo FAILED
pause
exit /b 1