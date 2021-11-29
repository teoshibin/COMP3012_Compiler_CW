rem compile all MT files into TAM
@echo off

echo.
echo Compiling compiler...
call cpcpl
echo Done

echo.
echo Compiling testing programs...
mtc program\test0.mt 
mtc program\test1.mt 
mtc program\test2.mt 
mtc program\test3.mt 
mtc program\test4.mt 
mtc program\test5.mt 
mtc program\error1.mt
mtc program\error2.mt
mtc program\error3.mt
echo Done
echo.
 