@echo off

echo Recompiling testing script...
call testing\cpcf.bat
echo Done

echo.
echo Test Compiled programs...
echo.
testing\cmpf program\test0.tam program\test0_ans.tam 
testing\cmpf program\test1.tam program\test1_ans.tam 
testing\cmpf program\test2.tam program\test2_ans.tam 
testing\cmpf program\test3.tam program\test3_ans.tam 
testing\cmpf program\test4.tam program\test4_ans.tam 
testing\cmpf program\test5.tam program\test5_ans.tam 
echo.
echo Done

