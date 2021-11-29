@echo off

echo.
echo Compiling compiler...
ghc Main -o mtc
echo Done

echo.
echo Compiling testing programs...
mtc testing\test0.mt 
mtc testing\test1.mt 
mtc testing\test2.mt 
mtc testing\test3.mt 
mtc testing\test4.mt 
mtc testing\test5.mt 
mtc testing\error1.mt
mtc testing\error2.mt
mtc testing\error3.mt
echo Done
echo.
 