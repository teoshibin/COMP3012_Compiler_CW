@echo off

echo.
echo Compiling compiler...
ghc Main -o mtc
echo Done

echo.
echo Compiling testing programs...
mtc testing\test0.mt --compile
mtc testing\test1.mt --compile
mtc testing\test2.mt --compile
mtc testing\test3.mt --compile
mtc testing\test4.mt --compile
mtc testing\test5.mt --compile
mtc testing\error1.mt --compile
mtc testing\error2.mt --compile
mtc testing\error3.mt --compile
echo Done
echo.
 