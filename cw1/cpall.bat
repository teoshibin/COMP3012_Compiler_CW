echo off

echo.
echo Compiling compiler...
call cpcpl.bat
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
echo Waiting for clean up
pause

echo.
echo Cleaning up...
del testing\test0.tam
del testing\test1.tam
del testing\test2.tam
del testing\test3.tam
del testing\test4.tam
del testing\test5.tam
if exist testing\error1.tam del testing\error1.tam
if exist testing\error2.tam del testing\error2.tam
if exist testing\error3.tam del testing\error3.tam
echo Done
 