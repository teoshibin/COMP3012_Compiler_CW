rem clean and remove all compiled tam during testing
@echo off

echo.
echo Cleaning up...
del program\test0.tam
del program\test1.tam
del program\test2.tam
del program\test3.tam
del program\test4.tam
del program\test5.tam
if exist program\error1.tam del program\error1.tam
if exist program\error2.tam del program\error2.tam
if exist program\error3.tam del program\error3.tam
echo Done