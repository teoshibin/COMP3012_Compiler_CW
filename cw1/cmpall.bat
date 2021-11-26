@echo off

echo Recompiling testing script...
ghc CompareFiles -o cmpf
echo Done

echo.
echo Test Compiled programs...
echo.
cmpf testing\test0.tam testing\test0_ans.tam 
cmpf testing\test1.tam testing\test1_ans.tam 
cmpf testing\test2.tam testing\test2_ans.tam 
cmpf testing\test3.tam testing\test3_ans.tam 
cmpf testing\test4.tam testing\test4_ans.tam 
cmpf testing\test5.tam testing\test5_ans.tam 
echo.
echo Done

