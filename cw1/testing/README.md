# Testing

## Program Files
Program directory contains a couple of test files.

- [test0.mt](..\program\test0.mt)   
- [test1.mt](..\program\test1.mt)   
- [test2.mt](..\program\test2.mt)   
- [test3.mt](..\program\test3.mt)   
- [test4.mt](..\program\test4.mt)   
- [test5.mt](..\program\test5.mt)   

## Compiled Program Files
The compiler should produce files that contain the same TAM program as below:

- [test0_ans.tam](..\program\test0_ans.tam) 
- [test1_ans.tam](..\program\test1_ans.tam) 
- [test2_ans.tam](..\program\test2_ans.tam) 
- [test3_ans.tam](..\program\test3_ans.tam)   
- [test4_ans.tam](..\program\test4_ans.tam)  
- [test5_ans.tam](..\program\test5_ans.tam)   

## Executed Program Files
- test0.tam > input 12 > output 479001600
- test1.tam > output 21
- test2.tam > output 55
- test3.tam > input 4 > outputs 2 and 1 (for background, read about the Collatz conjecture)
- test4.tam > no output 
- test5.tam > output 100

## Error Program Files
- [error1.mt](error1.mt)   
This is not a correct MT program and can't be compiled to TAM code. Your compiler should ideally produce an error message.
- [error2.mt](error2.mt)   
This has a semicolon that shouldn't be there. Apart from that, it is okay with the convention that `var x` means `var x := 0`. Without the semicolon, it would be fine to treat it as a correct program (not an error). The current version should ideally produce a compile error.
- [error3.mt](error3.mt) (previously test_6.mt)   
This can't be compiled to TAM code. Ideally, your compiler should produce an error message.



