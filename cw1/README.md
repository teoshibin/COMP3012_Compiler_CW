# MT to TAM Compiler

A compiler that extanded Arithmetic Expression to Mini Triangle Language using functional parsers.

## Table of contents

  - [Files](#files)
  - [Usage](#usage)
    - [Compiler](#compiler)
    - [Scripts](#scripts)

## Folders
* root/
    * [program/](program)   
        * Containing MT and TAM programs
    * [testing/](testing)   
        * Automation Batch Scripts related to testing
        * Some haskell code for testing
        * Expected ouput for testing program in dedicated README
    * [tmp/](tmp)
        * This will only be generated using command in `cpcpl.bat` to store haskell compilation `.o` & `.hi` files
    * **Files that are not in subfolders**   
        *Libaraies*
        * [FunParser.hs](FunParser.hs) Library for Functional Parser
        * [StateC.hs](StateC.hs) Library for Functional State Returning
        * [GlobalFunc.hs](GlobalFunc.hs) Globally Useful Functions   
    
        *Compiler*   
        * [Parser.hs](Parser.hs) Concrete Implementation of FunParser. Define AST.
        * [CodeGen.hs](CodeGen.hs) Compile AST into TAM Instructions
        * [TAM.hs](TAM.hs) TAM Instruction Parsing, Execution and TAM toString Function
        * [Compiler.hs](Compiler.hs) General Function to compile MT into TAM
        * [Main.hs](Main.hs) Compiler Command line IO  

        *Other*   
        * [cpcpl.bat](cpcpl.bat) Compile Compiler
        * [test.bat](test.bat) Main testing script
        * [mtc.exe](mtc.exe) MT to TAM Executable
        * [README.md](README.md) You are reading this

## Usage   
### Compiler

Run below code or run `cpcpl.bat` to compile the compiler
```batch
    ghc Main -o mtc -outputdir tmp
```
Produces executable compiler that compiles files containing mini triangle and executes TAM code according to file extension. This executable can be used on files with two extensions `.mt` and `.tam`. Files with ending `.mt` in [testing folder](testing) contains mini triangle programs. It can be compiled using below code (add prefix `./` in some other terminal)
```batch
    mtc {filename}.{extension} {Arguments}
```
**Examples** 
```batch
    mtc program/test0.mt
    mtc program/test1.mt --run --trace-all
    mtc program/test2.mt --evaluate --trace-parser
    mtc program/test0.tam --trace-stack
```
 **Extensions**   
 - `mt` 
 - `tam`   

**Arguments**   
- Main Args
    - `no arguments`    
        - Compile `.mt` without executing   
        - Execute `.tam`
    - `--run`    
        - Execute compiled code for `.mt`   
        - *No effect for `.tam`*
    - `--evaluate`   
        - Interpret `.mt` without compiling   
        - *No effect for `.tam`*
- Optional Args
    - `--trace-parser`   
        - Print abstract syntax tree   
        - *Effective when passing `.mt`*   
        - *No effect for `.tam`*
    - `--trace-stack`   
        - Trace execution stack   
        - *Effective when `--run` is specified for `.mt`*   
        - *Effective when passing `.tam`*
    - `--trace-all`   
        - Trace all traceable information of given task    
        - *Effectiveness based on individual trace args*   

### Scripts
- `cpcpl.bat`
    - compile compiler  into `mtc.exe`

- `test.bat`
    - see [testing outcome](testing)
    - run following in sequence
        1. `cpall.bat` (`cpcpl.bat` is called within here)
        2. `cmpall.bat`
        3. `clnall.bat`
- `cpall.bat`
    - Compile all testing MT programs within [Program](program)   
    - `cpcpl.bat` is called at the beginning of this script by default
- `cmpall.bat`
    - Compare all generated TAM against predefined TAM answers
    - `CompareFiles.hs` will be recompiled at the beginning by default
- `clnall.bat`
    - Remove all compiled programs

