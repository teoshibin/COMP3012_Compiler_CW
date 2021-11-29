# MT to TAM Compiler

A compiler that extanded Arithmetic Expression to Mini Triangle Language using functional parsers.

## Table of contents

  - [Files](#files)
  - [Usage](#usage)
    - [Compiler](#compiler)
    - [Scripts](#scripts)

## Files related to compiler

1. **FunParser.hs**

    Library for Functional Parser

2. **Parser.hs**

    Concrete Implementation of FunParser.
    Define abstract syntax trees of mini triangle.

3. **TAM.hs**

    Defines TAM programs. Containing parser  & execution function for TAM programs

4. **CodeGen.hs**
 
    A code generator for converting the abstract syntax trees into TAM.

5. **Compiler.hs**

    Uses all the modules above to define a compiler.

6. **main.hs**

    Main executable

7. **Testing Folder**   
    [MT Program Files & TAM Program Files](testing)

## Usage   
### Compiler

Run below code or run `cpcpl.bat` to compile the compiler
```batch
    ghc main -o mtc -outputdir tmp
```
Produces executable compiler that compiles files containing mini triangle and executes TAM code according to file extension. This executable can be used on files with two extensions `.mt` and `.tam`. Files with ending `.mt` in [testing folder](testing) contains mini triangle programs. It can be compiled using below code (add prefix `./` in some other terminal)
```batch
    mtc {filename}.{extension} {Arguments}
```
**Examples** 
```batch
    mtc testing/test0.mt
    mtc testing/test1.mt --run --trace-all
    mtc testing/test2.mt --evaluate --trace-parser
    mtc testing/test0.tam --trace-stack
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
    - run following in sequence
        1. `cpall.bat` (`cpcpl.bat` is called within here)
        2. `cmpall.bat`
        3. `clnall.bat`
- `cpall.bat`
    - Compile all testing MT programs within [testing folder](testing)   
    - `cpcpl.bat` is called at the beginning of this script by default
- `cmpall.bat`
    - Compare all generated TAM against predefined TAM answers
    - `CompareFiles.hs` will be recompiled at the beginning by default
- `clnall.bat`
    - remove all compile test MT programs

