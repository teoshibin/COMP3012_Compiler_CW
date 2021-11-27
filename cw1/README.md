# MT to TAM Compiler

A compiler that extanded Arithmetic Expression to Mini Triangle Language using functional parsers.

**Uses Material from:**   
Compilers COMP3012, University of Nottingham, 2021   
Venanzio Capretta / Nicolai Kraus   

## Contents

1. **FunParser.hs**

    Library for Functional Parser

2. **MTParser.hs**

    Concrete Implementation of FunParser.
    Define abstract syntax trees of mini triangle.

3. **TAM.hs**

    Defines TAM programs. Containing parser  & execution function for TAM programs

4. **MTTAM.hs**
 
    A code generator for converting the abstract syntax trees into TAM.

5. **MTCompiler.hs**

    Uses all the modules above to define a compiler.

6. **Main.hs**

    Main executable, see [usage](##Usage) for extra detail

7. **Testing Folder**   
    [MT Program Files and TAM Program Files](testing)

## Usage   
### MT to TAM Compiler

Main file. You can use a command such as (or run `cpcpl.bat` for the same result, see [details](###Scripts))
```batch
    ghc Main -o mtc
```
Produces executable compiler that compiles files containing mini triangle and executes TAM code according to file extension. This executable can be used on files with two extensions `.mt` and `.tam`. Files with ending `.mt` in [testing folder](testing) containing mini triangle programs. It can be compiled using (add prefix `./` in some other terminal)
```batch
    mtc {filename}.{extension} --{options}
```
- extensions `mt` `tam`
- options
    - `trace` can be added while using `run` option to trace execution stack
    - `run` execute compiled TAM code
    - `evaluate` interpret `.mt` file without compiling into TAM
    - `parse` print input strings and print parsed abstract syntax tree

This should generate a `.tam` file containing the TAM code for `.mt` file. If running `.tam` file it will execute the TAM code and print the final result.   


### Scripts
- `cpcpl.bat`   
    Compile the compiler using this batch or execute the line below
    ```batch
        ghc Main -o mtc
    ```

    Produces executable compiler that compiles files containing mini triangle and executes TAM code according to file extension. This executable can be used on files with two extensions `.mt` and `.tam`. Files with ending `.mt` in [testing folder](testing) containing mini triangle programs. It can be compiled using (add prefix `./` in some other terminal)
    ```batch
        mtc {filename}.{extension} --{options}
    ```
    - extensions `mt` `tam`
    - options
        - `trace` can be added while using `run` option to trace execution stack
        - `run` execute compiled TAM code
        - `evaluate` interpret `.mt` file without compiling into TAM
        - `parse` print input strings and print parsed abstract syntax tree
        - `compile` compiling MT into TAM without execution

    This should generate a `.tam` file containing the TAM code for `.mt` file. If running `.tam` file it will execute the TAM code and print the final result.

- `test.bat`
    - run following in sequence
        1. `cpall.bat` (`cpcpl.bat` is called in here)
        2. `cmpall.bat`
        3. `clnall.bat`

- `cpall.bat`
    - Compile all testing MT programs within [testing folder](testing)   
    - `cpcpl.bat` will be called by this script by default
- `cmpall.bat`   
    - Compare all compiled test MT with answers, e.g compare `test0.tam` `test0_ans.tam`
    - `CompareFiles.hs` will be recompiled by default
- `clnall.bat`   
    - remove all compile test MT programs

