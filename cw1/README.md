# MT to TAM Compiler

A compiler that extanded Arithmetic Expression to Mini Triangle Language using functional parsers.

**Uses Material from:**   
Compilers COMP3012, University of Nottingham, 2021
Venanzio Capretta / Nicolai Kraus

## Contents:

1. **FunParser.hs**

    Library for Functional Parser

2. **MTParser.hs**

    Concrete Implementation of FunParser.
    Define abstract syntax trees of mini triangle.

3. **TAM.hs**

    Defines TAM programs. Contains an extra parser for TAM programs to take a string and read it as a TAM program.

4. **MTTAM.hs**
 
    A code generator of trasnlating the abstract syntax trees defined in [MTParser.hs](MTParser.hs) to TAM.

5. **MTCompiler.hs**

    Uses all the modules above to define a compiler.

6. **Main.hs**

    Main file. You can use a command such as
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

