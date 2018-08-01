# [μ6](https://esolangs.org/wiki/Mu6)

This is an interpreter for μ6 - a programming language based on μ-recursive
functions:

```
usage: mu6 (-h | -e expr | file) [-a] [-v] INPUTS
  -e    --expr       evaluate an expression
  -a    --ascii      set ascii mode
  -6    --heximal    take user input in heximal format
  -v    --verbose    treat source as ascii string
  -t    --translate  translate source code
  -m M  --modulus=M  operate in N/M
  -h    --help       print this help
```

## Commands

Usually μ6 treats input as half-bytes where `0000` is used for padding
source-code to bytes at the beginning, however you can also use ascii source
code (`-v` flag):

| half-byte | ascii | function                      |
|:---------:|:-----:|-------------------------------|
|   `0000`  |  `0`  | digit 0                       |
|   `0001`  |  `1`  | digit 1                       |
|   `0010`  |  `2`  | digit 2                       |
|   `0011`  |  `3`  | digit 3                       |
|   `0100`  |  `4`  | digit 4                       |
|   `0101`  |  `5`  | digit 5                       |
|   `0110`  |  `[`  | begin of function composition |
|   `0111`  |  `]`  | end of function composition   |
|   `1000`  |  `/`  | projection                    |
|   `1001`  |  `.`  | constant zero-function        |
|   `1010`  |  `+`  | successor-function            |
|   `1011`  |  `,`  | pairing-function              |
|   `1100`  |  `<`  | left of pair or const         |
|   `1101`  |  `>`  | right of pair or const        |
|   `1110`  |  `#`  | primitive recursion           |
|   `1111`  |  `@`  | μ-operator (minimisation)     |


## Semantics

μ6 uses base6 integers in the source code and thus only needs digits `0-5`, a
program follows the following grammar:

```
PROGRAM  = FUNCTION [ INPUTS ]?

  NUMBER   = [ '0' .. '5' ]+

  INPUTS = NUMBER [ ',' NUMBER ]*

  FUNCTION = ATOM | PROJ | COMP | PRIM | MU

    ATOM = '.' | '+' | ',' | '<' | '>'
    PROJ = '/' NUMBER
    COMP = '[' FUNCTION FUNCTION* ']'
    PRIM = '#' FUNCTION FUNCTION
    MU   = '@' FUNCTION
```

For a program encoding the function *FUNCTION* with possibly constant inputs
*C0,..,CM*, invoking `mu6 program.mu in0 in1 .. inK` will evaluate the
μ-recursive function *FUNCTION(C0,..,CM,*`in0`*,..,*`inK`*)* and print the
result.

### μ-recursive functions

Usually these functions are *N^n -> N*, however a lot of interesting
computations make use of the `PAIR` function which Goedel-encodes two integers.
Since writing the encoding and decoding is tedious work μ6, provides three
functions additional to the basic μ-recursive functions/operators:

  - `,` takes the inputs and encodes them as a nested tuple
  - `<` extracts the left element
  - `>` extracts the right element

The remaining functions and operators work as usual:

  - `.` constant zero function, returns `0` for any inputs
  - `+` increments the first argument and returns the result
  - `/NUM` evaluates to the `NUM`th element (`0`-indexed)
  - `[f g0`*..*`gN]` evaluates to *f(g0(x0..xK)...gN(x0..xK))*
  - `#f g` evaluates to *f(x1..xK)* if *x0 = 0* and otherwise
     to *g(x0-1,*`#f g`*(x0-1,x1..xK),x1..xK)*
  - `@f` evaluates to the first *x* such that *f(x,x0..xK) == 0*


## Introduction to programming in μ6


### Hello, World!

Let me introduce the pairing-function `,` and the `-a` flag: The
pairing-function takes its arguments *x0..xN* and returns a nested tuple
*(x0,(..(x{N-1},xN)..))*. So the program `,` called with inputs `1`, `2` and
`3` would evaluate to `(1,(2,3))`. If we call μ6 with the `-a` flag it will
flatten the tuple and convert each number to an ASCII character. So `,` called
with `102`, `111`, `111` (in base10) and the `-a` flag would print `foo`.

However instead of supplying user input we can also provide constant inputs (in
base6) in the μ6-program itself:

    ,250,303,303

The first `,` is the μ6-program - simply encode the inputs as a tuple - and the
remaining `250,303,303` are the constant inputs `102,111,111`. So running that
with the `-a` flag will also print `foo`.

Now we can use the same idea for printing `Hello, World!`:

    ,200,245,300,300,303,112,52,223,303,310,300,244,53


### Addition

Let's start with a simple program that adds two numbers, for this we need to
formulate a binary function that adds the arguments. We don't have an addition
function yet, but adding *n* to a number is the same as incrementing the number
*n* times, we can use this idea to formulate a program:

    add( 0,x1) = x1
    add(x0,x1) = add(x0-1,x1)

The addition function can be written in terms of primitive recursion:

    add(x0,x1) = prim(project0, compose(succ,project1))

And as a μ6-program this would simply be:

    #/0[+/1]
