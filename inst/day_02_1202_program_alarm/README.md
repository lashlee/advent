Day 2
================
John
12/9/2019

``` r
library(advent)
library(magrittr)
```

# Introduction

Yesterday I read the input multiple times. I think today the more
prosocial behavior will be to read it as few times as possible,
hopefully only once.

``` r
input <- 
  get_data('https://adventofcode.com/2019/day/2/input') %>% 
  scan(text = ., sep = ',', quiet = TRUE)
```

# Problem

> An Intcode program is a list of integers separated by commas (like
> 1,0,0,3,99). To run one, start by looking at the first integer (called
> position 0). Here, you will find an opcode - either 1, 2, or 99. The
> opcode indicates what to do; for example, 99 means that the program is
> finished and should immediately halt. Encountering an unknown opcode
> means something went wrong.
> 
> Opcode 1 adds together numbers read from two positions and stores the
> result in a third position. The three integers immediately after the
> opcode tell you these three positions - the first two indicate the
> positions from which you should read the input values, and the third
> indicates the position at which the output should be stored.
> 
> …
> 
> Opcode 2 works exactly like opcode 1, except it multiplies the two
> inputs instead of adding them. Again, the three integers after the
> opcode indicate where the inputs and outputs are, not their values.
> 
> Once you’re done processing an opcode, move to the next one by
> stepping forward 4 positions.
> 
>   - 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
>   - 2,3,0,3,99 becomes 2,3,0,6,99 (3 \* 2 = 6).
>   - 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 \* 99 = 9801).
>   - 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
> 
> Once you have a working computer, the first step is to restore the
> gravity assist program (your puzzle input) to the “1202 program alarm”
> state it had just before the last computer caught fire. To do this,
> before running the program, replace position 1 with the value 12 and
> replace position 2 with the value 2. What value is left at position 0
> after the program halts?

OK, so now to write the action.

``` r
parse_intcode <- function(intcode, position = 1) {
  opcode <- intcode[position]
  if (opcode == 99) return(intcode)
  if (opcode == 1) f = sum
  if (opcode == 2) f = prod
  position_to_replace <- 1 + intcode[position + 3]
  value_to_insert <- f(intcode[1 + intcode[position + 1]], intcode[1 + intcode[position + 2]])
  parse_intcode(replace(intcode, position_to_replace, value_to_insert), position + 4)
}
```

Note that R vectors are
[one-indexed](https://stackoverflow.com/questions/3135325/why-do-vector-indices-in-r-start-with-1-instead-of-0)
so I keep that convention here by starting the position at 1.

Now for the provided tests.

``` r
identical(parse_intcode(c(1, 0, 0, 0, 99)), c(2, 0, 0, 0, 99))
```

    ## [1] TRUE

``` r
identical(parse_intcode(c(2, 3, 0, 3, 99)), c(2, 3, 0, 6, 99))
```

    ## [1] TRUE

``` r
identical(parse_intcode(c(2, 4, 4, 5, 99, 0)), c(2, 4, 4, 5, 99, 9801))
```

    ## [1] TRUE

``` r
identical(parse_intcode(c(1, 1, 1, 4, 99, 5, 6, 0, 99)), c(30, 1, 1, 4, 2, 5, 6, 0, 99))
```

    ## [1] TRUE

Great, all tests pass.

Quick reminder: the solution calls for some manual corrections and asks
what the value of the first element of the intcode is after the program
stops.

``` r
part_1 <- function(input = NULL) {
  if (is.null(input)) {
    input <- 
      get_data('https://adventofcode.com/2019/day/2/input') %>% 
      scan(text = ., sep = ',', quiet = TRUE)
  }
  input %>% 
  replace(c(2, 3), c(12, 2)) %>% 
  parse_intcode() %>% 
  `[`(1) %>% 
  write_to_clipboard() %>% 
  print()
}
part_1(input)
```

    ## [1] 3790645

Great, it works.
