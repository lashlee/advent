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

# Solving the Challenges

## Challenge 1

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

## Challenge 2

This one is a doozy\! I’ll just paste the answer in its entirety rather
than try to edit for brevity.

> Intcode programs are given as a list of integers; these values are
> used as the initial state for the computer’s memory. When you run an
> Intcode program, make sure to start by initializing memory to the
> program’s values. A position in memory is called an address (for
> example, the first value in memory is at “address 0”).
> 
> Opcodes (like 1, 2, or 99) mark the beginning of an instruction. The
> values used immediately after an opcode, if any, are called the
> instruction’s parameters. For example, in the instruction 1,2,3,4, 1
> is the opcode; 2, 3, and 4 are the parameters. The instruction 99
> contains only an opcode and has no parameters.
> 
> The address of the current instruction is called the instruction
> pointer; it starts at 0. After an instruction finishes, the
> instruction pointer increases by the number of values in the
> instruction; until you add more instructions to the computer, this is
> always 4 (1 opcode + 3 parameters) for the add and multiply
> instructions. (The halt instruction would increase the instruction
> pointer by 1, but it halts the program instead.)
> 
> “With terminology out of the way, we’re ready to proceed. To complete
> the gravity assist, you need to determine what pair of inputs produces
> the output 19690720.”
> 
> The inputs should still be provided to the program by replacing the
> values at addresses 1 and 2, just like before. In this program, the
> value placed in address 1 is called the noun, and the value placed in
> address 2 is called the verb. Each of the two input values will be
> between 0 and 99, inclusive.
> 
> Once the program has halted, its output is available at address 0,
> also just like before. Each time you try a pair of inputs, make sure
> you first reset the computer’s memory to the values in the program
> (your puzzle input) - in other words, don’t reuse memory from a
> previous attempt.
> 
> Find the input noun and verb that cause the program to produce the
> output 19690720. What is 100 \* noun + verb? (For example, if noun=12
> and verb=2, the answer would be 1202.)

So I need to find the two replacement values, noun and verb respectively
which produce the solution 19690720, and then compute `100 * noun +
verb`.

First I’ll revisit my part 1 solution and parametrize it by the noun and
verb.

``` r
part_1_revisited <- function(noun, verb, input = NULL) {
  if (is.null(input)) {
    input <- 
      get_data('https://adventofcode.com/2019/day/2/input') %>% 
      scan(text = ., sep = ',', quiet = TRUE)
  }
  res <-
    input %>% 
    replace(c(2, 3), c(noun , verb)) %>% 
    parse_intcode() %>% 
    `[`(1)
  res
}
```

And just for good measure, check that the revisited parametrized
solution gives identical output to the prior solution for the same noun
and verb.

``` r
identical(part_1_revisited(12, 2, input), part_1(input))
```

    ## [1] 3790645

    ## [1] TRUE

Great, it does.

Now write the world’s dumbest solver. It will try noun and verb equal to
1. Then it will increment one, then the other, etc., all the while
checking if the output is 19690720.

``` r
part_2 <- function(noun = 0, verb = 0, input = NULL, add_to = 'noun', verbose = TRUE) {
  if (is.null(input)) {
    input <- 
      get_data('https://adventofcode.com/2019/day/2/input') %>% 
      scan(text = ., sep = ',', quiet = TRUE)
  }
  check <- part_1_revisited(noun, verb, input)
  if (verbose) {
    cat(paste0('Noun, Verb, Check: ', noun, ', ', verb, ', ', check, '\n'))
  }
  if (noun + verb > 10) return('Eh, whatever, I\'ll find the bug later!')
  if (check == 19690720) {
    res <- print(write_to_clipboard(100 * noun + verb))
    return(res)
  } else if (add_to == 'noun') {
    part_2(noun + 1, verb, input = input, add_to = 'verb', verbose = verbose)
  } else if (add_to == 'verb') {
    part_2(noun, verb + 1, input = input, add_to = 'noun', verbose = verbose)
  }
}
```

``` r
part_2(input = input, verbose = TRUE)
```

    ## Noun, Verb, Check: 0, 0, 190643
    ## Noun, Verb, Check: 1, 0, 490643
    ## Noun, Verb, Check: 1, 1, 490644
    ## Noun, Verb, Check: 2, 1, 790644
    ## Noun, Verb, Check: 2, 2, 790645
    ## Noun, Verb, Check: 3, 2, 1090645
    ## Noun, Verb, Check: 3, 3, 1090646
    ## Noun, Verb, Check: 4, 3, 1390646
    ## Noun, Verb, Check: 4, 4, 1390647
    ## Noun, Verb, Check: 5, 4, 1690647
    ## Noun, Verb, Check: 5, 5, 1690648
    ## Noun, Verb, Check: 6, 5, 1990648

    ## [1] "Eh, whatever, I'll find the bug later!"

And there is a bug in here\! Eh, I’ll find it later. :)

## The next day…

The bug was that I wasn’t checking all combinations\! I’m going to check
all noun and verb values which are less than or equal to the length of
the input. If that doesn’t work, I will try a different solver. I will
try to find the minimum noun, verb combination.

``` r
part_2_second_pass <- function(critical_value = 19690720, input = NULL) {
  if (is.null(input)) {
    input <- 
      get_data('https://adventofcode.com/2019/day/2/input') %>% 
      scan(text = ., sep = ',', quiet = TRUE)
  }
  small_combs <- expand.grid(noun = seq_along(input), verb = seq_along(input))
  small_combs <- small_combs[order(small_combs$noun + small_combs$verb, small_combs$noun), ]
  part_1_vals <- mapply(
    function(noun, verb) part_1_revisited(noun, verb, input = input), 
    small_combs$noun, 
    small_combs$verb
  )
  n_v <- small_combs[which(part_1_vals == critical_value), ][1, ]
  print(write_to_clipboard(100 * n_v$noun + n_v$verb))
}
```

``` r
part_2_second_pass(input = input)
```

    ## [1] 6577

Great, it works. See you later for more puzzles.
