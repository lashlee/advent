Day 1
================
John
12/8/2019

``` r
library(advent)
library(magrittr)
```

# Introduction

I found out about Advent of Code this year and I really love the idea\!
I will be documenting my progress here. I hope you enjoy reading my
thoughts.

# Utilities

First I need to write a few utilities for getting data from the Advent
of Code website and copying it to my clipboard.

## Writing to Clipboard

I want to copy my answers to the clipboard so I can paste them into the
website. Here I have a function to detect the operating system, then
copy to the clipboard if it is MacOS or Windows.

``` r
is_mac
```

    ## function() {
    ##   .Platform$OS.type == 'unix' & Sys.info()['sysname'] == 'Darwin'
    ## }
    ## <bytecode: 0x7fe5e8c4b208>
    ## <environment: namespace:advent>

``` r
is_windows
```

    ## function() {
    ##   .Platform$OS.type == 'windows'
    ## }
    ## <bytecode: 0x7fe5e8ebf3e0>
    ## <environment: namespace:advent>

``` r
write_to_clipboard
```

    ## function(data) {
    ##   if (is_mac()) cat(data, file = pipe('pbcopy'))
    ##   if (is_windows()) writeClipboard(as.character(data))
    ##   invisible(data)
    ## }
    ## <bytecode: 0x7fe5cfa5b7b8>
    ## <environment: namespace:advent>

Note that `write_to_clipboard` invisibly returns its argument. This
allows me to put it into a chain of composed functions and keep the
chain evaluating.

## Get Data from Advent of Code

I want to download the input data from Advent of Code, but I need to
pass along my session cookie. I do this by using the httr package,
storing the session cookie in an environment variable in my .Rsession
file.

``` r
get_data
```

    ## function(url, session_cookie = NULL) {
    ##   if (is.null(session_cookie)) session_cookie <- Sys.getenv('AOC_SC')
    ##   res <- httr::GET(url, httr::set_cookies(session = session_cookie))
    ##   httr::content(res, encoding = 'UTF-8')
    ## }
    ## <bytecode: 0x7fe5d8050598>
    ## <environment: namespace:advent>

# Solving the Challenges

## Challenge 1

The first challenge is to add up some masses and calculate how much fuel
it takes to power Santa’s sleigh.

> Fuel required to launch a given module is based on its mass.
> Specifically, to find the fuel required for a module, take its mass,
> divide by three, round down, and subtract 2.
> 
> For example:
> 
>   - For a mass of 12, divide by 3 and round down to get 4, then
>     subtract 2 to get 2.
>   - For a mass of 14, dividing by 3 and rounding down still yields 4,
>     so the fuel required is also 2.
>   - For a mass of 1969, the fuel required is 654.
>   - For a mass of 100756, the fuel required is 33583.
> 
> The Fuel Counter-Upper needs to know the total fuel requirement. To
> find it, individually calculate the fuel needed for the mass of each
> module (your puzzle input), then add together all the fuel values.

Clear enough I hope. My first instinct was to add up all the masses
before calculating the fuel, but the guidance explicitly says to add
first. I’ll write a function that reads the input from the web,
calculates the fuel, writes it to the clipboard, and then prints it.

``` r
fuel_of_mass <- function(mass) max(floor(mass / 3) - 2, 0)
```

Now for the provided tests.

``` r
fuel_of_mass(12) == 2
```

    ## [1] TRUE

``` r
fuel_of_mass(14) == 2
```

    ## [1] TRUE

``` r
fuel_of_mass(1969) == 654
```

    ## [1] TRUE

``` r
fuel_of_mass(100756) == 33583
```

    ## [1] TRUE

Great, all tests pass.

``` r
part_1 <- function() {
  get_data('https://adventofcode.com/2019/day/1/input') %>% 
  scan(text = ., quiet = TRUE) %>% 
  Map(f = fuel_of_mass) %>% 
  Reduce(f = `+`) %>% 
  write_to_clipboard() %>% 
  print()
}
part_1()
```

    ## [1] 3412531

Great, it works.

## Challenge 2

Part 2 involves calculating the additional fuel needed to carry the fuel
itself.

> Fuel itself requires fuel just like a module - take its mass, divide
> by three, round down, and subtract 2. However, that fuel also requires
> fuel, and that fuel requires fuel, and so on. Any mass that would
> require negative fuel should instead be treated as if it requires zero
> fuel; the remaining mass, if any, is instead handled by wishing really
> hard, which has no mass and is outside the scope of this calculation.
> 
> So, for each module mass, calculate its fuel and add it to the total.
> Then, treat the fuel amount you just calculated as the input mass and
> repeat the process, continuing until a fuel requirement is zero or
> negative. For example:
> 
>   - A module of mass 14 requires 2 fuel. This fuel requires no further
>     fuel (2 divided by 3 and rounded down is 0, which would call for a
>     negative fuel), so the total fuel required is still just 2.
>   - At first, a module of mass 1969 requires 654 fuel. Then, this fuel
>     requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more
>     fuel, which requires 21 fuel, which requires 5 fuel, which
>     requires no further fuel. So, the total fuel required for a module
>     of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
>   - The fuel required by a module of mass 100756 and its fuel is:
>     33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.

I just need to write a function like `fuel_of_mass` above which can
calculate the amount of fuel needed for fuel.

``` r
fuel_of_fuel <- function(mass_of_fuel, accumulator = 0) {
  additional_fuel <- fuel_of_mass(mass_of_fuel)
  if (additional_fuel == 0) return(accumulator)
  fuel_of_fuel(additional_fuel, accumulator + additional_fuel)
}
total_fuel <- function(mass) {
  fuel_of_mass(mass) + fuel_of_fuel(fuel_of_mass(mass))
}
```

The funciton `fuel_of_fuel` takes a mass of fuel and calculates how much
more fuel is required to propel that mass of fuel. If that quantity is
zero, then you’re done and you just return the accumulated fuel amount.
If not, repeat the process by figuring out how much more fuel you need
and add the additional fuel to the accumulated fuel amount.

Now for the provided tests.

``` r
total_fuel(14) == 2
```

    ## [1] TRUE

``` r
total_fuel(1969) == 966
```

    ## [1] TRUE

``` r
total_fuel(100756) == 50346
```

    ## [1] TRUE

Great, all tests pass.

``` r
part_2 <- function() {
  get_data('https://adventofcode.com/2019/day/1/input') %>% 
  scan(text = ., quiet = TRUE) %>% 
  Map(f = total_fuel) %>% 
  Reduce(f = `+`) %>% 
  write_to_clipboard() %>% 
  print()
}
part_2()
```

    ## [1] 5115927

Great, it works. See you later for more puzzles\! If you liked my
solutions or have constructive criticism, reach out on Twitter or e-mail
me.
