Day 1
================
john
12/8/2019

``` r
library(advent)
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
    ## <bytecode: 0x7fdd3fa7fbd8>
    ## <environment: namespace:advent>

``` r
is_windows
```

    ## function() {
    ##   .Platform$OS.type == 'windows'
    ## }
    ## <bytecode: 0x7fdd3fc08378>
    ## <environment: namespace:advent>

``` r
write_to_clipboard
```

    ## function(data) {
    ##   if (is_mac()) cat(data, file = pipe('pbcopy'))
    ##   if (is_windows()) writeClipboard(as.character(data))
    ##   invisible(data)
    ## }
    ## <bytecode: 0x7fdd3fe5c950>
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
    ##   httr::content(res)
    ## }
    ## <bytecode: 0x7fdd0f9d0f68>
    ## <environment: namespace:advent>
