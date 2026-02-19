# Match values not in vector

Infix operator returning `TRUE` for elements of left operand (`lhs`) not
found in right operand (`rhs`). Equivalent to `!(lhs %in% rhs)`.

## Usage

``` r
lhs %!in% rhs
```

## Examples

``` r
"April" %!in% month.name
#> [1] FALSE
"Junvember" %!in% month.name
#> [1] TRUE

some_letters <- sample(letters, 10)
letters[letters %in% some_letters]
#>  [1] "a" "h" "i" "m" "o" "p" "r" "u" "x" "y"
letters[letters %!in% some_letters]
#>  [1] "b" "c" "d" "e" "f" "g" "j" "k" "l" "n" "q" "s" "t" "v" "w" "z"
```
