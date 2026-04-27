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
#>  [1] "c" "d" "e" "k" "n" "o" "q" "t" "v" "y"
letters[letters %!in% some_letters]
#>  [1] "a" "b" "f" "g" "h" "i" "j" "l" "m" "p" "r" "s" "u" "w" "x" "z"
```
