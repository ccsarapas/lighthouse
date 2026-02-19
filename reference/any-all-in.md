# Test whether multiple values are in a vector

These infix operators test whether any or all left-hand side elements
occur in the right-hand side.

## Usage

``` r
lhs %all_in% rhs

lhs %any_in% rhs
```

## Details

`%all_in%` returns `TRUE` if all elements of left operand (`lhs`) are
found in right operand (`rhs`). Equivalent to `all(lhs %in% rhs)`.

`%any_in%` returns `TRUE` if any elements of left operand (`lhs`) are
found in right operand (`rhs`). Equivalent to `any(lhs %in% rhs)`.

## Examples

``` r
maybe_states <- c("Idaho", "Illinois", "North Tuba", "Maine")
maybe_states %any_in% state.name  # TRUE
#> [1] TRUE
maybe_states %all_in% state.name  # FALSE
#> [1] FALSE
rm(maybe_states)
```
