# Collapse factor levels with additional controls

Collapses factor levels into manually defined groups like
[`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html),
with additional options to control behavior when specified levels don't
exist in the data and to order factor levels in the order listed.

## Usage

``` r
fct_collapse_alt(
  .f,
  ...,
  other_level = NULL,
  reorder = TRUE,
  unknown_levels = c("ignore", "warn", "error")
)
```

## Arguments

- .f:

  A factor (or character vector).

- ...:

  A series of named character vectors. The levels in each vector will be
  replaced with the name.

- other_level:

  Value of level used for "other" values not named in `...`. If `NULL`,
  no extra level is created.

- reorder:

  If `TRUE`, the collapsed levels are ordered in the order they are
  listed in `...`, followed by `other_level` if specified, and then
  existing levels.

- unknown_levels:

  How to handle levels listed in `...` that aren't present in the input
  factor `.f`. Options are:

  - "ignore": ignore unknown levels without a warning (default),

  - "warn": issue a warning and then ignore unknown levels,

  - "error": raise an error.

## Value

A factor with collapsed levels.

## Examples

``` r
f <- factor(c("a", "b", "c", "d", "e", "f"))

fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"))
#> [1] AB  AB  c   d   EFG EFG
#> Levels: EFG AB c d
fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), reorder = FALSE)
#> [1] AB  AB  c   d   EFG EFG
#> Levels: AB c d EFG
fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), other_level = "other")
#> [1] AB    AB    other other EFG   EFG  
#> Levels: EFG AB other
# `unknown_levels = "warn"` mirrors behavior of `forcats::fct_collapse()`
# \donttest{
fct_collapse_alt(f, EFG = c("e", "f", "g"), AB = c("a", "b"), unknown_levels = "warn")
#> Warning: Unknown levels in `f`: g
#> [1] AB  AB  c   d   EFG EFG
#> Levels: EFG AB c d
# }
```
