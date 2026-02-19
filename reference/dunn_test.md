# Pairwise post-hoc test following Kruskal-Wallis test

This is a tidy wrapper around
[`dunn.test::dunn.test()`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html).
It performs Dunn's test, a non-parametric pairwise follow-up for a
Kruskal-Wallis test.

## Usage

``` r
dunn_test(
  x,
  groups,
  data,
  p.adjust.method = c("holm", "hochberg", "bonferroni", "bh", "by", "sidak", "hs",
    "none"),
  alpha = 0.05
)
```

## Arguments

- x:

  a numeric vector.

- groups:

  a vector or factor giving the group for the corresponding elements of
  `x`.

- data:

  a data frame containing the variables.

- p.adjust.method:

  character. method for adjusting p-values for multiple comparisons.

- alpha:

  numeric. alpha level.

## Value

A tibble with columns:

- `contrast`: The compared groups.

- `statistic`: Dunn's test statistic (z).

- `adj.p.value`: Adjusted p-value based on the specified
  `p.adjust.method`.

## See also

[`dunn.test::dunn.test()`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html)

## Examples

``` r
mtcars2 <- transform(mtcars, cyl = factor(cyl))
kruskal.test(mpg ~ gear, data = mtcars2)
#> 
#>  Kruskal-Wallis rank sum test
#> 
#> data:  mpg by gear
#> Kruskal-Wallis chi-squared = 14.323, df = 2, p-value = 0.0007758
#> 
dunn_test(mpg, gear, data = mtcars2)
#> 
#>                    Dunn's Pairwise Comparison of x by groups                  
#>                                     (Holm)                                    
#> Col Mean-│
#> Row Mean │          3          4
#> ─────────┼──────────────────────
#>        4 │  -3.761746
#>          │     0.0003*
#>          │
#>        5 │  -1.645620   1.140586
#>          │     0.0998     0.1270 
#> 
#> FWER = 0.05
#> Reject Ho if p ≤ FWER/2 with stopping rule, where p = Pr(Z ≥ |z|)
#> # A tibble: 3 × 3
#>   contrast statistic adj.p.value
#>   <chr>        <dbl>       <dbl>
#> 1 3 - 4        -3.76    0.000253
#> 2 3 - 5        -1.65    0.0998  
#> 3 4 - 5         1.14    0.127   
```
