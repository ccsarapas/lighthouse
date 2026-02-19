# Convert between probabilities and odds ratios

These functions convert between probabilities and odds ratios. `p_to_OR`
calculates the odds ratio given two probabilities, while `OR_to_p2` and
`OR_to_p1` calculate the second and first probability, respectively,
given the other probability and the odds ratio.

## Usage

``` r
p_to_OR(p1, p2)

OR_to_p2(p1, OR)

OR_to_p1(p2, OR)
```

## Arguments

- p1:

  The first probability.

- p2:

  The second probability.

- OR:

  The odds ratio.

## Value

- `p_to_OR`: The odds ratio corresponding to the given probabilities.

- `OR_to_p2`: The second probability corresponding to the given first
  probability and odds ratio.

- `OR_to_p1`: The first probability corresponding to the given second
  probability and odds ratio.

## Examples

``` r
p_to_OR(0.4, 0.6)
#> [1] 2.25
OR_to_p2(0.4, 2.25)
#> [1] 0.6
OR_to_p1(0.6, 2.25)
#> [1] 0.4
```
