# Add crossings to a dataframe for area charts

Augments a dataframe with x-values where y = f(x) = 0. This is useful
for creating area charts with different fills for values less than
versus greater than 0.

## Usage

``` r
add_crossings(data, x, y, .by = NULL)
```

## Arguments

- data:

  A data frame containing the original x and y values.

- x:

  x-axis values.

- y:

  y-axis values.

- .by:

  Grouping variable(s). Useful for computing crossings for faceted
  plots.

## Value

The input data frame with additional rows representing the crossings
(where y = 0), and two new columns:

- `pos_neg`: Indicates whether the y-value is positive ("pos") or
  negative ("neg").

- `cross_grp`: A grouping variable for the segments between crossings.

## Details

The returned dataframe will include columns `pos_neg` and `cross_group`.
Within `geom_area()`, `cross_group` should be mapped to `group`, while
`pos_neg` should be mapped to aesthetics such as `fill` or `color`.

## Examples

``` r
nile_flow <- tibble::tibble(
  Year = time(Nile),
  Flow = as.numeric(Nile),
  FlowDelta = (Flow - Flow[[1]]) / Flow[[1]]
)
nile_flow_x0 <- add_crossings(nile_flow, Year, FlowDelta)
#> Error in dplyr::mutate(., xzero = -(({    {        y    }} * (dplyr::lead({    {        x    }}) - {    {        x    }}))/(dplyr::lead({    {        y    }}) - {    {        y    }})) + {    {        x    }}, `:=`({    {        x    }}, dplyr::if_else(xzero >= {    {        x    }} & xzero < dplyr::lead({    {        x    }}), xzero, NA)), `:=`({    {        y    }}, dplyr::if_else(is.na({    {        x    }}), NA, 0)), xzero = NULL, .by = {    {        .by    }}): ℹ In argument: `Year = dplyr::if_else(...)`.
#> Caused by error in `dplyr::if_else()`:
#> ! `condition` must be a logical vector, not a <ts> object.
ggplot2::ggplot(nile_flow_x0, ggplot2::aes(Year, FlowDelta)) +
  ggplot2::geom_area(
    ggplot2::aes(group = cross_grp, color = pos_neg, fill = pos_neg),
    alpha = 0.25,
    show.legend = FALSE
  ) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
  ggplot2::scale_color_manual(
    values = c("darkred", "blue"),
    aesthetics = c("color", "fill")
  ) +
  ggplot2::scale_y_continuous(
    "Nile River Annual Flow:\n% Change from 1871",
    labels = scales::percent
  ) +
  ggplot2::theme_minimal()
#> Error: object 'nile_flow_x0' not found
```
