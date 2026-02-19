# Translate colors before and after alpha blending

These functions translate colors between their original RGB values and
their RGB values after alpha blending with a background color.
`before_opacity` calculates the original color given the blended color,
while `after_opacity` calculates the blended color given the original
color.

## Usage

``` r
after_opacity(color, alpha, bg = "white")

before_opacity(color, alpha, bg = "white")
```

## Arguments

- color:

  The starting color as a color name, hex code, or RGB triplet.

- alpha:

  The opacity of the foreground color, a number between 0 and 1.

- bg:

  The background color for blending, as a color name, hex code, or RGB
  triplet. Defaults to "white".

## Value

- `before_opacity`: The original color before alpha blending, as a hex
  code.

- `after_opacity`: The blended color after alpha blending, as a hex
  code.

## Examples

``` r
red <- "red"
red_50 <- after_opacity(red, 0.5)
red_back <- before_opacity(red_50, 0.5)
scales::show_col(c(red, red_50, red_back), ncol = 3)


color_blends <- sapply(
  c("red", "blue", "yellow", "white", "black", "gray50"),
  after_opacity,
  color = "red",
  alpha = 0.5
)
scales::show_col(color_blends)

```
