# Winsorize extreme values

Sets all values more than `max_dev` deviations from center to be
`max_dev` deviations from center. Deviations defined as standard
deviation (the default) or mean absolute deviation (if
`method = "mad"`). Center defined as mean for `method = "sd"` and median
for `method = "mad"`, unless otherwise specified in `center` argument.

## Usage

``` r
winsorize(
  x,
  max_dev = 3,
  method = c("sd", "mad"),
  mad.center = c("median", "mean")
)
```
