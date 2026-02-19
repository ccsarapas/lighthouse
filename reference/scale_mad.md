# Scale based on median absolute deviation

Scales a vector of values based on the median absolute deviation. Values
may be centered around the median (default), mean, or not centered.
Compare to [`base::scale()`](https://rdrr.io/r/base/scale.html), which
uses standard deviation and centers around the mean by default.

## Usage

``` r
scale_mad(x, center = c("median", "mean", "none"), mad_constant = 1.4826)
```
