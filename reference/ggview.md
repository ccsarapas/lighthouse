# Nicer ggplot rendering - DEPRECATED

This function has been deprecated and is no longer maintained.
Improvements to RStudio graphics make it no longer needed.

Saves ggplot object as .svg in R temp directory, then displays in
RStudio Viewer pane. Results in better image quality on Windows
machines.

## Usage

``` r
ggview(
  plot = ggplot2::last_plot(),
  width = NULL,
  height = NULL,
  type = c("svg", "png")
)
```
