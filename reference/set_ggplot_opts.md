# Nicer default theme and palettes for `ggplot2`

Changes default theme and color scales for `ggplot2`.

## Usage

``` r
set_ggplot_opts(base_theme = NULL, brewer_pal_discrete = "Set1", ...)
```

## Details

Theme is based on
[`hrbrthemes::theme_ipsum_rc()`](https://rdrr.io/pkg/hrbrthemes/man/theme_ipsum_rc.html),
unless otherwise specified in `base_theme` argument. The theme is
further modified as follows:

- Axis titles are centered

- Legend title is omitted

- Minor gridlines are omitted

- Facet labels are placed outside axes

- Various tweaks to text size and margins

Default color and fill palettes are set based on scale type:

- For discrete scales, `RColorBrewer` palette "Set1," unless otherwise
  specified in `brewer_pal_discrete` argument

- For continuous and binned scales, `RColorBrewer` palette "Blues"

- For ordinal scales, `viridisLite` palette "viridis"

Default font family for
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
and
[`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
is set to match `base_theme`.
