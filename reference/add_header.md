# Add header rows to a table

Inserts header rows in `to` using unique values of `from`.

## Usage

``` r
add_header(
  data,
  from,
  to,
  skip_single_row = FALSE,
  indent = "",
  drop_from = TRUE
)
```

## Examples

``` r
dplyr::starwars %>%
  head(13) %>%
  dplyr::arrange(species) %>%
  add_header(from = species, to = name, indent = "  ")
#> # A tibble: 16 × 13
#>    name     height  mass hair_color skin_color eye_color birth_year sex   gender
#>    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#>  1 "Droid"      NA    NA NA         NA         NA              NA   NA    NA    
#>  2 "  C-3P…    167    75 NA         gold       yellow         112   none  mascu…
#>  3 "  R2-D…     96    32 NA         white, bl… red             33   none  mascu…
#>  4 "  R5-D…     97    32 NA         white, red red             NA   none  mascu…
#>  5 "Human"      NA    NA NA         NA         NA              NA   NA    NA    
#>  6 "  Luke…    172    77 blond      fair       blue            19   male  mascu…
#>  7 "  Dart…    202   136 none       white      yellow          41.9 male  mascu…
#>  8 "  Leia…    150    49 brown      light      brown           19   fema… femin…
#>  9 "  Owen…    178   120 brown, gr… light      blue            52   male  mascu…
#> 10 "  Beru…    165    75 brown      light      blue            47   fema… femin…
#> 11 "  Bigg…    183    84 black      light      brown           24   male  mascu…
#> 12 "  Obi-…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
#> 13 "  Anak…    188    84 blond      fair       blue            41.9 male  mascu…
#> 14 "  Wilh…    180    NA auburn, g… fair       blue            64   male  mascu…
#> 15 "Wookie…     NA    NA NA         NA         NA              NA   NA    NA    
#> 16 "  Chew…    228   112 brown      unknown    blue           200   male  mascu…
#> # ℹ 4 more variables: homeworld <chr>, films <list>, vehicles <list>,
#> #   starships <list>
```
