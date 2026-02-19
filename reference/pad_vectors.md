# Pad vectors to the same length

This function takes one or more vectors and pads them with `NA` values
to the length of the longest vector.

## Usage

``` r
pad_vectors(...)
```

## Arguments

- ...:

  One or more vectors.

## Value

A list of vectors, all having the same length as the longest input
vector.

## Examples

``` r
pad_vectors(1:3, 1:5, 1:4)
#> [[1]]
#> [1]  1  2  3 NA NA
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
#> [[3]]
#> [1]  1  2  3  4 NA
#> 

# supports list unpacking with `!!!` operator
pad_vectors(!!!list(1:3, 1:5, 1:4))
#> [[1]]
#> [1]  1  2  3 NA NA
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
#> [[3]]
#> [1]  1  2  3  4 NA
#> 


# one use case is assembling vectors of different lengths into a dataframe
# for example, to see unique column values at a glance:
unique_vals <- dplyr::starwars %>%
  dplyr::select(hair_color:eye_color) %>%
  lapply(unique)
pad_vectors(!!!unique_vals) %>%
  as.data.frame()
#>       hair_color          skin_color     eye_color
#> 1          blond                fair          blue
#> 2           <NA>                gold        yellow
#> 3           none         white, blue           red
#> 4          brown               white         brown
#> 5    brown, grey               light     blue-gray
#> 6          black          white, red         black
#> 7  auburn, white             unknown        orange
#> 8   auburn, grey               green         hazel
#> 9          white    green-tan, brown          pink
#> 10          grey                pale       unknown
#> 11        auburn               metal     red, blue
#> 12        blonde                dark          gold
#> 13          <NA>        brown mottle green, yellow
#> 14          <NA>               brown         white
#> 15          <NA>                grey          dark
#> 16          <NA>       mottled green          <NA>
#> 17          <NA>              orange          <NA>
#> 18          <NA>          blue, grey          <NA>
#> 19          <NA>           grey, red          <NA>
#> 20          <NA>                 red          <NA>
#> 21          <NA>                blue          <NA>
#> 22          <NA>          grey, blue          <NA>
#> 23          <NA> grey, green, yellow          <NA>
#> 24          <NA>              yellow          <NA>
#> 25          <NA>                 tan          <NA>
#> 26          <NA> fair, green, yellow          <NA>
#> 27          <NA>         silver, red          <NA>
#> 28          <NA>         green, grey          <NA>
#> 29          <NA>    red, blue, white          <NA>
#> 30          <NA>        brown, white          <NA>
#> 31          <NA>                none          <NA>
```
