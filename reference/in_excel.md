# Open dataframe in Excel

Saves dataframe as .csv in R temp directory, then opens in Excel. The
.csv will have a randomly-generated name unless otherwise specified in
`name`.

## Usage

``` r
in_excel(df, name = NULL, na = "")
```

## Arguments

- df:

  a data frame or data frame extension (e.g., a tibble).

- name:

  a string: the name to use for the .csv file. If `NULL`, a random name
  will be generated.

- na:

  string to use for missing values in the .csv file.
