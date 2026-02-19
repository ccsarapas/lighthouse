# Detect the presence of any pattern in a string

`str_detect_any()` returns TRUE if any element of `patterns` is present
in `string`. `str_starts_any()` and `str_ends_any()` match only at the
beginning or end of strings.

## Usage

``` r
str_detect_any(
  string,
  patterns,
  whole_word = FALSE,
  ignore_case = FALSE,
  negate = FALSE
)

str_starts_any(
  string,
  patterns,
  whole_word = FALSE,
  ignore_case = FALSE,
  negate = FALSE
)

str_ends_any(
  string,
  patterns,
  whole_word = FALSE,
  ignore_case = FALSE,
  negate = FALSE
)
```

## Arguments

- string:

  a character vector or something coercible to one.

- patterns:

  a character vector containing regular expressions to look for.

- whole_word:

  logical. Match only whole words in `string` (as defined by `"\\b"`
  word boundary)?

- ignore_case:

  logical. Ignore case when matching `patterns` to `string`?

- negate:

  Logical. if `TRUE`, inverts the resulting boolean vector.

## See also

[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
[`stringr::str_starts()`](https://stringr.tidyverse.org/reference/str_starts.html)
