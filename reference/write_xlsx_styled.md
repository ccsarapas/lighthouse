# Write a styled data frame to an Excel file

A wrapper around
[`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html)
with default styling options including frozen, bolded row headings and
auto column widths.

## Usage

``` r
write_xlsx_styled(x, file, asTable = FALSE, overwrite = TRUE, ...)
```

## Arguments

- x:

  The data frame to write.

- file:

  The path to the output Excel file.

- asTable:

  Whether to write as an Excel table.

- overwrite:

  Whether to overwrite an existing file.

- ...:

  Additional arguments passed to
  [`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html).
