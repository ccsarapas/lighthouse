# US state and territory data

`state.terr.name` and `state.terr.abb` expand the built-in `state.name`
and `state.abb` vectors by adding US territories and the District of
Columbia. This includes:

- American Samoa

- District of Columbia

- Guam

- Northern Mariana Islands

- Puerto Rico

- Virgin Islands

`state.terr.data` includes names, abbreviations, and FIPS codes for US
states and territories.

## Usage

``` r
state.terr.name

state.terr.abb

state.terr.data
```

## Format

An object of class `character` of length 56.

An object of class `character` of length 56.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 56
rows and 4 columns.

## See also

[`state.name`](https://rdrr.io/r/datasets/state.html)
