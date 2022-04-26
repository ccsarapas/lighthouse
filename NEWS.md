# lighthouse 0.6.0

* New functions:
    * Grouping and summary functions: 
        * `group_with_total()`
        * `count_multiple()`
        * `count_unique()`
        * `count_duplicates()`
        * `cols_info()`
    * Statistical and math functions: 
        * `wkappa()`
        * `cohen_w()`
        * `median_dbl()`
        * `safe_min()`, `safe_max()` 
        * `pmin_across()`, `pmax_across()`
        * `cumsum_desc()`
        * `scale_vec()`
        * `reverse_key()`
    * Data restructuring: 
        * `add_header()`
        * `t_tibble()`
        * `rev_rows()`
        * `fct_reorder_n()`
    * For working with missing values:
        * `find_na_cols()`, `drop_na_cols()`
        * `n_valid()`, `pct_valid()`, `n_pct_valid()`
        * `discard_na()`
        * `null_to_na()`
        * `is_valid()`
    * For working with strings: 
        * `str_prefix()`, `str_suffix()`
        * `glue_chr()`
    * For working with dates: 
        * `datetimes_to_date()`
        * `next_bizday()`
    * Logical tests: 
        * `is_duplicate()`
        * `is_spss()`
        * `is_coercible_integer()`, `is_coercible_logical()`
    * Other: 
        * `gain_ss_score()`

* Added datasets for federal (`holidays_us`), Illinois (`holidays_il`), and 
Chestnut Health Systems (holidays_chestnut`) holidays (primarily for use with 
`next_bizday()` function).

* Added `strict` argument to `is_TRUE()`, `is_FALSE()`, `is_TRUE_or_NA()`, and
`is_FALSE_or_NA()`

* Improvements to `set_ggplot_opts()`, `ggview()`, and `is_coercible_numeric()`

* Bugfixes for `in_excel()`, `count_na()`, `summary_table()`, 
`pivot_wider_alt()`, `print_all()`, `asterisks()`, and `coerce_na_range()`

* Remove check for `lighthouse` updates on load

# lighthouse 0.5.0

* Check if `lighthouse` update is available on load

* New infix operators: `%all_in%`, `%any_in%`

* Exported `na_like()`

# lighthouse 0.4.1

* Bugfix for `print_all()`

# lighthouse 0.4.0

* New logical tests: `is_TRUE()`, `is_FALSE()`, `is_TRUE_or_NA()`, 
`is_FALSE_or_NA()`, `is_coercible_numeric()`

* New count functions: `crosstab()`, `count_na()`

* New data transformations: `scale_mad()`, `winsorize()`

* New date functions: `floor_month()`, `floor_week()`, `floor_days()`, 
`days_diff()`

* Other new functions: `asterisks()`, `print_n()`, `print_all()`, 
`na_to_null()`, `set_ggplot_opts()`

* Added added optional `name` argument to `in_excel()`
