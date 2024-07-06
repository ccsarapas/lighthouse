# lighthouse 0.7.0
## New functions:
* Data visualization:
    * `add_crossings()` is a helper for creating area charts with different fills for positive vs negative values.
    * `after_opacity()` and `before_opacity()` are utilities for color blending.
* Exporting results:
    * `add_plot_slide()` is a helper for exporting plots to PowerPoint with easier control of size and positioning.
    * `write_xlsx_styled()` writes to xlsx with basic column formatting.
* Data restructuring:
    * `add_rows_at_value()` is similar to `add_blank_rows()`, but allows specifying position by column values rather than row numbers. Note there have been some changes in the function interface from the pre-release version; see the "Details" section of the documentation.
    * `pad_vectors()` pads a list of vectors with `NA`s to a common length.
* Variable transformation:
    * `fct_collapse_alt()` is a variant of `forcats::fct_collapse()` with options to handle non-existent values and level ordering.
    * `fct_na_if()` is a variant of `dplyr::na_if()` that also removes the specified value from a factor's levels.
* For working with missing values:
    * `drop_na_rows()` drops rows where all columns or a specific subset of columns are all `NA`.
    * `first_valid()`, `last_valid()`, `nth_valid()` return the nth non-missing value in a vector.
    * `na_if_range()` is a renamed, expanded, and bug-fixed version of `coerce_na_range()`. `coerce_na_range()` is retained as an alias for back compatibility.
* For working with strings:
    * `str_c_narm()` is a variant of `stringr::str_c()` with alternative handling of `NA`s.
    * `str_c_tidy()` is a variant of `stringr::str_c()` that accepts tidyselect expressions.
    * `str_ends_any()` was added to complement `str_starts_any()` and `str_detect_any()`.
* For working with dates:
    * `ffy()` and `sfy_il()` return the federal fiscal year or Illinois state fiscal year for a given date. They wrap `fiscal_year()`, which returns  the fiscal year based on a specified starting month.
    * `nth_bizday()` is a generalization of `next_bizday()`.
    * `strftime_no_lead()` formats a date without leading zeroes (e.g., "6/7/2024" instead of "06/07/2024").
* For working with service cascades:
    * `cascade_fill_bwd()` and `cascade_fill_fwd()` impute values into service cascade data based on previous or subsequent cascade steps.
    * `cascade_summarize()` returns a summary table for service cascade data.
    * These functions are not yet fully documented.
* Statistical and math functions:
    * `ci_sig()` tests if a confidence interval indicates statistical significance.
    * `dunn_test()` performs Dunn's test, a pairwise post-hoc test for following up a Kruskal-Wallis test.
    * `sum_if_any()`, `min_if_any()`, and `max_if_any()` are variants of `sum()`, `min()`, and `max()` that remove `NA`s unless all values are `NA`. `min_if_any()` and `max_if_any()` were renamed from `safe_min()` and `safe_max()`.
    * `OR_to_p1()` and `OR_to_p2()` convert odds ratios to probabilities. 
    * `row_sums_across()` is a variant of `base::rowSums()` that accepts tidyselect expressions and has alternative `NA` handling.
    * `se_mean()` and `se_prop()` compute standard error of the mean and of a proportion, respectively. `se_mean()` supersedes `se()`.
* Summary functions:
    * `df_compare()` is a utility for identifying differences between data frames. Given two data frames, it returns only rows and columns with differences.
    * `summary_report()` returns a summary of multiple variables, summarizing each variable based on its level of measurement.
* Datasets:
    * `gain_missing_codes` is a quick reference for missing value labels used in GAIN datasets.
    * `state.terr.name` and `state.terr.abb` are versions of `state.name` and `state.abb` that include US territories and the District of Columbia. `state.terr.data` includes names, abbreviations, and FIPS codes for US states, territories, and the District of Columbia.
* Other:
    * `eq_shape()` checks if two objects have the same number of dimensions and same length along each dimension.
    * `open_file()` (alias `file.open()`) opens a file with its default application. `open_folder()` (alias `dir.open()`) opens a folder in the system file manager.
    * Given two vectors, `set_compare()` returns labelled subsets of unique and shared elements.
    * `suppress_warnings_if()` and `suppress_messages_if()` conditionally suppress warnings or messages based on their text.
      
## Added functionality:
* `count_pct()` and `count_multiple()` now support the `.by` argument for per-operation grouping. Integration into other `count_*()` functions is planned for a future update.
* In `summary_table()`, the column of variable names can be dropped when only one variable is included by setting `.var_col_name = NULL` (#9). 
* `count_duplicates()` now returns both the unique and total number of duplicated values. (e.g., `c(2, 2, 4, 4)` has two unique and four total values.)
* Added `warn_factor` argument to `try_numeric()`

## Bug fixes:
* The `.cols_group_by` argument in `summary_table()` no longer throws an error (fixes #6).
* `count_with_total()` no longer throws an error when the column to be totaled isn't a character vector or factor (fixes #10).
* `days_diff()` now handles inputs of different types (e.g., a date and a datetime) with a warning.
* Added General Election Day to `holidays_il` and arranged by date (fixes #1).
* Removed Inauguration Day from `holidays_us`.

## Other changes:
* `asterisks()` - changed default for `include_key` from `TRUE` to `FALSE`.
* `percent()` and `comma()` re-exported from scales (#11)

## Lifecycle changes:
* `rbool()` has been *un*-deprecated. It was previously deprecated in favor of `purrr::rbernoulli()`, but `purrr::rbernoulli()` has since been deprecated itself.
* `pivot_wider_alt()` is defunct. Changes to `tidyr::pivot_wider()` made its most important functionality unnecessary. Further changes to tidyr broke it, and it was judged not worth the effort of fixing.
* `na_like()` and `median_dbl()` are deprecated. They are no longer needed given more flexible handling of mixed classes by `dplyr::if_else()` and `dplyr::case_when()` as of [dplyr v1.1.0][https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0}]. (Plus `na_like()` was quite buggy and unreliable; resolves #2).
* `row_sums_spss()` deprecated in favor of `row_sums_across()`.
* `safe_min()` and `safe_max()` renamed to `min_if_any()` and `max_if_any()`; the old names are deprecated.
* `se()` renamed to `se_mean()`; the old name is deprecated.

    
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