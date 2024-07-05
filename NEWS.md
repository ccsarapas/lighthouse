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
      *** `row_sums_across()` is a variant of `base::rowSums()` that accepts tidyselect expressions and has alternative `NA` handling.
      *** `se_mean()` and `se_prop()` compute standard error of the mean and of a proportion, respectively. `se_mean()` supersedes `se()`.
    * Summary functions:
        * `df_compare()` is a utility for identifying differences between data frames. Given two data frames, it returns only rows and columns with differences.
        * `summary_report()` returns a summary of multiple variables, summarizing each variable based on its level of measurement.
    * Datasets:
        *
    * Other:
        * `eq_shape()`
        * `open_file()`, `open_location()`
        * `set_compare()`
        * `suppress_messages_if()`
        * `suppress_warnings_if()`
      *** `syms_to_chr()`
      
      
      
* New functions:
    * Data visualization:
        * add_crossings()
        * after_opacity(), before_opacity()
    * Exporting results:
        * add_plot_slide()
        * write_xlsx_styled()
    * Data restructuring:
        * add_rows_at_value()
        * pad_vectors()
    * Variable transformation:
        * fct_collapse_alt()
        * fct_na_if()
    * For working with missing values:
        * drop_na_rows()
        * first_valid(), last_valid(), nth_valid()
        * %in_na%()
      *** na_if_range()
    * For working with strings:
        * str_c_narm()
        * str_c_tidy()
        * str_ends_any()
    * For working with dates:
        * fiscal_year(), ffy(), sfy_il()
      *** nth_bizday()
        * strftime_no_lead()
    * For working with service cascades:
        * cascade_fill_bwd(), cascade_fill_fwd()
        * cascade_summarize()
    * Statistical and math functions:
        * ci_sig()
        * dunn_test()
      *** min_if_any(), max_if_any(), sum_if_any()
        * OR_to_p1(), OR_to_p2()
      *** row_sums_across()
      *** se_mean(), se_prop()
    * Summary functions:
        * df_compare()
        * summary_report()
    * Datasets:
        *
    * Other:
        * eq_shape()
        * open_file(), open_location()
        * set_compare()
        * suppress_messages_if()
        * suppress_warnings_if()
      *** syms_to_chr()
* Added state / territory data
* Added GAIN missing codes
* Changes to holiday datasets:
    * Added General Election Day to `holidays_il` and arranged by date (#1)
    * Removed Inauguration Day from `holidays_us`
* Added `warn_factor` argument to `try_numeric()`
* `count_duplicates()` now returns both the unique and total number of duplicated values. (e.g., `c(2, 2, 4, 4)` has two unique and four total values.)
* `.by` argument for some count functions
* `days_diff()` now handles inputs of different types (e.g., a date and a datetime) with a warning.
* Added `nth_bizday()`, which `next_bizday()` now calls.
* `row_sums_spss()` deprecated in favor of `row_sums_across()`.
* `safe_min()` and `safe_max()` renamed to `min_if_any()` and `max_if_any()`; added `sum_if_any()`.
* `na_if_range()` - renamed from `coerce_na_range()`; methods added for numeric, labelled, and haven_labelled inputs; fixed bug due to non-exported methods. fixes #5.
* `asterisks()` - changed default for `include_key` from `TRUE` to `FALSE`.
* `se_mean()` and `se_prop()` as more specific replacements for `se()`. `se()` is deprecated and calls `se_mean()` with a warning.
* `percent()` and `comma()` re-exported from scales (#11)
* `summary_table()`:
   - Bug fix for #6 - `.cols_group_by` arg works again!
   - Column of variable names can be dropped when only one variable is included by setting (#9) `.var_col_name = NULL`
   - Now uses `tidyr::pivot_wider()` instead of `lighthouse::pivot_wider_alt()`.
* `add_rows_at_value()` added -- note change from pre-release in how values are passed.
    - `syms_to_chr()` as helper to adapt old code
** look through and see if anything can be better classified:
    - "misc"
    - "display"
    - "transform_filter"
    - "output" (currently empty)
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
