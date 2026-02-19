# Changelog

## lighthouse 0.7.4

- Removed unused `...` argument from
  [`n_valid()`](https://ccsarapas.github.io/lighthouse/reference/n_valid.md),
  `n_pct()`, and
  [`n_pct_valid()`](https://ccsarapas.github.io/lighthouse/reference/n_valid.md).

- Minor documentation tweaks.

- Copyright to Chestnut Health Systems.

## lighthouse 0.7.3

### New functions

- [`ffyq()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  and
  [`sfyq_il()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  return the federal fiscal year and quarter or Illinois state fiscal
  year and quarter for a given date. Return format can be set using the
  `type` parameter, defaulting to numeric `YYYY.Q` format. These
  functions wrap
  [`lubridate::quarter()`](https://lubridate.tidyverse.org/reference/quarter.html)
  and complement the existing lighthouse functions
  [`ffy()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  and
  [`sfy_il()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md).

### New functionality

- [`summary_table()`](https://ccsarapas.github.io/lighthouse/reference/summary_table.md)
  has new arguments `.cols_group_glue` and `.cols_group_order` to
  control column names and order when `.cols_group_by` is set. These are
  replacements for `.cols_group_opts`, which is deprecated and will be
  removed in a future release.

### Bug fixes

- [`fiscal_year()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md),
  [`ffy()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md),
  and
  [`sfy_il()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  are now vectorized (fixes
  [\#23](https://github.com/ccsarapas/lighthouse/issues/23)).

- [`summary_table()`](https://ccsarapas.github.io/lighthouse/reference/summary_table.md)
  now accepts functions that do not have an `na.rm` or `...` argument,
  which previously caused an error.

- [`strftime_no_lead()`](https://ccsarapas.github.io/lighthouse/reference/strftime_no_lead.md):

  - now returns results with default formatting when `format` is
    unspecified (fixes
    [\#21](https://github.com/ccsarapas/lighthouse/issues/21)).
  - now supports the `"%OSn"` conversion specification. This returns
    seconds with specified number of decimal places, up to 6; e.g.,
    `"%OS3"` would return seconds with 3 decimals places.

- [`summary_report()`](https://ccsarapas.github.io/lighthouse/reference/summary_report.md):

  - no longer issues a deprecation warning related to using `!!!` on a
    single language object (fixes
    [\#19](https://github.com/ccsarapas/lighthouse/issues/19)).
  - now returns consistent column types in output tibble (fixes \# 26).
  - setting `.missing_label` no longer throws errors in some situations
    (fixes [\#26](https://github.com/ccsarapas/lighthouse/issues/26)).
  - now treats dates and datetimes as nominal by default, and will error
    on attempts to treat dates or datetimes as continuous or binary.

## lighthouse 0.7.2

### Bug fixes

- [`open_file()`](https://ccsarapas.github.io/lighthouse/reference/open_file.md),
  [`open_location()`](https://ccsarapas.github.io/lighthouse/reference/open_file.md),
  and
  [`in_excel()`](https://ccsarapas.github.io/lighthouse/reference/in_excel.md)
  now work on MacOS (fixes
  [\#17](https://github.com/ccsarapas/lighthouse/issues/17)).

### Changes to row-wise aggregation functions

- Added
  [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
  and
  [`pmean()`](https://ccsarapas.github.io/lighthouse/reference/psum.md):

  - These compute “parallel” or row-wise sums or means, analogous to
    [`base::pmax()`](https://rdrr.io/r/base/Extremes.html) and
    [`base::pmin()`](https://rdrr.io/r/base/Extremes.html).

  - [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
    deprecates
    [`row_sums_spss()`](https://ccsarapas.github.io/lighthouse/reference/row_sums_spss.md).
    ([`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
    is a clearer and more consistent name, as its behavior is closer to
    that of [`pmin()`](https://rdrr.io/r/base/Extremes.html) /
    [`pmax()`](https://rdrr.io/r/base/Extremes.html) than
    [`rowSums()`](https://rdrr.io/r/base/colSums.html).) Note that
    [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
    has `na.rm = FALSE` by default whereas
    [`row_sums_spss()`](https://ccsarapas.github.io/lighthouse/reference/row_sums_spss.md)
    defaulted to `na.rm = TRUE`.

- Added
  [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
  and
  [`pmean_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md):

  - These are implementations of
    [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
    and
    [`pmean()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
    that take tidyselect expressions, complementing
    [`pmin_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
    and
    [`pmax_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md).

  - [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
    replaces `row_sums_across()`, which was introduced in 0.7.0 but is
    now removed (closes
    [\#16](https://github.com/ccsarapas/lighthouse/issues/16)).

- All `p*_across()` functions now accept tidyselect expressions via
  `...` rather than `cols`. This makes it easier to include multiple
  tidyselect expressions, e.g.,
  `psum_across(var1:var9, starts_with("An"))`.

- Updated documentation for
  [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md),
  [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md),
  and friends. In particular, see the Details section of
  [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md),
  which contrasts use cases for
  [`psum()`](https://ccsarapas.github.io/lighthouse/reference/psum.md)
  vs. [`psum_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md).

### Lifecycle changes

- [`coerce_na_range()`](https://ccsarapas.github.io/lighthouse/reference/coerce_na_range.md)
  is deprecated in favor of
  [`na_if_range()`](https://ccsarapas.github.io/lighthouse/reference/na_if_range.md).

### Other changes

- Added a hex logo!

## lighthouse 0.7.1

### Bug fix

- [`strftime_no_lead()`](https://ccsarapas.github.io/lighthouse/reference/strftime_no_lead.md)
  now removes leading zeroes only from specified components of
  date-times (fixes
  [\#14](https://github.com/ccsarapas/lighthouse/issues/14)).

## lighthouse 0.7.0

### New functions

#### Summary functions

- [`summary_report()`](https://ccsarapas.github.io/lighthouse/reference/summary_report.md)
  returns a summary of multiple variables, summarizing each variable
  based on its level of measurement.
- [`df_compare()`](https://ccsarapas.github.io/lighthouse/reference/df_compare.md)
  is a utility for identifying differences between data frames. Given
  two data frames, it returns only rows and columns with differences.

#### Tools for missing values

- [`na_if_range()`](https://ccsarapas.github.io/lighthouse/reference/na_if_range.md)
  is a renamed, expanded, and bug-fixed version of
  [`coerce_na_range()`](https://ccsarapas.github.io/lighthouse/reference/coerce_na_range.md).
  [`coerce_na_range()`](https://ccsarapas.github.io/lighthouse/reference/coerce_na_range.md)
  is retained as an alias for back compatibility.
- [`drop_na_rows()`](https://ccsarapas.github.io/lighthouse/reference/drop_na_rows.md)
  drops rows where all columns or a specific subset of columns are all
  `NA`.
- [`first_valid()`](https://ccsarapas.github.io/lighthouse/reference/nth_valid.md),
  [`last_valid()`](https://ccsarapas.github.io/lighthouse/reference/nth_valid.md),
  [`nth_valid()`](https://ccsarapas.github.io/lighthouse/reference/nth_valid.md)
  return the nth non-missing value in a vector.

#### Tools for character vectors

- [`str_c_narm()`](https://ccsarapas.github.io/lighthouse/reference/str_c_narm.md)
  is a variant of
  [`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html)
  with alternative handling of `NA`s.
- [`str_c_tidy()`](https://ccsarapas.github.io/lighthouse/reference/str_c_tidy.md)
  is a variant of
  [`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html)
  that accepts tidyselect expressions.
- [`str_ends_any()`](https://ccsarapas.github.io/lighthouse/reference/str_detect_any.md)
  was added to complement
  [`str_starts_any()`](https://ccsarapas.github.io/lighthouse/reference/str_detect_any.md)
  and
  [`str_detect_any()`](https://ccsarapas.github.io/lighthouse/reference/str_detect_any.md).

#### Tools for dates

- [`ffy()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  and
  [`sfy_il()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md)
  return the federal fiscal year or Illinois state fiscal year for a
  given date. They wrap
  [`fiscal_year()`](https://ccsarapas.github.io/lighthouse/reference/fiscal_year.md),
  which returns the fiscal year based on a specified starting month.
- [`strftime_no_lead()`](https://ccsarapas.github.io/lighthouse/reference/strftime_no_lead.md)
  formats a date without leading zeroes (e.g., “6/7/2024” instead of
  “06/07/2024”).
- [`nth_bizday()`](https://ccsarapas.github.io/lighthouse/reference/bizday.md)
  is a generalization of
  [`next_bizday()`](https://ccsarapas.github.io/lighthouse/reference/bizday.md).

#### Tools for service cascades

- [`cascade_fill_bwd()`](https://ccsarapas.github.io/lighthouse/reference/cascade.md)
  and
  [`cascade_fill_fwd()`](https://ccsarapas.github.io/lighthouse/reference/cascade.md)
  impute values into service cascade data based on previous or
  subsequent cascade steps.
- [`cascade_summarize()`](https://ccsarapas.github.io/lighthouse/reference/cascade.md)
  returns a summary table for service cascade data.
- These functions are not yet fully documented.

#### Statistical functions

- [`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md)
  and
  [`se_prop()`](https://ccsarapas.github.io/lighthouse/reference/se_prop.md)
  compute standard error of the mean and of a proportion, respectively.
  [`se_prop()`](https://ccsarapas.github.io/lighthouse/reference/se_prop.md)
  includes checks for unreliability due to low variance; see its
  “Details.”
  [`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md)
  replaces the ambiguously-named
  [`se()`](https://ccsarapas.github.io/lighthouse/reference/se.md),
  which is now deprecated.
- [`ci_sig()`](https://ccsarapas.github.io/lighthouse/reference/ci_sig.md)
  tests if a confidence interval indicates statistical significance.
- [`OR_to_p1()`](https://ccsarapas.github.io/lighthouse/reference/p_to_OR.md)
  and
  [`OR_to_p2()`](https://ccsarapas.github.io/lighthouse/reference/p_to_OR.md)
  convert odds ratios to probabilities. They complement
  [`p_to_OR()`](https://ccsarapas.github.io/lighthouse/reference/p_to_OR.md).
- [`dunn_test()`](https://ccsarapas.github.io/lighthouse/reference/dunn_test.md)
  performs Dunn’s test, a pairwise post-hoc test for following up a
  Kruskal-Wallis test.

#### Math functions

- `row_sums_across()` is a variant of
  [`base::rowSums()`](https://rdrr.io/r/base/colSums.html) that accepts
  tidyselect expressions and has alternative `NA` handling.
- [`sum_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md),
  [`min_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md),
  and
  [`max_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
  are variants of [`sum()`](https://rdrr.io/r/base/sum.html),
  [`min()`](https://rdrr.io/r/base/Extremes.html), and
  [`max()`](https://rdrr.io/r/base/Extremes.html) that remove `NA`s
  unless all values are `NA`.
  [`min_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
  and
  [`max_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
  were renamed from
  [`safe_min()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md)
  and
  [`safe_max()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md).

#### Variable transformation

- [`fct_collapse_alt()`](https://ccsarapas.github.io/lighthouse/reference/fct_collapse_alt.md)
  is a variant of
  [`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html)
  with options to handle non-existent values and level ordering.
- [`fct_na_if()`](https://ccsarapas.github.io/lighthouse/reference/fct_na_if.md)
  is a variant of
  [`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)
  that also removes the specified value from a factor’s levels.
- [`swap()`](https://ccsarapas.github.io/lighthouse/reference/swap.md)
  swaps values between two columns. It is an unconditional variant of
  [`swap_if()`](https://ccsarapas.github.io/lighthouse/reference/swap.md).

#### Data restructuring

- [`add_rows_at_value()`](https://ccsarapas.github.io/lighthouse/reference/add_rows_at_value.md)
  is similar to `add_blank_rows()`, but allows specifying position by
  column values rather than row numbers. Note there have been some
  changes in the function interface from the pre-release version; see
  the “Details” section of the documentation.
- [`pad_vectors()`](https://ccsarapas.github.io/lighthouse/reference/pad_vectors.md)
  pads a list of vectors with `NA`s to a common length.

#### Exporting results

- [`add_plot_slide()`](https://ccsarapas.github.io/lighthouse/reference/add_plot_slide.md)
  is a helper for exporting plots to PowerPoint with easier control of
  size and positioning.
- [`write_xlsx_styled()`](https://ccsarapas.github.io/lighthouse/reference/write_xlsx_styled.md)
  writes to .xlsx with basic column formatting.

#### Data visualization

- [`add_crossings()`](https://ccsarapas.github.io/lighthouse/reference/add_crossings.md)
  is a helper for creating area charts with different fills for positive
  vs negative values.
- [`after_opacity()`](https://ccsarapas.github.io/lighthouse/reference/opacity.md)
  and
  [`before_opacity()`](https://ccsarapas.github.io/lighthouse/reference/opacity.md)
  are utilities for color blending.

#### Other

- [`open_file()`](https://ccsarapas.github.io/lighthouse/reference/open_file.md)
  (alias
  [`file.open()`](https://ccsarapas.github.io/lighthouse/reference/open_file.md))
  opens a file with its default application. `open_folder()` (alias
  [`dir.open()`](https://ccsarapas.github.io/lighthouse/reference/open_file.md))
  opens a folder in the system file manager.
- Given two vectors,
  [`set_compare()`](https://ccsarapas.github.io/lighthouse/reference/set_compare.md)
  returns labelled subsets of unique and shared elements.
- [`suppress_warnings_if()`](https://ccsarapas.github.io/lighthouse/reference/suppress_if.md)
  and
  [`suppress_messages_if()`](https://ccsarapas.github.io/lighthouse/reference/suppress_if.md)
  conditionally suppress warnings or messages based on their text.
- [`eq_shape()`](https://ccsarapas.github.io/lighthouse/reference/eq_shape.md)
  checks if two objects have the same number of dimensions and same
  length along each dimension.

### New datasets

- `gain_missing_codes` is a quick reference for missing value labels
  used in GAIN datasets.

- `state.terr.name` and `state.terr.abb` are versions of `state.name`
  and `state.abb` that include US territories and the District of
  Columbia. `state.terr.data` is a data frame including names,
  abbreviations, and FIPS codes for US states, territories, and the
  District of Columbia.

### Added functionality

- [`count_pct()`](https://ccsarapas.github.io/lighthouse/reference/count_pct.md)
  and
  [`count_multiple()`](https://ccsarapas.github.io/lighthouse/reference/count_multiple.md)
  now support the `.by` argument for per-operation grouping. Integration
  of `.by` into other `count_*()` functions is planned for a future
  update.

- In
  [`summary_table()`](https://ccsarapas.github.io/lighthouse/reference/summary_table.md),
  the column of variable names can be dropped when only one variable is
  included by setting `.var_col_name = NULL`
  ([\#9](https://github.com/ccsarapas/lighthouse/issues/9)).

- [`count_duplicates()`](https://ccsarapas.github.io/lighthouse/reference/count_duplicates.md)
  now returns both the unique and total number of duplicated values.
  (e.g., `c(2, 2, 4, 4)` has two unique and four total values.)

- Added a `missing` argument to
  [`swap_if()`](https://ccsarapas.github.io/lighthouse/reference/swap.md)
  with options for cases where the condition is missing.

- Added a `warn_factor` argument to
  [`try_numeric()`](https://ccsarapas.github.io/lighthouse/reference/try_numeric.md)

### Bug fixes

- The `.cols_group_by` argument in
  [`summary_table()`](https://ccsarapas.github.io/lighthouse/reference/summary_table.md)
  now produces separate columns by group (fixes
  [\#6](https://github.com/ccsarapas/lighthouse/issues/6)).

- [`count_with_total()`](https://ccsarapas.github.io/lighthouse/reference/count_with_total.md)
  now produces totals for non-character columns (fixes
  [\#10](https://github.com/ccsarapas/lighthouse/issues/10)).

- [`days_diff()`](https://ccsarapas.github.io/lighthouse/reference/days_diff.md)
  now handles inputs of different types (e.g., a date and a datetime)
  with a warning (previously threw an error).

- Added General Election Day to `holidays_il` and arranged by date
  (fixes [\#1](https://github.com/ccsarapas/lighthouse/issues/1)).

- Removed Inauguration Day from `holidays_us`.

### Lifecycle changes

- [`rbool()`](https://ccsarapas.github.io/lighthouse/reference/rbool.md)
  has been *un*-deprecated. It was previously deprecated in favor of
  [`purrr::rbernoulli()`](https://purrr.tidyverse.org/reference/rbernoulli.html),
  but
  [`purrr::rbernoulli()`](https://purrr.tidyverse.org/reference/rbernoulli.html)
  has since been deprecated itself.

- [`pivot_wider_alt()`](https://ccsarapas.github.io/lighthouse/reference/pivot_wider_alt.md)
  is defunct. Changes to
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  made its most important functionality unnecessary. Further changes to
  tidyr broke it, and it was judged not worth the effort of fixing.

- [`na_like()`](https://ccsarapas.github.io/lighthouse/reference/na_like.md)
  and
  [`median_dbl()`](https://ccsarapas.github.io/lighthouse/reference/median_dbl.md)
  are deprecated. They are no longer needed given more flexible handling
  of mixed classes by
  [`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  and
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  as of \[dplyr
  v1.1.0\]\[<https://dplyr.tidyverse.org/news/index.html#vctrs-1-1-0>\].
  (Plus
  [`na_like()`](https://ccsarapas.github.io/lighthouse/reference/na_like.md)
  was quite buggy and unreliable; resolves
  [\#2](https://github.com/ccsarapas/lighthouse/issues/2)).

- [`row_sums_spss()`](https://ccsarapas.github.io/lighthouse/reference/row_sums_spss.md)
  deprecated in favor of `row_sums_across()`.

- [`safe_min()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md)
  and
  [`safe_max()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md)
  renamed to
  [`min_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md)
  and
  [`max_if_any()`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.md);
  the old names are deprecated.

- [`se()`](https://ccsarapas.github.io/lighthouse/reference/se.md)
  renamed to
  [`se_mean()`](https://ccsarapas.github.io/lighthouse/reference/se_mean.md);
  the old name is deprecated.

### Other changes

- In
  [`asterisks()`](https://ccsarapas.github.io/lighthouse/reference/asterisks.md),
  changed the default for `include_key` from `TRUE` to `FALSE`.

- [`percent()`](https://scales.r-lib.org/reference/percent_format.html)
  and [`comma()`](https://scales.r-lib.org/reference/comma.html)
  re-exported from scales
  ([\#11](https://github.com/ccsarapas/lighthouse/issues/11)).

## lighthouse 0.6.0

### New functions

- Grouping and summary functions:
  - [`group_with_total()`](https://ccsarapas.github.io/lighthouse/reference/group_with_total.md)
  - [`count_multiple()`](https://ccsarapas.github.io/lighthouse/reference/count_multiple.md)
  - [`count_unique()`](https://ccsarapas.github.io/lighthouse/reference/count_unique.md)
  - [`count_duplicates()`](https://ccsarapas.github.io/lighthouse/reference/count_duplicates.md)
  - [`cols_info()`](https://ccsarapas.github.io/lighthouse/reference/cols_info.md)
- Statistical and math functions:
  - [`wkappa()`](https://ccsarapas.github.io/lighthouse/reference/wkappa.md)
  - [`cohen_w()`](https://ccsarapas.github.io/lighthouse/reference/cohen_w.md)
  - [`median_dbl()`](https://ccsarapas.github.io/lighthouse/reference/median_dbl.md)
  - [`safe_min()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md),
    [`safe_max()`](https://ccsarapas.github.io/lighthouse/reference/safe_minmax.md)
  - [`pmin_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md),
    [`pmax_across()`](https://ccsarapas.github.io/lighthouse/reference/psum_across.md)
  - [`cumsum_desc()`](https://ccsarapas.github.io/lighthouse/reference/cumsum_desc.md)
  - [`scale_vec()`](https://ccsarapas.github.io/lighthouse/reference/scale_vec.md)
  - [`reverse_key()`](https://ccsarapas.github.io/lighthouse/reference/reverse_key.md)
- Data restructuring:
  - [`add_header()`](https://ccsarapas.github.io/lighthouse/reference/add_header.md)
  - [`t_tibble()`](https://ccsarapas.github.io/lighthouse/reference/t_tibble.md)
  - [`rev_rows()`](https://ccsarapas.github.io/lighthouse/reference/rev_rows.md)
  - [`fct_reorder_n()`](https://ccsarapas.github.io/lighthouse/reference/fct_reorder_n.md)
- For working with missing values:
  - [`find_na_cols()`](https://ccsarapas.github.io/lighthouse/reference/find_na_cols.md),
    [`drop_na_cols()`](https://ccsarapas.github.io/lighthouse/reference/find_na_cols.md)
  - [`n_valid()`](https://ccsarapas.github.io/lighthouse/reference/n_valid.md),
    [`pct_valid()`](https://ccsarapas.github.io/lighthouse/reference/n_valid.md),
    [`n_pct_valid()`](https://ccsarapas.github.io/lighthouse/reference/n_valid.md)
  - [`discard_na()`](https://ccsarapas.github.io/lighthouse/reference/discard_na.md)
  - [`null_to_na()`](https://ccsarapas.github.io/lighthouse/reference/na_to_null.md)
  - [`is_valid()`](https://ccsarapas.github.io/lighthouse/reference/is_valid.md)
- For working with strings:
  - [`str_prefix()`](https://ccsarapas.github.io/lighthouse/reference/str_prefix.md),
    [`str_suffix()`](https://ccsarapas.github.io/lighthouse/reference/str_prefix.md)
  - [`glue_chr()`](https://ccsarapas.github.io/lighthouse/reference/glue_chr.md)
- For working with dates:
  - [`datetimes_to_date()`](https://ccsarapas.github.io/lighthouse/reference/datetimes_to_date.md)
  - [`next_bizday()`](https://ccsarapas.github.io/lighthouse/reference/bizday.md)
- Logical tests:
  - [`is_duplicate()`](https://ccsarapas.github.io/lighthouse/reference/is_duplicate.md)
  - [`is_spss()`](https://ccsarapas.github.io/lighthouse/reference/is_spss.md)
  - [`is_coercible_integer()`](https://ccsarapas.github.io/lighthouse/reference/is_coercible_numeric.md),
    [`is_coercible_logical()`](https://ccsarapas.github.io/lighthouse/reference/is_coercible_numeric.md)
- Other:
  - [`gain_ss_score()`](https://ccsarapas.github.io/lighthouse/reference/gain_ss_score.md)

### Other changes

- Added datasets for federal (`holidays_us`), Illinois (`holidays_il`),
  and Chestnut Health Systems
  (holidays_chestnut`) holidays (primarily for use with`next_bizday()\`
  function).
- Added `strict` argument to
  [`is_TRUE()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_FALSE()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_TRUE_or_NA()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  and
  [`is_FALSE_or_NA()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md)
- Improvements to
  [`set_ggplot_opts()`](https://ccsarapas.github.io/lighthouse/reference/set_ggplot_opts.md),
  [`ggview()`](https://ccsarapas.github.io/lighthouse/reference/ggview.md),
  and
  [`is_coercible_numeric()`](https://ccsarapas.github.io/lighthouse/reference/is_coercible_numeric.md)
- Bugfixes for
  [`in_excel()`](https://ccsarapas.github.io/lighthouse/reference/in_excel.md),
  [`count_na()`](https://ccsarapas.github.io/lighthouse/reference/count_na.md),
  [`summary_table()`](https://ccsarapas.github.io/lighthouse/reference/summary_table.md),
  [`pivot_wider_alt()`](https://ccsarapas.github.io/lighthouse/reference/pivot_wider_alt.md),
  [`print_all()`](https://ccsarapas.github.io/lighthouse/reference/print_all.md),
  [`asterisks()`](https://ccsarapas.github.io/lighthouse/reference/asterisks.md),
  and
  [`coerce_na_range()`](https://ccsarapas.github.io/lighthouse/reference/coerce_na_range.md)
- Remove check for `lighthouse` updates on load

## lighthouse 0.5.0

- Check if `lighthouse` update is available on load
- New infix operators: `%all_in%`, `%any_in%`
- Exported
  [`na_like()`](https://ccsarapas.github.io/lighthouse/reference/na_like.md)

## lighthouse 0.4.1

- Bugfix for
  [`print_all()`](https://ccsarapas.github.io/lighthouse/reference/print_all.md)

## lighthouse 0.4.0

- New logical tests:
  [`is_TRUE()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_FALSE()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_TRUE_or_NA()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_FALSE_or_NA()`](https://ccsarapas.github.io/lighthouse/reference/is_TRUE.md),
  [`is_coercible_numeric()`](https://ccsarapas.github.io/lighthouse/reference/is_coercible_numeric.md)
- New count functions:
  [`crosstab()`](https://ccsarapas.github.io/lighthouse/reference/crosstab.md),
  [`count_na()`](https://ccsarapas.github.io/lighthouse/reference/count_na.md)
- New data transformations:
  [`scale_mad()`](https://ccsarapas.github.io/lighthouse/reference/scale_mad.md),
  [`winsorize()`](https://ccsarapas.github.io/lighthouse/reference/winsorize.md)
- New date functions:
  [`floor_month()`](https://ccsarapas.github.io/lighthouse/reference/floor_month.md),
  [`floor_week()`](https://ccsarapas.github.io/lighthouse/reference/floor_month.md),
  [`floor_days()`](https://ccsarapas.github.io/lighthouse/reference/floor_month.md),
  [`days_diff()`](https://ccsarapas.github.io/lighthouse/reference/days_diff.md)
- Other new functions:
  [`asterisks()`](https://ccsarapas.github.io/lighthouse/reference/asterisks.md),
  [`print_n()`](https://ccsarapas.github.io/lighthouse/reference/print_n.md),
  [`print_all()`](https://ccsarapas.github.io/lighthouse/reference/print_all.md),
  [`na_to_null()`](https://ccsarapas.github.io/lighthouse/reference/na_to_null.md),
  [`set_ggplot_opts()`](https://ccsarapas.github.io/lighthouse/reference/set_ggplot_opts.md)
- Added added optional `name` argument to
  [`in_excel()`](https://ccsarapas.github.io/lighthouse/reference/in_excel.md)
