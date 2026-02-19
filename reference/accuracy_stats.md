# Compute common accuracy and agreement metrics

Given a vector of `true_values` and one or more vectors of test values
(passed to `...`), computes sensitivity, specificity, positive
predictive value (PPV), negative predictive value (NPV), and Cohen's
kappa.

## Usage

``` r
accuracy_stats(.data, true_values, ..., include_counts = FALSE)
```

## Examples

``` r
# create example data w predictors with different properties:
ex_data <- tibble::tibble(
  actual = rbinom(250, 1, .3),                                   # 250 cases, p(outcome) = .3
  prediction1 = ifelse(runif(250) <= .05, 1L - actual, actual), # 5% error rate
  prediction2 = ifelse(runif(250) <= .15, 1L - actual, actual), # 15% error rate
  prediction3 = ifelse(runif(250) <= .35, 1L - actual, actual), # 35% error rate
  prediction4 = ifelse(runif(250) <= .15, 1L, actual),          # 15% with positive bias
  prediction5 = ifelse(runif(250) <= .15, 0L, actual)           # 15% with negative bias
)

# testing predicted v actual values
ex_data %>%
  accuracy_stats(actual, prediction1)
#> # A tibble: 1 × 7
#>   Predictor       n Kappa Sensitivity Specificity   PPV   NPV
#>   <fct>       <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl>
#> 1 prediction1   250 0.844         0.9       0.947 0.889 0.953

# can test multiple predictors simultaneously
ex_data %>%
  accuracy_stats(actual, prediction1:prediction5)
#> # A tibble: 5 × 7
#>   Predictor       n Kappa Sensitivity Specificity   PPV   NPV
#>   <fct>       <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl>
#> 1 prediction1   250 0.844        0.9        0.947 0.889 0.953
#> 2 prediction2   250 0.636        0.85       0.824 0.694 0.921
#> 3 prediction3   250 0.214        0.6        0.635 0.436 0.771
#> 4 prediction4   250 0.820        1          0.876 0.792 1    
#> 5 prediction5   250 0.845        0.8        1     1     0.914

# if `include_counts` = TRUE, will also return n of false positives,
# false negatives, etc., as well as and observed and expected % agreement
ex_data %>%
  accuracy_stats(actual, prediction1:prediction5, include_counts = TRUE)
#> # A tibble: 5 × 13
#>   Predictor       n    TP    FP    TN    FN pAgreeObserved pAgreeExpected Kappa
#>   <fct>       <dbl> <int> <int> <int> <int>          <dbl>          <dbl> <dbl>
#> 1 prediction1   250    72     9   161     8          0.932          0.563 0.844
#> 2 prediction2   250    68    30   140    12          0.832          0.539 0.636
#> 3 prediction3   250    48    62   108    32          0.624          0.522 0.214
#> 4 prediction4   250    80    21   149     0          0.916          0.535 0.820
#> 5 prediction5   250    64     0   170    16          0.936          0.588 0.845
#> # ℹ 4 more variables: Sensitivity <dbl>, Specificity <dbl>, PPV <dbl>,
#> #   NPV <dbl>
```
