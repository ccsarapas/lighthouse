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
#> 1 prediction1   250 0.845       0.901       0.947 0.890 0.952

# can test multiple predictors simultaneously
ex_data %>%
  accuracy_stats(actual, prediction1:prediction5)
#> # A tibble: 5 × 7
#>   Predictor       n Kappa Sensitivity Specificity   PPV   NPV
#>   <fct>       <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl>
#> 1 prediction1   250 0.845       0.901       0.947 0.890 0.952
#> 2 prediction2   250 0.628       0.840       0.822 0.694 0.914
#> 3 prediction3   250 0.213       0.593       0.639 0.440 0.766
#> 4 prediction4   250 0.828       1           0.882 0.802 1    
#> 5 prediction5   250 0.846       0.802       1     1     0.914

# if `include_counts` = TRUE, will also return n of false positives,
# false negatives, etc., as well as and observed and expected % agreement
ex_data %>%
  accuracy_stats(actual, prediction1:prediction5, include_counts = TRUE)
#> # A tibble: 5 × 13
#>   Predictor       n    TP    FP    TN    FN pAgreeObserved pAgreeExpected Kappa
#>   <fct>       <dbl> <int> <int> <int> <int>          <dbl>          <dbl> <dbl>
#> 1 prediction1   250    73     9   160     8          0.932          0.561 0.845
#> 2 prediction2   250    68    30   139    13          0.828          0.538 0.628
#> 3 prediction3   250    48    61   108    33          0.624          0.523 0.213
#> 4 prediction4   250    81    20   149     0          0.92           0.534 0.828
#> 5 prediction5   250    65     0   169    16          0.936          0.584 0.846
#> # ℹ 4 more variables: Sensitivity <dbl>, Specificity <dbl>, PPV <dbl>,
#> #   NPV <dbl>
```
