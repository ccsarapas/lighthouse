# Unpack and assign

Assigns a vector of values to an equal-length vector of names.

## Usage

``` r
lhs %<-% rhs

lhs %->% rhs
```

## Details

The `%<-%` operator and its left-to-right variant (`%->%`) allow
parallel assignment, similar to (e.g.) Python:

    # python
    var1, var2, var3 = [2, 4, 6]

    # R
    c(var1, var2, var3) %<-% c(2, 4, 6)

See the `zeallot` package for a similar operator with more advanced
functionality.

## Examples

``` r
c(cyl4, cyl6, cyl8) %<-% split(mtcars, mtcars$cyl)
cyl4
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

split(mtcars, mtcars$gear) %->% c(G3, G4, G5)
G5
#>                 mpg cyl  disp  hp drat    wt qsec vs am gear carb
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
#> Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
#> Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
#> Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8

c(model_hp, model_size, model_trans) %<-% purrr::map(
  c("hp", "disp + wt", "gear * am"),
  ~ lm(paste("mpg ~", .x), data = mtcars)
)
summary(model_size)
#> 
#> Call:
#> lm(formula = paste("mpg ~", .x), data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.4087 -2.3243 -0.7683  1.7721  6.3484 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 34.96055    2.16454  16.151 4.91e-16 ***
#> disp        -0.01773    0.00919  -1.929  0.06362 .  
#> wt          -3.35082    1.16413  -2.878  0.00743 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.917 on 29 degrees of freedom
#> Multiple R-squared:  0.7809, Adjusted R-squared:  0.7658 
#> F-statistic: 51.69 on 2 and 29 DF,  p-value: 2.744e-10
#> 
summary(model_trans)
#> 
#> Call:
#> lm(formula = paste("mpg ~", .x), data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -6.3800 -3.3063 -0.7567  3.1575  9.0200 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)    1.277      8.217   0.155  0.87764   
#> gear           4.943      2.539   1.947  0.06163 . 
#> am            44.578     14.010   3.182  0.00356 **
#> gear:am       -9.838      3.614  -2.722  0.01103 * 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 4.512 on 28 degrees of freedom
#> Multiple R-squared:  0.4938, Adjusted R-squared:  0.4396 
#> F-statistic: 9.105 on 3 and 28 DF,  p-value: 0.0002282
#> 
```
