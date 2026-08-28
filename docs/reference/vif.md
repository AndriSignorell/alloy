# Variance Inflation Factors (VIF / GVIF)

Computes variance inflation factors (VIF) or generalized VIFs (GVIF) for
a fitted model. For multi-parameter terms (e.g., factors), GVIFs are
returned along with a scaled version `GVIF^(1/(2*Df))`.

## Usage

``` r
vif(fit)
```

## Arguments

- fit:

  A fitted model object. Currently supports objects of class `lm`,
  `glm`, and `gls`.

## Value

If all terms have 1 degree of freedom, a named numeric vector of VIFs.
Otherwise, a matrix with columns:

- GVIF:

  Generalized variance inflation factor

- Df:

  Degrees of freedom for the term

- GVIF^(1/(2\*Df)):

  Scaled GVIF for comparability

## Details

The function is based on the implementation in the car package (Fox and
Weisberg). GVIFs are computed from the correlation matrix of the model
coefficients.

Interpretation:

- Values close to 1 indicate low multicollinearity

- Values \> 5 or 10 may indicate problematic collinearity

Note that VIFs are only meaningful if the model includes an intercept.
For models without intercept, a warning is issued.

## See also

Other regression.utils: [`coefCI()`](coefCI.md),
[`pseudoR2()`](pseudoR2.md), [`rSq()`](rSq.md),
[`refLevel()`](refLevel.md), [`response()`](response.md),
[`varImp()`](varImp.md)

## Examples

``` r
mod <- lm(mpg ~ wt + cyl, data = mtcars)
vif(mod)
#>       wt      cyl 
#> 2.579312 2.579312 
```
