
# pilotsampsize

<!-- badges: start -->
<!-- badges: end -->

ssPilot provides functions for determining the sample size of external pilot 
randomized trials for continuous outcomes. The implementation follows the 
methodology of Whitehead et al. (2016), which selects pilot trial sample sizes 
to minimize the overall sample size across the pilot and definitive trials. 
Sample size calculations for the definitive trial incorporate uncertainty in 
the estimated standard deviation from the pilot study using either the 
Upper Confidence Limit (Brownie, 1995) or the non-central t-distribution 
approach (Julious and Owen, 2006).

## Installation

You can install the development version of ssPilot from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("joeldadiboe-rgb/ssPilot")
```

## Example

The target effect size of the definitive trial is 0.5 
with 90% power at 5% significance level. The 80% Upper
Confidence Limit for the variance of the pilot study will 
be used to estimate the sample size of the definitive trial.

``` r
optimized_ucl_sample_size(
  sd = 1,
  effect = 0.50,
  power = 0.90,
  alpha = 0.05,
  conf_level = 0.80
)
```
The target effect size of the definitive trial is 0.5 
with 90% power at 5% significance level. 
The Non-central t-distribution incorporating the uncertainty of the 
variance from the pilot trial will be used to estimate the sample size of the definitive trial.

```r
optimized_nct_sample_size(
   sd = 1,
   effect = 0.50,
   power = 0.90,
   alpha = 0.05
)
```

## References

Browne RH. On the use of a pilot sample for sample size determination. Stat Med 1995; 14: 1933–1940.

Sim J and Lewis M. The size of a pilot study for a clinical should be calculated in relation to considerations of
precision and efficiency. J Clin Epidemiol 2012; 65:301–308.

Julious SA, Owen RJ. Sample size calculations for clinical studies allowing for uncertainty about the variance. Pharmaceutical Statistics: The Journal of Applied Statistics in the Pharmaceutical Industry. 2006 Jan;5(1):29-37.

Whitehead AL, Julious SA, Cooper CL, Campbell MJ. Estimating the sample size for a pilot randomised trial to minimise the overall trial sample size for the external pilot and main trial for a continuous outcome variable. Statistical methods in medical research. 2016 Jun;25(3):1057-73.
