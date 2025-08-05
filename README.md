# missmeta

**missmeta** provides flexible imputation for multivariate meta-analysis with missing outcomes.

Multivariate meta-analysis allows for the joint estimation of multiple correlated outcomes.
Standard multivariate meta-analysis can handle also incomplete dataset under the assumption of Missing At Random. 
The present R package extends it by enabling flexible imputation to generate multiple imputed datasets for multivariate meta-analysis with missing effect sizes, 
standard errors, and within-study correlations. Users can define custom imputation models for multiple outcomes. 
Missing standard errors are imputed using a multivariate log-normal model, and missing correlations can be 
specified manually or imputed with user-defined values.

## Installation
The present version of **missmeta** can be installed from GitHub with:

```r
remotes::install_github("ialfarone/missmeta")
```
