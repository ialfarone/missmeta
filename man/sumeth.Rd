% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumeth.R
\name{sumeth}
\alias{sumeth}
\title{Summarizes the results of bivariate meta-analyses conducted with imputed datasets}
\usage{
sumeth(eff1, eff2, se1, se2, cov12, method)
}
\arguments{
\item{eff1}{Character string. Column name in  for the first effect size to impute (e.g., "EstCR").}

\item{eff2}{Character string. Column name in  for the second effect size to impute (e.g., "EstSR").}

\item{se1}{Character string. Column name in  for the standard error associated with `eff1`.}

\item{se2}{Character string. Column name in  for the standard error associated with `eff2`.}

\item{cov12}{Character string. Column name in for the covariance between effect sizes.}

\item{method}{Character string. Name of the distribution used for imputing data.}
}
\value{
A data frame containing the pooled results.
}
\description{
Analyzes the results of multiple imputed datasets by using Rubin's Rules.
}
\examples{

 res <- data.frame(
  eff1 = c(3.7, 3.5),
  eff2 = c(3.2, 3.2),
  se1 = c(1.5, 1.3),
  se2 = c(2.7, 1.8),
  cov12 = c(0.02, 0.5),
  method = "Normal (1, 6)"
)

sumeth(eff1 = res$eff1, eff2 = res$eff2,
se1 = res$se1, se2 = res$se2,
cov12 = res$cov12, method = "Normal (1, 6)")

}
