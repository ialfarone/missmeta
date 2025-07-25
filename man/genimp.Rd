% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genimp.R
\name{genimp}
\alias{genimp}
\title{Generate flexible imputation for bivariate meta-analysis}
\usage{
genimp(
  df,
  iter = 100,
  imp1,
  imp2,
  eff1 = "Eff1",
  eff2 = "Eff2",
  se1 = "SE1",
  se2 = "SE2",
  cor = "Cor.ws",
  N = "N",
  imprho = 0.7
)
}
\arguments{
\item{df}{Dataframe for meta-analysis with missing values at study-level (dataframe).}

\item{iter}{Number of imputed datasets (numeric).}

\item{imp1}{Function that returns a numeric vector of length n for imputed values of eff1.}

\item{imp2}{Function that returns a numeric vector of length n for imputed values of eff2.}

\item{eff1}{Character string. Column name in `df` for the first effect size to impute (e.g., "EstCR").}

\item{eff2}{Character string. Column name in `df` for the second effect size to impute (e.g., "EstSR").}

\item{se1}{Character string. Column name in `df` for the standard error associated with `eff1`.}

\item{se2}{Character string. Column name in `df` for the standard error associated with `eff2`.}

\item{cor}{Character string. Column name in `df` for the within-study correlation between effect sizes.}

\item{N}{Character string. Column name in `df` for the sample size (or total number of observations) per study.}

\item{imprho}{Numeric. Assumed within-study correlation to impute for studies where `cor` is missing.}
}
\value{
A list of imputed data frames (one per iteration), with missing values replaced using user-specified models.
}
\description{
Generates multiple imputed datasets by filling in missing effect sizes using user-defined
distributions. The function allows separate imputation models for two outcome variables
(e.g., effect sizes from different measures), and uses a log-normal model to impute missing
standard errors. Within-study correlations can also be imputed.
}
\examples{

 dmnar <- data.frame(
  EstCR = c(3.7, 3.5, 4.2, NA),
  EstSR = c(3.2, 3.2, NA, 5.6),
  SECR = c(1.5, 1.3, 2.1, NA),
  SESR = c(0.7, 1.8, NA, 2.2),
  Cor.ws = c(0.75, 0.5, NA, NA),
  N = c(100, 150, 120, 95)
)

imp1 = function(n) rnorm(n, mean = 3, sd = 2)
imp2 = function(n) runif(n, min = 4, max = 7)

out = genimp(df = dmnar,
              iter = 2, # set 1000
              imp1 = imp1,
              imp2 = imp2,
              eff1 = "EstCR",
              eff2 = "EstSR",
              se1 = "SECR",
              se2 = "SESR",
              cor = "Cor.ws",
              N = "N",
              imprho = 0.3)
out
}
