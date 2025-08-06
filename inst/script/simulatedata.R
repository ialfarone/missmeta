library(mvtnorm)
library(systemfit)
library(dplyr)
library(tmvtnorm)

# Simulate dataset ####

S <- 50
N <- 100

Mu <- c(0, 0)
Tau <- c(1, 1)
rho <- 0.7 # between-study variance
Sigma <- diag(Tau) %*% matrix(c(1, rho, rho, 1), nrow = 2) %*% diag(Tau)
RTher <- rmvnorm(S, Mu, Sigma)

b0 <- rnorm(S, mean = 20, sd = 2) # intercept

b1 <- rnorm(S, mean = 0.5, sd = 0.2) # covariates
b2 <- rnorm(S, mean = 1.5, sd = 0.3)

b3 <- 3 # therapy effect

data = vector(mode = "list", length = S)

# generate studies

for (i in 1:S) {
  minA = runif(1, min = 18, max = 65)
  maxA = runif(1, min = minA + 5, max = 80)
  Age = runif(N, min = minA, max = maxA)

  pFem = 0.45
  Sex = rbinom(N, size = 1, prob = pFem)
  Ther = rbinom(N, size = 1, prob = 0.5)

  # two outcome measures, a clinician rating and a self-report

  CR = rnorm(N,
             mean = b0 + b1 * Age + b2 * Sex + (b3 + RTher[i, 1]) * Ther,
             sd = 5)
  SR = rnorm(N,
             mean = b0 + b1 * Age + b2 * Sex + (b3 + RTher[i, 2]) * Ther,
             sd = 7)

  data[[i]] = data.frame(
    Study = i,
    Age = Age,
    Sex = factor(Sex, levels = 0:1, labels = c("M", "F")),
    Ther = factor(Ther, levels = 0:1, labels = c("New", "Std")),
    CR = CR,
    SR = SR
  )
}

d = do.call(rbind, data)
dat = vector(mode = "list", length = S)

# Calculate effect size and standard errors ####
# with Seemingly Unrelated Regressions

for (s in 1:S) {
  Sn = d[d$Study == s, ]

  m1 = CR ~ Age + Sex + Ther
  m2 = SR ~ Age + Sex + Ther

  fitsur = systemfit(list(CR = m1, SR = m2), "SUR", data = Sn)
  sum = summary(fitsur)

  dat[[s]] = data.frame(
    Study = s,
    N = N,
    EstCR = sum$coefficients[4, 1],
    EstSR = sum$coefficients[8, 1],
    SECR = sum$coefficients[4, 2],
    SESR = sum$coefficients[8, 2],
    Cor.ws = sum$residCor["CR", "SR"]
  )
}

dat = do.call(rbind, dat)
head(dat)

# Generate missing ####

## Missing Not At Random ####
target = 0.2
betaCR = 2
betaSR = -1.5
invlogit <- function(x) plogis(x)

beta0_cr = uniroot(function(b0) mean(invlogit(b0 + betaCR * dat$EstCR)) - (1 - target / 2), c(-20, 20))$root
prob_cr = invlogit(beta0_cr + betaCR * dat$EstCR)
beta0_sr = uniroot(function(b0) mean(invlogit(b0 + betaSR * dat$EstSR)) - (1 - target / 2), c(-20, 20))$root
prob_sr = invlogit(beta0_sr + betaSR * dat$EstSR)

M_cr = rbinom(nrow(dat), 1, 1 - prob_cr)
M_sr = rbinom(nrow(dat), 1, 1 - prob_sr)

conflict = which(M_cr == 1 & M_sr == 1)
meanCR_obs = mean(dat$EstCR)
meanSR_obs = mean(dat$EstSR)

for (i in conflict) {
  dist_cr = abs(dat$EstCR[i] - meanCR_obs)
  dist_sr = abs(dat$EstSR[i] - meanSR_obs)
  if (dist_cr > dist_sr) {
    M_sr[i] = 0
  } else {
    M_cr[i] = 0
  }
}

dmnar = dat

dmnar[M_cr == 1, c("EstCR", "SECR")] = NA
dmnar[M_sr == 1, c("EstSR", "SESR")] = NA
dmnar$Cor.ws[is.na(dmnar$EstCR) | is.na(dmnar$EstSR)] = NA

head(dmnar)

usethis::use_data(dmnar, overwrite = TRUE)
