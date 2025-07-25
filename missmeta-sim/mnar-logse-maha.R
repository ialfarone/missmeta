################
rm(list = ls())
################

library(mixmeta)
library(mvtnorm)
library(systemfit)
library(tmvtnorm)
library(ggplot2)
library(dplyr)
library(parallel)
library(furrr)
library(purrr)

set.seed(2025)

##############################
# genimp.mnar: Imputation ####
##############################

cor2cov = function(sd1, sd2, rho) sd1 * sd2 * rho

genimp.mnar = function(df, iter = 500, distribution = c("uniform", "normal", "tmvn"),
                       minCR, maxCR, minSR, maxSR, meanCR, meanSR, sdCR, sdSR,
                       meantmv, sigmatmv, lower, upper, imprho) {

  distribution = match.arg(distribution)
  results = vector("list", iter)

  for (i in 1:iter) {
    dfi = df
    NmissCR = sum(is.na(dfi$EstCR))
    NmissSR = sum(is.na(dfi$EstSR))

    if (distribution == "uniform") {
      impCR = runif(NmissCR, min = minCR, max = maxCR)
      impSR = runif(NmissSR, min = minSR, max = maxSR)
    } else if (distribution == "normal") {
      impCR = rnorm(NmissCR, mean = meanCR, sd = sdCR)
      impSR = rnorm(NmissSR, mean = meanSR, sd = sdSR)
    } else if (distribution == "tmvn") {
      imputed = rtmvnorm(
        n = sum(is.na(dfi$Cor.ws)),
        mean = meantmv,
        sigma = sigmatmv,
        lower = lower,
        upper = upper
      )
      impCR = imputed[, 1]
      impSR = imputed[, 2]
    }

    dfi$EstCR[is.na(dfi$EstCR)] = impCR
    dfi$EstSR[is.na(dfi$EstSR)] = impSR
    dfi$Cor.ws[is.na(dfi$Cor.ws)] = imprho

    # Irene: See here from e-mail
    # new imputation of std.err from lognormal biv distr (see syst rev)

    log_sig = with(dfi[!is.na(dfi$SECR) & !is.na(dfi$SESR), ],
                   cbind(log(SECR * sqrt(N)),
                         log(SESR * sqrt(N))))
    mu_hat    = colMeans(log_sig)
    Sigma_hat = var(log_sig)

    miss = with(dfi, is.na(SECR) | is.na(SESR))
    log_draw  = rmvnorm(sum(miss), mu_hat, Sigma_hat)
    sigma_imp = exp(log_draw)

    idx_CR = miss & is.na(dfi$SECR)
    idx_SR = miss & is.na(dfi$SESR)

    dfi$SECR[idx_CR] = sigma_imp[ idx_CR[miss], 1 ] / sqrt(dfi$N[idx_CR])
    dfi$SESR[idx_SR] = sigma_imp[ idx_SR[miss], 2 ] / sqrt(dfi$N[idx_SR])

    # Irene: from here same as before

    theta = cbind(dfi$EstCR, dfi$EstSR)
    Sigma = cbind(dfi$SECR^2,
                  cor2cov(dfi$SECR, dfi$SESR, dfi$Cor.ws),
                  dfi$SESR^2)

    mv = mixmeta(theta, Sigma, method = "reml")
    ci = confint(mv)

    results[[i]] = data.frame(
      iter = i,
      eff1 = mv$coefficients[1],
      eff2 = mv$coefficients[2],
      se1 = sqrt(mv$vcov[1, 1]),
      se2 = sqrt(mv$vcov[2, 2]),
      cov = mv$vcov[1, 2],
      ci.lb1 = ci[1, 1], ci.ub1 = ci[1, 2],
      ci.lb2 = ci[2, 1], ci.ub2 = ci[2, 2]
    )
  }
  do.call(rbind, results)
}

##############################
# sum.meth: Rubin's rules ####
##############################

sum.meth = function(res, true1, true2) {
  m = nrow(res)
  Q_mat = cbind(res$eff1, res$eff2)
  Q_bar = colMeans(Q_mat)
  B = cov(Q_mat)

  U_list = lapply(1:m, function(i) {
    matrix(c(res$se1[i]^2, res$cov[i], res$cov[i], res$se2[i]^2), nrow = 2)
  })
  U_bar = Reduce("+", U_list) / m
  Tmat = U_bar + (1 + 1/m) * B
  se = sqrt(diag(Tmat))

  df = (m - 1) * (1 + diag(U_bar) / ((1 + 1/m) * diag(B)))^2

  ci1 = Q_bar[1] + c(-1, 1) * qt(0.975, df[1]) * se[1]
  ci2 = Q_bar[2] + c(-1, 1) * qt(0.975, df[2]) * se[2]

  data.frame(
    est_CR = Q_bar[1], est_SR = Q_bar[2],
    bias_CR = Q_bar[1] - true1,
    bias_SR = Q_bar[2] - true2,
    cover_CR = as.numeric(ci1[1] <= true1 && true1 <= ci1[2]),
    cover_SR = as.numeric(ci2[1] <= true2 && true2 <= ci2[2]),
    pci_lb_CR = ci1[1], pci_ub_CR = ci1[2],
    pci_lb_SR = ci2[1], pci_ub_SR = ci2[2]
  )
}


#################################
# scenarios: full simulation ####
#################################

scenarios = function(condition_grid, minCR, maxCR,
                     minSR, maxSR, lower, upper, iter = 500,
                     true1 = 3, true2 = 3) {

  results_all = lapply(1:nrow(condition_grid), function(i) {
    cond = condition_grid[i, ]
    sim(
      seed = cond$seed,
      target = cond$target,
      meanCR = cond$meanCR,
      meanSR = cond$meanSR,
      sdCR = cond$sdCR,
      sdSR = cond$sdSR,
      minCR = minCR,
      maxCR = maxCR,
      minSR = minSR,
      maxSR = maxSR,
      lower = lower,
      upper = upper,
      iter = iter,
      true1 = true1,
      true2 = true2,
      distribution = cond$distribution
    )

  })
  do.call(rbind, results_all)

}

##################################
# sim: data generation + MNAR ####
##################################

sim = function(target, meanCR, meanSR, sdCR, sdSR,
               minCR, maxCR, minSR, maxSR, lower, upper,
               iter = iter, true1 = 3, true2 = 3,
               distribution = c("normal", "uniform", "tmvn")) {
  distribution = match.arg(distribution)
  S = 50
  N = 100
  Mu = c(0, 0)
  Tau = c(1, 1)
  rho = 0.7
  Sigma = diag(Tau) %*% matrix(c(1, rho, rho, 1), 2) %*% diag(Tau)
  RTher = rmvnorm(S, Mu, Sigma)

  b0 = rnorm(S, 20, 2)
  b1 = rnorm(S, 0.5, 0.2)
  b2 = rnorm(S, 1.5, 0.3)
  b3 = 3

  data = lapply(1:S, function(i) {
    minA = runif(1, 18, 65)
    maxA = runif(1, minA + 5, 80)
    Age = runif(N, minA, maxA)
    Sex = rbinom(N, 1, 0.45)
    Ther = rbinom(N, 1, 0.5)

    CR = rnorm(N, b0[i] + b1[i] * Age + b2[i] * Sex + (b3 + RTher[i, 1]) * Ther, 5)
    SR = rnorm(N, b0[i] + b1[i] * Age + b2[i] * Sex + (b3 + RTher[i, 2]) * Ther, 7)

    data.frame(Study = i, Age, Sex = factor(Sex), Ther = factor(Ther), CR, SR)
  })

  d = do.call(rbind, data)

  dat = lapply(1:S, function(s) {
    Sn = d[d$Study == s, ]
    fitsur = systemfit(list(CR = CR ~ Age + Sex + Ther, SR = SR ~ Age + Sex + Ther),
                       "SUR", data = Sn)
    sum = summary(fitsur)
    data.frame(
      Study = s,
      N = N,
      EstCR = sum$coefficients[4, 1],
      EstSR = sum$coefficients[8, 1],
      SECR = sum$coefficients[4, 2],
      SESR = sum$coefficients[8, 2],
      Cor.ws = sum$residCor["CR", "SR"]
    )
  })

  dat = do.call(rbind, dat)

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

  dat[M_cr == 1, c("EstCR", "SECR")] = NA
  dat[M_sr == 1, c("EstSR", "SESR")] = NA
  dat$Cor.ws[is.na(dat$EstCR) | is.na(dat$EstSR)] = NA

  res = genimp.mnar(
    df = dat,
    distribution = distribution,
    iter = iter,
    minCR = minCR, maxCR = maxCR,
    minSR = minSR, maxSR = maxSR,
    meanCR = meanCR, meanSR = meanSR,
    sdCR = sdCR, sdSR = sdSR,
    imprho = 0.7,
    meantmv = c(meanCR, meanSR),
    sigmatmv = matrix(c(sdCR^2,
                        0.7 * sdCR * sdSR,
                        0.7 * sdCR * sdSR,
                        sdSR^2), nrow = 2),
    lower = lower,
    upper = upper
  )

  res_sum = sum.meth(res,
                     true1  = true1,
                     true2  = true2)

  res_sum$distribution = distribution
  res_sum$meanCR       = meanCR
  res_sum$meanSR       = meanSR
  res_sum$target       = target

  return(res_sum)
}

######################
# conditions grid ####
######################

data_grid = expand.grid(
  #seed = 1:10000, # Irene: to be extended to 5000 or 10^4 these are the MC reps
  distribution = c("uniform", "normal", "tmvn"),
  target = c(0.10, 0.20, 0.30),
  stringsAsFactors = FALSE
)

uniform_tmvn = subset(data_grid, distribution != "normal")
uniform_tmvn$meanCR = ifelse(uniform_tmvn$distribution == "uniform", 0, 1)
# i set meanCR for multivariate normal to 1 to reflect that 'smaller' studies
# have been deleted

uniform_tmvn$sdCR = ifelse(uniform_tmvn$distribution == "uniform", 0, 6)
uniform_tmvn$meanSR = ifelse(uniform_tmvn$distribution == "uniform", 0, 5)
# i set meanCR for multivariate normal to 5 to reflect that 'bigger' studies
# have been deleted

uniform_tmvn$sdSR = ifelse(uniform_tmvn$distribution == "uniform", 0, 6)

normeans = c(0, 3, -3)
normal_grid = do.call(rbind, lapply(normeans, function(m) {
  g = subset(data_grid, distribution == "normal")
  g$meanCR = m
  g$meanSR = m
  g$sdCR = 6
  g$sdSR = 6
  g
}))

grid = rbind(uniform_tmvn, normal_grid)
head(grid)

##########################
# Run full simulation ####
##########################

minCR = -100; maxCR = 100
minSR = -100; maxSR = 100
lower = c(-Inf, -Inf)
upper = c( Inf,  Inf)
iter  = 100
true1 = 3; true2 = 3
nsim = 1e4

NC <- 75 # number of cores
grid_final = grid[rep(1:nrow(grid), each = NC / nrow(grid)), ]
rownames(grid_final) = NULL
grid_final$nsim = nsim/(NC / nrow(grid))

s_sim <- purrr::possibly(sim, otherwise = NULL)

rep_sim <- function(nsim = 1, ...){
    args <- list(...)
    replicate(nsim, do.call(s_sim, args), simplify = FALSE)
}

# tictoc::tic()# tictoc::ticsim()
# rep_sim(
#     nsim = 1,
#     target = 0.1,
#     meanCR = 3,
#     meanSR = 3,
#     sdCR = 6,
#     sdSR = 6,
#     iter = 100,
#     distribution = "normal",
#     true1 = 3,
#     true2 = 3
# )
# tictoc::toc()

# grid_temp <- grid_final[1, ]
# grid_temp$nsim <- 5
#grid_final$nsim <- 1

plan(multicore(workers = 75))

res <- future_pmap(
    grid_final,
    ~ rep_sim(
        nsim = 5,
        target = ..2,
        meanCR = ..3,
        meanSR = ..5,
        sdCR = ..4,
        sdSR = ..6,
        iter = iter,
        distribution = ..1,
        true1 = true1,
        true2 = true2,
        minCR = minCR,
        maxCR = maxCR,
        minSR = minSR,
        maxSR = maxSR,
        lower = lower,
        upper = upper
    ),
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
)

res <- lapply(res, function(x) do.call(rbind, x))

grid_final <- tibble(grid_final)
grid_final$res <- res

saveRDS(grid_final, "sim.rds")
