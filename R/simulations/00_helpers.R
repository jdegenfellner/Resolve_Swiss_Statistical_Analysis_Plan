# =====================================================================
# Shared helpers for the claim-by-claim simulations.
#
# Data are generated from the world of the primary model, eq. (2) of
# the paper: y_ijt = beta0 + beta1*s_t + beta2*s_t*x_j + u_j + w_ij + e_ijt
# with u_j ~ N(0, tau2) a practice intercept constant over time,
# w_ij ~ N(0, nu2) a participant intercept, e_ijt ~ N(0, sig2e).
# With total variance sigma2 = tau2 + nu2 + sig2e this gives
#   ICC rho              = tau2 / sigma2
#   baseline-follow-up r = (tau2 + nu2) / sigma2
# so tau2 = rho*sigma2, nu2 = (r - rho)*sigma2, sig2e = (1 - r)*sigma2.
# =====================================================================

SIGMA <- sqrt((4.6^2 + 5.8^2) / 2)   # pooled SD 5.234, as in the paper
DELTA <- 2                            # target difference, RMDQ points
R     <- 0.6                          # baseline-follow-up correlation

# fixed practice sizes with mean m_bar and coefficient of variation cv,
# scaled to sum to n_total (deterministic, so the design is fixed)
make_sizes <- function(k, n_total, cv) {
  if (cv == 0) sz <- rep(n_total / k, k)
  else {
    q  <- qgamma((seq_len(k) - 0.5) / k, shape = 1 / cv^2)
    sz <- q / mean(q) * (n_total / k)
  }
  sz <- pmax(2, round(sz))
  while (sum(sz) != n_total) {
    i <- if (sum(sz) > n_total) which.max(sz) else which.min(sz)
    sz[i] <- sz[i] + sign(n_total - sum(sz))
  }
  sz
}

# one cluster randomised data set: k1 intervention and k2 control
# practices, baseline and follow-up per participant. rho_c is the
# cluster autocorrelation: 1 makes the practice effect time-constant
# (the literal world of model (2), where it cancels from the treatment
# contrast), smaller values give the practice effect a time-specific
# part that does not cancel. The cross-sectional ICC stays rho and the
# individual baseline-follow-up correlation stays r in every case.
gen_crt <- function(k1, k2, sizes, rho, delta = DELTA, r = R, sigma = SIGMA,
                    rho_c = 1) {
  k    <- k1 + k2
  arm  <- rep(c(1, 0), c(k1, k2))
  c2    <- rho_c * rho * sigma^2         # practice, constant over time
  ct2   <- (1 - rho_c) * rho * sigma^2   # practice, per occasion
  nu2   <- (r - rho_c * rho) * sigma^2   # participant, constant
  sig2e <- (1 - rho) * sigma^2 - nu2     # occasion-level noise
  stopifnot(nu2 >= 0, sig2e >= 0)
  n   <- sum(sizes)
  pr  <- rep(seq_len(k), sizes)
  uc  <- rnorm(k, 0, sqrt(c2))[pr]
  w   <- rnorm(n, 0, sqrt(nu2))
  gr  <- arm[pr]
  bl  <- uc + rnorm(k, 0, sqrt(ct2))[pr] + w + rnorm(n, 0, sqrt(sig2e))
  fu  <- uc + rnorm(k, 0, sqrt(ct2))[pr] + w + rnorm(n, 0, sqrt(sig2e)) - delta * gr
  data.frame(practice = pr, group = gr, bl = bl, fu = fu)
}

# empirical power/level with its Monte Carlo standard error
mc <- function(rejections, nsim)
  sprintf("%.3f (MC-SE %.3f)", mean(rejections), sd(rejections) / sqrt(nsim))

banner <- function(...) cat("\n---", ..., "---\n")
