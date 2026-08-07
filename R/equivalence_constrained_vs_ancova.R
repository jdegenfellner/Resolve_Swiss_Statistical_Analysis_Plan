# =====================================================================
# RESOLVE Swiss — empirical demonstration:
# the constrained longitudinal mixed model (cLDA, no main effect of
# treatment, baseline in the outcome vector) matches baseline-adjusted
# analysis of covariance for the 18-week treatment effect, while the
# unconstrained model (with a treatment main effect) is a change-score
# analysis and pays the precision penalty 2(1-r) vs (1-r^2).
#
# At r = 0.6 the predicted SE ratio unconstrained/constrained is
# sqrt(2(1-r)/(1-r^2)) = sqrt(2/(1+r)) = 1.118.
#
# Needs: lme4. Run time ~1 min at n_sim = 300.
# =====================================================================

library(lme4)

set.seed(2026)

n_sim   <- 300
k_arm   <- 7      # practices per arm
m       <- 14     # participants per practice
r       <- 0.6    # baseline-to-follow-up correlation
icc     <- 0.02   # practice-level ICC
sd_tot  <- 5.2    # between-participant SD of RMDQ
delta   <- 2      # true treatment effect at 18 weeks

tau   <- sqrt(icc) * sd_tot            # practice SD
sd_w  <- sqrt(1 - icc) * sd_tot        # within-practice SD

simulate_trial <- function() {
  practice <- rep(seq_len(2 * k_arm), each = m)
  group    <- rep(c(0, 1), each = k_arm * m)
  u        <- rnorm(2 * k_arm, 0, tau)[practice]
  # bivariate (baseline, follow-up) with correlation r within participant
  z0  <- rnorm(2 * k_arm * m)
  z1  <- r * z0 + sqrt(1 - r^2) * rnorm(2 * k_arm * m)
  y0  <- 10 + u + sd_w * z0
  y18 <- 8  + u + sd_w * z1 - delta * group
  data.frame(id = seq_along(y0), practice = factor(practice),
             group = group, y0 = y0, y18 = y18)
}

fit_all <- function(d) {
  # (a) baseline-adjusted ANCOVA-type mixed model
  a <- lmer(y18 ~ y0 + group + (1 | practice), data = d, REML = TRUE)

  # long format for the longitudinal models
  dl <- data.frame(
    id       = factor(rep(d$id, 2)),
    practice = rep(d$practice, 2),
    group    = rep(d$group, 2),
    time     = rep(c(0, 1), each = nrow(d)),
    y        = c(d$y0, d$y18)
  )

  # (b) constrained: no main effect of treatment (SAP primary model)
  b <- lmer(y ~ time + time:group + (1 | practice) + (1 | id),
            data = dl, REML = TRUE)

  # (c) unconstrained: protocol wording, group + time + group:time
  c_ <- lmer(y ~ group * time + (1 | practice) + (1 | id),
             data = dl, REML = TRUE)

  get <- function(fit, term) {
    cf <- summary(fit)$coefficients
    cf[term, c("Estimate", "Std. Error")]
  }
  c(ancova = get(a, "group"),
    cLDA   = get(b, "time:group"),
    unconstr = get(c_, "group:time"))
}

res <- t(replicate(n_sim, {
  d <- simulate_trial()
  suppressMessages(fit_all(d))
}))
colnames(res) <- c("est_ancova", "se_ancova", "est_cLDA", "se_cLDA",
                   "est_unconstr", "se_unconstr")
res <- as.data.frame(res)

cat("Mean estimates (truth = -2):\n")
print(round(colMeans(res[c("est_ancova", "est_cLDA", "est_unconstr")]), 3))

cat("\nMean model SEs:\n")
print(round(colMeans(res[c("se_ancova", "se_cLDA", "se_unconstr")]), 3))

cat("\nEmpirical SDs of the estimates:\n")
print(round(apply(res[c("est_ancova", "est_cLDA", "est_unconstr")], 2, sd), 3))

cat("\nPer-dataset agreement ANCOVA vs constrained:\n")
cat("  correlation of estimates:",
    round(cor(res$est_ancova, res$est_cLDA), 4), "\n")
cat("  mean |difference|:",
    signif(mean(abs(res$est_ancova - res$est_cLDA)), 3), "\n")

cat("\nSE ratio unconstrained / constrained (theory 1.118 at r = 0.6):",
    round(mean(res$se_unconstr) / mean(res$se_cLDA), 3), "\n")
