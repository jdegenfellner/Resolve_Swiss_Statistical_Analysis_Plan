# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Sample size and power for the cluster randomised design.
#
# Everything reported in the "Sample size" section of the manuscript is
# produced by this script. Run with:  Rscript R/sample_size.R
# No packages beyond base R are required.
# =====================================================================

options(digits = 4)
out <- function(...) cat(..., "\n", sep = "")

# ---------------------------------------------------------------------
# 1. Design inputs
# ---------------------------------------------------------------------
# Primary outcome: Roland-Morris Disability Questionnaire (RMDQ, 0-24)
# at 18 weeks post randomisation.
#
# sigma  : between-participant SD of RMDQ at 18 weeks. Taken from the
#          Australian RESOLVE trial (Bagg et al. 2022, Table 2):
#          intervention 3.6 (SD 4.6), control 6.4 (SD 5.8).
# delta  : target difference in RMDQ points (between-group).
# rho    : intra-cluster correlation at the practice level.
# m      : average number of analysed participants per practice.
# r      : correlation between baseline and 18-week RMDQ, used for the
#          ANCOVA variance reduction factor (1 - r^2).

sd_int  <- 4.6
sd_ctrl <- 5.8
sigma   <- sqrt((sd_int^2 + sd_ctrl^2) / 2)          # pooled SD
delta   <- 2                                          # RMDQ points
r       <- 0.6                                        # baseline-follow-up correlation
alpha   <- 0.05                                       # two-sided
power_t <- 0.80

out("Pooled SD of RMDQ at 18 weeks: ", round(sigma, 2))

# ---------------------------------------------------------------------
# 2. Individually randomised benchmark (per arm)
# ---------------------------------------------------------------------
n_per_arm <- function(sigma, delta, alpha = 0.05, power = 0.80,
                      ancova_r = 0) {
  z_a <- qnorm(1 - alpha / 2)
  z_b <- qnorm(power)
  n <- 2 * (z_a + z_b)^2 * sigma^2 * (1 - ancova_r^2) / delta^2
  ceiling(n)
}

n_unadj  <- n_per_arm(sigma, delta)
n_ancova <- n_per_arm(sigma, delta, ancova_r = r)
out("Individually randomised, unadjusted analysis: n = ", n_unadj, " per arm")
out("Individually randomised, ANCOVA (r = ", r, "):     n = ", n_ancova, " per arm")

# ---------------------------------------------------------------------
# 3. Design effect for clustering, allowing unequal cluster sizes
# ---------------------------------------------------------------------
# Eldridge, Ashby & Kerry (2006), Int J Epidemiol 35:1292-1300, eq. (1):
#   DE = 1 + ((cv^2 * (k - 1) / k + 1) * m_bar - 1) * rho
# with cv = SD(cluster size) / mean(cluster size). cv = 0 recovers the
# classical DE = 1 + (m_bar - 1) * rho.

design_effect <- function(m_bar, rho, cv = 0, k = Inf) {
  fac <- if (is.finite(k)) cv^2 * (k - 1) / k + 1 else cv^2 + 1
  1 + (fac * m_bar - 1) * rho
}

rho_grid <- c(0, 0.01, 0.02, 0.03, 0.05)
m_bar    <- 14        # analysed participants per practice at ~200 total / 14 practices
# Eldridge et al. (2006) report that unequal cluster size can be ignored when
# cv < 0.23, and that for trials randomising UK general practices cv is
# typically around 0.65. Physiotherapy practices are the closest analogue we
# have, so 0.65 is the primary planning value and 0.4 a more optimistic one.
cv_grid  <- c(0, 0.4, 0.65)

de_tab <- outer(rho_grid, cv_grid,
                Vectorize(function(rho, cv) design_effect(m_bar, rho, cv, k = 14)))
dimnames(de_tab) <- list(paste0("ICC=", rho_grid), paste0("cv=", cv_grid))
out("\nDesign effects (m_bar = ", m_bar, ", k = 14):")
print(round(de_tab, 3))

out("\nRequired n per arm = ANCOVA benchmark x design effect:")
print(round(n_ancova * de_tab, 0))

# ---------------------------------------------------------------------
# 4. Power of the design as it currently stands
# ---------------------------------------------------------------------
# Variance of the difference of two arm means in a cluster randomised
# trial with k_j clusters of average size m in arm j:
#   Var = sigma^2 * [1 + (m - 1) * rho] * (1/(m*k_1) + 1/(m*k_2))
# Power uses a t reference distribution with k_1 + k_2 - 2 degrees of
# freedom, which is the relevant approximation when the number of
# clusters is small (Hayes & Moulton 2017, ch. 7).

power_crt <- function(k1, k2, m, sigma, delta, rho, alpha = 0.05,
                      ancova_r = 0, cv = 0) {
  sig2 <- sigma^2 * (1 - ancova_r^2)
  de_m <- if (cv > 0) (cv^2 + 1) * m else m       # inflation from unequal sizes
  var_d <- sig2 * (1 + (de_m - 1) * rho) * (1 / (m * k1) + 1 / (m * k2))
  se <- sqrt(var_d)
  df <- k1 + k2 - 2
  tcrit <- qt(1 - alpha / 2, df)
  ncp <- delta / se
  pt(-tcrit, df, ncp) + (1 - pt(tcrit, df, ncp))
}

scenarios <- expand.grid(
  k_int  = c(5, 6, 7, 8, 9),
  rho    = c(0.01, 0.03, 0.05),
  KEEP.OUT.ATTRS = FALSE
)
scenarios$k_ctrl <- ifelse(scenarios$k_int <= 5, 9, 14 - scenarios$k_int)
scenarios$k_ctrl <- pmax(scenarios$k_ctrl, 5)

# Hold the total analysed sample at 200 (100 per arm) and let the number
# of participants per practice absorb the change in cluster numbers.
scenarios$m <- round(200 / (scenarios$k_int + scenarios$k_ctrl))
scenarios$power <- mapply(power_crt,
                          k1 = scenarios$k_int, k2 = scenarios$k_ctrl,
                          m = scenarios$m, rho = scenarios$rho,
                          MoreArgs = list(sigma = sigma, delta = delta,
                                          ancova_r = r, cv = 0.65))
out("\nPower at total analysed N = 200, ANCOVA, cv = 0.65:")
print(transform(scenarios, power = round(power, 3)))

# Balanced comparison: same 14 clusters, split 7/7 instead of 5/9
bal <- sapply(c(0.01, 0.03, 0.05), function(rho)
  power_crt(7, 7, m = 14, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
unb <- sapply(c(0.01, 0.03, 0.05), function(rho)
  power_crt(5, 9, m = 14, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
out("\n14 clusters, 200 participants — cost of the unbalanced split:")
print(data.frame(ICC = c(0.01, 0.03, 0.05),
                 power_5_9 = round(unb, 3),
                 power_7_7 = round(bal, 3)))

# ---------------------------------------------------------------------
# 5. Recruiting additional practices, holding practice size at m = 14
# ---------------------------------------------------------------------
# Here the total sample grows with the number of practices. This is the
# comparison that matters for the decision "recruit more practices" vs
# "recruit more patients in the practices we have".

add <- expand.grid(k_per_arm = 5:10, rho = c(0.01, 0.03, 0.05))
add$N_total <- 2 * add$k_per_arm * 14
add$power <- mapply(power_crt, k1 = add$k_per_arm, k2 = add$k_per_arm,
                    rho = add$rho,
                    MoreArgs = list(m = 14, sigma = sigma, delta = delta,
                                    ancova_r = r, cv = 0.65))
out("\nBalanced designs with 14 participants per practice:")
print(transform(add, power = round(power, 3)))

# Number of distinct allocations of k_int of 14 practices to the
# intervention arm; this is the size of the exact randomisation
# distribution used for the permutation test.
out("\nDistinct allocations, 5 of 14 practices: ", choose(14, 5))
out("Smallest attainable two-sided p value:  ", signif(2 / choose(14, 5), 3))
out("Distinct allocations, 7 of 14 practices: ", choose(14, 7))

# ---------------------------------------------------------------------
# 6. Session information
# ---------------------------------------------------------------------
out("\n")
print(sessionInfo())
