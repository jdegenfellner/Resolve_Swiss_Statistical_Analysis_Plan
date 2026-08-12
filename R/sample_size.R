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
# at 18 weeks after the baseline measurement.
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
# Two-sample test for equality, equal allocation: Chow, Shao, Wang &
# Lokhnygina (2018), Sample Size Calculations in Clinical Research,
# 3rd ed., section 3.2.1, eq. (3.11) with kappa = 1:
#   n = (z_{1-alpha/2} + z_{1-beta})^2 * 2 * sigma^2 / delta^2
# ANCOVA design factor (1 - r^2): Borm, Fransen & Lemmens (2007),
# J Clin Epidemiol 60:1234-1238 (ANCOVA with (1 - r^2) * n subjects has
# the same power as the unadjusted comparison with n).
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
m_bar    <- 200 / 13  # analysed participants per practice at ~200 total / 13 practices
# Eldridge et al. (2006) report that unequal cluster size can be ignored when
# cv < 0.23, and that for trials randomising UK general practices cv is
# typically around 0.65. Physiotherapy practices are the closest analogue we
# have, so 0.65 is the primary planning value and 0.4 a more optimistic one.
cv_grid  <- c(0, 0.4, 0.65)

de_tab <- outer(rho_grid, cv_grid,
                Vectorize(function(rho, cv) design_effect(m_bar, rho, cv, k = 13)))
dimnames(de_tab) <- list(paste0("ICC=", rho_grid), paste0("cv=", cv_grid))
out("\nDesign effects (m_bar = ", round(m_bar, 1), ", k = 13):")
print(round(de_tab, 3))

# Requirements are minima, so round up. Use the unrounded benchmark
# (68.8, before its own ceiling) times the design effect, then ceiling.
n_exact <- 2 * (qnorm(1 - alpha / 2) + qnorm(power_t))^2 *
  sigma^2 * (1 - r^2) / delta^2
req_tab <- ceiling(n_exact * de_tab)
out("\nRequired n per arm = ANCOVA benchmark x design effect (rounded up):")
print(req_tab)
out("\nRecruited per arm at 10% loss to follow-up (rounded up):")
print(ceiling(req_tab / 0.9))

# sensitivity of the inputs, quoted in the manuscript: baseline
# correlation 0.5 instead of 0.6 at ICC 0.03, and cv 0 instead of 0.65
n_r05 <- 2 * (qnorm(1 - alpha / 2) + qnorm(power_t))^2 *
  sigma^2 * (1 - 0.5^2) / delta^2
out("\nWith r = 0.5 instead of 0.6, ICC 0.01, cv 0.65: ",
    ceiling(n_r05 * design_effect(m_bar, 0.01, 0.65, k = 13)), " per arm")
out("With cv = 0 instead of 0.65, ICC 0.03, r = 0.6:  ",
    ceiling(n_exact * design_effect(m_bar, 0.03, 0, k = 13)), " per arm")

# ---------------------------------------------------------------------
# 4. Power of the design as it currently stands
# ---------------------------------------------------------------------
# Variance of the difference of two arm means in a cluster randomised
# trial with k_j clusters of average size m in arm j:
#   Var = sigma^2 * [1 + (m - 1) * rho] * (1/(m*k_1) + 1/(m*k_2))
# This is Hayes & Moulton (2017), Cluster Randomised Trials, 2nd ed.,
# eq. (7.12) rearranged (their c = 1 + (z_{a/2}+z_b)^2 (s0^2+s1^2)
# [1+(m-1)rho] / (m d^2), the "+1" being their allowance for the t-test),
# with the per-arm terms added separately for unequal numbers of
# clusters as in their section 7.6.2. The unequal-cluster-size inflation
# (cv^2+1)*m below is Eldridge et al. (2006), eq. (2), a slight
# overestimate of the design effect and hence conservative.
# Instead of adding one cluster per arm we evaluate power exactly from
# the noncentral t distribution (Chow et al. 2018, section 3.2.1) with
# k_1 + k_2 - 2 degrees of freedom, the df of the cluster-level t test
# (Hayes & Moulton 2017, eq. (5.1): df = 2(c-1) for equal arms).

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
  k_int  = c(4, 5, 6),
  rho    = c(0.01, 0.03, 0.05),
  KEEP.OUT.ATTRS = FALSE
)
scenarios$k_ctrl <- 13 - scenarios$k_int

# Hold the total analysed sample at 200 (100 per arm) and let the number
# of participants per practice absorb the change in cluster numbers.
# m is the average practice size and may be fractional; the variance
# formula only uses m through the design effect and the arm totals m*k.
scenarios$m <- 200 / (scenarios$k_int + scenarios$k_ctrl)
scenarios$power <- mapply(power_crt,
                          k1 = scenarios$k_int, k2 = scenarios$k_ctrl,
                          m = scenarios$m, rho = scenarios$rho,
                          MoreArgs = list(sigma = sigma, delta = delta,
                                          ancova_r = r, cv = 0.65))
out("\nPower at total analysed N = 200, ANCOVA, cv = 0.65:")
print(transform(scenarios, power = round(power, 3)))

# Balanced comparison: same 13 clusters, split 6/7 instead of 4/9
bal <- sapply(c(0.01, 0.03, 0.05), function(rho)
  power_crt(6, 7, m = 15, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
unb <- sapply(c(0.01, 0.03, 0.05), function(rho)
  power_crt(4, 9, m = 15, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
out("\n13 clusters, ~200 participants — cost of the unbalanced split:")
print(data.frame(ICC = c(0.01, 0.03, 0.05),
                 power_4_9 = round(unb, 3),
                 power_6_7 = round(bal, 3)))

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

# Number of distinct allocations of k_int of 13 practices to the
# intervention arm; this is the size of the exact randomisation
# distribution used for the permutation test.
out("\nDistinct allocations, 4 of 13 practices: ", choose(13, 4))
out("Smallest attainable two-sided p value:  ", signif(2 / choose(13, 4), 3))
out("Distinct allocations, 6 of 13 practices: ", choose(13, 6))

# ---------------------------------------------------------------------
# 6. Session information
# ---------------------------------------------------------------------
out("\n")
print(sessionInfo())

# ---------------------------------------------------------------------
# 6. How far off is the factorised approximation (1 - r^2) x DE?
# ---------------------------------------------------------------------
# Teerenstra et al. (2012), Stat Med 31:2169-2178, eq. (5) and (7):
# the exact ANCOVA design effect for a cluster randomised trial is
# (1 - r_comb^2) * [1 + (n - 1) * rho], where r_comb is a weighted
# average of the cluster autocorrelation rho_c and the subject
# autocorrelation rho_s:
#   r_comb = w * rho_c + (1 - w) * rho_s,  w = n*rho / (1 + (n-1)*rho).
# Our calculation uses rho_s alone (r = 0.6). The requirement ratio
# Teerenstra / ours is therefore (1 - r_comb^2) / (1 - rho_s^2):
# ratio 1 when rho_c = rho_s, ours conservative when rho_c > rho_s,
# optimistic when rho_c < rho_s.

out("\nRequirement ratio Teerenstra / factorised version (rho_s = 0.6):")
teer <- expand.grid(rho = c(0.01, 0.03, 0.05), rho_c = c(0.2, 0.4, 0.6, 0.8))
w <- m_bar * teer$rho / (1 + (m_bar - 1) * teer$rho)
r_comb <- w * teer$rho_c + (1 - w) * r
teer$ratio <- round((1 - r_comb^2) / (1 - r^2), 3)
print(reshape(teer, idvar = "rho", timevar = "rho_c", direction = "wide"))
