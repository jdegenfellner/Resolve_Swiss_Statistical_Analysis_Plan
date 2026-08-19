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
m_bar    <- 200 / 15  # analysed participants per practice at ~200 total / 15 practices
# Eldridge et al. (2006) report that unequal cluster size can be ignored when
# cv < 0.23, and that for trials randomising UK general practices cv is
# typically around 0.65. Physiotherapy practices are the closest analogue we
# have, so 0.65 is the primary planning value and 0.4 a more optimistic one.
cv_grid  <- c(0, 0.4, 0.65)

de_tab <- outer(rho_grid, cv_grid,
                Vectorize(function(rho, cv) design_effect(m_bar, rho, cv, k = 15)))
dimnames(de_tab) <- list(paste0("ICC=", rho_grid), paste0("cv=", cv_grid))
out("\nDesign effects (m_bar = ", round(m_bar, 1), ", k = 15):")
print(round(de_tab, 3))

# Requirements are minima, so round up. Use the unrounded benchmark
# (68.8, before its own ceiling) times the design effect, then ceiling.
# These are normal-approximation numbers; van Breukelen & Candel (2018)
# add 2-3 practices per arm for the z-to-t power loss, and section 4
# below applies that correction exactly via the noncentral t.
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
    ceiling(n_r05 * design_effect(m_bar, 0.01, 0.65, k = 15)), " per arm")
out("With cv = 0 instead of 0.65, ICC 0.01, r = 0.6:  ",
    ceiling(n_exact * design_effect(m_bar, 0.01, 0, k = 15)), " per arm")

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
# (Hayes & Moulton 2017, section 10.3.1, eqs. (10.2)-(10.3): compared
# with the t distribution with c_1 + c_0 - 2 df; ch. 5, eq. (5.1) gives
# the equal-arm special case 2(c-1)).

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
  rho    = c(0.01, 0.02, 0.03, 0.05),
  KEEP.OUT.ATTRS = FALSE
)
scenarios$k_ctrl <- 15 - scenarios$k_int

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

# Balanced comparison: same 15 clusters, split 7/8 instead of 6/9
bal <- sapply(c(0.01, 0.02, 0.03, 0.05), function(rho)
  power_crt(7, 8, m = 200 / 15, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
unb <- sapply(c(0.01, 0.02, 0.03, 0.05), function(rho)
  power_crt(6, 9, m = 200 / 15, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
out("\n15 clusters, ~200 participants — cost of the unbalanced split:")
print(data.frame(ICC = c(0.01, 0.02, 0.03, 0.05),
                 power_6_9 = round(unb, 3),
                 power_7_8 = round(bal, 3)))

# ---------------------------------------------------------------------
# 5. Recruiting additional practices, holding the analysed sample at 200
# ---------------------------------------------------------------------
# The trial is committed to 200 analysed participants, so the relevant
# question is not "more patients or more practices" but how the same
# participants are best distributed. Spreading them over more practices
# lowers the design effect (smaller m) and raises the degrees of freedom.

add <- expand.grid(k_per_arm = 5:10, rho = c(0.01, 0.02, 0.03, 0.05))
add$m <- 100 / add$k_per_arm
add$power <- mapply(power_crt, k1 = add$k_per_arm, k2 = add$k_per_arm,
                    m = add$m, rho = add$rho,
                    MoreArgs = list(sigma = sigma, delta = delta,
                                    ancova_r = r, cv = 0.65))
add$m <- round(add$m, 1)
out("\nBalanced designs, analysed N held at 200:")
print(transform(add, power = round(power, 3)))

# Imbalanced: the intervention arm capped at 7 practices while control
# practices are added. The arm with fewer practices dominates 1/k1 + 1/k2,
# so the extra control practices buy little.
imb <- expand.grid(k_ctrl = 7:12, rho = c(0.01, 0.02, 0.03, 0.05))
imb$k_int <- 7
imb$m <- 200 / (imb$k_int + imb$k_ctrl)
imb$power <- mapply(power_crt, k1 = imb$k_int, k2 = imb$k_ctrl,
                    m = imb$m, rho = imb$rho,
                    MoreArgs = list(sigma = sigma, delta = delta,
                                    ancova_r = r, cv = 0.65))
imb$m <- round(imb$m, 1)
out("\nIntervention arm capped at 7 practices, analysed N held at 200:")
print(transform(imb[c("k_int", "k_ctrl", "rho", "m", "power")],
                power = round(power, 3)))


# Recruiting seven further practices per arm from the current 6/9,
# analysed sample still 200: 13 vs 16 practices.
p1316 <- sapply(c(0.01, 0.02, 0.03, 0.05), function(rho)
  power_crt(13, 16, m = 200 / 29, sigma = sigma, delta = delta, rho = rho,
            ancova_r = r, cv = 0.65))
out("\nSeven further practices per arm (13 vs 16), analysed N = 200:")
print(data.frame(ICC = c(0.01, 0.02, 0.03, 0.05), power = round(p1316, 3)))

# Number of distinct allocations of k_int of 13 practices to the
# intervention arm; this is the size of the exact randomisation
# distribution used for the permutation test.
out("\nDistinct allocations, 6 of 15 practices: ", choose(15, 6))
out("Smallest attainable two-sided p value:  ", signif(2 / choose(15, 6), 3))
out("Distinct allocations, 7 of 15 practices: ", choose(15, 7))

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

# ---------------------------------------------------------------------
# 7. Session information
# ---------------------------------------------------------------------
out("\n")
print(sessionInfo())
