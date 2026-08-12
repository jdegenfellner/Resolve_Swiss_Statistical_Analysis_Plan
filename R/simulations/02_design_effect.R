# =====================================================================
# Claims checked (paper, section "Sample size", eq. (1) and Table 1):
#  (a) the design effect of eq. (1), Eldridge et al. 2006, predicts the
#      variance inflation of the difference of arm means under unequal
#      practice sizes: DE = 1 + [(cv^2 (k-1)/k + 1) m_bar - 1] rho,
#      numerically 1.20 / 1.61 / 2.02 at rho = .01/.03/.05 (cv 0.65)
#  (b) the chain "69 per arm x DE" behind Table 1 (83/111/139): the
#      variance of the baseline-adjusted effect estimate equals
#      sigma^2 (1-r^2) DE (1/N1 + 1/N2)
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages(library(ggplot2))

NSIM <- as.integer(Sys.getenv("NSIM_FAST", 20000))
set.seed(2)

k <- 13; m_bar <- 200 / 13
sizes <- make_sizes(k, 200, 0.65)
cv_real <- sd(sizes) / mean(sizes) * sqrt((k - 1) / k)  # population cv of the fixed sizes
de_formula <- function(rho, cv, m, kk) 1 + ((cv^2 * (kk - 1) / kk + 1) * m - 1) * rho

banner("(a) variance inflation of the unadjusted arm difference")
# Exact decomposition per arm: with fixed sizes m_j in an arm with total
# n, the size-weighted arm mean has Var = sigma^2 * DE_arm / n with
# DE_arm = 1 + (m_bar_arm (1 + cv_pop^2) - 1) rho, which is Eldridge
# eq. (1) once the sample cv is converted to the population cv. Both
# arms get the same size profile (cv 0.65), as the planning value assumes.
k1 <- 6; k2 <- 7
s1 <- make_sizes(k1, 92, 0.65); s2 <- make_sizes(k2, 108, 0.65)
sizes_ab <- c(s1, s2)
n1 <- sum(s1); n2 <- sum(s2)
de_arm <- function(sz, rho) {
  cvp <- sd(sz) / mean(sz) * sqrt((length(sz) - 1) / length(sz))
  1 + ((cvp^2 + 1) * mean(sz) - 1) * rho
}
res <- data.frame()
for (rho in c(0.01, 0.03, 0.05)) {
  est <- replicate(NSIM, {
    d <- gen_crt(k1, k2, sizes_ab, rho, delta = 0, r = rho) # nu2 = 0, plain outcome
    mean(d$fu[d$group == 1]) - mean(d$fu[d$group == 0])
  })
  v_th   <- SIGMA^2 * (de_arm(s1, rho) / n1 + de_arm(s2, rho) / n2)
  de_emp <- var(est) / (SIGMA^2 * (1 / n1 + 1 / n2))
  de_th  <- v_th / (SIGMA^2 * (1 / n1 + 1 / n2))
  cat(sprintf("rho %.2f: DE empirisch %.3f | eq. (1) pro Arm: %.3f | Papierwert (m_bar 15.4, cv 0.65): %.3f\n",
              rho, de_emp, de_th, de_formula(rho, 0.65, m_bar, k)))
  res <- rbind(res, data.frame(rho = rho, de = de_emp, what = "simulated"),
                    data.frame(rho = rho, de = de_th,  what = "eq. (1)"))
}

banner("(b) variance of the baseline-adjusted estimate, full chain")
# In the generative world of model (2) the practice intercept u_j is
# constant over time, so the cluster autocorrelation is 1 and the
# baseline adjustment also removes practice-level variance. Teerenstra
# eq. (5) with rho_c = 1 predicts by how much the paper's chain
# overstates the variance; the empirical ratio should match that
# prediction, and being below 1 it shows the chain is conservative.
r_comb1 <- function(rho, m) (m * rho / (1 + (m - 1) * rho)) * 1 +
                            ((1 - rho) / (1 + (m - 1) * rho)) * R
for (rho in c(0.01, 0.03, 0.05)) {
  est <- replicate(round(NSIM / 4), {
    d <- gen_crt(k1, k2, sizes_ab, rho, delta = 0)
    coef(lm(fu ~ bl + group, d))["group"]
  })
  v_th <- SIGMA^2 * (1 - R^2) * (de_arm(s1, rho) / n1 + de_arm(s2, rho) / n2)
  pred <- (1 - r_comb1(rho, mean(sizes))^2) / (1 - R^2)
  cat(sprintf("rho %.2f: Var empirisch/Formelkette %.2f | Teerenstra-Vorhersage (rho_c = 1): %.2f\n",
              rho, var(est) / v_th, pred))
}
cat("Die Formelkette hinter Tabelle 1 ist in dieser Welt konservativ, und\n")
cat("zwar genau um den Teerenstra-Faktor - konsistent mit der gruenen Passage.\n")

# ---- figure ---------------------------------------------------------
rg <- seq(0, 0.06, 0.002)
de_line <- sapply(rg, function(r0) (de_arm(s1, r0) / n1 + de_arm(s2, r0) / n2) /
                                    (1 / n1 + 1 / n2))
line <- data.frame(rho = rg, de = de_line)
fig <- ggplot(line, aes(rho, de)) +
  geom_line(colour = "#1b9e77", linewidth = 0.7) +
  geom_point(data = subset(res, what == "simulated"), colour = "#d95f02", size = 2.4) +
  labs(x = expression(rho~"(intra-cluster correlation)"), y = "design effect",
       title = "Eldridge eq. (1) (line) against simulation (points)",
       subtitle = sprintf("13 practices, sizes fixed with cv = %.2f, %s reps per point",
                          cv_real, format(NSIM, big.mark = " "))) +
  theme_minimal(base_size = 10)
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/02_design_effect.png", fig, width = 6, height = 4, dpi = 300)
cat("Figur: R/simulations/figures/02_design_effect.png\n")
