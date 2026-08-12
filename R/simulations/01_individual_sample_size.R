# =====================================================================
# Claims checked (paper, section "Sample size"):
#  (a) "an individually randomised trial would need 108 participants
#      per arm" (two-sided alpha 0.05, power 80%, sigma 5.234, delta 2)
#  (b) "using the baseline measurement ... reduces this by the factor
#      1 - r^2 to 69 per arm" (r = 0.6)
#  (c) analysing change scores is less precise than ANCOVA whenever
#      baseline and follow-up are positively correlated; at r = 0.6 the
#      theoretical SE ratio is sqrt(2/(1+r)) = 1.118
#  (d) a margin design (H0: delta <= 2, true effect 2.4, one-sided
#      alpha 0.05) "would require well over a thousand participants per
#      arm even with baseline adjustment" (formula: about 1355)
# =====================================================================
# run from the repository root, e.g. Rscript R/simulations/01_...
source("R/simulations/00_helpers.R")

NSIM <- as.integer(Sys.getenv("NSIM_LM", 4000))
set.seed(1)

# bivariate baseline/follow-up for individually randomised participants
gen_ind <- function(n_per_arm, delta, r = R, sigma = SIGMA) {
  n  <- 2 * n_per_arm
  gr <- rep(c(1, 0), each = n_per_arm)
  w  <- rnorm(n, 0, sigma * sqrt(r))       # shared component
  bl <- w + rnorm(n, 0, sigma * sqrt(1 - r))
  fu <- w + rnorm(n, 0, sigma * sqrt(1 - r)) - delta * gr
  data.frame(gr, bl, fu)
}

banner("(a) t-test, n = 108 per arm")
rej <- replicate(NSIM, {
  d <- gen_ind(108, DELTA)
  t.test(fu ~ gr, d, var.equal = TRUE)$p.value < 0.05
})
cat("empirical power:", mc(rej, NSIM), "| claim: 0.80\n")

banner("(b) ANCOVA, n = 69 per arm, r = 0.6")
p_anc <- function(d) summary(lm(fu ~ bl + gr, d))$coefficients["gr", 4]
rej <- replicate(NSIM, p_anc(gen_ind(69, DELTA)) < 0.05)
cat("empirical power:", mc(rej, NSIM), "| claim: 0.80\n")

banner("(c) precision: change score vs ANCOVA at r = 0.6")
est <- t(replicate(NSIM, {
  d <- gen_ind(100, 0)
  c(anc = coef(lm(fu ~ bl + gr, d))["gr"],
    chg = coef(lm(I(fu - bl) ~ gr, d))["gr"])
}))
ratio <- sd(est[, 2]) / sd(est[, 1])
cat(sprintf("empirical SE ratio change/ANCOVA: %.3f | theory sqrt(2/(1+r)) = %.3f\n",
            ratio, sqrt(2 / (1 + R))))

banner("(d) margin design, H0: delta <= 2, true 2.4, one-sided, ANCOVA")
n_marg <- 1355
rej <- replicate(round(NSIM / 2), {
  d <- gen_ind(n_marg, 2.4)
  f <- lm(fu ~ bl + gr, d); s <- summary(f)$coefficients["gr", ]
  (-s["Estimate"] - 2) / s["Std. Error"] > qnorm(0.95)   # benefit beyond 2
})
cat("empirical power at n = 1355/arm:", mc(rej, round(NSIM / 2)),
    "| claim: about 0.80, i.e. well over a thousand per arm\n")

# ---- figure: power vs n for the three designs -----------------------
suppressMessages(library(ggplot2))
ns <- seq(40, 160, 10)
pw <- function(n, adjust) {
  se <- SIGMA * sqrt((1 - adjust * R^2) * 2 / n)
  power <- pnorm(DELTA / se - qnorm(0.975))
  power
}
pd <- rbind(
  data.frame(n = ns, power = pw(ns, 0), what = "unadjusted (formula)"),
  data.frame(n = ns, power = pw(ns, 1), what = "ANCOVA, r = 0.6 (formula)"))
emp <- do.call(rbind, lapply(c(60, 80, 100, 120), function(n) {
  ra <- mean(replicate(1500, p_anc(gen_ind(n, DELTA)) < 0.05))
  rt <- mean(replicate(1500, t.test(fu ~ gr, gen_ind(n, DELTA),
                                    var.equal = TRUE)$p.value < 0.05))
  rbind(data.frame(n = n, power = ra, what = "ANCOVA, r = 0.6 (simulated)"),
        data.frame(n = n, power = rt, what = "unadjusted (simulated)"))
}))
fig <- ggplot(pd, aes(n, power, colour = what)) +
  geom_line(linewidth = 0.7) +
  geom_point(data = emp, size = 2) +
  geom_hline(yintercept = 0.8, linetype = 3) +
  geom_vline(xintercept = c(69, 108), linetype = 3) +
  scale_colour_manual(values = c("unadjusted (formula)" = "#7570b3",
                                 "unadjusted (simulated)" = "#7570b3",
                                 "ANCOVA, r = 0.6 (formula)" = "#d95f02",
                                 "ANCOVA, r = 0.6 (simulated)" = "#d95f02")) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "n per arm", y = "power", colour = NULL,
       title = "Claim 108 vs 69 per arm: formula lines, simulation points") +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/01_individual_sample_size.png", fig,
       width = 6.5, height = 4.2, dpi = 300)
cat("Figur: R/simulations/figures/01_individual_sample_size.png\n")
