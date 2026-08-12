# =====================================================================
# Claims checked:
#  (a) paper, section "Intra-cluster correlation": the reported ICC
#      rho_hat = tau2 / (tau2 + nu2 + sigma2) recovers the true
#      cross-sectional ICC of model (2), while the two-component
#      version tau2 / (tau2 + sigma2) would overstate it
#  (b) paper, section "The model": the constrained model "matches the
#      precision of adjusting the 18-week score for its baseline value"
#      (full demonstration in R/equivalence_constrained_vs_ancova.R,
#      spot-checked here) and the unconstrained model pays the
#      change-score-like precision penalty
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages({ library(lme4); library(ggplot2) })

NSIM <- as.integer(Sys.getenv("NSIM_KR", 400))
set.seed(6)

sz <- make_sizes(13, 200, 0.65)
rho_true <- 0.05   # large enough that estimation error does not drown the signal

fit_once <- function() {
  d <- gen_crt(6, 7, sample(sz), rho_true)
  long <- rbind(data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 0, y = d$bl),
                data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 1, y = d$fu))
  m  <- suppressMessages(lmer(y ~ time + time:group + (1 | practice) + (1 | id),
                              long, REML = TRUE))
  vc <- as.data.frame(VarCorr(m))
  tau2 <- vc$vcov[vc$grp == "practice"]
  nu2  <- vc$vcov[vc$grp == "id"]
  sig2 <- vc$vcov[vc$grp == "Residual"]
  mu <- suppressMessages(lmer(y ~ time + group + time:group + (1 | practice) + (1 | id),
                              long, REML = TRUE))   # unconstrained
  c(icc3 = tau2 / (tau2 + nu2 + sig2),
    icc2 = tau2 / (tau2 + sig2),
    se_c = sqrt(vcov(m)["time:group", "time:group"]),
    se_u = sqrt(vcov(mu)["time:group", "time:group"]))
}

banner("(a) ICC-Formel mit drei Varianzkomponenten")
r <- replicate(NSIM, fit_once())
cat(sprintf("wahres rho: %.3f | Median rho_hat (3 Komponenten): %.3f | mit 2 Komponenten: %.3f\n",
            rho_true, median(r["icc3", ]), median(r["icc2", ])))
cat("Die 2-Komponenten-Version laesst die Teilnehmervarianz weg und\n")
cat("ueberschaetzt die ICC grob - deshalb steht nu^2 in der Formel des Papers.\n")

banner("(b) Praezision constrained vs unconstrained")
cat(sprintf("mittlere SE constrained: %.3f | unconstrained: %.3f | Verhaeltnis %.3f\n",
            mean(r["se_c", ]), mean(r["se_u", ]), mean(r["se_u", ]) / mean(r["se_c", ])))
cat(sprintf("theoretische Obergrenze des Verhaeltnisses sqrt(2/(1+r)) = %.3f\n", sqrt(2 / (1 + R))))

# ---- figure ---------------------------------------------------------
pd <- rbind(data.frame(what = "3 components\n(paper formula)", icc = r["icc3", ]),
            data.frame(what = "2 components\n(nu2 omitted)",  icc = r["icc2", ]))
fig <- ggplot(pd, aes(what, icc)) +
  geom_boxplot(fill = "grey88", outlier.size = 0.6, width = 0.45) +
  geom_hline(yintercept = rho_true, colour = "#d95f02", linetype = 2, linewidth = 0.6) +
  annotate("text", x = 2.35, y = rho_true, label = "true ICC", colour = "#d95f02",
           vjust = -0.6, size = 3.2) +
  labs(x = NULL, y = expression(hat(rho)),
       title = "ICC formula claim: only the three-component version is unbiased",
       subtitle = sprintf("model (2) fitted %d times, 13 practices, true ICC %.2f", NSIM, rho_true)) +
  theme_minimal(base_size = 10)
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/06_icc_formula.png", fig, width = 5.8, height = 4, dpi = 300)
cat("Figur: R/simulations/figures/06_icc_formula.png\n")
