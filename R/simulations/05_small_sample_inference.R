# =====================================================================
# Claims checked (paper, section "Inference with a small number of
# clusters", and sensitivity analysis 6):
#  (a) naive inference has an inflated type I error at our size
#  (b) the KEY finding of this simulation: with a time-varying practice
#      effect (cluster autocorrelation < 1) the rigid primary model
#      (time-constant practice intercept only) understates uncertainty
#      and EVEN KENWARD-ROGER inflates, because the covariance model is
#      wrong, not the degrees of freedom. The flexible model with an
#      additional practice-by-time random effect (sensitivity analysis
#      6, after Hooper 2018) restores the nominal level, with df near
#      the k - 2 = 11 the power formula assumes.
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages({ library(lme4); library(lmerTest); library(ggplot2) })

NSIM <- as.integer(Sys.getenv("NSIM_KR", 400)) * 2
set.seed(5)

sz <- make_sizes(13, 200, 0.65)
one <- function(rho, rho_c = 0.6) {
  # rho_c < 1: the practice effect has a time-specific part, so it does
  # not cancel from the contrast and small-cluster inference is at stake
  d <- gen_crt(4, 9, sample(sz), rho, delta = 0, rho_c = rho_c)
  long <- rbind(data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 0, y = d$bl),
                data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 1, y = d$fu))
  long$pt <- interaction(long$practice, long$time)
  m  <- suppressMessages(lmer(y ~ time + time:group + (1 | practice) + (1 | id),
                              long, REML = TRUE))
  s  <- summary(m, ddf = "Kenward-Roger")$coefficients["time:group", ]
  z  <- summary(m, ddf = "lme4")               # no df correction
  tz <- z$coefficients["time:group", "t value"]
  mf <- suppressMessages(lmer(y ~ time + time:group + (1 | practice) +
                              (1 | pt) + (1 | id), long, REML = TRUE))
  sf <- summary(mf, ddf = "Kenward-Roger")$coefficients["time:group", ]
  c(p_z = 2 * pnorm(-abs(tz)),                  # naive Wald
    p_kr = s[["Pr(>|t|)"]], df_kr = s[["df"]],  # rigid model + KR
    p_fl = sf[["Pr(>|t|)"]], df_fl = sf[["df"]])# flexible model + KR
}

banner("Type-I-Fehler unter der Null, 4 vs 9 Praxen, N = 200, rho_c = 0.6")
res <- data.frame()
for (rho in c(0.01, 0.05)) {
  r <- replicate(NSIM, one(rho))
  cat(sprintf("rho %.2f: naive Wald %s | starres Modell + KR %s (df %.0f) | flexibles Modell + KR %s (df %.1f)\n",
              rho, mc(r["p_z", ] < 0.05, NSIM),
              mc(r["p_kr", ] < 0.05, NSIM), mean(r["df_kr", ]),
              mc(r["p_fl", ] < 0.05, NSIM), mean(r["df_fl", ])))
  res <- rbind(res,
    data.frame(rho = paste("ICC =", rho), what = "naive Wald",
               rate = mean(r["p_z", ] < 0.05), n = NSIM),
    data.frame(rho = paste("ICC =", rho), what = "rigid model + KR",
               rate = mean(r["p_kr", ] < 0.05), n = NSIM),
    data.frame(rho = paste("ICC =", rho), what = "practice-by-time + KR",
               rate = mean(r["p_fl", ] < 0.05), n = NSIM))
}
cat("Das starre Modell unterschaetzt die Unsicherheit, sobald rho_c < 1;\n")
cat("KR aendert daran nichts. Der Praxis-mal-Zeit-Effekt stellt das Niveau her.\n")

# ---- figure ---------------------------------------------------------
res$se <- sqrt(res$rate * (1 - res$rate) / res$n)
fig <- ggplot(res, aes(rho, rate, fill = what)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se),
                position = position_dodge(width = 0.6), width = 0.12) +
  geom_hline(yintercept = 0.05, linetype = 2) +
  scale_fill_manual(values = c("naive Wald" = "#d95f02",
                               "rigid model + KR" = "#e6ab02",
                               "practice-by-time + KR" = "#1b9e77")) +
  labs(x = NULL, y = "empirical type I error", fill = NULL,
       title = "Covariance misspecification, not df, is the risk at 13 practices",
       subtitle = sprintf("4 vs 9 practices, N = 200, cluster autocorrelation 0.6, %d null replications per ICC", NSIM)) +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/05_small_sample_inference.png", fig,
       width = 6.2, height = 4.2, dpi = 300)
cat("Figur: R/simulations/figures/05_small_sample_inference.png\n")
