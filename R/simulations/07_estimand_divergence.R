# =====================================================================
# Internal question, not a paper claim: how far apart can the reported
# numbers be?
#   (1) mixed model, eq. (2)          inverse-variance weighting
#   (2) size-weighted practice means   participant-average estimand
#   (3) unweighted practice means      cluster-average estimand
#   (4) independence estimating equations (IEE), the individual-level
#       estimator Kahan et al. (2023) recommend for the
#       participant-average effect: ordinary least squares on the
#       stacked data, which weights every observation equally, with a
#       cluster-robust sandwich variance by practice.
#
# The design is the trial as it stands: 13 practices, 4 randomised to
# the intervention, 200 analysed participants. The allocation is drawn
# afresh in every replicate, so the expectations are over allocations
# as well as over data, and the estimators can be compared with the
# population estimands directly.
#
# Practice j has its own true effect
#     delta_j = delta + gamma * z_j ,   z_j = (m_j - mean m) / sd(m),
# so gamma is the amount by which the effect changes per standard
# deviation of practice size. gamma = 0 is a constant effect
# (non-informative cluster size), gamma = 2 is severe.
#
# True estimands: participant-average weights delta_j by m_j, the
# cluster-average weights all practices equally.
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages({ library(lme4); library(sandwich); library(ggplot2) })

NSIM <- as.integer(Sys.getenv("NSIM_KR", 400))
set.seed(7)

K <- 13; K_INT <- 4; N_TOTAL <- 200

one_replicate <- function(sizes, delta_j, rho, r = R, sigma = SIGMA) {
  arm <- rep(0, length(sizes))
  arm[sample.int(length(sizes), K_INT)] <- 1        # fresh allocation
  pr   <- rep(seq_along(sizes), sizes)
  n    <- sum(sizes)
  tau2 <- rho * sigma^2
  nu2  <- (r - rho) * sigma^2
  sig2 <- (1 - r) * sigma^2
  u  <- rnorm(length(sizes), 0, sqrt(tau2))[pr]
  w  <- rnorm(n, 0, sqrt(nu2))
  bl <- u + w + rnorm(n, 0, sqrt(sig2))
  fu <- u + w + rnorm(n, 0, sqrt(sig2)) - delta_j[pr] * arm[pr]
  d  <- data.frame(id = seq_len(n), practice = pr, group = arm[pr],
                   bl = bl, fu = fu)

  # (1) the primary model of the plan
  long <- rbind(data.frame(d[c("id", "practice", "group")], time = 0, y = d$bl),
                data.frame(d[c("id", "practice", "group")], time = 1, y = d$fu))
  m <- suppressMessages(lmer(y ~ time + time:group + (1 | practice) + (1 | id),
                             long, REML = TRUE))
  est_mm <- fixef(m)[["time:group"]]

  # (2) and (3): practice-level summaries of the change
  ch  <- tapply(d$fu - d$bl, d$practice, mean)
  nj  <- as.vector(table(d$practice))
  g   <- arm
  est_sw <- weighted.mean(ch[g == 1], nj[g == 1]) -
            weighted.mean(ch[g == 0], nj[g == 0])
  est_uw <- mean(ch[g == 1]) - mean(ch[g == 0])

  # (4) independence estimating equations: the same mean structure as the
  # primary model, fitted by OLS so that every observation counts equally,
  # with the practice as the clustering unit for the sandwich variance
  fit_iee <- lm(y ~ time + time:group, data = long)
  est_iee <- coef(fit_iee)[["time:group"]]
  se_iee  <- sqrt(vcovCL(fit_iee, cluster = long$practice)["time:group",
                                                          "time:group"])

  c(mixed = est_mm, size_weighted = est_sw, unweighted = est_uw,
    iee = est_iee, se_iee = se_iee)
}

scen <- expand.grid(cv = c(0, 0.65, 1.0), rho = c(0.01, 0.05), gamma = c(0, 2))
res <- data.frame()
for (i in seq_len(nrow(scen))) {
  cv <- scen$cv[i]; rho <- scen$rho[i]; gam <- scen$gamma[i]
  sizes <- make_sizes(K, N_TOTAL, cv)
  z <- if (sd(sizes) > 0) (sizes - mean(sizes)) / sd(sizes) else rep(0, K)
  delta_j <- DELTA + gam * z
  true_pa <- -sum(sizes * delta_j) / sum(sizes)   # participant-average
  true_ca <- -mean(delta_j)                       # cluster-average
  r3 <- replicate(NSIM, one_replicate(sizes, delta_j, rho))
  res <- rbind(res, data.frame(
    cv = cv, rho = rho, gamma = gam,
    size_range = sprintf("%d-%d", min(sizes), max(sizes)),
    true_pa = round(true_pa, 2), true_ca = round(true_ca, 2),
    mixed = round(mean(r3["mixed", ]), 2),
    sw    = round(mean(r3["size_weighted", ]), 2),
    uw    = round(mean(r3["unweighted", ]), 2),
    iee   = round(mean(r3["iee", ]), 2),
    sd_mixed = round(sd(r3["mixed", ]), 2),
    sd_sw    = round(sd(r3["size_weighted", ]), 2),
    sd_uw    = round(sd(r3["unweighted", ]), 2),
    sd_iee   = round(sd(r3["iee", ]), 2),
    se_iee_mean = round(mean(r3["se_iee", ]), 2)))
  cat(sprintf("cv %.2f | ICC %.2f | gamma %.0f  ->  true PA %.2f / CA %.2f | mixed %.2f, sw %.2f, uw %.2f, IEE %.2f\n",
              cv, rho, gam, true_pa, true_ca,
              mean(r3["mixed", ]), mean(r3["size_weighted", ]),
              mean(r3["unweighted", ]), mean(r3["iee", ])))
}

banner("Vollstaendige Tabelle")
print(res, row.names = FALSE)

banner("Abweichung der drei Schaetzer vom jeweils gemeinten Estimand")
res$bias_mixed_pa <- round(res$mixed - res$true_pa, 2)
res$bias_sw_pa    <- round(res$sw - res$true_pa, 2)
res$bias_uw_ca    <- round(res$uw - res$true_ca, 2)
res$bias_iee_pa   <- round(res$iee - res$true_pa, 2)
print(res[c("cv", "rho", "gamma", "bias_mixed_pa", "bias_iee_pa",
            "bias_sw_pa", "bias_uw_ca")], row.names = FALSE)
cat("\nbias_mixed_pa: trifft das gemischte Modell den participant-average?\n")
cat("bias_sw_pa / bias_uw_ca: Kontrolle, beide sollten ~0 sein.\n")

# ---- figure ---------------------------------------------------------
pd <- rbind(
  data.frame(res[c("cv", "rho", "gamma")], est = res$mixed, what = "mixed model"),
  data.frame(res[c("cv", "rho", "gamma")], est = res$sw,    what = "size-weighted (participant-average)"),
  data.frame(res[c("cv", "rho", "gamma")], est = res$uw,    what = "unweighted (cluster-average)"),
  data.frame(res[c("cv", "rho", "gamma")], est = res$iee,   what = "IEE (participant-average)"))
pd$panel <- sprintf("gamma = %d, ICC = %.2f", pd$gamma, pd$rho)
fig <- ggplot(pd, aes(factor(cv), est, colour = what, group = what)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2) +
  facet_wrap(~ panel) +
  scale_colour_manual(values = c("#7570b3", "#d95f02", "#1b9e77", "#e7298a")) +
  labs(x = "coefficient of variation of practice size", y = "mean estimate",
       colour = NULL,
       title = "How far apart are the candidate estimators?",
       subtitle = sprintf("13 practices, 4 randomised to intervention, N = 200, %d replicates; true effect -2 on average", NSIM)) +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/07_estimand_divergence.png", fig,
       width = 8, height = 5.2, dpi = 300)
cat("\nFigur: R/simulations/figures/07_estimand_divergence.png\n")
