# =====================================================================
# RESOLVE Swiss — prior predictive check for the Bayesian supplement.
#
# Draws parameters from the priors of eq. (4) and simulates whole
# trials from them, without using any Swiss data. The question is
# whether the priors imply data that could plausibly be observed
# \citep{gabry2019}: RMDQ scores lie between 0 and 24, so a normal
# likelihood can put mass on impossible values, especially at 18 weeks
# where the mean is low.
#
# Hyperparameters are those of R/aus_prior_values.R.
# =====================================================================

suppressMessages(library(ggplot2))
set.seed(2026)

NSIM <- 2000          # prior draws
K1 <- 4; K2 <- 9      # practices per arm
M  <- 200 / 13        # mean practice size

# priors of eq. (4), points scale (R/aus_prior_values.R)
m0 <- 9.5;  s0 <- 2
m1 <- -3.5; s1 <- 2
delta_hat <- -2.4; se_delta <- 0.56
C <- 2                # discount factor, currently open in the plan
s_tau <- 2; s_nu <- 4; s_sigma <- 4

# priors for the beta-binomial version, logit scale. The coefficients
# there are log-odds, so the point-scale values above do not transfer.
# Centres come from beta-binomial fits to the four Australian
# arm-by-occasion subgroups (R/aus_rmdq_distributions.R).
lm0 <- -0.40; ls0 <- 0.4
lm1 <- -0.69; ls1 <- 0.4
ldelta <- -0.53; lse <- 0.20
THETA <- 4.3
l_tau <- 0.3; l_nu <- 0.8

half_normal <- function(n, s) abs(rnorm(n, 0, s))

draw_trial <- function(beta_binomial = FALSE) {
  b0 <- rnorm(1, m0, s0)
  b1 <- rnorm(1, m1, s1)
  b2 <- rnorm(1, delta_hat, C * se_delta)
  tau <- half_normal(1, s_tau); nu <- half_normal(1, s_nu)
  sig <- half_normal(1, s_sigma)

  k <- K1 + K2
  sizes <- rep(round(M), k)
  pr <- rep(seq_len(k), sizes)
  arm <- rep(c(1, 0), c(K1, K2))[pr]
  u <- rnorm(k, 0, tau)[pr]
  w <- rnorm(length(pr), 0, nu)

  if (!beta_binomial) {
    eta_bl <- b0 + u + w
    eta_fu <- b0 + b1 + b2 * arm + u + w
    bl <- eta_bl + rnorm(length(pr), 0, sig)
    fu <- eta_fu + rnorm(length(pr), 0, sig)
  } else {
    # own priors on the logit scale
    a0 <- rnorm(1, lm0, ls0); a1 <- rnorm(1, lm1, ls1)
    a2 <- rnorm(1, ldelta, C * lse)
    ul <- rnorm(k, 0, abs(rnorm(1, 0, l_tau)))[pr]
    wl <- rnorm(length(pr), 0, abs(rnorm(1, 0, l_nu)))
    p_bl <- plogis(a0 + ul + wl)
    p_fu <- plogis(a0 + a1 + a2 * arm + ul + wl)
    rbb <- function(p) rbinom(length(p), 24,
                              rbeta(length(p), p * THETA, (1 - p) * THETA))
    bl <- rbb(p_bl); fu <- rbb(p_fu)
  }
  c(mean_bl = mean(bl),
    mean_fu_int = mean(fu[arm == 1]), mean_fu_ctl = mean(fu[arm == 0]),
    out_of_range = mean(bl < 0 | bl > 24 | fu < 0 | fu > 24),
    effect = b2)
}

banner <- function(...) cat("\n---", ..., "---\n")

banner("Normal likelihood, priors of eq. (4)")
r <- t(replicate(NSIM, draw_trial(FALSE)))
cat(sprintf("implied baseline mean : %.1f  (2.5%%-97.5%%: %.1f to %.1f)\n",
            mean(r[, "mean_bl"]), quantile(r[, "mean_bl"], .025),
            quantile(r[, "mean_bl"], .975)))
cat(sprintf("implied 18-week mean, intervention: %.1f  (%.1f to %.1f)\n",
            mean(r[, "mean_fu_int"]), quantile(r[, "mean_fu_int"], .025),
            quantile(r[, "mean_fu_int"], .975)))
cat(sprintf("implied 18-week mean, control     : %.1f  (%.1f to %.1f)\n",
            mean(r[, "mean_fu_ctl"]), quantile(r[, "mean_fu_ctl"], .025),
            quantile(r[, "mean_fu_ctl"], .975)))
cat(sprintf("implied effect        : %.1f  (%.1f to %.1f)\n",
            mean(r[, "effect"]), quantile(r[, "effect"], .025),
            quantile(r[, "effect"], .975)))
cat(sprintf("share of simulated scores outside 0 to 24: %.1f%%\n",
            100 * mean(r[, "out_of_range"])))

banner("Beta-binomial likelihood")
rb <- t(replicate(NSIM, draw_trial(TRUE)))
cat(sprintf("implied baseline mean : %.1f  (%.1f to %.1f)\n",
            mean(rb[, "mean_bl"]), quantile(rb[, "mean_bl"], .025),
            quantile(rb[, "mean_bl"], .975)))
cat(sprintf("implied 18-week mean, intervention: %.1f  (%.1f to %.1f)\n",
            mean(rb[, "mean_fu_int"]), quantile(rb[, "mean_fu_int"], .025),
            quantile(rb[, "mean_fu_int"], .975)))
cat(sprintf("implied 18-week mean, control     : %.1f  (%.1f to %.1f)\n",
            mean(rb[, "mean_fu_ctl"]), quantile(rb[, "mean_fu_ctl"], .025),
            quantile(rb[, "mean_fu_ctl"], .975)))
cat(sprintf("share outside 0 to 24 : %.1f%% (zero by construction)\n",
            100 * mean(rb[, "out_of_range"])))

cat("\nObserved in the Australian trial: baseline 9.6, 18 weeks 3.6 and 6.4.\n")

# ---- figure ---------------------------------------------------------
mk <- function(m, lab) rbind(
  data.frame(value = m[, "mean_bl"],     what = "baseline",             model = lab),
  data.frame(value = m[, "mean_fu_int"], what = "18 weeks, intervention", model = lab),
  data.frame(value = m[, "mean_fu_ctl"], what = "18 weeks, control",      model = lab))
pd <- rbind(mk(r, "normal"), mk(rb, "beta-binomial"))
lev <- c("baseline", "18 weeks, intervention", "18 weeks, control")
pd$what <- factor(pd$what, lev)
obs <- data.frame(what = factor(lev, lev), value = c(9.8, 3.6, 6.4))
fig <- ggplot(pd, aes(value, fill = model)) +
  geom_density(alpha = 0.45, colour = NA) +
  geom_vline(data = obs, aes(xintercept = value), linetype = 2) +
  facet_wrap(~ what, scales = "free") +
  scale_fill_manual(values = c("normal" = "#7570b3",
                               "beta-binomial" = "#d95f02")) +
  labs(x = "implied arm mean RMDQ", y = NULL, fill = NULL,
       title = "Prior predictive distribution of the arm means",
       subtitle = "Dashed lines mark the Australian observations. Scores are bounded at 0 and 24.") +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("figures", showWarnings = FALSE)
ggsave("figures/prior_predictive.png", fig, width = 7.5, height = 3.8, dpi = 300)

# ---- the prior for the treatment effect itself ----------------------
x <- seq(-6, 3, length.out = 600)
pri <- rbind(
  data.frame(x, d = dnorm(x, delta_hat, C * se_delta),
             what = sprintf("informative: Normal(%.1f, %.2f)", delta_hat, C * se_delta)),
  data.frame(x, d = dnorm(x, 0, 2.5), what = "sceptical: Normal(0, 2.5)"))
p_ben  <- 1 - pnorm(0, delta_hat, C * se_delta)   # careful: effect is negative
fig2 <- ggplot(pri, aes(x, d, colour = what, fill = what)) +
  geom_area(data = subset(pri, grepl("informative", what) & x <= 0),
            alpha = 0.25, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = c(-2, 0), linetype = c(3, 2), colour = "grey35") +
  annotate("text", x = -2, y = 0, label = "target difference", angle = 90,
           hjust = -0.15, vjust = -0.5, size = 3, colour = "grey35") +
  scale_colour_manual(values = c("#d95f02", "#7570b3")) +
  scale_fill_manual(values = c("#d95f02", "#7570b3")) +
  labs(x = expression(beta[2]~"(RMDQ points, negative favours the intervention)"),
       y = "density", colour = NULL, fill = NULL,
       title = "Priors for the treatment effect",
       subtitle = sprintf("Under the informative prior, P(benefit) = %.2f and P(benefit > 2 points) = %.2f",
                          pnorm(0, delta_hat, C * se_delta),
                          pnorm(-2, delta_hat, C * se_delta))) +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
ggsave("figures/prior_beta2.png", fig2, width = 6.6, height = 4.0, dpi = 300)
cat(sprintf("\nUnter dem informativen Prior: P(Nutzen) = %.2f, P(Nutzen > 2 Punkte) = %.2f\n",
            pnorm(0, delta_hat, C * se_delta), pnorm(-2, delta_hat, C * se_delta)))
cat("Figuren: figures/prior_predictive.png, figures/prior_beta2.png\n")
