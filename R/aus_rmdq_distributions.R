# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Distribution of the RMDQ in the four prior-relevant subgroups of the
# Australian IPD: intervention / control at baseline / 18 weeks.
#
# Purpose: choose the distribution family for the Bayesian supplement
# (priors and likelihood; see SAP section "Supplementary Bayesian
# analysis"). The RMDQ is a count of 24 checked items, so the natural
# discrete family is the beta-binomial with size 24. Continuous
# families are fitted on the same footing by discretising their
# densities to the integer grid, P(k) = F(k+1) - F(k) on [0, 25), so
# that log-likelihoods and AICs are directly comparable.
#
# IPD are READ FROM THE INSTITUTE SHARE ONLY, never copied. The figure
# contains aggregate frequencies only and stays out of the repository
# (figures/aus_* is gitignored).
# =====================================================================

suppressMessages(library(ggplot2))

path_aus <- file.path("/Volumes/shared$/pools/g/G-PT-Resolve-Swiss-normal",
                      "z_DATA_AUS_from_Matt",
                      "data_clean_resolve sydney_04mar26.csv")
d <- read.csv(path_aus, check.names = FALSE)
d$group <- factor(d$allocn, 0:1, c("Control", "Intervention"))

sub <- list(
  `Intervention, baseline` = d$rmdq.t1[d$group == "Intervention"],
  `Control, baseline`      = d$rmdq.t1[d$group == "Control"],
  `Intervention, 18 weeks` = d$rmdq.t5[d$group == "Intervention"],
  `Control, 18 weeks`      = d$rmdq.t5[d$group == "Control"])
sub <- lapply(sub, function(x) x[!is.na(x)])

# ---- families, all as discrete pmf on 0..24 -------------------------
disc <- function(cdf) function(x, par) {
  p <- cdf(x + 1, par) - cdf(x, par)
  pmax(p, 1e-12)
}
fam <- list(
  `beta-binomial` = list(
    np = 2, start = c(qlogis(0.4), log(5)),
    pmf = function(x, par) {
      p <- plogis(par[1]); th <- exp(par[2])
      a <- p * th; b <- (1 - p) * th
      exp(lchoose(24, x) + lbeta(x + a, 24 - x + b) - lbeta(a, b))
    },
    report = function(par) sprintf("prob = %.3f, theta = %.2f", plogis(par[1]), exp(par[2]))),
  normal = list(
    np = 2, start = c(10, log(5)),
    pmf = disc(function(q, par) pnorm(q, par[1], exp(par[2]))),
    report = function(par) sprintf("mean = %.2f, sd = %.2f", par[1], exp(par[2]))),
  gamma = list(
    np = 2, start = c(log(3), log(0.3)),
    pmf = disc(function(q, par) pgamma(q, exp(par[1]), exp(par[2]))),
    report = function(par) sprintf("shape = %.2f, rate = %.3f", exp(par[1]), exp(par[2]))),
  lognormal = list(
    np = 2, start = c(2, log(0.8)),
    pmf = disc(function(q, par) plnorm(q, par[1], exp(par[2]))),
    report = function(par) sprintf("meanlog = %.2f, sdlog = %.2f", par[1], exp(par[2]))),
  weibull = list(
    np = 2, start = c(log(1.5), log(10)),
    pmf = disc(function(q, par) pweibull(q, exp(par[1]), exp(par[2]))),
    report = function(par) sprintf("shape = %.2f, scale = %.2f", exp(par[1]), exp(par[2]))),
  `beta (x/25)` = list(
    np = 2, start = c(log(1.5), log(2.5)),
    pmf = disc(function(q, par) pbeta(q / 25, exp(par[1]), exp(par[2]))),
    report = function(par) sprintf("alpha = %.2f, beta = %.2f", exp(par[1]), exp(par[2]))))

fit1 <- function(x, f) {
  nll <- function(par) -sum(log(f$pmf(x, par)))
  o <- optim(f$start, nll, method = "Nelder-Mead", control = list(maxit = 4000))
  list(par = o$par, aic = 2 * o$value + 2 * f$np)
}

cat("== AIC per subgroup (discretised likelihoods, directly comparable) ==\n")
fits <- list(); best <- list()
for (s in names(sub)) {
  x <- sub[[s]]
  fits[[s]] <- lapply(fam, function(f) fit1(x, f))
  aic <- sapply(fits[[s]], `[[`, "aic")
  ord <- order(aic)
  cat("\n", s, " (n=", length(x), ", Mittel ", round(mean(x), 1), "):\n", sep = "")
  for (i in ord) cat(sprintf("  %-14s AIC %8.1f   %s\n", names(fam)[i], aic[i],
                             fam[[i]]$report(fits[[s]][[i]]$par)))
  best[[s]] <- names(fam)[ord[1]]
}

cat("\n== rethinking-ready (beta-binomial, size 24) ==\n")
for (s in names(sub)) {
  par <- fits[[s]][["beta-binomial"]]$par
  cat(sprintf("%-24s rmdq ~ dbetabinom(24, prob, theta), prob = %.3f, theta = %.2f\n",
              s, plogis(par[1]), exp(par[2])))
}

# ---- SAP figure 1: distributions with fitted families ---------------
grid <- 0:24
pd <- do.call(rbind, lapply(names(sub), function(s) {
  x <- sub[[s]]
  obs <- tabulate(x + 1, 25) / length(x)
  bb <- fam[["beta-binomial"]]$pmf(grid, fits[[s]][["beta-binomial"]]$par)
  bc <- setdiff(names(sort(sapply(fits[[s]], `[[`, "aic"))), "beta-binomial")[1]
  cont <- fam[[bc]]$pmf(grid, fits[[s]][[bc]]$par)
  lab <- sprintf("%s  (best continuous: %s)", s, bc)
  rbind(data.frame(sub = lab, x = grid, y = obs, what = "observed"),
        data.frame(sub = lab, x = grid, y = bb,  what = "beta-binomial (size 24)"),
        data.frame(sub = lab, x = grid, y = cont, what = "best continuous fit"))
}))
pd$sub <- factor(pd$sub, unique(pd$sub))
fig <- ggplot() +
  geom_col(data = subset(pd, what == "observed"), aes(x, y),
           fill = "grey85", colour = "grey60", linewidth = 0.2, width = 0.9) +
  geom_line(data = subset(pd, what != "observed"),
            aes(x, y, colour = what), linewidth = 0.65) +
  geom_point(data = subset(pd, what == "beta-binomial (size 24)"),
             aes(x, y, colour = what), size = 1.0) +
  facet_wrap(~ sub, nrow = 2, scales = "free_y") +
  scale_colour_manual(values = c("beta-binomial (size 24)" = "#d95f02",
                                 "best continuous fit"     = "#1b9e77")) +
  labs(x = "RMDQ score", y = "probability", colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8))
dir.create("figures", showWarnings = FALSE)
ggsave("figures/aus_rmdq_dist_sap.png", fig, width = 8.2, height = 5.6, dpi = 300)

# ---- SAP figure 2: boxplots at baseline and 18 weeks ----------------
# Aggregate summaries only: box = IQR, whiskers = 1.5 IQR, diamond =
# mean, no individual points.
bx <- rbind(
  data.frame(group = d$group, time = "Baseline", rmdq = d$rmdq.t1),
  data.frame(group = d$group, time = "18 weeks", rmdq = d$rmdq.t5))
bx <- bx[!is.na(bx$rmdq), ]
bx$time <- factor(bx$time, c("Baseline", "18 weeks"))
dodge <- position_dodge(width = 0.72)
figb <- ggplot(bx, aes(time, rmdq, fill = group, colour = group)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.28,
                                             jitter.height = 0.22,
                                             dodge.width = 0.72),
             alpha = 0.30, size = 0.7, stroke = 0, show.legend = FALSE) +
  geom_boxplot(position = dodge, width = 0.5, outlier.shape = NA,
               coef = 1.5, alpha = 0.55, colour = "grey20",
               linewidth = 0.35) +
  stat_summary(aes(group = group), fun = mean, geom = "point",
               shape = 23, size = 2.2, fill = "white", colour = "grey20",
               stroke = 0.5, position = dodge, show.legend = FALSE) +
  scale_fill_manual(values = c(Control = "#4C7FB0", Intervention = "#D9722B")) +
  scale_colour_manual(values = c(Control = "#4C7FB0", Intervention = "#D9722B")) +
  scale_y_continuous(limits = c(-0.5, 24.5), breaks = seq(0, 24, 4),
                     expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = NULL, y = "RMDQ score (0\u201324)", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        legend.key.size = unit(0.9, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey25"),
        axis.title.y = element_text(colour = "grey25", margin = margin(r = 6)))
ggsave("figures/aus_rmdq_box_sap.png", figb, width = 5.6, height = 4.2, dpi = 300)
cat("\nFiguren geschrieben: figures/aus_rmdq_dist_sap.png, figures/aus_rmdq_box_sap.png\n")
