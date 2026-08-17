# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Hyperparameters of the Bayesian supplement, eq. (4), computed from the
# Australian RESOLVE individual participant data.
#
# The IPD are READ FROM THE INSTITUTE SHARE ONLY, never copied here.
# Timepoints: rmdq.t1 = baseline, rmdq.t5 = 18 weeks; allocn 1 =
# intervention, 0 = control.
#
# Centres come from the Australian data. Prior widths are deliberately
# wider than the Australian standard errors, so that the Swiss data can
# move them.
# =====================================================================

suppressMessages(library(lme4))

path_aus <- file.path("/Volumes/shared$/pools/g/G-PT-Resolve-Swiss-normal",
                      "z_DATA_AUS_from_Matt",
                      "data_clean_resolve sydney_04mar26.csv")
d <- read.csv(path_aus, check.names = FALSE)
d$group <- factor(d$allocn, 0:1, c("Control", "Intervention"))
d <- d[complete.cases(d$rmdq.t1, d$rmdq.t5), ]

cat("== beta_0, the common baseline mean ==\n")
m0 <- mean(d$rmdq.t1)
cat(sprintf("  baseline RMDQ: mean %.2f, SD %.2f, n = %d\n",
            m0, sd(d$rmdq.t1), nrow(d)))
cat(sprintf("  m_0 = %.1f, s_0 = 2 (wide relative to the standard error of %.2f)\n",
            m0, sd(d$rmdq.t1) / sqrt(nrow(d))))

cat("\n== beta_1, the control-arm change to 18 weeks ==\n")
ch_c <- with(d[d$group == "Control", ], rmdq.t5 - rmdq.t1)
cat(sprintf("  change: mean %.2f, SD %.2f, n = %d\n",
            mean(ch_c), sd(ch_c), length(ch_c)))
cat(sprintf("  m_1 = %.1f, s_1 = 2\n", mean(ch_c)))

cat("\n== beta_2, the treatment effect ==\n")
long <- rbind(data.frame(id = seq_len(nrow(d)), group = d$allocn, time = 0, y = d$rmdq.t1),
              data.frame(id = seq_len(nrow(d)), group = d$allocn, time = 1, y = d$rmdq.t5))
m <- suppressMessages(lmer(y ~ time + time:group + (1 | id), long, REML = TRUE))
est <- fixef(m)[["time:group"]]
se  <- sqrt(vcov(m)["time:group", "time:group"])
cat(sprintf("  constrained model on the Australian data: %.2f (SE %.2f)\n", est, se))
for (cc in c(1.5, 2, 3))
  cat(sprintf("  discount c = %.1f: Normal(%.1f, %.2f)\n", cc, est, cc * se))

cat("\n== variance components ==\n")
vc <- as.data.frame(VarCorr(m))
nu    <- vc$sdcor[vc$grp == "id"]
sigma <- vc$sdcor[vc$grp == "Residual"]
cat(sprintf("  participant-level nu = %.2f, residual sigma = %.2f\n", nu, sigma))
cat(sprintf("  s_nu = %.0f, s_sigma = %.0f (half-normal scales, generous)\n",
            ceiling(nu), ceiling(sigma)))
tot <- nu^2 + sigma^2
cat(sprintf("  total variance %.1f; a practice level with ICC 0.01 to 0.05 implies\n", tot))
cat(sprintf("  tau between %.2f and %.2f, so s_tau = 2 covers it\n",
            sqrt(0.01 * tot), sqrt(0.05 * tot)))
cat("  (the Australian trial randomised individuals, so tau cannot be\n")
cat("   estimated from it and its prior rests on the ICC literature)\n")
