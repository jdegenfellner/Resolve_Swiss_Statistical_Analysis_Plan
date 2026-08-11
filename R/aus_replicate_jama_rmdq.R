# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Replication of the RMDQ (disability) results of the Australian trial
# (Bagg et al. 2022, JAMA, Table 2 and Figure 2B) from the IPD.
#
# The IPD are READ FROM THE INSTITUTE SHARE ONLY, never copied into the
# repository. This script prints aggregate statistics and writes one
# figure containing aggregate summaries only (boxes, whiskers, means;
# no individual points).
#
# Timepoint mapping (verified against JAMA Table 2):
#   rmdq.t1 = baseline, rmdq.t5 = 18 weeks, t6 = 26, t7 = 52
#   allocn: 1 = intervention, 0 = control
# =====================================================================

suppressMessages({ library(lme4); library(lmerTest); library(ggplot2) })

path_aus <- file.path("/Volumes/shared$/pools/g/G-PT-Resolve-Swiss-normal",
                      "z_DATA_AUS_from_Matt",
                      "data_clean_resolve sydney_04mar26.csv")
d <- read.csv(path_aus, check.names = FALSE)

# ---- long format ----------------------------------------------------
map <- data.frame(var  = c("rmdq.t1", "rmdq.t5", "rmdq.t6", "rmdq.t7"),
                  time = c("Baseline", "18 wk", "26 wk", "52 wk"))
long <- do.call(rbind, lapply(seq_len(nrow(map)), function(i)
  data.frame(ID    = d$ID,
             group = factor(d$allocn, 0:1, c("Control", "Intervention")),
             time  = map$time[i],
             rmdq  = d[[map$var[i]]])))
long$time <- factor(long$time, levels = map$time)
long <- long[!is.na(long$rmdq), ]

# ---- 1. observed means (SD), n — against JAMA Table 2 ---------------
jama <- data.frame(
  time = rep(map$time, each = 2),
  group = rep(c("Intervention", "Control"), 4),
  jama = c("9.6 (5.4) n=138", "10 (5.0) n=138",
           "3.6 (4.6) n=127", "6.4 (5.8) n=130",
           "4.0 (5.0) n=126", "6.3 (5.8) n=127",
           "4.1 (5.4) n=122", "6.1 (5.8) n=124"))
obs <- aggregate(rmdq ~ time + group, long, function(x)
  sprintf("%.1f (%.1f) n=%d", mean(x), sd(x), length(x)))
cmp <- merge(jama, obs, sort = FALSE)
names(cmp)[3:4] <- c("JAMA", "replicated")
cat("== Observed RMDQ, JAMA Table 2 vs raw data ==\n")
print(cmp[order(cmp$time, cmp$group), ], row.names = FALSE)

# ---- 2. the JAMA model ----------------------------------------------
# Linear mixed model as in the Australian SAP (Bagg et al. 2021):
# fixed effects for group, time and group-by-time, random intercept per
# participant; baseline is part of the outcome vector and the group main
# effect is free (unconstrained model). The effect at each follow-up is
# the group-by-time interaction coefficient.
m <- lmer(rmdq ~ group * time + (1 | ID), data = long, REML = TRUE)
co <- fixef(m); V <- as.matrix(vcov(m))
# JAMA Table 2 reports the between-arm difference AT each time point.
# In the unconstrained model that is the group main effect plus the
# group-by-time interaction, with the variance of the sum.
cat("\n== Mixed model, arm difference at each time point vs JAMA ==\n")
jama_eff <- c("-2.6 (-3.9 to -1.3) p<.001",
              "-2.1 (-3.4 to -0.8) p=.002",
              "-1.8 (-3.1 to -0.5) p=.008")
for (i in seq_along(jama_eff)) {
  int <- paste0("groupIntervention:time", c("18 wk", "26 wk", "52 wk"))[i]
  est <- co["groupIntervention"] + co[int]
  se  <- sqrt(V["groupIntervention", "groupIntervention"] + V[int, int] +
              2 * V["groupIntervention", int])
  p   <- 2 * pnorm(-abs(est / se))
  cat(sprintf("%s wk: %5.1f (%.1f to %.1f) p=%.4f   | JAMA: %s\n",
              c("18", "26", "52")[i],
              est, est - 1.96 * se, est + 1.96 * se, p, jama_eff[i]))
}
cat(sprintf("(group main effect, i.e. baseline difference: %.2f;\n the pure group-by-time interactions are %.1f / %.1f / %.1f)\n",
            co["groupIntervention"],
            co["groupIntervention:time18 wk"],
            co["groupIntervention:time26 wk"],
            co["groupIntervention:time52 wk"]))

# ---- 3. Figure 2B-style boxplots ------------------------------------
# JAMA legend: box = IQR, whiskers = most extreme value within 1.5 IQR,
# marker = observed mean. No individual points are drawn.
fig <- ggplot(long, aes(time, rmdq, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6,
               outlier.shape = NA, coef = 1.5, alpha = 0.9,
               colour = "grey25", linewidth = 0.35) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2.6,
               colour = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c(Control = "#9ecae1", Intervention = "#fdae6b")) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  labs(x = NULL, y = "RMDQ score (0–24)", fill = NULL,
       title = "Disability (RMDQ) by treatment group",
       subtitle = "Replication of Bagg et al. 2022, Figure 2B. Box = IQR, whiskers = 1.5 × IQR, diamond = mean.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 8.5, colour = "grey35"))
out <- "figures/aus_rmdq_boxplots.png"
dir.create("figures", showWarnings = FALSE)
ggsave(out, fig, width = 7.5, height = 4.8, dpi = 300)
cat("\nFigur geschrieben:", out, "\n")
