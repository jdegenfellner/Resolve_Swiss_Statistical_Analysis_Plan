# =====================================================================
# Claims checked (paper, section "Sample size" and Table 2):
#  (a) "the committed 200 analysed participants give 73% at rho = 0.01,
#      falling to 51% at rho = 0.05" with 4 vs 9 practices (Table 2,
#      first row: 0.73 / 0.61 / 0.51), t reference with k1+k2-2 = 11 df
#  (b) Table 2, balanced designs with 14 per practice: 5/5 -> 0.62 and
#      7/7 -> 0.80 at rho = 0.01, 10/10 -> 0.85 at rho = 0.03
# Analysis in the simulation: the trial's actual primary analysis, the
# constrained model of eq. (2) with Kenward-Roger degrees of freedom.
# The formula behind Table 2 is a cluster-level t approximation, so
# agreement within a few points is the expected outcome, not identity.
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages({ library(lme4); library(lmerTest); library(ggplot2) })

NSIM <- as.integer(Sys.getenv("NSIM_KR", 400))
set.seed(3)

p_kr <- function(d) {
  long <- rbind(data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 0, y = d$bl),
                data.frame(id = seq_len(nrow(d)), practice = d$practice,
                           group = d$group, time = 1, y = d$fu))
  m <- suppressMessages(lmer(y ~ time + time:group + (1 | practice) + (1 | id),
                             long, REML = TRUE))
  summary(m, ddf = "Kenward-Roger")$coefficients["time:group", "Pr(>|t|)"]
}

run <- function(k1, k2, sizes, rho, label, claim, rho_c = 0.6) {
  rej <- replicate(NSIM, p_kr(gen_crt(k1, k2, sample(sizes), rho,
                                      rho_c = rho_c)) < 0.05)
  cat(sprintf("%-28s empirisch %s | Papier (Formel): %.2f\n", label, mc(rej, NSIM), claim))
  mean(rej)
}
# rho_c = 0.6 is the neutral world in which the factorised planning
# formula is exact (script 04); rho_c = 1 is the literal world of
# model (2), where the practice effect cancels from the contrast and
# the formula is conservative.

banner("(a) aktueller Stand: 4 vs 9 Praxen, N = 200, cv = 0.65, rho_c = 0.6")
sz49 <- make_sizes(13, 200, 0.65)
emp <- c(run(4, 9, sz49, 0.01, "rho 0.01:", 0.73),
         run(4, 9, sz49, 0.03, "rho 0.03:", 0.61),
         run(4, 9, sz49, 0.05, "rho 0.05:", 0.51))
banner("(a') dieselben Zeilen in der Modell-(2)-Welt (rho_c = 1)")
invisible(c(run(4, 9, sz49, 0.03, "rho 0.03:", 0.61, rho_c = 1),
            run(4, 9, sz49, 0.05, "rho 0.05:", 0.51, rho_c = 1)))

banner("(b) balancierte Designs, 14 pro Praxis")
emp2 <- c(run(5, 5,  make_sizes(10, 140, 0.65), 0.01, "5/5,  rho 0.01:", 0.62),
          run(7, 7,  make_sizes(14, 196, 0.65), 0.01, "7/7,  rho 0.01:", 0.80),
          run(10, 10, make_sizes(20, 280, 0.65), 0.03, "10/10, rho 0.03:", 0.85))

# ---- figure ---------------------------------------------------------
pd <- data.frame(
  scenario = factor(c("4/9\nrho .01", "4/9\nrho .03", "4/9\nrho .05",
                      "5/5\nrho .01", "7/7\nrho .01", "10/10\nrho .03"),
                    levels = c("4/9\nrho .01", "4/9\nrho .03", "4/9\nrho .05",
                               "5/5\nrho .01", "7/7\nrho .01", "10/10\nrho .03")),
  simulated = c(emp, emp2),
  formula   = c(0.73, 0.61, 0.51, 0.62, 0.80, 0.85))
pd_l <- reshape(pd, direction = "long", varying = c("simulated", "formula"),
                v.names = "power", timevar = "what",
                times = c("simulated (KR model)", "formula (Table 2)"))
pd_l$se <- ifelse(pd_l$what == "simulated (KR model)",
                  sqrt(pd_l$power * (1 - pd_l$power) / NSIM), NA)
fig <- ggplot(pd_l, aes(scenario, power, fill = what)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_errorbar(aes(ymin = power - 1.96 * se, ymax = power + 1.96 * se),
                position = position_dodge(width = 0.7), width = 0.15, na.rm = TRUE) +
  geom_hline(yintercept = 0.8, linetype = 3) +
  scale_fill_manual(values = c("formula (Table 2)" = "#1b9e77",
                               "simulated (KR model)" = "#d95f02")) +
  labs(x = NULL, y = "power", fill = NULL,
       title = "Power claims of Table 2 against simulation of the primary analysis",
       subtitle = sprintf("%d replications per scenario; error bars are 95%% MC intervals", NSIM)) +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/03_power_current_design.png", fig,
       width = 7, height = 4.4, dpi = 300)
cat("Figur: R/simulations/figures/03_power_current_design.png\n")
