# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Baseline-to-18-week correlation of the RMDQ in the Australian IPD.
#
# The individual participant data of RESOLVE Australia (Bagg et al. 2022)
# live on the institute share and are READ FROM THERE ONLY. Never copy
# them into this repository; .gitignore blocks data files as a guard.
# This script prints aggregate statistics only.
#
# Timepoint mapping (verified against Bagg 2022, JAMA, Table 2):
#   rmdq.t1 = baseline   (arm means 10.0 / 9.6)
#   rmdq.t5 = 18 weeks   (intervention 3.6 (4.6), control 6.4 (5.8))
#   rmdq.t6 = 26 weeks, rmdq.t7 = 52 weeks
#   allocn: 1 = intervention, 0 = control
# =====================================================================

path_aus <- file.path("/Volumes/shared$/pools/g/G-PT-Resolve-Swiss-normal",
                      "z_DATA_AUS_from_Matt",
                      "data_clean_resolve sydney_04mar26.csv")

d  <- read.csv(path_aus, check.names = FALSE)
cc <- complete.cases(d$rmdq.t1, d$rmdq.t5)

cat("RMDQ baseline vs 18 weeks, overall (n =", sum(cc), "): r =",
    round(cor(d$rmdq.t1[cc], d$rmdq.t5[cc]), 3), "\n")

# Within-arm correlations, pooled via Fisher's z. The within-arm value is
# the one the sample size formula needs: it is not distorted by the
# between-arm mean shift that the treatment effect induces.
rs <- ns <- c()
for (g in c(0, 1)) {
  i  <- d$allocn == g & cc
  rs <- c(rs, cor(d$rmdq.t1[i], d$rmdq.t5[i]))
  ns <- c(ns, sum(i))
  cat("  arm", g, ": n =", sum(i), ", r =", round(tail(rs, 1), 3), "\n")
}
r_pooled <- tanh(sum(atanh(rs) * (ns - 3)) / sum(ns - 3))
cat("within-arm pooled: r =", round(r_pooled, 3), "\n")
