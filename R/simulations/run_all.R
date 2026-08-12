# =====================================================================
# Runs every claim-by-claim simulation. From the repository root:
#   Rscript R/simulations/run_all.R
# Reduce or raise the effort with environment variables, e.g.
#   NSIM_FAST=50000 NSIM_LM=10000 NSIM_KR=1000 Rscript R/simulations/run_all.R
# Defaults: NSIM_FAST 20000 (vectorised), NSIM_LM 4000 (lm-based),
# NSIM_KR 400 (mixed models with Kenward-Roger, the slow part).
# =====================================================================

scripts <- c("01_individual_sample_size.R",
             "02_design_effect.R",
             "03_power_current_design.R",
             "04_teerenstra_factorisation.R",
             "05_small_sample_inference.R",
             "06_icc_and_change_penalty.R")

for (s in scripts) {
  cat("\n==============================", s, "==============================\n")
  t0 <- Sys.time()
  source(file.path("R/simulations", s), local = new.env())
  cat(sprintf("[%.1f s]\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("\nAlle Simulationen durchgelaufen. Figuren unter R/simulations/figures/.\n")
