# =====================================================================
# Schlusskontrolle: jede Zahl des SAP gegen Formel UND Simulation.
#
# Der Vergleich laeuft in zwei Richtungen. Die Formelspalte stammt aus
# R/sample_size.R, also aus derselben Quelle wie die Tabellen im Paper.
# Die Simulationsspalte zieht Daten aus Teerenstras Modell (1) und rechnet
# den ANCOVA-Kontrast direkt nach, ohne eine der Formeln zu benutzen.
# Stimmen beide ueberein, ist die Zahl im Paper zweifach abgesichert.
#
# Aufruf:  Rscript R/simulations/09_verify_paper_numbers.R
# Laufzeit etwa zwei Minuten.
# =====================================================================

set.seed(20260908)
invisible(capture.output(source("R/sample_size.R")))

ok <- function(a, b, tol) if (abs(a - b) <= tol) "ok" else "PRUEFEN"
line <- function(...) cat(sprintf(...), "\n", sep = "")

cat("\n=== 1. Eingaben ===\n")
line("gepoolte SD          Paper 5.2    Skript %.4f          %s",
     sigma, ok(sigma, 5.2, 0.05))
line("t-Test je Arm        Paper 109    Skript %d            %s",
     n_ind, ok(n_ind, 109, 0))

# ---------------------------------------------------------------------
# Simulation aus Teerenstras Modell (1): Cluster- und Subjektanteil je in
# einen ueber die Zeit konstanten und einen zeitspezifischen Teil zerlegt.
# ---------------------------------------------------------------------
sim_se <- function(rho, k1, k2, n_tot, B = 4000, rc = 0.6, rs = 0.6) {
  k <- k1 + k2; n <- round(n_tot / k)
  vc <- rho * sigma^2; vs <- sigma^2 - vc
  s_c <- rc * vc; s_ct <- (1 - rc) * vc
  s_s <- rs * vs; s_st <- (1 - rs) * vs
  d <- numeric(B)
  for (b in seq_len(B)) {
    ci <- rnorm(k, 0, sqrt(s_c))
    c0 <- rnorm(k, 0, sqrt(s_ct)); c1 <- rnorm(k, 0, sqrt(s_ct))
    bs <- fu <- vector("list", k)
    for (i in seq_len(k)) {
      si <- rnorm(n, 0, sqrt(s_s))
      bs[[i]] <- ci[i] + c0[i] + si + rnorm(n, 0, sqrt(s_st))
      fu[[i]] <- ci[i] + c1[i] + si + rnorm(n, 0, sqrt(s_st))
    }
    a <- seq_len(k1); cg <- (k1 + 1):k
    d[b] <- (mean(unlist(fu[a])) - rs * mean(unlist(bs[a]))) -
            (mean(unlist(fu[cg])) - rs * mean(unlist(bs[cg])))
  }
  sd(d)
}

cat("\n=== 2. Standardfehler, Formel gegen Simulation (cv = 0) ===\n")
cat("    Die Formel wird ohne Groessenvariation gerechnet, weil die\n")
cat("    Simulation gleich grosse Praxen zieht.\n")
for (rho in c(0.01, 0.02, 0.03)) {
  n <- round(200 / 15)
  de0 <- 1 + (n - 1) * rho
  f <- sqrt(sigma^2 * (1 - 0.6^2) * de0 * (1 / (n * 6) + 1 / (n * 9)))
  s <- sim_se(rho, 6, 9, 200)
  line("ICC %.2f   Formel %.4f   Simulation %.4f   Abweichung %4.1f%%   %s",
       rho, f, s, 100 * (s / f - 1), ok(s / f, 1, 0.04))
}

cat("\n=== 3. Tabelle 3, Power des tatsaechlichen Designs ===\n")
paper_pow <- c("0.01" = 0.81, "0.02" = 0.75, "0.03" = 0.69)
for (rho in c(0.01, 0.02, 0.03)) {
  p <- power_ancova(n_bar, k1, k2, rho, rho_c, rho_s, sigma, delta, cv, alpha)
  line("ICC %.2f   Paper %.2f   Skript %.4f   %s",
       rho, paper_pow[as.character(rho)], p,
       ok(round(p, 2), paper_pow[as.character(rho)], 0.001))
}

cat("\n=== 4. Tabelle 1, Fallzahl je Arm ===\n")
paper_n <- c("0" = 70, "0.01" = 83, "0.02" = 95, "0.03" = 107)
for (rho in c(0, 0.01, 0.02, 0.03)) {
  v <- n_ancova(n_bar, rho, rho_c, rho_s, sigma, delta, cv, alpha, power_t)
  line("ICC %.2f   Paper %3d   Skript %3d   %s",
       rho, paper_n[as.character(rho)], v,
       ok(v, paper_n[as.character(rho)], 0))
}

cat("\n=== 5. Standardfehler der Abbildung ===\n")
se_fig <- sqrt(var_ancova(n_bar, k1, k2, 0.01, rho_c, rho_s, sigma, cv))
line("SE          Paper 0.655   Skript %.4f   %s", se_fig, ok(se_fig, 0.655, 0.001))
line("2/SE        Paper 3.05    Skript %.3f    %s", 2 / se_fig, ok(2 / se_fig, 3.05, 0.01))

cat("\n=== 6. Bayes-Abschnitt ===\n")
a0 <- 1 / 9
line("c aus a0 = 1/9       Paper 3      gerechnet %.3f        %s",
     1 / sqrt(a0), ok(1 / sqrt(a0), 3, 0.001))
line("Prior-SD 3 x 0.56    Paper 1.7    gerechnet %.2f         %s",
     3 * 0.56, ok(3 * 0.56, 1.7, 0.02))
line("skeptisch 2/1.645    Paper 1.22   gerechnet %.3f        %s",
     2 / 1.645, ok(2 / 1.645, 1.22, 0.005))
line("1.645 = 95%%-Quantil  gerechnet %.4f                    %s",
     qnorm(0.95), ok(qnorm(0.95), 1.645, 0.001))
lo <- qlogis(6.4 / 24); hi <- qlogis(4.4 / 24)
line("Logit-Differenz      Paper 0.48   gerechnet %.3f        %s",
     hi - lo, ok(abs(hi - lo), 0.48, 0.01))
line("skeptisch auf Logit  Paper 0.29   gerechnet %.3f        %s",
     abs(hi - lo) / 1.645, ok(abs(hi - lo) / 1.645, 0.29, 0.01))
nu <- 3.7; sg <- 3.6
for (rho in c(0.01, 0.03)) {
  tau <- sqrt(rho * (nu^2 + sg^2) / (1 - rho))
  line("tau bei ICC %.2f      Paper %s    gerechnet %.2f         %s",
       rho, ifelse(rho == 0.01, "0.5", "0.9"), tau,
       ok(round(tau, 1), ifelse(rho == 0.01, 0.5, 0.9), 0.001))
}

cat("\n=== 7. Design-Effekte, Tabelle 1 ===\n")
paper_de <- c("0" = 1.00, "0.01" = 1.18, "0.02" = 1.35, "0.03" = 1.53)
for (rho in c(0, 0.01, 0.02, 0.03)) {
  d <- de_followup(n_bar, rho, cv, k1 + k2)
  line("ICC %.2f   Paper %.2f   Skript %.4f   %s",
       rho, paper_de[as.character(rho)], d,
       ok(round(d, 2), paper_de[as.character(rho)], 0.001))
}

cat("\n=== 8. Kombinierte Korrelation r ===\n")
line("r bei rho_c = rho_s = 0.6 ist konstant 0.6:")
for (rho in c(0.01, 0.02, 0.03)) {
  line("   ICC %.2f  r = %.4f   %s", rho, r_comb(n_bar, rho, rho_c, rho_s),
       ok(r_comb(n_bar, rho, rho_c, rho_s), 0.6, 1e-9))
}

cat("\n=== 9. Tabelle 2 vollstaendig, alle 18 Zellen ===\n")
tab2 <- matrix(c(86,84,83,82,79,77, 102,99,95,93,87,83, 118,113,107,104,95,88),
               nrow = 3, byrow = TRUE)
rcs <- c(0.4, 0.5, 0.6, 0.65, 0.8, 0.9); rhos <- c(0.01, 0.02, 0.03)
bad <- 0
for (i in seq_along(rhos)) for (j in seq_along(rcs)) {
  v <- n_ancova(n_bar, rhos[i], rcs[j], rho_s, sigma, delta, cv, alpha, power_t)
  if (v != tab2[i, j]) { bad <- bad + 1
    line("  ABWEICHUNG ICC %.2f rho_c %.2f: Paper %d Skript %d", rhos[i], rcs[j], tab2[i,j], v) }
}
line("18 Zellen geprueft, Abweichungen: %d   %s", bad, ifelse(bad == 0, "ok", "PRUEFEN"))

cat("\n=== 10. Tabelle 3 vollstaendig, alle 12 Zeilen ===\n")
tab3 <- list(
  list(c(6,9),   c(0.81,0.75,0.69)), list(c(10,10), c(0.85,0.81,0.77)),
  list(c(11,11), c(0.86,0.82,0.79)), list(c(5,5),   c(0.75,0.66,0.58)),
  list(c(6,6),   c(0.79,0.71,0.65)), list(c(7,7),   c(0.81,0.75,0.69)),
  list(c(8,8),   c(0.83,0.78,0.73)), list(c(9,9),   c(0.84,0.80,0.75)),
  list(c(6,10),  c(0.80,0.75,0.70)), list(c(6,11),  c(0.80,0.75,0.70)),
  list(c(6,12),  c(0.80,0.75,0.70)))
bad <- 0; cells <- 0
for (row in tab3) {
  a <- row[[1]]; want <- row[[2]]; nb <- n_tot / sum(a)
  for (i in seq_along(rhos)) {
    cells <- cells + 1
    v <- round(power_ancova(nb, a[1], a[2], rhos[i], rho_c, rho_s, sigma, delta, cv, alpha), 2)
    if (abs(v - want[i]) > 0.001) { bad <- bad + 1
      line("  ABWEICHUNG %d/%d ICC %.2f: Paper %.2f Skript %.2f", a[1], a[2], rhos[i], want[i], v) }
  }
}
line("%d Zellen geprueft, Abweichungen: %d   %s", cells, bad, ifelse(bad == 0, "ok", "PRUEFEN"))

cat("\nFertig.\n")
