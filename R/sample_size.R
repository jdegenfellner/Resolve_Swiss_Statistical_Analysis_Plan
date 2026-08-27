# =====================================================================
# RESOLVE Swiss — Fallzahl und Power, durchgehend nach
# Teerenstra, Eijkemans & Graham (2012), Stat Med 31:2169-2178.
#
# Ein Rahmen, eine Quelle, ein Modell. Die frühere Fassung rechnete in
# zwei Schritten, erst ein Benchmark für eine individuell randomisierte
# Studie über die Normalapproximation, dann Design-Effekt und
# Baseline-Korrektur, und erst danach eine Korrektur auf die
# t-Verteilung. Sie liegt in z_old_versions/.
#
# Teerenstras Modell (1), Seite 2170:
#   y_gtik = mu + beta_g + tau_t + (gamma tau)_gt
#            + c_i + (c tau)_{i,t} + s_ik + (s tau)_{ik,t}
# mit allen zufaelligen Termen normalverteilt. Daraus, Gleichung (2):
#   rho_c = sigma_c^2 / (sigma_c^2 + sigma_ctau^2)   Cluster-Autokorrelation
#   rho_s = sigma_s^2 / (sigma_s^2 + sigma_stau^2)   Subjekt-Autokorrelation
#   rho   = ICC am einzelnen Zeitpunkt                      (Gleichung 6)
#
# Gleichung (5): die Korrelation, mit der die ANCOVA optimal gewichtet,
#   r = n*rho/(1+(n-1)rho) * rho_c + (1-rho)/(1+(n-1)rho) * rho_s
# Sie liegt immer zwischen rho_c und rho_s.
#
# Abschnitt 2.3, Seite 2171: Fallzahl der ANCOVA = Fallzahl des
# t-Tests auf die Follow-up-Werte, mal [1 + (n-1)rho], mal (1 - r^2).
# Abschnitt 2.4, Seite 2172: Power aus der t-Verteilung mit I-2 df.
#
# Einzige Ergaenzung ausserhalb Teerenstra: ungleiche Praxisgroessen.
# Teerenstra rechnet mit gemeinsamer Groesse n. Eldridge et al. (2006)
# ersetzen n im Design-Effekt durch (cv^2+1)*n_quer. Diese eine Stelle
# ist markiert.
#
# Aufruf:  Rscript R/sample_size.R
# =====================================================================

options(digits = 4)
out <- function(...) cat(..., "\n", sep = "")

# ---------------------------------------------------------------------
# 1. Eingaben
# ---------------------------------------------------------------------
sd_int  <- 4.6      # Bagg et al. 2022, RMDQ in Woche 18, Intervention
sd_ctrl <- 5.8      # dieselbe Quelle, Kontrolle
sigma   <- sqrt((sd_int^2 + sd_ctrl^2) / 2)   # gepoolte SD
delta   <- 2        # Zielkriterium, Differenz zwischen den Armen
rho_s   <- 0.6      # Subjekt-Autokorrelation Baseline zu Woche 18
rho_c   <- 0.6      # Cluster-Autokorrelation, siehe Abschnitt 5
cv      <- 0.65     # Variationskoeffizient der Praxisgroessen
alpha   <- 0.05
power_t <- 0.80
k1 <- 6; k2 <- 9    # Praxen je Arm
n_tot <- 200        # ausgewertete Teilnehmer insgesamt

out("Gepoolte SD des RMDQ in Woche 18: ", round(sigma, 3))

# ---------------------------------------------------------------------
# 2. Die kombinierte Korrelation, Teerenstra Gleichung (5)
# ---------------------------------------------------------------------
r_comb <- function(n, rho, rho_c, rho_s) {
  w <- n * rho / (1 + (n - 1) * rho)
  w * rho_c + (1 - w) * rho_s
}

# Design-Effekt der Follow-up-Analyse, [1 + (n-1)rho].
# Bei ungleichen Praxisgroessen tritt (cv^2+1)*n_quer an die Stelle von
# n, Eldridge et al. (2006). Das ist die einzige Zutat ausserhalb
# Teerenstra.
# Eldridge definiert s_m^2 mit (k-1) im Nenner, daraus der Faktor (k-1)/k.
de_followup <- function(n_bar, rho, cv, k) 1 + ((cv^2 * (k - 1) / k + 1) * n_bar - 1) * rho

# ---------------------------------------------------------------------
# 3. Varianz und Power der ANCOVA
# ---------------------------------------------------------------------
# Teerenstra Gleichung (7): var(delta_ancova)
#   = sigma^2 (1 - r^2) [1 + (n-1)rho] (1/(n k1) + 1/(n k2))
var_ancova <- function(n_bar, k1, k2, rho, rho_c, rho_s, sigma, cv) {
  r <- r_comb(n_bar, rho, rho_c, rho_s)
  sigma^2 * (1 - r^2) * de_followup(n_bar, rho, cv, k1 + k2) *
    (1 / (n_bar * k1) + 1 / (n_bar * k2))
}

# Teerenstra Abschnitt 2.4: zentrale t mit I-2 Freiheitsgraden, um
# lambda = delta/SE verschoben.
power_ancova <- function(n_bar, k1, k2, rho, rho_c = 0.6, rho_s = 0.6,
                         sigma = 5.2345, delta = 2, cv = 0.65, alpha = 0.05) {
  se  <- sqrt(var_ancova(n_bar, k1, k2, rho, rho_c, rho_s, sigma, cv))
  df  <- k1 + k2 - 2
  tc  <- qt(1 - alpha / 2, df)
  pt(delta / se - tc, df)
}

# Fallzahl je Arm, Teerenstra Abschnitt 2.3. Der Ausgangspunkt ist die
# Fallzahl des t-Tests, also die Loesung von
#   power = 1 - T_{2n-2, delta/sqrt(2 sigma^2/n)}(t_{1-alpha/2, 2n-2}),
# hier numerisch, damit nirgends eine Normalapproximation einfliesst.
n_ttest <- function(sigma, delta, alpha, power_t) {
  f <- function(n) {
    df <- 2 * n - 2
    lam <- delta / sqrt(2 * sigma^2 / n)
    (1 - pt(qt(1 - alpha / 2, df), df, lam)) - power_t
  }
  ceiling(uniroot(f, c(4, 5000))$root)
}

n_ancova <- function(n_bar, rho, rho_c, rho_s, sigma, delta, cv, alpha, power_t,
                     k1 = 6, k2 = 9) {
  r <- r_comb(n_bar, rho, rho_c, rho_s)
  ceiling(n_ttest(sigma, delta, alpha, power_t) *
          de_followup(n_bar, rho, cv, k1 + k2) * (1 - r^2))
}

n_ind <- n_ttest(sigma, delta, alpha, power_t)
out("\nt-Test, individuell randomisiert, ohne Baseline: ", n_ind, " pro Arm")
out("(Normalapproximation zum Vergleich: ",
    ceiling(2 * (qnorm(1 - alpha/2) + qnorm(power_t))^2 * sigma^2 / delta^2), ")")

# ---------------------------------------------------------------------
# 4. Fallzahl über die angenommene ICC
# ---------------------------------------------------------------------
n_bar    <- n_tot / (k1 + k2)
rho_grid <- c(0, 0.01, 0.02, 0.03)

tab <- data.frame(
  ICC        = rho_grid,
  r_comb     = round(sapply(rho_grid, function(p) r_comb(n_bar, p, rho_c, rho_s)), 3),
  DE         = round(sapply(rho_grid, function(p) de_followup(n_bar, p, cv, k1 + k2)), 3),
  n_analysed = sapply(rho_grid, function(p)
                 n_ancova(n_bar, p, rho_c, rho_s, sigma, delta, cv, alpha, power_t)))
tab$n_recruit <- ceiling(tab$n_analysed / 0.9)
out("\nFallzahl je Arm, rho_c = ", rho_c, ", rho_s = ", rho_s,
    ", n_quer = ", round(n_bar, 1), ", cv = ", cv, ":")
print(tab, row.names = FALSE)

# ---------------------------------------------------------------------
# 5. Die Cluster-Autokorrelation ist eine Annahme, also über sie berichten
# ---------------------------------------------------------------------
# Empirie: Martin et al. (2016) schaetzen fuer britische Hausarztpraxen
# einen Median von 0.649 (IQR 0.612 bis 0.692) ueber Zwoelfmonatsperioden.
# Korevaar et al. (2021) finden in der CLOUD-Datenbank, 44 stetige Outcomes
# aus 29 Datensaetzen, einen Median von 0.73 (IQR 0.19 bis 0.91).
# Zum Vergleich zwei Rechenbeispiele ohne eigene Datenbasis: Giraudeau et al.
# (2008) setzen ICC 0.05 und Interperiodenkorrelation 0.01, das entspricht
# rho_c = 0.2; Hooper et al. (2016) nehmen 0.9 an.
rc_grid <- c(0.4, 0.5, 0.6, 0.65, 0.8, 0.9)
sens <- outer(rho_grid[rho_grid > 0], rc_grid, Vectorize(function(p, rc)
  n_ancova(n_bar, p, rc, rho_s, sigma, delta, cv, alpha, power_t)))
dimnames(sens) <- list(paste0("ICC=", rho_grid[rho_grid > 0]),
                       paste0("rho_c=", rc_grid))
out("\nAusgewertete Teilnehmer je Arm, ueber die Cluster-Autokorrelation:")
print(sens)

# ---------------------------------------------------------------------
# 6. Power des tatsaechlichen Designs
# ---------------------------------------------------------------------
out("\nPower bei ", n_tot, " ausgewerteten Teilnehmern, ", k1, " gegen ", k2,
    " Praxen, rho_c = ", rho_c, ":")
pw <- data.frame(ICC = rho_grid[rho_grid > 0],
                 power = round(sapply(rho_grid[rho_grid > 0], function(p)
                   power_ancova(n_bar, k1, k2, p, rho_c, rho_s, sigma, delta, cv, alpha)), 3))
print(pw, row.names = FALSE)

# Reihenfolge wie in Tabelle 3 des Papers, damit sich jede Zeile dort
# hier wiederfindet. 6+4/9+1 ist 10/10 und 6+5/9+2 ist 11/11.
alloc <- list(c(6, 9), c(10, 10), c(11, 11),
              c(5, 5), c(6, 6), c(7, 7), c(8, 8), c(9, 9),
              c(6, 10), c(6, 11), c(6, 12))
out("\nPower anderer Aufteilungen, analysierte Zahl bei ", n_tot, " gehalten:")
alt <- do.call(rbind, lapply(alloc, function(a) {
  nb <- n_tot / sum(a)
  data.frame(Praxen = sprintf("%d / %d", a[1], a[2]), n_quer = round(nb, 1),
             t(round(sapply(rho_grid[rho_grid > 0], function(p)
               power_ancova(nb, a[1], a[2], p, rho_c, rho_s, sigma, delta, cv, alpha)), 2)))
}))
names(alt)[-(1:2)] <- paste0("ICC=", rho_grid[rho_grid > 0])
print(alt, row.names = FALSE)

# ---------------------------------------------------------------------
# 7. Sitzungsinformationen
# ---------------------------------------------------------------------
out("\n")
print(sessionInfo())
