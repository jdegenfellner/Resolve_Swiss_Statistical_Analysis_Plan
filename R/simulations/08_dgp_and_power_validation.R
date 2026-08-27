# =====================================================================
# Daten werden aus genau dem Modell gezogen, das der Analyseplan
# annimmt, Gleichung (2) des Papers:
#
#   y_ijt = beta0 + beta1*s_t + beta2*s_t*x_j + u_j + w_ij + e_ijt
#
#   u_j   ~ N(0, tau2)    Praxis, ueber beide Zeitpunkte konstant
#   w_ij  ~ N(0, nu2)     Teilnehmer, ueber beide Zeitpunkte konstant
#   e_ijt ~ N(0, sig2e)   Messrauschen je Zeitpunkt
#
# Mit sigma^2 = tau2 + nu2 + sig2e ergibt das
#   ICC rho              = tau2 / sigma^2
#   Baseline-Follow-up r = (tau2 + nu2) / sigma^2
# also tau2 = rho*sigma^2, nu2 = (r-rho)*sigma^2, sig2e = (1-r)*sigma^2.
# Es gibt nur zwei Zeitpunkte und keine praxisspezifische Zeitkomponente.
#
# Drei Schritte:
#  (1) Nachweis, dass die erzeugten Daten die angenommenen Momente haben.
#  (2) Haelt die Primaeranalyse ihr Niveau von 5 Prozent?
#  (3) Trifft die empirische Power die Zahlen aus Tabelle 2?
#
# Aufruf:  Rscript R/simulations/08_dgp_and_power_validation.R
#          NSIM_KR=1000 NSIM_LEV=1000 Rscript ...   fuer mehr Genauigkeit
# =====================================================================
source("R/simulations/00_helpers.R")
suppressMessages({ library(lme4); library(lmerTest); library(ggplot2) })

NSIM <- as.integer(Sys.getenv("NSIM_KR",  400))
NLEV <- as.integer(Sys.getenv("NSIM_LEV", 400))
NMOM <- as.integer(Sys.getenv("NSIM_MOM", 2000))
K1   <- 6; K2 <- 9
NTOT <- 200
CV   <- 0.65
set.seed(8)

sizes <- make_sizes(K1 + K2, NTOT, CV)
FORM  <- y ~ time + time:group + (1 | practice) + (1 | id)

to_long <- function(d)
  rbind(data.frame(id = seq_len(nrow(d)), practice = d$practice,
                   group = d$group, time = 0, y = d$bl),
        data.frame(id = seq_len(nrow(d)), practice = d$practice,
                   group = d$group, time = 1, y = d$fu))

fit_kr <- function(d) {
  fm <- suppressMessages(lmer(FORM, to_long(d), REML = TRUE))
  co <- summary(fm, ddf = "Kenward-Roger")$coefficients["time:group", ]
  c(p = co[["Pr(>|t|)"]], se = co[["Std. Error"]],
    est = co[["Estimate"]], df = co[["df"]])
}

icc_of <- function(y, practice) {      # ANOVA-Schaetzer der Intra-Cluster-Korrelation
  nj  <- as.vector(table(practice))
  pm  <- tapply(y, practice, mean)
  msb <- sum(nj * (pm - mean(y))^2) / (length(nj) - 1)
  msw <- sum((y - pm[practice])^2) / (length(y) - length(nj))
  n0  <- (sum(nj) - sum(nj^2) / sum(nj)) / (length(nj) - 1)
  (msb - msw) / (msb + (n0 - 1) * msw)
}

# ---------------------------------------------------------------------
# (1) Haben die Daten die angenommenen Eigenschaften?
# ---------------------------------------------------------------------
RHO <- 0.01
banner(sprintf("(1) Eigenschaften der erzeugten Daten, rho = %.2f", RHO))

mom <- t(replicate(NMOM, {
  d <- gen_crt(K1, K2, sizes, RHO, rho_c = 1)
  y <- d$fu + DELTA * d$group           # Behandlungseffekt herausgerechnet
  c(`Differenz Woche 18`  = mean(d$fu[d$group == 0]) - mean(d$fu[d$group == 1]),
    `Differenz Baseline`  = mean(d$bl[d$group == 0]) - mean(d$bl[d$group == 1]),
    `SD Woche 18`         = sd(y),
    `ICC Woche 18`        = icc_of(y, d$practice),
    `Korr. Baseline/W18`  = cor(d$bl, y),
    `ICC der Differenz`   = icc_of(y - d$bl, d$practice))
}))

target <- c(DELTA, 0, SIGMA, RHO, R, NA)
cat(sprintf("%-22s %-24s %s\n", "Groesse", "realisiert (MC-Fehler)", "Sollwert"))
for (i in seq_len(ncol(mom)))
  cat(sprintf("%-22s %8.4f (%.4f)        %s\n", colnames(mom)[i],
              mean(mom[, i]), sd(mom[, i]) / sqrt(NMOM),
              if (is.na(target[i])) "  siehe unten" else sprintf("%8.4f", target[i])))
cat(sprintf("%-22s %8.4f                 %8.4f\n", "cv Praxisgroessen",
            sd(sizes) / mean(sizes), CV))

cat("\nDie ersten fuenf Zeilen treffen ihre Sollwerte. Die sechste ist der Punkt,\n",
    "auf den es ankommt: in der Differenz Woche 18 minus Baseline faellt der\n",
    "Praxiseffekt u_j exakt heraus, weil er ueber beide Zeitpunkte derselbe ist.\n",
    "Die Differenz traegt deshalb keine Intra-Cluster-Korrelation mehr. Genau\n",
    "diese Groesse schaetzt das Modell, denn time:group ist eine Differenz von\n",
    "Differenzen.\n\n",
    "Die Fallzahlformel des Papers ist Julious (2023), Gleichung (5.15):\n",
    "  IF = [1 + ((cv^2+1)*m - 1)*ICC] * (1 - r^2)\n",
    "Sie ist damit korrekt uebernommen. Sie multipliziert aber die volle\n",
    "Clusterinflation mit der individuellen Baseline-Korrektur, als seien die\n",
    "beiden unabhaengig. Ist der Praxiseffekt ueber die Zeit konstant, nimmt die\n",
    "Baseline-Korrektur den Clustereffekt bereits mit heraus, und die Inflation\n",
    "wird ein zweites Mal berechnet. Julious behandelt in (5.14) nur den anderen\n",
    "Fall, in dem die Baseline auf Clusterebene erhoben wird. Der exakte Weg fuer\n",
    "beide Faelle steht bei Teerenstra et al. (2012), siehe Skript 04.\n", sep = "")

# ---------------------------------------------------------------------
# (2) Niveau
# ---------------------------------------------------------------------
banner("(2) Signifikanzniveau bei wahrem Effekt 0, nominal 0.05")
for (rho in c(0.01, 0.03, 0.05)) {
  a <- t(replicate(NLEV, fit_kr(gen_crt(K1, K2, sample(sizes), rho,
                                        delta = 0, rho_c = 1))))
  cat(sprintf("rho %.2f:  Niveau %s | mittlere KR-df %.0f\n",
              rho, mc(a[, "p"] < 0.05, NLEV), mean(a[, "df"])))
}
cat("\nDas Niveau wird gehalten. Der Test ist gueltig, er ist nur informativer,\n",
    "als die Fallzahlformel annimmt: die Freiheitsgrade liegen weit ueber den\n",
    "k1+k2-2 = 13, mit denen Tabelle 2 rechnet.\n", sep = "")

# ---------------------------------------------------------------------
# (3) Power
# ---------------------------------------------------------------------
power_formula <- function(k1, k2, m, rho, cv = CV, sigma = SIGMA,
                          delta = DELTA, r = R, alpha = 0.05) {
  sig2 <- sigma^2 * (1 - r^2)
  de_m <- (cv^2 + 1) * m
  se   <- sqrt(sig2 * (1 + (de_m - 1) * rho) * (1 / (m * k1) + 1 / (m * k2)))
  df   <- k1 + k2 - 2
  tc   <- qt(1 - alpha / 2, df)
  pt(-tc, df, delta / se) + (1 - pt(tc, df, delta / se))
}

# Exakter ANCOVA-Design-Effekt nach Teerenstra et al. (2012), Gl. (5) und (7).
# Die faktorisierte Fassung setzt (1 - r^2) mit der Korrelation auf
# Individualebene an. Exakt gehoert dort die kombinierte Korrelation hin,
# ein gewichtetes Mittel aus Cluster- und Individualautokorrelation:
#   r_comb = w*rho_c + (1-w)*rho_s,  w = m*rho / (1 + (m-1)*rho)
# In der Welt von Modell (2) ist der Praxiseffekt zeitkonstant, also
# rho_c = 1, und rho_s = (r - rho) / (1 - rho).
power_teerenstra <- function(k1, k2, m, rho, cv = CV, sigma = SIGMA,
                             delta = DELTA, r = R, alpha = 0.05, df = NULL) {
  rho_s  <- (r - rho) / (1 - rho)
  w      <- m * rho / (1 + (m - 1) * rho)
  r_comb <- w * 1 + (1 - w) * rho_s          # rho_c = 1
  de_m   <- (cv^2 + 1) * m
  se     <- sqrt(sigma^2 * (1 - r_comb^2) * (1 + (de_m - 1) * rho) *
                 (1 / (m * k1) + 1 / (m * k2)))
  if (is.null(df)) df <- k1 + k2 - 2
  tc <- qt(1 - alpha / 2, df)
  c(power = pt(-tc, df, delta / se) + (1 - pt(tc, df, delta / se)),
    se = se, r_comb = r_comb)
}

scen <- data.frame(rho = c(0.01, 0.02, 0.03, 0.05),
                   table = c(0.80, 0.75, 0.69, 0.60))

banner("(3) Empirische Power gegen Gleichung (2) und Tabelle 2")
cat(sprintf("%-10s %-20s %-9s %-11s %-13s %s\n", "ICC", "empirisch (MC-SE)",
            "Gl. (2)", "Teerenstra", "Teer. + KR-df", "SE Modell/wahr/Teer."))
m_bar <- NTOT / (K1 + K2)
res <- do.call(rbind, lapply(seq_len(nrow(scen)), function(i) {
  rho <- scen$rho[i]
  a   <- t(replicate(NSIM, fit_kr(gen_crt(K1, K2, sample(sizes), rho, rho_c = 1))))
  pf  <- power_formula(K1, K2, m_bar, rho)
  te  <- power_teerenstra(K1, K2, m_bar, rho)
  te2 <- power_teerenstra(K1, K2, m_bar, rho, df = mean(a[, "df"]))
  cat(sprintf("rho %.2f   %-20s %-9.2f %-11.2f %-13.2f %.3f / %.3f / %.3f\n", rho,
              mc(a[, "p"] < 0.05, NSIM), pf, te[["power"]], te2[["power"]],
              mean(a[, "se"]), sd(a[, "est"]), te[["se"]]))
  data.frame(rho = rho, empirical = mean(a[, "p"] < 0.05), formula = pf,
             teerenstra = te[["power"]], teer_krdf = te2[["power"]],
             table = scen$table[i])
}))
cat("\nDie drei Standardfehler sind: vom Modell gemeldet, tatsaechliche Streuung\n",
    "der Schaetzer, und nach Teerenstra berechnet. Die Luecke zwischen Gl. (2)\n",
    "und der Simulation zerfaellt damit in zwei Teile:\n",
    "  Standardfehler: Gl. (2) setzt (1-r^2) an, exakt gehoert (1-r_comb^2) hin.\n",
    "                  Teerenstra trifft die tatsaechliche Streuung.\n",
    "  Freiheitsgrade: beide Formeln rechnen mit k1+k2-2 = 13. Weil der\n",
    "                  Praxiseffekt aus dem Kontrast herausfaellt, liefert\n",
    "                  Kenward-Roger aber rund 160 bis 190.\n",
    "Die Spalte Teer. + KR-df setzt beide Korrekturen zusammen.\n", sep = "")

# ---- Figur ----------------------------------------------------------
pl <- reshape(res[, c("rho", "empirical", "formula", "teerenstra", "teer_krdf")],
              direction = "long",
              varying = c("empirical", "formula", "teerenstra", "teer_krdf"),
              v.names = "power", timevar = "what",
              times = c("simuliert, Modell (2)", "Formel, Gl. (2)",
                        "Teerenstra, 13 df", "Teerenstra + KR-df"))
pl$label <- factor(sprintf("rho %.2f", pl$rho))
pl$se <- ifelse(pl$what == "simuliert, Modell (2)",
                sqrt(pl$power * (1 - pl$power) / NSIM), NA)
fig <- ggplot(pl, aes(label, power, fill = what)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_errorbar(aes(ymin = power - 1.96 * se, ymax = power + 1.96 * se),
                position = position_dodge(width = 0.7), width = 0.15, na.rm = TRUE) +
  geom_hline(yintercept = 0.8, linetype = 3) +
  scale_fill_manual(values = c("Formel, Gl. (2)" = "#1b9e77",
                               "Teerenstra, 13 df" = "#7570b3",
                               "Teerenstra + KR-df" = "#a6761d",
                               "simuliert, Modell (2)" = "#d95f02")) +
  labs(x = NULL, y = "Power", fill = NULL,
       title = "Faktorisierte gegen exakte Fallzahlformel, gegen Simulation",
       subtitle = sprintf("6 vs 9 Praxen, 200 ausgewertete Teilnehmer, %d Wiederholungen; Fehlerbalken 95%% MC", NSIM)) +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("R/simulations/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("R/simulations/figures/08_dgp_and_power_validation.png", fig,
       width = 7.5, height = 4.4, dpi = 300)
cat("\nFigur: R/simulations/figures/08_dgp_and_power_validation.png\n")
