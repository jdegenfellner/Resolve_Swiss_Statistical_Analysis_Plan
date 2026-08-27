# Fallzahlrechnung vor der Umstellung auf Teerenstra (Stand 26.8.2026)

Diese Fassung rechnet die Fallzahl in zwei Schritten:

1. Benchmark für eine individuell randomisierte Studie über die
   Normalapproximation, Chow et al. (2018) Abschnitt 3.2.1.
2. Darauf der Design-Effekt nach Eldridge et al. (2006) und der Faktor
   (1 - r^2) mit r = 0.6, also Julious (2023) Gleichung (5.15). Erst
   danach die Korrektur auf die t-Verteilung über die nichtzentrale t.

Warum ersetzt: die faktorisierte Fassung setzt (1 - r^2) mit der
Korrelation auf Individualebene an. Exakt gehört dort die kombinierte
Korrelation nach Teerenstra et al. (2012) Gleichung (5) hin, ein
gewichtetes Mittel aus Cluster- und Subjekt-Autokorrelation. Die
faktorisierte Fassung ist damit der Spezialfall rho_c = rho_s = 0.6.
Sie ist nicht falsch, aber sie verbirgt eine Annahme, die man nennen
sollte, und sie mischt zwei Quellen und zwei Verteilungsannahmen.

Belegt in R/simulations/08_dgp_and_power_validation.R.

## Zurückholen

    cp z_old_versions/2026-08-26_faktorisierte_fallzahl/paper.tex .
    cp z_old_versions/2026-08-26_faktorisierte_fallzahl/sample_size.R R/
    cp z_old_versions/2026-08-26_faktorisierte_fallzahl/power_figure.R R/

Der Stand entspricht ausserdem dem letzten Commit vor der Umstellung.
