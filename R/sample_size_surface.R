# =====================================================================
# RESOLVE Swiss — exploratory 3D surface of the required sample size.
# x: intra-cluster correlation rho, y: baseline-follow-up correlation r,
# z: required n per arm (two-sided alpha 0.05, power 80%, sigma 5.234,
# delta 2, design effect with m_bar = 200/13, cv = 0.65, k = 13).
# Same formulas as R/sample_size.R. Not part of the manuscript, just
# for looking at: writes an interactive plotly HTML and opens it.
# =====================================================================

suppressMessages({ library(plotly); library(htmlwidgets) })

sigma <- sqrt((4.6^2 + 5.8^2) / 2); delta <- 2
m_bar <- 200 / 13; cv <- 0.65; k <- 13
z2 <- (qnorm(0.975) + qnorm(0.80))^2

rho <- seq(0, 0.06, length.out = 61)
r   <- seq(0, 0.8,  length.out = 81)

n_arm <- outer(rho, r, function(rho, r)
  z2 * 2 * sigma^2 * (1 - r^2) / delta^2 *
    (1 + ((cv^2 * (k - 1) / k + 1) * m_bar - 1) * rho))

# plotly surfaces expect z[y, x]
fig <- plot_ly(x = ~rho, y = ~r, z = ~t(n_arm)) |>
  add_surface(colorscale = "Viridis",
              colorbar = list(title = "n per arm"),
              contours = list(z = list(show = TRUE, usecolormap = TRUE,
                                       project = list(z = TRUE)))) |>
  add_markers(x = c(0.01, 0.03, 0.05), y = rep(0.6, 3),
              z = c(83, 111, 139) + 3,
              marker = list(size = 4, color = "red"),
              name = "paper values (r = 0.6)") |>
  layout(title = "Required sample size per arm",
         scene = list(
           xaxis = list(title = "ICC ρ"),
           yaxis = list(title = "baseline correlation r"),
           zaxis = list(title = "n per arm"),
           camera = list(eye = list(x = -1.5, y = -1.6, z = 0.9))))

out <- file.path("figures", "sample_size_surface.html")
dir.create("figures", showWarnings = FALSE)
saveWidget(fig, out, selfcontained = TRUE, title = "RESOLVE Swiss sample size surface")
cat("geschrieben:", out, "\n")
