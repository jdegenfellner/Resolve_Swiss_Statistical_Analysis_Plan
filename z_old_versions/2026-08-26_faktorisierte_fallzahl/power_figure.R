# =====================================================================
# RESOLVE Swiss — statistical analysis plan
# Figure: how the power of the current allocation arises from the
# noncentral t distribution (eq. 2 of the manuscript).
#
# Inputs match R/sample_size.R: current split 6/9, ICC 0.01, which
# gives SE = 0.656 and lambda = 2 / 0.656 = 3.05 at nu = 13 df.
# Output: figures/power_noncentral_t.png
# =====================================================================

suppressMessages(library(ggplot2))

nu  <- 13                 # k1 + k2 - 2 for 6 + 9 practices
se  <- 0.656              # SE of the treatment effect, 6/9, ICC 0.01
lam <- 2 / se             # noncentrality: target difference / SE
tc  <- qt(0.975, nu)

x <- seq(-4.5, 8, length.out = 1200)
d <- rbind(
  data.frame(x, dens = dt(x, nu),            was = "no treatment effect: central t"),
  data.frame(x, dens = dt(x, nu, ncp = lam), was = "true difference of 2 points: noncentral t"))
pow   <- 1 - pt(tc, nu, lam) + pt(-tc, nu, lam)
shade <- subset(d, was != "no treatment effect: central t" & x >= tc)

p <- ggplot(d, aes(x, dens, colour = was, fill = was)) +
  geom_area(data = shade, alpha = 0.35, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(-tc, tc), linetype = 2, colour = "grey35") +
  annotate("text", x = tc, y = 0.40, hjust = -0.05, size = 3.4, colour = "grey25",
           label = sprintf("critical value %.2f", tc)) +
  annotate("text", x = lam, y = 0.06, size = 4.2, colour = "#1a5e20", fontface = "bold",
           label = sprintf("power = %.2f", pow)) +
  annotate("segment", x = 0, xend = lam, y = 0.435, yend = 0.435,
           arrow = arrow(length = unit(2.2, "mm")), colour = "grey25") +
  annotate("text", x = lam / 2, y = 0.455, size = 3.4, colour = "grey25",
           label = sprintf("shift by lambda = 2 / %.3f = %.2f", se, lam)) +
  scale_colour_manual(values = c("#4C7FB0", "#3a7d3a")) +
  scale_fill_manual(values = c("#4C7FB0", "#3a7d3a")) +
  labs(x = "test statistic t", y = "density", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

dir.create("figures", showWarnings = FALSE)
ggsave("figures/power_noncentral_t.png", p, width = 8.0, height = 4.4, dpi = 300)
cat(sprintf("Power: %.4f — figures/power_noncentral_t.png geschrieben\n", pow))
