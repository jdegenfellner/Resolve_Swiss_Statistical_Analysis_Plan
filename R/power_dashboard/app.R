# =====================================================================
# RESOLVE Swiss — power and sample size dashboard
#
# Interactive companion to the "Sample size" section of the SAP. Every
# formula mirrors R/sample_size.R and carries its source:
#   two-sample n:      Chow, Shao, Wang & Lokhnygina 2018, eq. (3.11)
#   baseline factor:   Borm, Fransen & Lemmens 2007, (1 - r^2)
#   design effect:     Eldridge, Ashby & Kerry 2006, eq. (1)
#   power:             noncentral t on k1 + k2 - 2 df, variance as in
#                      Hayes & Moulton 2017, eq. (7.12) and sect. 7.6.2
#
# Run from the repository root:  shiny::runApp("R/power_dashboard")
#
# Note: these are the planning formulas. They assume the full practice
# variance sits in the treatment contrast, the honest planning stance
# for time-varying practice effects (see R/simulations/, script 05).
# =====================================================================

library(shiny)
library(ggplot2)

de_eq1 <- function(m_bar, rho, cv, k)
  1 + ((cv^2 * (k - 1) / k + 1) * m_bar - 1) * rho

n_per_arm <- function(sigma, delta, alpha, power, r) {
  (qnorm(1 - alpha / 2) + qnorm(power))^2 * 2 * sigma^2 * (1 - r^2) / delta^2
}

power_crt <- function(k1, k2, m, sigma, delta, rho, alpha, r, cv) {
  sig2  <- sigma^2 * (1 - r^2)
  de_m  <- (cv^2 + 1) * m
  var_d <- sig2 * (1 + (de_m - 1) * rho) * (1 / (m * k1) + 1 / (m * k2))
  df    <- k1 + k2 - 2
  tcrit <- qt(1 - alpha / 2, df)
  ncp   <- delta / sqrt(var_d)
  pt(-tcrit, df, ncp) + (1 - pt(tcrit, df, ncp))
}

ui <- fluidPage(
  titlePanel("RESOLVE Swiss: power and sample size"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Effect and outcome"),
      numericInput("delta", "Target difference (RMDQ points)", 2, 0.5, 6, 0.5),
      numericInput("sigma", "SD of the outcome at 18 weeks", 5.23, 1, 12, 0.01),
      sliderInput("r", "Baseline-follow-up correlation r", 0, 0.9, 0.6, 0.05),
      helpText("Australian IPD: r = 0.51 (R/aus_baseline_correlation.R);",
               "the SAP plans with 0.6."),
      h4("Clustering"),
      sliderInput("rho", "Intra-cluster correlation ρ", 0, 0.10, 0.03, 0.005),
      sliderInput("cv", "CV of practice size", 0, 1, 0.65, 0.05),
      h4("Design"),
      numericInput("k1", "Intervention practices", 4, 2, 30, 1),
      numericInput("k2", "Control practices", 9, 2, 30, 1),
      numericInput("N", "Total analysed participants", 200, 40, 2000, 10),
      sliderInput("attr", "Loss to follow-up (%)", 0, 30, 10, 1),
      numericInput("alpha", "Two-sided α", 0.05, 0.001, 0.2, 0.005),
      numericInput("target_power", "Target power", 0.80, 0.5, 0.99, 0.01)
    ),
    mainPanel(width = 9,
      fluidRow(
        column(4, wellPanel(h4("Power of this design"), h2(textOutput("power")),
                            textOutput("df"))),
        column(4, wellPanel(h4("Required per arm"), h2(textOutput("n_req")),
                            textOutput("n_chain"))),
        column(4, wellPanel(h4("Recruit per arm"), h2(textOutput("n_recr")),
                            textOutput("de")))
      ),
      fluidRow(
        column(6, plotOutput("p_icc", height = 300)),
        column(6, plotOutput("p_k", height = 300))
      ),
      fluidRow(column(12, h4("Requirement per arm across the ICC range"),
                      tableOutput("tab"))),
      helpText("Formulas: Chow et al. 2018 eq. (3.11); Borm et al. 2007;",
               "Eldridge et al. 2006 eq. (1); noncentral t on k₁+k₂−2 df",
               "(Hayes & Moulton 2017). Identical to R/sample_size.R;",
               "simulation checks in R/simulations/.")
    )
  )
)

server <- function(input, output, session) {

  m_cur <- reactive(input$N / (input$k1 + input$k2))

  output$power <- renderText({
    p <- power_crt(input$k1, input$k2, m_cur(), input$sigma, input$delta,
                   input$rho, input$alpha, input$r, input$cv)
    sprintf("%.0f%%", 100 * p)
  })
  output$df <- renderText(sprintf(
    "t reference with %d df; %.1f analysed per practice on average",
    input$k1 + input$k2 - 2, m_cur()))

  n_req <- reactive({
    k <- input$k1 + input$k2
    base <- n_per_arm(input$sigma, input$delta, input$alpha,
                      input$target_power, input$r)
    ceiling(base * de_eq1(m_cur(), input$rho, input$cv, k))
  })
  output$n_req <- renderText(format(n_req()))
  output$n_chain <- renderText({
    b0 <- ceiling(n_per_arm(input$sigma, input$delta, input$alpha,
                            input$target_power, 0))
    b1 <- ceiling(n_per_arm(input$sigma, input$delta, input$alpha,
                            input$target_power, input$r))
    sprintf("unadjusted %d, with baseline %d, then × design effect", b0, b1)
  })
  output$n_recr <- renderText(format(ceiling(n_req() / (1 - input$attr / 100))))
  output$de <- renderText(sprintf(
    "design effect %.2f at ρ = %.3f, cv = %.2f",
    de_eq1(m_cur(), input$rho, input$cv, input$k1 + input$k2),
    input$rho, input$cv))

  output$p_icc <- renderPlot({
    rg <- seq(0, 0.10, 0.002)
    pw <- sapply(rg, function(rho) power_crt(input$k1, input$k2, m_cur(),
      input$sigma, input$delta, rho, input$alpha, input$r, input$cv))
    ggplot(data.frame(rho = rg, power = pw), aes(rho, power)) +
      geom_line(colour = "#1b9e77", linewidth = 0.9) +
      geom_hline(yintercept = input$target_power, linetype = 3) +
      geom_vline(xintercept = input$rho, linetype = 3, colour = "#d95f02") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = expression(rho), y = "power",
           title = "Power against the intra-cluster correlation") +
      theme_minimal(base_size = 12)
  })

  output$p_k <- renderPlot({
    ks <- 2:15
    pw_bal <- sapply(ks, function(k) power_crt(k, k, m_cur(), input$sigma,
      input$delta, input$rho, input$alpha, input$r, input$cv))
    add <- 0:8   # practices added to the current design, split as evenly as possible
    pw_add <- sapply(add, function(a) power_crt(input$k1 + ceiling(a / 2),
      input$k2 + floor(a / 2), m_cur(), input$sigma, input$delta,
      input$rho, input$alpha, input$r, input$cv))
    pd <- rbind(
      data.frame(x = 2 * ks, power = pw_bal, what = "balanced k/k, same m"),
      data.frame(x = input$k1 + input$k2 + add, power = pw_add,
                 what = sprintf("adding to %d/%d, same m", input$k1, input$k2)))
    ggplot(pd, aes(x, power, colour = what)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
      geom_hline(yintercept = input$target_power, linetype = 3) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_colour_manual(values = c("#1b9e77", "#d95f02"), name = NULL) +
      labs(x = "total number of practices", y = "power",
           title = "The lever is the number of practices") +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
  })

  output$tab <- renderTable({
    k <- input$k1 + input$k2
    rhos <- c(0, 0.01, 0.02, 0.03, 0.05, input$rho)
    rhos <- sort(unique(round(rhos, 4)))
    base <- n_per_arm(input$sigma, input$delta, input$alpha,
                      input$target_power, input$r)
    de   <- de_eq1(m_cur(), rhos, input$cv, k)
    req  <- ceiling(base * de)
    data.frame(`ICC` = sprintf("%.3f", rhos),
               `Design effect` = sprintf("%.2f", de),
               `Analysed per arm` = req,
               `Recruited per arm` = ceiling(req / (1 - input$attr / 100)),
               check.names = FALSE)
  }, align = "rrrr")
}

shinyApp(ui, server)
