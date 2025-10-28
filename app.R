# app.R — Quantum Shot Budget Demo (Shiny)
# Purpose: Interactive demo for shot budgeting formulas from the paper
#          "The Cost of Certainty: Shot Budgets in Quantum Program Testing"

# -----------------------------
# Packages
# -----------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(pwr)
})

# -----------------------------
# Math helpers (Sections 3–5)
# -----------------------------

# Inverse test (Eq. 8 and 9)
N_inverse <- function(F, Pe = 0.01, kappa = 1) {
  kappa * log(Pe) / log(F)
}

# Swap test (Eq. 11 and 12)
N_swap <- function(F, Pe = 0.01, kappa = 1) {
  Qswap <- 0.5 + 0.5 * F
  kappa * log(Pe) / log(Qswap)
}

# Chi-square: find noncentrality λ for df, alpha, beta for Eq. 14
lambda_for_power <- function(df, alpha = 0.01, beta = 0.01) {
  pwr.chisq.test(N = NULL, w = 1, df = df, sig.level = alpha, power = 1 - beta)$N
}

# Fidelity-attaining measurement lower bound (Eq. 17)
w2_attaining <- function(F) {
  0.25 * (1 - sqrt(F))^2
}

# Small-discrepancy upper envelope (Eq. 19)
w2_small <- function(F) {
  8 * (1 - sqrt(F))
}

# Shot counts for chi-square via Eq. (14)
N_chi2_attaining <- function(F, k = 16, alpha = 0.01, beta = 0.01) {
  df <- k - 1
  lambda <- lambda_for_power(df, alpha, beta)
  w2 <- w2_attaining(F)
  lambda / w2
}

N_chi2_small <- function(F, k = 16, alpha = 0.01, beta = 0.01) {
  df <- k - 1
  lambda <- lambda_for_power(df, alpha, beta)
  w2 <- w2_small(F)
  lambda / w2
}

# Bures angle utilities (Sec. 5)
A_from_F <- function(Fprog) acos(sqrt(Fprog))
F_from_theta <- function(theta) cos(theta)^2  # Eq. 22

# Allocate per-function angles using weights (Eq. 24)
allocate_thetas <- function(weights, Theta_star) {
  wsum <- sum(weights)
  if (wsum <= 0) stop("Sum of weights must be positive.")
  Theta_star * (weights / wsum)
}

# Per-block inverse approximation (Eq. 25)
N_block_inverse <- function(theta, Pe = 0.01, kappa = 1) {
  F <- F_from_theta(theta)
  N_inverse(F, Pe, kappa)
}

# Per-block swap approximation (Sec 5.2 below Eq. 25)
N_block_swap <- function(theta, Pe = 0.01, kappa = 1) {
  F <- F_from_theta(theta)
  N_swap(F, Pe, kappa)
}

# Noise calibration (Sec. 4): one-sided comparison vs baseline using power.prop.test
N_binomial_baseline <- function(q0, q1, alpha = 0.01, beta = 0.01) {
  if (!is.finite(q0) || !is.finite(q1) || q0 <= q1) return(NA_real_)  # enforce q0 > q1
  out <- tryCatch(
    power.prop.test(p1 = q0, p2 = q1, sig.level = alpha, power = 1 - beta, alternative = "one.sided"),
    error = function(e) NULL
  )
  if (is.null(out)) return(NA_real_)
  out$n
}

# -----------------------------
# UI
# -----------------------------

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly"
)

ui <- navbarPage(
  title = "Quantum Shot Budget Demo",
  theme = theme,
  collapsible = TRUE,
  
  tabPanel(
    "Overview (Sec. 3)",
    fluidPage(
      br(),
      fluidRow(
        column(
          7,
          h4("What this app shows"),
          p("Interactively explore shot-count formulas for inverse, swap, and chi-square tests,",
            "and allocate program-level fidelity budgets across functions using the Bures angle."),
          p(em(
              "For details please see the ",
              a("paper", href = "https://arxiv.org/abs/2510.22418", target = "_blank"),
              "."
          ))
        ),
        column(
          5,
          wellPanel(
            h5("Global parameters"),
            sliderInput("pe", "Pe (accept false-acceptance prob for inverse and swap tests):", min = 1e-6, max = 0.2, value = 0.01, step = 0.0005, sep = ""),
            sliderInput("kappa", "κ (pure↔mixed regime factor for inverse and swap tests):", min = 1, max = 2, value = 1, step = 0.1),
            sliderInput("alpha", "α (Type I error for χ² test):", min = 1e-4, max = 0.2, value = 0.01, step = 0.0005, sep = ""),
            sliderInput("beta", "β (Type II error for χ² test):", min = 1e-4, max = 0.2, value = 0.01, step = 0.0005, sep = ""),
            numericInput("k_bins", "k (bins for χ²):", value = 16, min = 2, step = 1)
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          6,
          h5("Compare tests across Fidelity"),
          sliderInput("F_range_min", "Min F:", min = 0.8, max = 0.9999, value = 0.90, step = 0.001),
          sliderInput("F_range_max", "Max F:", min = 0.81, max = 0.99999, value = 0.995, step = 0.001)
        ),
        column(
          6,
          plotOutput("compare_plot", height = 420)
        )
      )
    )
  ),
  
  tabPanel(
    "Noise Calibration (Sec. 4)",
    fluidPage(
      br(),
      fluidRow(
        column(4,
               sliderInput("q0", "Baseline q0 (noise-only success prob):", min = 0.90, max = 1.0, value = 0.995, step = 0.0005),
               sliderInput("q1", "Target q1 (effective fidelity-like):", min = 0.90, max = 0.9999, value = 0.99, step = 0.0005),
               sliderInput("alpha_noise", "α:", min = 1e-4, max = 0.2, value = 0.01, step = 0.0005, sep = ""),
               sliderInput("beta_noise", "Pe = β:", min = 1e-4, max = 0.2, value = 0.01, step = 0.0005, sep = ""),
               sliderInput("kappa_noise", "κ (pure↔mixed regime factor for  inverse/swap test reference lines):", min = 1, max = 2, value = 2, step = 0.1)
        ),
        column(8,
               wellPanel(
                 h5("Binomial baseline calibration (Sec. 4)"),
                 verbatimTextOutput("n_binom_text")
               ),
               plotOutput("noise_curve", height = 380)
        )
      )
    )
  ),
  
  tabPanel(
    "Program Budget (Ex. 5.1)",
    fluidPage(
      br(),
      fluidRow(
        column(4,
               sliderInput("F_prog", "Program-level Fidelity F_prog:", min = 0.8, max = 0.9999, value = 0.99, step = 0.0005),
               numericInput("k_blocks", "# of functions/blocks (k):", value = 3, min = 1, step = 1),
               textInput("weights_csv", "Weights (comma-separated)", value = "1,2,3"),
               sliderInput("pe_prog", "Pe (per-block):", min = 1e-6, max = 0.2, value = 0.05, step = 0.0005, sep = ""),
               sliderInput("kappa_prog", "κ (per-block):", min = 1, max = 2, value = 1, step = 0.1)
        ),
        column(8,
               tableOutput("budget_table"),
               plotOutput("budget_bar", height = 380)
        )
      )
    )
  ),
  
  tabPanel(
    "Program Budget (Ex. 5.3)",
    fluidPage(
      br(),
      fluidRow(
        column(
          4,
          h5("Program target & regime"),
          sliderInput("F_prog_53", "Program-level Fidelity F_prog:", min = 0.8, max = 0.9999, value = 0.99, step = 0.0005),
          sliderInput("pe_53", "Pe (per-instance inverse test):", min = 1e-6, max = 0.2, value = 0.05, step = 0.0005, sep = ""),
          sliderInput("kappa_53", "κ (pure↔mixed regime factor):", min = 1, max = 2, value = 1, step = 0.1),
          hr(),
          h5("Hardware error rates"),
          numericInput("r1_53", "1q error rate r1:", value = 1e-11, min = 0, step = 1e-12),
          numericInput("r2_53", "2q error rate r2:", value = 1e-10, min = 0, step = 1e-11),
          hr(),
          h5("Archetypes (CSV; arbitrary rows)"),
          helpText("Columns: label,g1,g2,n  (g1=1q gates, g2=2q gates, n=instances)"),
          textAreaInput(
            "arch_csv", NULL, rows = 8, resize = "vertical",
            value = paste(
              "label,g1,g2,n",
              "A,50000,10000,10",
              "B,20000, 4000,40",
              "C,50000,20000,50",
              sep = "\n"
            )
          ),
          actionButton("arch_reset", "Reset to A/B/C")
        ),
        column(
          8,
          tableOutput("ex53_table"),
          plotOutput("ex53_bar", height = 360)
        )
      )
    )
  ),
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  # Overview comparison plot (like Fig. 2)

  # Ensure max >= min
  observeEvent(input$F_range_min, {
    if (input$F_range_max <= input$F_range_min) {
      updateSliderInput(session, "F_range_max",
                        value = input$F_range_min + 0.001  # just above min
      )
    }
  })
  
  observeEvent(input$F_range_max, {
    if (input$F_range_min >= input$F_range_max) {
      updateSliderInput(session, "F_range_min",
                        value = input$F_range_max - 0.001  # just below max
      )
    }
  })
  
  output$compare_plot <- renderPlot({
    Fseq <- seq(input$F_range_min, input$F_range_max, length.out = 80)
    pe <- input$pe; kap <- input$kappa; a <- input$alpha; b <- input$beta; k <- input$k_bins
    
    df <- tibble(
      F = Fseq,
      N_inverse = N_inverse(Fseq, pe, kap),
      N_swap = N_swap(Fseq, pe, kap),
      N_chi_att = map_dbl(Fseq, ~ N_chi2_attaining(.x, k, a, b)),
      N_chi_small = map_dbl(Fseq, ~ N_chi2_small(.x, k, a, b))
    ) |>
      pivot_longer(-F, names_to = "test", values_to = "N") |>
      mutate(test = recode(test,
                           N_inverse = "Inverse (ideal/mixed via κ)",
                           N_swap = "Swap (≈2× inverse)",
                           N_chi_att = "χ² (fidelity-attaining bound)",
                           N_chi_small = "χ² (small-discrepancy bound)"))
    
    ggplot(df, aes(F, N, linetype = test, colour = test)) +
      geom_line(linewidth = 0.9) +
      scale_y_log10() +
      annotation_logticks(sides = "l") +
      labs(
        x = "Fidelity F",
        y = "Shots count",
        title = "Shot counts vs Fidelity",
        subtitle = paste0("Pe=", signif(pe,3), ", κ=", kap,
                          ", α=", signif(a,3), ", β=", signif(b,3), ", k=", k)
      ) +
      guides(
        colour = guide_legend("Test"),
        linetype = guide_legend("Test")
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  # Noise calibration panel (Sec. 4)
  output$n_binom_text <- renderText({
    n <- N_binomial_baseline(input$q0, input$q1, input$alpha_noise, input$beta_noise)
    paste0("Required shots (one-sided baseline binomial): ", ceiling(n))
  })
  
  output$noise_curve <- renderPlot({
    q0_grid <- seq(0.991, 1.0, length.out = 60)
    q1_vals <- c(0.90, 0.99)
    a <- input$alpha_noise; b <- input$beta_noise
    
    # Baseline binomial curves
    df <- crossing(q0 = q0_grid, q1 = q1_vals) |>
      mutate(N = map2_dbl(q0, q1, ~ N_binomial_baseline(.x, .y, a, b)))
    
    # --- NEW: reference lines for inverse & swap at F = q1 (using global Pe, κ) ---
    ref_df <- expand_grid(q1 = q1_vals, test = c("Inverse", "Swap")) |>
      mutate(N_ref = ifelse(test == "Inverse",
                            N_inverse(q1, Pe = input$beta_noise, kappa = input$kappa_noise),
                            N_swap(q1,    Pe = input$beta_noise, kappa = input$kappa_noise)))
    
    ggplot(df, aes(q0, N, color = factor(q1))) +
      geom_line(linewidth = 1) +
      # Reference lines: linetype encodes which test; color matches the q1 curve
      geom_hline(data = ref_df,
                 aes(yintercept = N_ref, linetype = test, color = factor(q1)),
                 linewidth = 0.7) +
      scale_linetype_manual(values = c("Inverse" = "dashed", "Swap" = "dotted")) +
      scale_y_log10() +
      annotation_logticks(sides = "l") +
      labs(
        x = "Baseline q0",
        y = "Shots count",
        color = "Target q1",
        linetype = "Reference (F = q1)",
        title = "Baseline calibration curves (one-sided binomial)",
        subtitle = paste0(
          "Reference lines: Inverse/Swap at F = q1 with Pe = ", signif(input$beta_noise, 3),
          ", κ = ", signif(input$kappa_noise, 3)
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  # Program budgeting panel (Ex. 5.1)
  budget_df <- reactive({
    k <- input$k_blocks
    w_str <- gsub("[\n\t[:space:]]", "", input$weights_csv)
    weights <- as.numeric(unlist(strsplit(w_str, ",")))
    if (length(weights) != k) {
      # If mismatched, pad or trim to length k with 1s
      weights <- rep(1, k)
    }
    Theta <- A_from_F(input$F_prog)
    thetas <- allocate_thetas(weights, Theta)
    tibble(
      block = paste0("f", seq_len(k)),
      weight = weights,
      theta = thetas,
      F_target = F_from_theta(thetas),
      N_inverse = N_block_inverse(thetas, Pe = input$pe_prog, kappa = input$kappa_prog),
      N_swap    = N_block_swap(thetas, Pe = input$pe_prog, kappa = input$kappa_prog)
    ) |> mutate(across(c(F_target), ~round(., 8)))
  })
  
  output$budget_table <- renderTable({
    budget_df() |>
      dplyr::mutate(
        weight   = formatC(weight,   format = "e", digits = 1),
        theta    = formatC(theta,    format = "e", digits = 1),
        F_target = F_target,
        N_inverse = formatC(N_inverse, format = "e", digits = 1),
        N_swap    = formatC(N_swap,    format = "e", digits = 1)
      ) |>
      dplyr::rename(
        `w` = weight,
        `θ` = theta
      )
  })

  output$budget_bar <- renderPlot({
    df <- budget_df() |>
      mutate(block = factor(block, levels = paste0("f", seq_len(n())))) |>
      pivot_longer(cols = c(N_inverse, N_swap), names_to = "Test", values_to = "N")
    
    ggplot(df, aes(x = block, y = N, fill = Test)) +
      geom_col(position = position_dodge()) +
      scale_y_log10() +
      annotation_logticks(sides = "l") +
      labs(x = NULL, y = "Shots count", title = "Per-block shot counts (inverse vs swap)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  # Program budgeting panel (Ex. 5.3)
  # Reset CSV to default A/B/C
  observeEvent(input$arch_reset, {
    updateTextAreaInput(
      session, "arch_csv",
      value = paste(
        "label,g1,g2,n",
        "A,50000,10000,10",
        "B,20000, 4000,40",
        "C,50000,20000,50",
        sep = "\n"
      )
    )
  })
  
  # Parse CSV safely
  parse_arch_csv <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) {
      return(tibble::tibble(label = character(), g1 = numeric(), g2 = numeric(), n = integer()))
    }
    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    df <- tryCatch(utils::read.csv(con, stringsAsFactors = FALSE, strip.white = TRUE), error = function(e) NULL)
    if (is.null(df)) return(tibble::tibble(label = character(), g1 = numeric(), g2 = numeric(), n = integer()))
    names(df) <- tolower(trimws(names(df)))
    needed <- c("label","g1","g2","n")
    if (!all(needed %in% names(df))) {
      return(tibble::tibble(label = character(), g1 = numeric(), g2 = numeric(), n = integer()))
    }
    df$label <- as.character(df$label)
    df$g1 <- suppressWarnings(as.numeric(df$g1))
    df$g2 <- suppressWarnings(as.numeric(df$g2))
    df$n  <- suppressWarnings(as.integer(df$n))
    df |>
      dplyr::filter(!is.na(label) & nzchar(trimws(label))) |>
      dplyr::mutate(
        g1 = ifelse(is.finite(g1) & g1 >= 0, g1, NA_real_),
        g2 = ifelse(is.finite(g2) & g2 >= 0, g2, NA_real_),
        n  = ifelse(is.finite(n)  & n  >= 0, n, NA_integer_)
      ) |>
      tidyr::drop_na(g1, g2, n)
  }
  
  ex53_df <- reactive({
    df <- parse_arch_csv(input$arch_csv)
    validate(
      need(nrow(df) > 0, "Provide at least one valid archetype row in the CSV."),
      need(all(df$g1 >= 0 & df$g2 >= 0 & df$n >= 0), "g1, g2, n must be non-negative.")
    )
    
    Theta_star <- A_from_F(input$F_prog_53)
    r1 <- input$r1_53; r2 <- input$r2_53
    validate(need(is.finite(r1) && is.finite(r2) && r1 >= 0 && r2 >= 0, "Invalid error rates."))
    
    df <- df |>
      dplyr::mutate(
        w = g1 * r1 + g2 * r2
      )
    
    Wtot <- sum(df$n * df$w)
    validate(need(is.finite(Wtot) && Wtot > 0, "Total weight Σ n·w must be positive."))
    
    df |>
      dplyr::mutate(
        theta = Theta_star * (w / Wtot),                  # per-instance θ_j
        F_target = F_from_theta(theta),                   # per-instance fidelity target
        N_inverse_per = N_block_inverse(theta, Pe = input$pe_53, kappa = input$kappa_53)
      )
  })
  
  output$ex53_table <- renderTable({
    df <- ex53_df()
    df |>
      dplyr::mutate(
        w = formatC(w, format = "e", digits = 1),
        theta = formatC(theta, format = "e", digits = 1),
        F_target = signif(F_target, 12),
        N_inverse_per = formatC(N_inverse_per, format = "e", digits = 1)
      ) |>
      dplyr::rename(
        `g(1)` = g1, `g(2)` = g2, `n (instances)` = n,
        `w` = w,
        `θ` = theta,
        `F_target` = F_target,
        `N_inverse` = N_inverse_per
      )
  })
  
  output$ex53_bar <- renderPlot({
    df <- ex53_df()
    ggplot2::ggplot(df, ggplot2::aes(x = label, y = N_inverse_per, fill = label)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_y_log10() +
      ggplot2::annotation_logticks(sides = "l") +
      ggplot2::labs(
        x = NULL, y = "Shots count",
        title = "Per-block shot counts"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "none")
  })
}

shinyApp(ui, server)
