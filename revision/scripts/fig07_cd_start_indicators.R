
# ================================================================
# FIGURE 7
# CONCEPTUAL DESIGN START YEAR AND MARKET READINESS INDICATORS
# NEA SMR DASHBOARD, EDITION 4
#
# a. All designs (n = 52)
# b. 2010–2025 (n = 44)
# c. 2010–2025, excluding microreactors (n = 38)
#
# Method: Proportional-odds ordinal logistic regression
# Predictor: Conceptual design start year, per five-year increase
#
# INPUT:
# revision/data/fig07_cd_start_indicators_source_data.xlsx
#
# OUTPUTS:
# revision/figures/fig07_cd_start_indicators.png
# revision/figures/fig07_cd_start_indicators.pdf
# revision/figures/fig07_cd_start_indicators_results.xlsx
#
# GitHub-compatible version.
# Original statistical analysis and figure appearance preserved.
# ================================================================


# ---------------------------------------------------------------
# 1. PACKAGES
# ---------------------------------------------------------------

packages <- c(
  "readxl", "dplyr", "ggplot2",
  "MASS", "cowplot", "openxlsx"
)

missing_packages <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(openxlsx)
})


# ---------------------------------------------------------------
# 2. GITHUB-COMPATIBLE FILE PATHS
# ---------------------------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig07_cd_start_indicators_source_data.xlsx"
)

find_repo_root <- function(
    start_dir = getwd(),
    target_file = SOURCE_FILE
) {
  current <- normalizePath(
    start_dir,
    winslash = "/",
    mustWork = TRUE
  )
  
  repeat {
    if (file.exists(file.path(current, target_file))) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop(
        "Could not locate repository root.\n",
        "Expected file: ", target_file, "\n",
        "Starting directory: ", start_dir, "\n\n",
        "Run this script from within the repository ",
        "or set the working directory to its root."
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

input_file <- file.path(
  ROOT_DIR,
  SOURCE_FILE
)

output_dir <- file.path(
  ROOT_DIR,
  "revision",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

png_file <- file.path(
  output_dir,
  "fig07_cd_start_indicators.png"
)

pdf_file <- file.path(
  output_dir,
  "fig07_cd_start_indicators.pdf"
)

excel_file <- file.path(
  output_dir,
  "fig07_cd_start_indicators_results.xlsx"
)

if (!file.exists(input_file)) {
  stop("Input workbook not found: ", input_file)
}


# ---------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ---------------------------------------------------------------

clean_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

find_column <- function(df, candidates, description) {
  
  found <- candidates[
    candidates %in% names(df)
  ]
  
  if (length(found) == 0) {
    stop(
      "Missing column: ", description,
      "\nAccepted names: ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  found[1]
}

parse_microreactor <- function(x) {
  
  value <- tolower(
    trimws(as.character(x))
  )
  
  dplyr::case_when(
    value %in% c(
      "true", "t", "1", "yes",
      "microreactor", "micro_reactor",
      "micro reactor"
    ) ~ TRUE,
    
    value %in% c(
      "false", "f", "0", "no",
      "smr", "conventional smr",
      "conventional_smr"
    ) ~ FALSE,
    
    TRUE ~ NA
  )
}


# ---------------------------------------------------------------
# 4. IMPORT DATA
# ---------------------------------------------------------------

sheet_name <- readxl::excel_sheets(input_file)[1]

raw <- readxl::read_excel(
  input_file,
  sheet = sheet_name
)

names(raw) <- clean_names(names(raw))


# ---------------------------------------------------------------
# 5. IDENTIFY VARIABLES
# ---------------------------------------------------------------

design_col <- find_column(
  raw,
  c("design", "smr_design"),
  "Design"
)

year_col <- find_column(
  raw,
  c("cd_start", "conceptual_design_start_year"),
  "Conceptual design start year"
)

micro_col <- find_column(
  raw,
  c(
    "microreactor",
    "micro_reactor",
    "classification"
  ),
  "Microreactor classification"
)

outcome_columns <- c(
  "Financing" = find_column(
    raw, "financing", "Financing"
  ),
  
  "Supply chain" = find_column(
    raw, "supply_chain", "Supply chain"
  ),
  
  "Fuel supply" = find_column(
    raw, c("fuel_supply", "fuel"), "Fuel supply"
  ),
  
  "Licensing" = find_column(
    raw, "licensing", "Licensing"
  ),
  
  "Siting" = find_column(
    raw, "siting", "Siting"
  ),
  
  "Stakeholder engagement" = find_column(
    raw,
    c("stakeholder_engagement", "engagement"),
    "Stakeholder engagement"
  )
)


# ---------------------------------------------------------------
# 6. PREPARE AND VALIDATE DATA
# ---------------------------------------------------------------

data <- raw

data$design_id <- trimws(
  as.character(data[[design_col]])
)

data$cd_year <- suppressWarnings(
  as.numeric(data[[year_col]])
)

data$is_microreactor <- parse_microreactor(
  data[[micro_col]]
)

data <- data[
  !is.na(data$design_id) &
    data$design_id != "" &
    !is.na(data$cd_year),
  ,
  drop = FALSE
]

if (nrow(data) != 52) {
  stop(
    "Expected 52 designs; found ",
    nrow(data)
  )
}

if (anyDuplicated(data$design_id)) {
  stop("Duplicate reactor design names detected.")
}

if (anyNA(data$is_microreactor)) {
  stop("Unrecognised microreactor classification.")
}

if (sum(data$is_microreactor) != 7) {
  stop(
    "Expected seven microreactors; found ",
    sum(data$is_microreactor)
  )
}

if (any(data$cd_year < 1900 | data$cd_year > 2025)) {
  stop("Conceptual design start years outside expected range.")
}

data$cohort <- dplyr::case_when(
  data$cd_year < 2010 ~ "Pre-2010",
  data$cd_year <= 2019 ~ "2010–2019",
  data$cd_year <= 2025 ~ "2020–2025",
  TRUE ~ "Other"
)

# Five-year predictor, centred at 2015.

data$year_5 <- (
  data$cd_year - 2015
) / 5


# ---------------------------------------------------------------
# 7. VALIDATE INDICATOR SCORES
# ---------------------------------------------------------------

outcome_order <- c(
  "Financing",
  "Supply chain",
  "Fuel supply",
  "Licensing",
  "Siting",
  "Stakeholder engagement"
)

for (outcome in outcome_order) {
  
  col <- outcome_columns[[outcome]]
  
  data[[col]] <- suppressWarnings(
    as.numeric(
      as.character(data[[col]])
    )
  )
  
  invalid <- !is.na(data[[col]]) &
    !(data[[col]] %in% 1:6)
  
  if (any(invalid)) {
    stop(
      "Invalid ordinal scores for: ",
      outcome
    )
  }
}


# ---------------------------------------------------------------
# 8. DEFINE ANALYTICAL SAMPLES
# ---------------------------------------------------------------

samples <- list(
  
  "All designs" = data,
  
  "2010–2025" = data[
    data$cd_year >= 2010 &
      data$cd_year <= 2025,
    ,
    drop = FALSE
  ],
  
  "2010–2025 excluding microreactors" = data[
    data$cd_year >= 2010 &
      data$cd_year <= 2025 &
      !data$is_microreactor,
    ,
    drop = FALSE
  ]
)

expected_n <- c(52L, 44L, 38L)

actual_n <- vapply(
  samples,
  nrow,
  integer(1)
)

if (!identical(unname(actual_n), expected_n)) {
  stop(
    "Unexpected analytical sample sizes: ",
    paste(actual_n, collapse = ", "),
    ". Expected 52, 44, 38."
  )
}

cat("\nANALYTICAL SAMPLE SIZES\n")
print(actual_n)


# ---------------------------------------------------------------
# 9. ORDINAL REGRESSION FUNCTION
# ---------------------------------------------------------------

fit_ordinal <- function(
    dataset,
    sample_name,
    outcome_name
) {
  
  col <- outcome_columns[[outcome_name]]
  
  model_data <- data.frame(
    response = ordered(
      dataset[[col]],
      levels = 1:6
    ),
    year_5 = dataset$year_5
  )
  
  model_data <- model_data[
    stats::complete.cases(model_data),
    ,
    drop = FALSE
  ]
  
  n_used <- nrow(model_data)
  
  if (
    n_used < 10 ||
    length(unique(model_data$response)) < 2 ||
    length(unique(model_data$year_5)) < 2
  ) {
    stop(
      "Insufficient variation: ",
      sample_name, " / ", outcome_name
    )
  }
  
  model <- tryCatch(
    MASS::polr(
      response ~ year_5,
      data = model_data,
      method = "logistic",
      Hess = TRUE
    ),
    error = function(e) {
      stop(
        "Model failed: ",
        sample_name, " / ", outcome_name,
        "\n", conditionMessage(e)
      )
    }
  )
  
  null_model <- MASS::polr(
    response ~ 1,
    data = model_data,
    method = "logistic",
    Hess = TRUE
  )
  
  coefficient <- coef(summary(model))[
    "year_5",
    ,
    drop = FALSE
  ]
  
  beta <- unname(
    coefficient[1, "Value"]
  )
  
  se <- unname(
    coefficient[1, "Std. Error"]
  )
  
  if (
    !is.finite(beta) ||
    !is.finite(se) ||
    se <= 0
  ) {
    stop(
      "Invalid coefficient or standard error: ",
      sample_name, " / ", outcome_name
    )
  }
  
  p_value <- 2 * stats::pnorm(
    abs(beta / se),
    lower.tail = FALSE
  )
  
  ll_full <- as.numeric(
    stats::logLik(model)
  )
  
  ll_null <- as.numeric(
    stats::logLik(null_model)
  )
  
  lr_stat <- 2 * (ll_full - ll_null)
  
  lr_p <- stats::pchisq(
    lr_stat,
    df = 1,
    lower.tail = FALSE
  )
  
  data.frame(
    sample = sample_name,
    outcome = outcome_name,
    n = n_used,
    
    beta_per_5_years = beta,
    standard_error = se,
    
    odds_ratio = exp(beta),
    
    ci_low = exp(
      beta - 1.96 * se
    ),
    
    ci_high = exp(
      beta + 1.96 * se
    ),
    
    p_value = p_value,
    
    lr_statistic = lr_stat,
    lr_p_value = lr_p,
    
    mcfadden_r2 = 1 - (
      ll_full / ll_null
    ),
    
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------
# 10. ESTIMATE ALL 18 MODELS
# ---------------------------------------------------------------

results_list <- list()

counter <- 1

for (sample_name in names(samples)) {
  
  for (outcome_name in outcome_order) {
    
    results_list[[counter]] <- fit_ordinal(
      dataset = samples[[sample_name]],
      sample_name = sample_name,
      outcome_name = outcome_name
    )
    
    counter <- counter + 1
  }
}

results <- dplyr::bind_rows(
  results_list
)

if (nrow(results) != 18) {
  stop("Expected 18 ordinal regression results.")
}

# Holm adjustment across six outcomes within each sample.

results <- results %>%
  dplyr::group_by(sample) %>%
  dplyr::mutate(
    p_holm = stats::p.adjust(
      p_value,
      method = "holm"
    )
  ) %>%
  dplyr::ungroup()

cat("\nORDINAL REGRESSION RESULTS\n")
print(results)


# ---------------------------------------------------------------
# 11. SAMPLE SUMMARY
# ---------------------------------------------------------------

sample_summary <- dplyr::bind_rows(
  lapply(
    names(samples),
    function(name) {
      
      df <- samples[[name]]
      
      data.frame(
        sample = name,
        n = nrow(df),
        microreactors = sum(df$is_microreactor),
        conventional_smrs = sum(!df$is_microreactor),
        earliest_start = min(df$cd_year),
        latest_start = max(df$cd_year)
      )
    }
  )
)


# ---------------------------------------------------------------
# 12. INDICATOR DESCRIPTIVES
# ---------------------------------------------------------------

descriptive_list <- list()

counter <- 1

for (sample_name in names(samples)) {
  
  df <- samples[[sample_name]]
  
  for (outcome_name in outcome_order) {
    
    scores <- df[[
      outcome_columns[[outcome_name]]
    ]]
    
    scores <- scores[
      !is.na(scores)
    ]
    
    descriptive_list[[counter]] <- data.frame(
      sample = sample_name,
      outcome = outcome_name,
      n = length(scores),
      mean_score = mean(scores),
      median_score = median(scores),
      sd_score = stats::sd(scores),
      min_score = min(scores),
      max_score = max(scores)
    )
    
    counter <- counter + 1
  }
}

indicator_descriptives <- dplyr::bind_rows(
  descriptive_list
)


# ---------------------------------------------------------------
# 13. ORIGINAL FIGURE COLOURS
# ---------------------------------------------------------------

dimension_colors <- c(
  "Financing" = "#2A9D8F",
  "Supply chain" = "#4C78A8",
  "Fuel supply" = "#9467BD",
  "Licensing" = "#9E2A3F",
  "Siting" = "#D18F22",
  "Stakeholder engagement" = "#666666"
)


# ---------------------------------------------------------------
# 14. ORIGINAL FIGURE FUNCTION
# ---------------------------------------------------------------

make_panel <- function(
    sample_name,
    panel_title,
    panel_label
) {
  
  plot_data <- results[
    results$sample == sample_name,
    ,
    drop = FALSE
  ]
  
  plot_data$outcome <- factor(
    plot_data$outcome,
    levels = rev(outcome_order)
  )
  
  # Preserve individual logarithmic x-axis ranges.
  
  x_values <- c(
    plot_data$ci_low,
    plot_data$ci_high,
    plot_data$odds_ratio,
    1
  )
  
  x_values <- x_values[
    is.finite(x_values) &
      x_values > 0
  ]
  
  x_limits <- exp(
    range(log(x_values)) +
      c(-0.12, 0.12)
  )
  
  candidate_breaks <- c(
    0.05, 0.1, 0.2, 0.3,
    0.5, 0.75, 1, 1.5,
    2, 3, 5, 10, 20
  )
  
  x_breaks <- candidate_breaks[
    candidate_breaks >= x_limits[1] &
      candidate_breaks <= x_limits[2]
  ]
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = odds_ratio,
      y = outcome,
      colour = outcome
    )
  ) +
    
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey35",
      linewidth = 0.45
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_low,
        xend = ci_high,
        yend = outcome
      ),
      linewidth = 0.85
    ) +
    
    ggplot2::geom_point(
      size = 2.6
    ) +
    
    ggplot2::scale_colour_manual(
      values = dimension_colors,
      guide = "none"
    ) +
    
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = function(x) {
        format(
          x,
          trim = TRUE,
          scientific = FALSE
        )
      },
      minor_breaks = NULL
    ) +
    
    ggplot2::labs(
      title = panel_title,
      x = "Odds ratio per five-year increase (log scale)",
      y = NULL
    ) +
    
    ggplot2::theme_minimal(
      base_size = 10
    ) +
    
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      
      axis.line.x = ggplot2::element_line(
        colour = "black",
        linewidth = 0.35
      ),
      
      axis.ticks.x = ggplot2::element_blank(),
      
      axis.text.x = ggplot2::element_text(
        colour = "black",
        size = 9
      ),
      
      axis.text.y = ggplot2::element_text(
        colour = "black",
        size = 9
      ),
      
      axis.title.x = ggplot2::element_text(
        colour = "black",
        size = 9
      ),
      
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 11,
        hjust = 0.5,
        colour = "black"
      ),
      
      plot.margin = ggplot2::margin(
        10, 10, 10, 12
      )
    )
  
  cowplot::ggdraw(p) +
    cowplot::draw_plot_label(
      panel_label,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 13,
      fontface = "bold"
    )
}


# ---------------------------------------------------------------
# 15. BUILD THREE PANELS
# ---------------------------------------------------------------

panel_a <- make_panel(
  "All designs",
  paste0(
    "All designs (n = ",
    nrow(samples[["All designs"]]),
    ")"
  ),
  "a"
)

panel_b <- make_panel(
  "2010–2025",
  paste0(
    "2010–2025 (n = ",
    nrow(samples[["2010–2025"]]),
    ")"
  ),
  "b"
)

panel_c <- make_panel(
  "2010–2025 excluding microreactors",
  paste0(
    "2010–2025, excluding microreactors (n = ",
    nrow(samples[["2010–2025 excluding microreactors"]]),
    ")"
  ),
  "c"
)

combined_figure <- cowplot::plot_grid(
  panel_a,
  panel_b,
  panel_c,
  nrow = 1,
  align = "h",
  axis = "tb",
  rel_widths = c(1, 1, 1)
)

print(combined_figure)


# ---------------------------------------------------------------
# 16. EXPORT FIGURE 7
# ---------------------------------------------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = combined_figure,
  width = 15,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = combined_figure,
  width = 15,
  height = 5.5,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# ---------------------------------------------------------------
# 17. DESIGN AUDIT
# ---------------------------------------------------------------

design_audit <- data.frame(
  design = data$design_id,
  cd_start = data$cd_year,
  cohort = data$cohort,
  microreactor = data$is_microreactor,
  
  financing = data[[
    outcome_columns[["Financing"]]
  ]],
  
  supply_chain = data[[
    outcome_columns[["Supply chain"]]
  ]],
  
  fuel_supply = data[[
    outcome_columns[["Fuel supply"]]
  ]],
  
  licensing = data[[
    outcome_columns[["Licensing"]]
  ]],
  
  siting = data[[
    outcome_columns[["Siting"]]
  ]],
  
  stakeholder_engagement = data[[
    outcome_columns[["Stakeholder engagement"]]
  ]]
)

design_audit <- design_audit %>%
  dplyr::arrange(
    cd_start,
    design
  )


# ---------------------------------------------------------------
# 18. EXPORT SINGLE EXCEL WORKBOOK
# ---------------------------------------------------------------

wb <- openxlsx::createWorkbook()

tables <- list(
  "Ordinal regression results" = results,
  "Sample summary" = sample_summary,
  "Indicator descriptives" = indicator_descriptives,
  "Design audit" = design_audit
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1",
  border = "Bottom",
  borderColour = "#A6A6A6"
)

for (sheet in names(tables)) {
  
  openxlsx::addWorksheet(
    wb,
    sheet
  )
  
  openxlsx::writeData(
    wb,
    sheet,
    tables[[sheet]],
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(tables[[sheet]])),
    widths = "auto"
  )
}

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# ---------------------------------------------------------------
# 19. VERIFY OUTPUT FILES
# ---------------------------------------------------------------

output_files <- c(
  png_file,
  pdf_file,
  excel_file
)

missing_outputs <- output_files[
  !file.exists(output_files)
]

if (length(missing_outputs) > 0) {
  stop(
    "The following outputs were not created:\n",
    paste(missing_outputs, collapse = "\n")
  )
}


# ---------------------------------------------------------------
# 20. FINAL CONSOLE OUTPUT
# ---------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("FIGURE 7 COMPLETE\n")
cat("============================================\n\n")

cat("Analytical samples:\n")
print(sample_summary)

cat("\nOrdinal regression results:\n")

print(
  results[
    ,
    c(
      "sample",
      "outcome",
      "n",
      "odds_ratio",
      "ci_low",
      "ci_high",
      "p_value",
      "p_holm"
    )
  ],
  row.names = FALSE
)

cat("\nPNG:\n", png_file)
cat("\nPDF:\n", pdf_file)
cat("\nExcel:\n", excel_file)

cat("\nAll Figure 7 outputs saved successfully.\n")
