
# ================================================================
# FIGURE 9
# NEA SMR DASHBOARD — EDITION 4
# REGIONAL MARKET-READINESS ASSOCIATIONS
#
# a. All designs (n = 89)
# b. Excluding microreactors (n = 51)
#
# Reference: North America
#
# Original regional colours and two-panel layout retained.
#
# INPUT:
# revision/data/fig09_nea_regional_source_data.xlsx
#
# OUTPUTS:
# revision/figures/Figure 9.png
# revision/figures/Figure 9.pdf
# revision/figures/Figure 9_statistics.xlsx
# ================================================================


# 1. RESET -------------------------------------------------------

rm(list = ls())
graphics.off()


# 2. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "MASS",
  "ggplot2",
  "scales",
  "patchwork",
  "openxlsx"
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
  library(tidyr)
  library(stringr)
  library(MASS)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(openxlsx)
})


# 3. GITHUB-COMPATIBLE PATHS -------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig09_nea_regional_source_data.xlsx"
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
        "Starting directory: ", start_dir, "\n",
        "Run this script from within the repository."
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

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ",
    input_file
  )
}


# 4. IMPORT ------------------------------------------------------

sheets <- readxl::excel_sheets(input_file)

sheet_name <- if ("nea_50" %in% sheets) {
  "nea_50"
} else {
  sheets[1]
}

raw <- readxl::read_excel(
  input_file,
  sheet = sheet_name
)

names(raw) <- trimws(names(raw))


# 5. VARIABLES ---------------------------------------------------

outcomes <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting",
  "stakeholder_engagement"
)

outcome_labels <- c(
  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  licensing = "Licensing",
  siting = "Siting",
  stakeholder_engagement = "Stakeholder engagement"
)

required <- c(
  "design",
  "region_cn_ru",
  "microreactor",
  outcomes
)

missing_cols <- setdiff(
  required,
  names(raw)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


# 6. CLEAN TEXT --------------------------------------------------

clean_text <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "\u00A0",
    " ",
    x,
    fixed = TRUE
  )
  
  stringr::str_squish(x)
}

normalise <- function(x) {
  tolower(clean_text(x))
}


# 7. FILTER EDITION 4 --------------------------------------------

dat <- raw

if ("edition" %in% names(dat)) {
  
  dat <- dat %>%
    dplyr::filter(
      suppressWarnings(
        as.numeric(as.character(edition))
      ) == 4
    )
}

dat <- dat %>%
  dplyr::mutate(
    design = clean_text(design)
  ) %>%
  dplyr::filter(
    !is.na(design),
    design != "",
    tolower(design) != "na",
    toupper(design) != "DF-CDE"
  )

if (nrow(dat) != 91) {
  stop(
    "Expected 91 designs; found ",
    nrow(dat)
  )
}

if (anyDuplicated(dat$design)) {
  stop("Duplicate design names detected.")
}


# 8. REGIONAL CODING ---------------------------------------------

region_levels <- c(
  "North America",
  "Europe",
  "OECD Asia",
  "China/Russia"
)

dat <- dat %>%
  dplyr::mutate(
    
    region_raw = normalise(region_cn_ru),
    
    region_group = dplyr::case_when(
      
      region_raw %in% c(
        "north america",
        "north_america"
      ) ~ "North America",
      
      region_raw == "europe" ~ "Europe",
      
      region_raw %in% c(
        "oecd asia",
        "oecd_asia"
      ) ~ "OECD Asia",
      
      region_raw %in% c(
        "china/russia",
        "china / russia",
        "china_russia",
        "china",
        "russia"
      ) ~ "China/Russia",
      
      TRUE ~ NA_character_
    )
  )

excluded_regions <- dat %>%
  dplyr::filter(is.na(region_group)) %>%
  dplyr::select(
    design,
    region_cn_ru
  )

if (nrow(excluded_regions) != 2) {
  stop(
    "Expected two Rest of world exclusions; found ",
    nrow(excluded_regions)
  )
}

dat <- dat %>%
  dplyr::filter(!is.na(region_group)) %>%
  dplyr::mutate(
    region_group = factor(
      region_group,
      levels = region_levels
    )
  )

if (nrow(dat) != 89) {
  stop("Expected 89 regional designs.")
}


# 9. VERIFY REGIONAL COUNTS --------------------------------------

regional_counts <- dat %>%
  dplyr::count(
    region_group,
    name = "n"
  )

expected_counts <- c(
  "North America" = 40,
  "Europe" = 26,
  "OECD Asia" = 12,
  "China/Russia" = 11
)

actual_counts <- stats::setNames(
  regional_counts$n,
  as.character(regional_counts$region_group)
)

if (!identical(
  as.integer(actual_counts[names(expected_counts)]),
  as.integer(expected_counts)
)) {
  stop("Regional counts differ from verified data.")
}


# 10. MICROREACTOR CODING ----------------------------------------

micro_raw <- normalise(dat$microreactor)

dat$is_microreactor <- dplyr::case_when(
  
  micro_raw %in% c(
    "true", "t", "1", "yes", "y",
    "microreactor", "micro_reactor", "micro"
  ) ~ TRUE,
  
  micro_raw %in% c(
    "false", "f", "0", "no", "n",
    "smr", "conventional",
    "conventional smr", "non-microreactor"
  ) ~ FALSE,
  
  TRUE ~ NA
)

if (any(is.na(dat$is_microreactor))) {
  
  print(
    dat %>%
      dplyr::filter(is.na(is_microreactor)) %>%
      dplyr::select(design, microreactor)
  )
  
  stop("Unrecognised microreactor coding.")
}


# 11. READINESS VARIABLES ----------------------------------------

for (v in outcomes) {
  
  dat[[v]] <- suppressWarnings(
    as.numeric(as.character(dat[[v]]))
  )
  
  if (any(
    !is.na(dat[[v]]) &
    !(dat[[v]] %in% 1:6)
  )) {
    stop("Invalid readiness scores in: ", v)
  }
}


# 12. DEFINE SAMPLES ---------------------------------------------

samples <- list(
  
  all_designs = dat,
  
  conventional = dat %>%
    dplyr::filter(!is_microreactor)
)

expected_n <- c(
  all_designs = 89,
  conventional = 51
)

for (s in names(samples)) {
  
  if (nrow(samples[[s]]) != expected_n[[s]]) {
    stop(
      "Unexpected sample size for ",
      s,
      ": ",
      nrow(samples[[s]])
    )
  }
}

sample_counts <- dplyr::bind_rows(
  lapply(names(samples), function(s) {
    
    samples[[s]] %>%
      dplyr::count(region_group, name = "n") %>%
      dplyr::mutate(sample = s, .before = 1)
    
  })
)


# 13. REGRESSION FUNCTION ----------------------------------------

fit_model <- function(df, outcome, sample_name) {
  
  tmp <- df %>%
    dplyr::select(
      region_group,
      dplyr::all_of(outcome)
    ) %>%
    dplyr::rename(
      score = dplyr::all_of(outcome)
    ) %>%
    dplyr::filter(
      !is.na(region_group),
      !is.na(score)
    ) %>%
    dplyr::mutate(
      score = ordered(score, levels = 1:6),
      region_group = droplevels(region_group)
    )
  
  if (dplyr::n_distinct(tmp$region_group) != 4) {
    stop(
      "Missing region in ",
      sample_name,
      " / ",
      outcome
    )
  }
  
  model_warnings <- character(0)
  
  full <- withCallingHandlers(
    
    MASS::polr(
      score ~ region_group,
      data = tmp,
      Hess = TRUE,
      method = "logistic",
      control = list(maxit = 2000)
    ),
    
    warning = function(w) {
      
      model_warnings <<- c(
        model_warnings,
        conditionMessage(w)
      )
      
      invokeRestart("muffleWarning")
    }
  )
  
  null <- MASS::polr(
    score ~ 1,
    data = tmp,
    Hess = TRUE,
    method = "logistic",
    control = list(maxit = 2000)
  )
  
  log_full <- as.numeric(logLik(full))
  log_null <- as.numeric(logLik(null))
  
  lr <- max(
    0,
    2 * (log_full - log_null)
  )
  
  df_lr <- attr(logLik(full), "df") -
    attr(logLik(null), "df")
  
  p_lr <- stats::pchisq(
    lr,
    df = df_lr,
    lower.tail = FALSE
  )
  
  omnibus <- data.frame(
    sample = sample_name,
    outcome = outcome,
    n = nrow(tmp),
    lr_statistic = lr,
    df = df_lr,
    p_lr = p_lr,
    mcfadden_r2 = 1 - log_full / log_null
  )
  
  ct <- coef(summary(full))
  
  terms <- grep(
    "^region_group",
    rownames(ct),
    value = TRUE
  )
  
  contrasts <- dplyr::bind_rows(
    lapply(terms, function(term) {
      
      beta <- ct[term, "Value"]
      se <- ct[term, "Std. Error"]
      
      data.frame(
        sample = sample_name,
        outcome = outcome,
        reference = "North America",
        
        comparison = sub(
          "^region_group",
          "",
          term
        ),
        
        n = nrow(tmp),
        coefficient = beta,
        standard_error = se,
        
        odds_ratio = exp(beta),
        
        ci_low = exp(beta - 1.96 * se),
        ci_high = exp(beta + 1.96 * se),
        
        p_wald = 2 * stats::pnorm(
          abs(beta / se),
          lower.tail = FALSE
        )
      )
    })
  )
  
  cells <- table(
    tmp$region_group,
    factor(tmp$score, levels = 1:6)
  )
  
  diagnostics <- data.frame(
    sample = sample_name,
    outcome = outcome,
    n = nrow(tmp),
    
    zero_cells = sum(cells == 0),
    
    max_abs_coefficient = max(
      abs(coef(full))
    ),
    
    max_standard_error = max(
      ct[terms, "Std. Error"]
    ),
    
    convergence_code = full$convergence,
    
    warnings = paste(
      unique(model_warnings),
      collapse = " | "
    )
  )
  
  list(
    omnibus = omnibus,
    contrasts = contrasts,
    diagnostics = diagnostics
  )
}


# 14. RUN ALL MODELS ---------------------------------------------

results <- list()

for (s in names(samples)) {
  
  for (v in outcomes) {
    
    key <- paste(s, v, sep = "__")
    
    cat("\nFitting:", key, "\n")
    
    results[[key]] <- fit_model(
      samples[[s]],
      v,
      s
    )
  }
}


# 15. COMBINE RESULTS --------------------------------------------

omnibus <- dplyr::bind_rows(
  lapply(results, `[[`, "omnibus")
)

contrasts <- dplyr::bind_rows(
  lapply(results, `[[`, "contrasts")
)

diagnostics <- dplyr::bind_rows(
  lapply(results, `[[`, "diagnostics")
)

omnibus <- omnibus %>%
  dplyr::group_by(sample) %>%
  dplyr::mutate(
    p_bh = stats::p.adjust(p_lr, method = "BH")
  ) %>%
  dplyr::ungroup()

contrasts <- contrasts %>%
  dplyr::group_by(sample, outcome) %>%
  dplyr::mutate(
    p_bh_within_outcome = stats::p.adjust(
      p_wald,
      method = "BH"
    )
  ) %>%
  dplyr::ungroup()

diagnostics <- diagnostics %>%
  dplyr::mutate(
    instability_flag =
      max_abs_coefficient > 10 |
      max_standard_error > 10 |
      convergence_code != 0 |
      warnings != ""
  )


# 16. PLOT DATA --------------------------------------------------

plot_df <- contrasts %>%
  dplyr::mutate(
    
    outcome_label = factor(
      unname(outcome_labels[outcome]),
      levels = rev(unname(outcome_labels))
    ),
    
    comparison = factor(
      comparison,
      levels = c(
        "Europe",
        "OECD Asia",
        "China/Russia"
      )
    )
  )

if (nrow(plot_df) != 36) {
  stop("Expected 36 regional estimates.")
}

if (any(
  !is.finite(plot_df$odds_ratio) |
  !is.finite(plot_df$ci_low) |
  !is.finite(plot_df$ci_high) |
  plot_df$ci_low <= 0
)) {
  stop(
    "Invalid odds ratios or confidence intervals. ",
    "Inspect model diagnostics."
  )
}


# 17. ORIGINAL REGIONAL COLOURS ----------------------------------

region_cols <- c(
  "North America" = "#4C78A8",
  "Russia" = "#E15759",
  "Asia" = "#9C6ADE",
  "Europe" = "#59A14F",
  "Rest of the World" = "#9D9D9D"
)

comparison_cols <- c(
  "Europe" = region_cols[["Europe"]],
  "OECD Asia" = region_cols[["Asia"]],
  "China/Russia" = region_cols[["Russia"]]
)


# 18. SHARED LOGARITHMIC AXIS ------------------------------------

x_min <- min(
  plot_df$ci_low,
  na.rm = TRUE
)

x_max <- max(
  plot_df$ci_high,
  na.rm = TRUE
)

x_limits <- c(
  10^floor(log10(x_min)),
  10^ceiling(log10(x_max))
)

x_limits[1] <- min(x_limits[1], 0.1)
x_limits[2] <- max(x_limits[2], 10)

dodge <- ggplot2::position_dodge(
  width = 0.58
)


# 19. ORIGINAL PANEL FUNCTION ------------------------------------

make_panel <- function(
    sample_name,
    panel_title
) {
  
  df <- plot_df %>%
    dplyr::filter(sample == sample_name)
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = odds_ratio,
      y = outcome_label,
      colour = comparison,
      group = comparison
    )
  ) +
    
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.5,
      colour = "grey45"
    ) +
    
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = ci_low,
        xmax = ci_high
      ),
      orientation = "y",
      position = dodge,
      width = 0,
      linewidth = 0.75
    ) +
    
    ggplot2::geom_point(
      position = dodge,
      size = 2.7
    ) +
    
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = scales::breaks_log(
        base = 10
      ),
      labels = scales::label_number()
    ) +
    
    ggplot2::scale_colour_manual(
      values = comparison_cols,
      breaks = c(
        "Europe",
        "OECD Asia",
        "China/Russia"
      ),
      drop = FALSE
    ) +
    
    ggplot2::labs(
      title = panel_title,
      x = "Odds ratio (log scale)",
      y = NULL,
      colour = "Reference: North America   |"
    ) +
    
    ggplot2::theme_classic(
      base_size = 10
    ) +
    
    ggplot2::theme(
      
      plot.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(
          b = 12
        )
      ),
      
      axis.text.x = ggplot2::element_text(
        size = 9,
        colour = "black"
      ),
      
      axis.text.y = ggplot2::element_text(
        size = 9,
        colour = "black"
      ),
      
      axis.title.x = ggplot2::element_text(
        size = 10,
        margin = ggplot2::margin(
          t = 10
        )
      ),
      
      axis.line = ggplot2::element_line(
        colour = "black",
        linewidth = 0.45
      ),
      
      axis.ticks = ggplot2::element_line(
        colour = "black",
        linewidth = 0.35
      ),
      
      panel.border = ggplot2::element_blank(),
      
      panel.grid = ggplot2::element_blank(),
      
      legend.position = "bottom",
      
      legend.direction = "horizontal",
      
      legend.title = ggplot2::element_text(
        size = 9,
        face = "plain"
      ),
      
      legend.text = ggplot2::element_text(
        size = 9
      ),
      
      legend.key = ggplot2::element_blank(),
      
      plot.margin = ggplot2::margin(
        15, 12, 5, 12
      )
    )
}


# 20. BUILD PANELS -----------------------------------------------

panel_a <- make_panel(
  sample_name = "all_designs",
  panel_title = "All designs (n = 89)"
)

panel_b <- make_panel(
  sample_name = "conventional",
  panel_title = "Excluding microreactors (n = 51)"
)


# 21. COMBINE PANELS ---------------------------------------------

final_plot <- (
  panel_a + panel_b
) +
  
  patchwork::plot_layout(
    ncol = 2,
    guides = "collect"
  ) +
  
  patchwork::plot_annotation(
    tag_levels = "a"
  ) &
  
  ggplot2::theme(
    legend.position = "bottom",
    
    plot.tag = ggplot2::element_text(
      size = 12,
      face = "bold",
      colour = "black"
    ),
    
    plot.tag.position = c(
      0.01,
      0.99
    ),
    
    legend.box = "horizontal",
    
    legend.spacing.x = grid::unit(
      0.25,
      "cm"
    )
  )

print(final_plot)


# 22. EXPORT PNG AND PDF -----------------------------------------

png_file <- file.path(
  output_dir,
  "Figure 9.png"
)

pdf_file <- file.path(
  output_dir,
  "Figure 9.pdf"
)

ggplot2::ggsave(
  filename = png_file,
  plot = final_plot,
  width = 13,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = final_plot,
  width = 13,
  height = 6.5,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# 23. DESIGN AUDIT -----------------------------------------------

design_audit <- dat %>%
  dplyr::select(
    design,
    region_cn_ru,
    region_group,
    microreactor,
    is_microreactor,
    dplyr::all_of(outcomes)
  )


# 24. EXCEL WORKBOOK ---------------------------------------------

excel_tables <- list(
  
  "Sample counts" = sample_counts,
  
  "Omnibus tests" = omnibus,
  
  "Regional odds ratios" = contrasts,
  
  "Model diagnostics" = diagnostics,
  
  "Forest plot data" = plot_df,
  
  "Excluded regions" = excluded_regions,
  
  "Design audit" = design_audit
)

excel_file <- file.path(
  output_dir,
  "Figure 9_statistics.xlsx"
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1",
  border = "Bottom",
  borderColour = "#A6A6A6"
)

for (sheet in names(excel_tables)) {
  
  sheet_data <- as.data.frame(
    excel_tables[[sheet]]
  )
  
  openxlsx::addWorksheet(
    wb,
    sheet
  )
  
  openxlsx::writeData(
    wb,
    sheet,
    sheet_data,
    headerStyle = header_style,
    withFilter = nrow(sheet_data) > 0
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(sheet_data)),
    widths = "auto"
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
}

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# 25. VERIFY OUTPUT FILES ----------------------------------------

output_files <- c(
  png_file,
  pdf_file,
  excel_file
)

for (f in output_files) {
  
  if (
    !file.exists(f) ||
    is.na(file.info(f)$size) ||
    file.info(f)$size == 0
  ) {
    
    stop(
      "Missing or empty output file: ",
      f
    )
  }
}


# 26. FINAL SUMMARY ----------------------------------------------

cat("\n")
cat("================================================\n")
cat("FIGURE 9 — REGIONAL ANALYSIS COMPLETE\n")
cat("================================================\n")

cat("\nReference: North America\n")

cat("\nSAMPLE COUNTS:\n")
print(sample_counts)

cat("\nOMNIBUS RESULTS:\n")
print(omnibus)

cat("\nREGIONAL ODDS RATIOS:\n")
print(contrasts)

cat("\nMODEL DIAGNOSTICS:\n")
print(diagnostics)

cat("\nOUTPUT FILES:\n")

for (f in output_files) {
  cat(basename(f), "\n")
}

cat("\nOutput directory:\n")
cat(output_dir, "\n")

cat("\nAnalysis complete.\n")
