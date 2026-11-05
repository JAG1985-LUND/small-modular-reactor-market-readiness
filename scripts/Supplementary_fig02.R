
# ================================================================
# SUPPLEMENTARY FIGURE S2
# TECHNOLOGICAL CORRELATES OF MARKET READINESS
# NEA SMR DASHBOARD, EDITION 4
#
# CONVENTIONAL SMRs ONLY (n = 53)
# MICROREACTORS EXCLUDED
#
# Panel a: Reactor cooling type
# Panel b: Neutron spectrum
# Panel c: Fuel type
# Panel d: Fuel enrichment
#
# Five ordinal outcomes:
# Financing, Supply chain, Fuel supply, Licensing, Siting
#
# INPUT:
# revision/data/fig08_nea_technological_source_data.xlsx
#
# OUTPUTS:
# revision/figures/supp_fig_S02_technological_excluding_microreactors.png
# revision/figures/supp_fig_S02_technological_excluding_microreactors.pdf
# revision/figures/supp_fig_S02_technological_excluding_microreactors_results.xlsx
#
# Original four-panel formatting and analysis retained.
# ================================================================


# 1. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "MASS",
  "cowplot",
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
  library(ggplot2)
  library(cowplot)
  library(openxlsx)
})


# 2. GITHUB-COMPATIBLE PATHS -------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig08_nea_technological_source_data.xlsx"
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
        "Expected: ", target_file
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

out_dir <- file.path(
  ROOT_DIR,
  "revision",
  "figures"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_name <- "supp_fig_S02_technological_excluding_microreactors"

png_file <- file.path(
  out_dir,
  paste0(output_name, ".png")
)

pdf_file <- file.path(
  out_dir,
  paste0(output_name, ".pdf")
)

excel_file <- file.path(
  out_dir,
  paste0(output_name, "_results.xlsx")
)

if (!file.exists(input_file)) {
  stop("Input workbook not found: ", input_file)
}


# 3. HELPER FUNCTIONS --------------------------------------------

clean_names <- function(x) {
  
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  
  make.unique(x, sep = "_")
}

normalise <- function(x) {
  
  x <- tolower(trimws(as.character(x)))
  
  x[x %in% c(
    "", "na", "n/a",
    "unknown", "not reported"
  )] <- NA_character_
  
  x
}

find_column <- function(
    df,
    candidates,
    description,
    required = TRUE
) {
  
  found <- candidates[
    candidates %in% names(df)
  ]
  
  if (length(found) > 0) {
    return(found[1])
  }
  
  if (required) {
    stop(
      "Missing column: ", description,
      "\nAccepted: ",
      paste(candidates, collapse = ", "),
      "\nAvailable: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  NA_character_
}

make_outcome <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(as.character(x))
  )
  
  invalid <- !is.na(x) & !(x %in% 1:6)
  
  if (any(invalid)) {
    stop("Invalid ordinal readiness score.")
  }
  
  ordered(x, levels = 1:6)
}


# 4. IMPORT WORKBOOK ---------------------------------------------

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

names(raw) <- clean_names(names(raw))


# 5. IDENTIFY COLUMNS --------------------------------------------

design_col <- find_column(
  raw,
  c("design", "smr_design", "reactor_design"),
  "Design"
)

edition_col <- find_column(
  raw,
  c("edition", "dashboard_edition"),
  "Edition",
  FALSE
)

micro_col <- find_column(
  raw,
  c(
    "microreactor",
    "micro_reactor",
    "classification",
    "reactor_classification"
  ),
  "Reactor classification"
)

cooling_col <- find_column(
  raw,
  c(
    "water_cooled",
    "water_cooled_binary",
    "cooling_type",
    "cooling"
  ),
  "Cooling type"
)

spectrum_col <- find_column(
  raw,
  c(
    "spectrum",
    "neutron_spectrum",
    "neturon_spectrum"
  ),
  "Neutron spectrum"
)

fuel_col <- find_column(
  raw,
  c(
    "fuel_type_recoded",
    "fuel_type",
    "fuel_form"
  ),
  "Fuel type"
)

enrichment_col <- find_column(
  raw,
  c(
    "fuel_enrichment_recode",
    "fuel_enrich",
    "fuel_enrichment",
    "enrichment"
  ),
  "Fuel enrichment"
)

outcome_columns <- c(
  Financing = find_column(
    raw, "financing", "Financing"
  ),
  
  `Supply chain` = find_column(
    raw, "supply_chain", "Supply chain"
  ),
  
  `Fuel supply` = find_column(
    raw, "fuel_supply", "Fuel supply"
  ),
  
  Licensing = find_column(
    raw, "licensing", "Licensing"
  ),
  
  Siting = find_column(
    raw, "siting", "Siting"
  )
)


# 6. FILTER EDITION 4 --------------------------------------------

data <- raw

if (!is.na(edition_col)) {
  
  edition_values <- suppressWarnings(
    as.numeric(as.character(data[[edition_col]]))
  )
  
  data <- data[
    !is.na(edition_values) &
      edition_values == 4,
    ,
    drop = FALSE
  ]
}

data$design_id <- trimws(
  as.character(data[[design_col]])
)

data <- data[
  !is.na(data$design_id) &
    data$design_id != "" &
    toupper(data$design_id) != "DF-CDE",
  ,
  drop = FALSE
]

if (anyDuplicated(toupper(data$design_id))) {
  stop("Duplicate reactor design names detected.")
}

if (nrow(data) != 91) {
  stop(
    "Expected 91 designs before excluding microreactors; found ",
    nrow(data)
  )
}


# 7. CLASSIFY AND EXCLUDE MICROREACTORS --------------------------

micro_raw <- normalise(data[[micro_col]])

data$reactor_class <- dplyr::case_when(
  
  micro_raw %in% c(
    "true", "t", "1", "yes",
    "microreactor", "micro_reactor",
    "micro reactor"
  ) ~ "Microreactor",
  
  micro_raw %in% c(
    "false", "f", "0", "no",
    "conventional",
    "conventional_smr",
    "conventional smr",
    "smr"
  ) ~ "Conventional SMR",
  
  TRUE ~ NA_character_
)

if (anyNA(data$reactor_class)) {
  stop("Unclassified reactor designs detected.")
}

data <- data[
  data$reactor_class == "Conventional SMR",
  ,
  drop = FALSE
]

if (nrow(data) != 53) {
  stop(
    "Expected 53 conventional SMRs; found ",
    nrow(data)
  )
}

cat(
  "\nSupplementary analytical sample: ",
  nrow(data),
  " conventional SMRs\n",
  sep = ""
)


# 8. TECHNOLOGICAL PREDICTORS ------------------------------------

# A. Cooling type
# Reference: Non-water-cooled

cooling_raw <- normalise(data[[cooling_col]])

data$cooling_group <- dplyr::case_when(
  
  cooling_raw %in% c(
    "true", "1", "yes",
    "water", "water-cooled",
    "water_cooled", "water cooled",
    "watercooled"
  ) ~ "Water-cooled",
  
  cooling_raw %in% c(
    "false", "0", "no",
    "non-water-cooled",
    "non_water_cooled",
    "non water cooled",
    "non-water", "non_water",
    "gas-cooled", "gas_cooled",
    "metal-cooled", "metal_cooled",
    "molten-salt-cooled",
    "molten_salt_cooled",
    "heat-pipe-cooled",
    "heat_pipe_cooled",
    "gas", "metal", "salt",
    "molten salt", "heat-pipe",
    "heat pipe", "solid-state"
  ) ~ "Non-water-cooled",
  
  TRUE ~ NA_character_
)

data$cooling_group <- factor(
  data$cooling_group,
  levels = c(
    "Non-water-cooled",
    "Water-cooled"
  )
)


# B. Neutron spectrum
# Reference: Fast

spectrum_raw <- normalise(data[[spectrum_col]])

data$spectrum_group <- dplyr::case_when(
  
  spectrum_raw %in% c(
    "thermal", "thermal spectrum",
    "thermal-spectrum"
  ) ~ "Thermal",
  
  spectrum_raw %in% c(
    "fast", "fast spectrum",
    "fast-spectrum"
  ) ~ "Fast",
  
  TRUE ~ NA_character_
)

data$spectrum_group <- factor(
  data$spectrum_group,
  levels = c("Fast", "Thermal")
)


# C. Fuel type
# Reference: TRISO/Other

fuel_raw <- normalise(data[[fuel_col]])

data$fuel_group <- dplyr::case_when(
  
  fuel_raw %in% c(
    "uranium", "uranium fuel",
    "uranium-based", "u",
    "uo2", "uo2 pellets",
    "uranium dioxide",
    "uranium nitride",
    "metallic u-zr alloy",
    "metallic uranium"
  ) ~ "Uranium",
  
  fuel_raw %in% c(
    "triso", "triso pebble",
    "triso-x pebble",
    "other", "other fuels",
    "molten salt fuel", "mox"
  ) ~ "TRISO/Other",
  
  TRUE ~ NA_character_
)

data$fuel_group <- factor(
  data$fuel_group,
  levels = c(
    "TRISO/Other",
    "Uranium"
  )
)


# D. Fuel enrichment
# Reference: LEU

enrichment_raw <- normalise(
  data[[enrichment_col]]
)

data$enrichment_group <- dplyr::case_when(
  
  enrichment_raw %in% c(
    "leu",
    "low enriched uranium",
    "low-enriched uranium"
  ) ~ "LEU",
  
  enrichment_raw %in% c(
    "haleu", "ha-leu", "ha leu",
    "high-assay low-enriched uranium"
  ) ~ "HALEU",
  
  enrichment_raw %in% c(
    "not_required",
    "not required",
    "not-required"
  ) ~ NA_character_,
  
  TRUE ~ NA_character_
)

data$enrichment_group <- factor(
  data$enrichment_group,
  levels = c("LEU", "HALEU")
)


# 9. CODING AUDIT ------------------------------------------------

coding_audit <- data.frame(
  design = data$design_id,
  reactor_class = data$reactor_class,
  
  cooling_raw = as.character(data[[cooling_col]]),
  cooling_group = data$cooling_group,
  
  spectrum_raw = as.character(data[[spectrum_col]]),
  spectrum_group = data$spectrum_group,
  
  fuel_raw = as.character(data[[fuel_col]]),
  fuel_group = data$fuel_group,
  
  enrichment_raw = as.character(data[[enrichment_col]]),
  enrichment_group = data$enrichment_group
)

if (anyNA(data[
  c(
    "cooling_group",
    "spectrum_group",
    "fuel_group"
  )
])) {
  
  print(
    coding_audit[
      !complete.cases(
        data[
          c(
            "cooling_group",
            "spectrum_group",
            "fuel_group"
          )
        ]
      ),
      ,
      drop = FALSE
    ]
  )
  
  stop("Unmapped technological predictor values detected.")
}

unexpected_enrichment <- is.na(
  data$enrichment_group
) & !(
  enrichment_raw %in% c(
    "not_required",
    "not required",
    "not-required"
  )
)

if (any(unexpected_enrichment)) {
  stop("Unexpected fuel-enrichment values detected.")
}


# 10. READINESS OUTCOMES -----------------------------------------

outcome_order <- c(
  "Financing",
  "Supply chain",
  "Fuel supply",
  "Licensing",
  "Siting"
)

for (outcome in outcome_order) {
  
  column <- outcome_columns[[outcome]]
  
  data[[column]] <- suppressWarnings(
    as.numeric(as.character(data[[column]]))
  )
  
  invalid <- !is.na(data[[column]]) &
    !(data[[column]] %in% 1:6)
  
  if (any(invalid)) {
    stop(
      "Invalid readiness scores for ",
      outcome
    )
  }
}


# 11. PREDICTOR INFORMATION --------------------------------------

predictor_info <- list(
  
  cooling = list(
    column = "cooling_group",
    title = "Reactor cooling type",
    comparison = "Water-cooled vs non-water-cooled",
    reference = "Non-water-cooled"
  ),
  
  spectrum = list(
    column = "spectrum_group",
    title = "Neutron spectrum",
    comparison = "Thermal vs fast",
    reference = "Fast"
  ),
  
  fuel = list(
    column = "fuel_group",
    title = "Fuel type",
    comparison = "Uranium vs TRISO/Other",
    reference = "TRISO/Other"
  ),
  
  enrichment = list(
    column = "enrichment_group",
    title = "Fuel enrichment",
    comparison = "HALEU vs LEU",
    reference = "LEU"
  )
)


# 12. ORDINAL REGRESSION -----------------------------------------

fit_model <- function(
    dataset,
    predictor_name,
    outcome_name
) {
  
  info <- predictor_info[[predictor_name]]
  
  model_data <- data.frame(
    response = make_outcome(
      dataset[[outcome_columns[[outcome_name]]]]
    ),
    predictor = dataset[[info$column]]
  )
  
  model_data <- model_data[
    stats::complete.cases(model_data),
    ,
    drop = FALSE
  ]
  
  n_used <- nrow(model_data)
  
  model_data$predictor <- droplevels(
    model_data$predictor
  )
  
  if (
    n_used < 10 ||
    nlevels(model_data$predictor) != 2 ||
    length(unique(model_data$response)) < 2
  ) {
    stop(
      "Cannot estimate model: ",
      predictor_name, " / ", outcome_name
    )
  }
  
  model <- MASS::polr(
    response ~ predictor,
    data = model_data,
    method = "logistic",
    Hess = TRUE
  )
  
  coefficients <- coef(summary(model))
  
  predictor_row <- grep(
    "^predictor",
    rownames(coefficients)
  )
  
  if (length(predictor_row) != 1) {
    stop("Unexpected model coefficient structure.")
  }
  
  beta <- unname(
    coefficients[predictor_row, "Value"]
  )
  
  se <- unname(
    coefficients[predictor_row, "Std. Error"]
  )
  
  if (
    !is.finite(beta) ||
    !is.finite(se) ||
    se <= 0
  ) {
    stop("Invalid regression coefficient or standard error.")
  }
  
  p <- 2 * stats::pnorm(
    abs(beta / se),
    lower.tail = FALSE
  )
  
  data.frame(
    predictor = predictor_name,
    predictor_label = info$title,
    comparison = info$comparison,
    reference = info$reference,
    outcome = outcome_name,
    n = n_used,
    beta = beta,
    standard_error = se,
    odds_ratio = exp(beta),
    ci_low = exp(beta - 1.96 * se),
    ci_high = exp(beta + 1.96 * se),
    p_value = p,
    stringsAsFactors = FALSE
  )
}


# 13. RUN 20 REGRESSIONS -----------------------------------------

results_list <- list()
counter <- 1

for (predictor in names(predictor_info)) {
  
  for (outcome in outcome_order) {
    
    results_list[[counter]] <- fit_model(
      data,
      predictor,
      outcome
    )
    
    counter <- counter + 1
  }
}

results <- dplyr::bind_rows(results_list)

if (nrow(results) != 20) {
  stop("Expected 20 ordinal regression models.")
}

results$sample <- "SMRs excluding microreactors"

results$p_holm <- stats::p.adjust(
  results$p_value,
  method = "holm"
)


# 14. CATEGORY COUNTS --------------------------------------------

category_counts <- dplyr::bind_rows(
  lapply(
    names(predictor_info),
    function(predictor) {
      
      info <- predictor_info[[predictor]]
      
      values <- as.character(
        data[[info$column]]
      )
      
      counts <- as.data.frame(
        table(values, useNA = "ifany"),
        stringsAsFactors = FALSE
      )
      
      names(counts) <- c("category", "n")
      
      counts$predictor <- info$title
      
      counts[
        ,
        c(
          "predictor",
          "category",
          "n"
        )
      ]
    }
  )
)


# 15. ORIGINAL INDICATOR COLOURS ---------------------------------

indicator_colours <- c(
  "Financing" = "#2A9D8F",
  "Supply chain" = "#4C78A8",
  "Fuel supply" = "#8E63CE",
  "Licensing" = "#8C2D3E",
  "Siting" = "#C7922F"
)


# 16. FOREST PLOT PANELS -----------------------------------------

make_panel <- function(
    results,
    predictor_name,
    panel_letter
) {
  
  info <- predictor_info[[predictor_name]]
  
  panel_data <- results[
    results$predictor == predictor_name,
    ,
    drop = FALSE
  ]
  
  panel_data$outcome <- factor(
    panel_data$outcome,
    levels = rev(outcome_order)
  )
  
  x_values <- c(
    panel_data$ci_low,
    panel_data$ci_high,
    1
  )
  
  x_limits <- exp(
    range(log(x_values)) + c(-0.15, 0.15)
  )
  
  candidate_breaks <- c(
    0.001, 0.002, 0.005,
    0.01, 0.02, 0.05,
    0.1, 0.2, 0.5,
    1, 2, 5, 10,
    20, 50, 100, 200
  )
  
  breaks <- candidate_breaks[
    candidate_breaks >= x_limits[1] &
      candidate_breaks <= x_limits[2]
  ]
  
  comparison_label <- paste0(
    info$comparison,
    " (ref: ",
    info$reference,
    ")"
  )
  
  panel <- ggplot2::ggplot(
    panel_data,
    ggplot2::aes(
      x = odds_ratio,
      y = outcome,
      colour = outcome
    )
  ) +
    
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.5
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_low,
        xend = ci_high,
        y = outcome,
        yend = outcome
      ),
      linewidth = 0.8
    ) +
    
    ggplot2::geom_point(
      size = 2.9
    ) +
    
    ggplot2::scale_colour_manual(
      values = indicator_colours,
      guide = "none"
    ) +
    
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = breaks,
      labels = function(x) {
        format(
          x,
          trim = TRUE,
          scientific = FALSE
        )
      },
      minor_breaks = NULL
    ) +
    
    ggplot2::scale_y_discrete(
      drop = FALSE
    ) +
    
    ggplot2::labs(
      title = info$title,
      subtitle = comparison_label,
      x = "Odds ratio (log scale)",
      y = NULL
    ) +
    
    ggplot2::theme_classic(
      base_size = 10
    ) +
    
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      
      axis.line = ggplot2::element_line(
        colour = "black",
        linewidth = 0.45
      ),
      
      axis.ticks = ggplot2::element_blank(),
      
      axis.text.x = ggplot2::element_text(
        colour = "black",
        size = 9,
        margin = ggplot2::margin(t = 7)
      ),
      
      axis.text.y = ggplot2::element_text(
        colour = "black",
        size = 9,
        margin = ggplot2::margin(r = 7)
      ),
      
      axis.title.x = ggplot2::element_text(
        colour = "black",
        size = 9.5,
        margin = ggplot2::margin(t = 8)
      ),
      
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 11.5,
        hjust = 0.5,
        colour = "black",
        margin = ggplot2::margin(b = 4)
      ),
      
      plot.subtitle = ggplot2::element_text(
        size = 8.5,
        hjust = 0.5,
        colour = "black",
        margin = ggplot2::margin(b = 10)
      ),
      
      plot.margin = ggplot2::margin(
        t = 8,
        r = 14,
        b = 10,
        l = 12
      )
    )
  
  cowplot::ggdraw(panel) +
    cowplot::draw_plot_label(
      panel_letter,
      x = 0.015,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 13,
      fontface = "bold"
    )
}


# 17. CREATE FOUR PANELS -----------------------------------------

panel_a <- make_panel(
  results, "cooling", "a"
)

panel_b <- make_panel(
  results, "spectrum", "b"
)

panel_c <- make_panel(
  results, "fuel", "c"
)

panel_d <- make_panel(
  results, "enrichment", "d"
)


# 18. COMBINE FOUR-PANEL FIGURE ----------------------------------

top_row <- cowplot::plot_grid(
  panel_a,
  panel_b,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

bottom_row <- cowplot::plot_grid(
  panel_c,
  panel_d,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

body <- cowplot::plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1)
)

header <- cowplot::ggdraw() +
  cowplot::draw_label(
    paste0(
      "SMRs excluding microreactors (n = ",
      nrow(data),
      ")"
    ),
    x = 0.5,
    y = 0.5,
    hjust = 0.5,
    fontface = "bold",
    size = 13,
    colour = "black"
  )

footer <- cowplot::ggdraw() +
  cowplot::draw_label(
    paste0(
      "Points indicate odds ratios; ",
      "horizontal lines indicate ",
      "95% confidence intervals. ",
      "Dashed line: OR = 1."
    ),
    x = 0.5,
    y = 0.5,
    hjust = 0.5,
    size = 9,
    colour = "black"
  )

supplementary_figure <- cowplot::plot_grid(
  header,
  body,
  footer,
  ncol = 1,
  rel_heights = c(
    0.065,
    1,
    0.065
  )
)

print(supplementary_figure)


# 19. EXPORT SUPPLEMENTARY FIGURE — PNG --------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = supplementary_figure,
  width = 270,
  height = 205,
  units = "mm",
  dpi = 600,
  bg = "white"
)


# 20. EXPORT SUPPLEMENTARY FIGURE — PDF --------------------------

ggplot2::ggsave(
  filename = pdf_file,
  plot = supplementary_figure,
  width = 270,
  height = 205,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# 21. EXPORT SUPPLEMENTARY RESULTS WORKBOOK ---------------------

sample_summary <- data.frame(
  metric = c(
    "Dashboard edition",
    "Analytical sample",
    "Number of designs",
    "Excluded reactor category",
    "Technological predictors",
    "Readiness indicators",
    "Ordinal regression models",
    "Multiple-testing adjustment"
  ),
  value = c(
    "4",
    "Conventional SMRs",
    as.character(nrow(data)),
    "Microreactors",
    "4",
    "5",
    as.character(nrow(results)),
    "Holm across 20 tests"
  )
)

wb <- openxlsx::createWorkbook()

tables <- list(
  "Regression results" = results,
  "Sample summary" = sample_summary,
  "Category counts" = category_counts,
  "Coding audit" = coding_audit
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


# 22. FINAL DIAGNOSTICS ------------------------------------------

cat("\n")
cat("============================================\n")
cat("SUPPLEMENTARY FIGURE S2 COMPLETE\n")
cat("============================================\n\n")

cat(
  "Conventional SMRs: ",
  nrow(data),
  "\n",
  sep = ""
)

cat(
  "Ordinal regression models: ",
  nrow(results),
  "\n",
  sep = ""
)

cat("\nOutput directory:\n")
cat(out_dir, "\n")

cat("\nFiles created:\n")
cat(basename(png_file), "\n")
cat(basename(pdf_file), "\n")
cat(basename(excel_file), "\n")

stopifnot(
  all(file.exists(c(
    png_file,
    pdf_file,
    excel_file
  )))
)

cat("\nSupplementary Figure S2 exported successfully.\n")
