# ============================================================
# Code for the manuscript Gordon, J.A.
#
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces Figure 4: Temporal, geopolitical and technological
# predictors of SMR market-readiness dimensions.
#
# Panels show odds ratios and 95% confidence intervals from
# univariate ordinal logistic regression models relating higher
# market-readiness scores to:
#
# a. Conceptual design start year
# b. OECD membership
# c. Cooling type
# d. Neutron spectrum
# e. Fuel type
# f. Fuel enrichment
#
# Dashed vertical lines indicate the null value:
# odds ratio = 1.
#
# Required input:
# data/fig04_predictors_source_data.xlsx
#
# Outputs:
# outputs/figure_04/fig04_a-f.png
# outputs/figure_04/fig04_a-f.pdf
# outputs/figure_04/fig04_predictors_model_results.csv
#
# The script can be run from:
# 1. the repository root;
# 2. the scripts/ directory; or
# 3. the active RStudio editor.
# ============================================================


# ============================================================
# 1. LOAD PACKAGES AND RESET SESSION
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(MASS)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()


# ============================================================
# 2. GENERAL HELPER FUNCTIONS
# ============================================================

clean_names_simple <- function(x) {

  x <- tolower(
    gsub(
      "[^A-Za-z0-9]+",
      "_",
      x
    )
  )

  x <- gsub(
    "^_|_$",
    "",
    x
  )

  x
}


make_ordered_outcome <- function(x) {

  x_chr <- trimws(
    as.character(x)
  )

  x_chr[x_chr == ""] <- NA

  x_num <- suppressWarnings(
    as.numeric(x_chr)
  )

  all_values_numeric <- (
    sum(!is.na(x_num)) > 0 &&
      all(
        is.na(x_chr) |
          !is.na(x_num)
      )
  )

  if (all_values_numeric) {

    outcome_levels <- sort(
      unique(
        x_num[
          !is.na(x_num)
        ]
      )
    )

    ordered(
      x_num,
      levels = outcome_levels
    )

  } else {

    outcome_levels <- sort(
      unique(
        x_chr[
          !is.na(x_chr)
        ]
      )
    )

    ordered(
      x_chr,
      levels = outcome_levels
    )
  }
}


format_axis_labels <- function(x) {

  output <- sprintf(
    "%.2f",
    x
  )

  output <- sub(
    "0+$",
    "",
    output
  )

  output <- sub(
    "\\.$",
    "",
    output
  )

  output
}


# ============================================================
# 3. LOCATE THE REPOSITORY
# ============================================================

get_script_path <- function() {

  source_file <- tryCatch(
    normalizePath(
      sys.frames()[[1]]$ofile,
      winslash = "/",
      mustWork = FALSE
    ),
    error = function(e) ""
  )

  if (
    length(source_file) == 1 &&
      nzchar(source_file)
  ) {

    return(
      source_file
    )
  }

  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
      rstudioapi::isAvailable()
  ) {

    rstudio_file <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )

    if (
      length(rstudio_file) == 1 &&
        nzchar(rstudio_file)
    ) {

      return(
        normalizePath(
          rstudio_file,
          winslash = "/",
          mustWork = FALSE
        )
      )
    }
  }

  ""
}


find_repository_root <- function(input_filename) {

  script_path <- get_script_path()

  candidate_dirs <- character(0)

  if (nzchar(script_path)) {

    script_dir <- dirname(
      script_path
    )

    candidate_dirs <- c(
      candidate_dirs,
      script_dir,
      dirname(script_dir)
    )
  }

  current_dir <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = FALSE
  )

  candidate_dirs <- c(
    candidate_dirs,
    current_dir,
    dirname(current_dir)
  )

  candidate_dirs <- unique(
    candidate_dirs
  )

  input_candidates <- file.path(
    candidate_dirs,
    "data",
    input_filename
  )

  input_matches <- which(
    file.exists(
      input_candidates
    )
  )

  if (length(input_matches) == 0) {

    stop(
      paste0(
        "The Figure 4 source-data workbook could not be found.\n\n",
        "Expected repository-relative path:\n",
        "data/",
        input_filename,
        "\n\n",
        "Place this script in the repository's scripts/ directory ",
        "or run it from the repository root."
      )
    )
  }

  normalizePath(
    candidate_dirs[
      input_matches[1]
    ],
    winslash = "/",
    mustWork = TRUE
  )
}


# ============================================================
# 4. DEFINE INPUT AND OUTPUT PATHS
# ============================================================

input_filename <- "fig04_predictors_source_data.xlsx"

project_dir <- find_repository_root(
  input_filename
)

data_file <- file.path(
  project_dir,
  "data",
  input_filename
)

output_dir <- file.path(
  project_dir,
  "outputs",
  "figure_04"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

png_out <- file.path(
  output_dir,
  "fig04_a-f.png"
)

pdf_out <- file.path(
  output_dir,
  "fig04_a-f.pdf"
)

results_out <- file.path(
  output_dir,
  "fig04_predictors_model_results.csv"
)

message(
  "Repository root: ",
  project_dir
)

message(
  "Input workbook: ",
  data_file
)

message(
  "Output directory: ",
  output_dir
)


# ============================================================
# 5. LOAD SOURCE DATA
# ============================================================

available_sheets <- readxl::excel_sheets(
  data_file
)

sheet_name <- if (
  "fig04" %in% available_sheets
) {

  "fig04"

} else if (
  "fig03" %in% available_sheets
) {

  "fig03"

} else {

  available_sheets[1]
}

fig04_data <- readxl::read_excel(
  path = data_file,
  sheet = sheet_name
)

names(fig04_data) <- clean_names_simple(
  names(fig04_data)
)

message(
  "Loaded ",
  nrow(fig04_data),
  " rows from worksheet '",
  sheet_name,
  "'."
)


# ============================================================
# 6. VALIDATE SOURCE DATA
# ============================================================

if (!"design" %in% names(fig04_data)) {

  stop(
    paste0(
      "The Figure 4 source-data workbook does not contain ",
      "the required 'design' column."
    )
  )
}

fig04_data <- fig04_data[
  !is.na(fig04_data$design) &
    trimws(
      as.character(
        fig04_data$design
      )
    ) != "",
  ,
  drop = FALSE
]

required_columns <- c(
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_type_recoded",
  "fuel_enrichment_recode",
  "financing",
  "supply_chain",
  "fuel",
  "licensing",
  "siting"
)

missing_columns <- setdiff(
  required_columns,
  names(fig04_data)
)

if (length(missing_columns) > 0) {

  stop(
    paste0(
      "The Figure 4 source-data workbook is missing ",
      "the following required columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

oecd_candidates <- c(
  "oecd",
  "oecd_membership"
)

oecd_matches <- intersect(
  oecd_candidates,
  names(fig04_data)
)

if (length(oecd_matches) == 0) {

  stop(
    paste0(
      "The Figure 4 source-data workbook does not contain ",
      "an OECD membership column.\n\n",
      "Expected one of:\n",
      paste(
        oecd_candidates,
        collapse = ", "
      )
    )
  )
}

oecd_col <- oecd_matches[1]

message(
  "Rows retained after removing missing designs: ",
  nrow(fig04_data)
)

message(
  "OECD variable used: ",
  oecd_col
)


# ============================================================
# 7. RECODE PREDICTORS
# ============================================================

fig04_data <- fig04_data %>%
  mutate(

    cd_start_num = suppressWarnings(
      as.numeric(
        as.character(
          cd_start
        )
      )
    ),

    oecd_group = case_when(

      tolower(
        trimws(
          as.character(
            .data[[oecd_col]]
          )
        )
      ) %in% c(
        "oecd",
        "yes",
        "y",
        "1",
        "true"
      ) ~ "OECD",

      tolower(
        trimws(
          as.character(
            .data[[oecd_col]]
          )
        )
      ) %in% c(
        "non-oecd",
        "non_oecd",
        "non oecd",
        "no",
        "n",
        "0",
        "false"
      ) ~ "Non-OECD",

      TRUE ~ NA_character_
    ),

    water_group = case_when(

      tolower(
        trimws(
          as.character(
            water_cooled
          )
        )
      ) %in% c(
        "water_cooled",
        "water-cooled",
        "water cooled",
        "water",
        "yes",
        "y",
        "1",
        "true"
      ) ~ "Water-cooled",

      tolower(
        trimws(
          as.character(
            water_cooled
          )
        )
      ) %in% c(
        "non_water_cooled",
        "non-water-cooled",
        "non water cooled",
        "non-water",
        "non water",
        "no",
        "n",
        "0",
        "false"
      ) ~ "Non-water-cooled",

      TRUE ~ NA_character_
    ),

    spectrum_group = case_when(

      tolower(
        trimws(
          as.character(
            spectrum
          )
        )
      ) == "thermal" ~ "Thermal",

      tolower(
        trimws(
          as.character(
            spectrum
          )
        )
      ) == "fast" ~ "Fast",

      TRUE ~ NA_character_
    ),

    fuel_group_binary = case_when(

      tolower(
        trimws(
          as.character(
            fuel_type_recoded
          )
        )
      ) %in% c(
        "uranium",
        "u",
        "uranium fuel",
        "uranium-based"
      ) ~ "Uranium",

      tolower(
        trimws(
          as.character(
            fuel_type_recoded
          )
        )
      ) %in% c(
        "triso",
        "other",
        "other fuels",
        "metallic",
        "molten salt",
        "mox"
      ) ~ "Other fuels",

      TRUE ~ NA_character_
    ),

    enrichment_group_binary = case_when(

      tolower(
        trimws(
          as.character(
            fuel_enrichment_recode
          )
        )
      ) %in% c(
        "leu",
        "low enriched uranium",
        "low-enriched uranium"
      ) ~ "LEU",

      tolower(
        trimws(
          as.character(
            fuel_enrichment_recode
          )
        )
      ) %in% c(
        "haleu",
        "ha-leu",
        "ha leu",
        "high-assay low-enriched uranium",
        "helu",
        "other",
        "other enrichments"
      ) ~ "Other enrichments",

      TRUE ~ NA_character_
    )
  )


# ============================================================
# 8. MODEL-RESULT EXTRACTION FUNCTION
# ============================================================

extract_uni_results <- function(
  model,
  null_model,
  predictor_name,
  outcome_name,
  outcome_label,
  n_used
) {

  coefficient_matrix <- coef(
    summary(model)
  )

  coefficient_matrix <- coefficient_matrix[
    !grepl(
      "\\|",
      rownames(
        coefficient_matrix
      )
    ),
    ,
    drop = FALSE
  ]

  if (nrow(coefficient_matrix) == 0) {

    return(
      NULL
    )
  }

  coefficient_matrix <- coefficient_matrix[
    1,
    ,
    drop = FALSE
  ]

  p_values <- 2 * pnorm(
    abs(
      coefficient_matrix[
        ,
        "t value"
      ]
    ),
    lower.tail = FALSE
  )

  full_log_likelihood <- as.numeric(
    logLik(
      model
    )
  )

  null_log_likelihood <- as.numeric(
    logLik(
      null_model
    )
  )

  likelihood_ratio_statistic <- 2 * (
    full_log_likelihood -
      null_log_likelihood
  )

  likelihood_ratio_df <- attr(
    logLik(model),
    "df"
  ) -
    attr(
      logLik(null_model),
      "df"
    )

  likelihood_ratio_p <- pchisq(
    likelihood_ratio_statistic,
    df = likelihood_ratio_df,
    lower.tail = FALSE
  )

  mcfadden_r2 <- 1 - (
    full_log_likelihood /
      null_log_likelihood
  )

  estimate_value <- unname(
    coefficient_matrix[
      ,
      "Value"
    ]
  )

  standard_error_value <- unname(
    coefficient_matrix[
      ,
      "Std. Error"
    ]
  )

  data.frame(
    predictor = predictor_name,
    outcome = outcome_name,
    outcome_label = outcome_label,
    term = rownames(
      coefficient_matrix
    ),
    estimate = estimate_value,
    std_error = standard_error_value,
    t_value = unname(
      coefficient_matrix[
        ,
        "t value"
      ]
    ),
    p_value = unname(
      p_values
    ),
    odds_ratio = exp(
      estimate_value
    ),
    ci_low = exp(
      estimate_value -
        1.96 *
        standard_error_value
    ),
    ci_high = exp(
      estimate_value +
        1.96 *
        standard_error_value
    ),
    lr_stat = likelihood_ratio_statistic,
    lr_df = likelihood_ratio_df,
    lr_p_value = likelihood_ratio_p,
    mcfadden_r2 = mcfadden_r2,
    n = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# ============================================================
# 9. UNIVARIATE ORDINAL MODEL FUNCTION
# ============================================================

run_univariate_panel_models <- function(
  df,
  predictor_name,
  x,
  is_factor = FALSE,
  factor_levels = NULL
) {

  outcome_variables <- c(
    "financing",
    "supply_chain",
    "fuel",
    "licensing",
    "siting"
  )

  outcome_labels <- c(
    financing = "Financing",
    supply_chain = "Supply Chain",
    fuel = "Fuel Supply",
    licensing = "Licensing",
    siting = "Siting"
  )

  results_list <- list()

  for (outcome_variable in outcome_variables) {

    model_data <- data.frame(
      outcome = make_ordered_outcome(
        df[
          [
            outcome_variable
          ]
        ]
      ),
      x = x,
      stringsAsFactors = FALSE
    )

    if (is_factor) {

      model_data$x <- factor(
        model_data$x,
        levels = factor_levels
      )

    } else {

      model_data$x <- suppressWarnings(
        as.numeric(
          model_data$x
        )
      )
    }

    model_data <- model_data[
      complete.cases(
        model_data
      ),
      ,
      drop = FALSE
    ]

    if (nrow(model_data) == 0) {

      warning(
        paste0(
          "No complete observations for ",
          predictor_name,
          " and ",
          outcome_variable,
          "."
        )
      )

      next
    }

    if (
      length(
        unique(
          model_data$outcome
        )
      ) < 2
    ) {

      warning(
        paste0(
          "Outcome ",
          outcome_variable,
          " has fewer than two observed categories for ",
          predictor_name,
          "."
        )
      )

      next
    }

    if (
      length(
        unique(
          model_data$x
        )
      ) < 2
    ) {

      warning(
        paste0(
          "Predictor ",
          predictor_name,
          " has fewer than two observed values for ",
          outcome_variable,
          "."
        )
      )

      next
    }

    fitted_model <- tryCatch(
      suppressWarnings(
        MASS::polr(
          outcome ~ x,
          data = model_data,
          method = "logistic",
          Hess = TRUE
        )
      ),
      error = function(e) {

        warning(
          paste0(
            "Model failed for ",
            predictor_name,
            " and ",
            outcome_variable,
            ": ",
            conditionMessage(e)
          )
        )

        NULL
      }
    )

    null_model <- tryCatch(
      suppressWarnings(
        MASS::polr(
          outcome ~ 1,
          data = model_data,
          method = "logistic",
          Hess = TRUE
        )
      ),
      error = function(e) {

        warning(
          paste0(
            "Null model failed for ",
            predictor_name,
            " and ",
            outcome_variable,
            ": ",
            conditionMessage(e)
          )
        )

        NULL
      }
    )

    if (
      is.null(fitted_model) ||
        is.null(null_model)
    ) {

      next
    }

    results_list[
      [
        outcome_variable
      ]
    ] <- extract_uni_results(
      model = fitted_model,
      null_model = null_model,
      predictor_name = predictor_name,
      outcome_name = outcome_variable,
      outcome_label = outcome_labels[
        [
          outcome_variable
        ]
      ],
      n_used = nrow(
        model_data
      )
    )
  }

  results_list <- Filter(
    Negate(
      is.null
    ),
    results_list
  )

  if (length(results_list) == 0) {

    stop(
      paste0(
        "No valid models were estimated for ",
        predictor_name,
        "."
      )
    )
  }

  do.call(
    rbind,
    results_list
  )
}


# ============================================================
# 10. PANEL-PLOTTING FUNCTION
# ============================================================

make_or_panel <- function(
  results_df,
  panel_label,
  panel_title,
  candidate_breaks,
  dimension_colours
) {

  plot_order <- c(
    "Financing",
    "Supply Chain",
    "Fuel Supply",
    "Licensing",
    "Siting"
  )

  results_df$outcome_label <- factor(
    results_df$outcome_label,
    levels = plot_order
  )

  results_df <- results_df[
    order(
      results_df$outcome_label
    ),
    ,
    drop = FALSE
  ]

  results_df$y_position <- seq_len(
    nrow(
      results_df
    )
  )

  cap_half_height <- 0.10

  axis_values <- c(
    results_df$ci_low,
    results_df$ci_high,
    results_df$odds_ratio,
    1
  )

  axis_values <- axis_values[
    is.finite(
      axis_values
    ) &
      axis_values > 0
  ]

  if (length(axis_values) == 0) {

    stop(
      paste0(
        "No valid positive odds-ratio values were available ",
        "for panel ",
        panel_label,
        "."
      )
    )
  }

  x_minimum <- min(
    axis_values,
    na.rm = TRUE
  )

  x_maximum <- max(
    axis_values,
    na.rm = TRUE
  )

  log_padding <- 0.10

  x_limits <- exp(
    c(
      log(
        x_minimum
      ) -
        log_padding,
      log(
        x_maximum
      ) +
        log_padding
    )
  )

  x_breaks <- candidate_breaks[
    candidate_breaks >= x_limits[1] &
      candidate_breaks <= x_limits[2]
  ]

  if (!1 %in% x_breaks) {

    x_breaks <- sort(
      unique(
        c(
          x_breaks,
          1
        )
      )
    )
  }

  panel_core <- ggplot(
    results_df,
    aes(
      x = odds_ratio,
      y = y_position,
      colour = outcome_label
    )
  ) +
    geom_segment(
      aes(
        x = ci_low,
        xend = ci_high,
        y = y_position,
        yend = y_position
      ),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(
        x = ci_low,
        xend = ci_low,
        y = y_position -
          cap_half_height,
        yend = y_position +
          cap_half_height
      ),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(
        x = ci_high,
        xend = ci_high,
        y = y_position -
          cap_half_height,
        yend = y_position +
          cap_half_height
      ),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_point(
      size = 2.2,
      shape = 16,
      stroke = 0,
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey35",
      linewidth = 0.5
    ) +
    scale_colour_manual(
      values = dimension_colours,
      guide = "none"
    ) +
    scale_x_log10(
      breaks = x_breaks,
      labels = format_axis_labels(
        x_breaks
      ),
      minor_breaks = NULL,
      limits = x_limits,
      name = "Odds ratio (log scale)"
    ) +
    scale_y_continuous(
      breaks = results_df$y_position,
      labels = as.character(
        results_df$outcome_label
      ),
      trans = "reverse",
      expand = expansion(
        mult = c(
          0.06,
          0.06
        )
      )
    ) +
    labs(
      title = panel_title,
      y = NULL
    ) +
    theme_minimal(
      base_size = 7
    ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),

      axis.line.x = element_line(
        colour = "grey35",
        linewidth = 0.3
      ),

      axis.line.y = element_blank(),

      axis.ticks.x = element_line(
        colour = "grey35",
        linewidth = 0.3
      ),

      axis.ticks.y = element_blank(),

      axis.text.x = element_text(
        size = 7,
        colour = "black"
      ),

      axis.text.y = element_text(
        size = 7,
        colour = "black"
      ),

      axis.title.x = element_text(
        size = 7,
        colour = "black"
      ),

      plot.title = element_text(
        size = 8,
        face = "bold",
        hjust = 0.5,
        colour = "black"
      ),

      legend.position = "none",

      plot.margin = margin(
        5,
        5,
        5,
        16
      )
    )

  ggdraw(
    panel_core
  ) +
    draw_plot_label(
      label = panel_label,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}


# ============================================================
# 11. DEFINE COLOURS
# ============================================================

dimension_colours <- c(
  "Financing" = "#2A9D8F",
  "Supply Chain" = "#4C78A8",
  "Fuel Supply" = "#8E63CE",
  "Licensing" = "#8C2D3E",
  "Siting" = "#C7922F"
)


# ============================================================
# 12. DEFINE PANEL CONFIGURATIONS
# ============================================================

panel_configs <- list(

  list(
    id = "a",
    title = "Conceptual design start year",
    predictor = "Conceptual design start year",
    x = fig04_data$cd_start_num,
    is_factor = FALSE,
    factor_levels = NULL,
    breaks = c(
      0.75,
      0.80,
      0.90,
      1.00,
      1.10
    )
  ),

  list(
    id = "b",
    title = "OECD membership",
    predictor = "OECD membership",
    x = fig04_data$oecd_group,
    is_factor = TRUE,
    factor_levels = c(
      "Non-OECD",
      "OECD"
    ),
    breaks = c(
      0.1,
      0.2,
      0.5,
      1,
      2,
      5,
      10
    )
  ),

  list(
    id = "c",
    title = "Cooling type",
    predictor = "Cooling type",
    x = fig04_data$water_group,
    is_factor = TRUE,
    factor_levels = c(
      "Non-water-cooled",
      "Water-cooled"
    ),
    breaks = c(
      1,
      2,
      5,
      10,
      20,
      50
    )
  ),

  list(
    id = "d",
    title = "Neutron spectrum",
    predictor = "Neutron spectrum",
    x = fig04_data$spectrum_group,
    is_factor = TRUE,
    factor_levels = c(
      "Fast",
      "Thermal"
    ),
    breaks = c(
      0.5,
      1,
      2,
      5,
      10,
      20
    )
  ),

  list(
    id = "e",
    title = "Fuel type",
    predictor = "Fuel type",
    x = fig04_data$fuel_group_binary,
    is_factor = TRUE,
    factor_levels = c(
      "Other fuels",
      "Uranium"
    ),
    breaks = c(
      0.5,
      1,
      2,
      5,
      10,
      20
    )
  ),

  list(
    id = "f",
    title = "Fuel enrichment",
    predictor = "Fuel enrichment",
    x = fig04_data$enrichment_group_binary,
    is_factor = TRUE,
    factor_levels = c(
      "Other enrichments",
      "LEU"
    ),
    breaks = c(
      0.5,
      1,
      2,
      5,
      10,
      20
    )
  )
)


# ============================================================
# 13. ESTIMATE MODELS AND CREATE PANELS
# ============================================================

panels <- list()
all_results <- list()

for (panel_config in panel_configs) {

  message(
    "Estimating panel ",
    panel_config$id,
    ": ",
    panel_config$title
  )

  panel_results <- run_univariate_panel_models(
    df = fig04_data,
    predictor_name = panel_config$predictor,
    x = panel_config$x,
    is_factor = panel_config$is_factor,
    factor_levels = panel_config$factor_levels
  )

  panel_plot <- make_or_panel(
    results_df = panel_results,
    panel_label = panel_config$id,
    panel_title = panel_config$title,
    candidate_breaks = panel_config$breaks,
    dimension_colours = dimension_colours
  )

  panels[
    [
      panel_config$id
    ]
  ] <- panel_plot

  all_results[
    [
      panel_config$id
    ]
  ] <- panel_results
}


# ============================================================
# 14. EXPORT MODEL RESULTS
# ============================================================

all_results_df <- bind_rows(
  all_results,
  .id = "panel"
)

utils::write.csv(
  all_results_df,
  results_out,
  row.names = FALSE,
  na = ""
)


# ============================================================
# 15. COMBINE PANELS
# ============================================================

top_row <- plot_grid(
  panels$a,
  panels$b,
  panels$c,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(
    1,
    1,
    1
  )
)

bottom_row <- plot_grid(
  panels$d,
  panels$e,
  panels$f,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(
    1,
    1,
    1
  )
)

combined_plot <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(
    1,
    1
  )
)

combined_plot <- ggdraw(
  combined_plot
) +
  theme(
    plot.margin = margin(
      6,
      6,
      6,
      6
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

print(
  combined_plot
)


# ============================================================
# 16. SAVE FIGURE OUTPUTS
# ============================================================

ggsave(
  filename = png_out,
  plot = combined_plot,
  width = 240,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_out,
  plot = combined_plot,
  width = 240,
  height = 150,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 17. VERIFY OUTPUTS
# ============================================================

if (!file.exists(png_out)) {

  stop(
    paste0(
      "The Figure 4 PNG output was not created:\n",
      png_out
    )
  )
}

if (!file.exists(pdf_out)) {

  stop(
    paste0(
      "The Figure 4 PDF output was not created:\n",
      pdf_out
    )
  )
}

if (!file.exists(results_out)) {

  stop(
    paste0(
      "The Figure 4 model-results CSV was not created:\n",
      results_out
    )
  )
}


# ============================================================
# 18. COMPLETION MESSAGE
# ============================================================

message("")
message("==============================================")
message("FIGURE 4 COMPLETE")
message("==============================================")
message("Repository root: ", project_dir)
message("Input workbook: ", data_file)
message("Worksheet: ", sheet_name)
message("Rows retained: ", nrow(fig04_data))
message("PNG: ", png_out)
message("PDF: ", pdf_out)
message("Model results: ", results_out)
message("==============================================")


# ============================================================
# 19. RETURN FINAL PLOT
# ============================================================

combined_plot
