
# ================================================================
# SUPPLEMENTARY FIGURES S3(a) AND S3(b)
# JOINT REGIONAL AND TECHNOLOGICAL ASSOCIATIONS
# NEA SMR DASHBOARD, EDITION 4
#
# GITHUB REPRODUCIBLE VERSION
#
# Input:
# revision/data/
#   edition_4_regional_technology_figs03_source_data.xlsx
#
# Outputs:
# revision/figures/
#   supp_fig_S03_joint_regional_technological_all_designs.png
#   supp_fig_S03_joint_regional_technological_all_designs.pdf
#   supp_fig_S04_joint_regional_technological_conventional_smrs.png
#   supp_fig_S04_joint_regional_technological_conventional_smrs.pdf
#   supp_fig_S03_S04_model_verification.xlsx
#
# S3(a): All designs (n = 89)
# S3(b): Conventional SMRs (n = 51)
#
# Two Rest-of-world designs excluded.
# China and Russia combined.
#
# Model:
# Cumulative-link ordinal logistic regression
# Region + cooling + spectrum + fuel type + enrichment
#
# Reference categories:
# North America, water, thermal, uranium, LEU
# ================================================================


# 1. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "ordinal",
  "stringr",
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
  library(dplyr)
  library(ggplot2)
})


# 2. REPOSITORY PATHS --------------------------------------------

# Optional override:
# Sys.setenv(SMR_REPO_ROOT = "/path/to/repository")
#
# Otherwise, search upwards from the working directory
# and the executing script's directory.

source_filename <- paste0(
  "edition_4_regional_technology_",
  "figs03_source_data.xlsx"
)

find_repo_root <- function() {
  
  override <- Sys.getenv(
    "SMR_REPO_ROOT",
    unset = ""
  )
  
  candidates <- getwd()
  
  if (nzchar(override)) {
    candidates <- c(override, candidates)
  }
  
  args <- commandArgs(
    trailingOnly = FALSE
  )
  
  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )
  
  if (length(file_arg) > 0) {
    candidates <- c(
      candidates,
      dirname(sub("^--file=", "", file_arg[1]))
    )
  }
  
  frames <- sys.frames()
  
  for (frame in frames) {
    if (!is.null(frame$ofile)) {
      candidates <- c(
        candidates,
        dirname(frame$ofile)
      )
    }
  }
  
  candidates <- unique(
    normalizePath(
      candidates,
      winslash = "/",
      mustWork = FALSE
    )
  )
  
  for (candidate in candidates) {
    
    current <- candidate
    
    repeat {
      
      target <- file.path(
        current,
        "revision",
        "data",
        source_filename
      )
      
      if (file.exists(target)) {
        return(current)
      }
      
      parent <- dirname(current)
      
      if (identical(parent, current)) {
        break
      }
      
      current <- parent
    }
  }
  
  stop(
    "Repository root not found. Expected ",
    "revision/data/",
    source_filename,
    ". Run from the repository, source the script, ",
    "or set SMR_REPO_ROOT."
  )
}

repo_root <- find_repo_root()

input_file <- file.path(
  repo_root,
  "revision",
  "data",
  source_filename
)

output_dir <- file.path(
  repo_root,
  "revision",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\nRepository root:\n", repo_root, "\n")
cat("\nInput workbook:\n", input_file, "\n")
cat("\nOutput directory:\n", output_dir, "\n")


# 3. IMPORT ------------------------------------------------------

sheets <- readxl::excel_sheets(input_file)

sheet_name <- if ("Source data" %in% sheets) {
  "Source data"
} else {
  sheets[1]
}

raw <- readxl::read_excel(
  input_file,
  sheet = sheet_name
)

names(raw) <- gsub(
  "_$",
  "",
  gsub(
    "[^a-z0-9]+",
    "_",
    tolower(names(raw))
  )
)

if (
  !"neutron_spectrum" %in% names(raw) &&
  "neturon_spectrum" %in% names(raw)
) {
  raw$neutron_spectrum <- raw$neturon_spectrum
}

required <- c(
  "design",
  "region_cn_ru",
  "microreactor",
  "neutron_spectrum",
  "fuel_type",
  "fuel_enrichment",
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting"
)

missing_columns <- setdiff(
  required,
  names(raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (
  !"water_cooled" %in% names(raw) &&
  !"cooling_type" %in% names(raw)
) {
  stop("Missing cooling classification.")
}

if ("edition" %in% names(raw)) {
  raw <- raw[
    as.character(raw$edition) == "4",
    ,
    drop = FALSE
  ]
}

raw <- raw[
  !is.na(raw$design) &
    trimws(as.character(raw$design)) != "" &
    toupper(trimws(as.character(raw$design))) != "DF-CDE",
  ,
  drop = FALSE
]

if (anyDuplicated(
  toupper(trimws(as.character(raw$design)))
)) {
  stop("Duplicate reactor design identifiers detected.")
}

if (nrow(raw) != 91) {
  stop(
    "Expected 91 Edition 4 designs; found ",
    nrow(raw)
  )
}

cat("\nImported designs:", nrow(raw), "\n")


# 4. CLEANING FUNCTIONS -----------------------------------------

clean_text <- function(x) {
  
  z <- stringr::str_squish(
    gsub(
      "\u00a0",
      " ",
      as.character(x),
      fixed = TRUE
    )
  )
  
  z[z %in% c("", "NA", "N/A")] <- NA_character_
  
  z
}

clean_binary <- function(x) {
  
  z <- toupper(clean_text(x))
  
  dplyr::case_when(
    z %in% c(
      "TRUE", "T", "1", "YES", "Y"
    ) ~ TRUE,
    
    z %in% c(
      "FALSE", "F", "0", "NO", "N"
    ) ~ FALSE,
    
    TRUE ~ NA
  )
}

normalise_region <- function(x) {
  
  z <- tolower(clean_text(x))
  
  z <- gsub(
    "[_/-]+",
    " ",
    z
  )
  
  stringr::str_squish(z)
}


# 5. REGIONAL HARMONISATION -------------------------------------

region_raw <- normalise_region(
  raw$region_cn_ru
)

region_harmonised <- dplyr::case_when(
  
  region_raw == "north america" ~
    "North America",
  
  region_raw == "europe" ~
    "Europe",
  
  region_raw == "oecd asia" ~
    "OECD Asia",
  
  region_raw %in% c(
    "china russia",
    "china and russia"
  ) ~ "China/Russia",
  
  region_raw %in% c(
    "row",
    "rest of world",
    "rest of the world"
  ) ~ "Rest of world",
  
  TRUE ~ NA_character_
)

if (anyNA(region_harmonised)) {
  
  print(
    table(
      raw$region_cn_ru[
        is.na(region_harmonised)
      ],
      useNA = "ifany"
    )
  )
  
  stop("Unrecognised regional classification.")
}

region_counts <- table(region_harmonised)

expected_regions <- c(
  "North America" = 40,
  "Europe" = 26,
  "OECD Asia" = 12,
  "China/Russia" = 11,
  "Rest of world" = 2
)

if (!isTRUE(all.equal(
  unname(as.integer(
    region_counts[names(expected_regions)]
  )),
  unname(as.integer(expected_regions))
))) {
  stop("Regional counts differ from verified workbook.")
}

cat("\nVerified regional counts:\n")
print(region_counts)


# 6. TECHNOLOGICAL HARMONISATION --------------------------------

if ("water_cooled" %in% names(raw)) {
  
  water <- clean_binary(raw$water_cooled)
  
} else {
  
  cooling_raw <- tolower(
    clean_text(raw$cooling_type)
  )
  
  water <- dplyr::case_when(
    cooling_raw == "water" ~ TRUE,
    !is.na(cooling_raw) ~ FALSE,
    TRUE ~ NA
  )
}

df <- raw %>%
  dplyr::mutate(
    
    design = clean_text(design),
    
    # Use harmonised region, not the original region column.
    region = .env$region_harmonised,
    
    microreactor = clean_binary(microreactor),
    
    spectrum = dplyr::case_when(
      tolower(clean_text(neutron_spectrum)) ==
        "thermal" ~ "Thermal",
      
      tolower(clean_text(neutron_spectrum)) ==
        "fast" ~ "Fast",
      
      TRUE ~ NA_character_
    ),
    
    fuel_type = dplyr::case_when(
      tolower(clean_text(fuel_type)) ==
        "uranium" ~ "Uranium",
      
      tolower(clean_text(fuel_type)) ==
        "triso" ~ "TRISO",
      
      tolower(clean_text(fuel_type)) ==
        "other" ~ "Other",
      
      TRUE ~ NA_character_
    ),
    
    enrichment = dplyr::case_when(
      toupper(clean_text(fuel_enrichment)) ==
        "LEU" ~ "LEU",
      
      toupper(clean_text(fuel_enrichment)) ==
        "HALEU" ~ "HALEU",
      
      tolower(clean_text(fuel_enrichment)) %in%
        c("not_required", "not required") ~
        "Not required",
      
      TRUE ~ NA_character_
    ),
    
    water_cooled = .env$water,
    
    cooling = dplyr::case_when(
      water_cooled == TRUE ~ "Water",
      water_cooled == FALSE ~ "Non-water",
      TRUE ~ NA_character_
    ),
    
    dplyr::across(
      dplyr::all_of(c(
        "financing",
        "supply_chain",
        "fuel_supply",
        "licensing",
        "siting"
      )),
      ~ suppressWarnings(
        as.numeric(as.character(.x))
      )
    )
  )

if (anyNA(df$microreactor)) {
  stop("Unrecognised microreactor classifications.")
}

if (sum(!df$microreactor) != 53) {
  stop("Expected 53 conventional SMRs before exclusions.")
}


# 7. REGRESSION SAMPLES -----------------------------------------

excluded <- df %>%
  dplyr::filter(
    region == "Rest of world"
  )

cat("\nExcluded Rest-of-world designs:\n")

print(
  excluded %>%
    dplyr::select(
      design,
      region,
      microreactor
    )
)

if (nrow(excluded) != 2) {
  stop("Expected exactly two Rest-of-world designs.")
}

df <- df %>%
  dplyr::filter(
    region != "Rest of world"
  ) %>%
  dplyr::mutate(
    
    region = factor(
      region,
      levels = c(
        "North America",
        "Europe",
        "OECD Asia",
        "China/Russia"
      )
    ),
    
    cooling = factor(
      cooling,
      levels = c(
        "Water",
        "Non-water"
      )
    ),
    
    spectrum = factor(
      spectrum,
      levels = c(
        "Thermal",
        "Fast"
      )
    ),
    
    fuel_type = factor(
      fuel_type,
      levels = c(
        "Uranium",
        "TRISO",
        "Other"
      )
    ),
    
    enrichment = factor(
      enrichment,
      levels = c(
        "LEU",
        "HALEU",
        "Not required"
      )
    )
  )

samples <- list(
  
  "All designs" = df,
  
  "Conventional SMRs" = df %>%
    dplyr::filter(
      microreactor == FALSE
    )
)

expected_n <- c(
  "All designs" = 89,
  "Conventional SMRs" = 51
)

actual_n <- vapply(
  samples,
  nrow,
  integer(1)
)

cat("\nRegression sample sizes:\n")
print(actual_n)

if (!isTRUE(all.equal(
  unname(actual_n),
  unname(expected_n)
))) {
  stop(
    "Unexpected sample sizes: ",
    paste(
      names(actual_n),
      actual_n,
      collapse = "; "
    )
  )
}


# 8. MODEL SPECIFICATION ----------------------------------------

outcomes <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting"
)

outcome_labels <- c(
  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  licensing = "Licensing",
  siting = "Siting"
)

predictors <- c(
  "region",
  "cooling",
  "spectrum",
  "fuel_type",
  "enrichment"
)

combined_formula <- stats::reformulate(
  predictors,
  response = "y"
)


# 9. COMBINED ORDINAL MODEL FUNCTION ----------------------------

fit_combined <- function(
    source,
    outcome,
    sample_name
) {
  
  needed <- c(
    outcome,
    predictors
  )
  
  complete <- stats::complete.cases(
    source[, needed, drop = FALSE]
  )
  
  dat <- droplevels(
    source[complete, , drop = FALSE]
  )
  
  if (any(
    !is.na(dat[[outcome]]) &
    !(dat[[outcome]] %in% 1:6)
  )) {
    stop(
      "Unexpected readiness score: ",
      outcome
    )
  }
  
  dat$y <- ordered(
    dat[[outcome]],
    levels = 1:6
  )
  
  xmat <- stats::model.matrix(
    stats::reformulate(predictors),
    data = dat
  )
  
  if (
    nrow(dat) < 20 ||
    qr(xmat)$rank < ncol(xmat)
  ) {
    stop(
      "Insufficient cases or rank-deficient model: ",
      sample_name, " / ", outcome
    )
  }
  
  model_warnings <- character(0)
  
  fit <- withCallingHandlers(
    
    ordinal::clm(
      combined_formula,
      data = dat,
      link = "logit",
      Hess = TRUE,
      control = ordinal::clm.control(
        maxIter = 200
      )
    ),
    
    warning = function(w) {
      
      model_warnings <<- c(
        model_warnings,
        conditionMessage(w)
      )
      
      invokeRestart("muffleWarning")
    }
  )
  
  cv <- fit$convergence
  
  code <- suppressWarnings(
    as.integer(
      if (is.list(cv)) {
        cv[[1L]]
      } else {
        cv[1L]
      }
    )
  )
  
  if (length(code) != 1L || is.na(code)) {
    code <- NA_integer_
  }
  
  hessian_condition <- if (
    length(fit$cond.H) == 1L
  ) {
    as.numeric(fit$cond.H)
  } else {
    NA_real_
  }
  
  status <- if (
    !is.na(code) &&
    code == 0L &&
    is.finite(hessian_condition) &&
    hessian_condition < 1e8 &&
    all(is.finite(fit$beta)) &&
    length(model_warnings) == 0
  ) {
    "OK"
  } else {
    "Review diagnostics"
  }
  
  diagnostic <- data.frame(
    sample = sample_name,
    outcome = outcome,
    n = nrow(dat),
    convergence_code = code,
    hessian_condition = hessian_condition,
    warnings = paste(
      unique(model_warnings),
      collapse = " | "
    ),
    status = status
  )
  
  mat <- coef(summary(fit))
  
  rows <- lapply(
    names(fit$beta),
    function(term) {
      
      beta <- unname(
        mat[term, "Estimate"]
      )
      
      se <- unname(
        mat[term, "Std. Error"]
      )
      
      data.frame(
        sample = sample_name,
        outcome = outcome,
        term = term,
        n = nrow(dat),
        beta = beta,
        se = se,
        odds_ratio = exp(beta),
        ci_low = exp(beta - 1.96 * se),
        ci_high = exp(beta + 1.96 * se),
        p_value = 2 * stats::pnorm(
          abs(beta / se),
          lower.tail = FALSE
        ),
        status = status
      )
    }
  )
  
  list(
    coefficients = dplyr::bind_rows(rows),
    diagnostic = diagnostic
  )
}


# 10. RUN COMBINED MODELS ---------------------------------------

results <- list()

for (s in names(samples)) {
  
  for (o in outcomes) {
    
    cat(
      "\nFitting M3 combined:",
      s, "/", o, "\n"
    )
    
    key <- paste(s, o, sep = "__")
    
    results[[key]] <- fit_combined(
      samples[[s]],
      o,
      s
    )
  }
}

coef_df <- dplyr::bind_rows(
  lapply(results, `[[`, "coefficients")
)

diagnostics_df <- dplyr::bind_rows(
  lapply(results, `[[`, "diagnostic")
)

cat("\nMODEL DIAGNOSTICS:\n")
print(diagnostics_df, row.names = FALSE)


# 11. PREPARE FOREST PLOT DATA ----------------------------------

term_labels <- c(
  "regionEurope" = "Europe",
  "regionOECD Asia" = "OECD Asia",
  "regionChina/Russia" = "China/Russia",
  "coolingNon-water" = "Non-water-cooled",
  "spectrumFast" = "Fast spectrum",
  "fuel_typeTRISO" = "TRISO fuel",
  "fuel_typeOther" = "Other fuel",
  "enrichmentHALEU" = "HALEU",
  "enrichmentNot required" = "Enrichment not required"
)

plot_order <- c(
  "China/Russia",
  "OECD Asia",
  "Europe",
  "Non-water-cooled",
  "Fast spectrum",
  "TRISO fuel",
  "Other fuel",
  "HALEU",
  "Enrichment not required"
)

plot_df <- coef_df %>%
  dplyr::filter(
    status == "OK",
    is.finite(odds_ratio),
    is.finite(ci_low),
    is.finite(ci_high),
    odds_ratio > 0,
    ci_low > 0,
    ci_high > 0
  ) %>%
  dplyr::mutate(
    
    label = unname(
      term_labels[term]
    ),
    
    label = ifelse(
      is.na(label),
      term,
      label
    ),
    
    label = factor(
      label,
      levels = rev(plot_order)
    ),
    
    outcome = factor(
      outcome,
      levels = outcomes,
      labels = unname(
        outcome_labels[outcomes]
      )
    )
  )

if (nrow(plot_df) == 0) {
  stop("No valid combined-model estimates.")
}

expected_outcomes <- unname(
  outcome_labels[outcomes]
)

for (s in names(samples)) {
  
  present <- unique(
    as.character(
      plot_df$outcome[
        plot_df$sample == s
      ]
    )
  )
  
  missing_outcomes <- setdiff(
    expected_outcomes,
    present
  )
  
  if (length(missing_outcomes) > 0) {
    stop(
      "Cannot produce complete figure for ",
      s, ". Missing outcomes: ",
      paste(missing_outcomes, collapse = ", "),
      ". Review model diagnostics."
    )
  }
}


# 12. FOREST PLOT FUNCTION --------------------------------------

make_forest <- function(sample_name, panel_label) {
  
  dat <- plot_df %>%
    dplyr::filter(
      sample == sample_name
    )
  
  n_sample <- unname(actual_n[sample_name])
  
  figure_heading <- paste0(
    "(",
    panel_label,
    ") ",
    sample_name,
    " (n = ",
    n_sample,
    ")"
  )
  
  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = odds_ratio,
      y = label
    )
  ) +
    
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.35
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_low,
        xend = ci_high,
        yend = label
      ),
      colour = "#4C78A8",
      linewidth = 0.55
    ) +
    
    ggplot2::geom_point(
      colour = "#4C78A8",
      size = 2.1
    ) +
    
    ggplot2::scale_x_log10() +
    
    ggplot2::facet_wrap(
      ~ outcome,
      ncol = 3
    ) +
    
    ggplot2::labs(
      title = figure_heading,
      x = "Adjusted odds ratio (log scale)",
      y = NULL
    ) +
    
    ggplot2::theme_classic(
      base_size = 10
    ) +
    
    ggplot2::theme(
      
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14,
        colour = "black",
        hjust = 0.5,
        margin = ggplot2::margin(b = 14)
      ),
      
      strip.background =
        ggplot2::element_rect(
          fill = "white",
          colour = "black",
          linewidth = 0.35
        ),
      
      strip.text =
        ggplot2::element_text(
          face = "bold",
          size = 10,
          colour = "black"
        ),
      
      axis.text.x =
        ggplot2::element_text(
          size = 8,
          colour = "black"
        ),
      
      axis.text.y =
        ggplot2::element_text(
          size = 9,
          colour = "black"
        ),
      
      axis.title.x =
        ggplot2::element_text(
          size = 10,
          colour = "black",
          margin = ggplot2::margin(t = 10)
        ),
      
      axis.line =
        ggplot2::element_line(
          colour = "grey35",
          linewidth = 0.3
        ),
      
      axis.ticks =
        ggplot2::element_line(
          colour = "grey35",
          linewidth = 0.3
        ),
      
      panel.grid =
        ggplot2::element_blank(),
      
      panel.spacing =
        grid::unit(10, "mm"),
      
      plot.background =
        ggplot2::element_rect(
          fill = "white",
          colour = NA
        ),
      
      plot.margin =
        ggplot2::margin(
          12, 12, 12, 12
        )
    )
}


# 13. CREATE SEPARATE FIGURES -----------------------------------

figure_s03 <- make_forest(
  sample_name = "All designs",
  panel_label = "a"
)

figure_s04 <- make_forest(
  sample_name = "Conventional SMRs",
  panel_label = "b"
)

print(figure_s03)
print(figure_s04)


# 14. EXPORT FUNCTION -------------------------------------------

export_figure <- function(plot, filename) {
  
  png_file <- file.path(
    output_dir,
    paste0(filename, ".png")
  )
  
  pdf_file <- file.path(
    output_dir,
    paste0(filename, ".pdf")
  )
  
  ggplot2::ggsave(
    filename = png_file,
    plot = plot,
    width = 280,
    height = 135,
    units = "mm",
    dpi = 600,
    bg = "white"
  )
  
  ggplot2::ggsave(
    filename = pdf_file,
    plot = plot,
    width = 280,
    height = 135,
    units = "mm",
    device = grDevices::cairo_pdf,
    bg = "white"
  )
  
  for (f in c(png_file, pdf_file)) {
    
    if (
      !file.exists(f) ||
      is.na(file.info(f)$size) ||
      file.info(f)$size == 0
    ) {
      stop("Missing or empty output: ", f)
    }
  }
  
  invisible(c(png_file, pdf_file))
}


# 15. EXPORT FIGURE S3(a) ---------------------------------------

files_s03 <- export_figure(
  figure_s03,
  "supp_fig_S03_joint_regional_technological_all_designs"
)


# 16. EXPORT FIGURE S3(b) ---------------------------------------

files_s04 <- export_figure(
  figure_s04,
  "supp_fig_S04_joint_regional_technological_conventional_smrs"
)


# 17. EXPORT MODEL VERIFICATION ---------------------------------

excel_file <- file.path(
  output_dir,
  "supp_fig_S03_S04_model_verification.xlsx"
)

wb <- openxlsx::createWorkbook()

excel_tables <- list(
  "Model diagnostics" = diagnostics_df,
  "Adjusted odds ratios" = coef_df,
  "Figure data" = plot_df,
  "Regional counts" = as.data.frame(region_counts),
  "Excluded designs" = excluded %>%
    dplyr::select(
      design,
      region,
      microreactor
    )
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1"
)

for (sheet in names(excel_tables)) {
  
  dat_sheet <- as.data.frame(
    excel_tables[[sheet]]
  )
  
  openxlsx::addWorksheet(wb, sheet)
  
  openxlsx::writeData(
    wb,
    sheet,
    dat_sheet,
    headerStyle = header_style
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(dat_sheet)),
    widths = "auto"
  )
}

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# 18. FINAL VERIFICATION ----------------------------------------

output_files <- c(
  files_s03,
  files_s04,
  excel_file
)

for (f in output_files) {
  
  if (
    !file.exists(f) ||
    is.na(file.info(f)$size) ||
    file.info(f)$size == 0
  ) {
    stop("Missing or empty output: ", f)
  }
}

cat("\n")
cat("====================================================\n")
cat("SUPPLEMENTARY FIGURE S3(a) AND S3(b) COMPLETE\n")
cat("====================================================\n")

cat("\nVerified sample sizes:\n")
print(actual_n)

cat("\nModel diagnostics:\n")
print(diagnostics_df, row.names = FALSE)

cat("\nExported files:\n")
cat(paste(output_files, collapse = "\n"))

cat("\n\nOutput directory:\n")
cat(output_dir, "\n")

cat("\nAnalysis complete.\n")
