# ================================================================
# SUPPLEMENTARY TABLE S17
# THERMAL CAPACITY (MWth) AND MARKET READINESS
# NEA SMR DASHBOARD, EDITION 4
#
# GitHub reproducible version
#
# Script:
# revision/scripts/supp_table_S17_thermal_capacity_readiness.R
#
# Input:
# revision/data/thermal_capacity_check_s17.xlsx
#
# Output:
# revision/figures/supp_table_S17_thermal_capacity_readiness_results.xlsx
#
# Analyses:
#   Spearman rank correlations
#   Pearson correlations
#   Holm correction across six readiness dimensions
#   Full sample and conventional-SMR sensitivity analysis
#
# Aggregate readiness is recalculated directly from the six
# updated readiness indicators and analysed separately.
#
# Expected samples:
#   All designs: n = 91
#   Conventional SMRs: n = 53
#
# Based on the updated JAG analysis.
# Supersedes the earlier November 5 analysis.
# Repository paths and existing GitHub filenames retained.
# This adaptation has not been independently executed.
# ================================================================


# 1. PACKAGES -----------------------------------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})


# 2. REPOSITORY PATHS ---------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "thermal_capacity_check_s17.xlsx"
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
        "Repository root not found. Expected: ",
        target_file
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

output_file <- file.path(
  output_dir,
  "supp_table_S17_thermal_capacity_readiness_results.xlsx"
)

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found:\n",
    input_file
  )
}

cat("\nRepository root:\n", ROOT_DIR, "\n")
cat("\nInput workbook:\n", input_file, "\n")
cat("\nOutput workbook:\n", output_file, "\n")


# 3. IMPORT DATA --------------------------------------------------

sheets <- readxl::excel_sheets(input_file)

sheet_name <- if ("nea_50" %in% sheets) {
  "nea_50"
} else {
  sheets[1]
}

dat <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = sheet_name
  )
)

names(dat) <- trimws(names(dat))

cat("\nImported worksheet:", sheet_name, "\n")
cat("Imported designs:", nrow(dat), "\n")


# 4. REQUIRED COLUMNS ---------------------------------------------

required_columns <- c(
  "design",
  "MWth",
  "microreactor",
  "licensing",
  "siting",
  "financing",
  "supply_chain",
  "stakeholder_engagement",
  "fuel_supply"
)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# 5. CLEAN DATA ---------------------------------------------------

normalise <- function(x) {
  tolower(trimws(gsub("\\s+", " ", as.character(x))))
}

if ("edition" %in% names(dat)) {

  edition_number <- suppressWarnings(
    as.numeric(as.character(dat$edition))
  )

  dat <- dat[
    !is.na(edition_number) &
      edition_number == 4,
    ,
    drop = FALSE
  ]
}

dat$design <- trimws(as.character(dat$design))

dat <- dat[
  !is.na(dat$design) &
    nzchar(dat$design) &
    toupper(dat$design) != "DF-CDE",
  ,
  drop = FALSE
]

if (anyDuplicated(dat$design)) {
  stop(
    "Duplicate reactor design names detected."
  )
}


# 6. THERMAL CAPACITY ---------------------------------------------

dat$MWth <- suppressWarnings(
  as.numeric(as.character(dat$MWth))
)

if (any(!is.finite(dat$MWth))) {
  stop(
    "Missing or non-finite thermal capacity values."
  )
}

if (any(dat$MWth < 0)) {
  stop(
    "Negative thermal capacity values detected."
  )
}


# 7. MICROREACTOR CLASSIFICATION ----------------------------------

micro_values <- normalise(dat$microreactor)

micro_true <- c(
  "true", "t", "yes", "y", "1",
  "microreactor", "micro_reactor", "micro reactor"
)

micro_false <- c(
  "false", "f", "no", "n", "0",
  "smr", "conventional", "conventional smr",
  "small modular reactor"
)

if (any(
  is.na(micro_values) |
    !(micro_values %in% c(micro_true, micro_false))
)) {

  print(unique(micro_values))

  stop(
    "Unrecognised microreactor classifications."
  )
}

dat$microreactor <- micro_values %in% micro_true


# 8. READINESS SCORES ---------------------------------------------

six_dimensions <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting",
  "stakeholder_engagement"
)

for (v in six_dimensions) {

  dat[[v]] <- suppressWarnings(
    as.numeric(as.character(dat[[v]]))
  )

  if (any(!is.finite(dat[[v]]))) {
    stop(
      "Missing or non-finite readiness scores: ",
      v
    )
  }

  if (any(
    dat[[v]] < 1 |
      dat[[v]] > 6
  )) {
    stop(
      "Readiness scores outside the 1-6 scale: ",
      v
    )
  }
}


# 9. AGGREGATE READINESS ------------------------------------------

# Recalculate directly from the six updated indicators.

dat$mean_readiness_recalculated <- rowMeans(
  dat[, six_dimensions, drop = FALSE]
)

# Compare with the existing aggregate column, if supplied.

if ("mean_readiness" %in% names(dat)) {

  dat$mean_readiness_original <- suppressWarnings(
    as.numeric(as.character(dat$mean_readiness))
  )

  dat$mean_difference <-
    dat$mean_readiness_original -
    dat$mean_readiness_recalculated

  mismatches <- which(
    is.finite(dat$mean_difference) &
      abs(dat$mean_difference) > 0.00001
  )

  if (length(mismatches) > 0) {

    cat(
      "\nAggregate readiness values differ for ",
      length(mismatches),
      " designs.\n",
      sep = ""
    )

    print(
      dat[
        mismatches,
        c(
          "design",
          "mean_readiness_original",
          "mean_readiness_recalculated",
          "mean_difference"
        )
      ]
    )
  }

} else {

  dat$mean_readiness_original <- NA_real_
  dat$mean_difference <- NA_real_
}

# Use the newly calculated six-indicator mean.

dat$mean_readiness <- dat$mean_readiness_recalculated

readiness_variables <- c(
  "mean_readiness",
  six_dimensions
)


# 10. DEFINE SAMPLES ----------------------------------------------

all_designs <- dat

conventional_smrs <- dat[
  !dat$microreactor,
  ,
  drop = FALSE
]

sample_sizes <- data.frame(
  sample = c(
    "All designs",
    "Conventional SMRs"
  ),
  n = c(
    nrow(all_designs),
    nrow(conventional_smrs)
  ),
  microreactors_excluded = c(
    0,
    sum(dat$microreactor)
  )
)

cat("\nSAMPLE SIZES:\n")
print(sample_sizes)

if (nrow(all_designs) != 91) {
  warning(
    "Full sample differs from expected n = 91."
  )
}

if (nrow(conventional_smrs) != 53) {
  warning(
    "Conventional-SMR sample differs from expected n = 53."
  )
}


# 11. CORRELATION FUNCTION ----------------------------------------

run_correlations <- function(data, sample_name) {

  result_list <- list()
  counter <- 1L

  for (variable in readiness_variables) {

    for (method in c("spearman", "pearson")) {

      x <- data$MWth
      y <- data[[variable]]

      complete <- complete.cases(x, y)

      x <- x[complete]
      y <- y[complete]

      if (
        length(x) < 3 ||
        length(unique(x)) < 2 ||
        length(unique(y)) < 2
      ) {

        result_list[[counter]] <- data.frame(
          sample = sample_name,
          outcome = variable,
          method = method,
          n = length(x),
          correlation = NA_real_,
          p_value = NA_real_
        )

      } else {

        test <- suppressWarnings(
          stats::cor.test(
            x,
            y,
            method = method,
            exact = FALSE
          )
        )

        result_list[[counter]] <- data.frame(
          sample = sample_name,
          outcome = variable,
          method = method,
          n = length(x),
          correlation = unname(test$estimate),
          p_value = test$p.value
        )
      }

      counter <- counter + 1L
    }
  }

  dplyr::bind_rows(result_list)
}


# 12. RUN CORRELATIONS --------------------------------------------

results_all <- run_correlations(
  all_designs,
  "All designs"
)

results_conventional <- run_correlations(
  conventional_smrs,
  "Conventional SMRs"
)

results <- dplyr::bind_rows(
  results_all,
  results_conventional
)


# 13. HOLM MULTIPLE-TEST CORRECTION -------------------------------

# Six individual readiness dimensions per sample and method.
# Aggregate readiness is excluded from this correction family.

results <- results %>%
  dplyr::group_by(sample, method) %>%
  dplyr::mutate(
    p_holm = {

      adjusted <- rep(
        NA_real_,
        dplyr::n()
      )

      idx <- which(
        outcome != "mean_readiness" &
          !is.na(p_value)
      )

      if (length(idx) > 0) {

        adjusted[idx] <- stats::p.adjust(
          p_value[idx],
          method = "holm",
          n = 6
        )
      }

      adjusted
    }
  ) %>%
  dplyr::ungroup()


# 14. PRESENTATION FORMATTING -------------------------------------

format_p <- function(x) {

  ifelse(
    is.na(x),
    NA_character_,
    ifelse(
      x < 0.001,
      "<0.001",
      sprintf("%.3f", x)
    )
  )
}

results <- results %>%
  dplyr::mutate(
    correlation_rounded = round(correlation, 3),
    p_formatted = format_p(p_value),
    p_holm_formatted = format_p(p_holm)
  ) %>%
  dplyr::arrange(
    factor(
      sample,
      levels = c(
        "All designs",
        "Conventional SMRs"
      )
    ),
    factor(
      outcome,
      levels = readiness_variables
    ),
    factor(
      method,
      levels = c(
        "spearman",
        "pearson"
      )
    )
  )


# 15. TABLE S17 SUMMARY -------------------------------------------

table_s17 <- results %>%
  dplyr::select(
    sample,
    outcome,
    method,
    n,
    correlation,
    p_value,
    p_holm,
    correlation_rounded,
    p_formatted,
    p_holm_formatted
  )

aggregate_results <- results %>%
  dplyr::filter(
    outcome == "mean_readiness"
  )

dimension_results <- results %>%
  dplyr::filter(
    outcome != "mean_readiness"
  )


# 16. CAPACITY AUDIT ----------------------------------------------

capacity_audit <- dat %>%
  dplyr::select(
    design,
    MWth,
    microreactor,
    mean_readiness,
    mean_readiness_original,
    mean_difference,
    dplyr::all_of(six_dimensions)
  )

audit_summary <- data.frame(
  metric = c(
    "Total designs",
    "Microreactors",
    "Conventional SMRs",
    "Minimum MWth",
    "Maximum MWth",
    "Zero MWth entries",
    "0.01 MWth entries",
    "Missing MWth"
  ),
  value = c(
    nrow(dat),
    sum(dat$microreactor),
    sum(!dat$microreactor),
    min(dat$MWth),
    max(dat$MWth),
    sum(dat$MWth == 0),
    sum(dat$MWth == 0.01),
    sum(is.na(dat$MWth))
  )
)


# 17. EXPORT ONE EXCEL WORKBOOK -----------------------------------

excel_tables <- list(
  "S17 results" = table_s17,
  "Aggregate readiness" = aggregate_results,
  "Individual dimensions" = dimension_results,
  "Sample sizes" = sample_sizes,
  "Capacity summary" = audit_summary,
  "Design audit" = capacity_audit,
  "Source data" = dat
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1"
)

for (sheet_name in names(excel_tables)) {

  tab <- as.data.frame(
    excel_tables[[sheet_name]]
  )

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    tab,
    headerStyle = header_style,
    withFilter = nrow(tab) > 0
  )

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(tab)),
    widths = "auto"
  )

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstRow = TRUE
  )
}

openxlsx::saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


# 18. PRINT RESULTS -----------------------------------------------

cat("\n")
cat("====================================================\n")
cat("SUPPLEMENTARY TABLE S17\n")
cat("THERMAL CAPACITY AND MARKET READINESS\n")
cat("====================================================\n")

cat("\nSPEARMAN CORRELATIONS:\n")

print(
  as.data.frame(
    table_s17 %>%
      dplyr::filter(method == "spearman")
  ),
  row.names = FALSE
)

cat("\nPEARSON CORRELATIONS:\n")

print(
  as.data.frame(
    table_s17 %>%
      dplyr::filter(method == "pearson")
  ),
  row.names = FALSE
)

cat("\nSAMPLE SIZES:\n")

print(
  sample_sizes,
  row.names = FALSE
)

cat("\nOUTPUT FILE:\n")

cat(
  output_file,
  "\n"
)

cat("\nAnalysis complete.\n")


# 19. FINAL OUTPUT VERIFICATION -----------------------------------

stopifnot(
  nrow(table_s17) == 28,
  nrow(aggregate_results) == 4,
  nrow(dimension_results) == 24,
  all(is.na(aggregate_results$p_holm)),
  file.exists(output_file),
  file.info(output_file)$size > 0
)

cat("\n====================================================\n")
cat("SUPPLEMENTARY TABLE S17 COMPLETE\n")
cat("====================================================\n")

cat("\nAggregate readiness recalculated from six indicators.\n")
cat("Both analytical samples included.\n")
cat("Both correlation methods included.\n")
cat("Holm correction applied to individual dimensions.\n")
cat("All 28 correlation result rows exported.\n")

cat(
  "\nWorkbook saved to:\n",
  normalizePath(output_file),
  "\n"
)

cat("====================================================\n")
