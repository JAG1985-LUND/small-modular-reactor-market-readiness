
# ================================================================
# SUPPLEMENTARY TABLE S17
# Thermal capacity (MWth) and market readiness
# NEA SMR Dashboard, Edition 4
#
# Full sample and conventional-SMR sensitivity analysis
#
# Script:
# revision/scripts/supp_table_S17_thermal_capacity_readiness.R
#
# Input:
# thermal_capacity_check_s17.xlsx
#
# Output:
# revision/figures/supp_table_S17_thermal_capacity_readiness_results.xlsx
# ================================================================


# 1. PACKAGES ----------------------------------------------------

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


# 2. REPOSITORY PATHS --------------------------------------------

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

input_file <- file.path(ROOT_DIR, SOURCE_FILE)

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


# 3. IMPORT AND VALIDATE DATA ------------------------------------

dat <- readxl::read_excel(input_file)

names(dat) <- trimws(names(dat))

six_dimensions <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting",
  "stakeholder_engagement"
)

readiness_variables <- c(
  "mean_readiness",
  six_dimensions
)

required_columns <- c(
  "design",
  "MWth",
  "microreactor",
  readiness_variables
)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

dat <- dat %>%
  mutate(
    design = trimws(as.character(design)),
    MWth = as.numeric(MWth),
    across(
      all_of(readiness_variables),
      as.numeric
    ),
    microreactor = case_when(
      toupper(as.character(microreactor)) ==
        "TRUE" ~ TRUE,
      toupper(as.character(microreactor)) ==
        "FALSE" ~ FALSE,
      TRUE ~ NA
    )
  )

if (
  anyNA(dat$design) ||
  any(dat$design == "") ||
  anyDuplicated(dat$design)
) {
  stop("Missing or duplicate reactor design names.")
}

if (anyNA(dat$microreactor)) {
  stop("Unrecognised microreactor classifications.")
}

if (
  any(!is.finite(dat$MWth)) ||
  any(dat$MWth < 0)
) {
  stop("Invalid thermal capacity values.")
}

readiness_matrix <- as.matrix(
  dat[readiness_variables]
)

if (
  any(!is.finite(readiness_matrix)) ||
  any(readiness_matrix < 1 | readiness_matrix > 6)
) {
  stop("Invalid readiness values.")
}


# 4. VERIFY SIX-INDICATOR COMPOSITE ------------------------------

dat <- dat %>%
  mutate(
    mean_readiness_recalculated = rowMeans(
      across(all_of(six_dimensions))
    ),
    mean_difference =
      mean_readiness -
      mean_readiness_recalculated
  )

if (any(abs(dat$mean_difference) > 0.00001)) {
  stop(
    "Supplied mean_readiness does not match ",
    "the six-indicator arithmetic mean."
  )
}


# 5. DEFINE ANALYTICAL SAMPLES ----------------------------------

all_designs <- dat

conventional_smrs <- dat %>%
  filter(microreactor == FALSE)

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

print(sample_sizes)

if (nrow(all_designs) != 91) {
  warning("Expected 91 designs in the full sample.")
}

if (nrow(conventional_smrs) != 53) {
  warning("Expected 53 conventional SMRs.")
}


# 6. CORRELATION FUNCTION ----------------------------------------

run_correlations <- function(data, sample_name) {
  
  results <- list()
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
        
        results[[counter]] <- data.frame(
          sample = sample_name,
          outcome = variable,
          method = method,
          n = length(x),
          correlation = NA_real_,
          p_value = NA_real_
        )
        
      } else {
        
        test <- suppressWarnings(
          cor.test(
            x,
            y,
            method = method,
            exact = FALSE
          )
        )
        
        results[[counter]] <- data.frame(
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
  
  dplyr::bind_rows(results)
}


# 7. RUN BOTH SAMPLES --------------------------------------------

results_all <- run_correlations(
  all_designs,
  "All designs"
)

results_conventional <- run_correlations(
  conventional_smrs,
  "Conventional SMRs"
)

results <- bind_rows(
  results_all,
  results_conventional
)


# 8. HOLM CORRECTION ---------------------------------------------

# Six readiness dimensions form the correction family.
# Aggregate readiness is reported separately.

results <- results %>%
  group_by(sample, method) %>%
  mutate(
    p_holm = {
      adjusted <- rep(NA_real_, n())
      
      idx <- which(
        outcome != "mean_readiness" &
          !is.na(p_value)
      )
      
      adjusted[idx] <- p.adjust(
        p_value[idx],
        method = "holm",
        n = 6
      )
      
      adjusted
    }
  ) %>%
  ungroup()


# 9. FORMAT RESULTS ----------------------------------------------

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
  mutate(
    correlation_rounded = round(correlation, 3),
    p_formatted = format_p(p_value),
    p_holm_formatted = format_p(p_holm)
  ) %>%
  arrange(
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
      levels = c("spearman", "pearson")
    )
  )


# 10. PREVIOUSLY REPORTED RESULTS CHECK -------------------------

previous_results <- data.frame(
  outcome = c(
    "mean_readiness",
    "mean_readiness",
    "supply_chain"
  ),
  method = c(
    "spearman",
    "pearson",
    "spearman"
  ),
  previous_correlation = c(
    0.137,
    0.180,
    0.213
  ),
  previous_p = c(
    0.194,
    0.088,
    0.043
  )
)

comparison <- results %>%
  filter(sample == "All designs") %>%
  inner_join(
    previous_results,
    by = c("outcome", "method")
  ) %>%
  mutate(
    correlation_difference =
      correlation - previous_correlation,
    p_difference =
      p_value - previous_p,
    matches_previous_rounded =
      round(correlation, 3) ==
      previous_correlation &
      round(p_value, 3) ==
      previous_p
  )

print(comparison)


# 11. THERMAL CAPACITY AUDIT -------------------------------------

capacity_audit <- dat %>%
  filter(
    design %in% c(
      "Alvin",
      "JAY-1",
      "Ward250",
      "KRONOS MMR",
      "TEPLATOR"
    )
  ) %>%
  select(
    design,
    MWth,
    microreactor,
    mean_readiness
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


# 12. ZERO-SUBSTITUTION SENSITIVITY ------------------------------

# Restore zero MWth for three named designs.
# This reproduces the original sensitivity analysis.

zero_check_data <- dat

zero_designs <- c(
  "Alvin",
  "JAY-1",
  "Ward250"
)

zero_check_data$MWth[
  zero_check_data$design %in% zero_designs
] <- 0

zero_check <- run_correlations(
  zero_check_data,
  "All designs, zero restored"
)

zero_comparison <- results_all %>%
  select(
    outcome,
    method,
    original_correlation = correlation,
    original_p = p_value
  ) %>%
  left_join(
    zero_check %>%
      select(
        outcome,
        method,
        zero_restored_correlation = correlation,
        zero_restored_p = p_value
      ),
    by = c("outcome", "method")
  ) %>%
  mutate(
    difference =
      original_correlation -
      zero_restored_correlation
  )


# 13. EXPORT ONE EXCEL WORKBOOK ----------------------------------

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "S17 results")
openxlsx::addWorksheet(wb, "Sample sizes")
openxlsx::addWorksheet(wb, "Previous results check")
openxlsx::addWorksheet(wb, "Capacity audit")
openxlsx::addWorksheet(wb, "Zero sensitivity")
openxlsx::addWorksheet(wb, "Source data")

openxlsx::writeData(
  wb,
  "S17 results",
  results
)

openxlsx::writeData(
  wb,
  "Sample sizes",
  sample_sizes
)

openxlsx::writeData(
  wb,
  "Previous results check",
  comparison
)

openxlsx::writeData(
  wb,
  "Capacity audit",
  audit_summary,
  startRow = 1
)

openxlsx::writeData(
  wb,
  "Capacity audit",
  capacity_audit,
  startRow = 13
)

openxlsx::writeData(
  wb,
  "Zero sensitivity",
  zero_comparison
)

openxlsx::writeData(
  wb,
  "Source data",
  dat
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1",
  halign = "center",
  border = "Bottom"
)

for (sheet in names(wb)) {
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = 1:15,
    widths = "auto"
  )
  
  openxlsx::addStyle(
    wb,
    sheet,
    style = header_style,
    rows = 1,
    cols = 1:15,
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


# 14. CONSOLE SUMMARY --------------------------------------------

cat(
  "\n========================================\n",
  "SUPPLEMENTARY TABLE S17\n",
  "========================================\n"
)

print(
  results %>%
    filter(method == "spearman") %>%
    select(
      sample,
      outcome,
      n,
      correlation_rounded,
      p_formatted,
      p_holm_formatted
    ),
  n = Inf
)

cat("\nPrevious-results comparison:\n")
print(comparison)

stopifnot(file.exists(output_file))

cat(
  "\nWorkbook saved to:\n",
  normalizePath(output_file),
  "\n"
)
