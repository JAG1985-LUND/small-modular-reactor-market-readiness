# ============================================================
# SUPPLEMENTARY TABLE S10
# MULTIVARIABLE TECHNOLOGICAL ASSOCIATIONS
# NEA SMR DASHBOARD, EDITION 4
#
# GitHub-compatible version of the updated JAG script.
#
# Input:
# revision/data/fig08_nea_technological_source_data.xlsx
#
# Output:
# revision/figures/table_S10_multivariable_technology_results.xlsx
#
# Five proportional-odds ordinal logistic models.
# Four technological predictors fitted simultaneously.
# Complete-case analysis by readiness outcome.
#
# Enrichment categories not_required and unresolved
# are excluded from the regression models.
#
# Additional analyses:
# Drop-one likelihood-ratio tests
# Matched-sample univariate regressions
# Adjusted versus unadjusted comparisons
# Holm multiple-testing adjustment
# Model fit and coding audits
#
# One Excel workbook containing 13 worksheets.
#
# Updated JAG analytical specifications retained.
# Original GitHub input and output filenames retained.
# This GitHub adaptation has not been independently executed.
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

packages <- c(
  "readxl",
  "dplyr",
  "MASS",
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
  library(MASS)
  library(openxlsx)
})


# ============================================================
# 2. GITHUB-COMPATIBLE PATHS
# ============================================================

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
        "Expected: ", target_file, "\n",
        "Run from within the repository."
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
  "table_S10_multivariable_technology_results.xlsx"
)

if (!file.exists(input_file)) {
  stop("Input workbook not found: ", input_file)
}


# ============================================================
# 3. HELPER FUNCTIONS
# ============================================================

clean_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

normalise <- function(x) {
  x <- tolower(trimws(as.character(x)))

  x[x %in% c(
    "", "na", "n/a", "unknown", "not reported"
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
      paste(candidates, collapse = ", ")
    )
  }

  NA_character_
}

make_outcome <- function(x) {

  values <- suppressWarnings(
    as.numeric(as.character(x))
  )

  invalid <- !is.na(values) &
    !(values %in% 1:6)

  if (any(invalid)) {
    stop("Unexpected readiness scores detected.")
  }

  ordered(values, levels = 1:6)
}

check_mapping <- function(raw, coded, label) {

  invalid <- is.na(coded)

  if (any(invalid)) {
    stop(
      "Unmapped ", label, " values: ",
      paste(
        unique(as.character(raw[invalid])),
        collapse = ", "
      )
    )
  }
}

fit_polr <- function(formula, df) {

  MASS::polr(
    formula = formula,
    data = df,
    method = "logistic",
    Hess = TRUE,
    control = list(maxit = 1000)
  )
}


# ============================================================
# 4. IMPORT DATA
# ============================================================

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

cat("\nReading sheet:", sheet_name, "\n")


# ============================================================
# 5. IDENTIFY COLUMNS
# ============================================================

design_col <- find_column(
  raw,
  c("design", "smr_design", "reactor_design"),
  "Design"
)

edition_col <- find_column(
  raw,
  c("edition", "dashboard_edition"),
  "Edition",
  required = FALSE
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
    "neutron_spectrum",
    "spectrum",
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


# ============================================================
# 6. FILTER EDITION 4
# ============================================================

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
  stop("Duplicate design names detected.")
}

if (nrow(data) != 91) {
  stop(
    "Expected 91 designs; found ",
    nrow(data)
  )
}

cat("\nEdition 4 designs:", nrow(data), "\n")


# ============================================================
# 7. RECODE COOLING TYPE
# Reference: Non-water-cooled
# ============================================================

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
    "gas", "gas-cooled", "gas_cooled",
    "metal", "metal-cooled", "metal_cooled",
    "salt", "molten salt",
    "molten-salt-cooled",
    "molten_salt_cooled",
    "heat pipe", "heat-pipe",
    "heat-pipe-cooled",
    "heat_pipe_cooled"
  ) ~ "Non-water-cooled",

  TRUE ~ NA_character_
)

check_mapping(
  cooling_raw,
  data$cooling_group,
  "cooling"
)

data$cooling_group <- factor(
  data$cooling_group,
  levels = c(
    "Non-water-cooled",
    "Water-cooled"
  )
)


# ============================================================
# 8. RECODE NEUTRON SPECTRUM
# Reference: Fast
# ============================================================

spectrum_raw <- normalise(
  data[[spectrum_col]]
)

data$spectrum_group <- dplyr::case_when(

  spectrum_raw %in% c(
    "thermal",
    "thermal spectrum",
    "thermal-spectrum"
  ) ~ "Thermal",

  spectrum_raw %in% c(
    "fast",
    "fast spectrum",
    "fast-spectrum"
  ) ~ "Fast",

  TRUE ~ NA_character_
)

check_mapping(
  spectrum_raw,
  data$spectrum_group,
  "neutron spectrum"
)

data$spectrum_group <- factor(
  data$spectrum_group,
  levels = c("Fast", "Thermal")
)


# ============================================================
# 9. RECODE FUEL TYPE
# Reference: TRISO/Other
# ============================================================

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

check_mapping(
  fuel_raw,
  data$fuel_group,
  "fuel type"
)

data$fuel_group <- factor(
  data$fuel_group,
  levels = c("TRISO/Other", "Uranium")
)


# ============================================================
# 10. RECODE FUEL ENRICHMENT
# Reference: LEU
#
# Exclude not_required and unresolved.
# ============================================================

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
    "not-required",
    "unresolved"
  ) ~ NA_character_,

  TRUE ~ NA_character_
)

valid_exclusions <- enrichment_raw %in% c(
  "not_required",
  "not required",
  "not-required",
  "unresolved"
)

unexpected_enrichment <- is.na(
  data$enrichment_group
) & !valid_exclusions

if (any(unexpected_enrichment)) {
  stop(
    "Unexpected fuel enrichment values: ",
    paste(
      unique(
        as.character(
          enrichment_raw[unexpected_enrichment]
        )
      ),
      collapse = ", "
    )
  )
}

data$enrichment_group <- factor(
  data$enrichment_group,
  levels = c("LEU", "HALEU")
)

cat("\nFuel enrichment classifications:\n")

print(table(
  enrichment_raw,
  useNA = "ifany"
))


# ============================================================
# 11. CODING AUDIT
# ============================================================

coding_audit <- data.frame(
  design = data$design_id,
  cooling_raw = as.character(
    data[[cooling_col]]
  ),
  cooling_group = data$cooling_group,
  spectrum_raw = as.character(
    data[[spectrum_col]]
  ),
  spectrum_group = data$spectrum_group,
  fuel_raw = as.character(
    data[[fuel_col]]
  ),
  fuel_group = data$fuel_group,
  enrichment_raw = as.character(
    data[[enrichment_col]]
  ),
  enrichment_group = data$enrichment_group,
  stringsAsFactors = FALSE
)


# ============================================================
# 12. OUTCOME VARIABLES
# ============================================================

outcome_order <- c(
  "Financing",
  "Supply chain",
  "Fuel supply",
  "Licensing",
  "Siting"
)

for (outcome in outcome_order) {

  source_col <- outcome_columns[[outcome]]

  new_col <- paste0(
    "outcome_",
    gsub(" ", "_", tolower(outcome))
  )

  data[[new_col]] <- make_outcome(
    data[[source_col]]
  )
}


# ============================================================
# 13. MODEL SPECIFICATION
# ============================================================

predictors <- c(
  "cooling_group",
  "spectrum_group",
  "fuel_group",
  "enrichment_group"
)

predictor_labels <- c(
  cooling_group = "Reactor cooling type",
  spectrum_group = "Neutron spectrum",
  fuel_group = "Fuel type",
  enrichment_group = "Fuel enrichment"
)

comparison_labels <- c(
  cooling_group =
    "Water-cooled vs non-water-cooled",
  spectrum_group = "Thermal vs fast",
  fuel_group = "Uranium vs TRISO/Other",
  enrichment_group = "HALEU vs LEU"
)

reference_labels <- c(
  cooling_group = "Non-water-cooled",
  spectrum_group = "Fast",
  fuel_group = "TRISO/Other",
  enrichment_group = "LEU"
)


# ============================================================
# 14. EXTRACT ODDS RATIOS
# ============================================================

extract_results <- function(
    model,
    outcome_name,
    model_type,
    n_used
) {

  coefficient_table <- coef(summary(model))
  coefficient_names <- names(coef(model))

  output <- vector(
    "list",
    length(coefficient_names)
  )

  for (i in seq_along(coefficient_names)) {

    term <- coefficient_names[i]

    beta <- unname(
      coefficient_table[term, "Value"]
    )

    se <- unname(
      coefficient_table[term, "Std. Error"]
    )

    z <- beta / se

    p <- 2 * pnorm(
      abs(z),
      lower.tail = FALSE
    )

    matching_predictor <- predictors[
      startsWith(term, predictors)
    ]

    if (length(matching_predictor) != 1) {
      stop("Unexpected coefficient: ", term)
    }

    predictor <- matching_predictor[1]

    output[[i]] <- data.frame(
      outcome = outcome_name,
      model = model_type,
      predictor = predictor_labels[[predictor]],
      comparison = comparison_labels[[predictor]],
      reference = reference_labels[[predictor]],
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

  dplyr::bind_rows(output)
}


# ============================================================
# 15. OUTPUT CONTAINERS
# ============================================================

full_results_list <- list()
unadjusted_results_list <- list()
drop_one_results_list <- list()
model_fit_list <- list()
sample_audit_list <- list()
predictor_counts_list <- list()


# ============================================================
# 16. RUN FIVE MULTIVARIABLE MODELS
# ============================================================

for (outcome in outcome_order) {

  cat(
    "\n========================================\n",
    "OUTCOME: ", outcome,
    "\n========================================\n",
    sep = ""
  )

  outcome_col <- paste0(
    "outcome_",
    gsub(" ", "_", tolower(outcome))
  )

  model_data <- data.frame(
    design = data$design_id,
    response = data[[outcome_col]],
    cooling_group = data$cooling_group,
    spectrum_group = data$spectrum_group,
    fuel_group = data$fuel_group,
    enrichment_group = data$enrichment_group
  )

  model_data <- model_data[
    complete.cases(
      model_data[, c("response", predictors)]
    ),
    ,
    drop = FALSE
  ]

  n_used <- nrow(model_data)

  cat("Complete-case n:", n_used, "\n")

  if (n_used < 20) {
    stop("Too few complete cases for ", outcome)
  }

  # Check predictor variation.

  for (predictor in predictors) {

    model_data[[predictor]] <- droplevels(
      model_data[[predictor]]
    )

    if (nlevels(model_data[[predictor]]) != 2) {
      stop(
        "Predictor lacks two categories: ",
        predictor, " / ", outcome
      )
    }
  }

  if (length(unique(model_data$response)) < 2) {
    stop("Outcome lacks variation: ", outcome)
  }

  # Full four-predictor formula.

  full_formula <- as.formula(
    paste(
      "response ~",
      paste(predictors, collapse = " + ")
    )
  )

  # Check for perfect predictor collinearity.

  model_matrix <- model.matrix(
    full_formula,
    data = model_data
  )

  if (
    qr(model_matrix)$rank <
    ncol(model_matrix)
  ) {
    stop(
      "Perfect predictor collinearity: ",
      outcome
    )
  }

  # --------------------------------------------------------
  # A. FULL MULTIVARIABLE MODEL
  # --------------------------------------------------------

  full_model <- fit_polr(
    full_formula,
    model_data
  )

  full_results_list[[outcome]] <- extract_results(
    full_model,
    outcome,
    "Full multivariable",
    n_used
  )

  # --------------------------------------------------------
  # B. MODEL FIT
  # --------------------------------------------------------

  null_model <- fit_polr(
    response ~ 1,
    model_data
  )

  full_loglik <- as.numeric(logLik(full_model))
  null_loglik <- as.numeric(logLik(null_model))

  full_df <- attr(logLik(full_model), "df")
  null_df <- attr(logLik(null_model), "df")

  omnibus_lr <- 2 * (
    full_loglik - null_loglik
  )

  omnibus_df <- full_df - null_df

  omnibus_p <- pchisq(
    omnibus_lr,
    df = omnibus_df,
    lower.tail = FALSE
  )

  model_fit_list[[outcome]] <- data.frame(
    outcome = outcome,
    n = n_used,
    predictors = 4,
    log_likelihood = full_loglik,
    AIC = AIC(full_model),
    BIC = BIC(full_model),
    null_log_likelihood = null_loglik,
    omnibus_lr = omnibus_lr,
    omnibus_df = omnibus_df,
    omnibus_p = omnibus_p,
    mcfadden_pseudo_r2 =
      1 - full_loglik / null_loglik,
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------------
  # C. DROP-ONE LIKELIHOOD-RATIO TESTS
  # --------------------------------------------------------

  drop_results <- list()

  for (removed in predictors) {

    remaining <- setdiff(
      predictors,
      removed
    )

    reduced_formula <- as.formula(
      paste(
        "response ~",
        paste(remaining, collapse = " + ")
      )
    )

    reduced_model <- fit_polr(
      reduced_formula,
      model_data
    )

    reduced_loglik <- as.numeric(
      logLik(reduced_model)
    )

    reduced_df <- attr(
      logLik(reduced_model),
      "df"
    )

    lr_statistic <- 2 * (
      full_loglik - reduced_loglik
    )

    lr_df <- full_df - reduced_df

    lr_p <- pchisq(
      lr_statistic,
      df = lr_df,
      lower.tail = FALSE
    )

    drop_results[[removed]] <- data.frame(
      outcome = outcome,
      removed_predictor =
        predictor_labels[[removed]],
      comparison =
        comparison_labels[[removed]],
      n = n_used,
      full_AIC = AIC(full_model),
      reduced_AIC = AIC(reduced_model),
      delta_AIC =
        AIC(reduced_model) - AIC(full_model),
      likelihood_ratio = lr_statistic,
      df = lr_df,
      p_value = lr_p,
      stringsAsFactors = FALSE
    )
  }

  drop_one_results_list[[outcome]] <- (
    dplyr::bind_rows(drop_results)
  )

  # --------------------------------------------------------
  # D. UNIVARIATE MODELS ON IDENTICAL SAMPLE
  # --------------------------------------------------------

  unadjusted_results <- list()

  for (predictor in predictors) {

    univariate_formula <- as.formula(
      paste("response ~", predictor)
    )

    univariate_model <- fit_polr(
      univariate_formula,
      model_data
    )

    unadjusted_results[[predictor]] <- (
      extract_results(
        univariate_model,
        outcome,
        "Unadjusted, matched sample",
        n_used
      )
    )
  }

  unadjusted_results_list[[outcome]] <- (
    dplyr::bind_rows(unadjusted_results)
  )

  # --------------------------------------------------------
  # E. SAMPLE AUDIT
  # --------------------------------------------------------

  sample_audit_list[[outcome]] <- data.frame(
    outcome = outcome,
    original_designs = nrow(data),
    complete_case_n = n_used,
    excluded_n = nrow(data) - n_used,
    unique_outcome_categories =
      length(unique(model_data$response)),
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------------
  # F. PREDICTOR COUNTS
  # --------------------------------------------------------

  counts <- list()

  for (predictor in predictors) {

    tab <- as.data.frame(
      table(model_data[[predictor]]),
      stringsAsFactors = FALSE
    )

    names(tab) <- c("category", "n")

    tab$outcome <- outcome

    tab$predictor <- predictor_labels[[predictor]]

    counts[[predictor]] <- tab[
      ,
      c("outcome", "predictor", "category", "n")
    ]
  }

  predictor_counts_list[[outcome]] <- (
    dplyr::bind_rows(counts)
  )

  cat(
    "Full model AIC:",
    round(AIC(full_model), 3),
    "\n"
  )

  cat(
    "Omnibus p:",
    signif(omnibus_p, 4),
    "\n"
  )
}


# ============================================================
# 17. COMBINE RESULTS
# ============================================================

full_results <- dplyr::bind_rows(
  full_results_list
)

unadjusted_results <- dplyr::bind_rows(
  unadjusted_results_list
)

drop_one_results <- dplyr::bind_rows(
  drop_one_results_list
)

model_fit <- dplyr::bind_rows(
  model_fit_list
)

sample_audit <- dplyr::bind_rows(
  sample_audit_list
)

predictor_counts <- dplyr::bind_rows(
  predictor_counts_list
)


# ============================================================
# 18. MULTIPLE-TESTING ADJUSTMENT
#
# Holm correction across 20 adjusted coefficient tests.
# Separate correction for 20 drop-one tests.
# ============================================================

if (nrow(full_results) != 20) {
  stop("Expected 20 adjusted coefficient results.")
}

if (nrow(unadjusted_results) != 20) {
  stop("Expected 20 matched univariate results.")
}

if (nrow(drop_one_results) != 20) {
  stop("Expected 20 drop-one LR results.")
}

full_results$p_holm <- p.adjust(
  full_results$p_value,
  method = "holm"
)

unadjusted_results$p_holm <- p.adjust(
  unadjusted_results$p_value,
  method = "holm"
)

drop_one_results$p_holm <- p.adjust(
  drop_one_results$p_value,
  method = "holm"
)


# ============================================================
# 19. ADJUSTED VS UNADJUSTED COMPARISON
# ============================================================

unadjusted_comparison <- unadjusted_results[
  ,
  c(
    "outcome", "predictor", "comparison", "n",
    "odds_ratio", "ci_low", "ci_high",
    "p_value"
  )
]

names(unadjusted_comparison)[
  names(unadjusted_comparison) == "odds_ratio"
] <- "unadjusted_OR"

names(unadjusted_comparison)[
  names(unadjusted_comparison) == "ci_low"
] <- "unadjusted_CI_low"

names(unadjusted_comparison)[
  names(unadjusted_comparison) == "ci_high"
] <- "unadjusted_CI_high"

names(unadjusted_comparison)[
  names(unadjusted_comparison) == "p_value"
] <- "unadjusted_p"


adjusted_comparison <- full_results[
  ,
  c(
    "outcome", "predictor", "comparison", "n",
    "odds_ratio", "ci_low", "ci_high",
    "p_value", "p_holm"
  )
]

names(adjusted_comparison)[
  names(adjusted_comparison) == "odds_ratio"
] <- "adjusted_OR"

names(adjusted_comparison)[
  names(adjusted_comparison) == "ci_low"
] <- "adjusted_CI_low"

names(adjusted_comparison)[
  names(adjusted_comparison) == "ci_high"
] <- "adjusted_CI_high"

names(adjusted_comparison)[
  names(adjusted_comparison) == "p_value"
] <- "adjusted_p"

names(adjusted_comparison)[
  names(adjusted_comparison) == "p_holm"
] <- "adjusted_p_holm"


comparison_table <- dplyr::left_join(
  unadjusted_comparison,
  adjusted_comparison,
  by = c(
    "outcome",
    "predictor",
    "comparison",
    "n"
  )
)

comparison_table$OR_change_ratio <- (
  comparison_table$adjusted_OR /
    comparison_table$unadjusted_OR
)

comparison_table$unadjusted_significant <- (
  comparison_table$unadjusted_p < 0.05
)

comparison_table$adjusted_significant <- (
  comparison_table$adjusted_p < 0.05
)

comparison_table$adjusted_holm_significant <- (
  comparison_table$adjusted_p_holm < 0.05
)


# ============================================================
# 20. FUEL SUPPLY RESULTS
# ============================================================

fuel_supply_comparison <- comparison_table[
  comparison_table$outcome == "Fuel supply",
  ,
  drop = FALSE
]

fuel_supply_drop_tests <- drop_one_results[
  drop_one_results$outcome == "Fuel supply",
  ,
  drop = FALSE
]


# ============================================================
# 21. DESCRIPTIVE PREDICTOR COUNTS
# ============================================================

make_counts <- function(variable, label) {

  tab <- as.data.frame(
    table(
      data[[variable]],
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )

  names(tab) <- c("category", "n")

  tab$predictor <- label

  tab[
    ,
    c("predictor", "category", "n")
  ]
}

overall_predictor_counts <- dplyr::bind_rows(
  make_counts(
    "cooling_group",
    "Reactor cooling type"
  ),
  make_counts(
    "spectrum_group",
    "Neutron spectrum"
  ),
  make_counts(
    "fuel_group",
    "Fuel type"
  ),
  make_counts(
    "enrichment_group",
    "Fuel enrichment"
  )
)


# ============================================================
# 22. PREDICTOR CROSS-TABULATIONS
# ============================================================

make_crosstab <- function(variable_a, variable_b) {

  tab <- as.data.frame(
    table(
      data[[variable_a]],
      data[[variable_b]],
      useNA = "no"
    ),
    stringsAsFactors = FALSE
  )

  names(tab) <- c(
    "category_1",
    "category_2",
    "n"
  )

  tab$predictor_1 <- predictor_labels[[variable_a]]
  tab$predictor_2 <- predictor_labels[[variable_b]]

  tab[
    ,
    c(
      "predictor_1",
      "category_1",
      "predictor_2",
      "category_2",
      "n"
    )
  ]
}

predictor_pairs <- combn(
  predictors,
  2,
  simplify = FALSE
)

predictor_crosstabs <- dplyr::bind_rows(
  lapply(
    predictor_pairs,
    function(pair) {
      make_crosstab(pair[1], pair[2])
    }
  )
)


# ============================================================
# 23. MODEL SPECIFICATION RECORD
# ============================================================

model_specification <- data.frame(
  item = c(
    "Data source",
    "Edition",
    "Initial sample",
    "Excluded design",
    "Statistical model",
    "Outcome scale",
    "Number of outcomes",
    "Number of predictors",
    "Cooling reference",
    "Spectrum reference",
    "Fuel type reference",
    "Enrichment reference",
    "Enrichment exclusion",
    "Missing data",
    "Reduced-model comparisons",
    "Univariate comparisons",
    "Confidence intervals",
    "Multiple-testing adjustment",
    "Model selection"
  ),

  specification = c(
    "OECD NEA SMR Dashboard Edition 4",
    "4",
    "91 designs",
    "DF-CDE",
    "Proportional-odds ordinal logistic regression",
    "Ordered categories 1 to 6",
    "5",
    "4",
    "Non-water-cooled",
    "Fast",
    "TRISO/Other",
    "LEU",
    "not_required and unresolved excluded",
    "Complete-case analysis by outcome",
    "Likelihood-ratio tests on identical observations",
    "Univariate models on identical observations",
    "Wald 95% confidence intervals",
    "Holm adjustment reported alongside raw p-values",
    "Prespecified full model; no automatic stepwise selection"
  ),

  stringsAsFactors = FALSE
)


# ============================================================
# 24. CREATE ONE COMBINED EXCEL WORKBOOK
# ============================================================

workbook <- openxlsx::createWorkbook()

add_sheet <- function(sheet_name, contents) {

  openxlsx::addWorksheet(
    workbook,
    sheet_name
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    contents,
    withFilter = TRUE
  )

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstRow = TRUE
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = seq_len(ncol(contents)),
    widths = "auto"
  )

  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#17365D",
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    style = header_style,
    rows = 1,
    cols = seq_len(ncol(contents)),
    gridExpand = TRUE
  )
}

add_sheet(
  "Adjusted results",
  full_results
)

add_sheet(
  "Drop-one LR tests",
  drop_one_results
)

add_sheet(
  "Unadjusted results",
  unadjusted_results
)

add_sheet(
  "Adjusted vs unadjusted",
  comparison_table
)

add_sheet(
  "Fuel supply comparison",
  fuel_supply_comparison
)

add_sheet(
  "Fuel supply LR tests",
  fuel_supply_drop_tests
)

add_sheet(
  "Model fit",
  model_fit
)

add_sheet(
  "Sample audit",
  sample_audit
)

add_sheet(
  "Model category counts",
  predictor_counts
)

add_sheet(
  "Overall category counts",
  overall_predictor_counts
)

add_sheet(
  "Predictor cross-tabs",
  predictor_crosstabs
)

add_sheet(
  "Coding audit",
  coding_audit
)

add_sheet(
  "Model specification",
  model_specification
)


# ============================================================
# 25. SAVE WORKBOOK
# ============================================================

openxlsx::saveWorkbook(
  workbook,
  output_file,
  overwrite = TRUE
)


# ============================================================
# 26. PRINT PRINCIPAL RESULTS
# ============================================================

cat(
  "\n========================================\n",
  "MULTIVARIABLE ANALYSIS COMPLETED\n",
  "========================================\n",
  sep = ""
)

cat("\nInput designs:", nrow(data), "\n")

cat("\nComplete-case sample sizes:\n")

print(
  sample_audit,
  row.names = FALSE
)

cat("\nADJUSTED FUEL SUPPLY RESULTS:\n")

print(
  full_results[
    full_results$outcome == "Fuel supply",
    c(
      "predictor",
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

cat("\nFUEL SUPPLY DROP-ONE TESTS:\n")

print(
  fuel_supply_drop_tests[
    ,
    c(
      "removed_predictor",
      "n",
      "likelihood_ratio",
      "df",
      "p_value",
      "p_holm"
    )
  ],
  row.names = FALSE
)

cat("\nFULL MODEL FIT:\n")

print(
  model_fit[
    ,
    c(
      "outcome",
      "n",
      "AIC",
      "omnibus_lr",
      "omnibus_p",
      "mcfadden_pseudo_r2"
    )
  ],
  row.names = FALSE
)

cat(
  "\nOUTPUT WORKBOOK:\n",
  output_file,
  "\n",
  sep = ""
)

cat("\nAll results are contained in ONE workbook.\n")
cat("\nEND OF SCRIPT.\n")
