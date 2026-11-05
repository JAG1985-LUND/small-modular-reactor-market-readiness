
# ================================================================
# SUPPLEMENTARY TABLE S10
# MULTIVARIABLE TECHNOLOGICAL ASSOCIATIONS
# NEA SMR DASHBOARD, EDITION 4
#
# Input:
# revision/data/fig08_nea_technological_source_data.xlsx
#
# Output:
# revision/figures/table_S10_multivariable_technology_results.xlsx
#
# Models:
# - Four-predictor proportional-odds regression
# - Five market-readiness outcomes
# - Four drop-one likelihood-ratio tests per outcome
# - Matched-sample univariate comparisons
# - Holm multiple-testing adjustment
# - Predictor and sample audits
#
# No figures. One Excel workbook, 13 worksheets.
# ================================================================


# 1. PACKAGES ----------------------------------------------------

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
  stop(
    "Install the following R packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(MASS)
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
      "\nAccepted names: ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  NA_character_
}

make_outcome <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(as.character(x))
  )
  
  x[is.na(x) | !(x %in% 1:6)] <- NA_real_
  
  ordered(x, levels = 1:6)
}


# 4. IMPORT DATA -------------------------------------------------

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
    !is.na(edition_values) & edition_values == 4,
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


# 7. RECODE COOLING TYPE -----------------------------------------

# Reference: Non-water-cooled

cooling_raw <- normalise(
  data[[cooling_col]]
)

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
    "heat_pipe_cooled"
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


# 8. RECODE NEUTRON SPECTRUM -------------------------------------

# Reference: Fast

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

data$spectrum_group <- factor(
  data$spectrum_group,
  levels = c("Fast", "Thermal")
)


# 9. RECODE FUEL TYPE --------------------------------------------

# Reference: TRISO/Other

fuel_raw <- normalise(
  data[[fuel_col]]
)

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


# 10. RECODE FUEL ENRICHMENT -------------------------------------

# Reference: LEU
# Designs with enrichment not required are excluded.

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


# 11. VALIDATE CODING --------------------------------------------

required_predictors <- c(
  "cooling_group",
  "spectrum_group",
  "fuel_group"
)

if (anyNA(data[, required_predictors])) {
  stop(
    "Unmapped cooling, spectrum or fuel values. ",
    "Check the input workbook."
  )
}

valid_not_required <- enrichment_raw %in% c(
  "not_required",
  "not required",
  "not-required"
)

unexpected_enrichment <- (
  is.na(data$enrichment_group) &
    !valid_not_required
)

if (any(unexpected_enrichment)) {
  stop("Unexpected fuel enrichment values detected.")
}


# 12. CODING AUDIT -----------------------------------------------

coding_audit <- data.frame(
  design = data$design_id,
  cooling_raw = as.character(data[[cooling_col]]),
  cooling_group = data$cooling_group,
  spectrum_raw = as.character(data[[spectrum_col]]),
  spectrum_group = data$spectrum_group,
  fuel_raw = as.character(data[[fuel_col]]),
  fuel_group = data$fuel_group,
  enrichment_raw = as.character(data[[enrichment_col]]),
  enrichment_group = data$enrichment_group,
  stringsAsFactors = FALSE
)


# 13. OUTCOME VARIABLES ------------------------------------------

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


# 14. MODEL SPECIFICATION ----------------------------------------

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
  cooling_group = "Water-cooled vs non-water-cooled",
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


# 15. EXTRACT ODDS RATIOS ----------------------------------------

extract_results <- function(
    model,
    outcome_name,
    model_type,
    n_used
) {
  
  coefficient_table <- coef(summary(model))
  coefficient_names <- names(coef(model))
  
  output <- list()
  
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


# 16. RESULT CONTAINERS ------------------------------------------

full_results_list <- list()
unadjusted_results_list <- list()
drop_one_results_list <- list()
model_fit_list <- list()
sample_audit_list <- list()
predictor_counts_list <- list()


# 17. RUN FIVE OUTCOMES ------------------------------------------

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
  
  # Complete-case dataset for this outcome.
  
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
  
  # Check predictor categories.
  
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
  
  # Check for perfect collinearity.
  
  full_formula <- as.formula(
    paste(
      "response ~",
      paste(predictors, collapse = " + ")
    )
  )
  
  model_matrix <- model.matrix(
    full_formula,
    data = model_data
  )
  
  if (qr(model_matrix)$rank < ncol(model_matrix)) {
    stop(
      "Perfect collinearity detected for ",
      outcome
    )
  }
  
  # Full four-predictor model.
  
  full_model <- MASS::polr(
    formula = full_formula,
    data = model_data,
    method = "logistic",
    Hess = TRUE
  )
  
  full_results_list[[outcome]] <- extract_results(
    full_model,
    outcome,
    "Full multivariable",
    n_used
  )
  
  # Model fit.
  
  null_model <- MASS::polr(
    response ~ 1,
    data = model_data,
    method = "logistic",
    Hess = TRUE
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
    mcfadden_pseudo_r2 = (
      1 - full_loglik / null_loglik
    ),
    stringsAsFactors = FALSE
  )
  
  # Drop-one-predictor likelihood-ratio tests.
  
  drop_results <- list()
  
  for (removed in predictors) {
    
    remaining <- setdiff(predictors, removed)
    
    reduced_formula <- as.formula(
      paste(
        "response ~",
        paste(remaining, collapse = " + ")
      )
    )
    
    reduced_model <- MASS::polr(
      formula = reduced_formula,
      data = model_data,
      method = "logistic",
      Hess = TRUE
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
      removed_predictor = predictor_labels[[removed]],
      comparison = comparison_labels[[removed]],
      n = n_used,
      full_AIC = AIC(full_model),
      reduced_AIC = AIC(reduced_model),
      delta_AIC = AIC(reduced_model) - AIC(full_model),
      likelihood_ratio = lr_statistic,
      df = lr_df,
      p_value = lr_p,
      stringsAsFactors = FALSE
    )
  }
  
  drop_one_results_list[[outcome]] <- (
    dplyr::bind_rows(drop_results)
  )
  
  # Univariate models using exactly the same rows.
  
  unadjusted_results <- list()
  
  for (predictor in predictors) {
    
    univariate_formula <- as.formula(
      paste("response ~", predictor)
    )
    
    univariate_model <- MASS::polr(
      formula = univariate_formula,
      data = model_data,
      method = "logistic",
      Hess = TRUE
    )
    
    unadjusted_results[[predictor]] <- extract_results(
      univariate_model,
      outcome,
      "Unadjusted, matched sample",
      n_used
    )
  }
  
  unadjusted_results_list[[outcome]] <- (
    dplyr::bind_rows(unadjusted_results)
  )
  
  # Sample audit.
  
  sample_audit_list[[outcome]] <- data.frame(
    outcome = outcome,
    original_designs = nrow(data),
    complete_case_n = n_used,
    excluded_n = nrow(data) - n_used,
    unique_outcome_categories = length(
      unique(model_data$response)
    ),
    stringsAsFactors = FALSE
  )
  
  # Category counts within complete-case sample.
  
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
    "Full model omnibus p:",
    signif(omnibus_p, 4),
    "\n"
  )
}


# 18. COMBINE RESULTS --------------------------------------------

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


# 19. MULTIPLE-TESTING ADJUSTMENT --------------------------------

# Holm adjustment across the 20 coefficient tests,
# and separately across the 20 drop-one tests.

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


# 20. ADJUSTED VS UNADJUSTED -------------------------------------

unadjusted_comparison <- unadjusted_results[
  ,
  c(
    "outcome", "predictor", "comparison", "n",
    "odds_ratio", "ci_low", "ci_high", "p_value"
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


# 21. FUEL SUPPLY RESULTS ----------------------------------------

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


# 22. OVERALL PREDICTOR COUNTS -----------------------------------

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
  
  tab[, c("predictor", "category", "n")]
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


# 23. PREDICTOR CROSS-TABULATIONS --------------------------------

make_crosstab <- function(
    variable_a,
    variable_b
) {
  
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


# 24. MODEL SPECIFICATION RECORD ---------------------------------

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
    "not_required excluded from multivariable models",
    "Complete-case analysis by outcome",
    "Likelihood-ratio tests on identical observations",
    "Univariate models on identical observations",
    "Wald 95% confidence intervals",
    "Holm adjustment reported alongside raw p-values",
    "Prespecified full model; no automatic stepwise selection"
  ),
  
  stringsAsFactors = FALSE
)


# 25. CREATE EXCEL WORKBOOK --------------------------------------

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


# 26. SAVE WORKBOOK ----------------------------------------------

openxlsx::saveWorkbook(
  workbook,
  output_file,
  overwrite = TRUE
)

if (
  !file.exists(output_file) ||
  is.na(file.info(output_file)$size) ||
  file.info(output_file)$size == 0
) {
  stop("Output workbook was not created correctly.")
}


# 27. PRINT RESULTS ----------------------------------------------

cat(
  "\n========================================\n",
  "MULTIVARIABLE ANALYSIS COMPLETED\n",
  "========================================\n",
  sep = ""
)

cat("\nInput designs:", nrow(data), "\n")

cat("\nFuel enrichment categories:\n")
print(table(enrichment_raw, useNA = "ifany"))

cat("\nComplete-case sample sizes:\n")
print(sample_audit, row.names = FALSE)

cat("\nADJUSTED FUEL SUPPLY RESULTS:\n")

print(
  full_results[
    full_results$outcome == "Fuel supply",
    c(
      "predictor", "n", "odds_ratio",
      "ci_low", "ci_high",
      "p_value", "p_holm"
    )
  ],
  row.names = FALSE
)

cat("\nFUEL SUPPLY DROP-ONE TESTS:\n")

print(
  fuel_supply_drop_tests[
    ,
    c(
      "removed_predictor", "n",
      "likelihood_ratio", "df",
      "p_value", "p_holm"
    )
  ],
  row.names = FALSE
)

cat("\nFULL MODEL FIT:\n")

print(
  model_fit[
    ,
    c(
      "outcome", "n", "AIC",
      "omnibus_lr", "omnibus_p",
      "mcfadden_pseudo_r2"
    )
  ],
  row.names = FALSE
)

cat("\nOUTPUT WORKBOOK:\n", output_file, "\n")
cat("\nExcel worksheets: 13\n")
cat("\nEND OF SCRIPT.\n")
