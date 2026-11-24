# ================================================================
# SUPPLEMENTARY TABLES S14 AND S15
# NEA SMR DASHBOARD, EDITION 4
# JOINT REGIONAL AND TECHNOLOGICAL ORDINAL REGRESSION
# GITHUB REPRODUCIBLE VERSION
#
# Input:
# revision/data/edition_4_regional_technology_source_data.xlsx
#
# Output:
# revision/figures/supp_tables_S14_S15_joint_models_results.xlsx
#
# S14: Fully adjusted ordinal logistic regression coefficients
#      All designs (n = 85)
#      Conventional SMRs (n = 51)
#
# S15: Nested-model likelihood-ratio comparisons
#
# 91 source designs
# 89 designs following regional exclusions
# 4 designs excluded owing to unresolved enrichment
# 85 complete-case designs
# 51 conventional SMRs
#
# 40 ordinal regression models
# 90 fully adjusted regression coefficients
# 20 nested-model comparisons
#
# Reference categories:
# North America, water-cooled, thermal spectrum,
# uranium fuel and LEU
# ================================================================

# ================================================================
# 1. PACKAGES
# ================================================================

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "ordinal",
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

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

# ================================================================
# 2. REPOSITORY PATHS
# ================================================================

source_filename <-
  "edition_4_regional_technology_source_data.xlsx"

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

  for (frame in sys.frames()) {

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
    "Repository root not found. Expected revision/data/",
    source_filename,
    ". Run from the repository or set SMR_REPO_ROOT."
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

excel_file <- file.path(
  output_dir,
  "supp_tables_S14_S15_joint_models_results.xlsx"
)

diagnostic_file <- file.path(
  output_dir,
  "supp_S03_model_failure_diagnostics.xlsx"
)

cat("\nRepository root:\n", repo_root, "\n")
cat("\nInput workbook:\n", input_file, "\n")
cat("\nOutput directory:\n", output_dir, "\n")

# ================================================================
# 3. READ SOURCE DATA
# ================================================================

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

names(raw) <- names(raw) |>
  stringr::str_trim() |>
  stringr::str_to_lower() |>
  stringr::str_replace_all("[^a-z0-9]+", "_") |>
  stringr::str_replace_all("^_|_$", "")

if (
  !"neutron_spectrum" %in% names(raw) &&
  "neturon_spectrum" %in% names(raw)
) {
  raw$neutron_spectrum <- raw$neturon_spectrum
}

required_columns <- c(
  "design",
  "region_cn_ru",
  "microreactor",
  "cooling_type",
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
  required_columns,
  names(raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
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
  stop("Duplicate design identifiers detected.")
}

if (nrow(raw) != 91) {
  stop(
    "Expected 91 source designs; found ",
    nrow(raw)
  )
}

# ================================================================
# 4. STANDARDISATION FUNCTIONS
# ================================================================

clean_text <- function(x) {

  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[_-]+", " ") |>
    stringr::str_squish()
}

standardise_region <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z %in% c(
      "north america",
      "na"
    ) ~ "North America",

    z %in% c(
      "europe",
      "eu"
    ) ~ "Europe",

    z %in% c(
      "oecd asia",
      "oecd asian"
    ) ~ "OECD Asia",

    z %in% c(
      "china/russia",
      "china russia",
      "china and russia",
      "cn ru"
    ) ~ "China/Russia",

    z %in% c(
      "rest of world",
      "rest of the world",
      "row"
    ) ~ "Rest of world",

    TRUE ~ NA_character_
  )
}

standardise_micro <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z %in% c(
      "true",
      "t",
      "1",
      "yes"
    ) ~ TRUE,

    z %in% c(
      "false",
      "f",
      "0",
      "no"
    ) ~ FALSE,

    TRUE ~ NA
  )
}

standardise_cooling <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z %in% c(
      "water",
      "water cooled",
      "watercooled"
    ) ~ "Water-cooled",

    z %in% c(
      "gas",
      "metal",
      "salt",
      "heat pipe",
      "heat pipes",
      "heatpipe",
      "heatpipe cooled",
      "heat pipe cooled",
      "non water",
      "non water cooled",
      "nonwater",
      "gas cooled",
      "metal cooled",
      "salt cooled",
      "molten salt",
      "liquid metal",
      "sodium",
      "lead",
      "helium"
    ) ~ "Non-water-cooled",

    TRUE ~ NA_character_
  )
}

standardise_spectrum <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z %in% c(
      "thermal",
      "thermal spectrum"
    ) ~ "Thermal",

    z %in% c(
      "fast",
      "fast spectrum"
    ) ~ "Fast",

    TRUE ~ NA_character_
  )
}

standardise_fuel <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z %in% c(
      "uranium",
      "uranium fuel"
    ) ~ "Uranium",

    z %in% c(
      "triso",
      "triso fuel"
    ) ~ "TRISO",

    z %in% c(
      "other",
      "other fuel"
    ) ~ "Other",

    TRUE ~ NA_character_
  )
}

standardise_enrichment <- function(x) {

  z <- clean_text(x)

  dplyr::case_when(

    z == "leu" ~ "LEU",

    z == "haleu" ~ "HALEU",

    z %in% c(
      "not required",
      "notrequired",
      "no enrichment",
      "no enrichment required"
    ) ~ "Not required",

    TRUE ~ NA_character_
  )
}

# ================================================================
# 5. BUILD ANALYSIS DATASET
# ================================================================

outcomes <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting"
)

data <- tibble::tibble(

  design = as.character(raw$design),

  region_raw = as.character(raw$region_cn_ru),
  microreactor_raw = as.character(raw$microreactor),
  cooling_raw = as.character(raw$cooling_type),
  spectrum_raw = as.character(raw$neutron_spectrum),
  fuel_raw = as.character(raw$fuel_type),
  enrichment_raw = as.character(raw$fuel_enrichment),

  region = standardise_region(raw$region_cn_ru),
  microreactor = standardise_micro(raw$microreactor),
  cooling = standardise_cooling(raw$cooling_type),
  spectrum = standardise_spectrum(raw$neutron_spectrum),
  fuel = standardise_fuel(raw$fuel_type),
  enrichment = standardise_enrichment(raw$fuel_enrichment),

  financing = raw$financing,
  supply_chain = raw$supply_chain,
  fuel_supply = raw$fuel_supply,
  licensing = raw$licensing,
  siting = raw$siting
)

for (v in outcomes) {

  data[[v]] <- suppressWarnings(
    as.numeric(as.character(data[[v]]))
  )

  invalid <- !is.na(data[[v]]) &
    (
      data[[v]] < 1 |
      data[[v]] > 6 |
      data[[v]] != floor(data[[v]])
    )

  if (any(invalid)) {
    stop("Invalid readiness scores in: ", v)
  }
}

# ================================================================
# 6. CLASSIFICATION AUDIT
# ================================================================

invalid_classifications <- data |>
  dplyr::filter(
    is.na(region) |
      is.na(microreactor) |
      is.na(cooling) |
      is.na(spectrum) |
      is.na(fuel)
  )

if (nrow(invalid_classifications) > 0) {

  print(
    invalid_classifications,
    n = Inf,
    width = Inf
  )

  stop("Unrecognised classifications detected.")
}

region_audit <- data |>
  dplyr::count(
    region,
    name = "n"
  )

excluded_region <- data |>
  dplyr::filter(
    region == "Rest of world"
  )

regional_data <- data |>
  dplyr::filter(
    region %in% c(
      "North America",
      "Europe",
      "OECD Asia",
      "China/Russia"
    )
  )

excluded_enrichment <- regional_data |>
  dplyr::filter(is.na(enrichment)) |>
  dplyr::select(
    design,
    region,
    microreactor,
    enrichment_raw,
    dplyr::all_of(outcomes)
  )

# ================================================================
# 7. COMPLETE-CASE SAMPLES
# ================================================================

predictors <- c(
  "region",
  "cooling",
  "spectrum",
  "fuel",
  "enrichment"
)

analysis_variables <- c(
  predictors,
  outcomes
)

all_designs <- regional_data |>
  dplyr::filter(
    stats::complete.cases(
      dplyr::pick(
        dplyr::all_of(analysis_variables)
      )
    )
  )

conventional <- all_designs |>
  dplyr::filter(
    microreactor == FALSE
  )

prepare_factors <- function(df) {

  df |>
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
          "Water-cooled",
          "Non-water-cooled"
        )
      ),

      spectrum = factor(
        spectrum,
        levels = c(
          "Thermal",
          "Fast"
        )
      ),

      fuel = factor(
        fuel,
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
}

all_designs <- prepare_factors(all_designs)
conventional <- prepare_factors(conventional)

sample_audit <- tibble::tibble(

  sample = c(
    "Source dataset",
    "Four-region dataset",
    "Unresolved enrichment",
    "All-design complete cases",
    "Conventional SMRs"
  ),

  n = c(
    nrow(data),
    nrow(regional_data),
    nrow(excluded_enrichment),
    nrow(all_designs),
    nrow(conventional)
  )
)

print(sample_audit)

if (
  nrow(data) != 91 ||
  nrow(regional_data) != 89 ||
  nrow(excluded_enrichment) != 4 ||
  nrow(all_designs) != 85 ||
  nrow(conventional) != 51
) {
  stop(
    "Unexpected sample sizes. ",
    "Inspect the source data."
  )
}

# ================================================================
# 8. DIAGNOSTIC DISTRIBUTIONS
# ================================================================

fuel_distribution <- dplyr::bind_rows(

  lapply(

    list(
      all_designs = all_designs,
      conventional = conventional
    ),

    function(df) {

      df |>
        dplyr::count(
          fuel_supply,
          name = "n"
        )
    }
  ),

  .id = "sample"
)

fuel_by_region <- all_designs |>
  dplyr::count(
    region,
    fuel_supply,
    name = "n"
  )

fuel_by_cooling <- all_designs |>
  dplyr::count(
    cooling,
    fuel_supply,
    name = "n"
  )

fuel_by_spectrum <- all_designs |>
  dplyr::count(
    spectrum,
    fuel_supply,
    name = "n"
  )

fuel_by_fuel <- all_designs |>
  dplyr::count(
    fuel,
    fuel_supply,
    name = "n"
  )

fuel_by_enrichment <- all_designs |>
  dplyr::count(
    enrichment,
    fuel_supply,
    name = "n"
  )

# ================================================================
# 9. MODEL SPECIFICATIONS
# ================================================================

samples <- list(
  all_designs = all_designs,
  conventional = conventional
)

model_specs <- list(

  M0 = character(0),

  M1_region = "region",

  M2_technology = c(
    "cooling",
    "spectrum",
    "fuel",
    "enrichment"
  ),

  M3_combined = c(
    "region",
    "cooling",
    "spectrum",
    "fuel",
    "enrichment"
  )
)

make_formula <- function(outcome, terms) {

  rhs <- if (length(terms) == 0) {
    "1"
  } else {
    paste(terms, collapse = " + ")
  }

  stats::as.formula(
    paste(outcome, "~", rhs)
  )
}

# ================================================================
# 10. ROBUST ORDINAL MODEL FITTING
# ================================================================

fit_clm <- function(df, outcome, terms) {

  model_df <- df |>
    dplyr::select(
      dplyr::all_of(
        c(outcome, terms)
      )
    )

  model_df[[outcome]] <- ordered(
    model_df[[outcome]],
    levels = 1:6
  )

  model_df[[outcome]] <- droplevels(
    model_df[[outcome]]
  )

  if (nlevels(model_df[[outcome]]) < 2) {

    return(
      list(
        model = NULL,
        status = "FAILED",
        attempt = NA_character_,
        error = "Fewer than two observed outcome categories",
        max_gradient = NA_real_,
        condition_number = NA_real_
      )
    )
  }

  formula <- make_formula(
    outcome,
    terms
  )

  attempts <- list(

    list(
      name = "Newton_standard",
      maxIter = 100,
      gradTol = 1e-5
    ),

    list(
      name = "Newton_extended",
      maxIter = 500,
      gradTol = 1e-6
    )
  )

  errors <- character()

  for (attempt in attempts) {

    warning_messages <- character()

    fitted <- tryCatch(

      withCallingHandlers(

        ordinal::clm(
          formula = formula,
          data = model_df,
          link = "logit",
          threshold = "flexible",
          Hess = TRUE,
          control = ordinal::clm.control(
            method = "Newton",
            maxIter = attempt$maxIter,
            gradTol = attempt$gradTol
          )
        ),

        warning = function(w) {

          warning_messages <<- c(
            warning_messages,
            conditionMessage(w)
          )

          invokeRestart("muffleWarning")
        }
      ),

      error = function(e) {

        errors <<- c(
          errors,
          paste(
            attempt$name,
            conditionMessage(e),
            sep = ": "
          )
        )

        NULL
      }
    )

    if (is.null(fitted)) {
      next
    }

    convergence <- suppressWarnings(
      as.integer(
        as.character(fitted$convergence)[1]
      )
    )

    if (
      length(convergence) != 1L ||
      is.na(convergence) ||
      convergence != 0L
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "convergence code:",
          paste(
            fitted$convergence,
            collapse = ", "
          )
        )
      )

      next
    }

    ll <- tryCatch(
      as.numeric(
        stats::logLik(fitted)
      ),
      error = function(e) NA_real_
    )

    if (!is.finite(ll)) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "non-finite log likelihood"
        )
      )

      next
    }

    covariance <- tryCatch(
      stats::vcov(fitted),
      error = function(e) NULL
    )

    if (
      is.null(covariance) ||
      any(!is.finite(covariance)) ||
      any(diag(covariance) <= 0)
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "invalid covariance matrix"
        )
      )

      next
    }

    beta <- fitted$beta

    if (
      length(terms) > 0 &&
      (
        length(beta) == 0 ||
        any(!is.finite(beta))
      )
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "invalid regression coefficients"
        )
      )

      next
    }

    if (
      length(terms) > 0 &&
      !all(
        names(beta) %in% rownames(covariance)
      )
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "coefficient covariance mismatch"
        )
      )

      next
    }

    max_gradient <- tryCatch(
      max(
        abs(fitted$gradient)
      ),
      error = function(e) NA_real_
    )

    condition_number <- tryCatch(
      kappa(
        fitted$Hessian
      ),
      error = function(e) NA_real_
    )

    if (
      !is.finite(max_gradient) ||
      max_gradient > attempt$gradTol
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "gradient criterion not satisfied:",
          signif(max_gradient, 5)
        )
      )

      next
    }

    if (
      !is.finite(condition_number) ||
      condition_number > 1e10
    ) {

      errors <- c(
        errors,
        paste(
          attempt$name,
          "ill-conditioned Hessian:",
          signif(condition_number, 5)
        )
      )

      next
    }

    return(
      list(
        model = fitted,
        status = "Converged",
        attempt = attempt$name,
        error = paste(
          unique(
            c(
              errors,
              warning_messages
            )
          ),
          collapse = " | "
        ),
        max_gradient = max_gradient,
        condition_number = condition_number
      )
    )
  }

  list(
    model = NULL,
    status = "FAILED",
    attempt = NA_character_,
    error = paste(
      unique(errors),
      collapse = " | "
    ),
    max_gradient = NA_real_,
    condition_number = NA_real_
  )
}

# ================================================================
# 11. FIT ALL 40 MODELS
# ================================================================

models <- list()
model_audit_list <- list()

counter <- 1

for (sample_name in names(samples)) {

  models[[sample_name]] <- list()

  for (outcome in outcomes) {

    models[[sample_name]][[outcome]] <- list()

    for (spec_name in names(model_specs)) {

      cat(
        "\nFITTING:",
        sample_name,
        "|",
        outcome,
        "|",
        spec_name,
        "\n"
      )

      result <- fit_clm(
        df = samples[[sample_name]],
        outcome = outcome,
        terms = model_specs[[spec_name]]
      )

      models[[sample_name]][[outcome]][[spec_name]] <-
        result$model

      model_audit_list[[counter]] <- tibble::tibble(

        sample = sample_name,
        outcome = outcome,
        specification = spec_name,
        n = nrow(samples[[sample_name]]),
        status = result$status,
        optimizer = result$attempt,
        max_gradient = result$max_gradient,
        condition_number = result$condition_number,
        error = result$error
      )

      cat(
        "STATUS:",
        result$status,
        "\n"
      )

      counter <- counter + 1
    }
  }
}

model_audit <- dplyr::bind_rows(
  model_audit_list
)

if (nrow(model_audit) != 40) {
  stop("Expected 40 model audit records.")
}

failed_models <- model_audit |>
  dplyr::filter(
    status != "Converged"
  )

# ================================================================
# 12. MODEL FAILURE DIAGNOSTICS
# ================================================================

if (nrow(failed_models) > 0) {

  diagnostic_tabs <- list(

    "Model audit" = model_audit,
    "Failed models" = failed_models,
    "Sample audit" = sample_audit,
    "Fuel distribution" = fuel_distribution,
    "Fuel by region" = fuel_by_region,
    "Fuel by cooling" = fuel_by_cooling,
    "Fuel by spectrum" = fuel_by_spectrum,
    "Fuel by fuel" = fuel_by_fuel,
    "Fuel by enrichment" = fuel_by_enrichment,
    "Excluded enrichment" = excluded_enrichment,
    "Complete case all" = all_designs,
    "Complete case SMRs" = conventional
  )

  openxlsx::write.xlsx(
    diagnostic_tabs,
    file = diagnostic_file,
    overwrite = TRUE
  )

  print(
    failed_models,
    n = Inf,
    width = Inf
  )

  stop(
    "\nOne or more models failed numerical validation.\n",
    "No incomplete regression tables were exported.\n",
    "Diagnostic workbook:\n",
    diagnostic_file
  )
}

cat(
  "\nALL 40 MODELS PASSED VALIDATION.\n"
)

# ================================================================
# 13. EXTRACT REGRESSION COEFFICIENTS
# ================================================================

extract_coefficients <- function(
    model,
    sample_name,
    outcome,
    n
) {

  beta <- model$beta

  covariance <- stats::vcov(
    model
  )

  se <- sqrt(
    diag(covariance)[names(beta)]
  )

  z <- beta / se

  p <- 2 * stats::pnorm(
    abs(z),
    lower.tail = FALSE
  )

  tibble::tibble(

    sample = sample_name,
    outcome = outcome,
    n = n,
    predictor = names(beta),
    coefficient = as.numeric(beta),
    standard_error = as.numeric(se),

    odds_ratio = exp(
      as.numeric(beta)
    ),

    ci_low = exp(
      as.numeric(beta) -
        1.96 * as.numeric(se)
    ),

    ci_high = exp(
      as.numeric(beta) +
        1.96 * as.numeric(se)
    ),

    p_wald = as.numeric(p)
  )
}

coefficient_list <- list()

counter <- 1

for (sample_name in names(samples)) {

  for (outcome in outcomes) {

    model <- models[[sample_name]][[outcome]][["M3_combined"]]

    coefficient_list[[counter]] <-
      extract_coefficients(

        model = model,
        sample_name = sample_name,
        outcome = outcome,
        n = nrow(samples[[sample_name]])
      )

    counter <- counter + 1
  }
}

coefficients <- dplyr::bind_rows(
  coefficient_list
) |>

  dplyr::group_by(
    sample,
    outcome
  ) |>

  dplyr::mutate(

    p_bh_within_outcome = stats::p.adjust(
      p_wald,
      method = "BH"
    ),

    p_holm_within_outcome = stats::p.adjust(
      p_wald,
      method = "holm"
    )
  ) |>

  dplyr::ungroup()

# ================================================================
# 14. PREDICTOR LABELS
# ================================================================

term_labels <- c(

  "regionEurope" = "Europe",
  "regionOECD Asia" = "OECD Asia",
  "regionChina/Russia" = "China/Russia",
  "coolingNon-water-cooled" = "Non-water-cooled",
  "spectrumFast" = "Fast-spectrum",
  "fuelTRISO" = "TRISO fuel",
  "fuelOther" = "Other fuel",
  "enrichmentHALEU" = "HALEU",
  "enrichmentNot required" = "Enrichment not required"
)

coefficients$term_label <- unname(
  term_labels[coefficients$predictor]
)

if (anyNA(coefficients$term_label)) {

  stop(
    "Unrecognised coefficient names: ",
    paste(
      unique(
        coefficients$predictor[
          is.na(coefficients$term_label)
        ]
      ),
      collapse = ", "
    )
  )
}

# ================================================================
# 15. COEFFICIENT AUDIT
# ================================================================

coefficient_audit <- coefficients |>

  dplyr::group_by(
    sample,
    outcome
  ) |>

  dplyr::summarise(
    n_coefficients = dplyr::n(),
    .groups = "drop"
  )

if (
  nrow(coefficients) != 90 ||
  nrow(coefficient_audit) != 10 ||
  any(
    coefficient_audit$n_coefficients != 9
  )
) {

  stop(
    "Incomplete coefficient output. ",
    "Expected 90 coefficients across 10 models."
  )
}

for (sample_name in names(samples)) {

  for (outcome_name in outcomes) {

    terms_found <- coefficients |>

      dplyr::filter(
        sample == sample_name,
        outcome == outcome_name
      ) |>

      dplyr::pull(predictor)

    if (
      !setequal(
        terms_found,
        names(term_labels)
      )
    ) {

      stop(
        "Missing or unexpected predictors in ",
        sample_name,
        " | ",
        outcome_name
      )
    }
  }
}

if (
  any(!is.finite(coefficients$odds_ratio)) ||
  any(!is.finite(coefficients$ci_low)) ||
  any(!is.finite(coefficients$ci_high)) ||
  any(coefficients$ci_low <= 0)
) {
  stop("Invalid odds ratios or confidence intervals.")
}

# ================================================================
# 16. LIKELIHOOD-RATIO FUNCTION
# ================================================================

extract_lr <- function(
    larger_model,
    smaller_model
) {

  ll_large <- stats::logLik(
    larger_model
  )

  ll_small <- stats::logLik(
    smaller_model
  )

  lr <- 2 * (
    as.numeric(ll_large) -
      as.numeric(ll_small)
  )

  df <- as.integer(
    attr(ll_large, "df") -
      attr(ll_small, "df")
  )

  if (
    !is.finite(lr) ||
    df <= 0 ||
    lr < -1e-5
  ) {

    stop(
      "Invalid nested-model likelihood-ratio comparison."
    )
  }

  lr <- max(
    0,
    lr
  )

  list(

    lr = lr,

    df = df,

    p = stats::pchisq(
      lr,
      df = df,
      lower.tail = FALSE
    )
  )
}

# ================================================================
# 17. OMNIBUS TESTS
# ================================================================

omnibus_list <- list()

counter <- 1

for (sample_name in names(samples)) {

  for (outcome in outcomes) {

    m0 <- models[[sample_name]][[outcome]][["M0"]]

    m3 <- models[[sample_name]][[outcome]][["M3_combined"]]

    test <- extract_lr(
      m3,
      m0
    )

    omnibus_list[[counter]] <- tibble::tibble(

      sample = sample_name,
      outcome = outcome,
      n = nrow(samples[[sample_name]]),
      lr_chisq = test$lr,
      df = test$df,
      p_raw = test$p,

      mcfadden_r2 = 1 -
        as.numeric(stats::logLik(m3)) /
        as.numeric(stats::logLik(m0))
    )

    counter <- counter + 1
  }
}

omnibus <- dplyr::bind_rows(
  omnibus_list
) |>

  dplyr::group_by(sample) |>

  dplyr::mutate(

    p_bh = stats::p.adjust(
      p_raw,
      method = "BH"
    ),

    p_holm = stats::p.adjust(
      p_raw,
      method = "holm"
    )
  ) |>

  dplyr::ungroup()

# ================================================================
# 18. NESTED-MODEL COMPARISONS
# ================================================================

nested_list <- list()

counter <- 1

for (sample_name in names(samples)) {

  for (outcome in outcomes) {

    m1 <- models[[sample_name]][[outcome]][["M1_region"]]

    m2 <- models[[sample_name]][[outcome]][["M2_technology"]]

    m3 <- models[[sample_name]][[outcome]][["M3_combined"]]

    technology_test <- extract_lr(
      m3,
      m1
    )

    region_test <- extract_lr(
      m3,
      m2
    )

    nested_list[[counter]] <- tibble::tibble(

      sample = sample_name,
      outcome = outcome,
      comparison = "Technology added to region",
      n = nrow(samples[[sample_name]]),
      lr_chisq = technology_test$lr,
      df = technology_test$df,
      p_raw = technology_test$p
    )

    counter <- counter + 1

    nested_list[[counter]] <- tibble::tibble(

      sample = sample_name,
      outcome = outcome,
      comparison = "Region added to technology",
      n = nrow(samples[[sample_name]]),
      lr_chisq = region_test$lr,
      df = region_test$df,
      p_raw = region_test$p
    )

    counter <- counter + 1
  }
}

nested_comparisons <- dplyr::bind_rows(
  nested_list
) |>

  dplyr::group_by(
    sample,
    comparison
  ) |>

  dplyr::mutate(

    p_holm = stats::p.adjust(
      p_raw,
      method = "holm"
    )
  ) |>

  dplyr::ungroup()

if (
  nrow(nested_comparisons) != 20 ||
  any(
    nested_comparisons$df[
      nested_comparisons$comparison ==
        "Technology added to region"
    ] != 6
  ) ||
  any(
    nested_comparisons$df[
      nested_comparisons$comparison ==
        "Region added to technology"
    ] != 3
  )
) {

  stop(
    "Nested-model comparison audit failed."
  )
}

# ================================================================
# 19. MODEL FIT STATISTICS
# ================================================================

model_fit_list <- list()

counter <- 1

for (sample_name in names(samples)) {

  for (outcome in outcomes) {

    for (spec_name in names(model_specs)) {

      model <- models[[sample_name]][[outcome]][[spec_name]]

      model_fit_list[[counter]] <- tibble::tibble(

        sample = sample_name,
        outcome = outcome,
        specification = spec_name,
        n = nrow(samples[[sample_name]]),

        log_likelihood = as.numeric(
          stats::logLik(model)
        ),

        parameters = as.integer(
          attr(
            stats::logLik(model),
            "df"
          )
        ),

        AIC = stats::AIC(model),

        BIC = stats::BIC(model),

        convergence = as.integer(
          as.character(model$convergence)[1]
        )
      )

      counter <- counter + 1
    }
  }
}

model_fit <- dplyr::bind_rows(
  model_fit_list
)

# ================================================================
# 20. SUPPLEMENTARY TABLE S14
# FULLY ADJUSTED ORDINAL LOGISTIC REGRESSION COEFFICIENTS
# BOTH ANALYTICAL SAMPLES
# ================================================================

outcome_labels <- c(

  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  licensing = "Licensing",
  siting = "Siting"
)

sample_labels <- c(

  all_designs = "All designs",
  conventional = "Conventional SMRs"
)

table_s14 <- coefficients |>

  dplyr::transmute(

    Sample = unname(
      sample_labels[sample]
    ),

    `Readiness dimension` = unname(
      outcome_labels[outcome]
    ),

    Predictor = term_label,

    n = n,

    `Adjusted OR` = odds_ratio,

    `95% CI lower` = ci_low,

    `95% CI upper` = ci_high,

    `Wald p` = p_wald,

    `BH-adjusted p` = p_bh_within_outcome,

    `Holm-adjusted p` = p_holm_within_outcome,

    `Model status` = "OK"
  )

stopifnot(

  nrow(table_s14) == 90,

  sum(
    table_s14$Sample == "All designs"
  ) == 45,

  sum(
    table_s14$Sample == "Conventional SMRs"
  ) == 45,

  all(
    table_s14$n[
      table_s14$Sample == "All designs"
    ] == 85
  ),

  all(
    table_s14$n[
      table_s14$Sample == "Conventional SMRs"
    ] == 51
  )
)

# ================================================================
# 21. SUPPLEMENTARY TABLE S15
# NESTED-MODEL LIKELIHOOD-RATIO COMPARISONS
# ================================================================

table_s15 <- nested_comparisons |>

  dplyr::transmute(

    Sample = unname(
      sample_labels[sample]
    ),

    `Readiness dimension` = unname(
      outcome_labels[outcome]
    ),

    Comparison = comparison,

    n = n,

    `LR chi-square` = lr_chisq,

    df = df,

    `Raw p` = p_raw,

    `Holm-adjusted p` = p_holm
  )

stopifnot(

  nrow(table_s15) == 20,

  sum(
    table_s15$Sample == "All designs"
  ) == 10,

  sum(
    table_s15$Sample == "Conventional SMRs"
  ) == 10
)

# ================================================================
# 22. CONSOLIDATED EXCEL WORKBOOK
# ================================================================

excel_tabs <- list(

  "table_S14" = table_s14,

  "table_S15" = table_s15,

  "Adjusted odds ratios" = coefficients,

  "Nested LR comparisons" = nested_comparisons,

  "Omnibus tests" = omnibus,

  "Model fit statistics" = model_fit,

  "Model audit" = model_audit,

  "Coefficient audit" = coefficient_audit,

  "Sample audit" = sample_audit,

  "Region audit" = region_audit,

  "Excluded region" = excluded_region,

  "Excluded enrichment" = excluded_enrichment,

  "Fuel distribution" = fuel_distribution,

  "Fuel by region" = fuel_by_region,

  "Fuel by cooling" = fuel_by_cooling,

  "Fuel by spectrum" = fuel_by_spectrum,

  "Fuel by fuel" = fuel_by_fuel,

  "Fuel by enrichment" = fuel_by_enrichment,

  "Classification audit" = data,

  "Complete case all" = all_designs,

  "Complete case SMRs" = conventional
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(

  fgFill = "#17324D",

  fontColour = "#FFFFFF",

  textDecoration = "bold",

  border = "Bottom",

  borderColour = "#B7C9D6"
)

for (sheet_name in names(excel_tabs)) {

  df <- as.data.frame(
    excel_tabs[[sheet_name]]
  )

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  openxlsx::writeData(

    wb,

    sheet_name,

    df,

    headerStyle = header_style
  )

  openxlsx::freezePane(

    wb,

    sheet_name,

    firstRow = TRUE
  )

  if (
    nrow(df) > 0 &&
    ncol(df) > 0
  ) {

    openxlsx::addFilter(

      wb,

      sheet_name,

      rows = 1,

      cols = seq_len(
        ncol(df)
      )
    )
  }

  openxlsx::setColWidths(

    wb,

    sheet_name,

    cols = seq_len(
      ncol(df)
    ),

    widths = "auto"
  )
}

openxlsx::saveWorkbook(

  wb,

  file = excel_file,

  overwrite = TRUE
)

# ================================================================
# 23. FINAL VERIFICATION
# ================================================================

if (
  !file.exists(excel_file) ||
  is.na(file.info(excel_file)$size) ||
  file.info(excel_file)$size == 0
) {

  stop(
    "Missing or empty output workbook."
  )
}

stopifnot(

  nrow(model_audit) == 40,

  all(
    model_audit$status == "Converged"
  ),

  nrow(model_fit) == 40,

  nrow(coefficients) == 90,

  nrow(table_s14) == 90,

  nrow(table_s15) == 20,

  nrow(omnibus) == 10
)

cat("\n========================================\n")
cat("SUPPLEMENTARY TABLES S14 AND S15 COMPLETE\n")
cat("========================================\n")

cat("\nSAMPLE SIZES:\n")

print(sample_audit)

cat("\nREGIONAL DISTRIBUTION:\n")

print(region_audit)

cat("\nEXCLUDED ENRICHMENT:\n")

print(
  excluded_enrichment,
  n = Inf,
  width = Inf
)

cat("\nMODEL STATUS:\n")

print(
  model_audit,
  n = Inf
)

cat("\nCOEFFICIENT COUNTS:\n")

print(
  coefficient_audit,
  n = Inf
)

cat("\nSUPPLEMENTARY TABLE S14:\n")

print(
  table_s14,
  n = Inf
)

cat("\nSUPPLEMENTARY TABLE S15:\n")

print(
  table_s15,
  n = Inf
)

cat("\nOMNIBUS TESTS:\n")

print(
  omnibus,
  n = Inf
)

cat("\nOUTPUT WORKBOOK:\n")

cat(
  excel_file,
  "\n"
)

cat("\nALL FIVE READINESS DIMENSIONS INCLUDED.\n")
cat("ALL 40 MODELS PASSED VALIDATION.\n")
cat("ALL 90 COEFFICIENTS EXPORTED TO TABLE S14.\n")
cat("ALL 20 NESTED COMPARISONS EXPORTED TO TABLE S15.\n")
cat("========================================\n")
