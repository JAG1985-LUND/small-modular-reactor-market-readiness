
# ================================================================
# SUPPLEMENTARY TABLE S18
# EXTERNAL IAEA DEVELOPMENT-PHASE AND TRL CROSS-CHECK
# Energy Strategy Reviews
#
# Source: IAEA development phases and Kang et al. TRL
#
# Script:
# revision/scripts/supp_table_S18_iaea_trl_maturity_crosscheck.R
#
# Input:
# revision/data/iaea_trl_crosscheck_source_data.xlsx
#
# Output:
# revision/figures/supp_table_S18_iaea_trl_maturity_crosscheck_results.xlsx
#
# Analyses:
# 1. Detailed design+ versus conceptual/basic
# 2. Five-stage ordinal development phase
# 3. Ordinal technology readiness level (TRL)
# 4. TRL 3+ versus TRL 1–2
#
# Predictors (univariate):
# - Conceptual design start year
# - Water-cooled status
# - Neutron spectrum
# - Uranium oxide fuel
# - Uranium enrichment
#
# Additional verification:
# - Centred-year ordinal phase and TRL regressions
#
# One Excel workbook; no figure.
# ================================================================


# 1. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "MASS",
  "writexl",
  "tibble"
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
  library(writexl)
  library(tibble)
})


# 2. REPOSITORY-RELATIVE PATHS -----------------------------------

source_file <- file.path(
  "revision",
  "data",
  "iaea_trl_crosscheck_source_data.xlsx"
)

find_repo_root <- function(
    start_dir = getwd(),
    target_file = source_file
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
        "Repository root not found.\n",
        "Expected input: ", target_file
      )
    }
    
    current <- parent
  }
}

root_dir <- find_repo_root()

input_file <- file.path(
  root_dir,
  source_file
)

output_dir <- file.path(
  root_dir,
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
  "supp_table_S18_iaea_trl_maturity_crosscheck_results.xlsx"
)

if (!file.exists(input_file)) {
  stop("Input workbook not found: ", input_file)
}


# 3. IMPORT DATA -------------------------------------------------

sheet_name <- "iaea_trl_crosscheck_data"

available_sheets <- readxl::excel_sheets(input_file)

if (!sheet_name %in% available_sheets) {
  stop(
    "Required worksheet not found: ",
    sheet_name
  )
}

# Only columns A:N are used.
# The duplicate DESIGN column O is excluded.

df <- readxl::read_excel(
  input_file,
  sheet = sheet_name,
  range = readxl::cell_cols("A:N"),
  .name_repair = "unique"
)

names(df) <- tolower(trimws(names(df)))
names(df) <- gsub("[^a-z0-9]+", "_", names(df))
names(df) <- gsub("^_+|_+$", "", names(df))

required_columns <- c(
  "design",
  "phase",
  "trl",
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_binary",
  "uranium_enrichment"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- df %>%
  filter(
    !is.na(design),
    trimws(as.character(design)) != ""
  )

df$design <- trimws(as.character(df$design))

if (anyDuplicated(df$design)) {
  stop("Duplicate reactor designs detected.")
}

if (nrow(df) != 65) {
  warning(
    "Expected 65 designs; found ",
    nrow(df)
  )
}


# 4. HELPER FUNCTIONS --------------------------------------------

clean_text <- function(x) {
  
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[[:space:]-]+", "_", x)
  
  x[x %in% c(
    "", "na", "nan", "n_a"
  )] <- NA_character_
  
  x
}

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

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    NA_character_,
    sprintf(
      paste0("%.", digits, "f"),
      x
    )
  )
}

safe_exp <- function(x) {
  exp(pmax(pmin(x, 700), -700))
}

excel_safe <- function(x) {
  
  x <- as.data.frame(x)
  
  x[] <- lapply(x, function(z) {
    if (is.factor(z)) as.character(z) else z
  })
  
  x
}


# 5. TECHNOLOGY READINESS LEVEL ----------------------------------

df$trl_num <- suppressWarnings(
  as.numeric(as.character(df$trl))
)

invalid_trl <- !is.na(df$trl_num) &
  !(df$trl_num %in% 1:9)

if (any(invalid_trl)) {
  stop(
    "Invalid TRL values: ",
    paste(
      df$design[invalid_trl],
      collapse = ", "
    )
  )
}

trl_levels <- sort(unique(
  df$trl_num[!is.na(df$trl_num)]
))

df$trl_ord <- ordered(
  df$trl_num,
  levels = trl_levels
)

df$trl_binary <- factor(
  case_when(
    df$trl_num >= 3 ~ "TRL 3+",
    df$trl_num %in% c(1, 2) ~ "TRL 1-2",
    TRUE ~ NA_character_
  ),
  levels = c("TRL 1-2", "TRL 3+")
)


# 6. CONCEPTUAL DESIGN START YEAR --------------------------------

df$cd_start_num <- suppressWarnings(
  as.numeric(as.character(df$cd_start))
)

invalid_year <- !is.na(df$cd_start_num) &
  (
    df$cd_start_num < 1950 |
      df$cd_start_num > 2026
  )

if (any(invalid_year)) {
  stop("Invalid conceptual design start years.")
}

# Centring does not change the per-year odds ratio.
# It improves numerical stability for ordinal models.

df$cd_start_centred <- df$cd_start_num - 2000


# 7. WATER-COOLED STATUS -----------------------------------------

water_raw <- clean_text(df$water_cooled)

df$water_bin <- factor(
  case_when(
    water_raw %in% c(
      "true", "t", "yes", "y", "1",
      "water", "water_cooled"
    ) ~ "Water-cooled",
    
    water_raw %in% c(
      "false", "f", "no", "n", "0",
      "non_water", "non_water_cooled"
    ) ~ "Non-water-cooled",
    
    TRUE ~ NA_character_
  ),
  levels = c(
    "Non-water-cooled",
    "Water-cooled"
  )
)


# 8. NEUTRON SPECTRUM --------------------------------------------

spectrum_raw <- clean_text(df$spectrum)

df$spectrum_bin <- factor(
  case_when(
    spectrum_raw == "fast" ~ "Fast",
    spectrum_raw == "thermal" ~ "Thermal",
    TRUE ~ NA_character_
  ),
  levels = c("Fast", "Thermal")
)


# 9. URANIUM OXIDE FUEL ------------------------------------------

fuel_raw <- clean_text(df$fuel_binary)

df$fuel_bin <- factor(
  case_when(
    fuel_raw %in% c(
      "uranium_oxide",
      "uranium_oxide_ceramic"
    ) ~ "Uranium oxide",
    
    fuel_raw == "other" ~ "Other",
    
    TRUE ~ NA_character_
  ),
  levels = c("Other", "Uranium oxide")
)


# 10. URANIUM ENRICHMENT -----------------------------------------

# Preserve the original source coding:
# Low, Medium and High.
#
# HALEU is grouped with High in this historical
# cross-check. This must not be confused with the
# LEU/HALEU coding in the NEA readiness regressions.

enrichment_raw <- clean_text(
  df$uranium_enrichment
)

df$enrichment_bin <- factor(
  case_when(
    enrichment_raw %in% c(
      "low", "leu"
    ) ~ "Low",
    
    enrichment_raw %in% c(
      "medium", "meu"
    ) ~ "Medium",
    
    enrichment_raw %in% c(
      "high", "heu", "haleu"
    ) ~ "High",
    
    TRUE ~ NA_character_
  ),
  levels = c("High", "Medium", "Low")
)


# 11. IAEA DEVELOPMENT PHASE -------------------------------------

phase_raw <- clean_text(df$phase)

df$phase_clean <- case_when(
  phase_raw %in% c(
    "conceptual", "conceptual_design"
  ) ~ "Conceptual design",
  
  phase_raw %in% c(
    "basic", "basic_design"
  ) ~ "Basic design",
  
  phase_raw %in% c(
    "detailed", "detailed_design"
  ) ~ "Detailed design",
  
  phase_raw %in% c(
    "licensing", "licensed"
  ) ~ "Licensing",
  
  phase_raw %in% c(
    "construction", "under_construction"
  ) ~ "Construction",
  
  phase_raw %in% c(
    "operational", "operating", "operation"
  ) ~ "Operational",
  
  TRUE ~ NA_character_
)

# Five-stage ordinal outcome.
# Operational designs are excluded.

df$phase_5 <- ordered(
  df$phase_clean,
  levels = c(
    "Conceptual design",
    "Basic design",
    "Detailed design",
    "Licensing",
    "Construction"
  )
)

# Binary phase outcome.
# Operational designs are included in the advanced group.

df$phase_binary <- factor(
  case_when(
    df$phase_clean %in% c(
      "Conceptual design",
      "Basic design"
    ) ~ "Conceptual/basic",
    
    df$phase_clean %in% c(
      "Detailed design",
      "Licensing",
      "Construction",
      "Operational"
    ) ~ "Detailed and beyond",
    
    TRUE ~ NA_character_
  ),
  levels = c(
    "Conceptual/basic",
    "Detailed and beyond"
  )
)


# 12. VALIDATION AND DISTRIBUTIONS -------------------------------

validation <- tibble(
  variable = c(
    "Total designs",
    "Unique designs",
    "TRL available",
    "Conceptual start year available",
    "Water cooling available",
    "Spectrum available",
    "Fuel type available",
    "Enrichment available",
    "Development phase available",
    "Five-phase outcome available",
    "Binary phase outcome available"
  ),
  n = c(
    nrow(df),
    length(unique(df$design)),
    sum(!is.na(df$trl_num)),
    sum(!is.na(df$cd_start_num)),
    sum(!is.na(df$water_bin)),
    sum(!is.na(df$spectrum_bin)),
    sum(!is.na(df$fuel_bin)),
    sum(!is.na(df$enrichment_bin)),
    sum(!is.na(df$phase_clean)),
    sum(!is.na(df$phase_5)),
    sum(!is.na(df$phase_binary))
  )
)

trl_counts <- df %>%
  count(trl_num, name = "n") %>%
  arrange(trl_num)

trl_binary_counts <- df %>%
  count(trl_binary, name = "n")

phase_counts <- df %>%
  count(phase_clean, name = "n")

phase_binary_counts <- df %>%
  count(phase_binary, name = "n")

coding_audit <- df %>%
  select(
    design,
    phase,
    phase_clean,
    phase_5,
    phase_binary,
    trl,
    trl_num,
    trl_binary,
    cd_start,
    cd_start_centred,
    water_cooled,
    water_bin,
    spectrum,
    spectrum_bin,
    fuel_binary,
    fuel_bin,
    uranium_enrichment,
    enrichment_bin
  )


# 13. REGRESSION FUNCTION ----------------------------------------

run_model <- function(
    data,
    outcome,
    predictor,
    outcome_label,
    predictor_label,
    model_type
) {
  
  dat <- data.frame(
    outcome = data[[outcome]],
    predictor = data[[predictor]]
  )
  
  dat <- dat[
    complete.cases(dat),
    ,
    drop = FALSE
  ]
  
  dat <- droplevels(dat)
  
  n_used <- nrow(dat)
  
  empty_result <- data.frame(
    outcome = outcome_label,
    predictor = predictor_label,
    model = model_type,
    n = n_used,
    term = NA_character_,
    estimate = NA_real_,
    se = NA_real_,
    or = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    wald_p = NA_real_,
    lr = NA_real_,
    lr_df = NA_real_,
    lr_p = NA_real_,
    mcfadden_r2 = NA_real_,
    status = "Not estimated",
    stringsAsFactors = FALSE
  )
  
  if (
    n_used < 10 ||
    length(unique(dat$outcome)) < 2 ||
    length(unique(dat$predictor)) < 2
  ) {
    empty_result$status <-
      "Insufficient data or variation"
    
    return(empty_result)
  }
  
  if (model_type == "binary") {
    dat$outcome <- as.integer(dat$outcome) - 1L
  }
  
  warnings_found <- character(0)
  
  fit_model <- function(formula) {
    
    tryCatch(
      withCallingHandlers(
        {
          if (model_type == "ordinal") {
            
            MASS::polr(
              formula,
              data = dat,
              method = "logistic",
              Hess = TRUE,
              control = list(maxit = 1000)
            )
            
          } else {
            
            stats::glm(
              formula,
              data = dat,
              family = stats::binomial(),
              control = stats::glm.control(
                maxit = 100
              )
            )
          }
        },
        warning = function(w) {
          warnings_found <<- c(
            warnings_found,
            conditionMessage(w)
          )
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        warnings_found <<- c(
          warnings_found,
          conditionMessage(e)
        )
        NULL
      }
    )
  }
  
  fit <- fit_model(
    outcome ~ predictor
  )
  
  fit_null <- fit_model(
    outcome ~ 1
  )
  
  if (is.null(fit) || is.null(fit_null)) {
    empty_result$status <- paste(
      "Estimation failed:",
      paste(
        unique(warnings_found),
        collapse = "; "
      )
    )
    return(empty_result)
  }
  
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(fit_null))
  
  df_full <- attr(logLik(fit), "df")
  df_null <- attr(logLik(fit_null), "df")
  
  lr <- 2 * (ll_full - ll_null)
  lr_df <- df_full - df_null
  
  lr_p <- if (
    is.finite(lr) && lr_df > 0
  ) {
    pchisq(
      max(lr, 0),
      df = lr_df,
      lower.tail = FALSE
    )
  } else {
    NA_real_
  }
  
  mcfadden <- if (
    is.finite(ll_null) && ll_null != 0
  ) {
    1 - ll_full / ll_null
  } else {
    NA_real_
  }
  
  cm <- tryCatch(
    coef(summary(fit)),
    error = function(e) NULL
  )
  
  if (is.null(cm)) {
    empty_result$status <-
      "Coefficient extraction failed"
    return(empty_result)
  }
  
  if (model_type == "ordinal") {
    
    # Exclude threshold coefficients.
    
    cm <- cm[
      !grepl("\\|", rownames(cm)),
      ,
      drop = FALSE
    ]
    
    estimates <- as.numeric(cm[, "Value"])
    se <- as.numeric(cm[, "Std. Error"])
    
    wald_p <- 2 * pnorm(
      abs(estimates / se),
      lower.tail = FALSE
    )
    
  } else {
    
    cm <- cm[
      rownames(cm) != "(Intercept)",
      ,
      drop = FALSE
    ]
    
    estimates <- as.numeric(cm[, "Estimate"])
    se <- as.numeric(cm[, "Std. Error"])
    
    wald_p <- as.numeric(
      cm[, "Pr(>|z|)"]
    )
  }
  
  if (length(estimates) == 0) {
    empty_result$status <-
      "No predictor coefficients"
    return(empty_result)
  }
  
  coefficient_terms <- rownames(cm)
  
  status <- "OK"
  
  if (model_type == "binary") {
    
    if (
      !isTRUE(fit$converged) ||
      !isTRUE(fit_null$converged)
    ) {
      status <- "Convergence warning"
    }
    
  } else {
    
    if (
      !is.null(fit$convergence) &&
      fit$convergence != 0
    ) {
      status <- "Convergence warning"
    }
  }
  
  if (length(warnings_found) > 0) {
    status <- paste(
      "Review warnings:",
      paste(
        unique(warnings_found),
        collapse = "; "
      )
    )
  }
  
  if (
    any(!is.finite(estimates)) ||
    any(!is.finite(se)) ||
    any(se <= 0)
  ) {
    status <- "Unstable coefficients"
  }
  
  data.frame(
    outcome = outcome_label,
    predictor = predictor_label,
    model = model_type,
    n = n_used,
    term = coefficient_terms,
    estimate = estimates,
    se = se,
    or = safe_exp(estimates),
    ci_low = safe_exp(
      estimates - 1.96 * se
    ),
    ci_high = safe_exp(
      estimates + 1.96 * se
    ),
    wald_p = wald_p,
    lr = lr,
    lr_df = lr_df,
    lr_p = lr_p,
    mcfadden_r2 = mcfadden,
    status = status,
    stringsAsFactors = FALSE
  )
}


# 14. MODEL SPECIFICATIONS ---------------------------------------

predictors <- list(
  c(
    "cd_start_centred",
    "Conceptual design start year"
  ),
  c(
    "water_bin",
    "Water-cooled"
  ),
  c(
    "spectrum_bin",
    "Thermal spectrum"
  ),
  c(
    "fuel_bin",
    "Uranium oxide"
  ),
  c(
    "enrichment_bin",
    "Uranium enrichment"
  )
)

outcomes <- list(
  c(
    "phase_binary",
    "Detailed design+ vs conceptual/basic",
    "binary"
  ),
  c(
    "phase_5",
    "Five-phase development status",
    "ordinal"
  ),
  c(
    "trl_ord",
    "Ordinal TRL",
    "ordinal"
  ),
  c(
    "trl_binary",
    "TRL 3+ vs TRL 1-2",
    "binary"
  )
)


# 15. RUN ALL 20 MODELS ------------------------------------------

results_list <- list()
counter <- 1L

for (o in outcomes) {
  
  for (p in predictors) {
    
    cat(
      "\nRunning:",
      o[2],
      "|",
      p[2],
      "\n"
    )
    
    results_list[[counter]] <- run_model(
      data = df,
      outcome = o[1],
      predictor = p[1],
      outcome_label = o[2],
      predictor_label = p[2],
      model_type = o[3]
    )
    
    counter <- counter + 1L
  }
}

all_results <- dplyr::bind_rows(
  results_list
)


# 16. MODEL-LEVEL LIKELIHOOD-RATIO TESTS ------------------------

model_tests <- all_results %>%
  group_by(outcome, predictor) %>%
  summarise(
    model = first(model),
    n = first(n),
    lr = first(lr),
    lr_df = first(lr_df),
    lr_p = first(lr_p),
    mcfadden_r2 = first(mcfadden_r2),
    status = first(status),
    .groups = "drop"
  )


# 17. HOLM CORRECTION --------------------------------------------

# Five model-level tests per outcome.
# Coefficient-level Wald p-values are unadjusted.

model_tests <- model_tests %>%
  group_by(outcome) %>%
  mutate(
    holm_p = p.adjust(
      lr_p,
      method = "holm",
      n = 5
    )
  ) %>%
  ungroup()

all_results <- all_results %>%
  left_join(
    model_tests %>%
      select(
        outcome,
        predictor,
        holm_p
      ),
    by = c("outcome", "predictor")
  )


# 18. COMPLETE SUPPLEMENTARY TABLE -------------------------------

table_full <- all_results %>%
  group_by(outcome, predictor) %>%
  summarise(
    n = first(n),
    
    or_ci = {
      valid <- !is.na(or)
      
      if (
        length(or) == 1 &&
        all(valid)
      ) {
        paste0(
          format_num(or[1]),
          " (",
          format_num(ci_low[1]),
          "–",
          format_num(ci_high[1]),
          ")"
        )
      } else {
        "—"
      }
    },
    
    lr = first(lr),
    lr_df = first(lr_df),
    lr_p = first(lr_p),
    holm_p = first(holm_p),
    mcfadden_r2 = first(mcfadden_r2),
    status = first(status),
    .groups = "drop"
  ) %>%
  mutate(
    lr_p_formatted = format_p(lr_p),
    holm_p_formatted = format_p(holm_p),
    mcfadden_r2_formatted =
      format_num(mcfadden_r2)
  )


# 19. ORIGINAL TEN-COMPARISON TABLE ------------------------------

# Preserve the historical Supplementary Table S6
# selection, without changing its comparisons.

table_original <- table_full %>%
  filter(
    outcome ==
      "Detailed design+ vs conceptual/basic" |
      
      (
        outcome ==
          "Five-phase development status" &
          predictor %in% c(
            "Water-cooled",
            "Thermal spectrum",
            "Uranium oxide"
          )
      ) |
      
      (
        outcome == "Ordinal TRL" &
          predictor == "Water-cooled"
      ) |
      
      (
        outcome == "TRL 3+ vs TRL 1-2" &
          predictor == "Water-cooled"
      )
  )

if (nrow(table_original) != 10) {
  stop(
    "Original table should contain 10 comparisons."
  )
}


# 20. CENTRED-YEAR VERIFICATION ----------------------------------

# These models are already included among the
# 20 regressions. Here they are extracted and
# compared with the previously reported LR values.

previous_lr <- data.frame(
  outcome = c(
    "Five-phase development status",
    "Ordinal TRL"
  ),
  previous_lr = c(
    30.51,
    28.77
  )
)

year_verification <- all_results %>%
  filter(
    predictor == "Conceptual design start year",
    outcome %in% previous_lr$outcome
  ) %>%
  left_join(
    previous_lr,
    by = "outcome"
  ) %>%
  mutate(
    lr_difference = lr - previous_lr,
    
    or_95ci = paste0(
      format_num(or),
      " (",
      format_num(ci_low),
      "–",
      format_num(ci_high),
      ")"
    ),
    
    lr_p_formatted = format_p(lr_p)
  ) %>%
  select(
    outcome,
    n,
    term,
    estimate,
    se,
    or,
    ci_low,
    ci_high,
    or_95ci,
    lr,
    lr_p,
    lr_p_formatted,
    previous_lr,
    lr_difference,
    status
  )

if (nrow(year_verification) != 2) {
  stop(
    "Expected two centred-year verification models."
  )
}


# 21. DIAGNOSTICS -----------------------------------------------

diagnostics <- model_tests %>%
  select(
    outcome,
    predictor,
    n,
    lr,
    lr_df,
    lr_p,
    holm_p,
    mcfadden_r2,
    status
  )

problem_models <- diagnostics %>%
  filter(status != "OK")

sample_sizes <- model_tests %>%
  select(
    outcome,
    predictor,
    n
  ) %>%
  arrange(outcome, predictor)


# 22. WORKBOOK DOCUMENTATION -------------------------------------

readme <- tibble(
  item = c(
    "Study",
    "Analysis",
    "Input workbook",
    "Input worksheet",
    "Number of designs",
    "Source columns",
    "TRL source",
    "Binary phase outcome",
    "Five-stage ordinal phase",
    "Ordinal TRL",
    "Binary TRL",
    "Predictors",
    "Start-year specification",
    "Enrichment coding",
    "Statistical models",
    "Likelihood-ratio tests",
    "Multiplicity correction",
    "Original table",
    "Centred-year verification",
    "Interpretation"
  ),
  
  description = c(
    "Energy Strategy Reviews",
    
    "External IAEA development-phase and TRL cross-check",
    
    basename(input_file),
    
    sheet_name,
    
    as.character(nrow(df)),
    
    "A:N; duplicate DESIGN column O excluded",
    
    "Kang et al. (2026), harmonised design names",
    
    "Detailed design, licensing, construction or operational versus conceptual/basic",
    
    "Conceptual through construction; operational designs excluded",
    
    "Ordered TRL categories observed in the source data",
    
    "TRL 3+ versus TRL 1–2",
    
    "Conceptual start year, water cooling, neutron spectrum, uranium oxide fuel, enrichment",
    
    "Start year centred at 2000; odds ratios per year",
    
    "Historical Low/Medium/High coding; HALEU grouped with High",
    
    "Univariate binary logistic and proportional-odds ordinal logistic regressions",
    
    "Full model versus intercept-only model",
    
    "Holm adjustment across five predictors within each outcome",
    
    "Original ten comparisons retained in a separate worksheet",
    
    "Two centred-year ordinal models compared with previously reported LR statistics",
    
    "Exploratory associations; IAEA phase and TRL are complementary maturity indicators, not equivalent to NEA market readiness"
  )
)


# 23. EXPORT SINGLE EXCEL WORKBOOK -------------------------------

sheets <- list(
  README = readme,
  validation = validation,
  cleaned_data = df,
  coding_audit = coding_audit,
  TRL_counts = trl_counts,
  TRL_binary_counts = trl_binary_counts,
  phase_counts = phase_counts,
  phase_binary_counts = phase_binary_counts,
  Table_S18 = table_original,
  Table_S18_full = table_full,
  model_tests = model_tests,
  coefficients = all_results,
  year_verification = year_verification,
  sample_sizes = sample_sizes,
  diagnostics = diagnostics,
  problem_models = problem_models
)

sheets <- lapply(
  sheets,
  excel_safe
)

writexl::write_xlsx(
  sheets,
  path = output_file
)


# 24. FINAL CONSOLE OUTPUT ---------------------------------------

cat("\n")
cat("================================================\n")
cat("SUPPLEMENTARY TABLE S18\n")
cat("IAEA / TRL MATURITY CROSS-CHECK\n")
cat("================================================\n")

cat(
  "\nDesigns imported: ",
  nrow(df),
  "\n",
  sep = ""
)

cat("\nTRL distribution:\n")
print(trl_counts, row.names = FALSE)

cat("\nOriginal ten-comparison table:\n")
print(table_original, row.names = FALSE)

cat("\nCentred-year verification:\n")
print(year_verification, row.names = FALSE)

cat("\nModels requiring review:\n")
print(problem_models, row.names = FALSE)

stopifnot(file.exists(output_file))

cat(
  "\nWorkbook saved to:\n",
  normalizePath(
    output_file,
    winslash = "/"
  ),
  "\n"
)

cat("\nAnalysis complete.\n")
