# ================================================================
# SUPPLEMENTARY TABLE S16
# US VERSUS NON-US LICENSING READINESS
# NEA SMR DASHBOARD - EDITION 4
#
# Panel A: Unadjusted and adjusted ordinal logistic regression
# Panel B: Descriptive licensing readiness by geography
# Panel C: Model diagnostics and nested-model LR comparisons
#
# All designs:       expected n = 91 (US 35, non-US 56)
# Conventional SMRs: expected n = 53 (US 16, non-US 37)
#
# Geographic reference: Other (non-US)
#
# Adjusted model controls for the arithmetic mean of:
# financing, supply chain, fuel supply, siting and
# stakeholder engagement.
#
# Both regressions use identical complete cases.
#
# Run from any directory inside the repository,
# or set the SMR_REPO_ROOT environment variable.
#
# INPUT:
# revision/data/us_licensing_regression_check.xlsx
#
# OUTPUT:
# revision/figures/table_S16_us_licensing_robustness_results.xlsx
#
# Updated JAG analytical specification retained.
# GitHub repository paths retained.
# Adapted script not independently executed.
# ================================================================


# 1. PACKAGES -----------------------------------------------------

packages <- c("readxl", "dplyr", "tidyr", "MASS", "openxlsx")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(MASS)
  library(openxlsx)
})


# 2. FILE PATHS ---------------------------------------------------

find_root <- function() {

  override <- Sys.getenv("SMR_REPO_ROOT", unset = "")

  starts <- unique(
    c(
      if (nzchar(override)) override,
      getwd()
    )
  )

  for (start in starts) {

    p <- normalizePath(
      start,
      winslash = "/",
      mustWork = FALSE
    )

    repeat {

      if (
        dir.exists(file.path(p, "revision", "data")) &&
        dir.exists(file.path(p, "revision", "scripts"))
      ) {
        return(p)
      }

      parent <- dirname(p)

      if (identical(parent, p)) {
        break
      }

      p <- parent
    }
  }

  stop(
    "Repository root not found. ",
    "Run within the repository or set SMR_REPO_ROOT."
  )
}

root <- find_root()

input_file <- file.path(
  root,
  "revision",
  "data",
  "us_licensing_regression_check.xlsx"
)

output_dir <- file.path(
  root,
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
  "table_S16_us_licensing_robustness_results.xlsx"
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}


# 3. IMPORT DATA --------------------------------------------------

sheets <- readxl::excel_sheets(input_file)

sheet_name <- if ("nea_50" %in% sheets) {
  "nea_50"
} else {
  sheets[1]
}

dat <- as.data.frame(
  readxl::read_excel(input_file, sheet = sheet_name)
)

names(dat) <- trimws(names(dat))

cat("\nImported worksheet:", sheet_name, "\n")
cat("Imported rows:", nrow(dat), "\n")


# 4. CHECK REQUIRED COLUMNS ---------------------------------------

required_cols <- c(
  "design",
  "country",
  "region",
  "microreactor",
  "licensing",
  "financing",
  "supply_chain",
  "fuel_supply",
  "siting",
  "stakeholder_engagement"
)

missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

normalise <- function(x) {
  tolower(trimws(gsub("\\s+", " ", as.character(x))))
}


# 5. CLEAN DATA ---------------------------------------------------

if ("edition" %in% names(dat)) {

  edition <- suppressWarnings(
    as.numeric(as.character(dat$edition))
  )

  dat <- dat[
    !is.na(edition) & edition == 4,
    ,
    drop = FALSE
  ]
}

design_names <- toupper(trimws(as.character(dat$design)))

dat <- dat[
  !is.na(design_names) &
    nzchar(design_names) &
    design_names != "DF-CDE",
  ,
  drop = FALSE
]

if (anyDuplicated(dat$design)) {
  stop("Duplicate design names detected.")
}

dat$region_clean <- normalise(dat$region)
dat$country_clean <- normalise(dat$country)
dat$micro_clean <- normalise(dat$microreactor)


# 6. US VERSUS OTHER CLASSIFICATION -------------------------------

us_values <- c(
  "us", "usa", "u.s.",
  "united states", "united states of america"
)

other_values <- c(
  "other", "non-us", "non us", "non_us"
)

valid_regions <- c(us_values, other_values)

if (any(
  is.na(dat$region_clean) |
  !(dat$region_clean %in% valid_regions)
)) {
  print(unique(dat$region_clean))
  stop("Unexpected region coding. Expected US or Other.")
}

dat$us_design <- dat$region_clean %in% us_values

# Verify region coding against country.

if (any(
  is.na(dat$country_clean) |
  dat$country_clean %in% c("", "na", "n/a")
)) {
  stop("Missing country classifications.")
}

country_us <- dat$country_clean %in% us_values

if (any(dat$us_design != country_us)) {

  print(
    dat[
      dat$us_design != country_us,
      c("design", "country", "region")
    ]
  )

  stop("Region and country coding disagree.")
}

dat$us_group <- factor(
  ifelse(dat$us_design, "United States", "Non-US"),
  levels = c("Non-US", "United States")
)


# 7. MICROREACTOR CLASSIFICATION ----------------------------------

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
  is.na(dat$micro_clean) |
  !(dat$micro_clean %in% c(micro_true, micro_false))
)) {
  print(unique(dat$micro_clean))
  stop("Unexpected microreactor classification.")
}

dat$is_microreactor <- dat$micro_clean %in% micro_true


# 8. READINESS VARIABLES ------------------------------------------

readiness_vars <- c(
  "licensing",
  "financing",
  "supply_chain",
  "fuel_supply",
  "siting",
  "stakeholder_engagement"
)

for (v in readiness_vars) {

  dat[[v]] <- suppressWarnings(
    as.numeric(as.character(dat[[v]]))
  )

  invalid <- !is.na(dat[[v]]) & !(dat[[v]] %in% 1:6)

  if (any(invalid)) {
    stop("Invalid readiness scores in: ", v)
  }
}


# 9. NON-LICENSING READINESS --------------------------------------

other_five <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "siting",
  "stakeholder_engagement"
)

dat$other_five_complete <- complete.cases(
  dat[, other_five, drop = FALSE]
)

dat$non_licensing_readiness <- NA_real_

complete_rows <- which(dat$other_five_complete)

if (length(complete_rows) > 0) {

  dat$non_licensing_readiness[complete_rows] <- rowMeans(
    dat[complete_rows, other_five, drop = FALSE]
  )
}

dat$model_complete <- complete.cases(
  dat[, c("licensing", "non_licensing_readiness")]
)

dat$sample_inclusion <- ifelse(
  dat$is_microreactor,
  "All designs only",
  "Both samples"
)


# 10. DEFINE AND CHECK SAMPLES ------------------------------------

samples <- list(
  "All designs" = dat,
  "Conventional SMRs" = dat[
    !dat$is_microreactor,
    ,
    drop = FALSE
  ]
)

expected_counts <- data.frame(
  sample = c("All designs", "Conventional SMRs"),
  n_total = c(91L, 53L),
  n_us = c(35L, 16L),
  n_non_us = c(56L, 37L)
)

sample_counts <- do.call(
  rbind,
  lapply(names(samples), function(s) {

    x <- samples[[s]]

    data.frame(
      sample = s,
      n_total = nrow(x),
      n_us = sum(x$us_design),
      n_non_us = sum(!x$us_design),
      n_complete = sum(x$model_complete)
    )
  })
)

rownames(sample_counts) <- NULL

cat("\nSAMPLE COUNTS:\n")
print(sample_counts)

for (i in seq_len(nrow(expected_counts))) {

  observed <- sample_counts[i, ]
  expected <- expected_counts[i, ]

  if (
    observed$n_total != expected$n_total ||
    observed$n_us != expected$n_us ||
    observed$n_non_us != expected$n_non_us
  ) {
    stop(
      "Sample count mismatch for ",
      observed$sample,
      ". Check the input workbook."
    )
  }

  if (observed$n_complete != observed$n_total) {
    stop(
      "Missing readiness data in ",
      observed$sample,
      ". The original S16 models require complete cases."
    )
  }
}


# 11. MODEL-FITTING FUNCTION --------------------------------------

fit_sample <- function(df, sample_name) {

  tmp <- df[df$model_complete, , drop = FALSE]

  tmp$licensing_ordinal <- ordered(
    tmp$licensing,
    levels = 1:6
  )

  if (length(unique(tmp$us_group)) != 2) {
    stop("Both US and non-US groups are required.")
  }

  if (length(unique(tmp$licensing)) < 2) {
    stop("Insufficient licensing variation.")
  }

  model_formulas <- list(
    Unadjusted = licensing_ordinal ~ us_group,
    Adjusted = licensing_ordinal ~
      us_group + non_licensing_readiness
  )

  # Fit the intercept-only model once.

  null_fit <- MASS::polr(
    licensing_ordinal ~ 1,
    data = tmp,
    method = "logistic",
    Hess = TRUE,
    control = list(maxit = 2000)
  )

  fitted_models <- list()
  warning_records <- list()

  for (model_name in names(model_formulas)) {

    current_warnings <- character(0)

    fitted_models[[model_name]] <- withCallingHandlers(
      MASS::polr(
        model_formulas[[model_name]],
        data = tmp,
        method = "logistic",
        Hess = TRUE,
        control = list(maxit = 2000)
      ),
      warning = function(w) {
        current_warnings <<- c(
          current_warnings,
          conditionMessage(w)
        )
        invokeRestart("muffleWarning")
      }
    )

    warning_records[[model_name]] <- paste(
      unique(current_warnings),
      collapse = " | "
    )
  }

  # Extract all coefficients and model-level statistics.

  estimate_list <- list()
  diagnostic_list <- list()

  for (model_name in names(fitted_models)) {

    fit <- fitted_models[[model_name]]

    coefficient_table <- coef(summary(fit))
    predictor_terms <- names(coef(fit))

    beta <- as.numeric(coef(fit)[predictor_terms])

    se <- as.numeric(
      coefficient_table[predictor_terms, "Std. Error"]
    )

    z <- beta / se

    lr <- max(
      0,
      2 * (
        as.numeric(logLik(fit)) -
          as.numeric(logLik(null_fit))
      )
    )

    lr_df <- as.numeric(
      attr(logLik(fit), "df") -
        attr(logLik(null_fit), "df")
    )

    mcfadden_r2 <- 1 -
      as.numeric(logLik(fit)) /
      as.numeric(logLik(null_fit))

    estimate_list[[model_name]] <- data.frame(
      sample = sample_name,
      model = model_name,
      predictor = predictor_terms,
      n_total = nrow(tmp),
      n_us = sum(tmp$us_design),
      n_non_us = sum(!tmp$us_design),
      coefficient = beta,
      standard_error = se,
      odds_ratio = exp(beta),
      ci_low_95 = exp(beta - 1.96 * se),
      ci_high_95 = exp(beta + 1.96 * se),
      p_wald = 2 * stats::pnorm(
        abs(z),
        lower.tail = FALSE
      ),
      lr_statistic = lr,
      lr_df = lr_df,
      p_lr = stats::pchisq(
        lr,
        df = lr_df,
        lower.tail = FALSE
      ),
      mcfadden_r2 = mcfadden_r2,
      row.names = NULL
    )

    diagnostic_list[[model_name]] <- data.frame(
      sample = sample_name,
      model = model_name,
      n = nrow(tmp),
      convergence_code = fit$convergence,
      max_abs_coefficient = max(abs(coef(fit))),
      max_standard_error = max(se),
      warnings = warning_records[[model_name]]
    )
  }

  # Compare adjusted and unadjusted models directly.

  fit_unadjusted <- fitted_models[["Unadjusted"]]
  fit_adjusted <- fitted_models[["Adjusted"]]

  lr_change <- max(
    0,
    2 * (
      as.numeric(logLik(fit_adjusted)) -
        as.numeric(logLik(fit_unadjusted))
    )
  )

  comparison <- data.frame(
    sample = sample_name,
    n = nrow(tmp),
    lr_change = lr_change,
    df = 1L,
    p_change = stats::pchisq(
      lr_change,
      df = 1,
      lower.tail = FALSE
    )
  )

  list(
    estimates = dplyr::bind_rows(estimate_list),
    diagnostics = dplyr::bind_rows(diagnostic_list),
    comparison = comparison
  )
}


# 12. RUN BOTH SAMPLES --------------------------------------------

model_outputs <- list()

for (s in names(samples)) {

  cat("\nRunning models:", s, "\n")

  model_outputs[[s]] <- fit_sample(
    samples[[s]],
    s
  )
}

regression_results <- dplyr::bind_rows(
  lapply(model_outputs, function(x) x$estimates)
)

model_diagnostics <- dplyr::bind_rows(
  lapply(model_outputs, function(x) x$diagnostics)
)

model_comparison <- dplyr::bind_rows(
  lapply(model_outputs, function(x) x$comparison)
)


# 13. PANEL A: US EFFECTS -----------------------------------------

us_effect_summary <- regression_results[
  regression_results$predictor == "us_groupUnited States",
  c(
    "sample", "model", "n_total", "n_us", "n_non_us",
    "odds_ratio", "ci_low_95", "ci_high_95", "p_wald"
  ),
  drop = FALSE
]

rownames(us_effect_summary) <- NULL

if (nrow(us_effect_summary) != 4) {
  stop("Expected four US-effect estimates.")
}

us_effect_wide <- tidyr::pivot_wider(
  us_effect_summary[
    ,
    c(
      "sample", "model", "odds_ratio",
      "ci_low_95", "ci_high_95", "p_wald"
    ),
    drop = FALSE
  ],
  names_from = "model",
  values_from = c(
    "odds_ratio",
    "ci_low_95",
    "ci_high_95",
    "p_wald"
  ),
  names_sep = "_"
)

us_effect_wide$or_ratio_adjusted_to_unadjusted <-
  us_effect_wide$odds_ratio_Adjusted /
  us_effect_wide$odds_ratio_Unadjusted

us_effect_wide$log_or_change_pct <- 100 * (
  log(us_effect_wide$odds_ratio_Adjusted) -
    log(us_effect_wide$odds_ratio_Unadjusted)
) / log(us_effect_wide$odds_ratio_Unadjusted)


# 14. PANEL B: DESCRIPTIVE STATISTICS -----------------------------

descriptive_results <- dplyr::bind_rows(
  lapply(names(samples), function(s) {

    df <- samples[[s]]

    df %>%
      dplyr::group_by(us_group) %>%
      dplyr::summarise(
        n = dplyr::n(),
        licensing_mean = mean(licensing, na.rm = TRUE),
        licensing_median = median(licensing, na.rm = TRUE),
        licensing_sd = sd(licensing, na.rm = TRUE),
        non_licensing_mean = mean(
          non_licensing_readiness,
          na.rm = TRUE
        ),
        non_licensing_median = median(
          non_licensing_readiness,
          na.rm = TRUE
        ),
        non_licensing_sd = sd(
          non_licensing_readiness,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        sample = s,
        .before = 1
      )
  })
)


# 15. LICENSING SCORE DISTRIBUTIONS -------------------------------

score_distributions <- dplyr::bind_rows(
  lapply(names(samples), function(s) {

    df <- samples[[s]]

    df <- df[
      !is.na(df$licensing),
      ,
      drop = FALSE
    ]

    df %>%
      dplyr::count(
        us_group,
        licensing,
        name = "n"
      ) %>%
      tidyr::complete(
        us_group,
        licensing = 1:6,
        fill = list(n = 0)
      ) %>%
      dplyr::group_by(us_group) %>%
      dplyr::mutate(
        share = n / sum(n)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        sample = s,
        .before = 1
      )
  })
)


# 16. PANEL C: MODEL DIAGNOSTICS ----------------------------------

panel_c <- regression_results %>%
  dplyr::select(
    sample,
    model,
    lr_statistic,
    lr_df,
    p_lr,
    mcfadden_r2
  ) %>%
  dplyr::distinct()


# 17. DESIGN-LEVEL AUDIT ------------------------------------------

design_audit <- dat %>%
  dplyr::select(
    design,
    country,
    region,
    us_group,
    microreactor,
    is_microreactor,
    licensing,
    financing,
    supply_chain,
    fuel_supply,
    siting,
    stakeholder_engagement,
    non_licensing_readiness,
    other_five_complete,
    model_complete,
    sample_inclusion
  )


# 18. EXCEL WORKBOOK ----------------------------------------------

excel_tables <- list(
  "S16 Panel A" = us_effect_summary,
  "S16 Panel B" = descriptive_results,
  "S16 Panel C" = panel_c,
  "Nested LR tests" = model_comparison,
  "OR change" = us_effect_wide,
  "All coefficients" = regression_results,
  "Licensing distributions" = score_distributions,
  "Model diagnostics" = model_diagnostics,
  "Sample counts" = sample_counts,
  "Design audit" = design_audit
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1"
)

for (sheet in names(excel_tables)) {

  tab <- as.data.frame(excel_tables[[sheet]])

  openxlsx::addWorksheet(wb, sheet)

  openxlsx::writeData(
    wb,
    sheet,
    tab,
    headerStyle = header_style,
    withFilter = nrow(tab) > 0
  )

  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(tab)),
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
  output_file,
  overwrite = TRUE
)


# 19. PRINT RESULTS -----------------------------------------------

cat("\n")
cat("====================================================\n")
cat("SUPPLEMENTARY TABLE S16\n")
cat("US VERSUS NON-US LICENSING READINESS\n")
cat("====================================================\n")

cat("\nPANEL A: US EFFECTS\n")
print(as.data.frame(us_effect_summary), row.names = FALSE)

cat("\nPANEL B: DESCRIPTIVE STATISTICS\n")
print(as.data.frame(descriptive_results), row.names = FALSE)

cat("\nPANEL C: MODEL DIAGNOSTICS\n")
print(as.data.frame(panel_c), row.names = FALSE)

cat("\nNESTED-MODEL COMPARISONS\n")
print(as.data.frame(model_comparison), row.names = FALSE)

cat("\nMODEL CONVERGENCE\n")
print(as.data.frame(model_diagnostics), row.names = FALSE)

cat("\nOUTPUT FILE:\n")
cat(output_file, "\n")

cat("\nAnalysis complete.\n")
