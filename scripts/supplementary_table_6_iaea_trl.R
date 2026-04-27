suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(MASS)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

make_ordered_outcome <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  
  if (sum(!is.na(x_num)) > 0 && all(is.na(x_chr) | !is.na(x_num))) {
    levs <- sort(unique(x_num[!is.na(x_num)]))
    ordered(x_num, levels = levs)
  } else {
    levs <- sort(unique(x_chr[!is.na(x_chr)]))
    ordered(x_chr, levels = levs)
  }
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

extract_polr_terms <- function(model, null_model, predictor_label, outcome_label, n_used) {
  coef_mat <- coef(summary(model))
  coef_mat <- coef_mat[!grepl("\\|", rownames(coef_mat)), , drop = FALSE]
  
  if (nrow(coef_mat) == 0) return(NULL)
  
  p_vals <- 2 * pnorm(abs(coef_mat[, "t value"]), lower.tail = FALSE)
  
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  data.frame(
    predictor   = predictor_label,
    outcome     = outcome_label,
    term        = rownames(coef_mat),
    estimate    = coef_mat[, "Value"],
    std_error   = coef_mat[, "Std. Error"],
    t_value     = coef_mat[, "t value"],
    p_value     = p_vals,
    odds_ratio  = exp(coef_mat[, "Value"]),
    ci_low      = exp(coef_mat[, "Value"] - 1.96 * coef_mat[, "Std. Error"]),
    ci_high     = exp(coef_mat[, "Value"] + 1.96 * coef_mat[, "Std. Error"]),
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ============================================================
# PATHS
# ============================================================

file_path <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/data/iaea_trl_crosscheck_source_data.xlsx"
sheet_name <- "iaea_trl_crosscheck_data"
out_dir <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/outputs/supplementary_table_6_iaea_trl_crosscheck"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# READ DATA
# ============================================================

df <- read_excel(file_path, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))

if ("design" %in% names(df)) {
  df <- df[!is.na(df$design), , drop = FALSE]
}

required_cols <- c(
  "trl",
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_binary",
  "uranium_enrichment"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ============================================================
# CLEAN VARIABLES
# ============================================================

df$trl_ord <- make_ordered_outcome(df$trl)
df$cd_start_num <- suppressWarnings(as.numeric(as.character(df$cd_start)))

water_raw <- tolower(trimws(as.character(df$water_cooled)))
water_raw <- gsub("\\s+", "_", water_raw)

df$water_cooled_bin <- dplyr::case_when(
  water_raw %in% c("true", "t", "yes", "y", "1", "water_cooled", "water-cooled", "water") ~ "Water-cooled",
  water_raw %in% c("false", "f", "no", "n", "0", "non_water_cooled", "non-water-cooled", "non_water", "non-water") ~ "Non-water-cooled",
  TRUE ~ NA_character_
)
df$water_cooled_bin <- factor(df$water_cooled_bin, levels = c("Non-water-cooled", "Water-cooled"))

spectrum_raw <- tolower(trimws(as.character(df$spectrum)))
spectrum_raw <- gsub("\\s+", "_", spectrum_raw)

df$spectrum_bin <- dplyr::case_when(
  spectrum_raw %in% c("fast") ~ "Fast",
  spectrum_raw %in% c("thermal") ~ "Thermal",
  TRUE ~ NA_character_
)
df$spectrum_bin <- factor(df$spectrum_bin, levels = c("Fast", "Thermal"))

fuel_raw <- tolower(trimws(as.character(df$fuel_binary)))
fuel_raw <- gsub("\\s+", "_", fuel_raw)

df$fuel_binary_bin <- dplyr::case_when(
  fuel_raw %in% c("uranium_oxide", "uranium-oxide", "uranium oxide") ~ "Uranium oxide",
  fuel_raw %in% c("other") ~ "Other",
  TRUE ~ NA_character_
)
df$fuel_binary_bin <- factor(df$fuel_binary_bin, levels = c("Other", "Uranium oxide"))

enrich_raw <- tolower(trimws(as.character(df$uranium_enrichment)))
enrich_raw <- gsub("\\s+", "_", enrich_raw)

df$uranium_enrichment_3 <- dplyr::case_when(
  enrich_raw %in% c("low", "leu", "low_enrichment") ~ "Low",
  enrich_raw %in% c("medium", "meu", "medium_enrichment") ~ "Medium",
  enrich_raw %in% c("high", "heu", "haleu", "high_enrichment") ~ "High",
  enrich_raw %in% c("not_applicable", "not-applicable", "na", "n_a", "n/a", "none", "not_required") ~ NA_character_,
  TRUE ~ NA_character_
)
df$uranium_enrichment_3 <- factor(df$uranium_enrichment_3, levels = c("High", "Medium", "Low"))

# ============================================================
# COUNTS
# ============================================================

trl_counts <- data.frame(
  TRL = names(table(df$trl_ord, useNA = "no")),
  n = as.integer(table(df$trl_ord, useNA = "no")),
  stringsAsFactors = FALSE
)

predictor_counts <- bind_rows(
  data.frame(
    predictor = "Water-cooled status",
    category = names(table(df$water_cooled_bin, useNA = "no")),
    n = as.integer(table(df$water_cooled_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Spectrum",
    category = names(table(df$spectrum_bin, useNA = "no")),
    n = as.integer(table(df$spectrum_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Fuel binary",
    category = names(table(df$fuel_binary_bin, useNA = "no")),
    n = as.integer(table(df$fuel_binary_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Uranium enrichment",
    category = names(table(df$uranium_enrichment_3, useNA = "no")),
    n = as.integer(table(df$uranium_enrichment_3, useNA = "no")),
    stringsAsFactors = FALSE
  )
)

cat("\nTRL COUNTS\n")
print(trl_counts)

cat("\nPREDICTOR COUNTS\n")
print(predictor_counts)

# ============================================================
# WATER-COOLED FIRST CHECK
# ============================================================

water_dat <- data.frame(
  outcome = df$trl_ord,
  predictor = df$water_cooled_bin
)

water_dat <- water_dat[complete.cases(water_dat$outcome, water_dat$predictor), , drop = FALSE]

if (nrow(water_dat) == 0) stop("No complete cases for water-cooled first check.")
if (length(unique(water_dat$predictor)) < 2) stop("Water-cooled predictor has fewer than two categories after cleaning.")
if (length(unique(water_dat$outcome)) < 2) stop("TRL outcome has fewer than two categories after cleaning.")

water_fit <- suppressWarnings(MASS::polr(outcome ~ predictor, data = water_dat, method = "logistic", Hess = TRUE))
water_fit_null <- suppressWarnings(MASS::polr(outcome ~ 1, data = water_dat, method = "logistic", Hess = TRUE))

water_results <- extract_polr_terms(
  model = water_fit,
  null_model = water_fit_null,
  predictor_label = "Water-cooled status",
  outcome_label = "Technology readiness level (TRL)",
  n_used = nrow(water_dat)
)

water_summary <- data.frame(
  Predictor    = water_results$predictor,
  Term         = water_results$term,
  Estimate     = format_num(water_results$estimate, 3),
  Std_Error    = format_num(water_results$std_error, 3),
  t_value      = format_num(water_results$t_value, 3),
  p_value      = format_p(water_results$p_value),
  Odds_Ratio   = format_num(water_results$odds_ratio, 3),
  CI_Lower     = format_num(water_results$ci_low, 3),
  CI_Upper     = format_num(water_results$ci_high, 3),
  LR_stat      = format_num(water_results$lr_stat, 3),
  LR_df        = water_results$lr_df,
  LR_p_value   = format_p(water_results$lr_p_value),
  McFadden_R2  = format_num(water_results$mcfadden_r2, 3),
  n            = water_results$n,
  stringsAsFactors = FALSE
)

cat("\nWATER-COOLED FIRST CHECK\n")
print(water_results[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "t_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nWATER-COOLED FIRST CHECK (FORMATTED)\n")
print(water_summary)

# ============================================================
# FULL EXPLORATORY UNIVARIATE ORDINAL LOGITS
# ============================================================

predictor_specs <- list(
  list(var = "cd_start_num", label = "Conceptual design start year"),
  list(var = "water_cooled_bin", label = "Water-cooled status"),
  list(var = "spectrum_bin", label = "Spectrum"),
  list(var = "fuel_binary_bin", label = "Fuel binary"),
  list(var = "uranium_enrichment_3", label = "Uranium enrichment")
)

results_list <- list()
omnibus_list <- list()
k <- 1
j <- 1

for (spec in predictor_specs) {
  dat <- data.frame(
    outcome = df$trl_ord,
    predictor = df[[spec$var]]
  )
  
  dat <- dat[complete.cases(dat$outcome, dat$predictor), , drop = FALSE]
  
  if (nrow(dat) == 0) next
  if (length(unique(dat$outcome)) < 2) next
  if (length(unique(dat$predictor)) < 2) next
  
  if (is.character(dat$predictor)) {
    dat$predictor <- factor(dat$predictor)
  }
  
  fit <- tryCatch(
    suppressWarnings(MASS::polr(outcome ~ predictor, data = dat, method = "logistic", Hess = TRUE)),
    error = function(e) NULL
  )
  
  fit_null <- tryCatch(
    suppressWarnings(MASS::polr(outcome ~ 1, data = dat, method = "logistic", Hess = TRUE)),
    error = function(e) NULL
  )
  
  if (is.null(fit) || is.null(fit_null)) next
  
  extracted <- extract_polr_terms(
    model = fit,
    null_model = fit_null,
    predictor_label = spec$label,
    outcome_label = "Technology readiness level (TRL)",
    n_used = nrow(dat)
  )
  
  if (!is.null(extracted)) {
    results_list[[k]] <- extracted
    k <- k + 1
  }
  
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(fit_null))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(fit), "df") - attr(logLik(fit_null), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  omnibus_list[[j]] <- data.frame(
    predictor   = spec$label,
    outcome     = "Technology readiness level (TRL)",
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = nrow(dat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  j <- j + 1
}

if (length(results_list) == 0) {
  stop("No valid ordinal logistic models were estimated.")
}

results_df <- do.call(rbind, results_list)
omnibus_df <- do.call(rbind, omnibus_list)

predictor_order <- c(
  "Conceptual design start year",
  "Water-cooled status",
  "Spectrum",
  "Fuel binary",
  "Uranium enrichment"
)

results_df$predictor <- factor(results_df$predictor, levels = predictor_order)
results_df <- results_df[order(results_df$predictor), , drop = FALSE]

omnibus_df$predictor <- factor(omnibus_df$predictor, levels = predictor_order)
omnibus_df <- omnibus_df[order(omnibus_df$predictor), , drop = FALSE]

cat("\nFULL EXPLORATORY ORDINAL LOGIT RESULTS: TRL AS OUTCOME\n")
print(results_df[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "t_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nFULL EXPLORATORY OMNIBUS LR TESTS\n")
print(omnibus_df)

results_summary <- data.frame(
  Predictor    = as.character(results_df$predictor),
  Term         = results_df$term,
  Estimate     = format_num(results_df$estimate, 3),
  Std_Error    = format_num(results_df$std_error, 3),
  t_value      = format_num(results_df$t_value, 3),
  p_value      = format_p(results_df$p_value),
  Odds_Ratio   = format_num(results_df$odds_ratio, 3),
  CI_Lower     = format_num(results_df$ci_low, 3),
  CI_Upper     = format_num(results_df$ci_high, 3),
  LR_stat      = format_num(results_df$lr_stat, 3),
  LR_df        = results_df$lr_df,
  LR_p_value   = format_p(results_df$lr_p_value),
  McFadden_R2  = format_num(results_df$mcfadden_r2, 3),
  n            = results_df$n,
  stringsAsFactors = FALSE
)

omnibus_summary <- data.frame(
  Predictor    = as.character(omnibus_df$predictor),
  Outcome      = omnibus_df$outcome,
  LR_stat      = format_num(omnibus_df$lr_stat, 3),
  LR_df        = omnibus_df$lr_df,
  LR_p_value   = format_p(omnibus_df$lr_p_value),
  McFadden_R2  = format_num(omnibus_df$mcfadden_r2, 3),
  n            = omnibus_df$n,
  stringsAsFactors = FALSE
)

cat("\nFULL EXPLORATORY COEFFICIENT TABLE (FORMATTED)\n")
print(results_summary)

cat("\nFULL EXPLORATORY OMNIBUS TABLE (FORMATTED)\n")
print(omnibus_summary)

# ============================================================
# SAVE OUTPUTS
# ============================================================

write.csv(trl_counts, file.path(out_dir, "iaea_trl_counts.csv"), row.names = FALSE)
write.csv(predictor_counts, file.path(out_dir, "iaea_trl_predictor_counts.csv"), row.names = FALSE)
write.csv(water_results, file.path(out_dir, "iaea_trl_water_cooled_first_check_raw.csv"), row.names = FALSE)
write.csv(water_summary, file.path(out_dir, "iaea_trl_water_cooled_first_check_summary.csv"), row.names = FALSE)
write.csv(results_df, file.path(out_dir, "iaea_trl_univariate_ordinal_logit_raw.csv"), row.names = FALSE)
write.csv(results_summary, file.path(out_dir, "iaea_trl_univariate_ordinal_logit_summary.csv"), row.names = FALSE)
write.csv(omnibus_df, file.path(out_dir, "iaea_trl_univariate_ordinal_logit_omnibus_raw.csv"), row.names = FALSE)
write.csv(omnibus_summary, file.path(out_dir, "iaea_trl_univariate_ordinal_logit_omnibus_summary.csv"), row.names = FALSE)


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

extract_glm_terms <- function(model, predictor_label, outcome_label, n_used) {
  coef_mat <- coef(summary(model))
  coef_mat <- coef_mat[rownames(coef_mat) != "(Intercept)", , drop = FALSE]
  
  if (nrow(coef_mat) == 0) return(NULL)
  
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(update(model, . ~ 1)))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(model), "df") - attr(logLik(update(model, . ~ 1)), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  data.frame(
    predictor   = predictor_label,
    outcome     = outcome_label,
    term        = rownames(coef_mat),
    estimate    = coef_mat[, "Estimate"],
    std_error   = coef_mat[, "Std. Error"],
    z_value     = coef_mat[, "z value"],
    p_value     = coef_mat[, "Pr(>|z|)"],
    odds_ratio  = exp(coef_mat[, "Estimate"]),
    ci_low      = exp(coef_mat[, "Estimate"] - 1.96 * coef_mat[, "Std. Error"]),
    ci_high     = exp(coef_mat[, "Estimate"] + 1.96 * coef_mat[, "Std. Error"]),
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ============================================================
# PATHS
# ============================================================

file_path <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/data/iaea_trl_crosscheck_source_data.xlsx"
sheet_name <- "iaea_trl_crosscheck_data"
out_dir <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/outputs/supplementary_table_6_iaea_trl_crosscheck"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# READ DATA
# ============================================================

df <- read_excel(file_path, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))

if ("design" %in% names(df)) {
  df <- df[!is.na(df$design), , drop = FALSE]
}

required_cols <- c(
  "trl",
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_binary",
  "uranium_enrichment"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ============================================================
# CLEAN VARIABLES
# ============================================================

df$trl_num <- suppressWarnings(as.numeric(as.character(df$trl)))
df$cd_start_num <- suppressWarnings(as.numeric(as.character(df$cd_start)))

df$trl_binary <- case_when(
  !is.na(df$trl_num) & df$trl_num %in% c(1, 2) ~ "TRL_1_2",
  !is.na(df$trl_num) & df$trl_num >= 3         ~ "TRL_3plus",
  TRUE ~ NA_character_
)
df$trl_binary <- factor(df$trl_binary, levels = c("TRL_1_2", "TRL_3plus"))

water_raw <- tolower(trimws(as.character(df$water_cooled)))
water_raw <- gsub("\\s+", "_", water_raw)

df$water_cooled_bin <- case_when(
  water_raw %in% c("true", "t", "yes", "y", "1", "water_cooled", "water-cooled", "water") ~ "Water-cooled",
  water_raw %in% c("false", "f", "no", "n", "0", "non_water_cooled", "non-water-cooled", "non_water", "non-water") ~ "Non-water-cooled",
  TRUE ~ NA_character_
)
df$water_cooled_bin <- factor(df$water_cooled_bin, levels = c("Non-water-cooled", "Water-cooled"))

spectrum_raw <- tolower(trimws(as.character(df$spectrum)))
spectrum_raw <- gsub("\\s+", "_", spectrum_raw)

df$spectrum_bin <- case_when(
  spectrum_raw %in% c("fast") ~ "Fast",
  spectrum_raw %in% c("thermal") ~ "Thermal",
  TRUE ~ NA_character_
)
df$spectrum_bin <- factor(df$spectrum_bin, levels = c("Fast", "Thermal"))

fuel_raw <- tolower(trimws(as.character(df$fuel_binary)))
fuel_raw <- gsub("\\s+", "_", fuel_raw)

df$fuel_binary_bin <- case_when(
  fuel_raw %in% c("uranium_oxide", "uranium-oxide", "uranium oxide") ~ "Uranium oxide",
  fuel_raw %in% c("other") ~ "Other",
  TRUE ~ NA_character_
)
df$fuel_binary_bin <- factor(df$fuel_binary_bin, levels = c("Other", "Uranium oxide"))

enrich_raw <- tolower(trimws(as.character(df$uranium_enrichment)))
enrich_raw <- gsub("\\s+", "_", enrich_raw)

df$uranium_enrichment_3 <- case_when(
  enrich_raw %in% c("low", "leu", "low_enrichment") ~ "Low",
  enrich_raw %in% c("medium", "meu", "medium_enrichment") ~ "Medium",
  enrich_raw %in% c("high", "heu", "haleu", "high_enrichment") ~ "High",
  enrich_raw %in% c("not_applicable", "not-applicable", "na", "n_a", "n/a", "none", "not_required") ~ NA_character_,
  TRUE ~ NA_character_
)
df$uranium_enrichment_3 <- factor(df$uranium_enrichment_3, levels = c("High", "Medium", "Low"))

# ============================================================
# COUNTS
# ============================================================

trl_binary_counts <- data.frame(
  TRL_binary = names(table(df$trl_binary, useNA = "no")),
  n = as.integer(table(df$trl_binary, useNA = "no")),
  stringsAsFactors = FALSE
)

predictor_counts <- bind_rows(
  data.frame(
    predictor = "Water-cooled status",
    category = names(table(df$water_cooled_bin, useNA = "no")),
    n = as.integer(table(df$water_cooled_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Spectrum",
    category = names(table(df$spectrum_bin, useNA = "no")),
    n = as.integer(table(df$spectrum_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Fuel binary",
    category = names(table(df$fuel_binary_bin, useNA = "no")),
    n = as.integer(table(df$fuel_binary_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Uranium enrichment",
    category = names(table(df$uranium_enrichment_3, useNA = "no")),
    n = as.integer(table(df$uranium_enrichment_3, useNA = "no")),
    stringsAsFactors = FALSE
  )
)

cat("\nCOLLAPSED TRL COUNTS\n")
print(trl_binary_counts)

cat("\nPREDICTOR COUNTS\n")
print(predictor_counts)

# ============================================================
# WATER-COOLED FIRST CHECK: BINARY LOGIT
# ============================================================

water_dat <- data.frame(
  outcome   = df$trl_binary,
  predictor = df$water_cooled_bin
)

water_dat <- water_dat[complete.cases(water_dat$outcome, water_dat$predictor), , drop = FALSE]

if (nrow(water_dat) == 0) stop("No complete cases for water-cooled first check.")
if (length(unique(water_dat$predictor)) < 2) stop("Water-cooled predictor has fewer than two categories after cleaning.")
if (length(unique(water_dat$outcome)) < 2) stop("Collapsed TRL outcome has fewer than two categories after cleaning.")

water_dat$outcome_num <- ifelse(water_dat$outcome == "TRL_3plus", 1, 0)

water_fit <- glm(outcome_num ~ predictor, data = water_dat, family = binomial(link = "logit"))

water_results <- extract_glm_terms(
  model = water_fit,
  predictor_label = "Water-cooled status",
  outcome_label = "TRL 3+ versus TRL 1-2",
  n_used = nrow(water_dat)
)

water_summary <- data.frame(
  Predictor    = water_results$predictor,
  Term         = water_results$term,
  Estimate     = format_num(water_results$estimate, 3),
  Std_Error    = format_num(water_results$std_error, 3),
  z_value      = format_num(water_results$z_value, 3),
  p_value      = format_p(water_results$p_value),
  Odds_Ratio   = format_num(water_results$odds_ratio, 3),
  CI_Lower     = format_num(water_results$ci_low, 3),
  CI_Upper     = format_num(water_results$ci_high, 3),
  LR_stat      = format_num(water_results$lr_stat, 3),
  LR_df        = water_results$lr_df,
  LR_p_value   = format_p(water_results$lr_p_value),
  McFadden_R2  = format_num(water_results$mcfadden_r2, 3),
  n            = water_results$n,
  stringsAsFactors = FALSE
)

cat("\nWATER-COOLED FIRST CHECK: TRL 3+ VS TRL 1-2\n")
print(water_results[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "z_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nWATER-COOLED FIRST CHECK (FORMATTED)\n")
print(water_summary)

# ============================================================
# FULL EXPLORATORY BINARY LOGITS
# ============================================================

predictor_specs <- list(
  list(var = "cd_start_num", label = "Conceptual design start year"),
  list(var = "water_cooled_bin", label = "Water-cooled status"),
  list(var = "spectrum_bin", label = "Spectrum"),
  list(var = "fuel_binary_bin", label = "Fuel binary"),
  list(var = "uranium_enrichment_3", label = "Uranium enrichment")
)

results_list <- list()
omnibus_list <- list()
k <- 1
j <- 1

for (spec in predictor_specs) {
  dat <- data.frame(
    outcome   = df$trl_binary,
    predictor = df[[spec$var]]
  )
  
  dat <- dat[complete.cases(dat$outcome, dat$predictor), , drop = FALSE]
  
  if (nrow(dat) == 0) next
  if (length(unique(dat$outcome)) < 2) next
  if (length(unique(dat$predictor)) < 2) next
  
  if (is.character(dat$predictor)) {
    dat$predictor <- factor(dat$predictor)
  }
  
  dat$outcome_num <- ifelse(dat$outcome == "TRL_3plus", 1, 0)
  
  fit <- tryCatch(
    glm(outcome_num ~ predictor, data = dat, family = binomial(link = "logit")),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  extracted <- extract_glm_terms(
    model = fit,
    predictor_label = spec$label,
    outcome_label = "TRL 3+ versus TRL 1-2",
    n_used = nrow(dat)
  )
  
  if (!is.null(extracted)) {
    results_list[[k]] <- extracted
    k <- k + 1
  }
  
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(fit), "df") - attr(logLik(update(fit, . ~ 1)), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  omnibus_list[[j]] <- data.frame(
    predictor   = spec$label,
    outcome     = "TRL 3+ versus TRL 1-2",
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = nrow(dat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  j <- j + 1
}

if (length(results_list) == 0) {
  stop("No valid binary logistic models were estimated.")
}

results_df <- do.call(rbind, results_list)
omnibus_df <- do.call(rbind, omnibus_list)

predictor_order <- c(
  "Conceptual design start year",
  "Water-cooled status",
  "Spectrum",
  "Fuel binary",
  "Uranium enrichment"
)

results_df$predictor <- factor(results_df$predictor, levels = predictor_order)
results_df <- results_df[order(results_df$predictor), , drop = FALSE]

omnibus_df$predictor <- factor(omnibus_df$predictor, levels = predictor_order)
omnibus_df <- omnibus_df[order(omnibus_df$predictor), , drop = FALSE]

cat("\nFULL EXPLORATORY BINARY LOGIT RESULTS: TRL 3+ VS TRL 1-2\n")
print(results_df[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "z_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nFULL EXPLORATORY OMNIBUS LR TESTS\n")
print(omnibus_df)

results_summary <- data.frame(
  Predictor    = as.character(results_df$predictor),
  Term         = results_df$term,
  Estimate     = format_num(results_df$estimate, 3),
  Std_Error    = format_num(results_df$std_error, 3),
  z_value      = format_num(results_df$z_value, 3),
  p_value      = format_p(results_df$p_value),
  Odds_Ratio   = format_num(results_df$odds_ratio, 3),
  CI_Lower     = format_num(results_df$ci_low, 3),
  CI_Upper     = format_num(results_df$ci_high, 3),
  LR_stat      = format_num(results_df$lr_stat, 3),
  LR_df        = results_df$lr_df,
  LR_p_value   = format_p(results_df$lr_p_value),
  McFadden_R2  = format_num(results_df$mcfadden_r2, 3),
  n            = results_df$n,
  stringsAsFactors = FALSE
)

omnibus_summary <- data.frame(
  Predictor    = as.character(omnibus_df$predictor),
  Outcome      = omnibus_df$outcome,
  LR_stat      = format_num(omnibus_df$lr_stat, 3),
  LR_df        = omnibus_df$lr_df,
  LR_p_value   = format_p(omnibus_df$lr_p_value),
  McFadden_R2  = format_num(omnibus_df$mcfadden_r2, 3),
  n            = omnibus_df$n,
  stringsAsFactors = FALSE
)

cat("\nFULL EXPLORATORY COEFFICIENT TABLE (FORMATTED)\n")
print(results_summary)

cat("\nFULL EXPLORATORY OMNIBUS TABLE (FORMATTED)\n")
print(omnibus_summary)

# ============================================================
# SAVE OUTPUTS
# ============================================================

write.csv(trl_binary_counts, file.path(out_dir, "iaea_trl_binary_counts.csv"), row.names = FALSE)
write.csv(predictor_counts, file.path(out_dir, "iaea_trl_binary_predictor_counts.csv"), row.names = FALSE)
write.csv(water_results, file.path(out_dir, "iaea_trl_binary_water_cooled_first_check_raw.csv"), row.names = FALSE)
write.csv(water_summary, file.path(out_dir, "iaea_trl_binary_water_cooled_first_check_summary.csv"), row.names = FALSE)
write.csv(results_df, file.path(out_dir, "iaea_trl_binary_logit_raw.csv"), row.names = FALSE)
write.csv(results_summary, file.path(out_dir, "iaea_trl_binary_logit_summary.csv"), row.names = FALSE)
write.csv(omnibus_df, file.path(out_dir, "iaea_trl_binary_logit_omnibus_raw.csv"), row.names = FALSE)
write.csv(omnibus_summary, file.path(out_dir, "iaea_trl_binary_logit_omnibus_summary.csv"), row.names = FALSE)


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(MASS)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

extract_polr_terms <- function(model, null_model, predictor_label, outcome_label, n_used) {
  coef_mat <- coef(summary(model))
  coef_mat <- coef_mat[!grepl("\\|", rownames(coef_mat)), , drop = FALSE]
  
  if (nrow(coef_mat) == 0) return(NULL)
  
  p_vals <- 2 * pnorm(abs(coef_mat[, "t value"]), lower.tail = FALSE)
  
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  data.frame(
    predictor   = predictor_label,
    outcome     = outcome_label,
    term        = rownames(coef_mat),
    estimate    = coef_mat[, "Value"],
    std_error   = coef_mat[, "Std. Error"],
    t_value     = coef_mat[, "t value"],
    p_value     = p_vals,
    odds_ratio  = exp(coef_mat[, "Value"]),
    ci_low      = exp(coef_mat[, "Value"] - 1.96 * coef_mat[, "Std. Error"]),
    ci_high     = exp(coef_mat[, "Value"] + 1.96 * coef_mat[, "Std. Error"]),
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

extract_glm_terms <- function(model, predictor_label, outcome_label, n_used) {
  coef_mat <- coef(summary(model))
  coef_mat <- coef_mat[rownames(coef_mat) != "(Intercept)", , drop = FALSE]
  
  if (nrow(coef_mat) == 0) return(NULL)
  
  null_model <- update(model, . ~ 1)
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  data.frame(
    predictor   = predictor_label,
    outcome     = outcome_label,
    term        = rownames(coef_mat),
    estimate    = coef_mat[, "Estimate"],
    std_error   = coef_mat[, "Std. Error"],
    z_value     = coef_mat[, "z value"],
    p_value     = coef_mat[, "Pr(>|z|)"],
    odds_ratio  = exp(coef_mat[, "Estimate"]),
    ci_low      = exp(coef_mat[, "Estimate"] - 1.96 * coef_mat[, "Std. Error"]),
    ci_high     = exp(coef_mat[, "Estimate"] + 1.96 * coef_mat[, "Std. Error"]),
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ============================================================
# PATHS
# ============================================================

file_path <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/data/iaea_trl_crosscheck_source_data.xlsx"
sheet_name <- "iaea_trl_crosscheck_data"
out_dir <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/outputs/supplementary_table_6_iaea_trl_crosscheck"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# READ DATA
# ============================================================

df <- read_excel(file_path, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))

if ("design" %in% names(df)) {
  df <- df[!is.na(df$design), , drop = FALSE]
}

required_cols <- c(
  "phase",
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_binary",
  "uranium_enrichment"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ============================================================
# CLEAN PREDICTORS
# ============================================================

df$cd_start_num <- suppressWarnings(as.numeric(as.character(df$cd_start)))

water_raw <- tolower(trimws(as.character(df$water_cooled)))
water_raw <- gsub("\\s+", "_", water_raw)

df$water_cooled_bin <- dplyr::case_when(
  water_raw %in% c("true", "t", "yes", "y", "1", "water_cooled", "water-cooled", "water") ~ "Water-cooled",
  water_raw %in% c("false", "f", "no", "n", "0", "non_water_cooled", "non-water-cooled", "non_water", "non-water") ~ "Non-water-cooled",
  TRUE ~ NA_character_
)
df$water_cooled_bin <- factor(df$water_cooled_bin, levels = c("Non-water-cooled", "Water-cooled"))

spectrum_raw <- tolower(trimws(as.character(df$spectrum)))
spectrum_raw <- gsub("\\s+", "_", spectrum_raw)

df$spectrum_bin <- dplyr::case_when(
  spectrum_raw %in% c("fast") ~ "Fast",
  spectrum_raw %in% c("thermal") ~ "Thermal",
  TRUE ~ NA_character_
)
df$spectrum_bin <- factor(df$spectrum_bin, levels = c("Fast", "Thermal"))

fuel_raw <- tolower(trimws(as.character(df$fuel_binary)))
fuel_raw <- gsub("\\s+", "_", fuel_raw)

df$fuel_binary_bin <- dplyr::case_when(
  fuel_raw %in% c("uranium_oxide", "uranium-oxide", "uranium oxide") ~ "Uranium oxide",
  fuel_raw %in% c("other") ~ "Other",
  TRUE ~ NA_character_
)
df$fuel_binary_bin <- factor(df$fuel_binary_bin, levels = c("Other", "Uranium oxide"))

enrich_raw <- tolower(trimws(as.character(df$uranium_enrichment)))
enrich_raw <- gsub("\\s+", "_", enrich_raw)

df$uranium_enrichment_3 <- dplyr::case_when(
  enrich_raw %in% c("low", "leu", "low_enrichment") ~ "Low",
  enrich_raw %in% c("medium", "meu", "medium_enrichment") ~ "Medium",
  enrich_raw %in% c("high", "heu", "haleu", "high_enrichment") ~ "High",
  enrich_raw %in% c("not_applicable", "not-applicable", "na", "n_a", "n/a", "none", "not_required") ~ NA_character_,
  TRUE ~ NA_character_
)
df$uranium_enrichment_3 <- factor(df$uranium_enrichment_3, levels = c("High", "Medium", "Low"))

# ============================================================
# CLEAN PHASE
# ============================================================

phase_raw <- tolower(trimws(as.character(df$phase)))
phase_raw <- gsub("\\s+", "_", phase_raw)

df$phase_clean <- dplyr::case_when(
  phase_raw %in% c("conceptual_design", "conceptual") ~ "Conceptual design",
  phase_raw %in% c("basic_design", "basic") ~ "Basic design",
  phase_raw %in% c("detailed_design", "detailed") ~ "Detailed design",
  phase_raw %in% c("licensing", "licensed") ~ "Licensing",
  phase_raw %in% c("construction", "under_construction") ~ "Construction",
  phase_raw %in% c("operational", "operation", "operating") ~ "Operational",
  TRUE ~ NA_character_
)

df$phase_5 <- dplyr::case_when(
  df$phase_clean %in% c("Conceptual design", "Basic design", "Detailed design", "Licensing", "Construction") ~ df$phase_clean,
  TRUE ~ NA_character_
)
df$phase_5 <- factor(
  df$phase_5,
  levels = c("Conceptual design", "Basic design", "Detailed design", "Licensing", "Construction"),
  ordered = TRUE
)

df$phase_binary <- dplyr::case_when(
  df$phase_clean %in% c("Conceptual design", "Basic design") ~ "Conceptual/basic",
  df$phase_clean %in% c("Detailed design", "Licensing", "Construction", "Operational") ~ "Detailed and beyond",
  TRUE ~ NA_character_
)
df$phase_binary <- factor(df$phase_binary, levels = c("Conceptual/basic", "Detailed and beyond"))

# ============================================================
# COUNTS
# ============================================================

phase_5_counts <- data.frame(
  Phase = names(table(df$phase_5, useNA = "no")),
  n = as.integer(table(df$phase_5, useNA = "no")),
  stringsAsFactors = FALSE
)

phase_binary_counts <- data.frame(
  Phase_binary = names(table(df$phase_binary, useNA = "no")),
  n = as.integer(table(df$phase_binary, useNA = "no")),
  stringsAsFactors = FALSE
)

predictor_counts <- bind_rows(
  data.frame(
    predictor = "Water-cooled status",
    category = names(table(df$water_cooled_bin, useNA = "no")),
    n = as.integer(table(df$water_cooled_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Spectrum",
    category = names(table(df$spectrum_bin, useNA = "no")),
    n = as.integer(table(df$spectrum_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Fuel binary",
    category = names(table(df$fuel_binary_bin, useNA = "no")),
    n = as.integer(table(df$fuel_binary_bin, useNA = "no")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    predictor = "Uranium enrichment",
    category = names(table(df$uranium_enrichment_3, useNA = "no")),
    n = as.integer(table(df$uranium_enrichment_3, useNA = "no")),
    stringsAsFactors = FALSE
  )
)

cat("\nFIVE-PHASE COUNTS\n")
print(phase_5_counts)

cat("\nBINARY PHASE COUNTS\n")
print(phase_binary_counts)

cat("\nPREDICTOR COUNTS\n")
print(predictor_counts)

# ============================================================
# FIVE-PHASE ORDINAL LOGITS
# ============================================================

predictor_specs <- list(
  list(var = "cd_start_num", label = "Conceptual design start year"),
  list(var = "water_cooled_bin", label = "Water-cooled status"),
  list(var = "spectrum_bin", label = "Spectrum"),
  list(var = "fuel_binary_bin", label = "Fuel binary"),
  list(var = "uranium_enrichment_3", label = "Uranium enrichment")
)

phase5_results_list <- list()
phase5_omnibus_list <- list()
k <- 1
j <- 1

for (spec in predictor_specs) {
  dat <- data.frame(
    outcome   = df$phase_5,
    predictor = df[[spec$var]]
  )
  
  dat <- dat[complete.cases(dat$outcome, dat$predictor), , drop = FALSE]
  
  if (nrow(dat) == 0) next
  if (length(unique(dat$outcome)) < 2) next
  if (length(unique(dat$predictor)) < 2) next
  
  if (is.character(dat$predictor)) {
    dat$predictor <- factor(dat$predictor)
  }
  
  fit <- tryCatch(
    suppressWarnings(MASS::polr(outcome ~ predictor, data = dat, method = "logistic", Hess = TRUE)),
    error = function(e) NULL
  )
  
  fit_null <- tryCatch(
    suppressWarnings(MASS::polr(outcome ~ 1, data = dat, method = "logistic", Hess = TRUE)),
    error = function(e) NULL
  )
  
  if (is.null(fit) || is.null(fit_null)) next
  
  extracted <- extract_polr_terms(
    model = fit,
    null_model = fit_null,
    predictor_label = spec$label,
    outcome_label = "Five-phase development status",
    n_used = nrow(dat)
  )
  
  if (!is.null(extracted)) {
    phase5_results_list[[k]] <- extracted
    k <- k + 1
  }
  
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(fit_null))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(fit), "df") - attr(logLik(fit_null), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  phase5_omnibus_list[[j]] <- data.frame(
    predictor   = spec$label,
    outcome     = "Five-phase development status",
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = nrow(dat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  j <- j + 1
}

if (length(phase5_results_list) == 0) {
  stop("No valid five-phase ordinal logistic models were estimated.")
}

phase5_results_df <- do.call(rbind, phase5_results_list)
phase5_omnibus_df <- do.call(rbind, phase5_omnibus_list)

predictor_order <- c(
  "Conceptual design start year",
  "Water-cooled status",
  "Spectrum",
  "Fuel binary",
  "Uranium enrichment"
)

phase5_results_df$predictor <- factor(phase5_results_df$predictor, levels = predictor_order)
phase5_results_df <- phase5_results_df[order(phase5_results_df$predictor), , drop = FALSE]

phase5_omnibus_df$predictor <- factor(phase5_omnibus_df$predictor, levels = predictor_order)
phase5_omnibus_df <- phase5_omnibus_df[order(phase5_omnibus_df$predictor), , drop = FALSE]

cat("\nFIVE-PHASE ORDINAL LOGIT RESULTS\n")
print(phase5_results_df[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "t_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nFIVE-PHASE OMNIBUS LR TESTS\n")
print(phase5_omnibus_df)

phase5_results_summary <- data.frame(
  Predictor    = as.character(phase5_results_df$predictor),
  Term         = phase5_results_df$term,
  Estimate     = format_num(phase5_results_df$estimate, 3),
  Std_Error    = format_num(phase5_results_df$std_error, 3),
  t_value      = format_num(phase5_results_df$t_value, 3),
  p_value      = format_p(phase5_results_df$p_value),
  Odds_Ratio   = format_num(phase5_results_df$odds_ratio, 3),
  CI_Lower     = format_num(phase5_results_df$ci_low, 3),
  CI_Upper     = format_num(phase5_results_df$ci_high, 3),
  LR_stat      = format_num(phase5_results_df$lr_stat, 3),
  LR_df        = phase5_results_df$lr_df,
  LR_p_value   = format_p(phase5_results_df$lr_p_value),
  McFadden_R2  = format_num(phase5_results_df$mcfadden_r2, 3),
  n            = phase5_results_df$n,
  stringsAsFactors = FALSE
)

phase5_omnibus_summary <- data.frame(
  Predictor    = as.character(phase5_omnibus_df$predictor),
  Outcome      = phase5_omnibus_df$outcome,
  LR_stat      = format_num(phase5_omnibus_df$lr_stat, 3),
  LR_df        = phase5_omnibus_df$lr_df,
  LR_p_value   = format_p(phase5_omnibus_df$lr_p_value),
  McFadden_R2  = format_num(phase5_omnibus_df$mcfadden_r2, 3),
  n            = phase5_omnibus_df$n,
  stringsAsFactors = FALSE
)

cat("\nFIVE-PHASE COEFFICIENT TABLE (FORMATTED)\n")
print(phase5_results_summary)

cat("\nFIVE-PHASE OMNIBUS TABLE (FORMATTED)\n")
print(phase5_omnibus_summary)

# ============================================================
# BINARY PHASE LOGITS
# ============================================================

binary_results_list <- list()
binary_omnibus_list <- list()
k <- 1
j <- 1

for (spec in predictor_specs) {
  dat <- data.frame(
    outcome   = df$phase_binary,
    predictor = df[[spec$var]]
  )
  
  dat <- dat[complete.cases(dat$outcome, dat$predictor), , drop = FALSE]
  
  if (nrow(dat) == 0) next
  if (length(unique(dat$outcome)) < 2) next
  if (length(unique(dat$predictor)) < 2) next
  
  if (is.character(dat$predictor)) {
    dat$predictor <- factor(dat$predictor)
  }
  
  dat$outcome_num <- ifelse(dat$outcome == "Detailed and beyond", 1, 0)
  
  fit <- tryCatch(
    glm(outcome_num ~ predictor, data = dat, family = binomial(link = "logit")),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  extracted <- extract_glm_terms(
    model = fit,
    predictor_label = spec$label,
    outcome_label = "Detailed and beyond versus conceptual/basic",
    n_used = nrow(dat)
  )
  
  if (!is.null(extracted)) {
    binary_results_list[[k]] <- extracted
    k <- k + 1
  }
  
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df   <- attr(logLik(fit), "df") - attr(logLik(update(fit, . ~ 1)), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  binary_omnibus_list[[j]] <- data.frame(
    predictor   = spec$label,
    outcome     = "Detailed and beyond versus conceptual/basic",
    lr_stat     = lr_stat,
    lr_df       = lr_df,
    lr_p_value  = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n           = nrow(dat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  j <- j + 1
}

if (length(binary_results_list) == 0) {
  stop("No valid binary phase logistic models were estimated.")
}

binary_results_df <- do.call(rbind, binary_results_list)
binary_omnibus_df <- do.call(rbind, binary_omnibus_list)

binary_results_df$predictor <- factor(binary_results_df$predictor, levels = predictor_order)
binary_results_df <- binary_results_df[order(binary_results_df$predictor), , drop = FALSE]

binary_omnibus_df$predictor <- factor(binary_omnibus_df$predictor, levels = predictor_order)
binary_omnibus_df <- binary_omnibus_df[order(binary_omnibus_df$predictor), , drop = FALSE]

cat("\nBINARY PHASE LOGIT RESULTS\n")
print(binary_results_df[, c(
  "predictor",
  "term",
  "estimate",
  "std_error",
  "z_value",
  "p_value",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "lr_stat",
  "lr_df",
  "lr_p_value",
  "mcfadden_r2",
  "n"
)])

cat("\nBINARY PHASE OMNIBUS LR TESTS\n")
print(binary_omnibus_df)

binary_results_summary <- data.frame(
  Predictor    = as.character(binary_results_df$predictor),
  Term         = binary_results_df$term,
  Estimate     = format_num(binary_results_df$estimate, 3),
  Std_Error    = format_num(binary_results_df$std_error, 3),
  z_value      = format_num(binary_results_df$z_value, 3),
  p_value      = format_p(binary_results_df$p_value),
  Odds_Ratio   = format_num(binary_results_df$odds_ratio, 3),
  CI_Lower     = format_num(binary_results_df$ci_low, 3),
  CI_Upper     = format_num(binary_results_df$ci_high, 3),
  LR_stat      = format_num(binary_results_df$lr_stat, 3),
  LR_df        = binary_results_df$lr_df,
  LR_p_value   = format_p(binary_results_df$lr_p_value),
  McFadden_R2  = format_num(binary_results_df$mcfadden_r2, 3),
  n            = binary_results_df$n,
  stringsAsFactors = FALSE
)

binary_omnibus_summary <- data.frame(
  Predictor    = as.character(binary_omnibus_df$predictor),
  Outcome      = binary_omnibus_df$outcome,
  LR_stat      = format_num(binary_omnibus_df$lr_stat, 3),
  LR_df        = binary_omnibus_df$lr_df,
  LR_p_value   = format_p(binary_omnibus_df$lr_p_value),
  McFadden_R2  = format_num(binary_omnibus_df$mcfadden_r2, 3),
  n            = binary_omnibus_df$n,
  stringsAsFactors = FALSE
)

cat("\nBINARY PHASE COEFFICIENT TABLE (FORMATTED)\n")
print(binary_results_summary)

cat("\nBINARY PHASE OMNIBUS TABLE (FORMATTED)\n")
print(binary_omnibus_summary)

# ============================================================
# SAVE OUTPUTS
# ============================================================

write.csv(phase_5_counts, file.path(out_dir, "iaea_phase_5_counts.csv"), row.names = FALSE)
write.csv(phase_binary_counts, file.path(out_dir, "iaea_phase_binary_counts.csv"), row.names = FALSE)
write.csv(predictor_counts, file.path(out_dir, "iaea_phase_predictor_counts.csv"), row.names = FALSE)
write.csv(phase5_results_df, file.path(out_dir, "iaea_phase_5_ordinal_logit_raw.csv"), row.names = FALSE)
write.csv(phase5_results_summary, file.path(out_dir, "iaea_phase_5_ordinal_logit_summary.csv"), row.names = FALSE)
write.csv(phase5_omnibus_df, file.path(out_dir, "iaea_phase_5_ordinal_logit_omnibus_raw.csv"), row.names = FALSE)
write.csv(phase5_omnibus_summary, file.path(out_dir, "iaea_phase_5_ordinal_logit_omnibus_summary.csv"), row.names = FALSE)
write.csv(binary_results_df, file.path(out_dir, "iaea_phase_binary_logit_raw.csv"), row.names = FALSE)
write.csv(binary_results_summary, file.path(out_dir, "iaea_phase_binary_logit_summary.csv"), row.names = FALSE)
write.csv(binary_omnibus_df, file.path(out_dir, "iaea_phase_binary_logit_omnibus_raw.csv"), row.names = FALSE)
write.csv(binary_omnibus_summary, file.path(out_dir, "iaea_phase_binary_logit_omnibus_summary.csv"), row.names = FALSE)