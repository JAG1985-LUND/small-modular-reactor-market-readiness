# Supplementary Table S16 | US versus non-US licensing readiness
# NEA SMR Dashboard, Edition 4
# Run from any directory inside the repository, or set SMR_REPO_ROOT.
# Input:  revision/data/us_licensing_regression_check.xlsx
# Output: revision/figures/table_S16_us_licensing_robustness_results.xlsx


required <- c('readxl', 'dplyr', 'tidyr', 'MASS', 'openxlsx')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Install required R packages: ', paste(missing, collapse = ', '))

find_root <- function() {
  override <- Sys.getenv('SMR_REPO_ROOT', unset = '')
  starts <- unique(c(if (nzchar(override)) override, getwd()))
  for (start in starts) {
    p <- normalizePath(start, winslash = '/', mustWork = FALSE)
    repeat {
      if (dir.exists(file.path(p, 'revision', 'data')) &&
          dir.exists(file.path(p, 'revision', 'scripts'))) return(p)
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
  }
  stop('Repository root not found. Run within the repository or set SMR_REPO_ROOT.')
}
root <- find_root()
input <- file.path(root, 'revision', 'data', 'us_licensing_regression_check.xlsx')
out_dir <- file.path(root, 'revision', 'figures')
out_file <- file.path(out_dir, 'table_S16_us_licensing_robustness_results.xlsx')
if (!file.exists(input)) stop('Missing input: ', input)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sheets <- readxl::excel_sheets(input)
sheet <- if ('nea_50' %in% sheets) 'nea_50' else sheets[[1]]
raw <- as.data.frame(readxl::read_excel(input, sheet = sheet))
names(raw) <- trimws(names(raw))
find_col <- function(options) {
  hit <- intersect(options, names(raw))
  if (!length(hit)) stop('Missing column; expected one of: ', paste(options, collapse = ', '))
  hit[[1]]
}
country <- find_col(c('country', 'country_code', 'designer_country', 'developer_country'))
micro <- find_col(c('microreactor', 'micro_reactor', 'classification'))
columns <- list(
  licensing = c('licensing', 'licensing_readiness', 'licensing_score'),
  financing = c('financing', 'financing_readiness', 'financing_score'),
  supply_chain = c('supply_chain', 'supply_chain_readiness', 'supply_chain_score'),
  fuel_supply = c('fuel_supply', 'fuel_supply_readiness', 'fuel_supply_score'),
  siting = c('siting', 'siting_readiness', 'siting_score'),
  stakeholder_engagement = c('stakeholder_engagement', 'stakeholder_engagement_readiness',
                             'stakeholder_engagement_score')
)
resolved <- vapply(columns, find_col, character(1))
normalise <- function(x) tolower(trimws(gsub('\\s+', ' ', as.character(x))))
dat <- raw
if ('edition' %in% names(dat)) {
  dat <- dat[suppressWarnings(as.numeric(as.character(dat$edition))) == 4 &
               !is.na(dat$edition), , drop = FALSE]
}
design_col <- intersect(c('design', 'smr_design'), names(dat))
if (length(design_col)) {
  names_ <- toupper(trimws(as.character(dat[[design_col[[1]]]])))
  dat <- dat[!is.na(names_) & nzchar(names_) & names_ != 'DF-CDE', , drop = FALSE]
}
country_values <- normalise(dat[[country]])
micro_values <- normalise(dat[[micro]])
us_values <- c('united states', 'united states of america', 'usa', 'us', 'u.s.', 'u.s.a.')
if (any(is.na(country_values) | country_values %in% c('', 'na', 'n/a'))) {
  stop('Missing country classifications; inspect source workbook.')
}
dat$us_design <- country_values %in% us_values
micro_true <- c('true', 't', 'yes', 'y', '1', 'microreactor', 'micro_reactor', 'micro reactor')
micro_false <- c('false', 'f', 'no', 'n', '0', 'smr', 'conventional',
                 'conventional smr', 'small modular reactor')
unknown <- !micro_values %in% c(micro_true, micro_false)
if (any(unknown)) stop('Unrecognised reactor classifications: ',
                       paste(unique(micro_values[unknown]), collapse = ', '))
dat$is_microreactor <- micro_values %in% micro_true
for (v in names(resolved)) {
  dat[[v]] <- suppressWarnings(as.numeric(as.character(dat[[resolved[[v]]]])))
  if (any(!is.na(dat[[v]]) & !dat[[v]] %in% 1:6)) stop('Invalid 1-6 scores: ', v)
}
other_five <- c('financing', 'supply_chain', 'fuel_supply', 'siting', 'stakeholder_engagement')
dat$other_five_complete <- complete.cases(dat[, other_five, drop = FALSE])
dat$non_licensing_readiness <- NA_real_
dat$non_licensing_readiness[dat$other_five_complete] <-
  rowMeans(dat[dat$other_five_complete, other_five, drop = FALSE])
dat$us_group <- factor(ifelse(dat$us_design, 'United States', 'Non-US'),
                       levels = c('Non-US', 'United States'))
dat$adjusted_model_complete <- !is.na(dat$licensing) & !is.na(dat$non_licensing_readiness)
dat$sample_inclusion <- ifelse(dat$is_microreactor, 'All designs only', 'Both samples')
samples <- list('All designs' = dat,
                'Conventional SMRs' = dat[!dat$is_microreactor, , drop = FALSE])
expected <- c('All designs' = 91L, 'Conventional SMRs' = 53L)
for (s in names(samples)) {
  if (nrow(samples[[s]]) != expected[[s]])
    stop(s, ': expected ', expected[[s]], ' designs; found ', nrow(samples[[s]]),
         '. Check input and filtering before interpreting results.')
}

fit_sample <- function(df, sample_name) {
  tmp <- df[df$adjusted_model_complete, , drop = FALSE]
  if (!nrow(tmp) || length(unique(tmp$us_group)) != 2L)
    stop('Missing model cases or geographic group in ', sample_name)
  if (length(unique(tmp$licensing)) < 2L) stop('Insufficient outcome variation: ', sample_name)
  tmp$licensing_ordinal <- ordered(tmp$licensing, levels = 1:6)
  formulas <- list(Unadjusted = licensing_ordinal ~ us_group,
                   Adjusted = licensing_ordinal ~ us_group + non_licensing_readiness)
  warning_log <- character()
  fit_polr <- function(formula) withCallingHandlers(
    MASS::polr(formula, data = tmp, method = 'logistic', Hess = TRUE,
               control = list(maxit = 2000)),
    warning = function(w) {
      warning_log <<- c(warning_log, conditionMessage(w))
      invokeRestart('muffleWarning')
    }
  )
  null <- fit_polr(licensing_ordinal ~ 1)
  fits <- lapply(formulas, fit_polr)
  estimates <- lapply(names(fits), function(model) {
    fit <- fits[[model]]
    tab <- coef(summary(fit))
    terms <- names(coef(fit))
    beta <- unname(coef(fit)[terms])
    se <- tab[terms, 'Std. Error']
    lr <- max(0, 2 * (as.numeric(logLik(fit)) - as.numeric(logLik(null))))
    df_lr <- attr(logLik(fit), 'df') - attr(logLik(null), 'df')
    data.frame(sample = sample_name, model = model, predictor = terms,
               n_total = nrow(tmp), n_us = sum(tmp$us_design), n_non_us = sum(!tmp$us_design),
               coefficient = beta, standard_error = se, odds_ratio = exp(beta),
               ci_low_95 = exp(beta - 1.96 * se), ci_high_95 = exp(beta + 1.96 * se),
               p_wald = 2 * pnorm(abs(beta / se), lower.tail = FALSE),
               lr_statistic = lr, lr_df = df_lr, p_lr = pchisq(lr, df_lr, lower.tail = FALSE),
               mcfadden_r2 = 1 - as.numeric(logLik(fit)) / as.numeric(logLik(null)))
  })
  diagnostics <- lapply(names(fits), function(model) {
    fit <- fits[[model]]
    data.frame(sample = sample_name, model = model, n = nrow(tmp),
               convergence_code = fit$convergence,
               max_abs_coefficient = max(abs(coef(fit))),
               max_standard_error = max(coef(summary(fit))[names(coef(fit)), 'Std. Error']),
               warnings = paste(unique(warning_log), collapse = ' | '))
  })
  lr_change <- max(0, 2 * (as.numeric(logLik(fits$Adjusted)) -
                             as.numeric(logLik(fits$Unadjusted))))
  comparison <- data.frame(sample = sample_name, n = nrow(tmp),
                           lr_change = lr_change, df = 1L,
                           p_change = pchisq(lr_change, 1, lower.tail = FALSE))
  list(estimates = dplyr::bind_rows(estimates),
       diagnostics = dplyr::bind_rows(diagnostics), comparison = comparison)
}
models <- Map(fit_sample, samples, names(samples))
regression <- dplyr::bind_rows(lapply(models, `[[`, 'estimates'))
diagnostics <- dplyr::bind_rows(lapply(models, `[[`, 'diagnostics'))
comparison <- dplyr::bind_rows(lapply(models, `[[`, 'comparison'))
us_effect <- regression[regression$predictor == 'us_groupUnited States', , drop = FALSE]
us_effect_wide <- tidyr::pivot_wider(
  us_effect[, c('sample', 'model', 'odds_ratio', 'ci_low_95', 'ci_high_95', 'p_wald')],
  names_from = 'model', values_from = c('odds_ratio', 'ci_low_95', 'ci_high_95', 'p_wald'),
  names_sep = '_')
us_effect_wide$or_ratio_adjusted_to_unadjusted <-
  us_effect_wide$odds_ratio_Adjusted / us_effect_wide$odds_ratio_Unadjusted
us_effect_wide$log_or_change_pct <- 100 *
  (log(us_effect_wide$odds_ratio_Adjusted) - log(us_effect_wide$odds_ratio_Unadjusted)) /
  log(us_effect_wide$odds_ratio_Unadjusted)

descriptive <- dplyr::bind_rows(lapply(names(samples), function(s) {
  df <- samples[[s]]
  df <- dplyr::group_by(df, us_group)
  out <- dplyr::summarise(df, n = dplyr::n(),
                          licensing_mean = mean(licensing, na.rm = TRUE),
                          licensing_median = median(licensing, na.rm = TRUE),
                          licensing_sd = sd(licensing, na.rm = TRUE),
                          non_licensing_mean = mean(non_licensing_readiness, na.rm = TRUE),
                          non_licensing_median = median(non_licensing_readiness, na.rm = TRUE),
                          non_licensing_sd = sd(non_licensing_readiness, na.rm = TRUE), .groups = 'drop')
  dplyr::mutate(out, sample = s, .before = 1)
}))
distributions <- dplyr::bind_rows(lapply(names(samples), function(s) {
  df <- samples[[s]]
  df <- df[!is.na(df$licensing), , drop = FALSE]
  out <- dplyr::count(df, us_group, licensing, name = 'n')
  out <- tidyr::complete(out, us_group, licensing = 1:6, fill = list(n = 0))
  out <- dplyr::group_by(out, us_group)
  out <- dplyr::mutate(out, share = n / sum(n))
  dplyr::mutate(dplyr::ungroup(out), sample = s, .before = 1)
}))
audit_cols <- unique(c(intersect(c('design', 'smr_design', 'country', 'country_code',
                                   'classification', 'microreactor', 'micro_reactor'), names(dat)),
                       'us_group', 'is_microreactor', names(columns),
                       'non_licensing_readiness', 'other_five_complete',
                       'adjusted_model_complete', 'sample_inclusion'))
audit <- dat[, audit_cols, drop = FALSE]

sheets_out <- list('US effect summary' = us_effect, 'OR change' = us_effect_wide,
                   'All coefficients' = regression, 'Model comparison' = comparison,
                   'Descriptive statistics' = descriptive, 'Licensing distributions' = distributions,
                   'Model diagnostics' = diagnostics, 'Design audit' = audit)
wb <- openxlsx::createWorkbook()
header <- openxlsx::createStyle(textDecoration = 'bold', fgFill = '#DCE6F1')
for (nm in names(sheets_out)) {
  openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, nm, as.data.frame(sheets_out[[nm]]),
                      headerStyle = header, withFilter = nrow(sheets_out[[nm]]) > 0)
  openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(sheets_out[[nm]])), widths = 'auto')
  openxlsx::freezePane(wb, nm, firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
cat('\nS16 US effects:\n'); print(us_effect[, c('sample', 'model', 'n_total',
                                                'n_us', 'n_non_us', 'odds_ratio', 'ci_low_95', 'ci_high_95', 'p_wald')])
cat('\nNested-model tests:\n'); print(comparison)
cat('\nSaved: ', out_file, '\n', sep = '')
