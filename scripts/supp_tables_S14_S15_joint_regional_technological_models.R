
# =============================================================================
# SUPPLEMENTARY TABLES S14 AND S15
# JOINT REGIONAL AND TECHNOLOGICAL ASSOCIATIONS
# NEA SMR Dashboard, Edition 4
#
# Input:
# revision/data/edition_4_regional_technology_source_data.xlsx
#
# Outputs:
# revision/figures/supp_tables_S14_S15_joint_models_results.xlsx
# revision/figures/supp_tables_S14_S15_joint_models_forest.png
# revision/figures/supp_tables_S14_S15_joint_models_forest.pdf
# =============================================================================

# 1. PACKAGES -----------------------------------------------------------------

packages <- c(
  "readxl", "dplyr", "ggplot2", "ordinal",
  "openxlsx", "stringr", "tibble", "cowplot"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})


# 2. REPOSITORY PATHS ---------------------------------------------------------

input_relative <- file.path(
  "revision", "data",
  "edition_4_regional_technology_source_data.xlsx"
)

find_repo_root <- function(start = getwd()) {
  
  override <- Sys.getenv("SMR_REPO_ROOT", unset = "")
  
  if (nzchar(override)) {
    root <- normalizePath(override, winslash = "/", mustWork = TRUE)
    
    if (!file.exists(file.path(root, input_relative))) {
      stop("Input workbook not found under SMR_REPO_ROOT.")
    }
    
    return(root)
  }
  
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  repeat {
    
    if (file.exists(file.path(current, input_relative))) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop("Cannot locate input workbook: ", input_relative)
    }
    
    current <- parent
  }
}

repo_root <- find_repo_root()

input_file <- file.path(repo_root, input_relative)

output_dir <- file.path(
  repo_root, "revision", "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_prefix <- file.path(
  output_dir,
  "supp_tables_S14_S15_joint_models"
)


# 3. IMPORT -------------------------------------------------------------------

sheets <- readxl::excel_sheets(input_file)

sheet_name <- if ("Source data" %in% sheets) {
  "Source data"
} else {
  sheets[1]
}

raw <- readxl::read_excel(input_file, sheet = sheet_name)

names(raw) <- gsub(
  "_$", "",
  gsub("[^a-z0-9]+", "_", tolower(names(raw)))
)

if (
  !"neutron_spectrum" %in% names(raw) &&
  "neturon_spectrum" %in% names(raw)
) {
  raw$neutron_spectrum <- raw$neturon_spectrum
}

required <- c(
  "design", "region_cn_ru", "microreactor",
  "neutron_spectrum", "fuel_type", "fuel_enrichment",
  "financing", "supply_chain", "fuel_supply",
  "licensing", "siting"
)

missing_columns <- setdiff(required, names(raw))

if (length(missing_columns)) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (
  !"water_cooled" %in% names(raw) &&
  !"cooling_type" %in% names(raw)
) {
  stop("Missing water-cooled or cooling-type variable.")
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

if (anyDuplicated(toupper(trimws(as.character(raw$design))))) {
  stop("Duplicate reactor design identifiers detected.")
}

if (nrow(raw) != 91) {
  stop("Expected 91 Edition 4 designs; found ", nrow(raw))
}


# 4. CLEANING FUNCTIONS -------------------------------------------------------

clean_text <- function(x) {
  
  z <- stringr::str_squish(
    gsub("\u00a0", " ", as.character(x), fixed = TRUE)
  )
  
  z[z %in% c("", "NA", "N/A")] <- NA_character_
  
  z
}

clean_binary <- function(x) {
  
  z <- toupper(clean_text(x))
  
  dplyr::case_when(
    z %in% c("TRUE", "T", "1", "YES") ~ TRUE,
    z %in% c("FALSE", "F", "0", "NO") ~ FALSE,
    TRUE ~ NA
  )
}


# 5. REGIONAL HARMONISATION ---------------------------------------------------

region_raw <- tolower(clean_text(raw$region_cn_ru))

region <- dplyr::case_when(
  region_raw %in% c("north_america", "north america") ~
    "North America",
  region_raw == "europe" ~ "Europe",
  region_raw %in% c("oecd_asia", "oecd asia") ~
    "OECD Asia",
  region_raw %in% c("china/russia", "china_russia") ~
    "China/Russia",
  region_raw %in% c("row", "rest of world", "rest_of_world") ~
    "Rest of world",
  TRUE ~ NA_character_
)

if (anyNA(region)) {
  stop(
    "Unrecognised region values: ",
    paste(unique(raw$region_cn_ru[is.na(region)]), collapse = "; ")
  )
}


# 6. TECHNOLOGICAL HARMONISATION ----------------------------------------------

if ("water_cooled" %in% names(raw)) {
  
  water <- clean_binary(raw$water_cooled)
  
} else {
  
  cooling_raw <- tolower(clean_text(raw$cooling_type))
  
  water <- dplyr::case_when(
    cooling_raw == "water" ~ TRUE,
    !is.na(cooling_raw) ~ FALSE,
    TRUE ~ NA
  )
}

df <- raw %>%
  mutate(
    design = clean_text(design),
    region = region,
    microreactor = clean_binary(microreactor),
    
    spectrum = case_when(
      tolower(clean_text(neutron_spectrum)) == "thermal" ~ "Thermal",
      tolower(clean_text(neutron_spectrum)) == "fast" ~ "Fast",
      TRUE ~ NA_character_
    ),
    
    fuel_type = case_when(
      tolower(clean_text(fuel_type)) == "uranium" ~ "Uranium",
      tolower(clean_text(fuel_type)) == "triso" ~ "TRISO",
      tolower(clean_text(fuel_type)) == "other" ~ "Other",
      TRUE ~ NA_character_
    ),
    
    enrichment = case_when(
      toupper(clean_text(fuel_enrichment)) == "LEU" ~ "LEU",
      toupper(clean_text(fuel_enrichment)) == "HALEU" ~ "HALEU",
      tolower(clean_text(fuel_enrichment)) %in%
        c("not_required", "not required") ~ "Not required",
      TRUE ~ NA_character_
    ),
    
    water_cooled = water,
    
    cooling = case_when(
      water_cooled == TRUE ~ "Water",
      water_cooled == FALSE ~ "Non-water",
      TRUE ~ NA_character_
    ),
    
    across(
      all_of(c(
        "financing", "supply_chain", "fuel_supply",
        "licensing", "siting"
      )),
      ~ suppressWarnings(as.numeric(as.character(.x)))
    )
  )

if (anyNA(df$microreactor)) {
  stop("Unrecognised microreactor classifications.")
}

if (sum(!df$microreactor) != 53) {
  stop("Expected 53 conventional SMRs.")
}


# 7. REGRESSION SAMPLES -------------------------------------------------------

region_audit <- df %>%
  count(region, microreactor, name = "n")

df <- df %>%
  filter(
    region %in% c(
      "North America", "Europe",
      "OECD Asia", "China/Russia"
    )
  ) %>%
  mutate(
    region = factor(
      region,
      levels = c(
        "North America", "Europe",
        "OECD Asia", "China/Russia"
      )
    ),
    cooling = factor(
      cooling,
      levels = c("Water", "Non-water")
    ),
    spectrum = factor(
      spectrum,
      levels = c("Thermal", "Fast")
    ),
    fuel_type = factor(
      fuel_type,
      levels = c("Uranium", "TRISO", "Other")
    ),
    enrichment = factor(
      enrichment,
      levels = c("LEU", "HALEU", "Not required")
    )
  )

samples <- list(
  "All designs" = df,
  "Conventional SMRs" = df %>%
    filter(microreactor == FALSE)
)

outcomes <- c(
  "financing", "supply_chain", "fuel_supply",
  "licensing", "siting"
)

outcome_labels <- c(
  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  licensing = "Licensing",
  siting = "Siting"
)

predictors <- c(
  "region", "cooling", "spectrum",
  "fuel_type", "enrichment"
)

models <- list(
  "M1 Region" = "region",
  "M2 Technology" = c(
    "cooling", "spectrum", "fuel_type", "enrichment"
  ),
  "M3 Combined" = predictors
)


# 8. MODEL FITTING ------------------------------------------------------------

fit_one <- function(dat, outcome, terms, sample_name, model_name) {
  
  diagnostic <- data.frame(
    sample = sample_name,
    outcome = outcome,
    model = model_name,
    n = nrow(dat),
    rank = NA_integer_,
    columns = NA_integer_,
    convergence_code = NA_integer_,
    hessian_condition = NA_real_,
    warnings = "",
    status = "Unavailable",
    stringsAsFactors = FALSE
  )
  
  formula <- stats::reformulate(terms, response = "y")
  
  xmat <- tryCatch(
    stats::model.matrix(
      stats::reformulate(terms),
      data = dat
    ),
    error = function(e) e
  )
  
  if (inherits(xmat, "error")) {
    diagnostic$status <- paste(
      "Design matrix error:",
      conditionMessage(xmat)
    )
    return(list(fit = NULL, diagnostic = diagnostic))
  }
  
  diagnostic$rank <- qr(xmat)$rank
  diagnostic$columns <- ncol(xmat)
  
  insufficient_levels <- any(
    vapply(
      dat[terms],
      function(x) {
        is.factor(x) && nlevels(droplevels(x)) < 2
      },
      logical(1)
    )
  )
  
  if (
    nrow(dat) < 20 ||
    diagnostic$rank < diagnostic$columns ||
    insufficient_levels
  ) {
    diagnostic$status <- if (nrow(dat) < 20) {
      "Insufficient complete cases"
    } else {
      "Rank deficient or one-level predictor"
    }
    
    return(list(fit = NULL, diagnostic = diagnostic))
  }
  
  warnings <- character()
  
  fit <- tryCatch(
    withCallingHandlers(
      ordinal::clm(
        formula,
        data = dat,
        link = "logit",
        Hess = TRUE,
        control = ordinal::clm.control(maxIter = 200)
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  diagnostic$warnings <- paste(
    unique(warnings),
    collapse = " | "
  )
  
  if (inherits(fit, "error")) {
    diagnostic$status <- paste(
      "Fit error:",
      conditionMessage(fit)
    )
    return(list(fit = NULL, diagnostic = diagnostic))
  }
  
  cv <- fit$convergence
  
  code <- suppressWarnings(
    as.integer(
      if (is.list(cv)) cv[[1L]] else cv[1L]
    )
  )
  
  if (length(code) != 1L || is.na(code)) {
    code <- NA_integer_
  }
  
  diagnostic$convergence_code <- code
  
  diagnostic$hessian_condition <- if (
    length(fit$cond.H) == 1L
  ) {
    as.numeric(fit$cond.H)
  } else {
    NA_real_
  }
  
  finite_beta <- all(is.finite(fit$beta))
  
  diagnostic$status <- if (
    !is.na(code) &&
    code == 0L &&
    is.finite(diagnostic$hessian_condition) &&
    diagnostic$hessian_condition < 1e8 &&
    finite_beta &&
    length(warnings) == 0L
  ) {
    "OK"
  } else {
    "Review diagnostics"
  }
  
  list(fit = fit, diagnostic = diagnostic)
}


# 9. RUN REGRESSIONS ----------------------------------------------------------

objects <- list()
diagnostics <- list()
fits <- list()
coefficients <- list()
comparisons <- list()
sample_audit <- list()

keyfun <- function(s, o, m) {
  paste(s, o, m, sep = " | ")
}

for (s in names(samples)) {
  
  for (o in outcomes) {
    
    source <- samples[[s]]
    
    needed <- c(o, predictors)
    
    complete <- stats::complete.cases(
      source[, needed, drop = FALSE]
    )
    
    dat <- droplevels(
      source[complete, , drop = FALSE]
    )
    
    if (any(
      !is.na(dat[[o]]) &
      !(dat[[o]] %in% 1:6)
    )) {
      stop("Unexpected readiness score: ", o)
    }
    
    dat$y <- ordered(dat[[o]], levels = 1:6)
    
    sample_audit[[paste(s, o)]] <- data.frame(
      sample = s,
      outcome = o,
      n_region_sample = nrow(source),
      n_complete = nrow(dat),
      n_excluded_missing = nrow(source) - nrow(dat)
    )
    
    for (m in names(models)) {
      
      result <- fit_one(
        dat, o, models[[m]], s, m
      )
      
      k <- keyfun(s, o, m)
      
      objects[[k]] <- result
      diagnostics[[k]] <- result$diagnostic
      
      fit <- result$fit
      
      if (is.null(fit)) next
      
      fits[[k]] <- data.frame(
        sample = s,
        outcome = o,
        model = m,
        n = nrow(dat),
        logLik = as.numeric(logLik(fit)),
        AIC = AIC(fit),
        BIC = BIC(fit),
        convergence_code =
          result$diagnostic$convergence_code,
        hessian_condition =
          result$diagnostic$hessian_condition,
        status = result$diagnostic$status
      )
      
      mat <- coef(summary(fit))
      
      for (term in names(fit$beta)) {
        
        if (!term %in% rownames(mat)) next
        
        beta <- unname(mat[term, "Estimate"])
        se <- unname(mat[term, "Std. Error"])
        z <- beta / se
        
        coefficients[[paste(k, term)]] <- data.frame(
          sample = s,
          outcome = o,
          model = m,
          term = term,
          n = nrow(dat),
          beta = beta,
          se = se,
          odds_ratio = exp(beta),
          ci_low = exp(beta - 1.96 * se),
          ci_high = exp(beta + 1.96 * se),
          z = z,
          p_value = 2 * pnorm(
            abs(z),
            lower.tail = FALSE
          ),
          status = result$diagnostic$status
        )
      }
    }
    
    for (comparison in c(
      "Adding technology to region",
      "Adding region to technology"
    )) {
      
      smaller <- if (
        comparison == "Adding technology to region"
      ) {
        "M1 Region"
      } else {
        "M2 Technology"
      }
      
      a <- objects[[keyfun(s, o, smaller)]]
      b <- objects[[keyfun(s, o, "M3 Combined")]]
      
      status <- "OK"
      statistic <- NA_real_
      df_difference <- NA_real_
      p <- NA_real_
      
      if (is.null(a$fit) || is.null(b$fit)) {
        
        status <- "Model unavailable"
        
      } else if (
        a$diagnostic$status != "OK" ||
        b$diagnostic$status != "OK"
      ) {
        
        status <- "Review diagnostics"
        
      } else {
        
        statistic <- 2 * (
          as.numeric(logLik(b$fit)) -
            as.numeric(logLik(a$fit))
        )
        
        df_difference <-
          attr(logLik(b$fit), "df") -
          attr(logLik(a$fit), "df")
        
        if (
          df_difference <= 0 ||
          statistic < -1e-6
        ) {
          
          status <- "Invalid nested comparison"
          
        } else {
          
          p <- pchisq(
            max(statistic, 0),
            df = df_difference,
            lower.tail = FALSE
          )
        }
      }
      
      comparisons[[paste(s, o, comparison)]] <- data.frame(
        sample = s,
        outcome = o,
        comparison = comparison,
        LR = statistic,
        df = df_difference,
        p_value = p,
        status = status
      )
    }
  }
}


# 10. CONSOLIDATE RESULTS -----------------------------------------------------

diagnostics_df <- dplyr::bind_rows(diagnostics)
fit_df <- dplyr::bind_rows(fits)
coef_df <- dplyr::bind_rows(coefficients)
sample_df <- dplyr::bind_rows(sample_audit)

lr_df <- dplyr::bind_rows(comparisons) %>%
  group_by(sample, comparison) %>%
  mutate(
    p_holm = {
      result <- rep(NA_real_, n())
      valid <- !is.na(p_value)
      
      if (any(valid)) {
        result[valid] <- p.adjust(
          p_value[valid],
          method = "holm",
          n = n()
        )
      }
      
      result
    }
  ) %>%
  ungroup()

if (nrow(coef_df) == 0) {
  coef_df <- data.frame(
    sample = character(),
    outcome = character(),
    model = character(),
    term = character(),
    n = integer(),
    beta = numeric(),
    se = numeric(),
    odds_ratio = numeric(),
    ci_low = numeric(),
    ci_high = numeric(),
    z = numeric(),
    p_value = numeric(),
    status = character()
  )
}


# 11. SPARSE-CELL AUDIT -------------------------------------------------------

cross_tabs <- list()

for (s in names(samples)) {
  
  for (pair in combn(predictors, 2, simplify = FALSE)) {
    
    tab <- as.data.frame(
      table(
        samples[[s]][[pair[1]]],
        samples[[s]][[pair[2]]]
      ),
      stringsAsFactors = FALSE
    )
    
    names(tab) <- c("category_1", "category_2", "n")
    
    tab$sample <- s
    tab$variable_1 <- pair[1]
    tab$variable_2 <- pair[2]
    
    cross_tabs[[
      paste(s, paste(pair, collapse = "_"))
    ]] <- tab
  }
}

cross_df <- dplyr::bind_rows(cross_tabs)


# 12. COMBINED-MODEL FOREST PLOT ----------------------------------------------

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
  "China/Russia", "OECD Asia", "Europe",
  "Non-water-cooled", "Fast spectrum",
  "TRISO fuel", "Other fuel",
  "HALEU", "Enrichment not required"
)

plot_df <- coef_df %>%
  filter(
    model == "M3 Combined",
    status == "OK",
    is.finite(odds_ratio),
    is.finite(ci_low),
    is.finite(ci_high),
    odds_ratio > 0,
    ci_low > 0,
    ci_high > 0
  ) %>%
  mutate(
    label = unname(term_labels[term]),
    label = ifelse(is.na(label), term, label),
    label = factor(label, levels = rev(plot_order)),
    outcome = factor(
      outcome,
      levels = outcomes,
      labels = unname(outcome_labels[outcomes])
    )
  )

make_forest <- function(sample_name) {
  
  dat <- plot_df %>%
    filter(sample == sample_name)
  
  if (nrow(dat) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(
          title = paste(
            sample_name, "- no valid combined models"
          )
        )
    )
  }
  
  ggplot(dat, aes(x = odds_ratio, y = label)) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.3
    ) +
    geom_segment(
      aes(
        x = ci_low,
        xend = ci_high,
        yend = label
      ),
      colour = "#4C78A8",
      linewidth = 0.4
    ) +
    geom_point(
      colour = "#4C78A8",
      size = 1.6
    ) +
    scale_x_log10() +
    facet_wrap(~outcome, ncol = 3) +
    labs(
      title = sample_name,
      x = "Adjusted odds ratio (log scale)",
      y = NULL
    ) +
    theme_classic(base_size = 7) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      strip.text = element_text(
        face = "bold",
        size = 8
      ),
      axis.line = element_line(
        colour = "grey35",
        linewidth = 0.3
      ),
      panel.spacing = grid::unit(8, "mm")
    )
}

combined <- cowplot::plot_grid(
  make_forest("All designs"),
  make_forest("Conventional SMRs"),
  ncol = 1,
  labels = c("a", "b"),
  label_size = 11,
  label_fontface = "bold"
)

png_file <- paste0(output_prefix, "_forest.png")
pdf_file <- paste0(output_prefix, "_forest.pdf")

ggplot2::ggsave(
  png_file,
  combined,
  width = 280,
  height = 230,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  pdf_file,
  combined,
  width = 280,
  height = 230,
  units = "mm",
  bg = "white"
)


# 13. EXCEL OUTPUT ------------------------------------------------------------

wb <- openxlsx::createWorkbook()

write_sheet <- function(name, data) {
  
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, data)
  
  if (ncol(data) > 0) {
    
    openxlsx::addStyle(
      wb,
      name,
      openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#DCE6F1"
      ),
      rows = 1,
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      wb, name, firstRow = TRUE
    )
    
    openxlsx::setColWidths(
      wb,
      name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }
}

write_sheet("Model diagnostics", diagnostics_df)
write_sheet("Model fit", fit_df)
write_sheet("Adjusted odds ratios", coef_df)
write_sheet("LR comparisons", lr_df)
write_sheet("Sample audit", sample_df)
write_sheet("Region audit", region_audit)
write_sheet("Cross tabulations", cross_df)
write_sheet("Design audit", df)

excel_file <- paste0(output_prefix, "_results.xlsx")

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# 14. SUMMARY -----------------------------------------------------------------

cat("\n====================================================\n")
cat("SUPPLEMENTARY TABLES S14 AND S15\n")
cat("JOINT REGIONAL AND TECHNOLOGICAL MODELS\n")
cat("====================================================\n")

cat("\nREGRESSION SAMPLE SIZES:\n")
print(vapply(samples, nrow, integer(1)))

cat("\nMODEL DIAGNOSTICS:\n")
print(diagnostics_df, row.names = FALSE)

cat("\nLIKELIHOOD-RATIO COMPARISONS:\n")
print(lr_df, row.names = FALSE)

cat("\nOUTPUT FILES:\n")
cat(excel_file, "\n")
cat(png_file, "\n")
cat(pdf_file, "\n")

cat("\nAnalysis complete.\n")
