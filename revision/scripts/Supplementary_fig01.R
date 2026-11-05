
# ================================================================
# SUPPLEMENTARY FIGURE S1
# Recurring versus newly included SMR designs
# OECD NEA SMR Dashboard, Edition 4
#
# Recurring: n = 62
# New:       n = 29
#
# INPUT:
# revision/data/nea_recurring_new_designs.xlsx
#
# OUTPUTS:
# revision/figures/supp_fig_S01_recurring_vs_new.png
# revision/figures/supp_fig_S01_recurring_vs_new.pdf
# revision/figures/supp_fig_S01_recurring_vs_new_results.xlsx
#
# Retains:
# - Original figure colours and layout
# - Six readiness indicators and overall readiness
# - Descriptive comparisons
# - Permutation tests and Hedges' g
# - Holm adjustment
# - Reactor-class and stratified comparisons
# - Fisher's exact test
# - HC3-robust regression models
# - Licensing-specific diagnostics
# - Twelve Excel worksheets
# ================================================================


# 1. PACKAGES ----------------------------------------------------

required <- c(
  "readxl", "dplyr", "tidyr", "ggplot2",
  "writexl", "broom", "sandwich", "lmtest"
)

missing_pkgs <- setdiff(
  required,
  rownames(installed.packages())
)

if (length(missing_pkgs)) {
  stop(
    "Please install: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

invisible(
  lapply(required, library, character.only = TRUE)
)


# 2. GITHUB-COMPATIBLE PATHS -------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "nea_recurring_new_designs.xlsx"
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
        "Cannot locate repository root. Expected: ",
        target_file
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

input <- file.path(ROOT_DIR, SOURCE_FILE)

outdir <- file.path(
  ROOT_DIR,
  "revision",
  "figures"
)

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_prefix <- "supp_fig_S01_recurring_vs_new"

png_file <- file.path(
  outdir,
  paste0(figure_prefix, ".png")
)

pdf_file <- file.path(
  outdir,
  paste0(figure_prefix, ".pdf")
)

excel_file <- file.path(
  outdir,
  paste0(figure_prefix, "_results.xlsx")
)

if (!file.exists(input)) {
  stop("Input workbook not found: ", input)
}


# 3. IMPORT DATA -------------------------------------------------

raw <- readxl::read_excel(input)

indicators <- c(
  "licensing",
  "siting",
  "financing",
  "supply_chain",
  "fuel_supply",
  "stakeholder_engagement"
)

metrics <- c("overall", indicators)

needed <- c(
  "status",
  "design",
  "microreactor",
  indicators
)

stopifnot(all(needed %in% names(raw)))


# 4. CLEAN DATA --------------------------------------------------

as_micro <- function(x) {
  y <- tolower(trimws(as.character(x)))
  
  dplyr::case_when(
    y %in% c("true", "t", "1", "yes") ~ "Microreactor",
    y %in% c("false", "f", "0", "no") ~ "Conventional SMR",
    TRUE ~ NA_character_
  )
}

dat <- raw |>
  dplyr::mutate(
    status = tolower(trimws(as.character(status))),
    design = trimws(as.character(design)),
    reactor_class = as_micro(microreactor),
    dplyr::across(
      dplyr::all_of(indicators),
      as.numeric
    )
  ) |>
  dplyr::mutate(
    overall = rowMeans(
      dplyr::across(dplyr::all_of(indicators)),
      na.rm = FALSE
    ),
    status = factor(
      status,
      levels = c("recurring", "new")
    ),
    reactor_class = factor(
      reactor_class,
      levels = c(
        "Conventional SMR",
        "Microreactor"
      )
    )
  )


# 5. DATA QUALITY CHECKS -----------------------------------------

if (nrow(dat) != 91) {
  stop("Expected 91 Edition 4 designs.")
}

if (anyNA(dat$status) || anyNA(dat$reactor_class)) {
  stop("Invalid status or reactor classification.")
}

if (anyNA(dat[, indicators])) {
  stop("Missing readiness indicator scores.")
}

if (any(!as.matrix(dat[, indicators]) %in% 1:6)) {
  stop("Readiness scores must be integers from 1 to 6.")
}

if (
  anyNA(dat$design) ||
  anyDuplicated(tolower(dat$design))
) {
  stop("Missing or duplicate reactor design names.")
}

n_recurring <- sum(dat$status == "recurring")
n_new <- sum(dat$status == "new")

if (n_recurring != 62 || n_new != 29) {
  stop(
    "Expected 62 recurring and 29 new designs. Found ",
    n_recurring, " recurring and ", n_new, " new."
  )
}

four_s <- dat |>
  dplyr::filter(toupper(design) == "4S")

if (
  nrow(four_s) != 1 ||
  as.character(four_s$status[1]) != "recurring"
) {
  stop("4S must be classified as recurring.")
}

if ("mean_readiness" %in% names(raw)) {
  reported <- suppressWarnings(
    as.numeric(raw$mean_readiness)
  )
  
  bad <- which(
    is.na(reported) |
      abs(reported - dat$overall) > 0.011
  )
  
  if (length(bad)) {
    warning(
      "Inconsistent workbook mean_readiness at rows: ",
      paste(bad, collapse = ", ")
    )
  }
}


# 6. DESCRIPTIVE STATISTICS --------------------------------------

counts <- dat |>
  dplyr::count(
    status,
    reactor_class,
    name = "n"
  )

long <- dat |>
  dplyr::select(
    design,
    status,
    reactor_class,
    dplyr::all_of(metrics)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(metrics),
    names_to = "indicator",
    values_to = "score"
  )

summary_stats <- function(data, groups) {
  data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(groups))
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(score),
      sd = sd(score),
      median = median(score),
      q1 = unname(quantile(score, 0.25)),
      q3 = unname(quantile(score, 0.75)),
      min = min(score),
      max = max(score),
      .groups = "drop"
    )
}

descriptive_status <- summary_stats(
  long,
  c("indicator", "status")
)

descriptive_class <- summary_stats(
  long,
  c("indicator", "reactor_class")
)

descriptive_four_groups <- summary_stats(
  long,
  c("indicator", "status", "reactor_class")
)


# 7. PERMUTATION TEST FUNCTION -----------------------------------

set.seed(20261025)

B <- 19999L

perm_diff <- function(
    y,
    group,
    label_a,
    label_b,
    B = 19999L
) {
  keep <- !is.na(y) &
    !is.na(group) &
    as.character(group) %in% c(label_a, label_b)
  
  y <- y[keep]
  group <- as.character(group[keep])
  
  na <- sum(group == label_a)
  nb <- sum(group == label_b)
  
  if (na < 2 || nb < 2) {
    return(
      tibble::tibble(
        n_a = na,
        n_b = nb,
        mean_a = NA_real_,
        mean_b = NA_real_,
        difference = NA_real_,
        p_perm = NA_real_,
        hedges_g = NA_real_
      )
    )
  }
  
  obs <- mean(y[group == label_a]) -
    mean(y[group == label_b])
  
  perm <- replicate(
    B,
    {
      ix <- sample.int(length(y), na)
      mean(y[ix]) - mean(y[-ix])
    }
  )
  
  pooled_sd <- sqrt(
    (
      (na - 1) * var(y[group == label_a]) +
        (nb - 1) * var(y[group == label_b])
    ) /
      (na + nb - 2)
  )
  
  g <- if (pooled_sd > 0) {
    (obs / pooled_sd) *
      (1 - 3 / (4 * (na + nb) - 9))
  } else {
    NA_real_
  }
  
  tibble::tibble(
    n_a = na,
    n_b = nb,
    mean_a = mean(y[group == label_a]),
    mean_b = mean(y[group == label_b]),
    difference = obs,
    p_perm = (
      1 + sum(abs(perm) >= abs(obs) - 1e-12)
    ) / (B + 1),
    hedges_g = g
  )
}


# 8. RECURRING VERSUS NEW ----------------------------------------

status_tests <- dplyr::bind_rows(
  lapply(
    metrics,
    function(v) {
      dplyr::bind_cols(
        tibble::tibble(indicator = v),
        perm_diff(
          dat[[v]],
          dat$status,
          "recurring",
          "new",
          B
        )
      )
    }
  )
)

status_tests$p_holm_six <- NA_real_

indicator_rows <- status_tests$indicator != "overall"

status_tests$p_holm_six[indicator_rows] <-
  p.adjust(
    status_tests$p_perm[indicator_rows],
    method = "holm"
  )


# 9. REACTOR CLASS COMPARISONS -----------------------------------

class_tests <- dplyr::bind_rows(
  lapply(
    metrics,
    function(v) {
      dplyr::bind_cols(
        tibble::tibble(indicator = v),
        perm_diff(
          dat[[v]],
          dat$reactor_class,
          "Conventional SMR",
          "Microreactor",
          B
        )
      )
    }
  )
)

class_tests$p_holm_six <- NA_real_

class_rows <- class_tests$indicator != "overall"

class_tests$p_holm_six[class_rows] <-
  p.adjust(
    class_tests$p_perm[class_rows],
    method = "holm"
  )


# 10. STRATIFIED COMPARISONS -------------------------------------

stratified_tests <- dplyr::bind_rows(
  lapply(
    levels(dat$reactor_class),
    function(cl) {
      sub <- dat[
        dat$reactor_class == cl,
      ]
      
      dplyr::bind_cols(
        tibble::tibble(reactor_class = cl),
        perm_diff(
          sub$overall,
          sub$status,
          "recurring",
          "new",
          B
        )
      )
    }
  )
)


# 11. FISHER'S EXACT TEST ----------------------------------------

ct <- table(
  dat$status,
  dat$reactor_class
)

fisher_result <- fisher.test(ct)

composition_test <- tibble::tibble(
  test = "Fisher exact: status x reactor class",
  p_value = fisher_result$p.value,
  odds_ratio = unname(fisher_result$estimate),
  conf_low = fisher_result$conf.int[1],
  conf_high = fisher_result$conf.int[2]
)


# 12. ADJUSTED REGRESSION MODELS ---------------------------------

fit1 <- lm(
  overall ~ status,
  data = dat
)

fit2 <- lm(
  overall ~ status + reactor_class,
  data = dat
)

fit3 <- lm(
  overall ~ status * reactor_class,
  data = dat
)

fit_lic <- lm(
  licensing ~ status + reactor_class,
  data = dat
)

fit_eng <- lm(
  stakeholder_engagement ~ status + reactor_class,
  data = dat
)

robust_table <- function(fit, name) {
  ct <- lmtest::coeftest(
    fit,
    vcov. = sandwich::vcovHC(
      fit,
      type = "HC3"
    )
  )
  
  tibble::tibble(
    model = name,
    term = rownames(ct),
    estimate = ct[, 1],
    robust_se = ct[, 2],
    statistic = ct[, 3],
    p_value = ct[, 4],
    ci_low = ct[, 1] -
      qt(0.975, df.residual(fit)) * ct[, 2],
    ci_high = ct[, 1] +
      qt(0.975, df.residual(fit)) * ct[, 2],
    r_squared = summary(fit)$r.squared
  )
}

models <- dplyr::bind_rows(
  robust_table(fit1, "Overall: status only"),
  robust_table(fit2, "Overall: status + class"),
  robust_table(fit3, "Overall: status x class"),
  robust_table(fit_lic, "Licensing: status + class"),
  robust_table(fit_eng, "Engagement: status + class")
)


# 13. LICENSING ANALYSIS -----------------------------------------

licensing_floor <- dat |>
  dplyr::group_by(
    status,
    reactor_class
  ) |>
  dplyr::summarise(
    n = dplyr::n(),
    licensing_mean = mean(licensing),
    licensing_median = median(licensing),
    licensing_at_1 = sum(licensing == 1),
    licensing_at_1_pct = 100 * mean(licensing == 1),
    licensing_at_1_or_2_pct = 100 * mean(licensing <= 2),
    .groups = "drop"
  )

licensing_vs_other <- dat |>
  dplyr::mutate(
    other_five_mean = rowMeans(
      dplyr::across(
        dplyr::all_of(
          setdiff(indicators, "licensing")
        )
      )
    ),
    licensing_gap = licensing - other_five_mean
  ) |>
  dplyr::group_by(
    status,
    reactor_class
  ) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_licensing_gap = mean(licensing_gap),
    median_licensing_gap = median(licensing_gap),
    .groups = "drop"
  )


# 14. FIGURE ORDER -----------------------------------------------

figure_order <- c(
  "Overall",
  "Licensing",
  "Siting",
  "Financing",
  "Supply chain",
  "Fuel supply",
  "Stakeholder engagement"
)

indicator_labels <- c(
  overall = "Overall",
  licensing = "Licensing",
  siting = "Siting",
  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  stakeholder_engagement = "Stakeholder engagement"
)

plot_data <- descriptive_status |>
  dplyr::mutate(
    indicator_label = unname(
      indicator_labels[as.character(indicator)]
    ),
    indicator_label = factor(
      indicator_label,
      levels = rev(figure_order)
    ),
    se = sd / sqrt(n),
    low = mean - 1.96 * se,
    high = mean + 1.96 * se
  )


# 15. LEGEND WITH AUTOMATIC SAMPLE SIZES -------------------------

legend_labels <- c(
  recurring = paste0(
    "Recurring (n = ", n_recurring, ")"
  ),
  new = paste0(
    "New (n = ", n_new, ")"
  )
)


# 16. BUILD ORIGINAL FIGURE --------------------------------------

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = mean,
    y = indicator_label,
    colour = status
  )
) +
  
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = low,
      xmax = high
    ),
    orientation = "y",
    width = 0.15,
    linewidth = 0.65,
    position = ggplot2::position_dodge(
      width = 0.45
    )
  ) +
  
  ggplot2::geom_point(
    size = 2.4,
    position = ggplot2::position_dodge(
      width = 0.45
    )
  ) +
  
  ggplot2::scale_colour_manual(
    values = c(
      recurring = "#F8766D",
      new = "#00BFC4"
    ),
    breaks = c("recurring", "new"),
    labels = legend_labels
  ) +
  
  ggplot2::scale_x_continuous(
    limits = c(1, 6),
    breaks = 1:6
  ) +
  
  ggplot2::labs(
    x = "Mean market-readiness score (1–6)",
    y = NULL,
    colour = "Design status"
  ) +
  
  ggplot2::theme_classic(
    base_size = 12
  ) +
  
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.7
    ),
    axis.text = ggplot2::element_text(
      colour = "black"
    ),
    legend.title = ggplot2::element_text(
      colour = "black"
    )
  )

print(p)


# 17. EXPORT SUPPLEMENTARY FIGURE S1 -----------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = p,
  width = 9,
  height = 5.5,
  bg = "white"
)


# 18. EXPORT ALL EXCEL RESULTS -----------------------------------

writexl::write_xlsx(
  list(
    counts = counts,
    descriptive_status = descriptive_status,
    descriptive_class = descriptive_class,
    descriptive_four_groups = descriptive_four_groups,
    status_tests = status_tests,
    class_tests = class_tests,
    stratified_status_tests = stratified_tests,
    composition_fisher = composition_test,
    adjusted_models = models,
    licensing_floor = licensing_floor,
    licensing_vs_other_five = licensing_vs_other,
    design_level = dat |>
      dplyr::select(
        design,
        status,
        reactor_class,
        dplyr::all_of(metrics)
      )
  ),
  path = excel_file
)


# 19. PRINT RESULTS ----------------------------------------------

cat("\n========================================\n")
cat("SAMPLE COUNTS\n")
cat("========================================\n")
print(counts)

cat("\n========================================\n")
cat("OVERALL AND INDICATOR COMPARISONS\n")
cat("========================================\n")
print(status_tests)

cat("\n========================================\n")
cat("REACTOR CLASS COMPARISONS\n")
cat("========================================\n")
print(class_tests)

cat("\n========================================\n")
cat("WITHIN-CLASS STATUS COMPARISONS\n")
cat("========================================\n")
print(stratified_tests)

cat("\n========================================\n")
cat("FISHER EXACT TEST\n")
cat("========================================\n")
print(composition_test)

cat("\n========================================\n")
cat("ADJUSTED MODELS (HC3)\n")
cat("========================================\n")
print(models)

cat("\n========================================\n")
cat("LICENSING FLOOR\n")
cat("========================================\n")
print(licensing_floor)

cat("\n========================================\n")
cat("RESULTS SAVED TO\n")
cat("========================================\n")

cat(png_file, "\n")
cat(pdf_file, "\n")
cat(excel_file, "\n")

stopifnot(
  all(file.exists(c(
    png_file,
    pdf_file,
    excel_file
  )))
)

cat("\nSupplementary Figure S1 completed.\n")
