
# ============================================================
# FIGURE 5
# NEA SMR DASHBOARD
# MARKET READINESS BY CONTINUITY COHORT
# Editions 1–4
#
# Panel a: Longitudinal readiness trajectories
# Panel b: Edition 4 readiness distributions
#
# INPUT:
# revision/data/fig05_nea_continuity_source_data.xlsx
#
# OUTPUTS:
# revision/figures/fig05_nea_continuity.png
# revision/figures/fig05_nea_continuity.pdf
# revision/figures/fig05_nea_continuity_results.xlsx
#
# GitHub-compatible version
# Original figure appearance and analysis preserved.
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "patchwork",
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
    "Please install the following R packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(
  lapply(packages, library, character.only = TRUE)
)


# ------------------------------------------------------------
# 2. GITHUB-COMPATIBLE INPUT AND OUTPUT PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig05_nea_continuity_source_data.xlsx"
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
        "Could not locate the repository root.\n",
        "Expected file: ", target_file, "\n",
        "Starting directory: ", start_dir, "\n\n",
        "Run this script from within the repository ",
        "or set the working directory to its root."
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
  showWarnings = FALSE,
  recursive = TRUE
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

output_png <- file.path(
  output_dir,
  "fig05_nea_continuity.png"
)

output_pdf <- file.path(
  output_dir,
  "fig05_nea_continuity.pdf"
)

output_excel <- file.path(
  output_dir,
  "fig05_nea_continuity_results.xlsx"
)


# ------------------------------------------------------------
# 3. IMPORT AND CLEAN DATA
# ------------------------------------------------------------

data <- readxl::read_excel(input_file)

names(data) <- stringr::str_trim(names(data))

required_columns <- c(
  "edition",
  "design",
  "mean_readiness"
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

data <- data %>%
  dplyr::mutate(
    edition = as.integer(edition),
    
    design_original = as.character(design),
    
    design = stringr::str_squish(
      as.character(design)
    ),
    
    mean_readiness = as.numeric(mean_readiness),
    
    design = dplyr::case_when(
      design == "Jimmy SMR" ~ "Jimmy HTR",
      
      stringr::str_detect(
        design,
        "^BREST-\\s*OD-300$"
      ) ~ "BREST-OD-300",
      
      design == "Natrium Reactor Plant" ~ "Natrium",
      
      TRUE ~ design
    )
  )


# ------------------------------------------------------------
# 4. VALIDATION
# ------------------------------------------------------------

duplicates <- data %>%
  dplyr::count(design, edition) %>%
  dplyr::filter(n > 1)

if (nrow(duplicates) > 0) {
  print(duplicates)
  
  stop(
    "Duplicate design-edition combinations detected."
  )
}

if (any(is.na(data$mean_readiness))) {
  stop("Missing mean-readiness values detected.")
}

if (
  any(
    data$mean_readiness < 1 |
    data$mean_readiness > 6
  )
) {
  stop("Readiness values outside the 1–6 scale.")
}

if (any(!data$edition %in% 1:4)) {
  stop("Unexpected edition values detected.")
}


# ------------------------------------------------------------
# 5. IDENTIFY CONTINUITY COHORTS
# ------------------------------------------------------------

design_histories <- data %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    has_E1 = any(edition == 1),
    has_E2 = any(edition == 2),
    has_E3 = any(edition == 3),
    has_E4 = any(edition == 4),
    
    editions = paste(
      sort(unique(edition)),
      collapse = ","
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    cohort = dplyr::case_when(
      has_E1 & has_E2 & has_E3 & has_E4 ~ "1–4",
      
      !has_E1 & has_E2 & has_E3 & has_E4 ~ "2–4",
      
      !has_E1 & !has_E2 & has_E3 & has_E4 ~ "3–4",
      
      TRUE ~ "Other"
    )
  )

cohort_levels <- c(
  "1–4",
  "2–4",
  "3–4"
)

continuity <- data %>%
  dplyr::left_join(
    design_histories %>%
      dplyr::select(design, cohort),
    by = "design"
  ) %>%
  dplyr::filter(cohort %in% cohort_levels) %>%
  dplyr::mutate(
    cohort = factor(
      cohort,
      levels = cohort_levels
    )
  )


# ------------------------------------------------------------
# 6. COHORT COUNTS
# ------------------------------------------------------------

cohort_counts <- continuity %>%
  dplyr::distinct(design, cohort) %>%
  dplyr::count(
    cohort,
    name = "n",
    .drop = FALSE
  )

cat("\nCONTINUITY COHORT COUNTS\n")

print(cohort_counts)

expected_counts <- c(17L, 25L, 20L)

if (!identical(as.integer(cohort_counts$n), expected_counts)) {
  stop(
    "Unexpected continuity cohort counts. ",
    "Expected 17, 25 and 20."
  )
}


# ------------------------------------------------------------
# 7. COHORT MEANS
# ------------------------------------------------------------

cohort_means <- continuity %>%
  dplyr::group_by(cohort, edition) %>%
  dplyr::summarise(
    mean_readiness = mean(mean_readiness),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 8. EDITION 4 DATA
# ------------------------------------------------------------

edition4 <- continuity %>%
  dplyr::filter(edition == 4)


# ------------------------------------------------------------
# 9. MATCHED EDITION 3–4 DATA
# ------------------------------------------------------------

matched_34 <- continuity %>%
  dplyr::filter(edition %in% c(3, 4)) %>%
  dplyr::select(
    design,
    cohort,
    edition,
    mean_readiness
  ) %>%
  tidyr::pivot_wider(
    names_from = edition,
    values_from = mean_readiness,
    names_prefix = "E"
  ) %>%
  dplyr::mutate(
    change_34 = E4 - E3
  )


# ------------------------------------------------------------
# 10. COHORT LABELS
# ------------------------------------------------------------

labels_A <- c(
  "1–4" = "Editions 1–4",
  "2–4" = "Editions 2–4",
  "3–4" = "Editions 3–4"
)

labels_B <- setNames(
  paste0(
    as.character(cohort_counts$cohort),
    "\n(n = ",
    cohort_counts$n,
    ")"
  ),
  as.character(cohort_counts$cohort)
)


# ------------------------------------------------------------
# 11. COMMON GRAPHICAL SETTINGS
# ------------------------------------------------------------

blue <- "#2166AC"

grey_line <- "grey72"

grey_point <- "grey55"

base_font_size <- 12

readiness_scale <- ggplot2::scale_y_continuous(
  limits = c(1, 6),
  breaks = 1:6,
  expand = ggplot2::expansion(
    mult = c(0.015, 0.025)
  )
)


# ------------------------------------------------------------
# 12. PANEL a: CONTINUITY TRAJECTORIES
# ------------------------------------------------------------

panel_a <- ggplot2::ggplot(
  continuity,
  ggplot2::aes(
    x = edition,
    y = mean_readiness,
    group = design
  )
) +
  
  ggplot2::geom_line(
    colour = grey_line,
    linewidth = 0.40,
    alpha = 0.70
  ) +
  
  ggplot2::geom_point(
    colour = grey_point,
    size = 1.3,
    alpha = 0.75
  ) +
  
  ggplot2::geom_line(
    data = cohort_means,
    ggplot2::aes(
      x = edition,
      y = mean_readiness,
      group = cohort
    ),
    inherit.aes = FALSE,
    colour = blue,
    linewidth = 1.2
  ) +
  
  ggplot2::geom_point(
    data = cohort_means,
    ggplot2::aes(
      x = edition,
      y = mean_readiness
    ),
    inherit.aes = FALSE,
    colour = blue,
    size = 2.6
  ) +
  
  ggplot2::facet_wrap(
    ~ cohort,
    ncol = 1,
    labeller = ggplot2::labeller(
      cohort = labels_A
    )
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = 1:4,
    limits = c(1, 4),
    expand = ggplot2::expansion(
      mult = c(0.04, 0.04)
    )
  ) +
  
  readiness_scale +
  
  ggplot2::labs(
    x = "NEA SMR Dashboard edition",
    y = "Mean market-readiness score (1–6)",
    title = "a"
  ) +
  
  ggplot2::theme_classic(
    base_size = base_font_size
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(
      face = "bold",
      size = 11,
      colour = "black",
      hjust = 0.5,
      margin = ggplot2::margin(
        t = 3,
        b = 5
      )
    ),
    
    panel.spacing.y = grid::unit(
      0.22,
      "cm"
    ),
    
    axis.title = ggplot2::element_text(
      size = 11
    ),
    
    axis.text = ggplot2::element_text(
      size = 10,
      colour = "black"
    ),
    
    plot.margin = ggplot2::margin(
      8, 14, 8, 8
    )
  )


# ------------------------------------------------------------
# 13. PANEL b: EDITION 4 DISTRIBUTIONS
# ------------------------------------------------------------

panel_b <- ggplot2::ggplot(
  edition4,
  ggplot2::aes(
    x = cohort,
    y = mean_readiness
  )
) +
  
  ggplot2::geom_boxplot(
    width = 0.50,
    fill = "grey90",
    colour = "black",
    linewidth = 0.55,
    outlier.shape = NA
  ) +
  
  ggplot2::geom_jitter(
    width = 0.11,
    height = 0,
    alpha = 0.75,
    size = 1.8,
    colour = blue,
    seed = 123
  ) +
  
  readiness_scale +
  
  ggplot2::scale_x_discrete(
    labels = labels_B
  ) +
  
  ggplot2::labs(
    x = "Continuity cohort",
    y = "Mean market-readiness score (1–6)",
    title = "b"
  ) +
  
  ggplot2::theme_classic(
    base_size = base_font_size
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    
    axis.title = ggplot2::element_text(
      size = 11
    ),
    
    axis.text = ggplot2::element_text(
      size = 10,
      colour = "black"
    ),
    
    plot.margin = ggplot2::margin(
      8, 8, 8, 14
    )
  )


# ------------------------------------------------------------
# 14. COMBINE PANELS
# ------------------------------------------------------------

figure_05 <- panel_a + panel_b +
  
  patchwork::plot_layout(
    ncol = 2,
    widths = c(1.55, 1)
  )

print(figure_05)


# ------------------------------------------------------------
# 15. EXPORT FIGURE 5
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = output_png,
  plot = figure_05,
  width = 13,
  height = 8,
  units = "in",
  dpi = 400,
  bg = "white"
)

ggplot2::ggsave(
  filename = output_pdf,
  plot = figure_05,
  width = 13,
  height = 8,
  units = "in",
  bg = "white"
)


# ------------------------------------------------------------
# 16. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

descriptive_by_edition <- continuity %>%
  dplyr::group_by(cohort, edition) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(mean_readiness),
    median = median(mean_readiness),
    sd = sd(mean_readiness),
    minimum = min(mean_readiness),
    maximum = max(mean_readiness),
    .groups = "drop"
  )

edition4_summary <- edition4 %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(mean_readiness),
    median = median(mean_readiness),
    sd = sd(mean_readiness),
    .groups = "drop"
  )

change_summary <- matched_34 %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_change = mean(change_34),
    median_change = median(change_34),
    improved = sum(change_34 > 0),
    unchanged = sum(change_34 == 0),
    declined = sum(change_34 < 0),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17. EXPLORATORY STATISTICAL TESTS
# ------------------------------------------------------------

test_E4 <- stats::kruskal.test(
  mean_readiness ~ cohort,
  data = edition4
)

test_change <- stats::kruskal.test(
  change_34 ~ cohort,
  data = matched_34
)

pairwise_E4 <- stats::pairwise.wilcox.test(
  edition4$mean_readiness,
  edition4$cohort,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_change <- stats::pairwise.wilcox.test(
  matched_34$change_34,
  matched_34$cohort,
  p.adjust.method = "holm",
  exact = FALSE
)


# ------------------------------------------------------------
# 18. EXPORT EXCEL RESULTS
# ------------------------------------------------------------

test_summary <- data.frame(
  comparison = c(
    "Edition 4 readiness across cohorts",
    "Edition 3–4 change across cohorts"
  ),
  statistic = c(
    unname(test_E4$statistic),
    unname(test_change$statistic)
  ),
  df = c(
    unname(test_E4$parameter),
    unname(test_change$parameter)
  ),
  p_value = c(
    test_E4$p.value,
    test_change$p.value
  )
)

pairwise_to_df <- function(x, outcome) {
  result <- as.data.frame(
    as.table(x$p.value),
    stringsAsFactors = FALSE
  )
  
  names(result) <- c(
    "group_1",
    "group_2",
    "p_holm"
  )
  
  result <- result[
    !is.na(result$p_holm),
    ,
    drop = FALSE
  ]
  
  result$outcome <- outcome
  
  result[, c(
    "outcome",
    "group_1",
    "group_2",
    "p_holm"
  )]
}

pairwise_results <- dplyr::bind_rows(
  pairwise_to_df(
    pairwise_E4,
    "Edition 4 readiness"
  ),
  pairwise_to_df(
    pairwise_change,
    "Edition 3–4 change"
  )
)

openxlsx::write.xlsx(
  list(
    cohort_counts = cohort_counts,
    cohort_means = cohort_means,
    descriptive_by_edition = descriptive_by_edition,
    edition4_summary = edition4_summary,
    change_summary = change_summary,
    statistical_tests = test_summary,
    pairwise_tests = pairwise_results,
    design_histories = design_histories,
    continuity_data = continuity,
    matched_changes = matched_34
  ),
  file = output_excel,
  overwrite = TRUE
)


# ------------------------------------------------------------
# 19. FINAL OUTPUT VERIFICATION
# ------------------------------------------------------------

stopifnot(
  file.exists(output_png),
  file.exists(output_pdf),
  file.exists(output_excel)
)

cat("\n========================================\n")
cat("FIGURE 5 COMPLETE\n")
cat("========================================\n")

cat("\nCohort counts:\n")
print(cohort_counts)

cat("\nEdition 4 readiness:\n")
print(edition4_summary)

cat("\nEdition 3–4 changes:\n")
print(change_summary)

cat("\nKruskal-Wallis: Edition 4 readiness\n")
print(test_E4)

cat("\nPairwise Edition 4 comparisons:\n")
print(pairwise_E4)

cat("\nKruskal-Wallis: Edition 3–4 changes\n")
print(test_change)

cat("\nPairwise Edition 3–4 changes:\n")
print(pairwise_change)

cat("\nPNG:", output_png)
cat("\nPDF:", output_pdf)
cat("\nExcel:", output_excel)

cat("\nAll output files created successfully.\n")
