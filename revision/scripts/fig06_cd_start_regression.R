
# ================================================================
# FIGURE 6
# CONCEPTUAL DESIGN START YEAR AND SMR MARKET READINESS
# NEA SMR DASHBOARD, EDITION 4
#
# Panel a: All designs, with year range and sample size
# Panel b: Conceptual design starts, 2010–2019
# Panel c: Conceptual design starts, 2020–2025
#
# Original three-panel figure appearance preserved.
# Only bold, centred panel subtitles have been added.
#
# INPUT:
# revision/data/fig06_cd_start_source_data.xlsx
#
# OUTPUT FILES:
# revision/figures/fig06_cd_start.png
# revision/figures/fig06_cd_start.pdf
# revision/figures/fig06_cd_start_results.xlsx
# ================================================================


# ---------------------------------------------------------------
# 1. PACKAGES
# ---------------------------------------------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "cowplot",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
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
  library(ggplot2)
  library(cowplot)
  library(openxlsx)
})


# ---------------------------------------------------------------
# 2. GITHUB-COMPATIBLE FILE PATHS
# ---------------------------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig06_cd_start_source_data.xlsx"
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
  recursive = TRUE,
  showWarnings = FALSE
)

png_file <- file.path(
  output_dir,
  "fig06_cd_start.png"
)

pdf_file <- file.path(
  output_dir,
  "fig06_cd_start.pdf"
)

excel_file <- file.path(
  output_dir,
  "fig06_cd_start_results.xlsx"
)

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found:\n",
    input_file
  )
}


# ---------------------------------------------------------------
# 3. LOAD DATA
# ---------------------------------------------------------------

sheet_name <- readxl::excel_sheets(input_file)[1]

df_raw <- readxl::read_excel(
  input_file,
  sheet = sheet_name
)

names(df_raw) <- tolower(
  gsub(
    "^_|_$",
    "",
    gsub(
      "[^a-zA-Z0-9]+",
      "_",
      names(df_raw)
    )
  )
)

required_columns <- c(
  "design",
  "cd_start",
  "mean_readiness"
)

missing_columns <- setdiff(
  required_columns,
  names(df_raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ---------------------------------------------------------------
# 4. CLEAN AND VALIDATE DATA
# ---------------------------------------------------------------

df <- df_raw %>%
  dplyr::mutate(
    design = trimws(as.character(design)),
    
    cd_start = suppressWarnings(
      as.numeric(cd_start)
    ),
    
    mean_readiness = suppressWarnings(
      as.numeric(mean_readiness)
    )
  ) %>%
  dplyr::filter(
    !is.na(cd_start),
    !is.na(mean_readiness),
    !is.na(design),
    nzchar(design)
  )

if (anyDuplicated(df$design) > 0) {
  stop(
    "Duplicate design names detected. ",
    "Please inspect the source workbook."
  )
}

if (any(df$cd_start < 1900 | df$cd_start > 2026)) {
  stop(
    "Conceptual design start years outside ",
    "the expected range."
  )
}

if (
  any(
    df$mean_readiness < 1 |
    df$mean_readiness > 6
  )
) {
  stop(
    "Mean readiness scores must fall ",
    "between 1 and 6."
  )
}

if (nrow(df) != 52) {
  warning(
    "Expected 52 designs; found ",
    nrow(df),
    ". Check the source workbook."
  )
}


# ---------------------------------------------------------------
# 5. VALIDATE MEAN READINESS
# ---------------------------------------------------------------

readiness_columns <- c(
  "licensing",
  "siting",
  "financing",
  "supply_chain",
  "stakeholder_engagement",
  "fuel_supply"
)

if (all(readiness_columns %in% names(df))) {
  
  df <- df %>%
    dplyr::mutate(
      mean_readiness_recalculated = rowMeans(
        dplyr::across(
          dplyr::all_of(readiness_columns),
          ~ suppressWarnings(as.numeric(.x))
        ),
        na.rm = FALSE
      ),
      
      readiness_difference =
        mean_readiness -
        mean_readiness_recalculated
    )
  
  inconsistent <- df %>%
    dplyr::filter(
      !is.na(readiness_difference),
      abs(readiness_difference) > 0.0001
    )
  
  if (nrow(inconsistent) > 0) {
    stop(
      "Mean-readiness discrepancies detected ",
      "for ",
      nrow(inconsistent),
      " designs."
    )
  }
  
  message(
    "Mean readiness validated against six indicators."
  )
  
} else {
  
  df$mean_readiness_recalculated <- NA_real_
  df$readiness_difference <- NA_real_
  
  warning(
    "Not all six readiness indicators were found. ",
    "Using the existing mean_readiness column."
  )
}


# ---------------------------------------------------------------
# 6. DEFINE HISTORICAL COHORTS
# ---------------------------------------------------------------

df <- df %>%
  dplyr::mutate(
    cohort = dplyr::case_when(
      cd_start < 2010 ~ "Before 2010",
      
      cd_start >= 2010 &
        cd_start <= 2019 ~ "2010–2019",
      
      cd_start >= 2020 &
        cd_start <= 2025 ~ "2020–2025",
      
      TRUE ~ "Outside cohort range"
    )
  )

df$cohort <- factor(
  df$cohort,
  levels = c(
    "Before 2010",
    "2010–2019",
    "2020–2025",
    "Outside cohort range"
  )
)

df <- df %>%
  dplyr::arrange(
    cd_start,
    design
  )

df_a <- df

df_b <- df %>%
  dplyr::filter(
    cd_start >= 2010,
    cd_start <= 2019
  )

df_c <- df %>%
  dplyr::filter(
    cd_start >= 2020,
    cd_start <= 2025
  )

if (nrow(df_b) < 3 || nrow(df_c) < 3) {
  stop(
    "Insufficient observations in one ",
    "of the contemporary cohorts."
  )
}

cat("\n====================================\n")
cat("SAMPLE COUNTS\n")
cat("====================================\n")

print(
  df %>%
    dplyr::count(cohort, name = "n")
)

cat(
  "\nAll designs:", nrow(df_a),
  "\n2010–2019:", nrow(df_b),
  "\n2020–2025:", nrow(df_c),
  "\n"
)


# ---------------------------------------------------------------
# 7. REGRESSION FUNCTION
# ---------------------------------------------------------------

analyse_cohort <- function(data, label) {
  
  model <- stats::lm(
    mean_readiness ~ cd_start,
    data = data
  )
  
  sm <- summary(model)
  
  coefficient <- sm$coefficients[
    "cd_start",
    ,
    drop = FALSE
  ]
  
  ci <- stats::confint(
    model,
    parm = "cd_start",
    level = 0.95
  )
  
  data.frame(
    sample = label,
    n = nrow(data),
    
    start_year_min = min(data$cd_start),
    start_year_max = max(data$cd_start),
    
    beta = unname(coefficient[1, "Estimate"]),
    
    standard_error = unname(
      coefficient[1, "Std. Error"]
    ),
    
    ci_lower = unname(ci[1]),
    ci_upper = unname(ci[2]),
    
    t_statistic = unname(
      coefficient[1, "t value"]
    ),
    
    p_value = unname(
      coefficient[1, "Pr(>|t|)"]
    ),
    
    r_squared = sm$r.squared,
    adjusted_r_squared = sm$adj.r.squared,
    
    mean_readiness = mean(
      data$mean_readiness
    ),
    
    sd_readiness = stats::sd(
      data$mean_readiness
    ),
    
    row.names = NULL
  )
}

regression_results <- dplyr::bind_rows(
  analyse_cohort(
    df_a,
    "All designs"
  ),
  
  analyse_cohort(
    df_b,
    "2010–2019"
  ),
  
  analyse_cohort(
    df_c,
    "2020–2025"
  )
)

cat("\nREGRESSION RESULTS\n")
print(regression_results)


# ---------------------------------------------------------------
# 8. COHORT AND SAMPLE SUMMARIES
# ---------------------------------------------------------------

cohort_summary <- df %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(
    n = dplyr::n(),
    
    earliest_start = min(cd_start),
    latest_start = max(cd_start),
    
    mean_start_year = mean(cd_start),
    
    mean_readiness = mean(mean_readiness),
    median_readiness = median(mean_readiness),
    
    sd_readiness = stats::sd(mean_readiness),
    
    min_readiness = min(mean_readiness),
    max_readiness = max(mean_readiness),
    
    .groups = "drop"
  )

sample_summary <- data.frame(
  metric = c(
    "Source workbook",
    "Source worksheet",
    "Total analysed designs",
    "Before 2010",
    "2010–2019",
    "2020–2025",
    "Earliest conceptual design start",
    "Latest conceptual design start",
    "Overall mean readiness"
  ),
  
  value = as.character(c(
    basename(input_file),
    sheet_name,
    nrow(df),
    sum(df$cd_start < 2010),
    nrow(df_b),
    nrow(df_c),
    min(df$cd_start),
    max(df$cd_start),
    round(mean(df$mean_readiness), 4)
  ))
)


# ---------------------------------------------------------------
# 9. ORIGINAL FIGURE COLOURS
# ---------------------------------------------------------------

colour_historical <- "#333333"
colour_2010s <- "#C97A2B"
colour_2020s <- "#4F8A3A"

colour_trend <- "#1F5AC6"
colour_ci <- "grey80"

cohort_colours <- c(
  "Before 2010" = colour_historical,
  "2010–2019" = colour_2010s,
  "2020–2025" = colour_2020s,
  "Outside cohort range" = "grey60"
)


# ---------------------------------------------------------------
# 10. ORIGINAL FIGURE THEME
# ---------------------------------------------------------------

theme_panel <- ggplot2::theme_classic(
  base_size = 11
) +
  ggplot2::theme(
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    axis.ticks = ggplot2::element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    axis.text = ggplot2::element_text(
      colour = "black",
      size = 9
    ),
    
    axis.title = ggplot2::element_text(
      colour = "black",
      size = 10
    ),
    
    legend.position = "none",
    
    plot.margin = ggplot2::margin(
      7, 9, 7, 9
    )
  )


# ---------------------------------------------------------------
# 11. PANEL FUNCTION
# ---------------------------------------------------------------

make_panel <- function(
    data,
    x_limits,
    x_breaks,
    show_y_title = TRUE,
    colour_by_cohort = FALSE,
    point_colour = "black",
    panel_subtitle = NULL
) {
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = cd_start,
      y = mean_readiness
    )
  ) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      colour = colour_trend,
      fill = colour_ci,
      linewidth = 0.8
    )
  
  if (colour_by_cohort) {
    
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = cohort
        ),
        size = 2.3,
        alpha = 0.9
      ) +
      ggplot2::scale_colour_manual(
        values = cohort_colours,
        drop = FALSE
      )
    
  } else {
    
    p <- p +
      ggplot2::geom_point(
        colour = point_colour,
        size = 2.3,
        alpha = 0.9
      )
  }
  
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = 1:6,
      limits = c(1, 6),
      
      expand = ggplot2::expansion(
        mult = c(0.025, 0.025)
      )
    ) +
    
    ggplot2::labs(
      x = NULL,
      
      y = if (show_y_title) {
        "Market readiness (mean)"
      } else {
        NULL
      },
      
      title = panel_subtitle
    ) +
    
    theme_panel +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 11,
        colour = "black",
        hjust = 0.5,
        margin = ggplot2::margin(
          b = 5
        )
      )
    )
  
  p
}


# ---------------------------------------------------------------
# 12. PANEL SUBTITLES
# ---------------------------------------------------------------

# These are calculated from the actual source data.
# No sample sizes or starting years are hard-coded.

subtitle_a <- sprintf(
  "All designs (%d–%d; n = %d)",
  as.integer(min(df_a$cd_start)),
  as.integer(max(df_a$cd_start)),
  nrow(df_a)
)

subtitle_b <- sprintf(
  "2010–2019 (n = %d)",
  nrow(df_b)
)

subtitle_c <- sprintf(
  "2020–2025 (n = %d)",
  nrow(df_c)
)

cat("\nPANEL SUBTITLES:\n")
cat("a:", subtitle_a, "\n")
cat("b:", subtitle_b, "\n")
cat("c:", subtitle_c, "\n")


# ---------------------------------------------------------------
# 13. PANEL A: ALL DESIGNS
# ---------------------------------------------------------------

p_a <- make_panel(
  data = df_a,
  
  x_limits = c(
    min(df_a$cd_start) - 1,
    2026
  ),
  
  x_breaks = c(
    1985, 1995, 2005, 2015, 2025
  ),
  
  show_y_title = TRUE,
  colour_by_cohort = TRUE,
  panel_subtitle = subtitle_a
)


# ---------------------------------------------------------------
# 14. PANEL B: 2010–2019
# ---------------------------------------------------------------

p_b <- make_panel(
  data = df_b,
  
  x_limits = c(
    2009.7,
    2019.3
  ),
  
  x_breaks = c(
    2010, 2013, 2016, 2019
  ),
  
  show_y_title = TRUE,
  colour_by_cohort = FALSE,
  point_colour = colour_2010s,
  panel_subtitle = subtitle_b
)


# ---------------------------------------------------------------
# 15. PANEL C: 2020–2025
# ---------------------------------------------------------------

p_c <- make_panel(
  data = df_c,
  
  x_limits = c(
    2019.7,
    2025.3
  ),
  
  x_breaks = c(
    2020, 2021, 2022,
    2023, 2024, 2025
  ),
  
  show_y_title = FALSE,
  colour_by_cohort = FALSE,
  point_colour = colour_2020s,
  panel_subtitle = subtitle_c
)


# ---------------------------------------------------------------
# 16. COMBINE PANELS
# ---------------------------------------------------------------

bottom_row <- cowplot::plot_grid(
  p_b,
  p_c,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

shared_x <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Conceptual design start year",
    x = 0.5,
    y = 0.5,
    size = 10
  )

bottom_block <- cowplot::plot_grid(
  bottom_row,
  shared_x,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

combined <- cowplot::plot_grid(
  p_a,
  bottom_block,
  ncol = 1,
  rel_heights = c(1.10, 1)
)

combined <- cowplot::ggdraw(combined) +
  cowplot::draw_plot_label(
    label = c("a", "b", "c"),
    
    x = c(
      0.015,
      0.015,
      0.515
    ),
    
    y = c(
      0.990,
      0.475,
      0.475
    ),
    
    size = 11,
    fontface = "bold",
    hjust = 0,
    vjust = 1
  )

print(combined)


# ---------------------------------------------------------------
# 17. EXPORT FIGURE 6
# ---------------------------------------------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = combined,
  width = 180,
  height = 210,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = combined,
  width = 180,
  height = 210,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# ---------------------------------------------------------------
# 18. EXPORT SINGLE EXCEL WORKBOOK
# ---------------------------------------------------------------

wb <- openxlsx::createWorkbook()

excel_sheets <- list(
  "Sample summary" = sample_summary,
  "Regression results" = regression_results,
  "Cohort summary" = cohort_summary,
  "Design audit" = as.data.frame(df)
)

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#355C7D",
  textDecoration = "bold",
  halign = "left",
  border = "Bottom",
  borderColour = "#FFFFFF"
)

for (sheet in names(excel_sheets)) {
  
  openxlsx::addWorksheet(
    wb,
    sheet
  )
  
  openxlsx::writeData(
    wb,
    sheet,
    excel_sheets[[sheet]],
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(
      ncol(excel_sheets[[sheet]])
    ),
    widths = "auto"
  )
}

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# ---------------------------------------------------------------
# 19. VERIFY OUTPUT FILES
# ---------------------------------------------------------------

output_files <- c(
  png_file,
  pdf_file,
  excel_file
)

missing_outputs <- output_files[
  !file.exists(output_files)
]

if (length(missing_outputs) > 0) {
  stop(
    "The following outputs were not created:\n",
    paste(
      missing_outputs,
      collapse = "\n"
    )
  )
}


# ---------------------------------------------------------------
# 20. FINAL RESULTS
# ---------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("FIGURE 6 COMPLETE\n")
cat("============================================\n\n")

cat("PANEL SUBTITLES:\n")
cat("a:", subtitle_a, "\n")
cat("b:", subtitle_b, "\n")
cat("c:", subtitle_c, "\n\n")

cat("All designs:", nrow(df_a), "\n")
cat("2010–2019:", nrow(df_b), "\n")
cat("2020–2025:", nrow(df_c), "\n\n")

cat("REGRESSION RESULTS:\n")
print(regression_results)

cat("\nOUTPUT DIRECTORY:\n")
cat(output_dir, "\n\n")

cat("FILES CREATED:\n")

for (f in output_files) {
  cat(basename(f), "\n")
}

cat("\nFigure 6 exported successfully.\n")
