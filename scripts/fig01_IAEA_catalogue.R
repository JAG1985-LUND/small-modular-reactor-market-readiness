# ============================================================
# Code for the manuscript Gordon, J.A.
#
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces Figure 1.
#
# Panel a shows the number of reactor designs by catalogue year,
# divided between small modular reactors and microreactors.
#
# Panel b shows the development-phase composition of small modular
# reactor designs by catalogue year.
#
# Panel c shows the development-phase composition of microreactor
# designs by catalogue year.
#
# Working directory:
# Run this script from either:
#   1. the repository root; or
#   2. the repository's scripts/ directory.
#
# Required input:
# data/fig01_pipeline_and_phase_panels_source_data.xlsx
#
# Figure outputs:
# outputs/fig01_pipeline_and_phase_panels.png
# outputs/fig01_pipeline_and_phase_panels.pdf
#
# Figure source-data outputs:
# data/fig01_panel_a_source_data.csv
# data/fig01_panel_b_source_data.csv
# data/fig01_panel_c_source_data.csv
#
# Descriptive-statistics outputs:
# data/fig01_descriptives_reactor_type_by_year.csv
# data/fig01_descriptives_phase_by_reactor_type_year.csv
# data/fig01_descriptives_phase_by_reactor_type_year_wide.csv
# data/fig01_descriptives_summary.csv
# ============================================================


# ============================================================
# 1. LOAD PACKAGES AND RESET SESSION
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(readr)
  library(grid)
  library(tibble)
})

rm(list = ls())
graphics.off()


# ============================================================
# 2. LOCATE THE REPOSITORY
# ============================================================

get_active_script_path <- function() {

  source_path <- tryCatch(
    {
      path <- sys.frames()[[1]]$ofile

      if (is.null(path)) {
        ""
      } else {
        normalizePath(
          path,
          winslash = "/",
          mustWork = FALSE
        )
      }
    },
    error = function(e) ""
  )

  if (nzchar(source_path)) {
    return(source_path)
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()
  ) {

    rstudio_path <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )

    if (nzchar(rstudio_path)) {
      return(
        normalizePath(
          rstudio_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    }
  }

  ""
}


find_repository_root <- function(input_filename) {

  current_dir <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )

  script_path <- get_active_script_path()

  candidate_dirs <- c(
    current_dir,
    dirname(current_dir)
  )

  if (nzchar(script_path)) {

    script_dir <- dirname(script_path)

    candidate_dirs <- c(
      candidate_dirs,
      script_dir,
      dirname(script_dir)
    )
  }

  candidate_dirs <- unique(
    normalizePath(
      candidate_dirs,
      winslash = "/",
      mustWork = FALSE
    )
  )

  candidate_files <- file.path(
    candidate_dirs,
    "data",
    input_filename
  )

  matched_index <- which(
    file.exists(candidate_files)
  )

  if (length(matched_index) == 0) {

    stop(
      paste0(
        "Figure 1 input workbook was not found.\n\n",
        "Expected repository-relative path:\n",
        "data/",
        input_filename,
        "\n\n",
        "Run the script from the repository root or the scripts/ directory.\n\n",
        "Locations checked:\n",
        paste(candidate_files, collapse = "\n")
      )
    )
  }

  candidate_dirs[matched_index[1]]
}


input_filename <- "fig01_pipeline_and_phase_panels_source_data.xlsx"

project_dir <- find_repository_root(
  input_filename
)

input_file <- file.path(
  project_dir,
  "data",
  input_filename
)

output_dir <- file.path(
  project_dir,
  "outputs"
)

data_dir <- file.path(
  project_dir,
  "data"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

message("Repository root: ", project_dir)
message("Input workbook: ", input_file)
message("Figure output directory: ", output_dir)
message("Source-data output directory: ", data_dir)


# ============================================================
# 3. DEFINE OUTPUT FILES
# ============================================================

png_output <- file.path(
  output_dir,
  "fig01_pipeline_and_phase_panels.png"
)

pdf_output <- file.path(
  output_dir,
  "fig01_pipeline_and_phase_panels.pdf"
)

panel_a_output <- file.path(
  data_dir,
  "fig01_panel_a_source_data.csv"
)

panel_b_output <- file.path(
  data_dir,
  "fig01_panel_b_source_data.csv"
)

panel_c_output <- file.path(
  data_dir,
  "fig01_panel_c_source_data.csv"
)

reactor_type_output <- file.path(
  data_dir,
  "fig01_descriptives_reactor_type_by_year.csv"
)

phase_long_output <- file.path(
  data_dir,
  "fig01_descriptives_phase_by_reactor_type_year.csv"
)

phase_wide_output <- file.path(
  data_dir,
  "fig01_descriptives_phase_by_reactor_type_year_wide.csv"
)

summary_output <- file.path(
  data_dir,
  "fig01_descriptives_summary.csv"
)


# ============================================================
# 4. EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI <- 600


# ============================================================
# 5. TYPOGRAPHY
# ============================================================

BASE_SIZE <- 8
AXIS_TEXT_SIZE <- 8
AXIS_TITLE_SIZE <- 8
LEGEND_TEXT_SIZE <- 8
LEGEND_TITLE_SIZE <- 8
TITLE_SIZE <- 8
TAG_SIZE <- 12


# ============================================================
# 6. DISPLAY ORDER AND COLOURS
# ============================================================

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design" = "#F4A582",
  "Detailed design" = "#D6604D",
  "Licensing" = "#D1E5F0",
  "Construction" = "#92C5DE",
  "Operational" = "#4393C3"
)

classification_order <- c(
  "Small modular reactor",
  "Microreactor"
)

classification_cols <- c(
  "Small modular reactor" = "#4C9A5F",
  "Microreactor" = "#6A6A6A"
)


# ============================================================
# 7. HELPER FUNCTIONS
# ============================================================

make_stack_data <- function(
  data,
  stack_col,
  stack_levels
) {

  data %>%
    dplyr::mutate(
      stack_group = factor(
        .data[[stack_col]],
        levels = stack_levels
      )
    ) %>%
    dplyr::arrange(
      publication_year,
      stack_group
    ) %>%
    dplyr::group_by(
      publication_year
    ) %>%
    dplyr::mutate(
      ymin = dplyr::lag(
        cumsum(n_reactors),
        default = 0
      ),
      ymax = cumsum(n_reactors)
    ) %>%
    dplyr::ungroup()
}


tag_panel <- function(
  plot,
  tag,
  x = 0.015,
  y = 0.99
) {

  cowplot::ggdraw(plot) +
    cowplot::draw_plot_label(
      label = tag,
      x = x,
      y = y,
      hjust = 0,
      vjust = 1,
      size = TAG_SIZE,
      fontface = "bold"
    )
}


get_year_value <- function(
  data,
  year,
  variable
) {

  value <- data %>%
    dplyr::filter(
      publication_year == year
    ) %>%
    dplyr::pull(
      {{ variable }}
    )

  if (length(value) == 0) {
    return(NA_real_)
  }

  as.numeric(value[1])
}


# ============================================================
# 8. COMMON FIGURE THEME
# ============================================================

base_theme <- ggplot2::theme_classic(
  base_size = BASE_SIZE
) +
  ggplot2::theme(
    text = ggplot2::element_text(
      colour = "black"
    ),

    axis.text.x = ggplot2::element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),

    axis.text.y = ggplot2::element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),

    axis.title.x = ggplot2::element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black",
      margin = ggplot2::margin(t = 4)
    ),

    axis.title.y = ggplot2::element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black"
    ),

    axis.line.x = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.line.y = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.ticks = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    legend.text = ggplot2::element_text(
      size = LEGEND_TEXT_SIZE,
      colour = "black"
    ),

    legend.title = ggplot2::element_text(
      size = LEGEND_TITLE_SIZE,
      colour = "black"
    ),

    legend.key.height = grid::unit(
      0.28,
      "cm"
    ),

    legend.key.width = grid::unit(
      0.28,
      "cm"
    ),

    legend.spacing.x = grid::unit(
      1.5,
      "mm"
    ),

    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),

    legend.margin = ggplot2::margin(
      0,
      0,
      0,
      0
    ),

    legend.box.margin = ggplot2::margin(
      0,
      0,
      0,
      0
    ),

    legend.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),

    plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = TITLE_SIZE,
      face = "plain",
      margin = ggplot2::margin(b = 2)
    ),

    plot.margin = ggplot2::margin(
      t = 4,
      r = 6,
      b = 4,
      l = 4
    ),

    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),

    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    )
  )


# ============================================================
# 9. LOAD SOURCE DATA
# ============================================================

available_sheets <- readxl::excel_sheets(
  input_file
)

if (length(available_sheets) == 0) {
  stop(
    "The Figure 1 input workbook contains no worksheets."
  )
}

sheet_name <- if ("fig01" %in% available_sheets) {
  "fig01"
} else {
  available_sheets[1]
}

source_data <- readxl::read_excel(
  path = input_file,
  sheet = sheet_name
)

message(
  "Loaded ",
  nrow(source_data),
  " rows and ",
  ncol(source_data),
  " columns from worksheet '",
  sheet_name,
  "'."
)


# ============================================================
# 10. VALIDATE SOURCE DATA
# ============================================================

required_columns <- c(
  "publication_year",
  "classification",
  "development_phase_recoded"
)

missing_columns <- setdiff(
  required_columns,
  names(source_data)
)

if (length(missing_columns) > 0) {

  stop(
    paste0(
      "The Figure 1 input workbook is missing the following required columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

if (nrow(source_data) == 0) {
  stop(
    "The Figure 1 input workbook contains no observations."
  )
}


# ============================================================
# 11. CLEAN AND PREPARE SOURCE DATA
# ============================================================

plot_data <- source_data %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      ~ stringr::str_squish(.x)
    ),

    publication_year = suppressWarnings(
      as.integer(publication_year)
    ),

    classification = classification %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(
        "[ -]+",
        "_"
      ),

    development_phase_recoded =
      development_phase_recoded %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(
        "[ -]+",
        "_"
      )
  ) %>%
  dplyr::filter(
    !is.na(publication_year)
  ) %>%
  dplyr::filter(
    classification %in% c(
      "small_modular_reactor",
      "micro_reactor"
    )
  ) %>%
  dplyr::mutate(
    classification_label = dplyr::case_when(
      classification == "small_modular_reactor" ~
        "Small modular reactor",

      classification == "micro_reactor" ~
        "Microreactor",

      TRUE ~ NA_character_
    ),

    phase_label = dplyr::case_when(
      development_phase_recoded == "conceptual_design" ~
        "Conceptual design",

      development_phase_recoded == "basic_design" ~
        "Basic design",

      development_phase_recoded == "detailed_design" ~
        "Detailed design",

      development_phase_recoded == "licensing" ~
        "Licensing",

      development_phase_recoded == "construction" ~
        "Construction",

      development_phase_recoded == "operational" ~
        "Operational",

      TRUE ~ NA_character_
    ),

    classification_label = factor(
      classification_label,
      levels = classification_order
    ),

    phase_label = factor(
      phase_label,
      levels = phase_order
    )
  )

if (nrow(plot_data) == 0) {
  stop(
    "No small modular reactor or microreactor observations were retained."
  )
}

years <- sort(
  unique(plot_data$publication_year)
)

if (length(years) == 0) {
  stop(
    "No valid publication years were retained."
  )
}


# ============================================================
# 12. CREATE FIGURE SOURCE DATA
# ============================================================

panel_a_data <- plot_data %>%
  dplyr::count(
    publication_year,
    classification_label,
    name = "n_reactors"
  ) %>%
  tidyr::complete(
    publication_year = years,
    classification_label = factor(
      classification_order,
      levels = classification_order
    ),
    fill = list(
      n_reactors = 0
    )
  ) %>%
  dplyr::mutate(
    classification_label = factor(
      classification_label,
      levels = classification_order
    )
  ) %>%
  make_stack_data(
    stack_col = "classification_label",
    stack_levels = classification_order
  ) %>%
  dplyr::mutate(
    year_index = match(
      publication_year,
      years
    )
  )


panel_b_data <- plot_data %>%
  dplyr::filter(
    classification_label ==
      "Small modular reactor"
  ) %>%
  dplyr::filter(
    !is.na(phase_label)
  ) %>%
  dplyr::count(
    publication_year,
    phase_label,
    name = "n_reactors"
  ) %>%
  tidyr::complete(
    publication_year = years,
    phase_label = factor(
      phase_order,
      levels = phase_order
    ),
    fill = list(
      n_reactors = 0
    )
  ) %>%
  dplyr::mutate(
    phase_label = factor(
      phase_label,
      levels = phase_order
    )
  ) %>%
  make_stack_data(
    stack_col = "phase_label",
    stack_levels = phase_order
  ) %>%
  dplyr::mutate(
    year_index = match(
      publication_year,
      years
    )
  )


panel_c_data <- plot_data %>%
  dplyr::filter(
    classification_label ==
      "Microreactor"
  ) %>%
  dplyr::filter(
    !is.na(phase_label)
  ) %>%
  dplyr::count(
    publication_year,
    phase_label,
    name = "n_reactors"
  ) %>%
  tidyr::complete(
    publication_year = years,
    phase_label = factor(
      phase_order,
      levels = phase_order
    ),
    fill = list(
      n_reactors = 0
    )
  ) %>%
  dplyr::mutate(
    phase_label = factor(
      phase_label,
      levels = phase_order
    )
  ) %>%
  make_stack_data(
    stack_col = "phase_label",
    stack_levels = phase_order
  ) %>%
  dplyr::mutate(
    year_index = match(
      publication_year,
      years
    )
  )


# ============================================================
# 13. EXPORT FIGURE SOURCE DATA
# ============================================================

readr::write_csv(
  panel_a_data %>%
    dplyr::select(
      publication_year,
      classification_label,
      n_reactors
    ),
  panel_a_output
)

readr::write_csv(
  panel_b_data %>%
    dplyr::select(
      publication_year,
      phase_label,
      n_reactors
    ),
  panel_b_output
)

readr::write_csv(
  panel_c_data %>%
    dplyr::select(
      publication_year,
      phase_label,
      n_reactors
    ),
  panel_c_output
)


# ============================================================
# 14. PANEL A
# ============================================================

panel_a <- ggplot2::ggplot(
  panel_a_data,
  ggplot2::aes(
    xmin = year_index - 0.36,
    xmax = year_index + 0.36,
    ymin = ymin,
    ymax = ymax,
    fill = classification_label
  )
) +
  ggplot2::geom_rect(
    colour = NA
  ) +
  ggplot2::scale_fill_manual(
    values = classification_cols,
    breaks = classification_order,
    limits = classification_order,
    drop = FALSE,
    name = "Reactor type"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      keywidth = grid::unit(
        3.6,
        "mm"
      ),
      keyheight = grid::unit(
        3.6,
        "mm"
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(years),
    labels = years,
    expand = ggplot2::expansion(
      mult = c(0.02, 0.02)
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 86),
    breaks = c(
      20,
      40,
      60,
      80
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.02)
    )
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Number of reactors"
  ) +
  base_theme +
  ggplot2::theme(
    legend.position = c(
      0.02,
      0.98
    ),

    legend.justification = c(
      0,
      1
    ),

    legend.direction = "vertical",
    legend.box = "vertical",

    legend.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),

    legend.title = ggplot2::element_text(
      size = LEGEND_TITLE_SIZE,
      colour = "black"
    ),

    legend.text = ggplot2::element_text(
      size = LEGEND_TEXT_SIZE,
      colour = "black"
    ),

    legend.spacing.y = grid::unit(
      3.5,
      "mm"
    ),

    legend.key.height = grid::unit(
      4.2,
      "mm"
    )
  )


# ============================================================
# 15. PANEL B
# ============================================================

panel_b <- ggplot2::ggplot(
  panel_b_data,
  ggplot2::aes(
    xmin = year_index - 0.36,
    xmax = year_index + 0.36,
    ymin = ymin,
    ymax = ymax,
    fill = phase_label
  )
) +
  ggplot2::geom_rect(
    colour = NA
  ) +
  ggplot2::scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    limits = phase_order,
    drop = FALSE,
    name = NULL
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = grid::unit(
        3.2,
        "mm"
      ),
      keyheight = grid::unit(
        3.2,
        "mm"
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(years),
    labels = years,
    expand = ggplot2::expansion(
      mult = c(0.02, 0.02)
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 60),
    breaks = c(
      20,
      40,
      60
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.02)
    )
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Number of reactors",
    title = "Small modular reactor"
  ) +
  base_theme


legend_phase <- cowplot::get_legend(
  panel_b +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.title = ggplot2::element_blank(),

      legend.text = ggplot2::element_text(
        size = LEGEND_TEXT_SIZE,
        colour = "black"
      ),

      legend.key.height = grid::unit(
        3.2,
        "mm"
      ),

      legend.key.width = grid::unit(
        3.2,
        "mm"
      ),

      legend.spacing.x = grid::unit(
        1.5,
        "mm"
      ),

      legend.margin = ggplot2::margin(
        0,
        0,
        0,
        0
      ),

      legend.box.margin = ggplot2::margin(
        0,
        0,
        0,
        0
      )
    )
)

panel_b <- panel_b +
  ggplot2::theme(
    legend.position = "none"
  )


# ============================================================
# 16. PANEL C
# ============================================================

panel_c <- ggplot2::ggplot(
  panel_c_data,
  ggplot2::aes(
    xmin = year_index - 0.36,
    xmax = year_index + 0.36,
    ymin = ymin,
    ymax = ymax,
    fill = phase_label
  )
) +
  ggplot2::geom_rect(
    colour = NA
  ) +
  ggplot2::scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    limits = phase_order,
    drop = FALSE,
    name = NULL
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = grid::unit(
        3.2,
        "mm"
      ),
      keyheight = grid::unit(
        3.2,
        "mm"
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(years),
    labels = years,
    expand = ggplot2::expansion(
      mult = c(0.02, 0.02)
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 30),
    breaks = c(
      10,
      20,
      30
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.02)
    )
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Number of reactors",
    title = "Microreactor"
  ) +
  base_theme +
  ggplot2::theme(
    legend.position = "none"
  )


# ============================================================
# 17. COMBINE FIGURE
# ============================================================

pa <- tag_panel(
  panel_a,
  "a"
)

pb <- tag_panel(
  panel_b,
  "b"
)

pc <- tag_panel(
  panel_c,
  "c"
)

bottom_panels <- cowplot::plot_grid(
  pb,
  pc,
  nrow = 1,
  rel_widths = c(
    1,
    1
  ),
  align = "hv",
  axis = "tblr"
)

phase_label_plot <- cowplot::ggdraw() +
  cowplot::draw_label(
    label = "Phase",
    x = 0,
    y = 0.5,
    hjust = 0,
    vjust = 0.5,
    size = LEGEND_TITLE_SIZE,
    fontface = "plain"
  )

phase_legend_inner <- cowplot::plot_grid(
  phase_label_plot,
  legend_phase,
  nrow = 1,
  rel_widths = c(
    0.08,
    0.92
  ),
  align = "h",
  axis = "tb"
)

phase_legend_row <- cowplot::plot_grid(
  NULL,
  phase_legend_inner,
  NULL,
  nrow = 1,
  rel_widths = c(
    0.08,
    0.84,
    0.08
  )
)

figure_core <- cowplot::plot_grid(
  pa,
  bottom_panels,
  phase_legend_row,
  ncol = 1,
  rel_heights = c(
    1.00,
    1.05,
    0.15
  ),
  align = "v",
  axis = "lr"
)

fig01 <- cowplot::ggdraw(
  figure_core
)

print(fig01)


# ============================================================
# 18. SAVE FIGURE
# ============================================================

ggplot2::ggsave(
  filename = png_output,
  plot = fig01,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = pdf_output,
  plot = fig01,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 19. DESCRIPTIVE STATISTICS
# ============================================================

reactor_type_by_year <- plot_data %>%
  dplyr::count(
    publication_year,
    classification_label,
    name = "n_reactors"
  ) %>%
  tidyr::complete(
    publication_year = years,
    classification_label = factor(
      classification_order,
      levels = classification_order
    ),
    fill = list(
      n_reactors = 0
    )
  ) %>%
  dplyr::mutate(
    classification_label = factor(
      classification_label,
      levels = classification_order
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = classification_label,
    values_from = n_reactors
  ) %>%
  dplyr::rename(
    small_modular_reactor =
      `Small modular reactor`,

    microreactor =
      Microreactor
  ) %>%
  dplyr::mutate(
    total_reactors =
      small_modular_reactor +
      microreactor,

    microreactor_share =
      dplyr::if_else(
        total_reactors > 0,
        microreactor /
          total_reactors,
        NA_real_
      )
  ) %>%
  dplyr::arrange(
    publication_year
  )


phase_by_reactor_type_year <- plot_data %>%
  dplyr::filter(
    !is.na(phase_label)
  ) %>%
  dplyr::count(
    publication_year,
    classification_label,
    phase_label,
    name = "n_reactors"
  ) %>%
  tidyr::complete(
    publication_year = years,

    classification_label = factor(
      classification_order,
      levels = classification_order
    ),

    phase_label = factor(
      phase_order,
      levels = phase_order
    ),

    fill = list(
      n_reactors = 0
    )
  ) %>%
  dplyr::mutate(
    classification_label = factor(
      classification_label,
      levels = classification_order
    ),

    phase_label = factor(
      phase_label,
      levels = phase_order
    )
  ) %>%
  dplyr::arrange(
    classification_label,
    publication_year,
    phase_label
  )


phase_by_reactor_type_year_wide <-
  phase_by_reactor_type_year %>%
  tidyr::pivot_wider(
    names_from = phase_label,
    values_from = n_reactors
  ) %>%
  dplyr::rename(
    reactor_type =
      classification_label,

    conceptual_design =
      `Conceptual design`,

    basic_design =
      `Basic design`,

    detailed_design =
      `Detailed design`,

    licensing =
      Licensing,

    construction =
      Construction,

    operational =
      Operational
  ) %>%
  dplyr::mutate(
    total_reactors =
      conceptual_design +
      basic_design +
      detailed_design +
      licensing +
      construction +
      operational
  ) %>%
  dplyr::arrange(
    reactor_type,
    publication_year
  )


summary_descriptives <- tibble::tibble(
  metric = c(
    "included_reactors_2012",
    "included_reactors_2022",
    "included_reactors_2024",
    "small_modular_reactors_2012",
    "small_modular_reactors_2022",
    "small_modular_reactors_2024",
    "microreactors_2012",
    "microreactors_2022",
    "microreactors_2024",
    "microreactor_share_2012",
    "microreactor_share_2022",
    "microreactor_share_2024",
    "peak_year",
    "peak_total_reactors"
  ),

  value = c(
    get_year_value(
      reactor_type_by_year,
      2012,
      total_reactors
    ),

    get_year_value(
      reactor_type_by_year,
      2022,
      total_reactors
    ),

    get_year_value(
      reactor_type_by_year,
      2024,
      total_reactors
    ),

    get_year_value(
      reactor_type_by_year,
      2012,
      small_modular_reactor
    ),

    get_year_value(
      reactor_type_by_year,
      2022,
      small_modular_reactor
    ),

    get_year_value(
      reactor_type_by_year,
      2024,
      small_modular_reactor
    ),

    get_year_value(
      reactor_type_by_year,
      2012,
      microreactor
    ),

    get_year_value(
      reactor_type_by_year,
      2022,
      microreactor
    ),

    get_year_value(
      reactor_type_by_year,
      2024,
      microreactor
    ),

    round(
      get_year_value(
        reactor_type_by_year,
        2012,
        microreactor_share
      ),
      3
    ),

    round(
      get_year_value(
        reactor_type_by_year,
        2022,
        microreactor_share
      ),
      3
    ),

    round(
      get_year_value(
        reactor_type_by_year,
        2024,
        microreactor_share
      ),
      3
    ),

    reactor_type_by_year$
      publication_year[
        which.max(
          reactor_type_by_year$
            total_reactors
        )
      ],

    max(
      reactor_type_by_year$
        total_reactors,
      na.rm = TRUE
    )
  )
)


# ============================================================
# 20. SAVE DESCRIPTIVE STATISTICS
# ============================================================

readr::write_csv(
  reactor_type_by_year,
  reactor_type_output
)

readr::write_csv(
  phase_by_reactor_type_year,
  phase_long_output
)

readr::write_csv(
  phase_by_reactor_type_year_wide,
  phase_wide_output
)

readr::write_csv(
  summary_descriptives,
  summary_output
)


# ============================================================
# 21. VERIFY OUTPUTS
# ============================================================

expected_outputs <- c(
  png_output,
  pdf_output,
  panel_a_output,
  panel_b_output,
  panel_c_output,
  reactor_type_output,
  phase_long_output,
  phase_wide_output,
  summary_output
)

missing_outputs <- expected_outputs[
  !file.exists(expected_outputs)
]

if (length(missing_outputs) > 0) {

  stop(
    paste0(
      "Figure 1 finished, but the following outputs were not created:\n",
      paste(
        missing_outputs,
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# 22. FINAL OUTPUT
# ============================================================

message("")
message("==============================================")
message("FIGURE 1 ANALYSIS COMPLETE")
message("==============================================")
message("Repository root: ", project_dir)
message("Input file: ", input_file)
message("Worksheet: ", sheet_name)
message("PNG: ", png_output)
message("PDF: ", pdf_output)
message("Source data and descriptives: ", data_dir)
message("==============================================")

print(
  reactor_type_by_year
)

print(
  phase_by_reactor_type_year_wide
)

print(
  summary_descriptives
)

fig01
