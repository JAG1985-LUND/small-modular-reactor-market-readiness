# ============================================================
# Code for the manuscript Gordon, J.A.
#
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces the graphical abstract:
# Design-level market-readiness scores grouped by region.
#
# Required input:
# data/graphical_abstract_source_data.xlsx
#
# Outputs:
# outputs/graphical_abstract/graphical_abstract.png
# outputs/graphical_abstract/graphical_abstract.pdf
# outputs/graphical_abstract/graphical_abstract_plot_data.csv
#
# Working directory:
# Run this script from either:
#   1. the repository root; or
#   2. the repository's scripts/ directory.
# ============================================================


# ============================================================
# 1. LOAD PACKAGES AND RESET SESSION
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

rm(list = ls())
graphics.off()


# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}


# ============================================================
# 3. REPOSITORY PATHS
# ============================================================

wd <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

project_dir <- if (basename(wd) == "scripts") {
  dirname(wd)
} else {
  wd
}

data_file <- file.path(
  project_dir,
  "data",
  "graphical_abstract_source_data.xlsx"
)

output_dir <- file.path(
  project_dir,
  "outputs",
  "graphical_abstract"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Graphical abstract source workbook not found:\n",
      data_file,
      "\n\nRun the script from the repository root or scripts/ directory."
    )
  )
}

png_out <- file.path(
  output_dir,
  "graphical_abstract.png"
)

pdf_out <- file.path(
  output_dir,
  "graphical_abstract.pdf"
)

csv_out <- file.path(
  output_dir,
  "graphical_abstract_plot_data.csv"
)

message("Repository root: ", project_dir)
message("Input workbook: ", data_file)
message("Output directory: ", output_dir)


# ============================================================
# 4. SELECT WORKSHEET
# ============================================================

available_sheets <- readxl::excel_sheets(
  data_file
)

if (length(available_sheets) == 0) {
  stop(
    "The graphical abstract workbook contains no worksheets."
  )
}

sheet_name <- if ("graphical_abstract" %in% available_sheets) {
  "graphical_abstract"
} else {
  available_sheets[1]
}


# ============================================================
# 5. LOAD SOURCE DATA
# ============================================================

source_data <- readxl::read_excel(
  path = data_file,
  sheet = sheet_name
)

names(source_data) <- clean_names_simple(
  names(source_data)
)

if (ncol(source_data) < 2) {
  stop(
    "The graphical abstract workbook must contain at least two columns."
  )
}

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
# 6. IDENTIFY REGION AND READINESS COLUMNS
# ============================================================

region_candidates <- c(
  "region",
  "region_filtered",
  "regional_group"
)

readiness_candidates <- c(
  "market_readiness",
  "market_readiness_score",
  "readiness",
  "readiness_score"
)

region_matches <- intersect(
  region_candidates,
  names(source_data)
)

readiness_matches <- intersect(
  readiness_candidates,
  names(source_data)
)

region_column <- if (length(region_matches) > 0) {
  region_matches[1]
} else {
  names(source_data)[1]
}

readiness_column <- if (length(readiness_matches) > 0) {
  readiness_matches[1]
} else {
  names(source_data)[2]
}

message("Region column: ", region_column)
message("Market-readiness column: ", readiness_column)


# ============================================================
# 7. CLEAN AND RECODE DATA
# ============================================================

region_order <- c(
  "Russia",
  "China",
  "North America",
  "Europe",
  "OECD Asia",
  "Rest of the World"
)

plot_data <- data.frame(
  region = source_data[[region_column]],
  market_readiness = source_data[[readiness_column]],
  stringsAsFactors = FALSE
)

plot_data <- plot_data %>%
  dplyr::mutate(
    region = trimws(
      as.character(region)
    ),
    market_readiness = suppressWarnings(
      as.numeric(market_readiness)
    )
  ) %>%
  dplyr::filter(
    !is.na(region),
    region != "",
    !is.na(market_readiness)
  ) %>%
  dplyr::mutate(
    region = dplyr::case_when(
      tolower(region) %in% c(
        "row",
        "rest of world",
        "rest of the world",
        "rest_of_world",
        "rest_of_the_world"
      ) ~ "Rest of the World",

      tolower(region) %in% c(
        "north america",
        "north_america",
        "n.america",
        "n america"
      ) ~ "North America",

      tolower(region) %in% c(
        "oecd asia",
        "oecd_asia"
      ) ~ "OECD Asia",

      tolower(region) == "russia" ~ "Russia",
      tolower(region) == "china" ~ "China",
      tolower(region) == "europe" ~ "Europe",

      TRUE ~ region
    )
  ) %>%
  dplyr::filter(
    tolower(region) != "global"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = region_order
    )
  ) %>%
  dplyr::filter(
    !is.na(region)
  ) %>%
  dplyr::arrange(
    region
  ) %>%
  dplyr::group_by(
    region
  ) %>%
  dplyr::mutate(
    within_region_id = dplyr::row_number()
  ) %>%
  dplyr::ungroup()

if (nrow(plot_data) == 0) {
  stop(
    "No valid regional market-readiness observations were retained."
  )
}


# ============================================================
# 8. CALCULATE GROUP POSITIONS
# ============================================================

group_sizes <- plot_data %>%
  dplyr::count(
    region,
    name = "n"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = region_order
    )
  ) %>%
  dplyr::arrange(
    region
  )

missing_regions <- setdiff(
  region_order,
  as.character(group_sizes$region)
)

if (length(missing_regions) > 0) {
  warning(
    paste0(
      "No observations were available for: ",
      paste(
        missing_regions,
        collapse = ", "
      )
    )
  )
}

gap <- 1

start_positions <- cumsum(
  c(
    1,
    head(
      group_sizes$n + gap,
      -1
    )
  )
)

end_positions <- start_positions +
  group_sizes$n -
  1

mid_positions <- (
  start_positions +
    end_positions
) / 2

group_sizes <- group_sizes %>%
  dplyr::mutate(
    start = start_positions,
    end = end_positions,
    mid = mid_positions
  )

group_position_data <- group_sizes[
  ,
  c(
    "region",
    "start"
  ),
  drop = FALSE
]

plot_data <- plot_data %>%
  dplyr::left_join(
    group_position_data,
    by = "region"
  ) %>%
  dplyr::mutate(
    design_id =
      start +
      within_region_id -
      1
  )

separator_positions <- head(
  group_sizes$end +
    gap / 2,
  -1
)


# ============================================================
# 9. COLOURS AND AXIS LABELS
# ============================================================

region_cols <- c(
  "Russia" = "#D7263D",
  "China" = "#F28E2B",
  "North America" = "#4C78A8",
  "Europe" = "#2A9D8F",
  "OECD Asia" = "#8E63CE",
  "Rest of the World" = "#9E9E9E"
)

x_labels_lookup <- c(
  "Russia" = "Russia",
  "China" = "China",
  "North America" = "North America",
  "Europe" = "Europe",
  "OECD Asia" = "OECD\nAsia",
  "Rest of the World" = "Rest of\nthe World"
)

x_labels <- unname(
  x_labels_lookup[
    as.character(
      group_sizes$region
    )
  ]
)


# ============================================================
# 10. CREATE GRAPHICAL ABSTRACT
# ============================================================

graphical_abstract <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = design_id,
    y = market_readiness,
    colour = region
  )
) +
  ggplot2::geom_vline(
    xintercept = separator_positions,
    colour = "grey85",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 3.1,
    alpha = 0.95
  ) +
  ggplot2::scale_colour_manual(
    values = region_cols,
    breaks = region_order,
    limits = region_order,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = group_sizes$mid,
    labels = x_labels,
    minor_breaks = NULL,
    name = "SMR designs by region",
    expand = ggplot2::expansion(
      mult = c(
        0.02,
        0.02
      )
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      0.75,
      6.25
    ),
    breaks = 1:6,
    minor_breaks = NULL,
    name = "Market readiness score",
    expand = ggplot2::expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  ggplot2::labs(
    colour = "Region"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),

    axis.line.x = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.line.y = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.ticks.x = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.ticks.y = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.text.x = ggplot2::element_text(
      colour = "black",
      size = 9,
      lineheight = 0.9,
      margin = ggplot2::margin(
        t = 6
      )
    ),

    axis.text.y = ggplot2::element_text(
      colour = "black",
      size = 10
    ),

    axis.title = ggplot2::element_text(
      colour = "black",
      size = 11
    ),

    legend.title = ggplot2::element_text(
      size = 10,
      colour = "black"
    ),

    legend.text = ggplot2::element_text(
      size = 9,
      colour = "black"
    ),

    legend.position = "right",

    plot.margin = ggplot2::margin(
      8,
      12,
      14,
      8
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

print(
  graphical_abstract
)


# ============================================================
# 11. EXPORT FIGURE
# ============================================================

ggplot2::ggsave(
  filename = png_out,
  plot = graphical_abstract,
  width = 180,
  height = 110,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = pdf_out,
  plot = graphical_abstract,
  width = 180,
  height = 110,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 12. EXPORT PLOTTING DATA
# ============================================================

plot_data_export <- plot_data %>%
  dplyr::arrange(
    region,
    within_region_id
  ) %>%
  dplyr::mutate(
    region = as.character(region)
  )

plot_data_export <- plot_data_export[
  ,
  c(
    "region",
    "within_region_id",
    "design_id",
    "market_readiness"
  ),
  drop = FALSE
]

readr::write_csv(
  plot_data_export,
  csv_out
)


# ============================================================
# 13. VERIFY OUTPUTS
# ============================================================

expected_outputs <- c(
  png_out,
  pdf_out,
  csv_out
)

missing_outputs <- expected_outputs[
  !file.exists(expected_outputs)
]

if (length(missing_outputs) > 0) {
  stop(
    paste0(
      "The graphical abstract ran, but these outputs were not created:\n",
      paste(
        missing_outputs,
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# 14. COMPLETION MESSAGE
# ============================================================

message("")
message("==============================================")
message("GRAPHICAL ABSTRACT COMPLETE")
message("==============================================")
message("Repository root: ", project_dir)
message("Input workbook: ", data_file)
message("Worksheet: ", sheet_name)
message("Observations plotted: ", nrow(plot_data))
message("PNG: ", png_out)
message("PDF: ", pdf_out)
message("Plot data: ", csv_out)
message("==============================================")

graphical_abstract
