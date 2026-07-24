# Code for the manuscript Gordon, J.A.
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces Supplementary Figure 1: Market-readiness dimensions across
# filtered regions and the global SMR design sample.
#
# Panels show mean readiness scores for North America, Europe, OECD Asia,
# China, Russia and the global SMR design sample across six dashboard
# dimensions: financing, supply chain, fuel supply, licensing, siting
# and engagement.
#
# Higher scores indicate more advanced readiness within each dimension.
#
# Working directory:
# Run this script from either the repository root or the scripts folder.
#
# Required input:
# data/figS1_regions_source_data.xlsx
#
# Outputs:
# outputs/figure_S1/figS1_regions.png
# outputs/figure_S1/figS1_regions.pdf
# outputs/figure_S1/figS1_regions_panel_values.csv


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()


# ============================================================
# HELPERS
# ============================================================

clean_names_simple <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


# ============================================================
# REPOSITORY PATHS
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

file_path <- file.path(
  project_dir,
  "data",
  "figS1_regions_source_data.xlsx"
)

out_dir <- file.path(
  project_dir,
  "outputs",
  "figure_S1"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(file_path)) {
  stop(
    paste0(
      "Input file not found:\n",
      file_path,
      "\n\nRun the script from the repository root or scripts folder."
    )
  )
}


# ============================================================
# WORKSHEET AND OUTPUT FILES
# ============================================================

available_sheets <- readxl::excel_sheets(file_path)

sheet_name <- if ("figS1" %in% available_sheets) {
  "figS1"
} else if ("figs1" %in% available_sheets) {
  "figs1"
} else if ("fig05" %in% available_sheets) {
  "fig05"
} else {
  available_sheets[1]
}

png_out <- file.path(
  out_dir,
  "figS1_regions.png"
)

pdf_out <- file.path(
  out_dir,
  "figS1_regions.pdf"
)

csv_out <- file.path(
  out_dir,
  "figS1_regions_panel_values.csv"
)


# ============================================================
# COLOURS AND ORDERING
# ============================================================

component_cols <- c(
  "Financing" = "#2A9D8F",
  "Supply Chain" = "#4C78A8",
  "Fuel Supply" = "#8E63CE",
  "Licensing" = "#8C2D3E",
  "Siting" = "#C7922F",
  "Engagement" = "#E07A5F"
)

component_order <- c(
  "Financing",
  "Supply Chain",
  "Fuel Supply",
  "Licensing",
  "Siting",
  "Engagement"
)

region_order <- c(
  "North America",
  "Europe",
  "OECD Asia",
  "China",
  "Russia"
)

panel_order <- c(
  region_order,
  "Global"
)


# ============================================================
# LOAD DATA
# ============================================================

df <- readxl::read_excel(
  path = file_path,
  sheet = sheet_name
)

names(df) <- clean_names_simple(
  names(df)
)


# ============================================================
# REQUIRED COLUMNS
# ============================================================

required_cols <- c(
  "region_filtered",
  "licensing",
  "siting",
  "financing",
  "supply_chain",
  "engagement",
  "fuel"
)

missing_cols <- setdiff(
  required_cols,
  names(df)
)

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# PREPARE DATA
# ============================================================

# Base-R column selection avoids conflicts with other select() functions.
analysis_df <- df[
  ,
  required_cols,
  drop = FALSE
]

analysis_df <- analysis_df %>%
  dplyr::mutate(
    region_filtered = trimws(
      as.character(region_filtered)
    ),
    licensing = suppressWarnings(
      as.numeric(licensing)
    ),
    siting = suppressWarnings(
      as.numeric(siting)
    ),
    financing = suppressWarnings(
      as.numeric(financing)
    ),
    supply_chain = suppressWarnings(
      as.numeric(supply_chain)
    ),
    engagement = suppressWarnings(
      as.numeric(engagement)
    ),
    fuel = suppressWarnings(
      as.numeric(fuel)
    )
  )

regional_df <- analysis_df %>%
  dplyr::filter(
    region_filtered %in% region_order
  )


# ============================================================
# REGIONAL MEANS
# ============================================================

regional_means <- regional_df %>%
  tidyr::pivot_longer(
    cols = c(
      financing,
      supply_chain,
      fuel,
      licensing,
      siting,
      engagement
    ),
    names_to = "component_raw",
    values_to = "score"
  ) %>%
  dplyr::mutate(
    component = dplyr::recode(
      component_raw,
      financing = "Financing",
      supply_chain = "Supply Chain",
      fuel = "Fuel Supply",
      licensing = "Licensing",
      siting = "Siting",
      engagement = "Engagement"
    )
  ) %>%
  dplyr::group_by(
    region_filtered,
    component
  ) %>%
  dplyr::summarise(
    mean_score = if (
      all(is.na(score))
    ) {
      NA_real_
    } else {
      mean(score, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  dplyr::rename(
    panel = region_filtered
  )


# ============================================================
# GLOBAL MEANS
# ============================================================

global_means <- analysis_df %>%
  tidyr::pivot_longer(
    cols = c(
      financing,
      supply_chain,
      fuel,
      licensing,
      siting,
      engagement
    ),
    names_to = "component_raw",
    values_to = "score"
  ) %>%
  dplyr::mutate(
    component = dplyr::recode(
      component_raw,
      financing = "Financing",
      supply_chain = "Supply Chain",
      fuel = "Fuel Supply",
      licensing = "Licensing",
      siting = "Siting",
      engagement = "Engagement"
    ),
    panel = "Global"
  ) %>%
  dplyr::group_by(
    panel,
    component
  ) %>%
  dplyr::summarise(
    mean_score = if (
      all(is.na(score))
    ) {
      NA_real_
    } else {
      mean(score, na.rm = TRUE)
    },
    .groups = "drop"
  )


# ============================================================
# COMBINE PANEL DATA
# ============================================================

plot_df <- dplyr::bind_rows(
  regional_means,
  global_means
) %>%
  dplyr::mutate(
    panel = factor(
      panel,
      levels = panel_order
    ),
    component = factor(
      component,
      levels = component_order
    )
  )


# ============================================================
# PANEL FUNCTION
# ============================================================

make_panel_plot <- function(panel_name, tag_letter) {

  dat <- plot_df %>%
    dplyr::filter(
      panel == panel_name
    )

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = component,
      y = mean_score,
      fill = component
    )
  ) +
    ggplot2::geom_col(
      width = 0.72
    ) +
    ggplot2::scale_fill_manual(
      values = component_cols,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 6),
      breaks = 0:6,
      expand = ggplot2::expansion(
        mult = c(0, 0.02)
      )
    ) +
    ggplot2::labs(
      title = panel_name,
      x = NULL,
      y = "Mean score"
    ) +
    ggplot2::theme_minimal(
      base_size = 7
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 8
      ),
      axis.text.x = ggplot2::element_text(
        angle = 30,
        hjust = 1,
        vjust = 1,
        size = 7
      ),
      axis.text.y = ggplot2::element_text(
        size = 7
      ),
      axis.title.y = ggplot2::element_text(
        size = 7
      ),
      axis.title.x = ggplot2::element_blank(),
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
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(
        4,
        6,
        4,
        6
      )
    )

  cowplot::ggdraw(p) +
    cowplot::draw_plot_label(
      label = tag_letter,
      x = 0.01,
      y = 0.985,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}


# ============================================================
# BUILD PANELS
# ============================================================

p1 <- make_panel_plot(
  "North America",
  "a"
)

p2 <- make_panel_plot(
  "Europe",
  "b"
)

p3 <- make_panel_plot(
  "OECD Asia",
  "c"
)

p4 <- make_panel_plot(
  "China",
  "d"
)

p5 <- make_panel_plot(
  "Russia",
  "e"
)

p6 <- make_panel_plot(
  "Global",
  "f"
)

combined_plot <- cowplot::plot_grid(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

print(
  combined_plot
)


# ============================================================
# EXPORT FIGURE
# ============================================================

ggplot2::ggsave(
  filename = png_out,
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = pdf_out,
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# EXPORT PANEL VALUES
# ============================================================

source_data_wide <- plot_df %>%
  dplyr::mutate(
    panel = as.character(panel)
  ) %>%
  tidyr::pivot_wider(
    names_from = component,
    values_from = mean_score
  ) %>%
  dplyr::arrange(
    match(
      panel,
      panel_order
    )
  )

utils::write.csv(
  source_data_wide,
  csv_out,
  row.names = FALSE
)


# ============================================================
# VERIFY OUTPUTS
# ============================================================

if (!file.exists(png_out)) {
  stop(
    paste0(
      "Supplementary Figure 1 PNG was not created:\n",
      png_out
    )
  )
}

if (!file.exists(pdf_out)) {
  stop(
    paste0(
      "Supplementary Figure 1 PDF was not created:\n",
      pdf_out
    )
  )
}

if (!file.exists(csv_out)) {
  stop(
    paste0(
      "Supplementary Figure 1 panel-values CSV was not created:\n",
      csv_out
    )
  )
}


# ============================================================
# COMPLETION MESSAGE
# ============================================================

message("")
message("Supplementary Figure 1 completed successfully.")
message("Repository root: ", project_dir)
message("Input: ", file_path)
message("Worksheet: ", sheet_name)
message("PNG: ", png_out)
message("PDF: ", pdf_out)
message("Panel values: ", csv_out)
