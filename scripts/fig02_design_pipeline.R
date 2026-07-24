# ============================================================
# Code for the manuscript Gordon, J.A.
#
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces Figure 2: Evolution of the SMR design pipeline and
# mean market readiness across the first three editions of the
# NEA SMR Dashboard.
#
# Panels a-c show the number of SMR designs by region in
# Editions 1, 2 and 3.
#
# Panels d-f show the number of SMR designs by reactor cooling type.
#
# Panels g-i show mean market-readiness scores for financing,
# supply chain, fuel supply, licensing, siting and engagement.
#
# Required input:
# data/fig02_design_pipeline_source_data.xlsx
#
# Outputs:
# outputs/fig02_a-i.png
# outputs/fig02_a-i.pdf
#
# This script can be run from either:
# 1. the repository root, or
# 2. the scripts/ directory.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(stringr)
})

rm(list = ls())
graphics.off()

# ============================================================
# 1. LOCATE REPOSITORY ROOT
# ============================================================

input_filename <- "fig02_design_pipeline_source_data.xlsx"

candidate_project_dirs <- unique(
  normalizePath(
    c(
      getwd(),
      file.path(getwd(), "..")
    ),
    winslash = "/",
    mustWork = FALSE
  )
)

input_candidates <- file.path(
  candidate_project_dirs,
  "data",
  input_filename
)

input_matches <- which(
  file.exists(input_candidates)
)

if (length(input_matches) == 0) {
  stop(
    paste0(
      "The Figure 2 source-data workbook could not be found.\n\n",
      "Expected file:\n",
      "data/",
      input_filename,
      "\n\n",
      "Run the script from either the repository root or the scripts/ directory."
    )
  )
}

project_dir <- candidate_project_dirs[
  input_matches[1]
]

data_file <- file.path(
  project_dir,
  "data",
  input_filename
)

output_dir <- file.path(
  project_dir,
  "outputs"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

png_out <- file.path(
  output_dir,
  "fig02_a-i.png"
)

pdf_out <- file.path(
  output_dir,
  "fig02_a-i.pdf"
)

message("Repository root: ", project_dir)
message("Input workbook: ", data_file)

# ============================================================
# 2. LOAD SOURCE DATA
# ============================================================

available_sheets <- readxl::excel_sheets(
  data_file
)

sheet_name <- if ("fig02" %in% available_sheets) {
  "fig02"
} else if ("fig01" %in% available_sheets) {
  "fig01"
} else {
  available_sheets[1]
}

fig02_data_smrs <- readxl::read_excel(
  path = data_file,
  sheet = sheet_name
)

required_columns <- c(
  "edition",
  "region",
  "cooling_type",
  "licensing",
  "financing",
  "supply chain",
  "fuel",
  "siting",
  "engagement"
)

missing_columns <- setdiff(
  required_columns,
  names(fig02_data_smrs)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The Figure 2 source-data workbook is missing the following columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

message(
  "Loaded ",
  nrow(fig02_data_smrs),
  " rows from worksheet '",
  sheet_name,
  "'."
)

# ============================================================
# 3. COMMON FIGURE SETTINGS
# ============================================================

edition_levels <- c(
  "Edition 1",
  "Edition 2",
  "Edition 3"
)

theme_ne <- ggplot2::theme_minimal(
  base_size = 9
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

    axis.ticks = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    plot.title = ggplot2::element_text(
      size = 10,
      face = "plain",
      hjust = 0.5
    ),

    axis.title = ggplot2::element_text(
      size = 9
    ),

    axis.text.x = ggplot2::element_text(
      size = 8,
      angle = 30,
      hjust = 1
    ),

    axis.text.y = ggplot2::element_text(
      size = 8
    ),

    plot.margin = ggplot2::margin(
      8,
      8,
      8,
      8
    ),

    legend.position = "none"
  )

# ============================================================
# 4. ROW 1: REGION
# ============================================================

region_order <- c(
  "N.America",
  "Europe",
  "Asia",
  "Russia",
  "RoW"
)

region_cols <- c(
  "N.America" = "#4C78A8",
  "Europe"     = "#59A14F",
  "Asia"       = "#9C6ADE",
  "Russia"     = "#E15759",
  "RoW"        = "#9D9D9D"
)

region_df <- fig02_data_smrs %>%
  dplyr::mutate(
    edition = as.character(
      edition
    ),

    region = stringr::str_squish(
      as.character(region)
    ),

    region = dplyr::case_when(
      region == "North America" ~
        "N.America",

      region == "Rest of the World" ~
        "RoW",

      TRUE ~ region
    )
  ) %>%
  dplyr::filter(
    edition %in% c(
      "1",
      "2",
      "3"
    ),
    !is.na(region),
    region != ""
  ) %>%
  dplyr::mutate(
    edition = factor(
      edition,
      levels = c(
        "1",
        "2",
        "3"
      ),
      labels = edition_levels
    ),

    region = factor(
      region,
      levels = region_order
    )
  ) %>%
  dplyr::count(
    edition,
    region,
    .drop = FALSE,
    name = "n"
  )

region_ymax <- 20

make_region_panel <- function(
  edition_name
) {

  ggplot2::ggplot(
    dplyr::filter(
      region_df,
      edition == edition_name
    ),
    ggplot2::aes(
      x = region,
      y = n,
      fill = region
    )
  ) +
    ggplot2::geom_col(
      width = 0.72
    ) +
    ggplot2::scale_fill_manual(
      values = region_cols,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        region_ymax
      ),
      expand = c(
        0,
        0
      ),
      breaks = seq(
        0,
        region_ymax,
        by = 5
      )
    ) +
    ggplot2::labs(
      title = edition_name,
      x = NULL,
      y = "Count"
    ) +
    theme_ne
}

p_a <- make_region_panel(
  "Edition 1"
)

p_b <- make_region_panel(
  "Edition 2"
)

p_c <- make_region_panel(
  "Edition 3"
)

# ============================================================
# 5. ROW 2: REACTOR COOLING TYPE
# ============================================================

cooling_order <- c(
  "Water-cooled",
  "Metal-cooled",
  "Gas-cooled",
  "Salt-cooled"
)

cooling_cols <- c(
  "Water-cooled" = "#6BAED6",
  "Metal-cooled" = "#8F8F8F",
  "Gas-cooled"   = "#F28E2B",
  "Salt-cooled"  = "#4ECDC4"
)

cooling_df <- fig02_data_smrs %>%
  dplyr::mutate(
    edition = as.character(
      edition
    ),

    cooling_type = stringr::str_squish(
      stringr::str_to_lower(
        as.character(cooling_type)
      )
    ),

    cooling_type = dplyr::case_when(
      cooling_type %in% c(
        "water",
        "water-cooled",
        "water cooled",
        "light water",
        "light-water",
        "heavy water",
        "heavy-water"
      ) ~ "Water-cooled",

      cooling_type %in% c(
        "metal",
        "metal-cooled",
        "metal cooled",
        "sodium",
        "sodium-cooled",
        "lead",
        "lead-cooled",
        "lead-bismuth",
        "lead-bismuth-cooled",
        "heat-pipe-cooled",
        "heat pipe cooled",
        "sodium heat-pipe-cooled",
        "sodium heat pipe cooled"
      ) ~ "Metal-cooled",

      cooling_type %in% c(
        "gas",
        "gas-cooled",
        "gas cooled",
        "helium",
        "helium-cooled",
        "co2",
        "carbon dioxide",
        "htr",
        "htgr"
      ) ~ "Gas-cooled",

      cooling_type %in% c(
        "salt",
        "salt-cooled",
        "salt cooled",
        "molten salt",
        "molten-salt"
      ) ~ "Salt-cooled",

      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(
    edition %in% c(
      "1",
      "2",
      "3"
    ),
    !is.na(cooling_type),
    cooling_type != ""
  ) %>%
  dplyr::mutate(
    edition = factor(
      edition,
      levels = c(
        "1",
        "2",
        "3"
      ),
      labels = edition_levels
    ),

    cooling_type = factor(
      cooling_type,
      levels = cooling_order
    )
  ) %>%
  dplyr::count(
    edition,
    cooling_type,
    .drop = FALSE,
    name = "n"
  )

cooling_ymax <- 20

make_cooling_panel <- function(
  edition_name
) {

  ggplot2::ggplot(
    dplyr::filter(
      cooling_df,
      edition == edition_name
    ),
    ggplot2::aes(
      x = cooling_type,
      y = n,
      fill = cooling_type
    )
  ) +
    ggplot2::geom_col(
      width = 0.72
    ) +
    ggplot2::scale_fill_manual(
      values = cooling_cols,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        cooling_ymax
      ),
      expand = c(
        0,
        0
      ),
      breaks = seq(
        0,
        cooling_ymax,
        by = 5
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Count"
    ) +
    theme_ne +
    ggplot2::theme(
      plot.title = ggplot2::element_blank()
    )
}

p_d <- make_cooling_panel(
  "Edition 1"
)

p_e <- make_cooling_panel(
  "Edition 2"
)

p_f <- make_cooling_panel(
  "Edition 3"
)

# ============================================================
# 6. ROW 3: MARKET-READINESS COMPONENTS
# ============================================================

component_order <- c(
  "Financing",
  "Supply Chain",
  "Fuel Supply",
  "Licensing",
  "Siting",
  "Engagement"
)

component_cols <- c(
  "Financing"    = "#2A9D8F",
  "Supply Chain" = "#4C78A8",
  "Fuel Supply"  = "#8E63CE",
  "Licensing"    = "#8C2D3E",
  "Siting"       = "#C7922F",
  "Engagement"   = "#E07A5F"
)

readiness_df <- fig02_data_smrs %>%
  dplyr::mutate(
    edition = as.character(
      edition
    ),

    licensing = suppressWarnings(
      as.numeric(licensing)
    ),

    financing = suppressWarnings(
      as.numeric(financing)
    ),

    `supply chain` = suppressWarnings(
      as.numeric(`supply chain`)
    ),

    fuel = suppressWarnings(
      as.numeric(fuel)
    ),

    siting = suppressWarnings(
      as.numeric(siting)
    ),

    engagement = suppressWarnings(
      as.numeric(engagement)
    )
  ) %>%
  dplyr::filter(
    edition %in% c(
      "1",
      "2",
      "3"
    )
  ) %>%
  dplyr::group_by(
    edition
  ) %>%
  dplyr::summarise(
    financing = mean(
      financing,
      na.rm = TRUE
    ),

    `supply chain` = mean(
      `supply chain`,
      na.rm = TRUE
    ),

    fuel = mean(
      fuel,
      na.rm = TRUE
    ),

    licensing = mean(
      licensing,
      na.rm = TRUE
    ),

    siting = mean(
      siting,
      na.rm = TRUE
    ),

    engagement = mean(
      engagement,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      financing,
      `supply chain`,
      fuel,
      licensing,
      siting,
      engagement
    ),
    names_to = "component",
    values_to = "mean_score"
  ) %>%
  dplyr::mutate(
    edition = factor(
      edition,
      levels = c(
        "1",
        "2",
        "3"
      ),
      labels = edition_levels
    ),

    component = dplyr::recode(
      component,
      financing = "Financing",
      `supply chain` = "Supply Chain",
      fuel = "Fuel Supply",
      licensing = "Licensing",
      siting = "Siting",
      engagement = "Engagement"
    ),

    component = factor(
      component,
      levels = component_order
    )
  )

make_readiness_panel <- function(
  edition_name
) {

  ggplot2::ggplot(
    dplyr::filter(
      readiness_df,
      edition == edition_name
    ),
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
      limits = c(
        0,
        6
      ),
      expand = c(
        0,
        0
      ),
      breaks = 0:6,
      labels = c(
        "",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6"
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Mean score"
    ) +
    theme_ne +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),

      axis.text.x = ggplot2::element_text(
        angle = 30,
        hjust = 1,
        vjust = 1
      )
    )
}

p_g <- make_readiness_panel(
  "Edition 1"
)

p_h <- make_readiness_panel(
  "Edition 2"
)

p_i <- make_readiness_panel(
  "Edition 3"
)

# ============================================================
# 7. COMBINE 3 × 3 FIGURE
# ============================================================

row_1 <- cowplot::plot_grid(
  p_a,
  p_b,
  p_c,
  ncol = 3,
  align = "hv",
  axis = "tblr"
)

row_2 <- cowplot::plot_grid(
  p_d,
  p_e,
  p_f,
  ncol = 3,
  align = "hv",
  axis = "tblr"
)

row_3 <- cowplot::plot_grid(
  p_g,
  p_h,
  p_i,
  ncol = 3,
  align = "hv",
  axis = "tblr"
)

final_plot <- cowplot::plot_grid(
  row_1,
  row_2,
  row_3,
  ncol = 1,
  rel_heights = c(
    1,
    1,
    1
  ),
  align = "v",
  axis = "lr"
)

final_plot <- cowplot::ggdraw(
  final_plot
) +
  cowplot::draw_plot_label(
    label = c(
      "a",
      "b",
      "c",
      "d",
      "e",
      "f",
      "g",
      "h",
      "i"
    ),

    x = c(
      0.01,
      0.34,
      0.67,
      0.01,
      0.34,
      0.67,
      0.01,
      0.34,
      0.67
    ),

    y = c(
      0.99,
      0.99,
      0.99,
      0.66,
      0.66,
      0.66,
      0.33,
      0.33,
      0.33
    ),

    hjust = 0,
    vjust = 1,
    size = 11,
    fontface = "bold"
  )

print(
  final_plot
)

# ============================================================
# 8. SAVE OUTPUTS
# ============================================================

ggplot2::ggsave(
  filename = png_out,
  plot = final_plot,
  width = 180,
  height = 255,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_out,
  plot = final_plot,
  width = 180,
  height = 255,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

# ============================================================
# 9. VERIFY OUTPUTS
# ============================================================

if (!file.exists(png_out)) {
  stop(
    paste0(
      "The PNG output was not created:\n",
      png_out
    )
  )
}

if (!file.exists(pdf_out)) {
  stop(
    paste0(
      "The PDF output was not created:\n",
      pdf_out
    )
  )
}

message("")
message("==============================================")
message("FIGURE 2 COMPLETE")
message("==============================================")
message("Repository root: ", project_dir)
message("Input: ", data_file)
message("Worksheet: ", sheet_name)
message("PNG: ", png_out)
message("PDF: ", pdf_out)
message("==============================================")

final_plot
