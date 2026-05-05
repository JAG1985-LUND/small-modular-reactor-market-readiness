# Code for the manuscript Gordon, J.A.
# Is the world ready for small modular reactors? A longitudinal analysis of market readiness
#
# Reproduces Figure 4: Technological composition of small modular reactor designs by region.
#
# Panels show regional shares of SMR designs by reactor cooling type, neutron spectrum,
# fuel type and fuel enrichment category.
#
# Shares are calculated within each regional group.
#
# Working directory:
# Set the working directory to the repository root or scripts folder before running this script.
#
# Required input:
# data/fig04_technology_source_data.xlsx
#
# Outputs:
# outputs/fig04_a-d.png
# outputs/fig04_a-d.pdf


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(scales)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_dir <- if (basename(wd) == "scripts") dirname(wd) else wd

dir.create(
  file.path(project_dir, "outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)

data_file <- file.path(
  project_dir,
  "data",
  "fig04_technology_source_data.xlsx"
)

sheet_name <- if ("fig04" %in% excel_sheets(data_file)) "fig04" else excel_sheets(data_file)[1]

png_out <- file.path(
  project_dir,
  "outputs",
  "fig04_a-d.png"
)

pdf_out <- file.path(
  project_dir,
  "outputs",
  "fig04_a-d.pdf"
)

df <- readxl::read_excel(data_file, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))

required_cols <- c(
  "region_filtered",
  "water_cooled",
  "spectrum",
  "fuel_type_recoded",
  "fuel_enrichment_recode"
)

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

region_order <- c("North America", "Europe", "OECD Asia", "China", "Russia")
plot_region_order <- rev(region_order)

analysis_df <- df %>%
  mutate(
    region_filtered = stringr::str_squish(as.character(region_filtered)),
    water_cooled = stringr::str_squish(as.character(water_cooled)),
    spectrum = stringr::str_squish(as.character(spectrum)),
    fuel_type_recoded = stringr::str_squish(as.character(fuel_type_recoded)),
    fuel_enrichment_recode = stringr::str_squish(as.character(fuel_enrichment_recode))
  ) %>%
  mutate(
    region_filtered = case_when(
      region_filtered %in% region_order ~ region_filtered,
      TRUE ~ NA_character_
    ),
    water_cooled = case_when(
      tolower(water_cooled) %in% c("water_cooled", "water-cooled", "water cooled") ~ "Water-cooled",
      tolower(water_cooled) %in% c("non_water_cooled", "non-water-cooled", "non water cooled") ~ "Non-water-cooled",
      TRUE ~ NA_character_
    ),
    spectrum = case_when(
      tolower(spectrum) == "thermal" ~ "Thermal",
      tolower(spectrum) == "fast" ~ "Fast",
      TRUE ~ NA_character_
    ),
    fuel_type_recoded = case_when(
      tolower(fuel_type_recoded) == "uranium" ~ "Uranium",
      tolower(fuel_type_recoded) == "triso" ~ "TRISO",
      tolower(fuel_type_recoded) == "other" ~ "Other",
      TRUE ~ NA_character_
    ),
    fuel_enrichment_recode = case_when(
      tolower(fuel_enrichment_recode) == "leu" ~ "LEU",
      tolower(fuel_enrichment_recode) == "haleu" ~ "HALEU",
      tolower(fuel_enrichment_recode) == "other" ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(region_filtered)) %>%
  mutate(
    region_filtered = factor(region_filtered, levels = plot_region_order)
  )

water_cols <- c(
  "Water-cooled"     = "#6BAED6",
  "Non-water-cooled" = "#E88A87"
)

spectrum_cols <- c(
  "Thermal" = "#4C78A8",
  "Fast"    = "#F28E2B"
)

fuel_type_cols <- c(
  "Uranium" = "#E15759",
  "TRISO"   = "#59A14F",
  "Other"   = "#9D9D9D"
)

fuel_enrichment_cols <- c(
  "LEU"   = "#E15759",
  "HALEU" = "#9C6ADE",
  "Other" = "#9D9D9D"
)

make_comp_df <- function(data, var_name, var_levels) {
  tmp <- data.frame(
    region_filtered = data$region_filtered,
    category = as.character(data[[var_name]]),
    stringsAsFactors = FALSE
  )
  
  tmp <- tmp[!is.na(tmp$region_filtered) & !is.na(tmp$category), , drop = FALSE]
  tmp$category <- factor(tmp$category, levels = var_levels)
  tmp <- tmp[!is.na(tmp$category), , drop = FALSE]
  
  tmp %>%
    count(region_filtered, category, name = "n") %>%
    complete(
      region_filtered = factor(plot_region_order, levels = plot_region_order),
      category = factor(var_levels, levels = var_levels),
      fill = list(n = 0)
    ) %>%
    group_by(region_filtered) %>%
    mutate(
      total = sum(n),
      share = ifelse(total > 0, n / total, 0)
    ) %>%
    ungroup()
}

water_df <- make_comp_df(
  data = analysis_df,
  var_name = "water_cooled",
  var_levels = c("Water-cooled", "Non-water-cooled")
)

spectrum_df <- make_comp_df(
  data = analysis_df,
  var_name = "spectrum",
  var_levels = c("Thermal", "Fast")
)

fuel_type_df <- make_comp_df(
  data = analysis_df,
  var_name = "fuel_type_recoded",
  var_levels = c("Uranium", "TRISO", "Other")
)

fuel_enrichment_df <- make_comp_df(
  data = analysis_df,
  var_name = "fuel_enrichment_recode",
  var_levels = c("LEU", "HALEU", "Other")
)

panel_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5, colour = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, colour = "black"),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(6, 10, 6, 6)
  )

make_panel <- function(plot_df, title_text, fill_values, legend_breaks) {
  ggplot(plot_df, aes(x = region_filtered, y = share, fill = category)) +
    geom_col(
      width = 0.75,
      colour = NA,
      position = position_stack(reverse = TRUE)
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.04))
    ) +
    scale_fill_manual(
      values = fill_values,
      breaks = legend_breaks,
      drop = FALSE
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = "Share"
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE
      )
    ) +
    panel_theme
}

p_a <- make_panel(
  plot_df = water_df,
  title_text = "Reactor cooling type",
  fill_values = water_cols,
  legend_breaks = c("Water-cooled", "Non-water-cooled")
)

p_b <- make_panel(
  plot_df = spectrum_df,
  title_text = "Neutron spectrum",
  fill_values = spectrum_cols,
  legend_breaks = c("Thermal", "Fast")
)

p_c <- make_panel(
  plot_df = fuel_type_df,
  title_text = "Fuel type",
  fill_values = fuel_type_cols,
  legend_breaks = c("Uranium", "TRISO", "Other")
)

p_d <- make_panel(
  plot_df = fuel_enrichment_df,
  title_text = "Fuel enrichment",
  fill_values = fuel_enrichment_cols,
  legend_breaks = c("LEU", "HALEU", "Other")
)

combined_plot <- cowplot::plot_grid(
  p_a, p_b, p_c, p_d,
  ncol = 2,
  labels = c("a", "b", "c", "d"),
  label_size = 11,
  label_fontface = "bold",
  label_x = 0.01,
  label_y = 0.99,
  hjust = 0,
  vjust = 1,
  align = "hv"
)

print(combined_plot)

ggsave(
  filename = png_out,
  plot = combined_plot,
  width = 180,
  height = 140,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_out,
  plot = combined_plot,
  width = 180,
  height = 140,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)
