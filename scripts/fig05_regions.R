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
# INPUTS
# ============================================================

file_path  <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/data/fig05_regions_source_data.xlsx"
sheet_name <- 1
out_dir    <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github/outputs"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

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
# COLORS
# ============================================================

component_cols <- c(
  "Financing"    = "#2A9D8F",
  "Supply Chain" = "#4C78A8",
  "Fuel Supply"  = "#8E63CE",
  "Licensing"    = "#8C2D3E",
  "Siting"       = "#C7922F",
  "Engagement"   = "#E07A5F"
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

panel_order <- c(region_order, "Global")
panel_tags  <- c("a", "b", "c", "d", "e", "f")

# ============================================================
# LOAD DATA
# ============================================================

df <- read_excel(file_path, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))

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

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# ============================================================
# PREPARE DATA
# ============================================================

analysis_df <- df %>%
  dplyr::select(all_of(required_cols)) %>%
  mutate(
    region_filtered = trimws(as.character(region_filtered)),
    licensing    = suppressWarnings(as.numeric(licensing)),
    siting       = suppressWarnings(as.numeric(siting)),
    financing    = suppressWarnings(as.numeric(financing)),
    supply_chain = suppressWarnings(as.numeric(supply_chain)),
    engagement   = suppressWarnings(as.numeric(engagement)),
    fuel         = suppressWarnings(as.numeric(fuel))
  )

regional_df <- analysis_df %>%
  filter(region_filtered %in% region_order)

# ============================================================
# REGIONAL MEANS
# ============================================================

regional_means <- regional_df %>%
  pivot_longer(
    cols = c(financing, supply_chain, fuel, licensing, siting, engagement),
    names_to = "component_raw",
    values_to = "score"
  ) %>%
  mutate(
    component = dplyr::recode(
      component_raw,
      financing    = "Financing",
      supply_chain = "Supply Chain",
      fuel         = "Fuel Supply",
      licensing    = "Licensing",
      siting       = "Siting",
      engagement   = "Engagement"
    )
  ) %>%
  group_by(region_filtered, component) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  rename(panel = region_filtered)

# ============================================================
# GLOBAL MEANS
# ============================================================

global_means <- analysis_df %>%
  pivot_longer(
    cols = c(financing, supply_chain, fuel, licensing, siting, engagement),
    names_to = "component_raw",
    values_to = "score"
  ) %>%
  mutate(
    component = dplyr::recode(
      component_raw,
      financing    = "Financing",
      supply_chain = "Supply Chain",
      fuel         = "Fuel Supply",
      licensing    = "Licensing",
      siting       = "Siting",
      engagement   = "Engagement"
    ),
    panel = "Global"
  ) %>%
  group_by(panel, component) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")

plot_df <- bind_rows(regional_means, global_means) %>%
  mutate(
    panel = factor(panel, levels = panel_order),
    component = factor(component, levels = component_order)
  )

# ============================================================
# PANEL FUNCTION
# ============================================================

make_panel_plot <- function(panel_name, tag_letter) {
  dat <- plot_df %>%
    filter(panel == panel_name)
  
  p <- ggplot(dat, aes(x = component, y = mean_score, fill = component)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = component_cols, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, 6),
      breaks = 0:6,
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = panel_name,
      x = NULL,
      y = "Mean score"
    ) +
    theme_minimal(base_size = 7) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks.y = element_line(colour = "grey35", linewidth = 0.3),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      plot.margin = margin(4, 6, 4, 6)
    )
  
  ggdraw(p) +
    draw_plot_label(
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

p1 <- make_panel_plot("North America", "a")
p2 <- make_panel_plot("Europe", "b")
p3 <- make_panel_plot("OECD Asia", "c")
p4 <- make_panel_plot("China", "d")
p5 <- make_panel_plot("Russia", "e")
p6 <- make_panel_plot("Global", "f")

combined_plot <- plot_grid(
  p1, p2,
  p3, p4,
  p5, p6,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

print(combined_plot)

# ============================================================
# EXPORT FIGURE
# ============================================================

ggsave(
  filename = file.path(out_dir, "fig05_regions.png"),
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = file.path(out_dir, "fig05_regions.pdf"),
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

# ============================================================
# OPTIONAL WIDE TABLE OF PANEL VALUES
# ============================================================

source_data_wide <- plot_df %>%
  mutate(panel = as.character(panel)) %>%
  tidyr::pivot_wider(
    names_from = component,
    values_from = mean_score
  ) %>%
  arrange(match(panel, panel_order))

write.csv(
  source_data_wide,
  file.path(out_dir, "fig05_regions_panel_values.csv"),
  row.names = FALSE
)