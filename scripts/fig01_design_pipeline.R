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

project_dir <- "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github"

data_file <- file.path(
  project_dir,
  "data",
  "fig01_design_pipeline_source_data.xlsx"
)

png_out <- file.path(
  project_dir,
  "outputs",
  "fig01_a-i.png"
)

pdf_out <- file.path(
  project_dir,
  "outputs",
  "fig01_a-i.pdf"
)

fig01_data_smrs <- read_excel(
  data_file,
  sheet = "fig01"
)

edition_levels <- c("Edition 1", "Edition 2", "Edition 3")

theme_ne <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
    plot.title = element_text(size = 10, face = "plain", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "none"
  )

# =========================
# ROW 1 — REGION
# =========================

region_order <- c("N.America", "Europe", "Asia", "Russia", "RoW")

region_cols <- c(
  "N.America" = "#4C78A8",
  "Europe"    = "#59A14F",
  "Asia"      = "#9C6ADE",
  "Russia"    = "#E15759",
  "RoW"       = "#9D9D9D"
)

region_df <- fig01_data_smrs %>%
  mutate(
    edition = as.character(edition),
    region = str_squish(as.character(region)),
    region = case_when(
      region == "North America" ~ "N.America",
      region == "Rest of the World" ~ "RoW",
      TRUE ~ region
    )
  ) %>%
  filter(
    edition %in% c("1", "2", "3"),
    !is.na(region),
    region != ""
  ) %>%
  mutate(
    edition = factor(edition, levels = c("1", "2", "3"), labels = edition_levels),
    region = factor(region, levels = region_order)
  ) %>%
  count(edition, region, .drop = FALSE)

region_ymax <- 20

make_region_panel <- function(ed) {
  ggplot(filter(region_df, edition == ed), aes(x = region, y = n, fill = region)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = region_cols, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, region_ymax),
      expand = c(0, 0),
      breaks = seq(0, region_ymax, by = 5)
    ) +
    labs(
      title = ed,
      x = NULL,
      y = "Count"
    ) +
    theme_ne
}

p_a <- make_region_panel("Edition 1")
p_b <- make_region_panel("Edition 2")
p_c <- make_region_panel("Edition 3")

# =========================
# ROW 2 — REACTOR COOLING TYPE
# =========================

cooling_order <- c("Water-cooled", "Metal-cooled", "Gas-cooled", "Salt-cooled")

cooling_cols <- c(
  "Water-cooled" = "#6BAED6",
  "Metal-cooled" = "#8F8F8F",
  "Gas-cooled"   = "#F28E2B",
  "Salt-cooled"  = "#4ECDC4"
)

cooling_df <- fig01_data_smrs %>%
  mutate(
    edition = as.character(edition),
    cooling_type = str_squish(str_to_lower(as.character(cooling_type))),
    cooling_type = case_when(
      cooling_type %in% c(
        "water", "water-cooled", "water cooled", "light water",
        "light-water", "heavy water", "heavy-water"
      ) ~ "Water-cooled",
      cooling_type %in% c(
        "metal", "metal-cooled", "metal cooled", "sodium",
        "sodium-cooled", "lead", "lead-cooled", "lead-bismuth",
        "lead-bismuth-cooled", "heat-pipe-cooled",
        "heat pipe cooled", "sodium heat-pipe-cooled",
        "sodium heat pipe cooled"
      ) ~ "Metal-cooled",
      cooling_type %in% c(
        "gas", "gas-cooled", "gas cooled", "helium",
        "helium-cooled", "co2", "carbon dioxide", "htr", "htgr"
      ) ~ "Gas-cooled",
      cooling_type %in% c(
        "salt", "salt-cooled", "salt cooled", "molten salt", "molten-salt"
      ) ~ "Salt-cooled",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    edition %in% c("1", "2", "3"),
    !is.na(cooling_type),
    cooling_type != ""
  ) %>%
  mutate(
    edition = factor(edition, levels = c("1", "2", "3"), labels = edition_levels),
    cooling_type = factor(cooling_type, levels = cooling_order)
  ) %>%
  count(edition, cooling_type, .drop = FALSE)

cooling_ymax <- 20

make_cooling_panel <- function(ed) {
  ggplot(filter(cooling_df, edition == ed), aes(x = cooling_type, y = n, fill = cooling_type)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = cooling_cols, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, cooling_ymax),
      expand = c(0, 0),
      breaks = seq(0, cooling_ymax, by = 5)
    ) +
    labs(
      x = NULL,
      y = "Count"
    ) +
    theme_ne +
    theme(plot.title = element_blank())
}

p_d <- make_cooling_panel("Edition 1")
p_e <- make_cooling_panel("Edition 2")
p_f <- make_cooling_panel("Edition 3")

# =========================
# ROW 3 — READINESS COMPONENTS
# =========================

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

readiness_df <- fig01_data_smrs %>%
  mutate(
    edition = as.character(edition),
    licensing = suppressWarnings(as.numeric(licensing)),
    financing = suppressWarnings(as.numeric(financing)),
    `supply chain` = suppressWarnings(as.numeric(`supply chain`)),
    fuel = suppressWarnings(as.numeric(fuel)),
    siting = suppressWarnings(as.numeric(siting)),
    engagement = suppressWarnings(as.numeric(engagement))
  ) %>%
  filter(edition %in% c("1", "2", "3")) %>%
  group_by(edition) %>%
  summarise(
    financing = mean(financing, na.rm = TRUE),
    `supply chain` = mean(`supply chain`, na.rm = TRUE),
    fuel = mean(fuel, na.rm = TRUE),
    licensing = mean(licensing, na.rm = TRUE),
    siting = mean(siting, na.rm = TRUE),
    engagement = mean(engagement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(financing, `supply chain`, fuel, licensing, siting, engagement),
    names_to = "component",
    values_to = "mean_score"
  ) %>%
  mutate(
    edition = factor(edition, levels = c("1", "2", "3"), labels = edition_levels),
    component = recode(
      component,
      financing = "Financing",
      `supply chain` = "Supply Chain",
      fuel = "Fuel Supply",
      licensing = "Licensing",
      siting = "Siting",
      engagement = "Engagement"
    ),
    component = factor(component, levels = component_order)
  )

make_readiness_panel <- function(ed) {
  ggplot(filter(readiness_df, edition == ed), aes(x = component, y = mean_score, fill = component)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = component_cols, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, 6),
      expand = c(0, 0),
      breaks = 0:6,
      labels = c("", "1", "2", "3", "4", "5", "6")
    ) +
    labs(
      x = NULL,
      y = "Mean score"
    ) +
    theme_ne +
    theme(
      plot.title = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
    )
}

p_g <- make_readiness_panel("Edition 1")
p_h <- make_readiness_panel("Edition 2")
p_i <- make_readiness_panel("Edition 3")

# =========================
# COMBINED 3 × 3 FIGURE
# =========================

row_1 <- plot_grid(p_a, p_b, p_c, ncol = 3, align = "hv", axis = "tblr")
row_2 <- plot_grid(p_d, p_e, p_f, ncol = 3, align = "hv", axis = "tblr")
row_3 <- plot_grid(p_g, p_h, p_i, ncol = 3, align = "hv", axis = "tblr")

final_plot <- plot_grid(
  row_1, row_2, row_3,
  ncol = 1,
  rel_heights = c(1, 1, 1),
  align = "v",
  axis = "lr"
)

final_plot <- ggdraw(final_plot) +
  draw_plot_label(
    label = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
    x = c(0.01, 0.34, 0.67, 0.01, 0.34, 0.67, 0.01, 0.34, 0.67),
    y = c(0.99, 0.99, 0.99, 0.66, 0.66, 0.66, 0.33, 0.33, 0.33),
    hjust = 0,
    vjust = 1,
    size = 11,
    fontface = "bold"
  )

print(final_plot)

ggsave(
  filename = png_out,
  plot = final_plot,
  width = 180,
  height = 255,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = pdf_out,
  plot = final_plot,
  width = 180,
  height = 255,
  units = "mm",
  bg = "white"
)