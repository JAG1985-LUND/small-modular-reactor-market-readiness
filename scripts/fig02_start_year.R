# Code for the manuscript Gordon, J.A.
# Is the world ready for small modular reactors? A longitudinal analysis of market readiness
#
# Reproduces Figure 2: Aggregate SMR market readiness by conceptual design start year.
#
# Panel a shows the relationship between mean market readiness and conceptual design
# start year across the full sample of assessed SMR designs from 1984 to 2025.
# Panel b restricts the sample to designs initiated between 2009 and 2018.
# Panel c restricts the sample to designs initiated between 2019 and 2025.
#
# Points represent individual SMR designs, the blue line shows the fitted linear trend,
# and the shaded area indicates the 95% confidence interval.
#
# Working directory:
# Set the working directory to the repository root before running this script.
#
# Required input:
# data/fig02_start_year_source_data.xlsx
#
# Outputs:
# outputs/fig02_a-c.png
# outputs/fig02_a-c.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

# Working directory should be set to the repository root before running this script.
project_dir <- getwd()

data_file <- file.path(
  project_dir,
  "data",
  "fig02_start_year_source_data.xlsx"
)

png_out <- file.path(
  project_dir,
  "outputs",
  "fig02_a-c.png"
)

pdf_out <- file.path(
  project_dir,
  "outputs",
  "fig02_a-c.pdf"
)

df <- read_excel(
  data_file,
  sheet = "fig02"
)

names(df) <- clean_names_simple(names(df))

plot_df <- df %>%
  transmute(
    design = as.character(design),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    mean_readiness = suppressWarnings(as.numeric(mean_readiness))
  ) %>%
  filter(!is.na(cd_start), !is.na(mean_readiness)) %>%
  arrange(cd_start, design)

df_a <- plot_df

df_b <- plot_df %>%
  filter(cd_start >= 2009, cd_start <= 2018)

df_c <- plot_df %>%
  filter(cd_start >= 2019, cd_start <= 2025)

df_a <- df_a %>%
  mutate(
    cohort = case_when(
      cd_start >= 2009 & cd_start <= 2018 ~ "B",
      cd_start >= 2019 & cd_start <= 2025 ~ "C",
      TRUE ~ "Other"
    )
  )

col_other <- "black"
col_b     <- "#C97A2B"
col_c     <- "#4F8A3A"
line_col  <- "#1F5AC6"
band_fill <- "grey80"

theme_panel <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 11, colour = "black"),
    axis.title.y = element_text(size = 11, colour = "black"),
    plot.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(4, 6, 4, 6)
  )

p_a <- ggplot(df_a, aes(x = cd_start, y = mean_readiness)) +
  geom_point(
    data = subset(df_a, cohort == "Other"),
    colour = col_other,
    size = 2.4,
    alpha = 0.85
  ) +
  geom_point(
    data = subset(df_a, cohort == "B"),
    colour = col_b,
    size = 2.4,
    alpha = 0.95
  ) +
  geom_point(
    data = subset(df_a, cohort == "C"),
    colour = col_c,
    size = 2.4,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = line_col,
    fill = band_fill,
    linewidth = 0.9
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = c(1985, 1995, 2005, 2015, 2025),
    limits = c(1984, 2025.5),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  scale_y_continuous(
    name = "Market readiness (mean)",
    breaks = 1:6,
    limits = c(1, 6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_panel

p_b <- ggplot(df_b, aes(x = cd_start, y = mean_readiness)) +
  geom_point(
    size = 2.4,
    colour = col_b,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = line_col,
    fill = band_fill,
    linewidth = 0.9
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = c(2009, 2012, 2015, 2018),
    limits = c(2008.8, 2018.2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "Market readiness (mean)",
    breaks = 1:6,
    limits = c(1, 6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_panel

p_c <- ggplot(df_c, aes(x = cd_start, y = mean_readiness)) +
  geom_point(
    size = 2.4,
    colour = col_c,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = line_col,
    fill = band_fill,
    linewidth = 0.9
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = c(2019, 2021, 2023, 2025),
    limits = c(2018.8, 2025.2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "Market readiness (mean)",
    breaks = 1:6,
    limits = c(1, 6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_panel +
  theme(
    axis.title.y = element_text(colour = "white")
  )

bottom_row <- plot_grid(
  p_b,
  p_c,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

shared_x <- ggdraw() +
  draw_label(
    "Conceptual design start year",
    x = 0.5,
    y = 0.52,
    hjust = 0.5,
    vjust = 0.5,
    size = 11,
    fontface = "plain"
  )

bottom_block <- plot_grid(
  bottom_row,
  shared_x,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

combined_plot <- plot_grid(
  p_a,
  bottom_block,
  ncol = 1,
  rel_heights = c(1.12, 1)
)

combined_plot <- ggdraw(combined_plot) +
  draw_plot_label(
    label = c("a", "b", "c"),
    x = c(0.014, 0.014, 0.515),
    y = c(0.989, 0.472, 0.472),
    hjust = 0,
    vjust = 1,
    size = 12,
    fontface = "bold"
  )

print(combined_plot)

ggsave(
  filename = png_out,
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = pdf_out,
  plot = combined_plot,
  width = 180,
  height = 210,
  units = "mm",
  bg = "white"
)
