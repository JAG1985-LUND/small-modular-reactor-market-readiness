# Code for the manuscript Gordon, J.A.
# Is the world ready for small modular reactors? A longitudinal analysis of market readiness
#
# Reproduces graphical abstract panel: SMR market readiness scores by region.
#
# Points show individual SMR designs grouped by region and coloured by regional category.
# Dashed vertical lines separate regional groups.
#
# Working directory:
# Set the working directory to the repository root or scripts folder before running this script.
#
# Required input:
# data/graphical_abstract_source_data.xlsx
#
# Outputs:
# outputs/graphical_abstract.png
# outputs/graphical_abstract.pdf


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
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

data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(project_dir, "outputs")

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

input_file <- file.path(data_dir, "graphical_abstract_source_data.xlsx")
png_out    <- file.path(out_dir, "graphical_abstract.png")
pdf_out    <- file.path(out_dir, "graphical_abstract.pdf")

df <- read_excel(input_file, sheet = 1)
names(df) <- clean_names_simple(names(df))

region_col <- intersect(c("region", "region_filtered", "regions"), names(df))[1]
score_col  <- intersect(c("market_readiness", "market_readiness_score", "mean_readiness", "score", "readiness"), names(df))[1]

if (is.na(region_col)) region_col <- names(df)[1]
if (is.na(score_col))  score_col  <- names(df)[2]

df <- df %>%
  transmute(
    region = trimws(as.character(.data[[region_col]])),
    market_readiness = suppressWarnings(as.numeric(.data[[score_col]]))
  ) %>%
  filter(!is.na(region), region != "", !is.na(market_readiness)) %>%
  mutate(
    region = case_when(
      tolower(region) %in% c("row", "rest of world", "rest of the world") ~ "Rest of the World",
      tolower(region) %in% c("north america", "north_america", "n.america", "n america") ~ "North America",
      tolower(region) %in% c("oecd asia", "oecd_asia") ~ "OECD Asia",
      tolower(region) == "russia" ~ "Russia",
      tolower(region) == "china" ~ "China",
      tolower(region) == "europe" ~ "Europe",
      TRUE ~ region
    )
  ) %>%
  filter(!tolower(region) %in% c("global")) %>%
  mutate(
    region = factor(
      region,
      levels = c("Russia", "China", "North America", "Europe", "OECD Asia", "Rest of the World")
    )
  ) %>%
  filter(!is.na(region)) %>%
  arrange(region) %>%
  group_by(region) %>%
  mutate(within_region_id = row_number()) %>%
  ungroup()

region_order <- c("Russia", "China", "North America", "Europe", "OECD Asia", "Rest of the World")

group_sizes <- df %>%
  count(region, name = "n") %>%
  mutate(region = factor(region, levels = region_order)) %>%
  arrange(region)

gap <- 1

start_positions <- cumsum(c(1, head(group_sizes$n + gap, -1)))
end_positions   <- start_positions + group_sizes$n - 1
mid_positions   <- (start_positions + end_positions) / 2

group_sizes <- group_sizes %>%
  mutate(
    start = start_positions,
    end   = end_positions,
    mid   = mid_positions
  )

df <- df %>%
  left_join(select(group_sizes, region, start), by = "region") %>%
  mutate(design_id = start + within_region_id - 1)

separator_positions <- head(group_sizes$end + gap / 2, -1)

region_cols <- c(
  "Russia"            = "#D7263D",
  "China"             = "#F28E2B",
  "North America"     = "#4C78A8",
  "Europe"            = "#2A9D8F",
  "OECD Asia"         = "#8E63CE",
  "Rest of the World" = "#9E9E9E"
)

x_label_map <- c(
  "Russia" = "Russia",
  "China" = "China",
  "North America" = "North America",
  "Europe" = "Europe",
  "OECD Asia" = "OECD\nAsia",
  "Rest of the World" = "Rest of\nthe World"
)

p <- ggplot(df, aes(x = design_id, y = market_readiness, colour = region)) +
  geom_point(size = 3.1, alpha = 0.95) +
  geom_vline(
    xintercept = separator_positions,
    colour = "grey85",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  scale_color_manual(values = region_cols, drop = FALSE) +
  scale_x_continuous(
    breaks = group_sizes$mid,
    labels = unname(x_label_map[as.character(group_sizes$region)]),
    minor_breaks = NULL,
    name = "SMR designs by region",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(1, 6),
    breaks = 1:6,
    minor_breaks = NULL,
    name = "Market readiness score"
  ) +
  labs(colour = "Region") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(
      colour = "black",
      size = 9,
      lineheight = 0.9,
      margin = margin(t = 6)
    ),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title = element_text(colour = "black", size = 11),
    legend.title = element_text(size = 10, colour = "black"),
    legend.text = element_text(size = 9, colour = "black"),
    legend.position = "right",
    plot.margin = margin(8, 12, 14, 8)
  )

print(p)

ggsave(
  filename = png_out,
  plot = p,
  width = 180,
  height = 110,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_out,
  plot = p,
  width = 180,
  height = 110,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)
