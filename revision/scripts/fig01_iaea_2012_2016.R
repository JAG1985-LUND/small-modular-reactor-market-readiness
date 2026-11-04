
# ============================================================
# FIGURE 1: IAEA SMR CATALOGUES, 2012–2026
#
# a: SMR and microreactor counts
# b: SMR development phases
# c: Microreactor development phases
#
# Required input:
# revision/data/fig01_iaea_2012_2026_source_data.xlsx
#
# Outputs:
# revision/figures/fig01_iaea_2012_2026.png
# revision/figures/fig01_iaea_2012_2026.pdf
#
# GitHub-compatible version
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(tibble)
})

# ------------------------------------------------------------
# 1. REPOSITORY PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig01_iaea_2012_2026_source_data.xlsx"
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

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# ------------------------------------------------------------
# 2. COLOURS AND CATEGORY ORDER
# ------------------------------------------------------------

years <- seq(2012, 2026, by = 2)

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

type_order <- c(
  "Small modular reactor",
  "Microreactor"
)

type_cols <- c(
  "Small modular reactor" = "#4C9A5F",
  "Microreactor" = "#6A6A6A"
)

# ------------------------------------------------------------
# 3. IMPORT AND HARMONISE
# ------------------------------------------------------------

raw_data <- readxl::read_excel(
  input_file,
  sheet = 1
)

required_cols <- c(
  "publication_year",
  "classification",
  "development_phase_recoded"
)

missing_cols <- setdiff(
  required_cols,
  names(raw_data)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns:\n",
    paste(names(raw_data), collapse = "\n")
  )
}

data <- raw_data %>%
  mutate(
    across(where(is.character), str_squish),
    
    publication_year = as.integer(
      publication_year
    ),
    
    classification = str_replace_all(
      str_to_lower(classification),
      "[ -]+",
      "_"
    ),
    
    development_phase_recoded = str_replace_all(
      str_to_lower(development_phase_recoded),
      "[ -]+",
      "_"
    )
  ) %>%
  filter(
    publication_year %in% years,
    
    classification %in% c(
      "small_modular_reactor",
      "micro_reactor"
    ),
    
    !is.na(development_phase_recoded),
    
    development_phase_recoded != "failed_design"
  ) %>%
  mutate(
    reactor_type = factor(
      if_else(
        classification == "small_modular_reactor",
        "Small modular reactor",
        "Microreactor"
      ),
      levels = type_order
    ),
    
    phase = case_when(
      development_phase_recoded ==
        "conceptual_design" ~ "Conceptual design",
      
      development_phase_recoded ==
        "basic_design" ~ "Basic design",
      
      development_phase_recoded ==
        "detailed_design" ~ "Detailed design",
      
      development_phase_recoded ==
        "licensing" ~ "Licensing",
      
      development_phase_recoded ==
        "construction" ~ "Construction",
      
      development_phase_recoded ==
        "operational" ~ "Operational",
      
      TRUE ~ NA_character_
    ),
    
    phase = factor(
      phase,
      levels = phase_order
    )
  )

if (anyNA(data$phase)) {
  stop("Unrecognised development phases detected.")
}

# ------------------------------------------------------------
# 4. PANEL DATA
# ------------------------------------------------------------

panel_a_data <- data %>%
  count(
    publication_year,
    reactor_type,
    name = "n"
  ) %>%
  complete(
    publication_year = years,
    reactor_type = factor(
      type_order,
      levels = type_order
    ),
    fill = list(n = 0)
  )

panel_b_data <- data %>%
  filter(
    reactor_type == "Small modular reactor"
  ) %>%
  count(
    publication_year,
    phase,
    name = "n"
  ) %>%
  complete(
    publication_year = years,
    phase = factor(
      phase_order,
      levels = phase_order
    ),
    fill = list(n = 0)
  )

panel_c_data <- data %>%
  filter(
    reactor_type == "Microreactor"
  ) %>%
  count(
    publication_year,
    phase,
    name = "n"
  ) %>%
  complete(
    publication_year = years,
    phase = factor(
      phase_order,
      levels = phase_order
    ),
    fill = list(n = 0)
  )

# ------------------------------------------------------------
# 5. CONSISTENCY CHECKS
# ------------------------------------------------------------

check_a <- panel_a_data %>%
  group_by(publication_year) %>%
  summarise(
    total = sum(n),
    .groups = "drop"
  )

check_bc <- bind_rows(
  panel_b_data,
  panel_c_data
) %>%
  group_by(publication_year) %>%
  summarise(
    total = sum(n),
    .groups = "drop"
  )

stopifnot(
  identical(check_a$total, check_bc$total)
)

stopifnot(
  max(
    panel_b_data %>%
      group_by(publication_year) %>%
      summarise(
        n = sum(n),
        .groups = "drop"
      ) %>%
      pull(n)
  ) <= 60
)

stopifnot(
  max(
    panel_c_data %>%
      group_by(publication_year) %>%
      summarise(
        n = sum(n),
        .groups = "drop"
      ) %>%
      pull(n)
  ) <= 30
)

# ------------------------------------------------------------
# 6. COMMON THEME
# ------------------------------------------------------------

base_theme <- theme_classic(base_size = 9) +
  theme(
    text = element_text(colour = "black"),
    
    axis.text = element_text(
      size = 9,
      colour = "grey20"
    ),
    
    axis.title = element_text(
      size = 9,
      colour = "black"
    ),
    
    axis.line = element_line(
      colour = "grey35",
      linewidth = 0.35
    ),
    
    axis.ticks = element_line(
      colour = "grey35",
      linewidth = 0.35
    ),
    
    plot.title = element_text(
      size = 9,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    
    plot.margin = margin(
      t = 5,
      r = 7,
      b = 5,
      l = 7
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# ------------------------------------------------------------
# 7. PANEL A: REACTOR COUNTS
# ------------------------------------------------------------

panel_a <- ggplot(
  panel_a_data,
  aes(
    x = factor(publication_year, levels = years),
    y = n,
    fill = reactor_type
  )
) +
  geom_col(
    width = 0.72,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = type_cols,
    breaks = type_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = c(20, 40, 60, 80),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Year",
    y = "Number of reactors",
    fill = "Reactor type"
  ) +
  base_theme +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0,
      ncol = 1
    )
  ) +
  theme(
    legend.position = c(0.025, 0.98),
    legend.justification = c(0, 1),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.key.size = grid::unit(4, "mm")
  )

# ------------------------------------------------------------
# 8. PANEL B: SMALL MODULAR REACTORS
# ------------------------------------------------------------

panel_b <- ggplot(
  panel_b_data,
  aes(
    x = factor(publication_year, levels = years),
    y = n,
    fill = phase
  )
) +
  geom_col(
    width = 0.72,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(20, 40, 60),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Small modular reactor",
    x = "Year",
    y = "Number of reactors"
  ) +
  base_theme +
  theme(
    legend.position = "none"
  )

# ------------------------------------------------------------
# 9. PANEL C: MICROREACTORS
# ------------------------------------------------------------

panel_c <- ggplot(
  panel_c_data,
  aes(
    x = factor(publication_year, levels = years),
    y = n,
    fill = phase
  )
) +
  geom_col(
    width = 0.72,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = c(10, 20, 30),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Microreactor",
    x = "Year",
    y = "Number of reactors"
  ) +
  base_theme +
  theme(
    legend.position = "none"
  )

# ------------------------------------------------------------
# 10. PANEL LETTERS
# ------------------------------------------------------------

add_panel_letter <- function(p, letter) {
  ggdraw(p) +
    draw_plot_label(
      letter,
      x = 0.005,
      y = 0.995,
      hjust = 0,
      vjust = 1,
      size = 14,
      fontface = "bold"
    )
}

# ------------------------------------------------------------
# 11. HORIZONTAL PHASE LEGEND
# ------------------------------------------------------------

legend_items <- tibble(
  phase = factor(
    phase_order,
    levels = phase_order
  ),
  
  x = c(
    1.20,
    4.05,
    6.35,
    9.10,
    11.05,
    13.35
  )
)

legend_plot <- ggplot() +
  geom_rect(
    data = legend_items,
    aes(
      xmin = x,
      xmax = x + 0.30,
      ymin = 0.38,
      ymax = 0.62,
      fill = phase
    ),
    colour = NA
  ) +
  geom_text(
    data = legend_items,
    aes(
      x = x + 0.40,
      y = 0.5,
      label = phase
    ),
    hjust = 0,
    vjust = 0.5,
    size = 9 / ggplot2::.pt,
    colour = "black"
  ) +
  annotate(
    "text",
    x = 0.10,
    y = 0.5,
    label = "Phase",
    hjust = 0,
    vjust = 0.5,
    size = 9 / ggplot2::.pt
  ) +
  scale_fill_manual(
    values = phase_cols,
    guide = "none"
  ) +
  coord_cartesian(
    xlim = c(0, 15.65),
    ylim = c(0, 1),
    expand = FALSE,
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

# ------------------------------------------------------------
# 12. ASSEMBLE FIGURE
# ------------------------------------------------------------

bottom_panels <- plot_grid(
  add_panel_letter(panel_b, "b"),
  add_panel_letter(panel_c, "c"),
  nrow = 1,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

figure_1 <- plot_grid(
  add_panel_letter(panel_a, "a"),
  bottom_panels,
  legend_plot,
  ncol = 1,
  rel_heights = c(
    1.05,
    1.00,
    0.12
  ),
  align = "v",
  axis = "lr"
)

# ------------------------------------------------------------
# 13. EXPORT MAIN FIGURE
# ------------------------------------------------------------

png_file <- file.path(
  output_dir,
  "fig01_iaea_2012_2026.png"
)

pdf_file <- file.path(
  output_dir,
  "fig01_iaea_2012_2026.pdf"
)

ggsave(
  filename = png_file,
  plot = figure_1,
  width = 180,
  height = 180,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = pdf_file,
  plot = figure_1,
  width = 180,
  height = 180,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

# ------------------------------------------------------------
# 14. FINAL CHECK
# ------------------------------------------------------------

stopifnot(
  file.exists(png_file),
  file.exists(pdf_file)
)

cat("\nFIGURE 1 COMPLETE\n")
cat("Input:", input_file, "\n")
cat("PNG:", png_file, "\n")
cat("PDF:", pdf_file, "\n\n")

print(check_a)

print(figure_1)
