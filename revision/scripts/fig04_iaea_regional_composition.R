
# ============================================================
# FIGURE 4
# REGIONAL COMPOSITION OF THE GLOBAL SMR LANDSCAPE
# IAEA SMR CATALOGUES, 2012–2026
#
# Panels:
#   a. All designs
#   b. Conventional SMRs
#   c. Microreactors
#
# Regional categories:
#   North America
#   Russia
#   Asia
#   Europe
#   Rest of the World
#
# Input:
# revision/data/fig04_iaea_regional_composition_source_data.xlsx
#
# Outputs:
# revision/figures/fig04_iaea_regional_composition.png
# revision/figures/fig04_iaea_regional_composition.pdf
#
# Export size: 180 x 205 mm
# Resolution: 600 dpi
# GitHub-compatible version
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "cowplot",
  "grid",
  "gridExtra"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(cowplot)
  library(grid)
  library(gridExtra)
})


# ============================================================
# 2. GITHUB-COMPATIBLE FILE PATHS
# ============================================================

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig04_iaea_regional_composition_source_data.xlsx"
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

FILE_IN <- file.path(
  ROOT_DIR,
  SOURCE_FILE
)

OUT_DIR <- file.path(
  ROOT_DIR,
  "revision",
  "figures"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

PNG_OUT <- file.path(
  OUT_DIR,
  "fig04_iaea_regional_composition.png"
)

PDF_OUT <- file.path(
  OUT_DIR,
  "fig04_iaea_regional_composition.pdf"
)

if (!file.exists(FILE_IN)) {
  stop("Input workbook not found: ", FILE_IN)
}

years <- seq(2012, 2026, by = 2)


# ============================================================
# 3. IMPORT DATA
# ============================================================

clean_names <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

df <- read_excel(FILE_IN)

names(df) <- clean_names(names(df))

required_columns <- c(
  "publication_year",
  "smr_design",
  "region",
  "classification"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- df %>%
  mutate(
    publication_year = as.integer(publication_year),
    smr_design = str_squish(as.character(smr_design)),
    region = str_to_lower(
      str_squish(as.character(region))
    ),
    classification = str_to_lower(
      str_squish(as.character(classification))
    )
  ) %>%
  filter(publication_year %in% years)


# ============================================================
# 4. EXCLUDE FAILED DESIGNS, IF FLAGGED
# ============================================================

# Only explicitly flagged failed designs are excluded.
# Stalled designs are retained.
#
# If the Figure 4 workbook has already been filtered,
# this step leaves it unchanged.

if ("failed_design" %in% names(df)) {
  
  failed_flag <- tolower(
    trimws(as.character(df$failed_design))
  )
  
  exclude_failed <- failed_flag %in% c(
    "true",
    "t",
    "1",
    "yes",
    "y"
  )
  
  cat(
    "\nExplicitly flagged failed records excluded:",
    sum(exclude_failed),
    "\n"
  )
  
  df <- df[!exclude_failed, , drop = FALSE]
}


# ============================================================
# 5. REACTOR CLASSIFICATION
# ============================================================

df <- df %>%
  mutate(
    reactor_group = case_when(
      classification %in% c(
        "small_modular_reactor",
        "conventional_smr",
        "smr"
      ) ~ "Conventional SMRs",
      
      classification %in% c(
        "micro_reactor",
        "microreactor"
      ) ~ "Microreactors",
      
      TRUE ~ NA_character_
    )
  )

unclassified_reactors <- df %>%
  filter(is.na(reactor_group)) %>%
  count(classification, name = "n")

if (nrow(unclassified_reactors) > 0) {
  
  print(unclassified_reactors, n = Inf)
  
  stop(
    "Unrecognised reactor classifications detected."
  )
}


# ============================================================
# 6. REGIONAL CATEGORIES AND COLOURS
# ============================================================

region_levels <- c(
  "North America",
  "Russia",
  "Asia",
  "Europe",
  "Rest of the World"
)

region_cols <- c(
  "North America" = "#4C78A8",
  "Russia" = "#E15759",
  "Asia" = "#9C6ADE",
  "Europe" = "#59A14F",
  "Rest of the World" = "#9D9D9D"
)

df <- df %>%
  mutate(
    region_plot = case_when(
      region %in% c(
        "north america",
        "north_america",
        "n.america"
      ) ~ "North America",
      
      region == "russia" ~ "Russia",
      
      region == "asia" ~ "Asia",
      
      region == "europe" ~ "Europe",
      
      region %in% c(
        "row",
        "rest of the world",
        "rest_of_the_world"
      ) ~ "Rest of the World",
      
      TRUE ~ NA_character_
    )
  )

unclassified_regions <- df %>%
  filter(is.na(region_plot)) %>%
  select(
    publication_year,
    smr_design,
    region
  )

if (nrow(unclassified_regions) > 0) {
  
  cat("\nUNCLASSIFIED REGIONS:\n")
  
  print(
    unclassified_regions,
    n = Inf
  )
  
  stop(
    "Unclassified regional records detected. ",
    "Review the source workbook."
  )
}


# ============================================================
# 7. VALIDATE DUPLICATES
# ============================================================

duplicates <- df %>%
  count(
    publication_year,
    smr_design,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  
  cat("\nDUPLICATE DESIGN-YEAR RECORDS:\n")
  
  print(duplicates, n = Inf)
  
  stop(
    "Duplicate design-year records detected."
  )
}


# ============================================================
# 8. VALIDATE ANNUAL TOTALS
# ============================================================

expected_totals <- tibble(
  publication_year = years,
  expected_n = c(
    17,
    29,
    40,
    50,
    68,
    80,
    70,
    75
  )
)

observed_totals <- df %>%
  count(
    publication_year,
    name = "observed_n"
  )

validation <- expected_totals %>%
  left_join(
    observed_totals,
    by = "publication_year"
  ) %>%
  mutate(
    observed_n = replace_na(
      observed_n,
      0L
    ),
    difference = observed_n - expected_n
  )

cat("\nANNUAL TOTAL VALIDATION:\n")

print(validation, n = Inf)

if (any(validation$difference != 0)) {
  
  stop(
    "Figure 4 annual totals do not match ",
    "the validated Figure 1 totals. ",
    "Review the source workbook."
  )
}

cat("\nAnnual totals validated successfully.\n")


# ============================================================
# 9. PREPARE REGIONAL COUNTS
# ============================================================

regional_data <- bind_rows(
  
  df %>%
    mutate(
      panel = "All designs"
    ),
  
  df %>%
    mutate(
      panel = reactor_group
    )
  
) %>%
  count(
    publication_year,
    panel,
    region_plot,
    name = "n"
  ) %>%
  mutate(
    publication_year = factor(
      publication_year,
      levels = years
    ),
    
    region_plot = factor(
      region_plot,
      levels = region_levels
    )
  ) %>%
  complete(
    publication_year,
    panel = c(
      "All designs",
      "Conventional SMRs",
      "Microreactors"
    ),
    region_plot,
    fill = list(n = 0)
  )


# ============================================================
# 10. VALIDATE PANEL TOTALS
# ============================================================

panel_totals <- regional_data %>%
  group_by(
    publication_year,
    panel
  ) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  ) %>%
  arrange(
    panel,
    publication_year
  )

cat("\nREGIONAL PANEL TOTALS:\n")

print(panel_totals, n = Inf)

all_design_totals <- panel_totals %>%
  filter(panel == "All designs") %>%
  mutate(
    publication_year = as.integer(
      as.character(publication_year)
    )
  ) %>%
  arrange(publication_year)

if (!identical(
  as.integer(all_design_totals$n),
  as.integer(expected_totals$expected_n)
)) {
  
  stop(
    "All-design regional totals do not match ",
    "the validated annual totals."
  )
}

group_check <- panel_totals %>%
  tidyr::pivot_wider(
    names_from = panel,
    values_from = n,
    values_fill = 0
  )

if (any(
  group_check$`All designs` !=
  group_check$`Conventional SMRs` +
  group_check$Microreactors
)) {
  
  stop(
    "Conventional SMR and microreactor totals ",
    "do not sum to all designs."
  )
}


# ============================================================
# 11. PLOTTING THEME
# ============================================================

theme_iaea <- theme_minimal(
  base_size = 10
) +
  theme(
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    axis.line.y = element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    axis.text = element_text(
      colour = "black",
      size = 8.5
    ),
    
    axis.title = element_text(
      colour = "black",
      size = 9.5
    ),
    
    plot.title = element_text(
      colour = "black",
      size = 10.5,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.margin = margin(
      4,
      9,
      2,
      9
    ),
    
    legend.position = "none"
  )


# ============================================================
# 12. CREATE INDIVIDUAL PANELS
# ============================================================

axis_settings <- list(
  
  "All designs" = list(
    maximum = 90,
    breaks = seq(0, 80, 20)
  ),
  
  "Conventional SMRs" = list(
    maximum = 60,
    breaks = seq(0, 60, 10)
  ),
  
  "Microreactors" = list(
    maximum = 30,
    breaks = seq(0, 30, 5)
  )
)

make_panel <- function(group_name) {
  
  settings <- axis_settings[[group_name]]
  
  panel_data <- regional_data %>%
    filter(panel == group_name)
  
  observed_max <- panel_data %>%
    group_by(publication_year) %>%
    summarise(
      total = sum(n),
      .groups = "drop"
    ) %>%
    pull(total)
  
  if (any(observed_max > settings$maximum)) {
    
    stop(
      "Y-axis maximum is too low for ",
      group_name
    )
  }
  
  ggplot(
    panel_data,
    aes(
      x = publication_year,
      y = n,
      fill = region_plot
    )
  ) +
    
    geom_col(
      width = 0.72,
      position = position_stack(
        reverse = TRUE
      )
    ) +
    
    scale_fill_manual(
      values = region_cols,
      limits = region_levels,
      drop = FALSE
    ) +
    
    scale_y_continuous(
      limits = c(
        0,
        settings$maximum
      ),
      breaks = settings$breaks,
      expand = c(0, 0)
    ) +
    
    labs(
      title = group_name,
      x = "Publication year",
      y = "Number of designs"
    ) +
    
    theme_iaea
}


# ============================================================
# 13. GENERATE THREE PANELS
# ============================================================

p_a <- make_panel(
  "All designs"
)

p_b <- make_panel(
  "Conventional SMRs"
)

p_c <- make_panel(
  "Microreactors"
)


# ============================================================
# 14. COMBINE PANELS
# ============================================================

panels <- cowplot::plot_grid(
  
  p_a,
  p_b,
  p_c,
  
  ncol = 1,
  
  align = "v",
  axis = "lr",
  
  labels = c(
    "a",
    "b",
    "c"
  ),
  
  label_size = 11,
  label_fontface = "bold",
  
  label_x = 0.01,
  label_y = 0.99
)


# ============================================================
# 15. RESTORE ORIGINAL FIVE-CATEGORY LEGEND
# ============================================================

# Draw the legend directly.
# This avoids the disappearing-legend problem.

legend_items <- lapply(
  seq_along(region_levels),
  function(i) {
    
    grid::grobTree(
      
      grid::rectGrob(
        x = grid::unit(0, "npc"),
        y = grid::unit(0.5, "npc"),
        
        width = grid::unit(3, "mm"),
        height = grid::unit(3, "mm"),
        
        just = c(
          "left",
          "centre"
        ),
        
        gp = grid::gpar(
          fill = region_cols[[region_levels[i]]],
          col = "black",
          lwd = 0.4
        )
      ),
      
      grid::textGrob(
        label = region_levels[i],
        
        x = grid::unit(4.3, "mm"),
        y = grid::unit(0.5, "npc"),
        
        just = c(
          "left",
          "centre"
        ),
        
        gp = grid::gpar(
          fontsize = 9,
          col = "black"
        )
      )
    )
  }
)

legend_grob <- gridExtra::arrangeGrob(
  
  grobs = legend_items,
  
  nrow = 1,
  
  widths = grid::unit(
    c(
      32,
      20,
      17,
      22,
      37
    ),
    "mm"
  )
)


# ============================================================
# 16. FINAL FIGURE
# ============================================================

fig04 <- cowplot::ggdraw() +
  
  cowplot::draw_plot(
    panels,
    
    x = 0,
    y = 0.055,
    
    width = 1,
    height = 0.945
  ) +
  
  cowplot::draw_grob(
    legend_grob,
    
    x = 0.10,
    y = 0.008,
    
    width = 0.84,
    height = 0.035
  )

print(fig04)


# ============================================================
# 17. EXPORT PNG
# ============================================================

ggsave(
  
  filename = PNG_OUT,
  
  plot = fig04,
  
  width = 180,
  height = 205,
  
  units = "mm",
  
  dpi = 600,
  
  bg = "white"
)


# ============================================================
# 18. EXPORT PDF
# ============================================================

ggsave(
  
  filename = PDF_OUT,
  
  plot = fig04,
  
  width = 180,
  height = 205,
  
  units = "mm",
  
  device = grDevices::cairo_pdf,
  
  bg = "white"
)


# ============================================================
# 19. CONFIRM OUTPUTS
# ============================================================

stopifnot(
  file.exists(PNG_OUT),
  file.exists(PDF_OUT)
)

cat(
  "\n============================================\n"
)

cat(
  "FIGURE 4 COMPLETE\n"
)

cat(
  "============================================\n"
)

cat(
  "\nPNG:\n",
  PNG_OUT,
  "\n"
)

cat(
  "\nPDF:\n",
  PDF_OUT,
  "\n"
)

cat(
  "\nLegend: five regional categories, one row\n"
)

cat(
  "Panel subtitles: bold\n"
)

cat(
  "Panel labels: bold\n"
)

cat(
  "Annual totals: validated\n"
)

cat(
  "Export: 180 x 205 mm, 600 dpi\n"
)

cat(
  "\n============================================\n"
)
