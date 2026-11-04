
# ============================================================
# FIGURE 2: IAEA DESIGN TURNOVER AND PERSISTENCE
# 2012–2026
#
# a: Retained, new and no-longer-listed design lineages
# b: Number of catalogue appearances
#
# Input:
# revision/data/fig02_iaea_turnover_2012_2026_source_data.xlsx
#
# Outputs:
# revision/figures/fig02_iaea_turnover_2012_2026.png
# revision/figures/fig02_iaea_turnover_2012_2026.pdf
#
# Export dimensions: 180 x 180 mm
# GitHub-compatible version
# ============================================================

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "scales",
  "patchwork"
)

missing_packages <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ------------------------------------------------------------
# 2. GITHUB-COMPATIBLE FILE PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig02_iaea_turnover_2012_2026_source_data.xlsx"
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

png_file <- file.path(
  output_dir,
  "fig02_iaea_turnover_2012_2026.png"
)

pdf_file <- file.path(
  output_dir,
  "fig02_iaea_turnover_2012_2026.pdf"
)

# ------------------------------------------------------------
# 3. SETTINGS
# ------------------------------------------------------------

years <- seq(2012, 2026, by = 2)

expected_totals <- c(
  17, 29, 40, 50,
  68, 80, 70, 75
)

turnover_colours <- c(
  "Retained" = "#4FA161",
  "New" = "#E9A35B",
  "Removed" = "#D65A4A"
)

transition_levels <- paste0(
  years[-length(years)],
  "\u2192",
  years[-1]
)

# ------------------------------------------------------------
# 4. IMPORT SOURCE DATA
# ------------------------------------------------------------

required_columns <- c(
  "publication_year",
  "smr_design",
  "lineage_id",
  "classification",
  "development_phase_recoded"
)

available_sheets <- excel_sheets(input_file)

selected_sheet <- NA_character_

for (sheet_name in available_sheets) {
  
  header <- read_excel(
    input_file,
    sheet = sheet_name,
    n_max = 0
  )
  
  if (all(required_columns %in% names(header))) {
    selected_sheet <- sheet_name
    break
  }
}

if (is.na(selected_sheet)) {
  stop(
    "No worksheet contains all required columns."
  )
}

raw <- read_excel(
  input_file,
  sheet = selected_sheet
)

cat("\nSource worksheet:", selected_sheet, "\n")

# ------------------------------------------------------------
# 5. CLEAN AND FILTER
# ------------------------------------------------------------

df <- raw %>%
  mutate(
    across(
      where(is.character),
      str_squish
    ),
    
    publication_year = as.integer(
      publication_year
    ),
    
    smr_design = str_squish(
      as.character(smr_design)
    ),
    
    lineage_id = str_squish(
      as.character(lineage_id)
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
      "micro_reactor",
      "microreactor"
    ),
    
    !is.na(development_phase_recoded),
    
    development_phase_recoded != "failed_design"
  ) %>%
  mutate(
    reactor_category = if_else(
      classification == "small_modular_reactor",
      "Conventional SMRs",
      "Microreactors"
    )
  )

# Apply optional separate failed-design flag.

if ("failed_design" %in% names(df)) {
  
  df <- df %>%
    filter(
      is.na(failed_design) |
        !tolower(
          trimws(as.character(failed_design))
        ) %in% c("true", "1", "yes")
    )
}

# ------------------------------------------------------------
# 6. VALIDATE DESIGN IDENTIFIERS
# ------------------------------------------------------------

if (any(
  is.na(df$lineage_id) |
  df$lineage_id == ""
)) {
  stop("Missing lineage identifiers detected.")
}

if (any(
  is.na(df$smr_design) |
  df$smr_design == ""
)) {
  stop("Missing design names detected.")
}

# ------------------------------------------------------------
# 7. VALIDATE DUPLICATES
# ------------------------------------------------------------

analytical_columns <- c(
  "publication_year",
  "smr_design",
  "lineage_id",
  "reactor_category",
  "development_phase_recoded"
)

exact_duplicates <- df %>%
  count(
    across(all_of(analytical_columns)),
    name = "copies"
  ) %>%
  filter(copies > 1)

if (nrow(exact_duplicates) > 0) {
  
  message(
    "Exact analytical duplicates removed: ",
    nrow(exact_duplicates)
  )
}

df <- df %>%
  distinct(
    across(all_of(analytical_columns)),
    .keep_all = TRUE
  )

conflicts <- df %>%
  count(
    publication_year,
    lineage_id,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(conflicts) > 0) {
  
  print(conflicts, n = Inf)
  
  stop(
    "Conflicting lineage-year observations detected."
  )
}

# ------------------------------------------------------------
# 8. CREATE LINEAGE PRESENCE DATA
# ------------------------------------------------------------

presence <- df %>%
  distinct(
    publication_year,
    lineage_id,
    reactor_category
  ) %>%
  arrange(
    lineage_id,
    publication_year
  )

# ------------------------------------------------------------
# 9. VALIDATE ANNUAL COUNTS
# ------------------------------------------------------------

annual_counts <- presence %>%
  count(
    publication_year,
    name = "n_designs"
  ) %>%
  complete(
    publication_year = years,
    fill = list(n_designs = 0L)
  ) %>%
  arrange(publication_year)

cat("\nANNUAL DESIGN COUNTS\n")

print(annual_counts, n = Inf)

if (!identical(
  as.integer(annual_counts$n_designs),
  as.integer(expected_totals)
)) {
  
  stop(
    "Annual counts differ from the Figure 1 baseline."
  )
}

if (nrow(presence) != 429) {
  stop(
    "Expected 429 catalogue observations."
  )
}

if (n_distinct(presence$lineage_id) != 124) {
  stop(
    "Expected 124 distinct design lineages."
  )
}

# ------------------------------------------------------------
# 10. PANEL A: CATALOGUE TURNOVER
# ------------------------------------------------------------

turnover_list <- lapply(
  seq_len(length(years) - 1L),
  function(i) {
    
    previous_year <- years[i]
    current_year <- years[i + 1L]
    
    previous <- presence %>%
      filter(
        publication_year == previous_year
      ) %>%
      pull(lineage_id)
    
    current <- presence %>%
      filter(
        publication_year == current_year
      ) %>%
      pull(lineage_id)
    
    tibble(
      previous_year = previous_year,
      
      current_year = current_year,
      
      transition = paste0(
        previous_year,
        "\u2192",
        current_year
      ),
      
      retained = length(
        intersect(previous, current)
      ),
      
      new = length(
        setdiff(current, previous)
      ),
      
      removed = length(
        setdiff(previous, current)
      ),
      
      previous_total = length(previous),
      
      current_total = length(current)
    )
  }
)

turnover_summary <- bind_rows(
  turnover_list
)

stopifnot(
  all(
    turnover_summary$retained +
      turnover_summary$new ==
      turnover_summary$current_total
  ),
  
  all(
    turnover_summary$retained +
      turnover_summary$removed ==
      turnover_summary$previous_total
  )
)

cat("\nCATALOGUE TURNOVER\n")

print(turnover_summary, n = Inf)

# ------------------------------------------------------------
# 11. PREPARE PANEL A DATA
# ------------------------------------------------------------

turnover_plot_data <- turnover_summary %>%
  select(
    transition,
    retained,
    new,
    removed
  ) %>%
  pivot_longer(
    cols = c(
      retained,
      new,
      removed
    ),
    
    names_to = "turnover_status",
    
    values_to = "n_designs"
  ) %>%
  mutate(
    plot_value = if_else(
      turnover_status == "removed",
      -n_designs,
      n_designs
    ),
    
    turnover_status = factor(
      turnover_status,
      
      levels = c(
        "retained",
        "new",
        "removed"
      ),
      
      labels = c(
        "Retained",
        "New",
        "Removed"
      )
    ),
    
    transition = factor(
      transition,
      levels = transition_levels
    )
  )

# ------------------------------------------------------------
# 12. PANEL B: DESIGN PERSISTENCE
# ------------------------------------------------------------

design_persistence <- presence %>%
  group_by(lineage_id) %>%
  summarise(
    catalogue_appearances = n_distinct(
      publication_year
    ),
    
    first_seen = min(publication_year),
    
    last_seen = max(publication_year),
    
    .groups = "drop"
  )

panel_b_data <- design_persistence %>%
  count(
    catalogue_appearances,
    name = "n_designs"
  ) %>%
  complete(
    catalogue_appearances = 1:8,
    fill = list(n_designs = 0L)
  ) %>%
  arrange(catalogue_appearances)

stopifnot(
  sum(panel_b_data$n_designs) == 124
)

cat("\nCATALOGUE PERSISTENCE\n")

print(panel_b_data, n = Inf)

# ------------------------------------------------------------
# 13. COMMON FIGURE THEME
# ------------------------------------------------------------

figure_theme <- theme_classic(
  base_size = 9
) +
  theme(
    text = element_text(
      colour = "black"
    ),
    
    axis.line = element_line(
      linewidth = 0.35,
      colour = "grey35"
    ),
    
    axis.ticks = element_line(
      linewidth = 0.35,
      colour = "grey35"
    ),
    
    axis.text = element_text(
      size = 9,
      colour = "grey20"
    ),
    
    axis.title = element_text(
      size = 9,
      colour = "black"
    ),
    
    plot.tag = element_text(
      size = 14,
      face = "bold",
      colour = "black"
    ),
    
    plot.tag.position = c(0, 1),
    
    plot.margin = margin(
      t = 6,
      r = 7,
      b = 6,
      l = 7
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# ------------------------------------------------------------
# 14. FIGURE 2A: TURNOVER
# ------------------------------------------------------------

panel_a <- ggplot(
  turnover_plot_data,
  
  aes(
    x = transition,
    y = plot_value,
    fill = turnover_status
  )
) +
  geom_col(
    width = 0.70
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "grey25"
  ) +
  
  scale_fill_manual(
    values = turnover_colours,
    
    breaks = c(
      "Retained",
      "New",
      "Removed"
    ),
    
    name = "Design classification"
  ) +
  
  scale_y_continuous(
    labels = function(x) abs(x),
    
    expand = expansion(
      mult = c(0.08, 0.06)
    )
  ) +
  
  labs(
    tag = "a",
    x = "Catalogue transition",
    y = "Number of designs"
  ) +
  
  figure_theme +
  
  theme(
    axis.line.x = element_blank(),
    
    axis.ticks.x = element_blank(),
    
    legend.position = "top",
    
    legend.justification = "left",
    
    legend.box.just = "left",
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.height = grid::unit(
      0.38,
      "cm"
    ),
    
    legend.key.width = grid::unit(
      0.38,
      "cm"
    )
  )

# ------------------------------------------------------------
# 15. FIGURE 2B: PERSISTENCE
# ------------------------------------------------------------

panel_b <- ggplot(
  panel_b_data,
  
  aes(
    x = factor(
      catalogue_appearances,
      levels = 1:8
    ),
    
    y = n_designs
  )
) +
  
  geom_col(
    width = 0.70,
    fill = "#4FA161"
  ) +
  
  geom_text(
    data = panel_b_data %>%
      filter(n_designs > 0),
    
    aes(label = n_designs),
    
    vjust = -0.45,
    
    size = 3.0
  ) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  
  labs(
    tag = "b",
    x = "Number of catalogue appearances",
    y = "Number of designs"
  ) +
  
  figure_theme

# ------------------------------------------------------------
# 16. ASSEMBLE FIGURE 2
# ------------------------------------------------------------

figure_2 <- (
  panel_a /
    panel_b
) +
  plot_layout(
    heights = c(1.05, 1)
  )

print(figure_2)

# ------------------------------------------------------------
# 17. EXPORT MAIN FIGURE
# ------------------------------------------------------------

ggsave(
  filename = png_file,
  
  plot = figure_2,
  
  width = 180,
  height = 180,
  units = "mm",
  
  dpi = 600,
  
  bg = "white"
)

ggsave(
  filename = pdf_file,
  
  plot = figure_2,
  
  width = 180,
  height = 180,
  units = "mm",
  
  device = cairo_pdf,
  
  bg = "white"
)

# ------------------------------------------------------------
# 18. FINAL VALIDATION
# ------------------------------------------------------------

stopifnot(
  file.exists(png_file),
  
  file.exists(pdf_file),
  
  nrow(presence) == 429,
  
  nrow(design_persistence) == 124,
  
  sum(panel_b_data$n_designs) == 124
)

cat("\n")
cat("========================================\n")
cat("FIGURE 2 COMPLETE\n")
cat("========================================\n")

cat("\nCatalogue observations:", nrow(presence))

cat(
  "\nDistinct lineages:",
  nrow(design_persistence)
)

cat(
  "\nLineages in all eight editions:",
  sum(
    design_persistence$catalogue_appearances == 8
  )
)

cat("\n\nPNG:", png_file)
cat("\nPDF:", pdf_file)

cat("\n\nALL CHECKS PASSED.\n")
