# ============================================================
# Code for the manuscript Gordon, J.A.
#
# From pipeline growth to first-of-a-kind deployment:
# market readiness across the global small modular reactor landscape
#
# Reproduces Figure 3: Aggregate SMR market readiness by
# conceptual design start year.
#
# Panel a shows the relationship between mean market readiness
# and conceptual design start year across the full sample of
# assessed SMR designs from 1984 to 2025.
#
# Panel b restricts the sample to designs initiated between
# 2009 and 2018.
#
# Panel c restricts the sample to designs initiated between
# 2019 and 2025.
#
# Points represent individual SMR designs, the blue line shows
# the fitted linear trend, and the shaded area indicates the
# 95% confidence interval.
#
# Required input:
# data/fig03_start_year_source_data.xlsx
#
# Outputs:
# outputs/figure_03/fig03_a-c.png
# outputs/figure_03/fig03_a-c.pdf
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()

# ============================================================
# 1. HELPER FUNCTIONS
# ============================================================

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

normalise_existing <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  unique(normalizePath(x, winslash = "/", mustWork = FALSE))
}

find_repo_root <- function(input_filename) {

  candidates <- character(0)

  # A. Optional environment-variable override
  env_root <- Sys.getenv("SMR_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    candidates <- c(candidates, env_root)
  }

  # B. Directory containing the active RStudio script
  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    active_file <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )

    if (nzchar(active_file)) {
      script_dir <- dirname(active_file)
      candidates <- c(
        candidates,
        script_dir,
        file.path(script_dir, "..")
      )
    }
  }

  # C. Current working directory and several parent directories
  current_dir <- getwd()

  for (i in 0:6) {
    candidates <- c(candidates, current_dir)
    current_dir <- dirname(current_dir)
  }

  # D. User's established local repository path
  # This fallback makes the script run even when RStudio's working
  # directory is elsewhere. It does not prevent portability because
  # the repository-relative checks above are attempted first.
  candidates <- c(
    candidates,
    "C:/Users/jo3151go/OneDrive/NEA Dashboard Dataset/small-modular-reactor-market-readiness_github"
  )

  candidates <- normalise_existing(candidates)

  matches <- vapply(
    candidates,
    function(root) {
      file.exists(file.path(root, "data", input_filename))
    },
    logical(1)
  )

  if (!any(matches)) {
    stop(
      paste0(
        "The Figure 3 source-data workbook could not be found.\n\n",
        "Expected repository-relative file:\n",
        "data/", input_filename, "\n\n",
        "Current working directory:\n",
        normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n\n",
        "Either place this script in the repository's scripts folder, ",
        "run it from the repository root, or set the SMR_REPO_ROOT ",
        "environment variable to the repository path."
      )
    )
  }

  candidates[which(matches)[1]]
}

# ============================================================
# 2. LOCATE REPOSITORY AND DEFINE PATHS
# ============================================================

input_filename <- "fig03_start_year_source_data.xlsx"

project_dir <- find_repo_root(input_filename)

data_file <- file.path(
  project_dir,
  "data",
  input_filename
)

output_dir <- file.path(
  project_dir,
  "outputs",
  "figure_03"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

png_out <- file.path(
  output_dir,
  "fig03_a-c.png"
)

pdf_out <- file.path(
  output_dir,
  "fig03_a-c.pdf"
)

message("Repository root: ", project_dir)
message("Input workbook: ", data_file)

# ============================================================
# 3. LOAD SOURCE DATA
# ============================================================

available_sheets <- readxl::excel_sheets(data_file)

sheet_name <- if ("fig03" %in% available_sheets) {
  "fig03"
} else if ("fig02" %in% available_sheets) {
  "fig02"
} else {
  available_sheets[1]
}

fig03_data <- readxl::read_excel(
  path = data_file,
  sheet = sheet_name
)

names(fig03_data) <- clean_names_simple(names(fig03_data))

required_columns <- c(
  "design",
  "cd_start",
  "mean_readiness"
)

missing_columns <- setdiff(
  required_columns,
  names(fig03_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The Figure 3 source-data workbook is missing these columns:\n",
      paste(missing_columns, collapse = ", ")
    )
  )
}

message(
  "Loaded ",
  nrow(fig03_data),
  " rows from worksheet '",
  sheet_name,
  "'."
)

# ============================================================
# 4. CLEAN AND PREPARE DATA
# ============================================================

plot_df <- fig03_data %>%
  transmute(
    design = as.character(design),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    mean_readiness = suppressWarnings(as.numeric(mean_readiness))
  ) %>%
  filter(
    !is.na(cd_start),
    !is.na(mean_readiness)
  ) %>%
  arrange(
    cd_start,
    design
  )

if (nrow(plot_df) == 0) {
  stop("No valid observations remained after data cleaning.")
}

df_a <- plot_df %>%
  mutate(
    cohort = case_when(
      cd_start >= 2009 & cd_start <= 2018 ~ "B",
      cd_start >= 2019 & cd_start <= 2025 ~ "C",
      TRUE ~ "Other"
    )
  )

df_b <- plot_df %>%
  filter(
    cd_start >= 2009,
    cd_start <= 2018
  )

df_c <- plot_df %>%
  filter(
    cd_start >= 2019,
    cd_start <= 2025
  )

if (nrow(df_b) < 2) {
  stop("Panel b contains fewer than two valid observations.")
}

if (nrow(df_c) < 2) {
  stop("Panel c contains fewer than two valid observations.")
}

# ============================================================
# 5. COLOURS AND THEME
# ============================================================

col_other <- "black"
col_b     <- "#C97A2B"
col_c     <- "#4F8A3A"
line_col  <- "#1F5AC6"
band_fill <- "grey80"

theme_panel <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.line.y = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.ticks = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.text.x = element_text(
      size = 11,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 11,
      colour = "black"
    ),
    axis.title.x = element_text(
      size = 11,
      colour = "black"
    ),
    axis.title.y = element_text(
      size = 11,
      colour = "black"
    ),
    plot.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(4, 6, 4, 6)
  )

# ============================================================
# 6. PANEL A: FULL SAMPLE
# ============================================================

p_a <- ggplot(
  df_a,
  aes(
    x = cd_start,
    y = mean_readiness
  )
) +
  geom_point(
    data = filter(df_a, cohort == "Other"),
    colour = col_other,
    size = 2.4,
    alpha = 0.85
  ) +
  geom_point(
    data = filter(df_a, cohort == "B"),
    colour = col_b,
    size = 2.4,
    alpha = 0.95
  ) +
  geom_point(
    data = filter(df_a, cohort == "C"),
    colour = col_c,
    size = 2.4,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
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

# ============================================================
# 7. PANEL B: 2009-2018
# ============================================================

p_b <- ggplot(
  df_b,
  aes(
    x = cd_start,
    y = mean_readiness
  )
) +
  geom_point(
    size = 2.4,
    colour = col_b,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
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

# ============================================================
# 8. PANEL C: 2019-2025
# ============================================================

p_c <- ggplot(
  df_c,
  aes(
    x = cd_start,
    y = mean_readiness
  )
) +
  geom_point(
    size = 2.4,
    colour = col_c,
    alpha = 0.95
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
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

# ============================================================
# 9. COMBINE PANELS
# ============================================================

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

# ============================================================
# 10. SAVE OUTPUTS
# ============================================================

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
  device = grDevices::cairo_pdf,
  bg = "white"
)

# ============================================================
# 11. VERIFY OUTPUTS
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
message("FIGURE 3 COMPLETE")
message("==============================================")
message("Repository root: ", project_dir)
message("Input: ", data_file)
message("Worksheet: ", sheet_name)
message("Observations, full sample: ", nrow(df_a))
message("Observations, 2009-2018: ", nrow(df_b))
message("Observations, 2019-2025: ", nrow(df_c))
message("PNG: ", png_out)
message("PDF: ", pdf_out)
message("==============================================")

combined_plot
