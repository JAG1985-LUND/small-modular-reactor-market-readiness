
# ================================================================
# FIGURE 10 — NEA SMR DASHBOARD, EDITION 4
# REGIONAL TECHNOLOGICAL COMPOSITION
#
# Panels:
#   a. Reactor cooling type
#   b. Neutron spectrum
#   c. Fuel type
#   d. Fuel enrichment
#
# Sample:
#   91 Edition 4 designs, including microreactors
#   89 designs in the four-region comparison
#
# Regions:
#   North America
#   Europe
#   OECD Asia
#   China/Russia
#
# Rest-of-world designs are excluded from the four-region
# comparison but retained in the source-data audit.
#
# INPUT:
# revision/data/fig10_nea_technological_regional_source_data.xlsx
#
# OUTPUTS:
# revision/figures/fig10_nea_technological_regional.png
# revision/figures/fig10_nea_technological_regional.pdf
# revision/figures/fig10_nea_technological_regional_results.xlsx
#
# Figure dimensions: 220 x 160 mm
# PNG resolution: 600 dpi
# ================================================================


# 1. RESET ENVIRONMENT -------------------------------------------

rm(list = ls())
graphics.off()


# 2. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "cowplot",
  "scales",
  "openxlsx"
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
  library(cowplot)
  library(scales)
  library(openxlsx)
})


# 3. GITHUB-COMPATIBLE FILE PATHS --------------------------------

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig10_nea_technological_regional_source_data.xlsx"
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
        "Starting directory: ", start_dir, "\n",
        "Run this script from within the repository."
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
  stop("Input workbook not found: ", input_file)
}

output_prefix <- "fig10_nea_technological_regional"

png_file <- file.path(
  output_dir,
  paste0(output_prefix, ".png")
)

pdf_file <- file.path(
  output_dir,
  paste0(output_prefix, ".pdf")
)

excel_file <- file.path(
  output_dir,
  paste0(output_prefix, "_results.xlsx")
)


# 4. IMPORT DATA -------------------------------------------------

sheet_names <- readxl::excel_sheets(input_file)

sheet_to_use <- if ("nea_50" %in% sheet_names) {
  "nea_50"
} else {
  sheet_names[1]
}

df <- readxl::read_excel(
  input_file,
  sheet = sheet_to_use
)

names(df) <- trimws(names(df))

# Support the legacy spelling of neutron_spectrum.

if (
  !"neutron_spectrum" %in% names(df) &&
  "neturon_spectrum" %in% names(df)
) {
  
  names(df)[
    names(df) == "neturon_spectrum"
  ] <- "neutron_spectrum"
}

cat("\nImported worksheet:", sheet_to_use, "\n")
cat("Imported rows:", nrow(df), "\n")


# 5. CHECK REQUIRED COLUMNS -------------------------------------

required_cols <- c(
  "edition",
  "design",
  "region_cn_ru",
  "cooling_type",
  "neutron_spectrum",
  "fuel_type",
  "fuel_enrichment"
)

missing_cols <- setdiff(
  required_cols,
  names(df)
)

if (length(missing_cols) > 0) {
  
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


# 6. FILTER EDITION 4 --------------------------------------------

df <- df %>%
  dplyr::filter(
    suppressWarnings(
      as.numeric(as.character(edition))
    ) == 4,
    
    !is.na(design),
    
    trimws(as.character(design)) != ""
  ) %>%
  dplyr::filter(
    toupper(
      stringr::str_squish(as.character(design))
    ) != "DF-CDE"
  )

df <- df %>%
  dplyr::mutate(
    design = stringr::str_squish(as.character(design))
  )

if (anyDuplicated(df$design) > 0) {
  stop("Duplicate design names detected.")
}

if (nrow(df) != 91) {
  
  stop(
    "Expected 91 Edition 4 designs; found ",
    nrow(df)
  )
}

cat("\nEdition 4 designs:", nrow(df), "\n")


# 7. TEXT CLEANING -----------------------------------------------

clean_text <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "\u00A0",
    " ",
    x,
    fixed = TRUE
  )
  
  stringr::str_squish(x)
}

normalise <- function(x) {
  stringr::str_to_lower(clean_text(x))
}


# 8. REGIONAL CLASSIFICATION -------------------------------------

region_order <- c(
  "North America",
  "Europe",
  "OECD Asia",
  "China/Russia"
)

df <- df %>%
  dplyr::mutate(
    
    region_raw = normalise(region_cn_ru),
    
    region_group = dplyr::case_when(
      
      region_raw %in% c(
        "north america",
        "north_america"
      ) ~ "North America",
      
      region_raw == "europe" ~ "Europe",
      
      region_raw %in% c(
        "oecd asia",
        "oecd_asia"
      ) ~ "OECD Asia",
      
      region_raw %in% c(
        "china/russia",
        "china / russia",
        "china_russia",
        "china",
        "russia"
      ) ~ "China/Russia",
      
      TRUE ~ NA_character_
    )
  )


# Rest-of-world designs are not included in the four panels.

excluded_regions <- df %>%
  dplyr::filter(is.na(region_group)) %>%
  dplyr::select(
    design,
    region_cn_ru
  )

expected_excluded <- c(
  "HTMR-100",
  "ThorCon 500"
)

if (
  nrow(excluded_regions) != 2 ||
  !setequal(
    excluded_regions$design,
    expected_excluded
  )
) {
  
  print(excluded_regions)
  
  stop(
    "Unexpected regional exclusions detected."
  )
}


# Four-region analysis sample.

analysis_df <- df %>%
  dplyr::filter(!is.na(region_group)) %>%
  dplyr::mutate(
    region_group = factor(
      region_group,
      levels = region_order
    )
  )

if (nrow(analysis_df) != 89) {
  
  stop(
    "Expected 89 regional designs; found ",
    nrow(analysis_df)
  )
}


# Regional counts.

regional_counts <- analysis_df %>%
  dplyr::count(
    region_group,
    name = "n"
  ) %>%
  tidyr::complete(
    region_group = factor(
      region_order,
      levels = region_order
    ),
    fill = list(n = 0)
  )

if (any(regional_counts$n == 0)) {
  stop("One or more regional groups contain no designs.")
}

cat("\nREGIONAL COUNTS:\n")
print(regional_counts)

cat("\nEXCLUDED REST-OF-WORLD DESIGNS:\n")
print(excluded_regions)


# 9. TECHNOLOGICAL CLASSIFICATIONS -------------------------------

analysis_df <- analysis_df %>%
  dplyr::mutate(
    
    cooling_raw = normalise(cooling_type),
    
    spectrum_raw = normalise(neutron_spectrum),
    
    fuel_raw = normalise(fuel_type),
    
    enrichment_raw = normalise(fuel_enrichment),
    
    
    # a. Reactor cooling type
    
    cooling_group = dplyr::case_when(
      
      cooling_raw %in% c(
        "water",
        "water-cooled",
        "water_cooled",
        "water cooled"
      ) ~ "Water-cooled",
      
      cooling_raw %in% c(
        "gas",
        "metal",
        "molten salt",
        "heat pipe",
        "other",
        "gas-cooled",
        "metal-cooled",
        "molten-salt-cooled",
        "heat-pipe-cooled",
        "non-water-cooled"
      ) ~ "Non-water-cooled",
      
      TRUE ~ NA_character_
    ),
    
    
    # b. Neutron spectrum
    
    spectrum_group = dplyr::case_when(
      
      spectrum_raw == "thermal" ~ "Thermal",
      
      spectrum_raw == "fast" ~ "Fast",
      
      TRUE ~ NA_character_
    ),
    
    
    # c. Fuel type
    
    fuel_group = dplyr::case_when(
      
      fuel_raw == "uranium" ~ "Uranium",
      
      fuel_raw == "triso" ~ "TRISO",
      
      fuel_raw == "other" ~ "Other",
      
      TRUE ~ NA_character_
    ),
    
    
    # d. Fuel enrichment
    
    enrichment_group = dplyr::case_when(
      
      enrichment_raw == "leu" ~ "LEU",
      
      enrichment_raw == "haleu" ~ "HALEU",
      
      enrichment_raw %in% c(
        "other",
        "not_required",
        "not required",
        "not-required"
      ) ~ "Not required",
      
      TRUE ~ NA_character_
    )
  )


# 10. VALIDATE TECHNOLOGICAL CLASSIFICATIONS ---------------------

technology_vars <- c(
  "cooling_group",
  "spectrum_group",
  "fuel_group",
  "enrichment_group"
)

missing_summary <- data.frame(
  
  variable = technology_vars,
  
  missing = vapply(
    technology_vars,
    function(v) {
      sum(is.na(analysis_df[[v]]))
    },
    integer(1)
  )
)

if (any(missing_summary$missing > 0)) {
  
  print(
    analysis_df %>%
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(technology_vars),
          is.na
        )
      ) %>%
      dplyr::select(
        design,
        region_group,
        cooling_type,
        neutron_spectrum,
        fuel_type,
        fuel_enrichment
      )
  )
  
  stop(
    "Unrecognised technological classifications. ",
    "Check the source workbook."
  )
}

cat("\nTECHNOLOGICAL CLASSIFICATION CHECK:\n")
print(missing_summary)


# 11. COMPOSITION FUNCTION ---------------------------------------

make_comp_df <- function(data, variable, categories) {
  
  data %>%
    dplyr::transmute(
      
      region = region_group,
      
      category = factor(
        as.character(.data[[variable]]),
        levels = categories
      )
    ) %>%
    dplyr::count(
      region,
      category,
      name = "n"
    ) %>%
    tidyr::complete(
      
      region = factor(
        region_order,
        levels = region_order
      ),
      
      category = factor(
        categories,
        levels = categories
      ),
      
      fill = list(n = 0)
    ) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      
      total = sum(n),
      
      share = ifelse(
        total > 0,
        n / total,
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
}


# 12. PREPARE FOUR COMPOSITION DATASETS --------------------------

water_df <- make_comp_df(
  analysis_df,
  "cooling_group",
  c(
    "Water-cooled",
    "Non-water-cooled"
  )
)

spectrum_df <- make_comp_df(
  analysis_df,
  "spectrum_group",
  c(
    "Thermal",
    "Fast"
  )
)

fuel_df <- make_comp_df(
  analysis_df,
  "fuel_group",
  c(
    "Uranium",
    "TRISO",
    "Other"
  )
)

enrichment_df <- make_comp_df(
  analysis_df,
  "enrichment_group",
  c(
    "LEU",
    "HALEU",
    "Not required"
  )
)


# 13. COMBINED COMPOSITION RESULTS -------------------------------

composition_data <- dplyr::bind_rows(
  
  dplyr::mutate(
    water_df,
    dimension = "Reactor cooling type"
  ),
  
  dplyr::mutate(
    spectrum_df,
    dimension = "Neutron spectrum"
  ),
  
  dplyr::mutate(
    fuel_df,
    dimension = "Fuel type"
  ),
  
  dplyr::mutate(
    enrichment_df,
    dimension = "Fuel enrichment"
  )
  
) %>%
  dplyr::select(
    dimension,
    region,
    category,
    n,
    total,
    share
  ) %>%
  dplyr::mutate(
    percentage = round(
      100 * share,
      2
    )
  )


# 14. ORIGINAL COLOURS -------------------------------------------

water_cols <- c(
  "Water-cooled" = "#6BAED6",
  "Non-water-cooled" = "#E88A87"
)

spectrum_cols <- c(
  "Thermal" = "#4C78A8",
  "Fast" = "#F28E2B"
)

fuel_cols <- c(
  "Uranium" = "#E15759",
  "TRISO" = "#59A14F",
  "Other" = "#9D9D9D"
)

enrichment_cols <- c(
  "LEU" = "#E15759",
  "HALEU" = "#9C6ADE",
  "Not required" = "#9D9D9D"
)


# 15. ORIGINAL FIGURE THEME --------------------------------------

panel_theme <- ggplot2::theme_minimal(
  base_size = 10
) +
  ggplot2::theme(
    
    panel.grid = ggplot2::element_blank(),
    
    axis.line.x = ggplot2::element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    axis.line.y = ggplot2::element_blank(),
    
    axis.ticks.x = ggplot2::element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    axis.ticks.y = ggplot2::element_blank(),
    
    axis.ticks.length = grid::unit(
      2.5,
      "pt"
    ),
    
    axis.text.x = ggplot2::element_text(
      size = 9,
      colour = "black"
    ),
    
    axis.text.y = ggplot2::element_text(
      size = 9,
      colour = "black"
    ),
    
    axis.title.x = ggplot2::element_text(
      size = 10,
      colour = "black",
      margin = ggplot2::margin(t = 5)
    ),
    
    axis.title.y = ggplot2::element_blank(),
    
    plot.title = ggplot2::element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      colour = "black",
      margin = ggplot2::margin(b = 8)
    ),
    
    legend.position = "bottom",
    
    legend.title = ggplot2::element_blank(),
    
    legend.text = ggplot2::element_text(
      size = 9,
      colour = "black"
    ),
    
    legend.key = ggplot2::element_blank(),
    
    legend.key.size = grid::unit(
      10,
      "pt"
    ),
    
    legend.spacing.x = grid::unit(
      4,
      "pt"
    ),
    
    legend.margin = ggplot2::margin(
      t = 8,
      b = 2
    ),
    
    plot.margin = ggplot2::margin(
      t = 8,
      r = 12,
      b = 8,
      l = 8
    )
  )


# 16. PANEL FUNCTION ---------------------------------------------

make_panel <- function(
    plot_df,
    title_text,
    fill_values,
    legend_breaks
) {
  
  plot_df <- plot_df %>%
    dplyr::mutate(
      region = factor(
        as.character(region),
        levels = rev(region_order)
      )
    )
  
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = region,
      y = share,
      fill = category
    )
  ) +
    
    ggplot2::geom_col(
      width = 0.72,
      
      position = ggplot2::position_stack(
        reverse = TRUE
      )
    ) +
    
    ggplot2::coord_flip(
      clip = "off"
    ) +
    
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      
      breaks = c(
        0,
        0.25,
        0.50,
        0.75,
        1
      ),
      
      labels = scales::percent_format(
        accuracy = 1
      ),
      
      expand = ggplot2::expansion(
        mult = c(0, 0)
      )
    ) +
    
    ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = legend_breaks,
      drop = FALSE
    ) +
    
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = "Share"
    ) +
    
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    ) +
    
    panel_theme
}


# 17. BUILD FOUR PANELS ------------------------------------------

p_a <- make_panel(
  water_df,
  "Reactor cooling type",
  water_cols,
  c(
    "Water-cooled",
    "Non-water-cooled"
  )
)

p_b <- make_panel(
  spectrum_df,
  "Neutron spectrum",
  spectrum_cols,
  c(
    "Thermal",
    "Fast"
  )
)

p_c <- make_panel(
  fuel_df,
  "Fuel type",
  fuel_cols,
  c(
    "Uranium",
    "TRISO",
    "Other"
  )
)

p_d <- make_panel(
  enrichment_df,
  "Fuel enrichment",
  enrichment_cols,
  c(
    "LEU",
    "HALEU",
    "Not required"
  )
)


# 18. COMBINE PANELS ---------------------------------------------

combined_plot <- cowplot::plot_grid(
  p_a,
  p_b,
  p_c,
  p_d,
  
  ncol = 2,
  
  labels = c(
    "a",
    "b",
    "c",
    "d"
  ),
  
  label_size = 11,
  label_fontface = "bold",
  
  label_x = 0.01,
  label_y = 0.99,
  
  hjust = 0,
  vjust = 1,
  
  align = "hv",
  
  rel_widths = c(1, 1),
  rel_heights = c(1, 1)
)

print(combined_plot)


# 19. EXPORT PNG AND PDF -----------------------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = combined_plot,
  
  width = 220,
  height = 160,
  units = "mm",
  
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = combined_plot,
  
  width = 220,
  height = 160,
  units = "mm",
  
  device = grDevices::cairo_pdf,
  bg = "white",
  limitsize = FALSE
)


# 20. PREPARE EXCEL TABLES ---------------------------------------

sample_summary <- data.frame(
  
  measure = c(
    "Edition",
    "Total valid designs",
    "Regional analysis designs",
    "Excluded regional classifications",
    "Microreactors included"
  ),
  
  value = c(
    "4",
    as.character(nrow(df)),
    as.character(nrow(analysis_df)),
    as.character(nrow(excluded_regions)),
    "Yes"
  )
)

design_audit <- analysis_df %>%
  dplyr::select(
    design,
    region_group,
    cooling_group,
    spectrum_group,
    fuel_group,
    enrichment_group
  )


# 21. CREATE ONE COMBINED EXCEL WORKBOOK -------------------------

excel_tables <- list(
  
  "Sample summary" = sample_summary,
  
  "Regional counts" = regional_counts,
  
  "Composition results" = composition_data,
  
  "Cooling type" = water_df,
  
  "Neutron spectrum" = spectrum_df,
  
  "Fuel type" = fuel_df,
  
  "Fuel enrichment" = enrichment_df,
  
  "Excluded regions" = excluded_regions,
  
  "Design audit" = design_audit
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1",
  border = "Bottom",
  borderColour = "#A6A6A6"
)

percentage_style <- openxlsx::createStyle(
  numFmt = "0.0%"
)

for (sheet in names(excel_tables)) {
  
  sheet_data <- as.data.frame(
    excel_tables[[sheet]]
  )
  
  openxlsx::addWorksheet(
    wb,
    sheet
  )
  
  openxlsx::writeData(
    wb,
    sheet,
    sheet_data
  )
  
  n_cols <- ncol(sheet_data)
  
  openxlsx::addStyle(
    wb,
    sheet,
    style = header_style,
    rows = 1,
    cols = seq_len(n_cols),
    gridExpand = TRUE,
    stack = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(n_cols),
    widths = "auto"
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  if (nrow(sheet_data) > 0) {
    
    openxlsx::addFilter(
      wb,
      sheet,
      rows = 1,
      cols = seq_len(n_cols)
    )
    
    share_col <- which(
      names(sheet_data) == "share"
    )
    
    if (length(share_col) == 1) {
      
      openxlsx::addStyle(
        wb,
        sheet,
        style = percentage_style,
        rows = 2:(nrow(sheet_data) + 1),
        cols = share_col,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }
}

openxlsx::saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)


# 22. VERIFY OUTPUT FILES ----------------------------------------

output_files <- c(
  png_file,
  pdf_file,
  excel_file
)

if (!all(file.exists(output_files))) {
  stop("One or more output files were not created.")
}

file_information <- file.info(output_files)

if (any(
  is.na(file_information$size) |
  file_information$size == 0
)) {
  stop("One or more output files are empty.")
}


# 23. FINAL SUMMARY ----------------------------------------------

cat("\n")
cat("================================================\n")
cat("FIGURE 10 — REGIONAL TECHNOLOGICAL COMPOSITION\n")
cat("================================================\n")

cat(
  "\nTotal Edition 4 designs:",
  nrow(df),
  "\n"
)

cat(
  "Designs in regional analysis:",
  nrow(analysis_df),
  "\n"
)

cat(
  "Excluded Rest-of-world designs:",
  nrow(excluded_regions),
  "\n"
)

cat("\nREGIONAL COUNTS:\n")
print(regional_counts)

cat("\nCOMPOSITION RESULTS:\n")
print(composition_data, n = Inf)

cat("\nOUTPUT DIRECTORY:\n")
cat(output_dir, "\n")

cat("\nOUTPUT FILES:\n")

for (f in output_files) {
  cat(basename(f), "\n")
}

cat("\nEXCEL WORKSHEETS:\n")
print(names(excel_tables))

cat("\nAll output files verified.\n")
cat("Figure 10 complete.\n")
