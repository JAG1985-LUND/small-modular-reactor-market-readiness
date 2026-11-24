# ============================================================
# FIGURE 10
# REGIONAL TECHNOLOGICAL COMPOSITION OF SMR DESIGNS
# NEA SMR DASHBOARD, EDITION 4
#
# Panels:
# a. Cooling: Water-cooled / Non-water-cooled
# b. Neutron spectrum: Thermal / Fast
# c. Fuel type: Uranium / TRISO / Other
# d. Enrichment: LEU / HALEU / Not required
#
# Master sample: 91 designs, including microreactors.
# Regional sample: 89 designs.
#
# Rest of World excluded from the four-region comparison.
# Unresolved enrichment excluded ONLY from panel d.
#
# INPUT:
# revision/data/fig10_nea_technological_regional_source_data.xlsx
#
# OUTPUTS:
# revision/figures/fig10_nea_technological_regional.pdf
# revision/figures/fig10_nea_technological_regional.png
# revision/figures/fig10_nea_technological_regional_results.xlsx
#
# Figure dimensions: 180 x 145 mm
# PNG resolution: 600 dpi
# ============================================================


# 1. PACKAGES ------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "patchwork",
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
  invisible(lapply(
    packages,
    library,
    character.only = TRUE
  ))
})


# 2. GITHUB-COMPATIBLE FILE PATHS -----------------------------

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

output_prefix <- "fig10_nea_technological_regional"

pdf_file <- file.path(
  output_dir,
  paste0(output_prefix, ".pdf")
)

png_file <- file.path(
  output_dir,
  paste0(output_prefix, ".png")
)

excel_file <- file.path(
  output_dir,
  paste0(output_prefix, "_results.xlsx")
)

if (!file.exists(input_file)) {
  stop("Source Excel workbook not found: ", input_file)
}


# 3. IMPORT AND VALIDATE MASTER SAMPLE -----------------------

sheet_names <- readxl::excel_sheets(input_file)

sheet_to_use <- if ("nea_50" %in% sheet_names) {
  "nea_50"
} else {
  sheet_names[1]
}

raw <- readxl::read_excel(
  input_file,
  sheet = sheet_to_use
)

names(raw) <- trimws(names(raw))

# Support the legacy spelling of neutron_spectrum.

if (
  !"neutron_spectrum" %in% names(raw) &&
  "neturon_spectrum" %in% names(raw)
) {
  names(raw)[
    names(raw) == "neturon_spectrum"
  ] <- "neutron_spectrum"
}

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
  names(raw)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

cat("\nImported worksheet:", sheet_to_use, "\n")
cat("Imported rows:", nrow(raw), "\n")

# Restrict the analysis to Edition 4.
# Remove blank design names and DF-CDE.

raw <- raw %>%
  dplyr::filter(
    suppressWarnings(
      as.numeric(as.character(edition))
    ) == 4,
    !is.na(design),
    trimws(as.character(design)) != ""
  ) %>%
  dplyr::mutate(
    design = trimws(as.character(design))
  ) %>%
  dplyr::filter(
    toupper(design) != "DF-CDE"
  )

if (anyDuplicated(raw$design)) {
  stop("Duplicate design names detected.")
}

if (nrow(raw) != 91) {
  stop(
    "Expected 91 Edition 4 designs; found ",
    nrow(raw),
    ". Check the source workbook."
  )
}

cat("\nEdition 4 designs:", nrow(raw), "\n")

normalise <- function(x) {
  tolower(trimws(as.character(x)))
}


# 4. CLEAN AND CLASSIFY --------------------------------------

data <- raw %>%
  dplyr::mutate(
    design = trimws(as.character(design)),

    region_raw = normalise(region_cn_ru),
    cooling_raw = normalise(cooling_type),
    spectrum_raw = normalise(neutron_spectrum),
    fuel_raw = normalise(fuel_type),
    enrichment_raw = normalise(fuel_enrichment),

    region = dplyr::case_when(
      region_raw %in% c(
        "north_america",
        "north america"
      ) ~ "North America",

      region_raw == "europe" ~ "Europe",

      region_raw %in% c(
        "oecd_asia",
        "oecd asia"
      ) ~ "OECD Asia",

      region_raw %in% c(
        "china/russia",
        "china_russia",
        "china",
        "russia"
      ) ~ "China/Russia",

      region_raw %in% c(
        "row",
        "rest of world",
        "rest_of_world"
      ) ~ "Rest of World",

      TRUE ~ NA_character_
    ),

    cooling = dplyr::case_when(
      cooling_raw == "water" ~ "Water-cooled",

      cooling_raw %in% c(
        "gas",
        "metal",
        "salt",
        "heat pipe",
        "heat_pipe"
      ) ~ "Non-water-cooled",

      TRUE ~ NA_character_
    ),

    spectrum = dplyr::case_when(
      spectrum_raw == "thermal" ~ "Thermal",
      spectrum_raw == "fast" ~ "Fast",
      TRUE ~ NA_character_
    ),

    fuel = dplyr::case_when(
      fuel_raw == "uranium" ~ "Uranium",
      fuel_raw == "triso" ~ "TRISO",
      fuel_raw == "other" ~ "Other",
      TRUE ~ NA_character_
    ),

    enrichment = dplyr::case_when(
      enrichment_raw == "leu" ~ "LEU",
      enrichment_raw == "haleu" ~ "HALEU",

      enrichment_raw %in% c(
        "not_required",
        "not required"
      ) ~ "Not required",

      enrichment_raw %in% c(
        "unresolved",
        "",
        "na"
      ) | is.na(enrichment_raw) ~ NA_character_,

      TRUE ~ NA_character_
    )
  )


# 5. REGIONAL VALIDATION -------------------------------------

region_order <- c(
  "North America",
  "Europe",
  "OECD Asia",
  "China/Russia"
)

# Unknown regional classifications are errors.
# Rest of World is a recognised exclusion.

unknown_regions <- data %>%
  dplyr::filter(is.na(region))

if (nrow(unknown_regions) > 0) {
  print(
    unknown_regions %>%
      dplyr::select(design, region_raw),
    n = Inf
  )

  stop("Unrecognised regional classifications.")
}

excluded_regions <- data %>%
  dplyr::filter(region == "Rest of World")

# Validate the two expected Rest-of-world exclusions.

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

  print(
    excluded_regions %>%
      dplyr::select(
        design,
        region_raw,
        region
      ),
    n = Inf
  )

  stop("Unexpected regional exclusions detected.")
}

cat("\nREST-OF-WORLD DESIGNS EXCLUDED:\n")

print(
  excluded_regions %>%
    dplyr::select(design, region_raw),
  n = Inf
)

regional_data <- data %>%
  dplyr::filter(region %in% region_order)

if (anyDuplicated(regional_data$design)) {
  stop("Duplicate design names detected.")
}

if (nrow(regional_data) != 89) {
  stop(
    "Expected 89 regional designs; found ",
    nrow(regional_data)
  )
}

cat(
  "\nREGIONAL DESIGNS:",
  nrow(regional_data),
  "\n"
)

# Expected regional sample:
# North America 40, Europe 26, OECD Asia 12,
# China/Russia 11, total 89.

expected_regions <- c(
  "North America" = 40L,
  "Europe" = 26L,
  "OECD Asia" = 12L,
  "China/Russia" = 11L
)

actual_regions <- table(
  factor(
    regional_data$region,
    levels = region_order
  )
)

cat("\nREGIONAL COUNTS:\n")
print(actual_regions)

if (!identical(
  as.integer(actual_regions),
  as.integer(expected_regions)
)) {
  stop(
    "Regional sample counts differ from the expected ",
    "40/26/12/11 distribution. Check the source data."
  )
}


# 6. TECHNOLOGICAL VALIDATION --------------------------------

check_classification <- function(
    source,
    raw_column,
    clean_column,
    description,
    allow_unresolved = FALSE
) {

  invalid <- source %>%
    dplyr::filter(
      is.na(.data[[clean_column]])
    )

  if (allow_unresolved) {
    invalid <- invalid %>%
      dplyr::filter(
        !is.na(.data[[raw_column]]),
        !(.data[[raw_column]] %in% c(
          "",
          "na",
          "unresolved"
        ))
      )
  }

  if (nrow(invalid) > 0) {
    print(
      invalid %>%
        dplyr::select(
          design,
          region,
          dplyr::all_of(raw_column)
        ),
      n = Inf
    )

    stop(
      "Unrecognised ",
      description,
      " classification."
    )
  }
}

check_classification(
  regional_data,
  "cooling_raw",
  "cooling",
  "cooling"
)

check_classification(
  regional_data,
  "spectrum_raw",
  "spectrum",
  "neutron-spectrum"
)

check_classification(
  regional_data,
  "fuel_raw",
  "fuel",
  "fuel-type"
)

check_classification(
  regional_data,
  "enrichment_raw",
  "enrichment",
  "enrichment",
  allow_unresolved = TRUE
)

cat(
  "\nUNRESOLVED ENRICHMENT:",
  sum(is.na(regional_data$enrichment)),
  "\n"
)


# 7. PANEL COMPOSITION DATA ----------------------------------

dimensions <- c(
  "cooling",
  "spectrum",
  "fuel",
  "enrichment"
)

make_composition <- function(variable) {

  regional_data %>%
    dplyr::filter(
      !is.na(.data[[variable]])
    ) %>%
    dplyr::count(
      region,
      category = .data[[variable]],
      name = "n"
    ) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      region_n = sum(n),
      share = n / region_n
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dimension = variable
    )
}

composition_data <- dplyr::bind_rows(
  lapply(dimensions, make_composition)
)

panel_sample_sizes <- regional_data %>%
  dplyr::summarise(
    `Cooling type` = sum(!is.na(cooling)),
    `Neutron spectrum` = sum(!is.na(spectrum)),
    `Fuel type` = sum(!is.na(fuel)),
    `Fuel enrichment` = sum(!is.na(enrichment))
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "panel",
    values_to = "n"
  )

region_sample_sizes <- regional_data %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    cooling_n = sum(!is.na(cooling)),
    spectrum_n = sum(!is.na(spectrum)),
    fuel_n = sum(!is.na(fuel)),
    enrichment_n = sum(!is.na(enrichment)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = region_order
    )
  ) %>%
  dplyr::arrange(region) %>%
  dplyr::mutate(
    region = as.character(region)
  )


# 8. SUPPLEMENTARY TABLE S13 ---------------------------------

# All four technological panels are calculated independently.
# Unresolved enrichment is excluded from enrichment counts,
# but remains in the other three panels.

s13_main <- regional_data %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n = dplyr::n(),

    water = sum(cooling == "Water-cooled"),
    non_water = sum(cooling == "Non-water-cooled"),

    thermal = sum(spectrum == "Thermal"),
    fast = sum(spectrum == "Fast"),

    uranium = sum(fuel == "Uranium"),
    triso = sum(fuel == "TRISO"),
    other = sum(fuel == "Other"),

    leu = sum(enrichment == "LEU", na.rm = TRUE),
    haleu = sum(enrichment == "HALEU", na.rm = TRUE),
    not_required = sum(
      enrichment == "Not required",
      na.rm = TRUE
    ),

    enrichment_n = sum(!is.na(enrichment)),

    .groups = "drop"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = region_order
    )
  ) %>%
  dplyr::arrange(region)

format_count <- function(count, denominator) {
  sprintf(
    "%d (%.1f)",
    count,
    100 * count / denominator
  )
}

s13_display <- s13_main %>%
  dplyr::transmute(
    Region = as.character(region),
    n = n,

    `Water-cooled` = format_count(water, n),
    `Non-water-cooled` = format_count(non_water, n),

    Thermal = format_count(thermal, n),
    Fast = format_count(fast, n),

    Uranium = format_count(uranium, n),
    TRISO = format_count(triso, n),
    `Other fuel` = format_count(other, n),

    LEU = format_count(leu, enrichment_n),
    HALEU = format_count(haleu, enrichment_n),
    `Not required` = format_count(
      not_required,
      enrichment_n
    )
  )

# Overall total row.
# Enrichment percentages use classified enrichment records,
# not the full regional sample.

s13_total <- s13_main %>%
  dplyr::summarise(
    n = sum(n),

    water = sum(water),
    non_water = sum(non_water),

    thermal = sum(thermal),
    fast = sum(fast),

    uranium = sum(uranium),
    triso = sum(triso),
    other = sum(other),

    leu = sum(leu),
    haleu = sum(haleu),
    not_required = sum(not_required),

    enrichment_n = sum(enrichment_n)
  ) %>%
  dplyr::transmute(
    Region = "Total",
    n = n,

    `Water-cooled` = format_count(water, n),
    `Non-water-cooled` = format_count(non_water, n),

    Thermal = format_count(thermal, n),
    Fast = format_count(fast, n),

    Uranium = format_count(uranium, n),
    TRISO = format_count(triso, n),
    `Other fuel` = format_count(other, n),

    LEU = format_count(leu, enrichment_n),
    HALEU = format_count(haleu, enrichment_n),
    `Not required` = format_count(
      not_required,
      enrichment_n
    )
  )

s13_display <- dplyr::bind_rows(
  s13_display,
  s13_total
)

# Verify that the categories sum correctly.

if (any(
  s13_main$water +
  s13_main$non_water != s13_main$n
)) {
  stop("Cooling counts do not sum to regional sample sizes.")
}

if (any(
  s13_main$thermal +
  s13_main$fast != s13_main$n
)) {
  stop("Spectrum counts do not sum to regional sample sizes.")
}

if (any(
  s13_main$uranium +
  s13_main$triso +
  s13_main$other != s13_main$n
)) {
  stop("Fuel-type counts do not sum to regional sample sizes.")
}

if (any(
  s13_main$leu +
  s13_main$haleu +
  s13_main$not_required != s13_main$enrichment_n
)) {
  stop("Enrichment counts do not sum to classified samples.")
}


# 9. FIGURE THEME --------------------------------------------

figure_theme <- ggplot2::theme_classic(
  base_size = 7
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 9,
      hjust = 0.5
    ),

    axis.title.y = ggplot2::element_blank(),

    axis.title.x = ggplot2::element_text(
      size = 7,
      colour = "grey35"
    ),

    axis.text = ggplot2::element_text(
      size = 7,
      colour = "grey35"
    ),

    axis.line = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    axis.ticks = ggplot2::element_line(
      colour = "grey35",
      linewidth = 0.3
    ),

    panel.grid = ggplot2::element_blank(),

    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),

    legend.text = ggplot2::element_text(
      size = 7
    ),

    legend.key.size = grid::unit(
      3,
      "mm"
    ),

    plot.margin = ggplot2::margin(
      4, 5, 4, 5
    )
  )


# 10. PANEL FUNCTION -----------------------------------------

make_panel <- function(
    variable,
    panel_title,
    categories,
    colours
) {

  plot_data <- composition_data %>%
    dplyr::filter(
      dimension == variable
    ) %>%
    dplyr::mutate(
      region = factor(
        region,
        levels = rev(region_order)
      ),

      category = factor(
        category,
        levels = categories
      )
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = share,
      y = region,
      fill = category
    )
  ) +
    ggplot2::geom_col(
      width = 0.72,
      position = ggplot2::position_stack(
        reverse = TRUE
      )
    ) +
    ggplot2::scale_fill_manual(
      values = colours,
      breaks = categories,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(
        accuracy = 1
      ),
      breaks = c(
        0, 0.25, 0.5, 0.75, 1
      ),
      limits = c(0, 1),
      expand = ggplot2::expansion(
        mult = c(0, 0)
      )
    ) +
    ggplot2::labs(
      title = panel_title,
      x = "Share"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    ) +
    figure_theme
}


# 11. BUILD FOUR PANELS --------------------------------------

panel_a <- make_panel(
  "cooling",
  "Reactor cooling type",
  c(
    "Water-cooled",
    "Non-water-cooled"
  ),
  c(
    "Water-cooled" = "#70B2D6",
    "Non-water-cooled" = "#E68B8B"
  )
)

panel_b <- make_panel(
  "spectrum",
  "Neutron spectrum",
  c(
    "Thermal",
    "Fast"
  ),
  c(
    "Thermal" = "#4E80B1",
    "Fast" = "#F28E2B"
  )
)

panel_c <- make_panel(
  "fuel",
  "Fuel type",
  c(
    "Uranium",
    "TRISO",
    "Other"
  ),
  c(
    "Uranium" = "#E15759",
    "TRISO" = "#59A14F",
    "Other" = "#9D9D9D"
  )
)

panel_d <- make_panel(
  "enrichment",
  "Fuel enrichment",
  c(
    "LEU",
    "HALEU",
    "Not required"
  ),
  c(
    "LEU" = "#E15759",
    "HALEU" = "#9C6ADE",
    "Not required" = "#9D9D9D"
  )
)


# 12. COMBINE FIGURE -----------------------------------------

figure_10 <- (
  panel_a + panel_b
) / (
  panel_c + panel_d
) +
  patchwork::plot_annotation(
    tag_levels = "a"
  ) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(
      face = "bold",
      size = 10
    ),

    plot.tag.position = c(0, 1)
  )

print(figure_10)


# 13. EXPORT FIGURE ------------------------------------------

ggplot2::ggsave(
  filename = pdf_file,
  plot = figure_10,
  width = 180,
  height = 145,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggplot2::ggsave(
  filename = png_file,
  plot = figure_10,
  width = 180,
  height = 145,
  units = "mm",
  dpi = 600,
  bg = "white"
)


# 14. EXPORT EXCEL -------------------------------------------

classification_audit <- regional_data %>%
  dplyr::select(
    design,
    region,
    cooling,
    spectrum,
    fuel,
    enrichment
  )

excel_tables <- list(

  "S13 descriptive table" = s13_display,

  "Composition" = composition_data,

  "Panel sample sizes" = panel_sample_sizes,

  "Regional sample sizes" = region_sample_sizes,

  "Classification audit" = classification_audit,

  "Excluded regions" = excluded_regions %>%
    dplyr::select(
      design,
      region_raw,
      region
    )
)

openxlsx::write.xlsx(
  excel_tables,
  file = excel_file,
  overwrite = TRUE
)


# 15. FINAL VALIDATION ---------------------------------------

output_files <- c(
  pdf_file,
  png_file,
  excel_file
)

if (!all(file.exists(output_files))) {
  stop("One or more output files are missing.")
}

file_information <- file.info(output_files)

if (any(
  is.na(file_information$size) |
  file_information$size == 0
)) {
  stop("One or more output files are empty.")
}

cat("\n")
cat("================================================\n")
cat("FIGURE 10 COMPLETE\n")
cat("================================================\n")

cat(
  "\nTotal Edition 4 designs:",
  nrow(data),
  "\n"
)

cat(
  "Designs in regional analysis:",
  nrow(regional_data),
  "\n"
)

cat(
  "Excluded Rest-of-world designs:",
  nrow(excluded_regions),
  "\n"
)

cat(
  "Unresolved regional enrichment records:",
  sum(is.na(regional_data$enrichment)),
  "\n"
)

cat("\nREGIONAL COUNTS:\n")
print(actual_regions)

cat("\nSUPPLEMENTARY TABLE S13:\n")
print(s13_display, n = Inf, width = Inf)

cat("\nPANEL SAMPLE SIZES:\n")
print(panel_sample_sizes, n = Inf)

cat("\nREGIONAL SAMPLE SIZES:\n")
print(region_sample_sizes, n = Inf)

cat("\nEXCLUDED REGIONS:\n")

print(
  excluded_regions %>%
    dplyr::select(design, region_raw),
  n = Inf
)

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


# ============================================================
# END OF SCRIPT
#
# Conversion:
# Updated JAG analytical logic, enrichment denominators,
# Supplementary Table S13 calculations, figure specifications
# and Excel worksheet structure retained.
#
# Repository-relative paths and existing GitHub output
# filenames restored.
#
# Existing GitHub safeguards retained for Edition 4 filtering,
# master sample size, regional exclusions and output validation.
