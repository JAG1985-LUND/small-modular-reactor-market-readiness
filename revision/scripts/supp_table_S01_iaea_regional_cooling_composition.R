
# ================================================================
# SUPPLEMENTARY TABLE S1
# IAEA SMR catalogue composition, 2012–2026
# Regional and cooling distributions
#
# Script:
# revision/scripts/supp_table_S01_iaea_regional_cooling_composition.R
#
# Input:
# table_S01_iaea_2012_2026_source_data.xlsx
#
# Output:
# revision/figures/supp_table_S01_iaea_regional_cooling_composition_results.xlsx
#
# Five worksheets:
# 1. S1_Regional
# 2. S1_Cooling
# 3. Audit_Totals
# 4. Audit_Groups
# 5. Source_Data
# ================================================================


# 1. PACKAGES ----------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
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
  library(openxlsx)
})


# 2. GITHUB-COMPATIBLE FILE PATHS --------------------------------

source_file <- file.path(
  "revision",
  "data",
  "table_S01_iaea_2012_2026_source_data.xlsx"
)

find_repo_root <- function(
    start_dir = getwd(),
    target_file = source_file
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
        "Cannot locate repository root.\n",
        "Expected input file: ",
        target_file
      )
    }
    
    current <- parent
  }
}

root_dir <- find_repo_root()

input_file <- file.path(
  root_dir,
  source_file
)

output_dir <- file.path(
  root_dir,
  "revision",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  paste0(
    "supp_table_S01_",
    "iaea_regional_cooling_composition_results.xlsx"
  )
)


# 3. IMPORT DATA -------------------------------------------------

dat <- readxl::read_excel(input_file)

required <- c(
  "publication_year",
  "smr_design",
  "classification",
  "region",
  "cooling_type"
)

missing_columns <- setdiff(
  required,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# 4. STANDARDISE VARIABLES ---------------------------------------

dat <- dat %>%
  dplyr::mutate(
    publication_year = as.integer(publication_year),
    
    smr_design = stringr::str_trim(
      as.character(smr_design)
    ),
    
    classification = stringr::str_trim(
      as.character(classification)
    ),
    
    region = stringr::str_trim(
      as.character(region)
    ),
    
    cooling_type = stringr::str_trim(
      as.character(cooling_type)
    ),
    
    reactor_group = dplyr::case_when(
      classification == "small_modular_reactor" ~
        "Conventional SMRs",
      
      classification == "micro_reactor" ~
        "Microreactors",
      
      TRUE ~ NA_character_
    ),
    
    region = dplyr::case_when(
      region == "rest_of_the_world" ~
        "Rest of the World",
      
      TRUE ~ region
    )
  )

if (anyNA(dat$reactor_group)) {
  stop("Unrecognised reactor classification found.")
}

if (
  anyNA(dat$publication_year) ||
  anyNA(dat$smr_design) ||
  any(dat$smr_design == "")
) {
  stop(
    "Missing publication years or reactor design names."
  )
}


# 5. CHECK DUPLICATE DESIGN-EDITION RECORDS ---------------------

duplicates <- dat %>%
  dplyr::count(
    publication_year,
    smr_design,
    name = "n"
  ) %>%
  dplyr::filter(n > 1)

if (nrow(duplicates) > 0) {
  
  print(duplicates)
  
  stop(
    "Duplicate design names within an edition detected. ",
    "Inspect the source before calculating totals."
  )
}


# 6. ADD ALL-DESIGNS ROWS ----------------------------------------

all_designs <- dat %>%
  dplyr::mutate(
    reactor_group = "All designs"
  )

combined <- dplyr::bind_rows(
  dat,
  all_designs
)

years <- sort(
  unique(dat$publication_year)
)

groups <- c(
  "All designs",
  "Conventional SMRs",
  "Microreactors"
)


# 7. REGIONAL DISTRIBUTION ---------------------------------------

region_levels <- c(
  "North America",
  "Russia",
  "Asia",
  "Europe",
  "Rest of the World"
)

unknown_regions <- setdiff(
  unique(combined$region),
  region_levels
)

if (length(unknown_regions) > 0) {
  stop(
    "Unrecognised regions: ",
    paste(unknown_regions, collapse = ", ")
  )
}

regional <- combined %>%
  dplyr::count(
    publication_year,
    reactor_group,
    region,
    name = "n"
  ) %>%
  tidyr::complete(
    publication_year = years,
    reactor_group = groups,
    region = region_levels,
    fill = list(n = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = region,
    values_from = n
  ) %>%
  dplyr::mutate(
    `Total designs` = rowSums(
      dplyr::across(
        dplyr::all_of(region_levels)
      )
    )
  ) %>%
  dplyr::select(
    `Publication year` = publication_year,
    `Reactor group` = reactor_group,
    dplyr::all_of(region_levels),
    `Total designs`
  ) %>%
  dplyr::arrange(
    `Publication year`,
    match(`Reactor group`, groups)
  )


# 8. COOLING DISTRIBUTION ----------------------------------------

cooling_levels <- c(
  "water-cooled",
  "metal-cooled",
  "gas-cooled",
  "molten salt-cooled"
)

cooling <- combined %>%
  dplyr::mutate(
    cooling_group = dplyr::case_when(
      cooling_type %in% cooling_levels ~
        cooling_type,
      
      TRUE ~ "Excluded other cooling"
    )
  ) %>%
  dplyr::count(
    publication_year,
    reactor_group,
    cooling_group,
    name = "n"
  ) %>%
  tidyr::complete(
    publication_year = years,
    reactor_group = groups,
    cooling_group = c(
      cooling_levels,
      "Excluded other cooling"
    ),
    fill = list(n = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = cooling_group,
    values_from = n
  ) %>%
  dplyr::mutate(
    `Included total` = rowSums(
      dplyr::across(
        dplyr::all_of(cooling_levels)
      )
    ),
    
    `Full catalogue total` =
      `Included total` + `Excluded other cooling`
  ) %>%
  dplyr::select(
    `Publication year` = publication_year,
    `Reactor group` = reactor_group,
    
    `Water-cooled` = `water-cooled`,
    `Metal-cooled` = `metal-cooled`,
    `Gas-cooled` = `gas-cooled`,
    `Molten salt-cooled` = `molten salt-cooled`,
    
    `Excluded other cooling`,
    `Included total`,
    `Full catalogue total`
  ) %>%
  dplyr::arrange(
    `Publication year`,
    match(`Reactor group`, groups)
  )


# 9. RECONCILIATION AUDIT ----------------------------------------

audit <- regional %>%
  dplyr::select(
    `Publication year`,
    `Reactor group`,
    `Regional total` = `Total designs`
  ) %>%
  dplyr::left_join(
    cooling %>%
      dplyr::select(
        `Publication year`,
        `Reactor group`,
        `Cooling total` = `Full catalogue total`
      ),
    by = c(
      "Publication year",
      "Reactor group"
    )
  ) %>%
  dplyr::mutate(
    Difference = `Regional total` - `Cooling total`,
    
    Check = dplyr::if_else(
      Difference == 0,
      "PASS",
      "FAIL"
    )
  )

group_audit <- regional %>%
  dplyr::select(
    `Publication year`,
    `Reactor group`,
    `Total designs`
  ) %>%
  tidyr::pivot_wider(
    names_from = `Reactor group`,
    values_from = `Total designs`
  ) %>%
  dplyr::mutate(
    `Conventional + micro` =
      `Conventional SMRs` + Microreactors,
    
    Check = dplyr::if_else(
      `All designs` == `Conventional + micro`,
      "PASS",
      "FAIL"
    )
  )

if (
  any(audit$Check != "PASS") ||
  any(group_audit$Check != "PASS")
) {
  stop(
    "Reconciliation failed. Check the source data."
  )
}


# 10. EXPORT ONE EXCEL WORKBOOK ----------------------------------

wb <- openxlsx::createWorkbook()

tables <- list(
  "S1_Regional" = regional,
  "S1_Cooling" = cooling,
  "Audit_Totals" = audit,
  "Audit_Groups" = group_audit,
  "Source_Data" = dat
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#DCE6F1",
  halign = "center",
  border = "Bottom"
)

for (sheet in names(tables)) {
  
  openxlsx::addWorksheet(
    wb,
    sheet
  )
  
  openxlsx::writeData(
    wb,
    sheet,
    tables[[sheet]],
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  openxlsx::freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(tables[[sheet]])),
    widths = "auto"
  )
}

openxlsx::saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


# 11. DISPLAY RESULTS --------------------------------------------

cat(
  "\nSUPPLEMENTARY TABLE S1: REGIONAL DISTRIBUTION\n"
)

print(
  regional,
  n = Inf,
  width = Inf
)

cat(
  "\nSUPPLEMENTARY TABLE S1: COOLING DISTRIBUTION\n"
)

print(
  cooling,
  n = Inf,
  width = Inf
)

cat("\nGROUP RECONCILIATION\n")

print(
  group_audit,
  n = Inf
)

stopifnot(
  file.exists(output_file)
)

cat(
  "\nWorkbook saved to:\n",
  normalizePath(
    output_file,
    winslash = "/"
  ),
  "\n"
)
