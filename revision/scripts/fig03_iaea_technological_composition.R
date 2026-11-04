
# ============================================================
# 2. GITHUB-COMPATIBLE PATHS
# ============================================================

SOURCE_FILE <- file.path(
  "revision",
  "data",
  "fig03_iaea_technology_composition_source_data.xlsx"
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
  stop("Input workbook not found: ", input_file)
}

output_png <- file.path(
  output_dir,
  "fig03_iaea_technology_composition.png"
)

output_pdf <- file.path(
  output_dir,
  "fig03_iaea_technology_composition.pdf"
)

years <- seq(2012, 2026, by = 2)
