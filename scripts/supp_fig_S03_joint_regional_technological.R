# ================================================================
# SUPPLEMENTARY FIGURES S3(a) AND S3(b)
# JOINT REGIONAL AND TECHNOLOGICAL ASSOCIATIONS
# NEA SMR DASHBOARD, EDITION 4
#
# GITHUB REPRODUCIBLE VERSION
#
# Run the Supplementary Tables S14-S15 script first.
#
# Input:
# revision/figures/
#   supp_tables_S14_S15_joint_models_results.xlsx
#
# Outputs:
# revision/figures/
#   supp_fig_S03_joint_regional_technological_all_designs.png
#   supp_fig_S03_joint_regional_technological_all_designs.pdf
#   supp_fig_S04_joint_regional_technological_conventional_smrs.png
#   supp_fig_S04_joint_regional_technological_conventional_smrs.pdf
#   supp_fig_S03_S04_model_verification.xlsx
#
# S3(a): All designs (n = 85)
# S3(b): Conventional SMRs (n = 51)
#
# Two Rest-of-world designs excluded.
# Four additional designs excluded owing to unresolved enrichment.
# China and Russia combined.
#
# Model:
# Cumulative-link ordinal logistic regression
# Region + cooling + spectrum + fuel type + enrichment
#
# Reference categories:
# North America, water-cooled, thermal spectrum,
# uranium fuel and LEU.
#
# Figures are reconstructed directly from the validated
# regression results reported in Supplementary Table S14.
# No regression refitting is undertaken.
#
# The plotting specifications reproduce the successful
# JAG figure recovery script.
#
# This GitHub adaptation has not been independently executed.
# ================================================================


# ================================================================
# 1. PACKAGES
# ================================================================

packages <- c(
  "readxl",
  "ggplot2",
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

graphics.off()


# ================================================================
# 2. REPOSITORY PATHS
# ================================================================

source_filename <-
  "supp_tables_S14_S15_joint_models_results.xlsx"

find_repo_root <- function() {

  override <- Sys.getenv(
    "SMR_REPO_ROOT",
    unset = ""
  )

  candidates <- getwd()

  if (nzchar(override)) {
    candidates <- c(
      override,
      candidates
    )
  }

  args <- commandArgs(
    trailingOnly = FALSE
  )

  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )

  if (length(file_arg) > 0) {

    candidates <- c(
      candidates,
      dirname(
        sub(
          "^--file=",
          "",
          file_arg[1]
        )
      )
    )
  }

  frames <- sys.frames()

  for (frame in frames) {

    if (!is.null(frame$ofile)) {

      candidates <- c(
        candidates,
        dirname(frame$ofile)
      )
    }
  }

  candidates <- unique(
    normalizePath(
      candidates,
      winslash = "/",
      mustWork = FALSE
    )
  )

  for (candidate in candidates) {

    current <- candidate

    repeat {

      target <- file.path(
        current,
        "revision",
        "figures",
        source_filename
      )

      if (file.exists(target)) {
        return(current)
      }

      parent <- dirname(current)

      if (identical(parent, current)) {
        break
      }

      current <- parent
    }
  }

  stop(
    "Repository root or regression results workbook not found.\n",
    "Expected revision/figures/",
    source_filename,
    "\nRun the Supplementary Tables S14-S15 script first."
  )
}

repo_root <- find_repo_root()

output_dir <- file.path(
  repo_root,
  "revision",
  "figures"
)

input_file <- file.path(
  output_dir,
  source_filename
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\nRepository root:\n", repo_root, "\n")

cat("\nInput workbook:\n", input_file, "\n")

cat("\nOutput directory:\n", output_dir, "\n")


# ================================================================
# 3. IMPORT VALIDATED REGRESSION RESULTS
# ================================================================

required_sheets <- c(
  "Adjusted odds ratios",
  "Model audit",
  "Sample audit",
  "Region audit",
  "Excluded region",
  "Excluded enrichment",
  "table_S14"
)

available_sheets <- readxl::excel_sheets(
  input_file
)

missing_sheets <- setdiff(
  required_sheets,
  available_sheets
)

if (length(missing_sheets) > 0) {

  stop(
    "Missing required worksheets: ",
    paste(
      missing_sheets,
      collapse = ", "
    )
  )
}

coefficients <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Adjusted odds ratios"
  )
)

model_audit <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Model audit"
  )
)

sample_audit <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Sample audit"
  )
)

region_audit <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Region audit"
  )
)

excluded_region <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Excluded region"
  )
)

excluded_enrichment <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "Excluded enrichment"
  )
)

table_s14 <- as.data.frame(
  readxl::read_excel(
    input_file,
    sheet = "table_S14"
  )
)


# ================================================================
# 4. VALIDATE MODEL RESULTS
# ================================================================

required_columns <- c(
  "sample",
  "outcome",
  "n",
  "predictor",
  "term_label",
  "odds_ratio",
  "ci_low",
  "ci_high",
  "p_wald"
)

missing_columns <- setdiff(
  required_columns,
  names(coefficients)
)

if (length(missing_columns) > 0) {

  stop(
    "Missing regression columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

stopifnot(

  nrow(coefficients) == 90,

  nrow(model_audit) == 40,

  all(
    model_audit$status == "Converged"
  ),

  nrow(table_s14) == 90,

  nrow(excluded_region) == 2,

  nrow(excluded_enrichment) == 4,

  all(
    is.finite(coefficients$odds_ratio)
  ),

  all(
    is.finite(coefficients$ci_low)
  ),

  all(
    is.finite(coefficients$ci_high)
  ),

  all(
    coefficients$odds_ratio > 0
  ),

  all(
    coefficients$ci_low > 0
  ),

  all(
    coefficients$ci_high >=
      coefficients$ci_low
  ),

  all(
    coefficients$odds_ratio >=
      coefficients$ci_low
  ),

  all(
    coefficients$odds_ratio <=
      coefficients$ci_high
  )
)

expected_samples <- c(
  "all_designs",
  "conventional"
)

expected_n <- c(
  all_designs = 85,
  conventional = 51
)

stopifnot(

  setequal(
    unique(coefficients$sample),
    expected_samples
  ),

  sum(
    coefficients$sample == "all_designs"
  ) == 45,

  sum(
    coefficients$sample == "conventional"
  ) == 45,

  all(
    coefficients$n[
      coefficients$sample == "all_designs"
    ] == 85
  ),

  all(
    coefficients$n[
      coefficients$sample == "conventional"
    ] == 51
  )
)

cat("\nALL 40 MODELS PASSED VALIDATION.\n")

cat("\nALL 90 REGRESSION COEFFICIENTS AVAILABLE.\n")


# ================================================================
# 5. OUTCOME AND PREDICTOR LABELS
# ================================================================

outcomes <- c(
  "financing",
  "supply_chain",
  "fuel_supply",
  "licensing",
  "siting"
)

outcome_labels <- c(
  financing = "Financing",
  supply_chain = "Supply chain",
  fuel_supply = "Fuel supply",
  licensing = "Licensing",
  siting = "Siting"
)

sample_labels <- c(
  all_designs = "All designs",
  conventional = "Conventional SMRs"
)

term_labels <- c(
  "regionEurope" = "Europe",
  "regionOECD Asia" = "OECD Asia",
  "regionChina/Russia" = "China/Russia",
  "coolingNon-water-cooled" = "Non-water-cooled",
  "spectrumFast" = "Fast-spectrum",
  "fuelTRISO" = "TRISO fuel",
  "fuelOther" = "Other fuel",
  "enrichmentHALEU" = "HALEU",
  "enrichmentNot required" = "Enrichment not required"
)

plot_order <- c(
  "Enrichment not required",
  "HALEU",
  "Other fuel",
  "TRISO fuel",
  "Fast-spectrum",
  "Non-water-cooled",
  "Europe",
  "OECD Asia",
  "China/Russia"
)


# ================================================================
# 6. COEFFICIENT COMPLETENESS AUDIT
# ================================================================

for (sample_id in expected_samples) {

  for (outcome_id in outcomes) {

    df <- coefficients[
      coefficients$sample == sample_id &
        coefficients$outcome == outcome_id,
      ,
      drop = FALSE
    ]

    stopifnot(

      nrow(df) == 9,

      !anyDuplicated(df$predictor),

      setequal(
        df$predictor,
        names(term_labels)
      ),

      all(
        df$n == expected_n[[sample_id]]
      ),

      all(
        df$term_label ==
          unname(
            term_labels[df$predictor]
          )
      )
    )
  }
}

combined_model_audit <- model_audit[
  model_audit$specification == "M3_combined",
  ,
  drop = FALSE
]

stopifnot(

  nrow(combined_model_audit) == 10,

  all(
    combined_model_audit$status == "Converged"
  ),

  !anyDuplicated(
    paste(
      combined_model_audit$sample,
      combined_model_audit$outcome,
      sep = "__"
    )
  )
)

for (sample_id in expected_samples) {

  df <- combined_model_audit[
    combined_model_audit$sample == sample_id,
    ,
    drop = FALSE
  ]

  stopifnot(

    nrow(df) == 5,

    setequal(
      df$outcome,
      outcomes
    ),

    all(
      df$n == expected_n[[sample_id]]
    )
  )
}


# ================================================================
# 7. VERIFY CONSISTENCY WITH SUPPLEMENTARY TABLE S14
# ================================================================

required_table_columns <- c(
  "Sample",
  "Readiness dimension",
  "Predictor",
  "n",
  "Adjusted OR",
  "95% CI lower",
  "95% CI upper",
  "Wald p"
)

stopifnot(
  all(
    required_table_columns %in%
      names(table_s14)
  )
)

coefficient_keys <- paste(

  unname(
    sample_labels[coefficients$sample]
  ),

  unname(
    outcome_labels[coefficients$outcome]
  ),

  coefficients$term_label,

  sep = "__"
)

table_keys <- paste(

  table_s14$Sample,

  table_s14[["Readiness dimension"]],

  table_s14$Predictor,

  sep = "__"
)

stopifnot(

  !anyDuplicated(coefficient_keys),

  !anyDuplicated(table_keys),

  setequal(
    coefficient_keys,
    table_keys
  )
)

matched_rows <- match(
  coefficient_keys,
  table_keys
)

stopifnot(
  !anyNA(matched_rows)
)

matched_table <- table_s14[
  matched_rows,
  ,
  drop = FALSE
]

compare_numeric <- function(x, y) {

  isTRUE(
    all.equal(
      as.numeric(x),
      as.numeric(y),
      tolerance = 1e-10,
      check.attributes = FALSE
    )
  )
}

stopifnot(

  compare_numeric(
    coefficients$n,
    matched_table$n
  ),

  compare_numeric(
    coefficients$odds_ratio,
    matched_table[["Adjusted OR"]]
  ),

  compare_numeric(
    coefficients$ci_low,
    matched_table[["95% CI lower"]]
  ),

  compare_numeric(
    coefficients$ci_high,
    matched_table[["95% CI upper"]]
  ),

  compare_numeric(
    coefficients$p_wald,
    matched_table[["Wald p"]]
  )
)

cat(
  "\nFIGURE DATA VERIFIED AGAINST SUPPLEMENTARY TABLE S14.\n"
)


# ================================================================
# 8. RECONSTRUCT PLOTTING DATA
# ================================================================

plot_data <- as.data.frame(
  coefficients
)

plot_data$term_label <- factor(
  plot_data$term_label,
  levels = plot_order
)

plot_data$outcome <- factor(
  plot_data$outcome,
  levels = outcomes,
  labels = unname(
    outcome_labels
  )
)

recovery_data <- as.data.frame(
  plot_data
)

stopifnot(

  nrow(recovery_data) == 90,

  !anyNA(
    recovery_data$term_label
  ),

  !anyNA(
    recovery_data$outcome
  ),

  all(
    is.finite(recovery_data$odds_ratio)
  ),

  all(
    is.finite(recovery_data$ci_low)
  ),

  all(
    is.finite(recovery_data$ci_high)
  ),

  all(
    recovery_data$ci_low > 0
  )
)


# ================================================================
# 9. RECONSTRUCT PANELS
# SUCCESSFUL JAG FIGURE RECOVERY SPECIFICATION
# ================================================================

recover_panel <- function(
    sample_id,
    title_text
) {

  df <- recovery_data[
    recovery_data$sample == sample_id,
    ,
    drop = FALSE
  ]

  stopifnot(
    nrow(df) == 45
  )

  ggplot2::ggplot(df) +

    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.3
    ) +

    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_low,
        xend = ci_high,
        y = term_label,
        yend = term_label
      ),
      colour = "#527FB1",
      linewidth = 0.35
    ) +

    ggplot2::geom_point(
      ggplot2::aes(
        x = odds_ratio,
        y = term_label
      ),
      shape = 16,
      colour = "#527FB1",
      size = 1.2
    ) +

    ggplot2::scale_x_log10(
      breaks = c(
        0.001,
        0.01,
        0.1,
        1,
        10,
        100,
        1000
      ),
      labels = scales::label_number()
    ) +

    ggplot2::facet_wrap(
      ~ outcome,
      ncol = 3,
      drop = FALSE,
      scales = "free_x"
    ) +

    ggplot2::labs(
      title = title_text,
      x = "Adjusted odds ratio (log scale)",
      y = NULL
    ) +

    ggplot2::theme_bw(
      base_size = 7
    ) +

    ggplot2::theme(

      panel.grid = ggplot2::element_blank(),

      panel.border = ggplot2::element_blank(),

      axis.line.x = ggplot2::element_line(
        colour = "grey35",
        linewidth = 0.3
      ),

      axis.line.y = ggplot2::element_line(
        colour = "grey35",
        linewidth = 0.3
      ),

      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = "grey50",
        linewidth = 0.3
      ),

      strip.text = ggplot2::element_text(
        face = "bold"
      ),

      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        size = 9
      ),

      axis.text.y = ggplot2::element_text(
        size = 7
      ),

      axis.text.x = ggplot2::element_text(
        size = 6
      ),

      panel.spacing = grid::unit(
        2.5,
        "mm"
      )
    )
}


# ================================================================
# 10. SUPPLEMENTARY FIGURE S3(a)
# ALL DESIGNS
# ================================================================

recovered_a <- recover_panel(

  "all_designs",

  "(a) All designs (n = 85)"
)


# ================================================================
# 11. SUPPLEMENTARY FIGURE S3(b)
# CONVENTIONAL SMRs
# ================================================================

recovered_b <- recover_panel(

  "conventional",

  "(b) Conventional SMRs (n = 51)"
)


# ================================================================
# 12. EXPORT USING FRESH GRAPHICS DEVICES
# ================================================================

export_recovered <- function(
    plot,
    filename
) {

  pdf_file <- file.path(
    output_dir,
    paste0(
      filename,
      ".pdf"
    )
  )

  png_file <- file.path(
    output_dir,
    paste0(
      filename,
      ".png"
    )
  )

  ggplot2::ggsave(

    filename = pdf_file,

    plot = plot,

    width = 180,

    height = 145,

    units = "mm",

    device = grDevices::pdf,

    bg = "white"
  )

  ggplot2::ggsave(

    filename = png_file,

    plot = plot,

    width = 180,

    height = 145,

    units = "mm",

    dpi = 600,

    bg = "white"
  )

  for (f in c(pdf_file, png_file)) {

    if (
      !file.exists(f) ||
      is.na(file.info(f)$size) ||
      file.info(f)$size == 0
    ) {

      stop(
        "Missing or empty figure output: ",
        f
      )
    }
  }

  invisible(
    c(
      pdf_file,
      png_file
    )
  )
}


# ================================================================
# 13. EXPORT SUPPLEMENTARY FIGURE S3(a)
# ================================================================

files_s03 <- export_recovered(

  recovered_a,

  "supp_fig_S03_joint_regional_technological_all_designs"
)


# ================================================================
# 14. EXPORT SUPPLEMENTARY FIGURE S3(b)
# ================================================================

files_s04 <- export_recovered(

  recovered_b,

  "supp_fig_S04_joint_regional_technological_conventional_smrs"
)


# ================================================================
# 15. EXPORT MODEL VERIFICATION WORKBOOK
# ================================================================

excel_file <- file.path(

  output_dir,

  "supp_fig_S03_S04_model_verification.xlsx"
)

figure_audit <- data.frame(

  Figure = c(
    "Supplementary Figure S3(a)",
    "Supplementary Figure S3(b)"
  ),

  Sample = c(
    "All designs",
    "Conventional SMRs"
  ),

  n = c(
    85,
    51
  ),

  Readiness_dimensions = c(
    5,
    5
  ),

  Coefficients = c(
    45,
    45
  ),

  Model_status = c(
    "Converged",
    "Converged"
  ),

  Table_S14_consistency = c(
    "Verified",
    "Verified"
  )
)

excel_tables <- list(

  "Model diagnostics" = combined_model_audit,

  "Adjusted odds ratios" = coefficients,

  "Figure data" = recovery_data,

  "Regional counts" = region_audit,

  "Excluded designs" = excluded_region,

  "Excluded enrichment" = excluded_enrichment,

  "Sample audit" = sample_audit,

  "Full model audit" = model_audit,

  "Figure audit" = figure_audit,

  "Table S14 verification" = matched_table
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(

  textDecoration = "bold",

  fgFill = "#DCE6F1"
)

for (sheet in names(excel_tables)) {

  dat_sheet <- as.data.frame(
    excel_tables[[sheet]]
  )

  openxlsx::addWorksheet(
    wb,
    sheet
  )

  openxlsx::writeData(

    wb,

    sheet,

    dat_sheet,

    headerStyle = header_style
  )

  openxlsx::freezePane(

    wb,

    sheet,

    firstRow = TRUE
  )

  if (
    nrow(dat_sheet) > 0 &&
    ncol(dat_sheet) > 0
  ) {

    openxlsx::addFilter(

      wb,

      sheet,

      rows = 1,

      cols = seq_len(
        ncol(dat_sheet)
      )
    )
  }

  openxlsx::setColWidths(

    wb,

    sheet,

    cols = seq_len(
      ncol(dat_sheet)
    ),

    widths = "auto"
  )
}

openxlsx::saveWorkbook(

  wb,

  file = excel_file,

  overwrite = TRUE
)


# ================================================================
# 16. FINAL VERIFICATION
# ================================================================

output_files <- c(

  files_s03,

  files_s04,

  excel_file
)

stopifnot(

  length(output_files) == 5,

  all(
    file.exists(output_files)
  ),

  all(
    !is.na(
      file.info(output_files)$size
    )
  ),

  all(
    file.info(output_files)$size > 0
  ),

  nrow(recovery_data) == 90,

  nrow(model_audit) == 40,

  nrow(combined_model_audit) == 10,

  all(
    model_audit$status == "Converged"
  )
)

cat("\n========================================\n")
cat("SUPPLEMENTARY FIGURES EXPORTED\n")
cat("========================================\n")

cat("\nSAMPLE AUDIT:\n")

print(
  sample_audit,
  row.names = FALSE
)

cat("\nFIGURE AUDIT:\n")

print(
  figure_audit,
  row.names = FALSE
)

cat("\nCOMBINED MODEL DIAGNOSTICS:\n")

print(
  combined_model_audit,
  row.names = FALSE
)

cat("\nOUTPUT FILES:\n")

cat(
  paste(
    output_files,
    collapse = "\n"
  ),
  "\n"
)

cat("\nFIGURE S3(a): ALL DESIGNS (n = 85).\n")
cat("FIGURE S3(b): CONVENTIONAL SMRs (n = 51).\n")
cat("ALL FIVE READINESS DIMENSIONS INCLUDED.\n")
cat("ALL 90 ADJUSTED COEFFICIENTS VERIFIED.\n")
cat("NUMERICAL CONSISTENCY WITH TABLE S14 VERIFIED.\n")
cat("NO REGRESSION REFITTING UNDERTAKEN.\n")
cat("PANELS EXPORTED SEPARATELY IN PDF AND PNG.\n")
cat("FIGURE DIMENSIONS: 180 x 145 mm.\n")
cat("PNG RESOLUTION: 600 dpi.\n")
cat("========================================\n")
