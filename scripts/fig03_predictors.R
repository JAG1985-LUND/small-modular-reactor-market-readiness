# Code for the manuscript Gordon, J.A.
# Is the world ready for small modular reactors? A longitudinal analysis of market readiness
#
# Reproduces Figure 3: Temporal, geopolitical and technological predictors
# of SMR market readiness dimensions.
#
# Panels show odds ratios and 95% confidence intervals from univariate ordinal
# logistic regression models relating higher market-readiness scores to
# conceptual design start year, OECD membership, cooling type, neutron spectrum,
# fuel type and fuel enrichment.
#
# Dashed vertical lines indicate the null value (odds ratio = 1).
#
# Working directory:
# Set the working directory to the repository root or scripts folder before running this script.
#
# Required input:
# data/fig03_predictors_source_data.xlsx
#
# Outputs:
# outputs/fig03_a-f.png
# outputs/fig03_a-f.pdf
# outputs/fig03_predictors_model_results.csv

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(MASS)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

make_ordered_outcome <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  
  if (sum(!is.na(x_num)) > 0 && all(is.na(x_chr) | !is.na(x_num))) {
    levs <- sort(unique(x_num[!is.na(x_num)]))
    ordered(x_num, levels = levs)
  } else {
    levs <- sort(unique(x_chr[!is.na(x_chr)]))
    ordered(x_chr, levels = levs)
  }
}

format_axis_labels <- function(x) {
  out <- sprintf("%.2f", x)
  out <- sub("0+$", "", out)
  out <- sub("\\.$", "", out)
  out
}

extract_uni_results <- function(model, null_model, predictor_name, outcome_name, outcome_label, n_used) {
  coef_mat <- coef(summary(model))
  coef_mat <- coef_mat[!grepl("\\|", rownames(coef_mat)), , drop = FALSE]
  
  if (nrow(coef_mat) == 0) return(NULL)
  
  coef_mat <- coef_mat[1, , drop = FALSE]
  p_vals <- 2 * pnorm(abs(coef_mat[, "t value"]), lower.tail = FALSE)
  
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  lr_stat <- 2 * (ll_full - ll_null)
  lr_df <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
  lr_p <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  data.frame(
    predictor = predictor_name,
    outcome = outcome_name,
    outcome_label = outcome_label,
    term = rownames(coef_mat),
    estimate = unname(coef_mat[, "Value"]),
    std_error = unname(coef_mat[, "Std. Error"]),
    t_value = unname(coef_mat[, "t value"]),
    p_value = unname(p_vals),
    odds_ratio = exp(unname(coef_mat[, "Value"])),
    ci_low = exp(unname(coef_mat[, "Value"]) - 1.96 * unname(coef_mat[, "Std. Error"])),
    ci_high = exp(unname(coef_mat[, "Value"]) + 1.96 * unname(coef_mat[, "Std. Error"])),
    lr_stat = lr_stat,
    lr_df = lr_df,
    lr_p_value = lr_p,
    mcfadden_r2 = mcfadden_r2,
    n = n_used,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

run_univariate_panel_models <- function(df, predictor_name, x, is_factor = FALSE, factor_levels = NULL) {
  outcomes <- c("financing", "supply_chain", "fuel", "licensing", "siting")
  
  outcome_labels <- c(
    financing = "Financing",
    supply_chain = "Supply Chain",
    fuel = "Fuel Supply",
    licensing = "Licensing",
    siting = "Siting"
  )
  
  results_list <- list()
  
  for (outcome_var in outcomes) {
    dat <- data.frame(
      outcome = make_ordered_outcome(df[[outcome_var]]),
      x = x,
      stringsAsFactors = FALSE
    )
    
    if (is_factor) {
      dat$x <- factor(dat$x, levels = factor_levels)
    } else {
      dat$x <- suppressWarnings(as.numeric(dat$x))
    }
    
    dat <- dat[complete.cases(dat), , drop = FALSE]
    
    if (nrow(dat) == 0) next
    if (length(unique(dat$outcome)) < 2) next
    if (length(unique(dat$x)) < 2) next
    
    fit <- tryCatch(
      suppressWarnings(MASS::polr(outcome ~ x, data = dat, method = "logistic", Hess = TRUE)),
      error = function(e) NULL
    )
    
    fit_null <- tryCatch(
      suppressWarnings(MASS::polr(outcome ~ 1, data = dat, method = "logistic", Hess = TRUE)),
      error = function(e) NULL
    )
    
    if (is.null(fit) || is.null(fit_null)) next
    
    results_list[[outcome_var]] <- extract_uni_results(
      model = fit,
      null_model = fit_null,
      predictor_name = predictor_name,
      outcome_name = outcome_var,
      outcome_label = outcome_labels[[outcome_var]],
      n_used = nrow(dat)
    )
  }
  
  results_list <- Filter(Negate(is.null), results_list)
  
  if (length(results_list) == 0) {
    stop(paste0("No valid models were estimated for ", predictor_name, "."))
  }
  
  do.call(rbind, results_list)
}

make_or_panel <- function(results_df, panel_label, panel_title, candidate_breaks, dimension_colors) {
  plot_order <- c("Financing", "Supply Chain", "Fuel Supply", "Licensing", "Siting")
  
  results_df$outcome_label <- factor(results_df$outcome_label, levels = plot_order)
  results_df <- results_df[order(results_df$outcome_label), , drop = FALSE]
  results_df$y_pos <- seq_len(nrow(results_df))
  
  cap_half_height <- 0.10
  
  axis_vals <- c(results_df$ci_low, results_df$ci_high, results_df$odds_ratio, 1)
  axis_vals <- axis_vals[is.finite(axis_vals) & axis_vals > 0]
  
  x_min <- min(axis_vals, na.rm = TRUE)
  x_max <- max(axis_vals, na.rm = TRUE)
  
  pad_log <- 0.10
  x_limits <- exp(c(log(x_min) - pad_log, log(x_max) + pad_log))
  
  x_breaks <- candidate_breaks[candidate_breaks >= x_limits[1] & candidate_breaks <= x_limits[2]]
  
  if (!1 %in% x_breaks) {
    x_breaks <- sort(unique(c(x_breaks, 1)))
  }
  
  panel_core <- ggplot(results_df, aes(x = odds_ratio, y = y_pos, colour = outcome_label)) +
    geom_segment(
      aes(x = ci_low, xend = ci_high, y = y_pos, yend = y_pos),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(x = ci_low, xend = ci_low, y = y_pos - cap_half_height, yend = y_pos + cap_half_height),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_segment(
      aes(x = ci_high, xend = ci_high, y = y_pos - cap_half_height, yend = y_pos + cap_half_height),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_point(
      size = 2.2,
      shape = 16,
      stroke = 0,
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey35",
      linewidth = 0.5
    ) +
    scale_color_manual(values = dimension_colors, guide = "none") +
    scale_x_log10(
      breaks = x_breaks,
      labels = format_axis_labels(x_breaks),
      minor_breaks = NULL,
      limits = x_limits,
      name = "Odds Ratio (log scale)"
    ) +
    scale_y_continuous(
      breaks = results_df$y_pos,
      labels = as.character(results_df$outcome_label),
      trans = "reverse",
      expand = expansion(mult = c(0.06, 0.06))
    ) +
    labs(
      title = panel_title,
      y = NULL
    ) +
    theme_minimal(base_size = 7) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7, colour = "black"),
      axis.text.y = element_text(size = 7, colour = "black"),
      axis.title.x = element_text(size = 7, colour = "black"),
      plot.title = element_text(size = 8, face = "bold", hjust = 0.5, colour = "black"),
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 16)
    )
  
  ggdraw(panel_core) +
    draw_plot_label(
      label = panel_label,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}

wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_dir <- if (basename(wd) == "scripts") dirname(wd) else wd

dir.create(
  file.path(project_dir, "outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)

data_file <- file.path(
  project_dir,
  "data",
  "fig03_predictors_source_data.xlsx"
)

sheet_name <- if ("fig03" %in% excel_sheets(data_file)) "fig03" else excel_sheets(data_file)[1]

png_out <- file.path(
  project_dir,
  "outputs",
  "fig03_a-f.png"
)

pdf_out <- file.path(
  project_dir,
  "outputs",
  "fig03_a-f.pdf"
)

results_out <- file.path(
  project_dir,
  "outputs",
  "fig03_predictors_model_results.csv"
)

df <- read_excel(data_file, sheet = sheet_name)
names(df) <- clean_names_simple(names(df))
df <- df[!is.na(df$design), , drop = FALSE]

required_cols <- c(
  "cd_start",
  "water_cooled",
  "spectrum",
  "fuel_type_recoded",
  "fuel_enrichment_recode",
  "financing",
  "supply_chain",
  "fuel",
  "licensing",
  "siting"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

oecd_col <- intersect(c("oecd", "oecd_membership"), names(df))[1]
if (is.na(oecd_col)) stop("OECD column not found.")

df <- df %>%
  mutate(
    cd_start_num = suppressWarnings(as.numeric(as.character(cd_start))),
    
    oecd_group = case_when(
      tolower(trimws(as.character(.data[[oecd_col]]))) %in% c("oecd", "yes", "y", "1", "true") ~ "OECD",
      tolower(trimws(as.character(.data[[oecd_col]]))) %in% c("non-oecd", "non_oecd", "non oecd", "no", "n", "0", "false") ~ "Non-OECD",
      TRUE ~ NA_character_
    ),
    
    water_group = case_when(
      tolower(trimws(as.character(water_cooled))) %in% c("water_cooled", "water-cooled", "water cooled", "water", "yes", "y", "1", "true") ~ "Water-cooled",
      tolower(trimws(as.character(water_cooled))) %in% c("non_water_cooled", "non-water-cooled", "non water cooled", "non-water", "non water", "no", "n", "0", "false") ~ "Non-water-cooled",
      TRUE ~ NA_character_
    ),
    
    spectrum_group = case_when(
      tolower(trimws(as.character(spectrum))) == "thermal" ~ "Thermal",
      tolower(trimws(as.character(spectrum))) == "fast" ~ "Fast",
      TRUE ~ NA_character_
    ),
    
    fuel_group_binary = case_when(
      tolower(trimws(as.character(fuel_type_recoded))) %in% c("uranium", "u", "uranium fuel", "uranium-based") ~ "Uranium",
      tolower(trimws(as.character(fuel_type_recoded))) %in% c("triso", "other", "other fuels", "metallic", "molten salt", "mox") ~ "Other fuels",
      TRUE ~ NA_character_
    ),
    
    enrichment_group_binary = case_when(
      tolower(trimws(as.character(fuel_enrichment_recode))) %in% c("leu", "low enriched uranium", "low-enriched uranium") ~ "LEU",
      tolower(trimws(as.character(fuel_enrichment_recode))) %in% c("haleu", "ha-leu", "ha leu", "high-assay low-enriched uranium", "helu", "other", "other enrichments") ~ "Other enrichments",
      TRUE ~ NA_character_
    )
  )

dimension_colors <- c(
  "Financing" = "#2A9D8F",
  "Supply Chain" = "#4C78A8",
  "Fuel Supply" = "#8E63CE",
  "Licensing" = "#8C2D3E",
  "Siting" = "#C7922F"
)

panel_configs <- list(
  list(
    id = "a",
    title = "Conceptual Design Start Year",
    predictor = "Conceptual Design Start Year",
    x = df$cd_start_num,
    is_factor = FALSE,
    factor_levels = NULL,
    breaks = c(0.75, 0.80, 0.90, 1.00, 1.10)
  ),
  list(
    id = "b",
    title = "OECD Membership",
    predictor = "OECD Membership",
    x = df$oecd_group,
    is_factor = TRUE,
    factor_levels = c("Non-OECD", "OECD"),
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)
  ),
  list(
    id = "c",
    title = "Cooling Type",
    predictor = "Cooling Type",
    x = df$water_group,
    is_factor = TRUE,
    factor_levels = c("Non-water-cooled", "Water-cooled"),
    breaks = c(1, 2, 5, 10, 20, 50)
  ),
  list(
    id = "d",
    title = "Neutron spectrum",
    predictor = "Neutron spectrum",
    x = df$spectrum_group,
    is_factor = TRUE,
    factor_levels = c("Fast", "Thermal"),
    breaks = c(0.5, 1, 2, 5, 10, 20)
  ),
  list(
    id = "e",
    title = "Fuel Type",
    predictor = "Fuel Type",
    x = df$fuel_group_binary,
    is_factor = TRUE,
    factor_levels = c("Other fuels", "Uranium"),
    breaks = c(0.5, 1, 2, 5, 10, 20)
  ),
  list(
    id = "f",
    title = "Fuel Enrichment",
    predictor = "Fuel Enrichment",
    x = df$enrichment_group_binary,
    is_factor = TRUE,
    factor_levels = c("Other enrichments", "LEU"),
    breaks = c(0.5, 1, 2, 5, 10, 20)
  )
)

panels <- list()
all_results <- list()

for (cfg in panel_configs) {
  res <- run_univariate_panel_models(
    df = df,
    predictor_name = cfg$predictor,
    x = cfg$x,
    is_factor = cfg$is_factor,
    factor_levels = cfg$factor_levels
  )
  
  panel_obj <- make_or_panel(
    results_df = res,
    panel_label = cfg$id,
    panel_title = cfg$title,
    candidate_breaks = cfg$breaks,
    dimension_colors = dimension_colors
  )
  
  panels[[cfg$id]] <- panel_obj
  all_results[[cfg$id]] <- res
}

all_results_df <- bind_rows(all_results, .id = "panel")

write.csv(
  all_results_df,
  results_out,
  row.names = FALSE
)

top_row <- plot_grid(
  panels$a, panels$b, panels$c,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1, 1)
)

bottom_row <- plot_grid(
  panels$d, panels$e, panels$f,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1, 1)
)

combined_plot <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1)
)

combined_plot <- ggdraw(combined_plot) +
  theme(
    plot.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(combined_plot)

ggsave(
  filename = png_out,
  plot = combined_plot,
  width = 240,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_out,
  plot = combined_plot,
  width = 240,
  height = 150,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)
