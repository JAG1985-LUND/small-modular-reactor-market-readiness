# Revised manuscript materials

## Is the world ready for small modular reactors? Longitudinal insights into market readiness

This directory contains the revised manuscript materials, source data, figure outputs and R scripts supporting the above study, submitted to *Energy Strategy Reviews*.

The manuscript was originally submitted under the title *“From Pipeline Growth to First-of-a-Kind Deployment: Market Readiness Across the Global Small Modular Reactor Landscape”*. The title was subsequently revised to reflect the expanded longitudinal analysis and broader assessment of SMR market readiness.

## Repository structure

The revised materials are organised into three directories:

### `data/` – Source data and supplementary tables

Contains the Excel source-data workbooks supporting main Figures 1–10, supplementary analyses and robustness checks. It also includes the consolidated workbook of Supplementary Tables S1–S18, corresponding to the tables presented in the supplementary materials, and additional regression outputs and diagnostic worksheets retained for transparency and provenance.

### `figures/` – Figure outputs

Contains the exported figures associated with the revised manuscript, including main Figures 1–10 and Supplementary Figures S1–S3. Figures are provided in publication-ready formats, including PDF and PNG where applicable.

### `scripts/` – R analysis scripts

Contains the R scripts used to reproduce the main figures, supplementary figures and associated statistical analyses. These include longitudinal pipeline analyses, market-readiness assessments, ordinal regression models, robustness checks and supplementary-table calculations.

Some supplementary results are generated directly by the main-figure scripts and therefore do not require separate scripts.

## Reproducibility

The R scripts are configured to use source-data workbooks from `revision/data/` and generate outputs in `revision/figures/`, using repository-relative paths rather than the original author's local directory structure.

Additional information on the source files, outputs and scripts is provided in the README files within the respective directories.
