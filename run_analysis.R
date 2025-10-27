#!/usr/bin/env Rscript
# Kenya Decomposition Research - Main Runner
# Run this script to reproduce the analysis

cat("Kenya Decomposition Research\n")
cat("============================\n")
cat("Researcher: Daniel Manyasa\n")
cat("Date: October 2025\n\n")

# Check for required packages
required_packages <- c("sf", "terra", "tidyverse", "ggplot2")
missing_packages <- required_packages[!required_packages %in% installed.packages()]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

cat("All packages available!\n")
cat("To run specific analyses:\n")
cat("1. source('scripts/01_main_analysis.R')\n")
cat("2. source('scripts/02_spatial_analysis.R')\n")
cat("3. Check outputs/ directory for results\n")
