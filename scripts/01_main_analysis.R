# Kenya Decomposition Research - Main Analysis
# Researcher: Daniel Manyasa
# Date: October 2025

# Load required packages
library(sf)
library(terra)
library(tidyverse)
library(ggplot2)

# Set up project
cat("Kenya Decomposition Research Analysis\n")
cat("=====================================\n")

# Load data
decomp_data <- read.csv("data/processed/kenya_decomposition_data.csv")
cat("Decomposition data loaded:", nrow(decomp_data), "observations\n")

# Load climate rasters
temp_var <- rast("data/raw/kenya_temperature_variability.tif")
precip_var <- rast("data/raw/kenya_precipitation_variability.tif")
cat("Climate rasters loaded\n")

# Basic analysis
cat("\nBasic Statistics:\n")
cat("Microbe decomposition:", round(mean(decomp_data$microbe_decomp), 1), "%\n")
cat("Fauna decomposition:", round(mean(decomp_data$fauna_decomp), 1), "%\n")
cat("Fauna contribution:", round(mean(decomp_data$fauna_contribution), 1), "%\n")

# Correlation analysis
cor_temp_microbe <- cor(decomp_data$microbe_decomp, decomp_data$temp_variability)
cor_temp_fauna <- cor(decomp_data$fauna_decomp, decomp_data$temp_variability)

cat("\nCorrelation with Temperature Variability:\n")
cat("Microbes:", round(cor_temp_microbe, 3), "\n")
cat("Fauna:", round(cor_temp_fauna, 3), "\n")

cat("\nAnalysis complete! Check outputs/ for results.\n")
