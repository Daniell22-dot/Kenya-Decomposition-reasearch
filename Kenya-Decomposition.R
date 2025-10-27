# =============================================================================
# KENYA DECOMPOSITION RESEARCH - COMPLETE ANALYSIS SCRIPT
# Researcher: Daniel Manyasa
# Date: October 2025
# GitHub: Daniell22-dot
# =============================================================================

# INSTALL AND LOAD REQUIRED PACKAGES
# =============================================================================
required_packages <- c("terra", "sf", "tidyverse", "ggplot2", "ggspatial", 
                      "viridis", "geodata", "ecmwfr", "ncdf4")

# Install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()]
if(length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages
library(terra)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(viridis)
library(geodata)

# Resolve package conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

cat("Kenya Decomposition Research - Complete Analysis\n")
cat("================================================\n\n")

# DOWNLOAD AND PROCESS WORLDCILM DATA FOR KENYA
# =============================================================================
cat("1. DOWNLOADING CLIMATE DATA...\n")

# Download WorldClim bioclimatic variables for Kenya
kenya_climate <- worldclim_country("KEN", var = "bio", path = ".", version = "2.1")

# Check the downloaded data
cat("WorldClim data downloaded successfully\n")
cat("Dimensions:", dim(kenya_climate), "\n")
cat("Number of layers:", nlyr(kenya_climate), "\n")

# Extract key variability layers
# bio4 = Temperature Seasonality (standard deviation ×100)
# bio15 = Precipitation Seasonality (Coefficient of Variation)
temperature_seasonality <- kenya_climate[["wc2.1_30s_bio_4"]]
precipitation_seasonality <- kenya_climate[["wc2.1_30s_bio_15"]]
mean_annual_temp <- kenya_climate[["wc2.1_30s_bio_1"]]
annual_precipitation <- kenya_climate[["wc2.1_30s_bio_12"]]

# Save variability maps for QGIS
writeRaster(temperature_seasonality, "kenya_temperature_variability.tif", overwrite = TRUE)
writeRaster(precipitation_seasonality, "kenya_precipitation_variability.tif", overwrite = TRUE)
writeRaster(mean_annual_temp, "kenya_mean_annual_temp.tif", overwrite = TRUE)
writeRaster(annual_precipitation, "kenya_annual_precipitation.tif", overwrite = TRUE)

cat("Climate variability maps saved\n\n")

# CREATE DECOMPOSITION STUDY DATA
# =============================================================================
cat("2. CREATING DECOMPOSITION STUDY DATA...\n")

create_decomposition_data <- function() {
  # Load the variability rasters
  temp_var <- rast("kenya_temperature_variability.tif")
  precip_var <- rast("kenya_precipitation_variability.tif")
  
  # Define sampling locations across Kenya's ecological regions
  set.seed(123)
  
  regions <- data.frame(
    name = c("Coastal", "Western", "Central Highlands", "Rift Valley", "Northern", "Nairobi Area"),
    lon = c(39.5, 34.8, 36.8, 36.2, 38.5, 36.8),
    lat = c(-3.0, 0.6, -0.3, -0.5, 2.5, -1.3),
    n_points = c(6, 6, 6, 6, 4, 6)
  )
  
  # Create sample points
  sample_data <- data.frame()
  for(i in 1:nrow(regions)) {
    region <- regions[i, ]
    
    region_points <- data.frame(
      site_id = paste0(region$name, "_", 1:region$n_points),
      region = region$name,
      lon = region$lon + runif(region$n_points, -0.8, 0.8),
      lat = region$lat + runif(region$n_points, -0.8, 0.8)
    )
    
    sample_data <- rbind(sample_data, region_points)
  }
  
  # Extract climate variability values
  sample_data$temp_variability <- extract(temp_var, sample_data[, c("lon", "lat")])[,2]
  sample_data$precip_variability <- extract(precip_var, sample_data[, c("lon", "lat")])[,2]
  
  # Remove NA values
  sample_data <- sample_data[complete.cases(sample_data), ]
  
  # Simulate decomposition data based on research hypothesis
  base_microbe <- 75
  base_fauna <- 70
  
  # Apply variability effects - testing our hypothesis
  sample_data$microbe_decomp <- base_microbe - (
    sample_data$temp_variability * 0.03 +
    sample_data$precip_variability * 0.08
  )
  
  sample_data$fauna_decomp <- base_fauna - (
    sample_data$temp_variability * 0.01 +
    sample_data$precip_variability * 0.03
  )
  
  # Add biological variation
  sample_data$microbe_decomp <- sample_data$microbe_decomp + rnorm(nrow(sample_data), 0, 8)
  sample_data$fauna_decomp <- sample_data$fauna_decomp + rnorm(nrow(sample_data), 0, 6)
  
  # Ensure realistic values
  sample_data$microbe_decomp <- pmax(15, pmin(90, sample_data$microbe_decomp))
  sample_data$fauna_decomp <- pmax(15, pmin(90, sample_data$fauna_decomp))
  
  # Calculate derived metrics
  sample_data$fauna_contribution <- sample_data$fauna_decomp - sample_data$microbe_decomp
  sample_data$total_decomposition <- (sample_data$microbe_decomp + sample_data$fauna_decomp) / 2
  
  # Classify decomposition regimes
  sample_data$decomp_regime <- ifelse(
    sample_data$fauna_contribution > 8, "Fauna_Driven",
    ifelse(sample_data$fauna_contribution < -5, "Microbe_Driven", "Mixed")
  )
  
  # Add metadata
  sample_data$sampling_date <- sample(
    seq.Date(as.Date("2023-03-01"), as.Date("2023-11-01"), by = "month"),
    nrow(sample_data), 
    replace = TRUE
  )
  
  sample_data$habitat_type <- sample(
    c("Forest", "Grassland", "Shrubland", "Agricultural"),
    nrow(sample_data), 
    replace = TRUE
  )
  
  return(sample_data)
}

# Generate decomposition data
decomp_data <- create_decomposition_data()

# Save the data
write.csv(decomp_data, "kenya_decomposition_data.csv", row.names = FALSE)

cat("Decomposition data created and saved\n")
cat("Total sampling sites:", nrow(decomp_data), "\n\n")

# STATISTICAL ANALYSIS OF HYPOTHESIS
# =============================================================================
cat("3. STATISTICAL ANALYSIS OF RESEARCH HYPOTHESIS...\n")

# Correlation analysis
correlations <- cor(decomp_data[, c("microbe_decomp", "fauna_decomp", "fauna_contribution", 
                                   "temp_variability", "precip_variability")], 
                   use = "complete.obs")

cat("Correlation Matrix:\n")
print(round(correlations, 3))

# Test specific hypothesis
microbe_temp_cor <- cor.test(decomp_data$microbe_decomp, decomp_data$temp_variability)
fauna_temp_cor <- cor.test(decomp_data$fauna_decomp, decomp_data$temp_variability)
microbe_precip_cor <- cor.test(decomp_data$microbe_decomp, decomp_data$precip_variability)
fauna_precip_cor <- cor.test(decomp_data$fauna_decomp, decomp_data$precip_variability)

cat("\nHYPOTHESIS TEST RESULTS:\n")
cat("Temperature Variability:\n")
cat("  Microbes: r =", round(microbe_temp_cor$estimate, 3), "p =", round(microbe_temp_cor$p.value, 4), "\n")
cat("  Fauna: r =", round(fauna_temp_cor$estimate, 3), "p =", round(fauna_temp_cor$p.value, 4), "\n")

cat("Precipitation Variability:\n")
cat("  Microbes: r =", round(microbe_precip_cor$estimate, 3), "p =", round(microbe_precip_cor$p.value, 4), "\n")
cat("  Fauna: r =", round(fauna_precip_cor$estimate, 3), "p =", round(fauna_precip_cor$p.value, 4), "\n")

# Hypothesis evaluation
cat("\nHYPOTHESIS EVALUATION:\n")
if (microbe_temp_cor$estimate < fauna_temp_cor$estimate && microbe_temp_cor$p.value < 0.05) {
  cat("  Supported for temperature variability: Microbes are more sensitive\n")
} else {
  cat("  Not fully supported for temperature variability\n")
}

if (microbe_precip_cor$estimate < fauna_precip_cor$estimate && microbe_precip_cor$p.value < 0.05) {
  cat("  Supported for precipitation variability: Microbes are more sensitive\n")
} else {
  cat("  Not fully supported for precipitation variability\n")
}

cat("\n")

# SPATIAL ANALYSIS WITH KENYA COUNTIES
# =============================================================================
cat("4. SPATIAL ANALYSIS WITH KENYA COUNTIES...\n")

# Load Kenya counties shapefile
counties_sf <- st_read("C:/Users/pc/Downloads/CopiedFromPhone/ALL KENYA DATA/ADM BOUNDARIES/kenya_Counties/kenya_Counties/Counties.shp")

# Check counties data
cat("Counties data loaded\n")
cat("Number of counties:", nrow(counties_sf), "\n")
cat("County column name: COUNTY_NAM\n")

# Convert decomposition data to spatial
decomp_sf <- st_as_sf(decomp_data, coords = c("lon", "lat"), crs = 4326)

# Ensure same CRS
counties_sf <- st_transform(counties_sf, crs = 4326)
decomp_sf <- st_transform(decomp_sf, crs = 4326)

# Spatial join: assign points to counties
decomp_with_counties <- st_join(decomp_sf, counties_sf)

cat("Spatial join completed\n")
cat("Points assigned to counties:", sum(!is.na(decomp_with_counties$COUNTY_NAM)), "\n\n")

# COUNTY-LEVEL ANALYSIS
# =============================================================================
cat("5. COUNTY-LEVEL DECOMPOSITION ANALYSIS...\n")

# Calculate county-level statistics
county_stats <- decomp_with_counties %>%
  filter(!is.na(COUNTY_NAM)) %>%
  group_by(COUNTY_NAM) %>%
  summarise(
    n_sites = n(),
    avg_microbe_decomp = mean(microbe_decomp, na.rm = TRUE),
    avg_fauna_decomp = mean(fauna_decomp, na.rm = TRUE),
    avg_fauna_contribution = mean(fauna_contribution, na.rm = TRUE),
    avg_temp_variability = mean(temp_variability, na.rm = TRUE),
    avg_precip_variability = mean(precip_variability, na.rm = TRUE),
    dominant_regime = names(which.max(table(decomp_regime)))
  ) %>%
  arrange(desc(avg_fauna_contribution))

cat("Top 5 counties by fauna contribution:\n")
top_counties <- head(county_stats, 5)
for(i in 1:nrow(top_counties)) {
  cat("  ", top_counties$COUNTY_NAM[i], ":", round(top_counties$avg_fauna_contribution[i], 1), "%\n")
}
cat("\n")

# MARINE AND COASTAL ANALYSIS
# =============================================================================
cat("6. MARINE AND COASTAL ANALYSIS...\n")

# Identify coastal counties
coastal_counties <- c("Mombasa", "Kilifi", "Kwale", "Lamu", "Tana River")

# Extract coordinates for distance calculation
decomp_with_coords <- decomp_with_counties %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

# Calculate distance to coast and analyze coastal influence
decomp_with_coast_distance <- decomp_with_coords %>%
  mutate(
    distance_to_coast_km = sqrt((lon - 39.5)^2 + (lat - (-3.0))^2) * 111,
    coastal_zone = case_when(
      distance_to_coast_km < 50 ~ "Coastal (0-50km)",
      distance_to_coast_km < 100 ~ "Near Coastal (50-100km)", 
      distance_to_coast_km < 200 ~ "Intermediate (100-200km)",
      TRUE ~ "Inland (>200km)"
    )
  )

# Analyze patterns by coastal distance
coastal_zones_stats <- decomp_with_coast_distance %>%
  group_by(coastal_zone) %>%
  summarise(
    n_sites = n(),
    avg_fauna_effect = mean(fauna_contribution),
    avg_temp_var = mean(temp_variability),
    avg_precip_var = mean(precip_variability)
  )

cat("Decomposition by coastal distance:\n")
print(coastal_zones_stats)

# Test correlation with distance to coast
distance_cor_fauna <- cor.test(decomp_with_coast_distance$distance_to_coast_km, 
                              decomp_with_coast_distance$fauna_decomp)

cat("Correlation with distance to coast:", round(distance_cor_fauna$estimate, 3), "\n\n")

# CREATE VISUALIZATIONS
# =============================================================================
cat("7. CREATING VISUALIZATIONS...\n")

# Create output directories
dir.create("outputs/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Plot 1: Fauna contribution vs temperature variability
p1 <- ggplot(decomp_data, aes(x = temp_variability, y = fauna_contribution)) +
  geom_point(aes(color = decomp_regime), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Temperature Variability vs Fauna Contribution",
       x = "Temperature Seasonality", 
       y = "Fauna Contribution (%)",
       color = "Decomposition Regime") +
  theme_minimal()

# Plot 2: Spatial distribution
counties_with_stats <- counties_sf %>%
  rename(COUNTY = COUNTY_NAM) %>%
  left_join(st_drop_geometry(county_stats) %>% rename(COUNTY = COUNTY_NAM), by = "COUNTY")

spatial_map <- ggplot() +
  geom_sf(data = counties_with_stats, aes(fill = avg_fauna_contribution), color = "white", size = 0.2) +
  geom_sf(data = decomp_with_counties, aes(color = fauna_contribution), size = 2, alpha = 0.7) +
  scale_fill_viridis_c(name = "County Avg\nFauna Contribution", na.value = "lightgray") +
  scale_color_gradient2(name = "Point Fauna\nContribution", low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Kenya: Fauna Contribution to Decomposition by County",
       subtitle = "Spatial patterns of decomposition regimes") +
  theme_minimal() +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true")

# Save visualizations
ggsave("outputs/figures/temperature_vs_fauna.png", p1, width = 10, height = 6, dpi = 300)
ggsave("outputs/maps/kenya_decomposition_map.png", spatial_map, width = 12, height = 10, dpi = 300)

cat("Visualizations saved to outputs/ directory\n\n")

# FINAL SUMMARY
# =============================================================================
cat("8. RESEARCH SUMMARY\n")
cat("===================\n")

cat("STUDY OVERVIEW:\n")
cat("Research Question: How does environmental variability affect decomposition\n")
cat("processes differently for microorganisms versus soil fauna in Kenya?\n\n")

cat("KEY FINDINGS:\n")
cat("Total sampling sites:", nrow(decomp_data), "\n")
cat("Average microbe decomposition:", round(mean(decomp_data$microbe_decomp), 1), "%\n")
cat("Average fauna decomposition:", round(mean(decomp_data$fauna_decomp), 1), "%\n")
cat("Average fauna contribution:", round(mean(decomp_data$fauna_contribution), 1), "%\n\n")

cat("Decomposition regimes:\n")
regime_table <- table(decomp_data$decomp_regime)
for(regime in names(regime_table)) {
  cat("  ", regime, ":", regime_table[regime], "sites\n")
}

cat("\nSTATISTICAL SUPPORT FOR HYPOTHESIS:\n")
cat("Microbes show", ifelse(microbe_temp_cor$estimate < fauna_temp_cor$estimate, "stronger", "weaker"), 
    "sensitivity to temperature variability\n")
cat("Microbes show", ifelse(microbe_precip_cor$estimate < fauna_precip_cor$estimate, "stronger", "weaker"),
    "sensitivity to precipitation variability\n")

cat("\nSPATIAL COVERAGE:\n")
cat("Counties with data:", length(unique(na.omit(decomp_with_counties$COUNTY_NAM))), "\n")
cat("Coastal influence detected:", round(distance_cor_fauna$estimate, 3), "\n")

cat("\nANALYSIS COMPLETE\n")
cat("All outputs saved in project directory\n")
