# Spatial Analysis of Decomposition Patterns
# Kenya Counties Analysis

library(sf)
library(ggplot2)
library(viridis)

cat("Spatial Analysis - Kenya Counties\n")
cat("=================================\n")

# Load decomposition data
decomp_data <- read.csv("data/processed/kenya_decomposition_data.csv")

# Convert to spatial
decomp_sf <- st_as_sf(decomp_data, coords = c("lon", "lat"), crs = 4326)

cat("Spatial data created:", nrow(decomp_sf), "points\n")

# Create basic spatial plot
spatial_plot <- ggplot() +
  geom_sf(data = decomp_sf, aes(color = fauna_contribution), size = 2) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "Fauna Contribution") +
  labs(title = "Kenya Decomposition Patterns",
       subtitle = "Fauna contribution across sampling sites") +
  theme_minimal()

# Save plot
ggsave("outputs/figures/spatial_distribution.png", spatial_plot, 
       width = 10, height = 8, dpi = 300)

cat("Spatial plot saved: outputs/figures/spatial_distribution.png\n")
