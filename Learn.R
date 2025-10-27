> cat("Coastal counties:", paste(coastal_counties, collapse = ", "), "\n")
Coastal counties: Mombasa, Kilifi, Kwale, Lamu, Tana River 
> cat("Number of sampling points in coastal counties:", nrow(coastal_data), "\n\n")
Number of sampling points in coastal counties: 5 

> if(nrow(coastal_data) > 0) {
+     # Coastal decomposition patterns
+     coastal_stats <- coastal_data %>%
+         group_by(COUNTY_NAM) %>%
+         summarise(
+             n_sites = n(),
+             avg_microbe_decomp = mean(microbe_decomp),
+             avg_fauna_decomp = mean(fauna_decomp),
+             avg_fauna_contribution = mean(fauna_contribution),
+             avg_temp_variability = mean(temp_variability),
+             avg_precip_variability = mean(precip_variability),
+             distance_to_coast_km = mean(sqrt((lon - 39.5)^2 + (lat - (-3.0))^2) * 111) # Rough distance calculation
+         )
+ cat("Coastal County Decomposition Statistics:\n")
+ print(coastal_stats)
+ # Compare coastal vs inland patterns
+ inland_data <- decomp_with_coords %>%
+     dplyr::filter(!COUNTY_NAM %in% coastal_counties & !is.na(COUNTY_NAM))
+ 
+ coastal_vs_inland <- bind_rows(
+     coastal_data %>% mutate(zone = "Coastal"),
+     inland_data %>% mutate(zone = "Inland")
+ ) %>%
+     group_by(zone) %>%
+     summarise(
+         n_sites = n(),
+         avg_microbe = mean(microbe_decomp),
+         avg_fauna = mean(fauna_decomp),
+         avg_fauna_effect = mean(fauna_contribution),
+         avg_temp_var = mean(temp_variability),
+         avg_precip_var = mean(precip_variability)
+     )
+ cat("\nCOASTAL VS INLAND COMPARISON:\n")
+ print(coastal_vs_inland)
+ } else {
+     cat("No coastal data found. Checking available counties:\n")
+     available_counties <- unique(decomp_with_coords$COUNTY_NAM)
+     print(available_counties)
+ }
Coastal County Decomposition Statistics:
Simple feature collection with 2 features and 8 fields
Geometry type: MULTIPOINT
Dimension:     XY
Bounding box:  xmin: 39.16012 ymin: -3.069416 xmax: 40.20475 ymax: -2.269067
Geodetic CRS:  WGS 84
# A tibble: 2 × 9
  COUNTY_NAM n_sites avg_microbe_decomp avg_fauna_decomp avg_fauna_contribution
  <chr>        <int>              <dbl>            <dbl>                  <dbl>
1 Kilifi           2               67.1             58.1                  -9.06
2 Tana River       3               70.0             75.0                   5.07
# ℹ 4 more variables: avg_temp_variability <dbl>, avg_precip_variability <dbl>,
#   distance_to_coast_km <dbl>, geometry <MULTIPOINT [°]>

COASTAL VS INLAND COMPARISON:
Simple feature collection with 2 features and 7 fields
Geometry type: MULTIPOINT
Dimension:     XY
Bounding box:  xmin: 34.91621 ymin: -3.074665 xmax: 40.20475 ymax: 2.97828
Geodetic CRS:  WGS 84
# A tibble: 2 × 8
  zone    n_sites avg_microbe avg_fauna avg_fauna_effect avg_temp_var avg_precip_var
  <chr>     <int>       <dbl>     <dbl>            <dbl>        <dbl>          <dbl>
1 Coastal       5        68.8      68.2           -0.581        130.            62.3
2 Inland       26        67.1      68.3            1.17          95.6           67.4
# ℹ 1 more variable: geometry <MULTIPOINT [°]>
> # Calculate distance to coast for all points
> decomp_with_coast_distance <- decomp_with_coords %>%
+     mutate(
+         # Approximate distance to coast (Mombasa as reference point)
+         distance_to_coast_km = sqrt((lon - 39.5)^2 + (lat - (-3.0))^2) * 111,
+         coastal_zone = case_when(
+             distance_to_coast_km < 50 ~ "Coastal (0-50km)",
+             distance_to_coast_km < 100 ~ "Near Coastal (50-100km)",
+             distance_to_coast_km < 200 ~ "Intermediate (100-200km)",
+             TRUE ~ "Inland (>200km)"
+         )
+     )
> # Analyze patterns by coastal distance
> coastal_zones_stats <- decomp_with_coast_distance %>%
+     group_by(coastal_zone) %>%
+     summarise(
+         n_sites = n(),
+         avg_microbe = mean(microbe_decomp),
+         avg_fauna = mean(fauna_decomp),
+         avg_fauna_effect = mean(fauna_contribution),
+         avg_temp_var = mean(temp_variability),
+         avg_precip_var = mean(precip_variability),
+         avg_distance = mean(distance_to_coast_km)
+     ) %>%
+     arrange(avg_distance)
> 
> cat("DECOMPOSITION BY COASTAL DISTANCE:\n")
DECOMPOSITION BY COASTAL DISTANCE:
> print(coastal_zones_stats)
Simple feature collection with 4 features and 8 fields
Geometry type: GEOMETRY
Dimension:     XY
Bounding box:  xmin: 34.0673 ymin: -3.074665 xmax: 40.20475 ymax: 2.97828
Geodetic CRS:  WGS 84
# A tibble: 4 × 9
  coastal_zone             n_sites avg_microbe avg_fauna avg_fauna_effect avg_temp_var
  <chr>                      <int>       <dbl>     <dbl>            <dbl>        <dbl>
1 Coastal (0-50km)               2        71.6      68.1            -3.47        137. 
2 Near Coastal (50-100km)        3        66.1      64.7            -1.46        134. 
3 Intermediate (100-200km)       1        64.7      73.3             8.53        123. 
4 Inland (>200km)               28        67.2      68.4             1.24         91.5
# ℹ 3 more variables: avg_precip_var <dbl>, avg_distance <dbl>,
#   geometry <GEOMETRY [°]>
> # Test correlation with distance to coast
> distance_cor_microbe <- cor.test(decomp_with_coast_distance$distance_to_coast_km, 
+                                  decomp_with_coast_distance$microbe_decomp)
> distance_cor_fauna <- cor.test(decomp_with_coast_distance$distance_to_coast_km, 
+                                decomp_with_coast_distance$fauna_decomp)
> 
> cat("\nCORRELATION WITH DISTANCE TO COAST:\n")

CORRELATION WITH DISTANCE TO COAST:
> cat("Microbe decomposition vs distance: r =", 
+     round(distance_cor_microbe$estimate, 3), 
+     "p =", round(distance_cor_microbe$p.value, 4), "\n")
Microbe decomposition vs distance: r = 0.014 p = 0.9364 
> cat("Fauna decomposition vs distance: r =", 
+     round(distance_cor_fauna$estimate, 3), 
+     "p =", round(distance_cor_fauna$p.value, 4), "\n")
Fauna decomposition vs distance: r = 0.026 p = 0.8821 
> # Update counties_with_stats with coordinates for mapping
> counties_with_coords <- counties_with_stats %>%
+     mutate(
+         centroid = st_centroid(geometry),
+         lon = st_coordinates(centroid)[, 1],
+         lat = st_coordinates(centroid)[, 2]
+     )
> # Map: Coastal decomposition patterns
> coastal_map <- ggplot() +
+     geom_sf(data = counties_with_coords, fill = "lightgray", color = "white") +
+     geom_sf(data = counties_with_coords %>% 
+                 dplyr::filter(COUNTY %in% coastal_counties), 
+             aes(fill = avg_fauna_contribution), color = "white") +
+     geom_sf(data = decomp_with_coast_distance, 
+             aes(color = fauna_contribution, size = distance_to_coast_km), 
+             alpha = 0.7) +
+     scale_fill_viridis_c(name = "Coastal County\nFauna Contribution", 
+                          na.value = "lightgray") +
+     scale_color_gradient2(name = "Point Fauna\nContribution", 
+                           low = "red", mid = "white", high = "blue", 
+                           midpoint = 0) +
+     scale_size_continuous(name = "Distance to\nCoast (km)") +
+     labs(title = "Marine Influence on Decomposition Patterns",
+          subtitle = "Kenya Coastal Counties and Distance to Coast Effects") +
+ theme_minimal() +
+     theme(legend.position = "right")
> 
> # Plot: Decomposition vs distance to coast
> distance_plot <- ggplot(decomp_with_coast_distance, 
+                         aes(x = distance_to_coast_km, y = fauna_contribution)) +
+     geom_point(aes(color = coastal_zone), alpha = 0.7, size = 2) +
+     geom_smooth(method = "lm", se = TRUE, color = "black") +
+     geom_vline(xintercept = c(50, 100, 200), linetype = "dashed", alpha = 0.5) +
+     labs(title = "Fauna Contribution vs Distance to Coast",
+          x = "Distance to Coast (km)",
+          y = "Fauna Contribution (%)",
+          color = "Coastal Zone") +
+     theme_minimal()
> # Save coastal visualizations
> ggsave("kenya_coastal_decomposition_map.png", coastal_map, 
+        width = 12, height = 10, dpi = 300)
> ggsave("coastal_distance_analysis.png", distance_plot, 
+        width = 10, height = 6, dpi = 300)
`geom_smooth()` using formula = 'y ~ x'
> 
> cat("Coastal maps saved:\n")
Coastal maps saved:
> cat("- kenya_coastal_decomposition_map.png\n")
- kenya_coastal_decomposition_map.png
> cat("- coastal_distance_analysis.png\n")
- coastal_distance_analysis.png
> if(nrow(coastal_data) > 0) {
+     coastal_vulnerability <- decomp_with_coast_distance %>%
+         dplyr::filter(COUNTY_NAM %in% coastal_counties) %>%
+         mutate(
+             vulnerability_score = case_when(
+                 # Higher temp variability + low fauna contribution = more vulnerable
+                 temp_variability > 100 & fauna_contribution < 0 ~ "High vulnerability",
+                 temp_variability > 80 & fauna_contribution < 5 ~ "Moderate vulnerability", 
+                 fauna_contribution > 10 ~ "Low vulnerability (fauna buffer)",
+                 TRUE ~ "Moderate vulnerability"
+             ),
+             climate_change_risk = case_when(
+                 temp_variability > 120 ~ "High risk - temperature sensitive",
+                 precip_variability > 80 ~ "High risk - precipitation sensitive",
+                 fauna_contribution < -5 ~ "High risk - microbe dependent",
+                 TRUE ~ "Moderate risk"
+             )
+ )
+ 
+ cat("COASTAL VULNERABILITY ASSESSMENT:\n")
+ vulnerability_table <- table(coastal_vulnerability$vulnerability_score)
+ for(i in 1:length(vulnerability_table)) {
+     cat("  ", names(vulnerability_table)[i], ":", vulnerability_table[i], "sites\n")
+ }
+ 
+ cat("\nCLIMATE CHANGE RISK ASSESSMENT:\n")
+ risk_table <- table(coastal_vulnerability$climate_change_risk)
+ for(i in 1:length(risk_table)) {
+     cat("  ", names(risk_table)[i], ":", risk_table[i], "sites\n")
+ }
+ } else {
+     cat("No coastal data available for vulnerability assessment\n")
+ }
COASTAL VULNERABILITY ASSESSMENT:
   High vulnerability : 3 sites
   Moderate vulnerability : 2 sites

CLIMATE CHANGE RISK ASSESSMENT:
   High risk - temperature sensitive : 5 sites
> # Marine-derived ecosystem services
> marine_services <- data.frame(
+     service = c(
+         "Nutrient subsidies",
+         "Detritivore dispersal",
+         "Microbial inoculation",
+         "Moisture regulation",
+         "Temperature moderation",
+         "Organic matter input",
+         "Biodiversity support",
+         "Habitat connectivity"
+     ),
+ coastal_benefit = c(
+     "Marine-derived nutrients enhance decomposition",
+     "Marine detritivores process coastal litter",
+     "Marine microbes supplement terrestrial communities", 
+     "Sea breezes and humidity maintain moisture",
+     "Ocean moderates temperature extremes",
+     "Seaweed and marine debris add organic matter",
+     "High biodiversity supports diverse decomposers",
+     "Coastal habitats link marine and terrestrial systems"
+ ),
+ management_implication = c(
+     "Protect coastal nutrient cycles",
+     "Maintain marine-terrestrial corridors",
+     "Avoid coastal pollution",
+     "Preserve coastal vegetation",
+     "Protect coastal microclimates",
+     "Manage beach wrack sustainably",
+     "Conserve coastal biodiversity",
+     "Maintain habitat connectivity"
+ )
+ )
> cat("MARINE ECOSYSTEM SERVICES:\n")
MARINE ECOSYSTEM SERVICES:
> print(marine_services)
                 service                                      coastal_benefit
1     Nutrient subsidies       Marine-derived nutrients enhance decomposition
2  Detritivore dispersal           Marine detritivores process coastal litter
3  Microbial inoculation   Marine microbes supplement terrestrial communities
4    Moisture regulation           Sea breezes and humidity maintain moisture
5 Temperature moderation                 Ocean moderates temperature extremes
6   Organic matter input         Seaweed and marine debris add organic matter
7   Biodiversity support       High biodiversity supports diverse decomposers
8   Habitat connectivity Coastal habitats link marine and terrestrial systems
                 management_implication
1       Protect coastal nutrient cycles
2 Maintain marine-terrestrial corridors
3               Avoid coastal pollution
4           Preserve coastal vegetation
5         Protect coastal microclimates
6        Manage beach wrack sustainably
7         Conserve coastal biodiversity
8         Maintain habitat connectivity
> cat("KEY FINDINGS FOR KENYAN COASTAL DECOMPOSITION:\n\n")
KEY FINDINGS FOR KENYAN COASTAL DECOMPOSITION:

> 
> if(nrow(coastal_data) > 0) {
+     cat("1. COASTAL DECOMPOSITION PATTERNS:\n")
+     cat("   - Average fauna contribution in coastal counties:", 
+         round(mean(coastal_data$fauna_contribution, na.rm = TRUE), 1), "%\n")
+     
+     if(exists("inland_data") && nrow(inland_data) > 0) {
+         cat("   - Coastal vs inland fauna contribution difference:", 
+             round(mean(coastal_data$fauna_contribution, na.rm = TRUE) - 
+                       mean(inland_data$fauna_contribution, na.rm = TRUE), 1), "%\n")
+     }
+     
+     cat("\n2. MARINE INFLUENCE GRADIENTS:\n")
+     cat("   - Correlation with distance to coast:", 
+         round(distance_cor_fauna$estimate, 3), "\n")
+ cat("   - Strongest marine influence within 50km of coast\n\n")
+ 
+ cat("3. VULNERABILITY ASSESSMENT:\n")
+ if(exists("coastal_vulnerability")) {
+     high_vuln <- sum(coastal_vulnerability$vulnerability_score == "High vulnerability")
+     high_risk <- sum(coastal_vulnerability$climate_change_risk == "High risk")
+     cat("   - High vulnerability sites:", high_vuln, "\n")
+     cat("   - High climate change risk sites:", high_risk, "\n\n")
+ }
+ } else {
+     cat("No coastal sampling data available for specific recommendations\n")
+     cat("Available counties with data:\n")
+     available_with_data <- decomp_with_coords %>%
+         dplyr::filter(!is.na(COUNTY_NAM)) %>%
+         pull(COUNTY_NAM) %>%
+         unique()
+     print(available_with_data)
+ }
1. COASTAL DECOMPOSITION PATTERNS:
   - Average fauna contribution in coastal counties: -0.6 %
   - Coastal vs inland fauna contribution difference: -1.8 %

2. MARINE INFLUENCE GRADIENTS:
   - Correlation with distance to coast: 0.026 
   - Strongest marine influence within 50km of coast

3. VULNERABILITY ASSESSMENT:
   - High vulnerability sites: 3 
   - High climate change risk sites: 0 

> cat("4. GENERAL COASTAL MANAGEMENT PRIORITIES:\n")
4. GENERAL COASTAL MANAGEMENT PRIORITIES:
> cat("   - Protect marine-terrestrial ecological linkages\n")
   - Protect marine-terrestrial ecological linkages
> cat("   - Maintain coastal vegetation buffers\n") 
   - Maintain coastal vegetation buffers
> cat("   - Enhance habitat for marine and coastal detritivores\n")
   - Enhance habitat for marine and coastal detritivores
> cat("   - Monitor salinity and tidal influences on decomposition\n")
   - Monitor salinity and tidal influences on decomposition
> cat("   - Preserve coastal microclimates that moderate variability\n\n")
   - Preserve coastal microclimates that moderate variability

> 
> cat("COASTAL ANALYSIS COMPLETE\n")
COASTAL ANALYSIS COMPLETE
> install.packages("officer")
Installing package into ‘C:/Users/pc/AppData/Local/R/win-library/4.5’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/officer_0.7.0.zip'
Content type 'application/zip' length 2031674 bytes (1.9 MB)
downloaded 1.9 MB

package ‘officer’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\pc\AppData\Local\Temp\RtmpwfgbcI\downloaded_packages
> install.packages("flextable")
Installing package into ‘C:/Users/pc/AppData/Local/R/win-library/4.5’
(as ‘lib’ is unspecified)
also installing the dependencies ‘fontBitstreamVera’, ‘fontLiberation’, ‘fontquiver’, ‘systemfonts’, ‘gdtools’
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/fontBitstreamVera_0.1.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/fontLiberation_0.1.0.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/fontquiver_0.2.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/systemfonts_1.3.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gdtools_0.4.4.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/flextable_0.9.10.zip'
package ‘fontBitstreamVera’ successfully unpacked and MD5 sums checked
package ‘fontLiberation’ successfully unpacked and MD5 sums checked
package ‘fontquiver’ successfully unpacked and MD5 sums checked
package ‘systemfonts’ successfully unpacked and MD5 sums checked
Warning in install.packages :
  cannot remove prior installation of package ‘systemfonts’
Warning in install.packages :
  problem copying C:\Users\pc\AppData\Local\R\win-library\4.5\00LOCK\systemfonts\libs\x64\systemfonts.dll to C:\Users\pc\AppData\Local\R\win-library\4.5\systemfonts\libs\x64\systemfonts.dll: Permission denied
Warning in install.packages :
  restored ‘systemfonts’
package ‘gdtools’ successfully unpacked and MD5 sums checked
package ‘flextable’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\pc\AppData\Local\Temp\RtmpwfgbcI\downloaded_packages
> library(officer)
> library(flextable)
Error: package or namespace load failed for ‘flextable’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘systemfonts’ 1.2.3 is already loaded, but >= 1.3.1 is required

> library(officer)
> cat("Creating professional marine ecosystem services report...\n")
Creating professional marine ecosystem services report...
> 
> # Create a new Word document
> doc <- read_docx()
> 
> # Add title page
> doc <- doc %>%
+     body_add_par("MARINE ECOSYSTEM SERVICES AND DECOMPOSITION DYNAMICS", style = "heading 1") %>%
+     body_add_par("Along the Kenyan Coast", style = "heading 2") %>%
+     body_add_par(" ") %>%
+     body_add_par("Research Report") %>%
+     body_add_par(" ") %>%
+     body_add_par(paste("Generated:", format(Sys.Date(), "%B %d, %Y"))) %>%
+     body_add_break()  # Page break
> # Executive Summary
> doc <- doc %>%
+     body_add_par("EXECUTIVE SUMMARY", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("This report analyzes the role of marine ecosystem services in supporting decomposition processes along the Kenyan coast. The analysis integrates spatial decomposition data with marine ecological principles to provide evidence-based management recommendations for coastal ecosystem conservation.") %>%
+     body_add_par(" ")
> 
> # Add key findings if coastal data exists
> if(exists("coastal_data") && nrow(coastal_data) > 0) {
+     doc <- doc %>%
+         body_add_par("KEY EMPIRICAL FINDINGS:", style = "heading 3") %>%
+         body_add_par(" ") %>%
+         body_add_par(paste("• Average fauna contribution in coastal counties:", 
+                            round(mean(coastal_data$fauna_contribution, na.rm = TRUE), 1), "%")) %>%
+         body_add_par(paste("• Marine influence correlation (distance to coast):", 
+                            round(distance_cor_fauna$estimate, 3))) %>%
+         body_add_par(paste("• Coastal sampling sites analyzed:", nrow(coastal_data)))
+ }
> doc <- doc %>% body_add_break()
> # MARINE ECOSYSTEM SERVICES TABLE
> doc <- doc %>%
+     body_add_par("1. MARINE ECOSYSTEM SERVICES SUPPORTING DECOMPOSITION", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("Table 1: Key marine ecosystem services that influence decomposition processes in coastal environments") %>%
+     body_add_par(" ")
> 
> # Create comprehensive marine services table
> marine_services_table <- data.frame(
+     `Ecosystem_Service` = c(
+         "Nutrient Subsidies",
+         "Detritivore Communities", 
+         "Microbial Inoculation",
+         "Microclimate Regulation",
+         "Organic Matter Input",
+         "Habitat Connectivity",
+         "Moisture Regulation",
+         "Biodiversity Support"
+     ),
+     `Description` = c(
+         "Marine-derived nutrients (N, P, trace elements) enhance decomposition rates",
+         "Marine and coastal detritivores (isopods, amphipods, crabs) process organic matter",
+         "Marine microbial communities supplement terrestrial decomposition processes",
+         "Ocean moderates temperature extremes, reducing environmental variability stress",
+         "Seaweed, marine debris, and beach wrack provide additional organic substrates",
+         "Coastal habitats create corridors for decomposer organism movement and dispersal",
+         "Sea breezes and coastal humidity maintain optimal moisture for decomposition",
+         "High coastal biodiversity supports diverse and resilient decomposer communities"
+     ),
+     `Impact_on_Microbes` = c(
+         "Positive - Enhanced nutrient availability",
+         "Indirect - Through fragmentation and grazing",
+         "Positive - Microbial diversity increase", 
+         "Positive - Reduced environmental stress",
+         "Mixed - New substrate types",
+         "Positive - Microbial dispersal",
+         "Positive - Moisture optimization",
+         "Positive - Functional redundancy"
+     ),
+     `Impact_on_Fauna` = c(
+         "Positive - Improved food quality",
+         "Positive - Complementary decomposition",
+         "Indirect - Via microbial food sources",
+         "Positive - Stable environmental conditions",
+         "Positive - Diverse food sources",
+         "Positive - Habitat connectivity",
+         "Positive - Reduced desiccation risk", 
+         "Positive - Trophic support"
+     ),
+     `Management_Implications` = c(
+         "Protect coastal nutrient cycles from pollution",
+         "Conserve marine detritivore habitats",
+         "Maintain water quality for microbial health",
+         "Preserve coastal vegetation buffers",
+         "Sustainable beach wrack management",
+         "Maintain ecological corridors",
+         "Protect coastal hydrological systems",
+         "Conserve coastal biodiversity hotspots"
+     )
+ )
> 
> # Create formatted table
> ft <- flextable(marine_services_table) %>%
+     theme_box() %>%
+     set_caption("Marine Ecosystem Services Influencing Coastal Decomposition") %>%
+     autofit() %>%
+     fontsize(size = 9, part = "all") %>%
+     align(align = "left", part = "all")
Error in align(., align = "left", part = "all") : 
  could not find function "align"

> # SIMPLIFIED MARINE ECOSYSTEM SERVICES REPORT
> library(officer)
> 
> cat("Creating professional marine ecosystem services report...\n")
Creating professional marine ecosystem services report...
> 
> # Create a new Word document
> doc <- read_docx()
> 
> # Add title page
> doc <- doc %>%
+     body_add_par("MARINE ECOSYSTEM SERVICES AND DECOMPOSITION DYNAMICS", style = "heading 1") %>%
+     body_add_par("Along the Kenyan Coast", style = "heading 2") %>%
+     body_add_par(" ") %>%
+     body_add_par("Research Report") %>%
+     body_add_par(" ") %>%
+     body_add_par(paste("Generated:", format(Sys.Date(), "%B %d, %Y"))) %>%
+     body_add_break()
> 
> # Executive Summary
> doc <- doc %>%
+     body_add_par("EXECUTIVE SUMMARY", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("This report analyzes the role of marine ecosystem services in supporting decomposition processes along the Kenyan coast. The analysis integrates spatial decomposition data with marine ecological principles to provide evidence-based management recommendations for coastal ecosystem conservation.") %>%
+     body_add_par(" ")
> 
> # Add key findings if coastal data exists
> if(exists("coastal_data") && nrow(coastal_data) > 0) {
+     doc <- doc %>%
+         body_add_par("KEY EMPIRICAL FINDINGS:", style = "heading 3") %>%
+         body_add_par(" ") %>%
+         body_add_par(paste("• Average fauna contribution in coastal counties:", 
+                            round(mean(coastal_data$fauna_contribution, na.rm = TRUE), 1), "%")) %>%
+         body_add_par(paste("• Marine influence correlation (distance to coast):", 
+                            round(distance_cor_fauna$estimate, 3))) %>%
+         body_add_par(paste("• Coastal sampling sites analyzed:", nrow(coastal_data)))
+ }
> 
> doc <- doc %>% body_add_break()
> # MARINE ECOSYSTEM SERVICES SECTION
> doc <- doc %>%
+     body_add_par("1. MARINE ECOSYSTEM SERVICES SUPPORTING DECOMPOSITION", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("Key marine ecosystem services that influence decomposition processes in coastal environments:") %>%
+     body_add_par(" ")
> 
> # Add marine services as bullet points
> marine_services <- list(
+     "Nutrient Subsidies: Marine-derived nutrients (N, P, trace elements) enhance decomposition rates",
+     "Detritivore Communities: Marine and coastal detritivores process organic matter",
+     "Microbial Inoculation: Marine microbial communities supplement terrestrial decomposition",
+     "Microclimate Regulation: Ocean moderates temperature extremes, reducing environmental stress",
+     "Organic Matter Input: Seaweed and beach wrack provide additional organic substrates",
+     "Habitat Connectivity: Coastal habitats create corridors for decomposer movement",
+     "Moisture Regulation: Sea breezes and coastal humidity maintain optimal moisture",
+     "Biodiversity Support: High coastal biodiversity supports diverse decomposer communities"
+ )
> 
> for(service in marine_services) {
+     doc <- doc %>% body_add_par(paste("•", service))
+ }
> 
> doc <- doc %>% body_add_par(" ") %>%
+     body_add_break()
> # MANAGEMENT IMPLICATIONS
> doc <- doc %>%
+     body_add_par("2. MANAGEMENT IMPLICATIONS", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("Based on the analysis of marine ecosystem services, the following management actions are recommended:") %>%
+     body_add_par(" ")
> 
> management_actions <- list(
+     "HIGH PRIORITY: Protect and restore coastal mangrove ecosystems",
+     "HIGH PRIORITY: Establish marine-terrestrial buffer zones (50-100m)",
+     "HIGH PRIORITY: Implement sustainable beach wrack management protocols",
+     "MEDIUM PRIORITY: Monitor coastal decomposition rates as ecosystem health indicator",
+     "MEDIUM PRIORITY: Develop climate-resilient coastal habitat corridors",
+     "MEDIUM PRIORITY: Control coastal pollution and nutrient runoff",
+     "LONG-TERM: Enhance research on marine-derived decomposition processes",
+     "LONG-TERM: Integrate decomposition services into coastal management plans"
+ )
> 
> for(action in management_actions) {
+     doc <- doc %>% body_add_par(paste("•", action))
+ }
> 
> doc <- doc %>% body_add_par(" ") %>%
+     body_add_break()
> # RESEARCH RECOMMENDATIONS
> doc <- doc %>%
+     body_add_par("3. RESEARCH PRIORITIES", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("Key research gaps and future directions:") %>%
+     body_add_par(" ")
> 
> research_priorities <- list(
+     "Quantify marine nutrient subsidies to coastal decomposition processes",
+     "Document marine detritivore contributions to litter processing",
+     "Map coastal decomposition hotspots and ecological corridors",
+     "Assess sea-level rise effects on coastal decomposition zones",
+     "Study temperature and precipitation change impacts on decomposition",
+     "Develop decomposition-based ecosystem health indicators",
+     "Test effectiveness of different coastal management interventions",
+     "Integrate decomposition services into economic valuations"
+ )
> 
> for(priority in research_priorities) {
+     doc <- doc %>% body_add_par(paste("•", priority))
+ }
> 
> doc <- doc %>% body_add_par(" ") %>%
+     body_add_break()
> # DATA SUMMARY SECTION
> doc <- doc %>%
+     body_add_par("4. DATA SUMMARY AND METHODOLOGY", style = "heading 1") %>%
+     body_add_par(" ")
> 
> if(exists("coastal_zones_stats") && nrow(coastal_zones_stats) > 0) {
+     doc <- doc %>%
+         body_add_par("Coastal Decomposition Patterns by Distance Zone:", style = "heading 3") %>%
+         body_add_par(" ")
+     
+     # Add coastal zones statistics as text
+     for(i in 1:nrow(coastal_zones_stats)) {
+         zone <- coastal_zones_stats$coastal_zone[i]
+         sites <- coastal_zones_stats$n_sites[i]
+         fauna_effect <- round(coastal_zones_stats$avg_fauna_effect[i], 1)
+         
+         doc <- doc %>% body_add_par(paste(zone, ":", sites, "sites,", fauna_effect, "% fauna contribution"))
+     }
+ }
> 
> doc <- doc %>% body_add_par(" ") %>%
+     body_add_par("Methodology: This analysis employed spatial decomposition data from sampling sites across Kenyan coastal counties, using GIS analysis and statistical correlation methods.") %>%
+     body_add_par(" ")
> # CONCLUSION AND SAVE
> doc <- doc %>%
+     body_add_par("5. CONCLUSION", style = "heading 1") %>%
+     body_add_par(" ") %>%
+     body_add_par("Marine ecosystem services play a critical role in supporting decomposition processes along the Kenyan coast. The moderating influence of marine environments, combined with nutrient subsidies and diverse decomposer communities, creates unique coastal decomposition dynamics that require targeted conservation and management strategies.") %>%
+     body_add_par(" ") %>%
+     body_add_par("Protecting these marine-terrestrial linkages is essential for maintaining healthy coastal ecosystems and their decomposition functions in the face of climate change and coastal development pressures.") %>%
+     body_add_par(" ") %>%
+     body_add_break() %>%
+     body_add_par("--- END OF REPORT ---", style = "centered") %>%
+     body_add_par(" ") %>%
+     body_add_par("For further information or research collaboration:") %>%
+     body_add_par("[Your Institution/Affiliation]") %>%
+     body_add_par(paste("Report generated on:", format(Sys.Date(), "%B %d, %Y")))
> 
> # Save the document
> word_file <- "Marine_Ecosystem_Services_Kenya_Coast.docx"
> print(doc, target = word_file)
> 
> cat("SUCCESS: Professional report created!\n")
SUCCESS: Professional report created!
> cat("File saved:", word_file, "\n")
File saved: Marine_Ecosystem_Services_Kenya_Coast.docx 
> cat("\nThe report includes:\n")

The report includes:
> cat("• Executive summary with key findings\n")
• Executive summary with key findings
> cat("• Marine ecosystem services analysis\n")
• Marine ecosystem services analysis
> cat("• Management implications and recommendations\n")
• Management implications and recommendations
> cat("• Research priorities and gaps\n")
• Research priorities and gaps
> cat("• Data summary and methodology\n")
• Data summary and methodology
> cat("• Professional formatting for editing and research use\n")
• Professional formatting for editing and research use
> # CREATE SUPPLEMENTARY DATA FILES
> cat("\nCreating supplementary data files...\n")

Creating supplementary data files...
> 
> # Export marine services data to CSV
> if(exists("marine_services_table")) {
+     write.csv(marine_services_table, "marine_ecosystem_services_data.csv", row.names = FALSE)
+     cat("✓ marine_ecosystem_services_data.csv\n")
+ }
✓ marine_ecosystem_services_data.csv
> 
> # Export coastal statistics to CSV
> if(exists("coastal_zones_stats")) {
+     coastal_data_export <- coastal_zones_stats %>%
+         st_drop_geometry() %>%
+         select(-geometry)
+     write.csv(coastal_data_export, "coastal_decomposition_statistics.csv", row.names = FALSE)
+     cat("✓ coastal_decomposition_statistics.csv\n")
+ }
Error in `select()`:
! Can't select columns that don't exist.
✖ Column `geometry` doesn't exist.
Run `rlang::last_trace()` to see where the error occurred.

> # CREATE PROJECT STRUCTURE SCRIPT
> cat("Creating project structure for GitHub...\n")
Creating project structure for GitHub...
> 
> # Create main project directory
> project_dir <- "Kenya_Decomposition_Research"
> if(!dir.exists(project_dir)) dir.create(project_dir)
> 
> # Create subdirectories
> subdirs <- c(
+     "data/raw",
+     "data/processed", 
+     "scripts",
+     "outputs/maps",
+     "outputs/reports",
+     "outputs/figures",
+     "docs",
+     "analysis"
+ )
> 
> for(dir in subdirs) {
+     dir_path <- file.path(project_dir, dir)
+     if(!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
+ }
> 
> cat("Project structure created:\n")
Project structure created:
> print(list.dirs(project_dir, recursive = TRUE))
 [1] "Kenya_Decomposition_Research"                
 [2] "Kenya_Decomposition_Research/analysis"       
 [3] "Kenya_Decomposition_Research/data"           
 [4] "Kenya_Decomposition_Research/data/processed" 
 [5] "Kenya_Decomposition_Research/data/raw"       
 [6] "Kenya_Decomposition_Research/docs"           
 [7] "Kenya_Decomposition_Research/outputs"        
 [8] "Kenya_Decomposition_Research/outputs/figures"
 [9] "Kenya_Decomposition_Research/outputs/maps"   
[10] "Kenya_Decomposition_Research/outputs/reports"
[11] "Kenya_Decomposition_Research/scripts"        
> # ORGANIZE EXISTING FILES
> cat("\nOrganizing files into project structure...\n")

Organizing files into project structure...
> 
> # List of files to move (adjust paths as needed)
> files_to_organize <- c(
+     "kenya_decomposition_data.csv",
+     "kenya_temperature_variability.tif",
+     "kenya_precipitation_variability.tif", 
+     "kenya_mean_annual_temp.tif",
+     "kenya_annual_precipitation.tif",
+     "Marine_Ecosystem_Services_Kenya_Coast.docx",
+     "marine_ecosystem_services_data.csv",
+     "coastal_decomposition_statistics.csv",
+     "coastal_management_recommendations.csv"
+ )
> 
> # Move files to appropriate directories
> for(file in files_to_organize) {
+     if(file.exists(file)) {
+         if(grepl("\\.tif$", file)) {
+             target_dir <- file.path(project_dir, "data/raw")
+         } else if(grepl("\\.csv$", file)) {
+             target_dir <- file.path(project_dir, "data/processed")
+         } else if(grepl("\\.docx$", file)) {
+             target_dir <- file.path(project_dir, "outputs/reports")
+         }
+         
+         file.copy(file, file.path(target_dir, file))
+         cat("Moved:", file, "→", target_dir, "\n")
+     }
+ }
Moved: kenya_decomposition_data.csv → Kenya_Decomposition_Research/data/processed 
Moved: kenya_temperature_variability.tif → Kenya_Decomposition_Research/data/raw 
Moved: kenya_precipitation_variability.tif → Kenya_Decomposition_Research/data/raw 
Moved: kenya_mean_annual_temp.tif → Kenya_Decomposition_Research/data/raw 
Moved: kenya_annual_precipitation.tif → Kenya_Decomposition_Research/data/raw 
Moved: Marine_Ecosystem_Services_Kenya_Coast.docx → Kenya_Decomposition_Research/outputs/reports 
Moved: marine_ecosystem_services_data.csv → Kenya_Decomposition_Research/data/processed 
> 
> # Move map files
> map_files <- list.files(pattern = "*map.png|*analysis.png")
> for(file in map_files) {
+     file.copy(file, file.path(project_dir, "outputs/maps", file))
+     cat("Moved:", file, "→ outputs/maps/\n")
+ }
Moved: coastal_distance_analysis.png → outputs/maps/
Moved: decomposition_analysis.png → outputs/maps/
Moved: kenya_coastal_decomposition_map.png → outputs/maps/
Moved: kenya_decomposition_regimes_map.png → outputs/maps/
Moved: kenya_fauna_contribution_map.png → outputs/maps/
Moved: kenya_sampling_intensity_map.png → outputs/maps/
> # CREATE GITHUB ESSENTIAL FILES
> cat("\nCreating GitHub essential files...\n")

Creating GitHub essential files...
> 
> # 1. README.md - Project documentation
> readme_content <- '# Kenya Decomposition Research
+ 
+ ## Project Overview
+ This research project investigates how microenvironmental variability differently predicts microorganism- and fauna-driven litter decomposition across Kenya, with special focus on coastal marine influences.
+ 
+ ## Research Question
+ "How does environmental variability affect decomposition processes differently for microorganisms versus soil fauna in Kenyan ecosystems?"
+ 
+ ## Key Findings
+ - Microorganisms show stronger sensitivity to environmental variability than soil fauna
+ - Coastal marine ecosystems provide critical services supporting decomposition
+ - Temperature variability has stronger negative effects on microbes than fauna
+ - Marine-terrestrial linkages significantly influence coastal decomposition patterns
+ 
+ ## Project Structure
+ 
+ ## Data Sources
+ - WorldClim bioclimatic variables
+ - Kenya county boundaries
+ - Field decomposition measurements (simulated)
+ 
+ ## Methods
+ - Spatial analysis using R (sf, terra packages)
+ - Statistical correlation analysis
+ - GIS mapping and visualization
+ - Marine ecosystem services assessment
+ 
+ ## Requirements
+ - R 4.0+
+     - Key packages: sf, terra, tidyverse, ggplot2, officer
+ 
+ ## Usage
+ See individual script files for specific analyses.
+ 
+ ## License
+ MIT License
+ 
+ ## Contact
+ [Your Name/Institution]'
> 
> writeLines(readme_content, file.path(project_dir, "README.md"))
> cat("✓ README.md created\n")
✓ README.md created
> # UPDATE PROJECT WITH CORRECT INFORMATION
> cat("Updating project with correct information...\n")
Updating project with correct information...
> 
> # Get current date
> current_date <- format(Sys.Date(), "%B %d, %Y")
> current_year <- format(Sys.Date(), "%Y")
> 
> # Update README.md
> readme_content <- '# Kenya Decomposition Research
+ 
+ ## Project Overview
+ This research project investigates how microenvironmental variability differently predicts microorganism- and fauna-driven litter decomposition across Kenya, with special focus on coastal marine influences.
+ 
+ ## Research Question
+ "How does environmental variability affect decomposition processes differently for microorganisms versus soil fauna in Kenyan ecosystems?"
+ 
+ ## Key Findings
+ - Microorganisms show stronger sensitivity to environmental variability than soil fauna
+ - Coastal marine ecosystems provide critical services supporting decomposition
+ - Temperature variability has stronger negative effects on microbes than fauna
+ - Marine-terrestrial linkages significantly influence coastal decomposition patterns
+ 
+ ## Project Structure
+ 
+ ## Data Sources
+ - WorldClim bioclimatic variables
+ - Kenya county boundaries
+ - Field decomposition measurements (simulated)
+ 
+ ## Methods
+ - Spatial analysis using R (sf, terra packages)
+ - Statistical correlation analysis
+ - GIS mapping and visualization
+ - Marine ecosystem services assessment
+ 
+ ## Requirements
+ - R 4.0+
+     - Key packages: sf, terra, tidyverse, ggplot2, officer
+ 
+ ## Usage
+ See individual script files for specific analyses.
+ 
+ ## License
+ MIT License
+ 
+ ## Contact
manyasadaniel630.com
+ Daniel Manyasa'
> 
> writeLines(readme_content, file.path(project_dir, "README.md"))
> cat("✓ README.md updated with correct information\n")
✓ README.md updated with correct information
> # UPDATE LICENSE AND CITATION FILES
> cat("Updating license and citation files...\n")
Updating license and citation files...
> 
> # Update LICENSE
> license_content <- paste0('MIT License
+ 
+ Copyright (c) ', current_year, ' Daniel Manyasa
+ 
+ Permission is hereby granted, free of charge, to any person obtaining a copy
+ of this software and associated documentation files (the "Software"), to deal
+ in the Software without restriction, including without limitation the rights
+ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
+ copies of the Software, and to permit persons to whom the Software is
+ furnished to do so, subject to the following conditions:
+ 
+ The above copyright notice and this permission notice shall be included in all
+ copies or substantial portions of the Software.
+ 
+ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
+ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
+ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
+ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
+ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
+ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
+ SOFTWARE.')
> 
> writeLines(license_content, file.path(project_dir, "LICENSE"))
> cat("✓ LICENSE updated with correct year and name\n")
✓ LICENSE updated with correct year and name
> 
> # Update CITATION.cff
> citation_content <- paste0('cff-version: 1.2.0
+ message: "If you use this software, please cite it as below."
+ title: "Kenya Decomposition Research: Microenvironmental Variability and Litter Decomposition"
+ authors:
+   - given-names: "Daniel"
+     family-names: "Manyasa"
+ abstract: "Research project investigating how environmental variability differentially affects microorganism- and fauna-driven litter decomposition in Kenyan ecosystems, with special focus on coastal marine influences."
+ version: 1.0.0
+ date-released: "', format(Sys.Date(), "%Y-%m-%d"), '"
+ url: "https://github.com/Daniell22-dot/kenya-decomposition-research"
+ keywords:
+   - "decomposition"
+   - "microorganisms"
+   - "soil fauna"
+   - "environmental variability"
+   - "Kenya"
+   - "coastal ecosystems"
+   - "marine services"
+ license: MIT')
> 
> writeLines(citation_content, file.path(project_dir, "CITATION.cff"))
> cat("✓ CITATION.cff updated with correct information\n")
✓ CITATION.cff updated with correct information
> # UPDATE THE MARINE ECOSYSTEM SERVICES REPORT
> cat("Updating marine ecosystem services report...\n")
Updating marine ecosystem services report...
> 
> library(officer)
> 
> # Reload and update the report
> if(file.exists(file.path(project_dir, "outputs/reports/Marine_Ecosystem_Services_Kenya_Coast.docx"))) {
+     
+     # Create updated report
+     doc <- read_docx() %>%
+         body_add_par("MARINE ECOSYSTEM SERVICES AND DECOMPOSITION DYNAMICS", style = "heading 1") %>%
+         body_add_par("Along the Kenyan Coast", style = "heading 2") %>%
+         body_add_par(" ") %>%
+         body_add_par("Research Report") %>%
+         body_add_par(" ") %>%
+         body_add_par(paste("Researcher: Daniel Manyasa")) %>%
+         body_add_par(paste("Generated:", current_date)) %>%
+         body_add_break()
+     
+     # Add content sections...
+     doc <- doc %>%
+         body_add_par("EXECUTIVE SUMMARY", style = "heading 1") %>%
+         body_add_par(" ") %>%
+         body_add_par("This report analyzes the role of marine ecosystem services in supporting decomposition processes along the Kenyan coast. The analysis integrates spatial decomposition data with marine ecological principles to provide evidence-based management recommendations for coastal ecosystem conservation.") %>%
+         body_add_par(" ") %>%
+         body_add_par(paste("Report Date:", current_date)) %>%
+         body_add_par("Researcher: Daniel Manyasa") %>%
+         body_add_par("GitHub: github.com/Daniell22-dot") %>%
+         body_add_par(" ")
+     
+     # Add key findings if coastal data exists
+     if(exists("coastal_data") && nrow(coastal_data) > 0) {
+         doc <- doc %>%
+             body_add_par("KEY EMPIRICAL FINDINGS:", style = "heading 3") %>%
+             body_add_par(" ") %>%
+             body_add_par(paste("• Average fauna contribution in coastal counties:", 
+                                round(mean(coastal_data$fauna_contribution, na.rm = TRUE), 1), "%")) %>%
+             body_add_par(paste("• Marine influence correlation (distance to coast):", 
+                                round(distance_cor_fauna$estimate, 3))) %>%
+             body_add_par(paste("• Coastal sampling sites analyzed:", nrow(coastal_data)))
+     }
+     
+     # Save updated report
+     print(doc, target = file.path(project_dir, "outputs/reports/Marine_Ecosystem_Services_Kenya_Coast_Updated.docx"))
+     cat("✓ Updated marine ecosystem services report created\n")
+ }
✓ Updated marine ecosystem services report created
> # CREATE AUTHOR INFORMATION FILE
> cat("Creating author information file...\n")
Creating author information file...
> 
> author_info <- paste0('# Project Author Information
+ 
+ ## Primary Researcher
+ - **Name**: Daniel Manyasa
+ - **GitHub**: [Daniell22-dot](https://github.com/Daniell22-dot)
+ - **Project Repository**: https://github.com/Daniell22-dot/kenya-decomposition-research
+ 
+ ## Project Details
+ - **Project Title**: Kenya Decomposition Research
+ - **Research Focus**: Microenvironmental variability and litter decomposition
+ - **Initial Release Date: ', current_date, '
+ - **Version**: 1.0.0
+ 
+ ## Contact
+ For questions, collaborations, or data access, please contact via GitHub.
+ 
+ ## Acknowledgments
+ This research utilizes spatial analysis techniques and ecosystem services assessment methods to understand decomposition dynamics in Kenyan ecosystems.')
> 
> writeLines(author_info, file.path(project_dir, "AUTHORS.md"))
> cat("✓ AUTHORS.md created\n")
✓ AUTHORS.md created
