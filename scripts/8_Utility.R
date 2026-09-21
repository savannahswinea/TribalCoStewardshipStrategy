# The purpose of this script is to combine all objectives
# to arrive at an overall utility score across the Area of Interest

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(here)
library(terra)
library(dplyr)

here::i_am("scripts/8_Utility.R")

# What you need
# The habitat presence/absence data
HeadwatersandCreeks <- rast(here("outputs", "HeadwatersandCreeks.tif"))
MediumRivers <- rast(here("outputs", "MediumRivers.tif"))
LakesReservoirs <- rast(here("outputs", "LakesandReservoirs.tif"))
WaterRichness <- rast(here("outputs", "WaterHabitat_Richness.tif"))

Forests <- rast(here("outputs", "Forests.tif"))
Grasslands <- rast(here("outputs", "Grasslands.tif"))
Shrublands <- rast(here("outputs", "Shrublands.tif"))

Cliffs <- rast(here("outputs", "Cliffs.tif"))
UpperSlopes <- rast(here("outputs", "UpperSlopes.tif"))
SideSlopes <- rast(here("outputs", "SideSlopes.tif"))
Flats <- rast(here("outputs", "Flats.tif"))

# Tribal Value of Habitats
# ___________________________________________________________________________________________________________

tribalValue <- read.csv(here("TribalValue_Normalized.csv"))

headwaters_value <- HeadwatersandCreeks * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "HeadwatersandCreeks"]
mediumrivers_value <- MediumRivers * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "MediumRivers"]
lakes_value <- LakesReservoirs * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "LakesandReservoirs"]
water_value <- ifel(
  
  # If a pixel does not contain any water, its water utility is 0
  WaterRichness == 0, 0,
  
  # Otherwise, water value is the average score of the water habitats present in that pixel 
  (headwaters_value +
     mediumrivers_value +
     lakes_value) / WaterRichness
)

forest_value <- Forests * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "Forests"]
grassland_value <- Grasslands * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "Grasslands"]
shrubland_value <- Shrublands * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "Shrublands"]

cliff_value <- Cliffs * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "Cliffs"]
upper_value <- UpperSlopes * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "UpperSlopes"]
side_value <- SideSlopes * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "SideSlopes"]
flats_value <- Flats * tribalValue$NormalizedTribalValue[tribalValue$Habitat == "Flats"]

tribalValue_rast <- (water_value + forest_value + grassland_value + shrubland_value + 
                    cliff_value + upper_value + side_value + flats_value)

plot(tribalValue_rast)

# Examining the distribution of tribal value
tribalValue_vals <- data.frame(values(tribalValue_rast))
tribalValue_vals_clean <- tribalValue_vals %>%
  rename(values = HeadwatersandCreeks) %>%
  filter(is.finite(values))
hist(tribalValue_vals_clean$values)
boxplot(tribalValue_vals_clean$values)

# Optional: truncate the distribution to prevent outliers from
# skewing the result
Q3 <- quantile(tribalValue_vals_clean$values, 0.75, na.rm = TRUE)

iqr <- IQR(tribalValue_vals_clean$values, na.rm = TRUE)

tribalValue_thresh <- Q3 + 1.5 * iqr

tribalValue_trunc <- pmin(tribalValue_vals_clean$values, tribalValue_thresh)
hist(tribalValue_trunc)

tribalValue_rast_trunc <- clamp(tribalValue_rast, upper = tribalValue_thresh)
plot(tribalValue_rast_trunc)

# Normalizing tribal value from 0 to 1
tribalValue_min <- global(tribalValue_rast_trunc, "min", na.rm = TRUE)[1,1]
tribalValue_max <- global(tribalValue_rast_trunc, "max", na.rm = TRUE)[1,1]

tribalValue_normalized <- app(
  tribalValue_rast_trunc,
  fun = function(x) (x - tribalValue_min) / (tribalValue_max - tribalValue_min)
)

# Rarity of Habitats
# ___________________________________________________________________________________________________________

rarity <- read.csv(here("HabitatRarity_Normalized.csv"))

headwaters_rarity <- HeadwatersandCreeks * rarity$NormalizedRarity[rarity$Habitat == "HeadwatersandCreeks"]
mediumrivers_rarity <- MediumRivers * rarity$NormalizedRarity[rarity$Habitat == "MediumRivers"]
lakes_rarity <- LakesReservoirs * rarity$NormalizedRarity[rarity$Habitat == "LakesandReservoirs"]
water_rarity <- ifel(
  
  # If a pixel does not contain any water, its water utility is 0
  WaterRichness == 0, 0,
  
  # Otherwise, water value is the average score of the water habitats present in that pixel 
  (headwaters_rarity +
     mediumrivers_rarity +
     lakes_rarity) / WaterRichness
)

forest_rarity <- Forests * rarity$NormalizedRarity[rarity$Habitat == "Forests"]
grassland_rarity <- Grasslands * rarity$NormalizedRarity[rarity$Habitat == "Grasslands"]
shrubland_rarity <- Shrublands * rarity$NormalizedRarity[rarity$Habitat == "Shrublands"]

cliff_rarity <- Cliffs * rarity$NormalizedRarity[rarity$Habitat == "Cliffs"]
upper_rarity <- UpperSlopes * rarity$NormalizedRarity[rarity$Habitat == "UpperSlopes"]
side_rarity <- SideSlopes * rarity$NormalizedRarity[rarity$Habitat == "SideSlopes"]
flats_rarity <- Flats * rarity$NormalizedRarity[rarity$Habitat == "Flats"]

rarity_rast <- (water_rarity + forest_rarity + grassland_rarity + shrubland_rarity + 
                       cliff_rarity + upper_rarity + side_rarity + flats_rarity)

plot(rarity_rast)

# Examining the distribution of rarity
rarity_vals <- data.frame(values(rarity_rast))
rarity_vals_clean <- rarity_vals %>%
  rename(values = HeadwatersandCreeks) %>%
  filter(is.finite(values))
hist(rarity_vals_clean$values)
boxplot(rarity_vals_clean$values)

# Optional: truncate the distribution to prevent outliers from
# skewing the result
Q3 <- quantile(rarity_vals_clean$values, 0.75, na.rm = TRUE)

iqr <- IQR(rarity_vals_clean$values, na.rm = TRUE)

rarity_thresh <- Q3 + 1.5 * iqr

rarity_trunc <- pmin(rarity_vals_clean$values, rarity_thresh)
hist(rarity_trunc)

rarity_rast_trunc <- clamp(rarity_rast, upper = rarity_thresh)
plot(rarity_rast_trunc)

# Normalizing rarity from 0 to 1
rarity_min <- global(rarity_rast_trunc, "min", na.rm = TRUE)[1,1]
rarity_max <- global(rarity_rast_trunc, "max", na.rm = TRUE)[1,1]

rarity_normalized <- app(
  rarity_rast_trunc,
  fun = function(x) (x - rarity_min) / (rarity_max - rarity_min)
)

# Availability of Habitats
# ___________________________________________________________________________________________________________

availability <- read.csv(here("HabitatAvailability_Normalized.csv"))

headwaters_availability <- HeadwatersandCreeks * availability$NormalizedAvailability[availability$Habitat == "HeadwatersandCreeks"]
mediumrivers_availability <- MediumRivers * availability$NormalizedAvailability[availability$Habitat == "MediumRivers"]
lakes_availability <- LakesReservoirs * availability$NormalizedAvailability[availability$Habitat == "LakesandReservoirs"]
water_availability <- ifel(
  
  # If a pixel does not contain any water, its water utility is 0
  WaterRichness == 0, 0,
  
  # Otherwise, water value is the average score of the water habitats present in that pixel 
  (headwaters_availability +
     mediumrivers_availability +
     lakes_availability) / WaterRichness
)

forest_availability <- Forests * availability$NormalizedAvailability[availability$Habitat == "Forests"]
grassland_availability <- Grasslands * availability$NormalizedAvailability[rarity$Habitat == "Grasslands"]
shrubland_availability <- Shrublands * availability$NormalizedAvailability[rarity$Habitat == "Shrublands"]

cliff_availability <- Cliffs * availability$NormalizedAvailability[availability$Habitat == "Cliffs"]
upper_availability <- UpperSlopes * availability$NormalizedAvailability[availability$Habitat == "UpperSlopes"]
side_availability <- SideSlopes * availability$NormalizedAvailability[availability$Habitat == "SideSlopes"]
flats_availability <- Flats * availability$NormalizedAvailability[availability$Habitat == "Flats"]

availability_rast <- (water_availability + forest_availability + grassland_availability + shrubland_availability + 
                  cliff_availability + upper_availability + side_availability + flats_availability)

plot(availability_rast)

# Examining the distribution of rarity
availability_vals <- data.frame(values(availability_rast))
availability_vals_clean <- availability_vals %>%
  rename(values = HeadwatersandCreeks) %>%
  filter(is.finite(values))
hist(availability_vals_clean$values)
boxplot(availability_vals_clean$values)

# Optional: truncate the distribution to prevent outliers from
# skewing the result
Q3 <- quantile(availability_vals_clean$values, 0.75, na.rm = TRUE)

iqr <- IQR(availability_vals_clean$values, na.rm = TRUE)

availability_thresh <- Q3 + 1.5 * iqr

availability_trunc <- pmin(availability_vals_clean$values, availability_thresh)
hist(availability_trunc)

availability_rast_trunc <- clamp(availability_rast, upper = availability_thresh)
plot(availability_rast_trunc)

# Normalizing rarity from 0 to 1
availability_min <- global(availability_rast_trunc, "min", na.rm = TRUE)[1,1]
availability_max <- global(availability_rast_trunc, "max", na.rm = TRUE)[1,1]

availability_normalized <- app(
  availability_rast_trunc,
  fun = function(x) (x - availability_min) / (availability_max - availability_min)
)

# Other Objectives: Proximity, Climate Similarity, and Connectivity
# ___________________________________________________________________________________________________________

proximity <- rast(here("outputs", "Proximity_Normalized.tif"))
climateSimilarity <- rast(here("outputs", "ClimateSimilarity_Normalized.tif"))
connectivity <- rast(here("outputs", "Connectivity_Normalized.tif"))

# Add all the objectives together to get a raw utility score
utility <- tribalValue_normalized + rarity_normalized + availability_normalized +
  proximity + climateSimilarity + connectivity

plot(utility)

# Normalizing utility from 0 to 1
utility_min <- global(utility, "min", na.rm = TRUE)[1,1]
utility_max <- global(utility, "max", na.rm = TRUE)[1,1]

utility_normalized <- app(
  utility,
  fun = function(x) (x - utility_min) / (utility_max - utility_min)
)

plot(utility_normalized)

# Plot normalized utility using the mean and distance in standard deviations
utility_stats <- global(utility, c("mean", "sd"), na.rm = TRUE)
utility_mu  <- utility_stats[1, "mean"]
utility_sdv <- utility_stats[1, "sd"]
breaks <- c(-Inf, utility_mu - 2*utility_sdv, utility_mu - 1*utility_sdv, utility_mu, 
            utility_mu + 1*utility_sdv, utility_mu + 2*utility_sdv, Inf)

plot(utility,
     breaks = breaks,
     col = hcl.colors(length(breaks)-1, rev = TRUE))

writeRaster(utility_normalized, filename = here("outputs", "Utility_Normalized.tif"), overwrite = TRUE)
