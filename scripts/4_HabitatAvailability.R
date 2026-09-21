# The purpose of this script is to calculate the availability
# Of each habitat in the Area of Interest
# This objective will only be relevant for tribes that have lands that they administer

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(terra)
library(here)

here::i_am("scripts/4_HabitatAvailability.R")

# What you need
# The habitat presence/absence data
HeadwatersandCreeks <- rast(here("outputs", "HeadwatersandCreeks.tif"))
MediumRivers <- rast(here("outputs", "MediumRivers.tif"))
LakesReservoirs <- rast(here("outputs", "LakesandReservoirs.tif"))

Forests <- rast(here("outputs", "Forests.tif"))
Grasslands <- rast(here("outputs", "Grasslands.tif"))
Shrublands <- rast(here("outputs", "Shrublands.tif"))

Cliffs <- rast(here("outputs", "Cliffs.tif"))
UpperSlopes <- rast(here("outputs", "UpperSlopes.tif"))
SideSlopes <- rast(here("outputs", "SideSlopes.tif"))
Flats <- rast(here("outputs", "Flats.tif"))

# Read in tribal boundary shapefile
tribe <- st_read("tribe/tribe.shp")

# Read in Area of Interest shapefile
aoi <- st_read("AOI/AOI.shp")

# Check that the coordinate reference systems match
crs(aoi) == crs(tribe)

# Habitat Availability on Tribal Lands vs. the Area of Interest
# __________________________________________________________________________________________________________________

# Each habitat raster has only values of 0/1 (absence/presence)
# Taking the mean tells us the proportion of cells with that habitat present
# We will use the habitat presence on tribal lands compared to the Area of Interest
# To develop an idea of which habitats are relatively available/unavailable for the tribe

# Put habitat names in a list to help with organizing
habitats <- list(
  HeadwatersandCreeks = HeadwatersandCreeks,
  MediumRivers = MediumRivers,
  LakesandReservoirs = LakesReservoirs,
  Forests = Forests,
  Grasslands = Grasslands,
  Shrublands = Shrublands,
  Cliffs = Cliffs,
  UpperSlopes = UpperSlopes,
  SideSlopes = SideSlopes,
  Flats = Flats
)

# Calculate mean presence (proportion of cells equal to 1)
aoi_means <- sapply(habitats, function(x) {
  global(x, "mean", na.rm = TRUE)[1, 1]
})

# Convert sf object to SpatVector
tribe_vect <- vect(tribe)

# Calculate mean within the tribal boundary
tribe_means <- sapply(habitats, function(x) {
  x_tribe <- mask(crop(x, tribe_vect), tribe_vect)
  global(x_tribe, "mean", na.rm = TRUE)[1, 1]
})

# Availability ratio
Availability <- tribe_means / aoi_means

# Invert values that are less than 1 to represent a departure from 1, preserving magnitude
Availability <- ifelse(
  Availability < 1,
  -(1 / Availability),
  Availability
)

# Normalize to 0-1
normalized <- (Availability - min(Availability, na.rm = TRUE)) /
  (max(Availability, na.rm = TRUE) -
     min(Availability, na.rm = TRUE))

# Data frame
habitatAvailability <- data.frame(
  Habitat = names(Availability),
  NormalizedAvailability = normalized,
  row.names = NULL
)

# Write to CSV
write.csv(habitatAvailability,
          "HabitatAvailability_Normalized.csv",
          row.names = FALSE)
