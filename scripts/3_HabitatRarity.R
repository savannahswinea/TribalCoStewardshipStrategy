# The purpose of this script is to calculate the rarity 
# Of each habitat in the Area of Interest

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(terra)
library(here)

here::i_am("scripts/3_HabitatRarity.R")

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

# Habitat Rarity in the Area of Interest
# __________________________________________________________________________________________________________________

# Each habitat raster has only values of 0/1 (absence/presence)
# Taking the mean tells us the proportion of cells with that habitat present
# We will use the habitat presence to develop an idea of which habitats 
# Are common vs. rare

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
means <- sapply(habitats, function(x) {
  global(x, "mean", na.rm = TRUE)[1, 1]
})

log <- log(means)

# Normalize to range 0–1 using min-max scaling
normalized <- (log - min(log)) / (max(log) - min(log))

# Create data frame
habitatRarity <- data.frame(
  Habitat = names(means),
  NormalizedRarity = normalized,
  row.names = NULL
)

# Write to CSV
write.csv(habitatRarity,
          "HabitatRarity_Normalized.csv",
          row.names = FALSE)
