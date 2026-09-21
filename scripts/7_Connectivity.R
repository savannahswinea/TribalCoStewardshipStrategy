# The purpose of this script is to develop a raster
# repesenting connectivity constrained to the Area of Interest

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(here)
library(terra)
library(dplyr)

here::i_am("scripts/7_Connectivity.R")

# What you need

# Read in the proximity raster made in 5_Proximity.R
proximity <- rast(here("outputs", "Proximity_Normalized.tif"))

# Connectivity and Climate Flow (Continuous)
# From The Center for Resilient Conservation Science (CRCS) at The Nature Conservancy
# Link: https://geospatial.tnc.org/maps/TNC::connectivity-and-climate-flow-continuous/about
# Once downloaded and extracted, place the folder inside the project folder

#connectivity_raw <- rast("C:/Users/sswinea/Documents/ArcGIS/Projects/EBCI/Connectivity_and_Climate_Flow_Raw/Connectivity_and_Climate_Flow_Raw/c_flow_w2w")
connectivity_raw <- rast("Connectivity_and_Climate_Flow_Raw/Connectivity_and_Climate_Flow_Raw/c_flow_w2w")

plot(connectivity_raw)

# Check if the coordinate reference systems match
crs(proximity) == crs(connectivity_raw)

# If not, project connectivity to match Area of Interest
connectivity_proj <- project(connectivity_raw, crs(proximity))

# Crop connectivity to the Area of Interest
connectivity_crop <- crop(connectivity_proj, proximity)

# The connectivity raster is resampled to match the other rasters
connectivity_resampled <- resample(connectivity_crop, proximity)

# One more step to constrain the raster to the Area of Interest
connectivity_mask <- mask(connectivity_resampled, proximity)

plot(connectivity_mask)

# Normalizing connectivity from 0 to 1
connectivity_min <- global(connectivity_mask, "min", na.rm = TRUE)[1,1]
connectivity_max <- global(connectivity_mask, "max", na.rm = TRUE)[1,1]

connectivity_normalized <- app(
  connectivity_mask,
  fun = function(x) (x - connectivity_min) / (connectivity_max - connectivity_min)
)

plot(connectivity_normalized)

# Lakes do not have connectivity values in the original raster, so I'm assigning them 0s
# So it gets carried forward in subsequent analysis
connectivity_normalized[is.na(connectivity_normalized)] <- 0
connectivity_normalized <- mask(connectivity_normalized, proximity)

plot(connectivity_normalized)

writeRaster(connectivity_normalized, filename = here("outputs", "Connectivity_Normalized.tif"), overwrite = TRUE)
