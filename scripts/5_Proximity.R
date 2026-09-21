# The purpose of this script is to assign cells in the Area of Interest
# Scores based on how close (proximate) they are to tribal lands
# This objective will only be relevant for tribes that have lands that they administer

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(terra)
library(osrm)
library(sf)
library(ggplot2)

here::i_am("scripts/5_Proximity.R")

# What you need

# Read in tribal boundary shapefile
tribe <- st_read("tribe/tribe.shp")

# Read in Area of Interest shapefile
aoi <- st_read("AOI/AOI.shp")

# Check that the coordinate reference systems match
crs(aoi) == crs(tribe)

# Proximity to Tribal Lands
# __________________________________________________________________________________________________________________

# Find the center point of the tribal boundary
center <- st_centroid(tribe)

# This function calculates isochrones, which are polygons that
# encompass the distance that can be traveled in a certain amount of drive time
# Here we make isochrones at 60 minutes (1 hour) and 120 minutes (2 hours)
# The number of isochrones to produce depends on how big the Area of Interest is
# And the drive times that are meaningful to the tribe
drive <- osrmIsochrone(center, breaks = seq(from = 0, to = 120, by = 60))

# Add drive time description
drive$drive_times <- factor(paste(drive$isomin, "to", drive$isomax, "min"))

# Plot the isochrones against tribal lands and the Area of Interest
ggplot() +
  geom_sf(data = drive) +
  geom_sf(data = aoi) +
  geom_sf(data = tribe) +
  coord_sf()

# Read in an existing habitat raster that we can use as a template
template <- rast(here("outputs", "Forests.tif"))
values(template) <- NA

# Add the values that correspond to proximity
# Areas within a 1 hour drive get a value of 1 (most proximate)
# Areas that are between 1 and 2 hours away get a value of 0 (least proximate)
drive$value <- c(1, 0)

# Double check that the drive time and value match correctly
drive[, c("drive_times", "value")]

# Map those values to the template raster
Proximity <- rasterize(
  drive,
  template,
  field = "value",
  background = NA
)

# Constraining to the Area of Interest
Proximity_mask <- mask(Proximity, aoi)

plot(Proximity_mask)

writeRaster(Proximity_mask, filename = here("outputs", "Proximity_Normalized.tif"), overwrite = TRUE)

  