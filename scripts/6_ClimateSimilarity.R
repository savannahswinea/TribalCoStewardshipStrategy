# The purpose of this script is to calculate where future climate
# will be similar to what is currently experienced on tribal lands
# This objective will only be relevant for tribes that have lands that they administer

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(here)
library(terra)
library(dplyr)
library(sf)

here::i_am("scripts/6_ClimateSimilarity.R")

# What you need

# Read in tribal boundary shapefile
tribe <- st_read("tribe/tribe.shp")

# Read in Area of Interest shapefile
aoi <- st_read("AOI/AOI.shp")

# Check that the coordinate reference systems match
crs(aoi) == crs(tribe)

# Read in the proximity raster made in 5_Proximity.R
proximity <- rast(here("outputs", "Proximity_Normalized.tif"))

# Climate Similarity to Current Tribal Lands
# __________________________________________________________________________________________________________________

clim_hist_rast <- rast(here("data", "Climate_Raster_Historical.tif"))
clim_proj_45_rast <- rast(here("data", "Climate_Raster_Projected_RCP45.tif"))
clim_proj_85_rast <- rast(here("data", "Climate_Raster_Projected_RCP85.tif"))

clim_hist_rast_proj <- project(clim_hist_rast, crs(aoi))
clim_proj_45_rast_proj <- project(clim_proj_45_rast, crs(aoi))
clim_proj_85_rast_proj <- project(clim_proj_85_rast, crs(aoi))

clim_hist_crop <- crop(clim_hist_rast_proj, tribe)
clim_hist_tribe <- mask(clim_hist_crop, tribe)

clim_proj_45_crop <- crop(clim_proj_45_rast_proj, aoi)
clim_proj_45_aoi <- mask(clim_proj_45_crop, aoi)

clim_proj_85_crop <- crop(clim_proj_85_rast_proj, aoi)
clim_proj_85_aoi <- mask(clim_proj_85_crop, aoi)

clim_hist_tribe_df <- as.data.frame(clim_hist_tribe, xy = TRUE)
clim_proj_45_aoi_df <- as.data.frame(clim_proj_45_aoi, xy = TRUE)
clim_proj_85_aoi_df <- as.data.frame(clim_proj_85_aoi, xy = TRUE)

clim_hist_tribe_center <- clim_hist_tribe_df %>%
  select(-c(x, y)) %>%
  colMeans()

# Isolate climate data
clim_vars45 <- clim_proj_45_aoi_df[, !(names(clim_proj_45_aoi_df) %in% c("x", "y"))]
clim_vars85 <- clim_proj_85_aoi_df[, !(names(clim_proj_85_aoi_df) %in% c("x", "y"))]

# Compute covariance matrix
cov_mat45 <- cov(clim_vars45, use = "complete.obs")
cov_mat85 <- cov(clim_vars85, use = "complete.obs")

# Calculate Mahalanobis distance
md45 <- mahalanobis(clim_vars45, center = clim_hist_tribe_center, cov = cov_mat45)
md85 <- mahalanobis(clim_vars85, center = clim_hist_tribe_center, cov = cov_mat85)

# Bind back to original data
clim_proj_45_aoi_df$mahal_dist45 <- md45
clim_proj_85_aoi_df$mahal_dist85 <- md85

# Calculating an average Mahalanobis score across RCP4.5 and RCP8.5
# And then normalizing those from 0 to 1
climateSimilarity <- clim_proj_45_aoi_df %>%
  left_join(clim_proj_85_aoi_df, by = c("x", "y")) %>%
  select(c(x, y, mahal_dist45, mahal_dist85)) %>%
  mutate(mahal_dist = (mahal_dist45 + mahal_dist85) / 2) %>%
  mutate(ClimateSimilarityNormalized = 1 - 
           (mahal_dist - min(mahal_dist)) / 
           (max(mahal_dist) - min(mahal_dist))) %>%
  select(x, y, ClimateSimilarityNormalized)

climateSimilarity_rast <- rast(climateSimilarity, type = 'xyz', crs = crs(aoi))

# Resample so that the extent will match up with the other rasters
climateSimilarity_rast_resampled <- resample(climateSimilarity_rast, proximity)

writeRaster(climateSimilarity_rast_resampled, here("outputs", "ClimateSimilarity_Normalized.tif"), overwrite = TRUE)

            