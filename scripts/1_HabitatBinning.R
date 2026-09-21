# The purpose of this script is to help users produce spatial habitat information
# That is relevant to a general audience and constrained to an Area of Interest

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(here)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

here::i_am("scripts/1_HabitatBinning.R")

# What you need

# Read in tribal boundary shapefile
tribe <- st_read("tribe/tribe.shp")

# Read in Area of Interest shapefile
aoi <- st_read("AOI/AOI.shp")

# Check that the coordinate reference systems match
crs(aoi) == crs(tribe)

# Plot to check that the Area of Interest looks right
ggplot() +
  geom_sf(data = aoi) +
  geom_sf(data = tribe) +
  coord_sf()

# Land Habitats
# __________________________________________________________________________________________________________________

# Read in land habitats raster
# Source: LANDFIRE Existing Vegetation Type
# Stable link: https://doi.org/10.5066/P1XVKXRL
# Link: https://www.landfire.gov/vegetation/nvc
# Once downloaded and extracted, place the folder inside the project folder
land <- rast("LF2024_EVT_CONUS/Tif/LF2024_EVT_CONUS.tif")

# Check that the coordinate reference systems match
crs(aoi) == crs(land)

# Project land habitats raster to match
land_proj <- project(land, crs(aoi))
rm(land)

# Crop and mask the land habitats to the AOI
land_crop <- crop(land_proj, aoi)
rm(land_proj)
land_mask <- mask(land_crop, aoi)
rm(land_crop)

# Plot the land habitats within the AOI
plot(land_mask)

# View the attributes of the land habitats raster
land_table <- cats(land_mask)[[1]]
View(land_table)

# The masked raster retains all US habitats, so we need to filter this list down
levels(land_mask)
vals_present <- unique(freq(land_mask)$value) # Identify the land habitats that occur in the AOI
rat_filtered <- land_table[land_table$EVT_NAME %in% vals_present, ] # Filter the table so it contains land habitats in the AOI
levels(land_mask) <- rat_filtered # Assign that filtered list back to the raster

# Now we can view all land habitats in the AOI
land_table_filt <- cats(land_mask)[[1]]
View(land_table_filt)

# This is where you will make individual decisions based on what is relevant to you
# Explore column EVT_NAME, which is the most specific names of the land habitats
land_table_filt$EVT_NAME
# Also explore EVT_PHYS and EVT_SBCLS which convey the vegetation physiology
unique(land_table_filt$EVT_PHYS)
unique(land_table_filt$EVT_SBCLS)

# We are going to filter out developed areas as they do not contain relevant habitats
# We will also ignore water at this stage because we will use a better water dataset later to decide our water habitat categories
# For the purposes of this demonstration, we will group land habitats as follows:

# Forest
forest_names <- land_table_filt %>%
  filter(EVT_SBCLS %in% c(
    "Deciduous closed tree canopy", "Deciduous open tree canopy", 
    "Evergreen closed tree canopy", "Evergreen open tree canopy",
    "Mixed evergreen-deciduous closed tree canopy", "Mixed evergreen-deciduous closed tree canopy"
  )) %>%
  select(Value, EVT_SBCLS) %>%
  mutate(habitat = "forest") %>%
  mutate(class = 1)
  
# Grassland
grassland_names <- land_table_filt %>%
  filter(EVT_SBCLS %in% c(
    "Annual Graminoid/Forb", "Perennial graminoid", 
    "Perennial graminoid grassland"
  )) %>%
select(Value, EVT_SBCLS) %>%
  mutate(habitat = "grassland") %>%
  mutate(class = 2)

# Shrubland
shrubland_names <- land_table_filt %>%
  filter(EVT_SBCLS %in% c(
    "Deciduous shrubland", "Mixed evergreen-deciduous shrubland"
  )) %>%
  select(Value, EVT_SBCLS) %>%
  mutate(habitat = "shrubland") %>%
  mutate(class = 3)

# Combine all of these new binned habitat names into one list
land_habitat_names <- rbind(forest_names, grassland_names, shrubland_names)

# Create a matrix that pairs habitat bins with the unique habitat value
land_reclass <- land_habitat_names %>%
  select(Value, class) %>%
  as.matrix()

# Re-classify the land habitat raster using the habitat bins
land_raster_reclass <- classify(land_mask, land_reclass, others = NA)
freq(land_raster_reclass)

# Re-assign the habitat bin names developed above back to the raster
levels(land_raster_reclass) <- data.frame(
  ID    = c(1, 2, 3),
  class = c(
    "Forest",
    "Grassland",
    "Shrubland"))

# Check that the new land habitats are mapping in the AOI as you would expect
plot(land_raster_reclass)

# Develop presence/absence rasters for each habitat bin
forest_presabs <- as.int(land_raster_reclass == 1) # Select the habitat
forest_presabs[is.na(forest_presabs)] <- 0 # Clean the result so that the raster has only 0s and 1s

# Plot to check your result
plot(forest_presabs)

# Repeat for the other habitat bins
grassland_presabs <- as.int(land_raster_reclass == 2) 
grassland_presabs[is.na(grassland_presabs)] <- 0
shrubland_presabs <- as.int(land_raster_reclass == 3) 
shrubland_presabs[is.na(shrubland_presabs)] <- 0

# Writing the land habitat presence/absence data to be used in future analysis
writeRaster(forest_presabs, filename = here("outputs", "Forests.tif"), overwrite = TRUE)
writeRaster(grassland_presabs, filename = here("outputs", "Grasslands.tif"), overwrite = TRUE)
writeRaster(shrubland_presabs, filename = here("outputs", "Shrublands.tif"), overwrite = TRUE)

# Landforms
# __________________________________________________________________________________________________________________
# These data are stored on Google Earth Engine
# Source: Conservation Science Partners
# Link: https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_US_landforms#bands
landforms <- rast(here("data", "landforms", "landforms.tif"))

# Because we are reading in a continuous raster, we'll keep the original coordinate system to do our classification
aoi_vect <- vect(aoi)
aoi_vect <- project(aoi_vect, crs(landforms))
landforms_mask <- crop(landforms, aoi_vect) |> mask(aoi_vect)

# These data do not have attributes directly associated, but the values correspond to the following landforms:
# These are described in a publication (link: https://doi.org/10.1371/journal.pone.0143619)
# 11: Peak/ridge warm
# 12: Peak/ridge
# 13: Peak/ridge cool
# 14: Mountain/divide
# 15: Cliff
# 21: Upper slope warm
# 22: Upper slope neutral
# 23: Upper slope cool
# 24: Upper slope flat
# 31: Lower slope warm
# 32: Lower slope neutral
# 33: Lower slope cool
# 34: Lower slope flat
# 41: Valley
# 42: Valley (narrow)

# We have chosen to bin these landforms into four unique categories (in order):
# Cliffs and steep slopes, upper slopes, side slopes and coves, and flats

# This is the reclassification matrix
rcl <- matrix(c(
  10, 15, 1,
  20, 24, 2,
  30, 34, 3,
  40, 42, 4
), ncol = 3, byrow = TRUE)

# Reclassify the landforms into the bins
landforms_reclass <- classify(landforms_mask, rcl)

# Assign landform names to those bins
levels(landforms_reclass) <- data.frame(
  ID    = c(1, 2, 3, 4),
  class = c(
    "Cliffs and steep slopes",
    "Upper slopes",
    "Side slopes and coves",
    "Flats"
  )
)

# Project the landform raster so it matches the aoi shapefile
landforms_proj <- project(landforms_reclass, crs(aoi))

# When we project the dimensions don't align perfectly, so this resampling aligns them
landforms_resampled <- resample(landforms_proj, land_mask, method = "near")
plot(landforms_resampled)

# Develop presence/absence rasters for each habitat bin
cliffs_presabs <- as.int(landforms_resampled == 1) # Select the habitat
cliffs_presabs[is.na(cliffs_presabs)] <- 0 # Clean the result so that the raster has only 0s and 1s

# Plot to check your result
plot(cliffs_presabs)

# Repeat for the other habitat bins
upperslopes_presabs <- as.int(landforms_resampled == 2) 
upperslopes_presabs[is.na(upperslopes_presabs)] <- 0
sideslopes_presabs <- as.int(landforms_resampled == 3) 
sideslopes_presabs[is.na(sideslopes_presabs)] <- 0
flats_presabs <- as.int(landforms_resampled == 4) 
flats_presabs[is.na(flats_presabs)] <- 0

# Writing the landform presence/absence data to be used in future analysis
writeRaster(cliffs_presabs, filename = here("outputs", "Cliffs.tif"), overwrite = TRUE)
writeRaster(upperslopes_presabs, filename = here("outputs", "UpperSlopes.tif"), overwrite = TRUE)
writeRaster(sideslopes_presabs, filename = here("outputs", "SideSlopes.tif"), overwrite = TRUE)
writeRaster(flats_presabs, filename = here("outputs", "Flats.tif"), overwrite = TRUE)

# Water Habitats
# __________________________________________________________________________________________________________________
# Source: National Hydrography Dataset, maintained by the EPA and USGS
# Link: https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#v2datamap
# Once downloaded and extracted, place the folder inside the project folder

nhd_path <- "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

nhd_flow <- st_read(nhd_path, layer = "NHDFlowline_Network") # national streams and rivers, line feature class
nhd_body <- st_read(nhd_path, layer = "NHDWaterbody") # national lakes and reservoirs, polygon feature class

# Make a projected version of the area of interest that matches the water data (for now)
aoi_water <- st_transform(aoi, st_crs(nhd_flow))

# Filtering out some invalid geometries (only like 50 out of 500,000)
nhd_body <- nhd_body %>% filter(st_is_valid(.))

# Crop water shapefiles to bounding boxes for faster computation
nhd_flow_bbox_aoi <- st_crop(nhd_flow, aoi_water)
sf_use_s2(FALSE)
nhd_body_bbox_aoi <- st_crop(nhd_body, aoi_water)
sf_use_s2(TRUE)
rm(nhd_flow, nhd_body)

# Clip water features to AOI
nhd_flow_bbox_aoi_clip <- st_intersection(nhd_flow_bbox_aoi, aoi_water)
nhd_body_bbox_aoi_clip <- st_intersection(nhd_body_bbox_aoi, aoi_water)
rm(nhd_flow_bbox_aoi, nhd_body_bbox_aoi)

# Join size classification data
# Source: McManamay and DeRolph 2019
# Link: https://doi.org/10.6084/m9.figshare.c.4233740
east_size <- read.csv(here("data", "Water_Size_Gradient", "East_SizeGradient.csv"))
lm_size   <- read.csv(here("data", "Water_Size_Gradient", "LM_SizeGradient.csv"))
um_size   <- read.csv(here("data", "Water_Size_Gradient", "UM_SizeGradient.csv"))
west_size <- read.csv(here("data", "Water_Size_Gradient", "West_SizeGradient.csv"))

size_all <- bind_rows(east_size, lm_size, um_size, west_size)
View(head(size_all))
rm(east_size, lm_size, um_size, west_size)

# What we are interested in is SizeClass
unique(size_all$SizeClass)

# In order of size:
# HW: Headwater
# SC: Small Creek
# LC: Large Creek
# SR: Small River
# MR: Medium River
# MS: Medium Stream
# LR: Large River
# GR: Great River

# We are binning these water habitats as follows:
size_all <- size_all %>%
  mutate(
    SurveyHabitat = case_when(
      SizeClass %in% c(
        "HW", "SC", "LC"
      ) ~ "Headwaters and Creeks",
      
      SizeClass %in% c(
        "SR", "MR", "MS"
      ) ~ "Medium Rivers",
      
      SizeClass %in% c(
        "LR", "GR"
      ) ~ "Large Rivers",
      
      TRUE ~ "Other"
    )
  )

# The water dataset uses a variable called COMID as a unique identifier
size_all$COMID <- as.integer(size_all$COMID)
nhd_flow_bbox_aoi_clip$COMID <- as.integer(nhd_flow_bbox_aoi_clip$COMID)

size_all_unique <- size_all %>%
  distinct(COMID, .keep_all = TRUE)

# Join the size classes to the water network data
nhd_flow_bbox_aoi_clip_joined <- nhd_flow_bbox_aoi_clip %>%
  left_join(size_all_unique, by = "COMID") %>%
  filter(SurveyHabitat != "Other")

# Filtering swamps and marshes out of the water bodies dataset so just lakes and reservoirs are left behind
nhd_body_bbox_aoi_clip_filt <- nhd_body_bbox_aoi_clip %>%
  filter(FTYPE != "SwampMarsh")

# Plot the distribution of rivers and water bodies
ggplot() +
  geom_sf(data = aoi, fill = "white", color = "black", alpha = 0.4) +
  geom_sf(data = nhd_flow_bbox_aoi_clip_joined, aes(color = SurveyHabitat, size = SurveyHabitat)) +
  scale_color_manual(values = c("Headwaters and Creeks" = "#deebf7",
                                "Medium Rivers"         = "lightblue3",
                                "Large Rivers"          = "royalblue")) +
  scale_size_manual(values = c("Headwaters and Creeks" = 0.1,
                               "Medium Rivers"         = 0.6,
                               "Large Rivers"          = 0.8)) +
  guides(size = "none") + 
  geom_sf(data = nhd_body_bbox_aoi_clip, color = "darkblue", size = 0.6) +
  theme_minimal() +
  theme(legend.position = "none")

# Lining up coordinate reference systems
nhd_flow_raster <- st_transform(nhd_flow_bbox_aoi_clip_joined, crs(land_mask))
nhd_body_raster <- st_transform(nhd_body_bbox_aoi_clip_filt, crs(land_mask))

water_hab_types <- unique(nhd_flow_raster$SurveyHabitat)

# Identify presence/absence of each stream type in raster pixels
binary_streams <- lapply(water_hab_types, function(h) {
  
  s <- nhd_flow_raster %>% filter(SurveyHabitat == h)
  
  r <- rasterize(
    vect(s),
    land_mask,
    field = 1,
    background = 0,
    touches = TRUE
  )
  
  names(r) <- paste0("stream_", h, "_bin")
  r
})

binary_streams <- rast(binary_streams)

plot(binary_streams)

# Identify presence/absence of water bodies in raster pixels
nhd_body_raster_bin <- rasterize(
  vect(nhd_body_raster),
  land_mask,
  field = 1,
  background = 0
)

names(nhd_body_raster_bin) <- "polygon_bin"
plot(nhd_body_raster_bin)

# Cleaning up layer names
names(binary_streams) <- c(
  "HeadwatersandCreeks",
  "MediumRivers"
)
names(nhd_body_raster_bin) <- "LakesandReservoirs"

# Grabbing all of the water habitats in one raster stack
water_bin_raster <- c(binary_streams, nhd_body_raster_bin)

# How many cells have multiple co-occurring water habitats?
water_richness <- app(water_bin_raster, fun = sum)
plot(water_richness)

water_bin_raster_crop <- crop(water_bin_raster, aoi)
water_bin_raster_mask <- mask(water_bin_raster_crop, aoi)

# Writing water habitat presence/absence data for use in future analysis
writeRaster(water_bin_raster$HeadwatersandCreeks, filename = here("outputs", "HeadwatersandCreeks.tif"), overwrite = TRUE)
writeRaster(water_bin_raster$MediumRivers, filename = here("outputs", "MediumRivers.tif"), overwrite = TRUE)
writeRaster(water_bin_raster$LakesandReservoirs, filename = here("outputs", "LakesandReservoirs.tif"), overwrite = TRUE)
writeRaster(water_richness, filename = here("outputs", "WaterHabitat_Richness.tif"), overwrite = TRUE)

# OPTIONAL

# Habitats in Three Dimensions
# __________________________________________________________________________________________________________________

# Slope at the same resolution as the land habitats
# Source: USGS GAP Analysis Program
# Link: https://doi.org/10.5066/F75D8QQF
# Once downloaded and extracted, place the folder inside the project folder
slope <- rast("National_Slope/National_Slope.img")

# Calculating a slope correction factor
# Why? The habitat data are represented in 2 dimensions, but the real world has 3 dimensions
# We want to represent the surface (3D) area of habitats, not the planimetric (2D) area
slope_correction <- 1 / cos(slope * (pi / 180))
# How to interpret this: a slope correction factor of 1 means the land is flat, anything above 1 has slope

slope_proj <- project(slope, crs(aoi))

# When we project the dimensions don't align perfectly, so this resampling aligns them
slope_resampled <- resample(slope_proj, land_mask, method = "near")

slope_crop <- crop(slope_resampled, aoi)
slope_correction <- mask(slope_crop, aoi)

# Multiplying water habitat presence/absence by slope correction factor
headwaterscreeks_presabs_3d <- water_bin_raster$HeadwatersandCreeks * slope_correction
mediumrivers_presabs_3d <- water_bin_raster$MediumRivers * slope_correction
water_body_3d <- water_bin_raster$LakesandReservoirs * slope_correction

# Multiplying land habitat presence/absence by slope correction factor
forest_presabs_3d <- forest_presabs * slope_correction
grassland_presabs_3d <- grassland_presabs * slope_correction
shrubland_presabs_3d <- shrubland_presabs * slope_correction

# Multiplying landform presence/absence by slope correction factor
cliffs_presabs_3d <- cliffs_presabs * slope_correction
upperslopes_presabs_3d <- upperslopes_presabs * slope_correction
sideslopes_presabs_3d <- sideslopes_presabs * slope_correction
flats_presabs_3d <- flats_presabs * slope_correction

writeRaster(headwaterscreeks_presabs_3d, filename = here("outputs", "HeadwatersandCreeks.tif"), overwrite = TRUE)
writeRaster(mediumrivers_presabs_3d, filename = here("outputs", "MediumRivers.tif"), overwrite = TRUE)
writeRaster(water_body_3d, filename = here("outputs", "LakesandReservoirs.tif"), overwrite = TRUE)

writeRaster(forest_presabs_3d, filename = here("outputs", "Forests.tif"), overwrite = TRUE)
writeRaster(grassland_presabs_3d, filename = here("outputs", "Grasslands.tif"), overwrite = TRUE)
writeRaster(shrubland_presabs_3d, filename = here("outputs", "Shrublands.tif"), overwrite = TRUE)

writeRaster(cliffs_presabs_3d, filename = here("outputs", "Cliffs.tif"), overwrite = TRUE)
writeRaster(upperslopes_presabs_3d, filename = here("outputs", "UpperSlopes.tif"), overwrite = TRUE)
writeRaster(sideslopes_presabs_3d, filename = here("outputs", "SideSlopes.tif"), overwrite = TRUE)
writeRaster(flats_presabs_3d, filename = here("outputs", "Flats.tif"), overwrite = TRUE)
