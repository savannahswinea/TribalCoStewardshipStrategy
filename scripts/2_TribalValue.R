# The purpose of this script is to understand tribal value of habitats
# Using survey data collected from a tribal audience

# Author: Savannah Swinea
# Contact: savannah.swinea@gmail.com

# Getting Started
# __________________________________________________________________________________________________________________

# Open necessary packages
library(here)
library(terra)
library(dplyr)
library(ggplot2)

here::i_am("scripts/2_TribalValue.R")

# What you need

# Survey data
survey <- read.csv("Survey_Data.csv")

# Tribal Habitat Value from Survey Data
# __________________________________________________________________________________________________________________

# We need to be able to make fair comparisons across habitats
# In the survey exercise, points will be allocated to a different number of habitats
# Depending on what habitat groups the tribe develops
# This workflow calculates a standardized value that takes this into account

# This must be edited to reflect your list of habitats
water_habitat_list <- c("HeadwatersandCreeks", "MediumRivers", "LakesandReservoirs")
land_habitat_list <- c("Forests", "Grasslands", "Shrublands")
landform_list <- c("Cliffs", "UpperSlopes", "SideSlopes", "Flats")

# This may also need to be edited to reflect habitat groupings
groups <- list(
  WaterHabitat = water_habitat_list,
  LandHabitat = land_habitat_list,
  Landform = landform_list
)

# Function to calculate a standardized mean for each habitat group
calculate_mean <- function(cols, group_name, data) {
  
  # Expected mean = 100 points divided by the number of habitats in the question
  # In essence: how points would be allocated equally among habitats
  expected_mean <- 100 / length(cols)
  
  # Mean for each habitat from survey data
  means <- colMeans(data[, cols], na.rm = TRUE)
  
  data.frame(
    Group = group_name,
    Habitat = names(means),
    Mean = means,
    ExpectedMean = expected_mean,
    
    # Standardized mean: how different is the survey mean from the expected mean?
    StandardizedMean = means / expected_mean,
    row.names = NULL
  )
}

# Apply the calculate_mean function to all habitat groups
tribalValue <- do.call(
  rbind,
  Map(calculate_mean,
      cols = groups,
      group_name = names(groups),
      MoreArgs = list(data = survey))
)

# Normalize standardized means across habitats from 0 to 1
tribalValue$NormalizedTribalValue <-
  (tribalValue$StandardizedMean - min(tribalValue$StandardizedMean)) /
  (max(tribalValue$StandardizedMean) - min(tribalValue$StandardizedMean))

# Write the resulting information to a .csv file
write.csv(
  tribalValue[, c("Habitat", "NormalizedTribalValue")],
  "TribalValue_Normalized.csv",
  row.names = FALSE
)




