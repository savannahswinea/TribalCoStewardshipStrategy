#### README file for analyses associated with Swinea et al.

## Overview
This repository contains R scripts and data to assist tribes in reproducing analyses to understand co-stewardship priority areas.

## Contact
Savannah Swinea, PhD
Affiliation: Department of Applied Ecology, North Carolina State University
Email: sswinea@ncsu.edu

## citations
# Repository name
TribalCoStewardshipStrategy

# README title
R code and data for tribal co-stewardship analyses

# Zenodo title
Add later

# Manuscript citation
Add later

# Software citation
S.H. Swinea. 2026. R code and data for tribal co-stewardship analyses.

## Repository file structure includes:
- TribalCoStewardshipStrategy.Rproj: R project
- scripts: setup and analysis R scripts
- data: import data files to run R scripts
- AOI: hypothetical Area of Interest shapefile
- tribe: hypothetical tribal boundary shapefile
- outputs: landing place for data generated in analyses
- renv: R project management files maintained by the renv package

## repository files
# data folder
- landforms folder: landform raster and associated files
- Water_Size_Gradient: stream size classifications. Source: <a href="https://doi.org/10.1038/sdata.2019.17">https://doi.org/10.1038/sdata.2019.17</a>
- Climate_Raster_Historical.tif: Average seasonal (i.e., winter: December, January, February; spring: March, April, May; summer: June, July, August; fall: September, October, November) metrics for maximum and minimum monthly temperature and total precipitation during a historical period (1980-2019).
- Climate_Raster_Projected_RCP45.tif: Average seasonal (i.e., winter: December, January, February; spring: March, April, May; summer: June, July, August; fall: September, October, November) metrics for maximum and minimum monthly temperature and total precipitation during a projected period (2060-2099) under RCP 4.5.
- Climate_Raster_Projected_RCP85.tif: Average seasonal (i.e., winter: December, January, February; spring: March, April, May; summer: June, July, August; fall: September, October, November) metrics for maximum and minimum monthly temperature and total precipitation during a projected period (2060-2099) under RCP 8.5.

# scripts
- 0_Setup: Set up file to configure R packages.
- 1_HabitatBinning: Bin land, landform, and water habitats so they are understandable for a tribal audience and generate habitat rasters.
- 2_TribalValue: Calculate average tribal value of habitats using survey data.
- 3_HabitatRarity: Calculate rarity of each habitat in the Area of Interest.
- 4_HabitatAvailability: Calculate availability of each habitat, or how abundant a habitat is on tribal lands compared to the Area of Interest.
- 5_Proximity: Generate a raster representing the value of areas being within specific driving distances of the tribe.
- 6_ClimateSimilarity: Generate a raster representing the value of areas having similar future climates to the current climate of the tribe.
- 7_Connectivity: Generate a raster representing the value of well-connected areas in the landscape.
- 8_Utility: Combine all the objectives generated in scripts #2-7 to represent utility, or where co-stewardship priorities exist in the Area of Interest.

# outputs
- The contents of this folder are generated using a hypothetical tribal boundary and a hypothetical Area of Interest. As you work through the scripts, these files will be overwritten, but they are included in case you have trouble executing the scripts from start to finish.

## Software requirements
- R version: R version 4.4.2 (2024-10-31 ucrt)
- R Studio version: 2026.08.1
- Required R packages are documented in `renv.lock`.

## Reproducibility:
- Open "TribalCoStewardshipStrategy.Rproj" to establish the project root
- Run "0_Setup" once for R package management
- Run scripts #1-8 to reproduce analyses

## License
- MIT License

