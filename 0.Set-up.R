## ---------------------------
##
## Script name:
##
## Project:
##
## Purpose of script:
##
## Author: Andrei Wong Espejo
##
## Date Created: 2026-06-28
##
## Email: awonge01@student.bbk.ac.uk
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## Load required packages ----

library("here")

pkgs <- c(
  "tidyverse", "janitor", "sf", "xfun", "remotes", "patchwork",
  "data.table", "ggspatial", "wesanderson", "ggrepel",
  "ggbreak"
)

pak::pak(pkgs)

## Program Set-up ------------
options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2()

# Create directories
# dirs <- c("Output", "Figures", "Tables")
# lapply(dirs, dir.create)

# Modify gitignore

## Load required packages ----
require(sf)
require(tidyverse)

## Runs the following --------
# 1. Load and clean POI data
# 2. Applies zonal algebra
# 3. Implement spatial point data analysis
# 4. Constructs spatial aggregation measures
# 5. Exports spatial dataset

## 1. Load and clean POI data--------------------------------------

# check available layers from a geopackage
st_layers(here("Data", "poi_6378383.gpkg"))
# Driver: GPKG
# Available layers:
#   layer_name
# 1 Points of Interest 2015_12
# geometry_type features fields
# 1                 499853     31
# crs_name
# 1 OSGB36 / British National Grid

# Verified urpn no overflow

## 2. Applies zonal algebra--------------------------------------

# Extent from file: 485134.5578360225, 159350.5971887972 - 569030.7335019625, 207660.76212754718

london_bbox <- st_bbox(
  c(
    xmin = 503568, ymin = 155850,
    xmax = 561957, ymax = 200933
  ),
  crs = 27700
) # EPSG:27700 for British National Grid

poi_sf <- st_read(here("Data", "poi_6378383.gpkg"),
  wkt_filter = st_as_text(st_as_sfc(london_bbox))
)

# Add 2011 LSOA and OA boundaries: ----
## Get LSOA boundaries
# from: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
download.file(
  "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip",
  "london_boundaries.zip"
)

unzip("london_boundaries.zip",
  exdir = here("Data", "Boundaries"),
  overwrite = TRUE,
  junkpaths = TRUE # remove the defauklt path from the zip file
)

london <- read_sf(here("Data", "Boundaries", "LSOA_2011_London_gen_MHW.shp"))
london <- st_transform(london, 27700) # EPSG:27700 for British National Grid

# WARNING better use DIGIMAP boundaries

# Simplify shapes, slower but keeps polygon topology consistent vis a vis st_simplify
london2 <- rmapshaper::ms_simplify(london, keep = 0.05, keep_shapes = TRUE)

# Filter relevant points

# Food outlets
# 01 Accommodation, eating and drinking  01 Accommodation
# 09 Retail 47 Food, drink and multi item retail
# 02 Eating and drinking  0013 Cafes, snack bars and tea rooms 0020 Fish and chip shops
# 0018 Fast food and takeaway outlets 0034 Pubs, bars and inns 0019 Fast food delivery services 0043 Restaurants
#


# Social capital
# 35 Organisations  0445 Animal welfare organisations 0449 Political parties and related organisations
# 0816 Charitable organisations 0450 Religious organisations 0769 Community networks and projects 0447 Sports clubs and associations 0446 Fan clubs and associations 0452 Youth organisations 0448 Institutes and professional organisations

# Review positional accuracy


# 3. Implement spatial point data analysis

# # Construct co-location indicators using leslieColocationQuotientNew2011 and
# sadahiroMethodEvaluatingPoint2025 and sadahiroNewStatisticalMethod2025

# If time, replicate neural network embedding of niuDelineatingUrbanFunctional2021
