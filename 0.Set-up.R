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
# renv::init()
options(renv.config.pak.enabled = TRUE)

if (!require("here")) {

  renv::install("here"
  )
}


pkgs <- c(
  "tidyverse",
  "janitor",
  "sf",
  "xfun",
  "remotes",
  "patchwork",
  "data.table",
  "ggspatial",
  "wesanderson",
  "ggrepel",
  "ggbreak",
  "duckdb",
  "duckspatial"
)

renv::install(pkgs, verbose = TRUE)

## Load required packages ----
require(sf)
require(tidyverse)
require(duckdb)
require(duckspatial)
require(janitor)

## Program Set-up ------------
options(scipen = 100, digits = 4) # Prefer non-scientific notation
sf_use_s2()
# Default output has changed on v1.0.0:
#   duckspatial now returns lazy `duckspatial_df` (dbplyr) objects
# instead of `sf` objects.

# To restore the previous behaviour:
ddbs_options(mode = "sf")

# Create directories
# dirs <- c("Output", "Figures", "Tables")
# lapply(dirs, dir.create)

# Modify gitignore

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
    xmin = 503568,
    ymin = 155850,
    xmax = 561957,
    ymax = 200933
  ),
  crs = 27700
) # EPSG:27700 for British National Grid

poi_sf <- st_read(
  here("Data", "poi_6378383.gpkg"),
  wkt_filter = st_as_text(st_as_sfc(london_bbox))
)

# Add 2011 LSOA and OA boundaries: ----
## Get LSOA boundaries
# from: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
# options(timeout = max(600, getOption("timeout")))
#
# download.file(
#   url = "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip",
#   destfile = "london_boundaries.zip",
#   method = "libcurl",
#   mode = "wb"
# )
#
# unzip(
#   "london_boundaries.zip",
#   exdir = here("Data", "Boundaries"),
#   overwrite = TRUE,
#   junkpaths = TRUE # remove the irectory structure from the zip file
# )
# file.remove("london_boundaries.zip")

london <- read_sf(here("Data", "Boundaries", "LSOA_2011_London_gen_MHW.shp"))
london <- st_transform(london, 27700) # EPSG:27700 for British National Grid

# Simplify shapes, slower but keeps polygon topology consistent vis a vis st_simplify
london2 <- rmapshaper::ms_simplify(london, keep = 0.05, keep_shapes = TRUE)

# Filter relevant points

# Food outlets
# 01 Accommodation, eating and drinking
# 09 Retail 47 Food, drink and multi item retail
# 02 Eating and drinking  0013 Cafes, snack bars and tea rooms 0020 Fish and chip shops
# 0018 Fast food and takeaway outlets 0034 Pubs, bars and inns 0019 Fast food delivery services 0043 Restaurants

# Retail shadow index: independent specialty food stores (e.g., green grocers, Halal butchers, traditional markets)
# Group 09 = Retail.
# Category 47 = Food, drink and multi item retail.
# Then select the specific class, such as 0662 Butchers or 0669 Grocers, farm shops
# Also use keyword search in respective qualifier_type and qualifier_data columns

# Social capital
# 35 Organisations  0445 Animal welfare organisations 0449 Political parties and related organisations
# 0816 Charitable organisations 0450 Religious organisations 0769 Community networks and projects
# 0447 Sports clubs and associations 0446 Fan clubs and associations 0452 Youth organisations
# 0448 Institutes and professional organisations
require(duckdb)
require(duckspatial)

# con <- ddbs_create_conn("poi_boundaries.duckdb")
# ERROR!!! cause that path is inside OneDrive, the safest conclusion is that the extension
# directory location is the current blocker, not ddbs_create_conn()
# SOLUTION: Create directories not in Onedrive
dir.create("C:/duckdb_ext", showWarnings = FALSE, recursive = TRUE)
dir.create("C:/duckdb_data", showWarnings = FALSE, recursive = TRUE)

Sys.setenv(DUCKDB_EXTENSION_DIRECTORY = "C:/duckdb_ext")

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "C:/duckdb_data/poi_boundaries.duckdb")
DBI::dbExecute(con, "INSTALL spatial;")
DBI::dbExecute(con, "LOAD spatial;")

# write existing object into DuckDB, avoided sf as db reading of sf geometries unstabl path <- here("Data", "poi_6378383.gpkg")

# Cause in DuckDB, having a geometry-typed column is not enough if the table was
# created in a way that strips or bypasses the spatial type binding.
# binder error suggests DuckDB is not seeing poi.geom as a true GEOMETRY column
# at index-creation time, so build table with an explicit geometry cast
path <- here("Data", "poi_6378383.gpkg")

dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE poi AS
  SELECT *,
  geom::GEOMETRY AS geom
  FROM ST_Read('%s')
", path))
# [1] 499853

# Reviewing database
DBI::dbGetQuery(con, "PRAGMA table_info('poi')")

DBI::dbGetQuery(con, "
  SELECT
    COUNT(*) AS n_total,
    SUM(CASE WHEN ST_IsValid(geom) THEN 1 ELSE 0 END) AS n_valid,
    SUM(CASE WHEN NOT ST_IsValid(geom) THEN 1 ELSE 0 END) AS n_invalid
  FROM poi
")

DBI::dbGetQuery(con, "
  SELECT
    id,
    ST_GeometryType(geom) AS geom_type,
    ST_AsText(geom) AS wkt
  FROM poi
  LIMIT 10
")

DBI::dbGetQuery(con, "
  SELECT
    COUNT(*) AS n_total,
    SUM(CASE WHEN geom IS NULL THEN 1 ELSE 0 END) AS n_null_geom,
    COUNT(DISTINCT ST_AsText(geom)) AS n_unique_geoms
  FROM poi
")

DBI::dbGetQuery(con, "
  SELECT
    COUNT(*) AS n_mismatch
  FROM poi
  WHERE ABS(feature_easting - ST_X(geom)) > 0.001
     OR ABS(feature_northing - ST_Y(geom)) > 0.001
")

path <- here("Data", "Boundaries", "LSOA_2011_London_gen_MHW.shp")

dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE lsoa_boundaries AS
  SELECT *,
  geom::GEOMETRY AS geom
  FROM ST_Read('%s')
", path))

DBI::dbGetQuery(con, "PRAGMA table_info('lsoa_boundaries')")

# create R-tree index
dbExecute(con, "CREATE INDEX poi_rtree ON poi USING RTREE (geom_1)")

# Implement zonal algebra using is_within, is_touches and edge rule
dbExecute(con, "
CREATE OR REPLACE TABLE classified_poi AS
WITH candidates AS (
  SELECT
    p.id AS point_id,
    l.LSOA11CD AS lsoa_id,
    ST_Within(p.geom_1, l.geom_1) AS is_within,
    ST_Touches(p.geom_1, l.geom_1) AS is_touching
  FROM poi p
  LEFT JOIN lsoa_boundaries l
    ON ST_Intersects(p.geom_1, l.geom_1)
)
SELECT
  point_id,
  CASE
    WHEN MAX(CAST(is_within AS INTEGER)) = 1 THEN 'inside'
    WHEN MAX(CAST(is_touching AS INTEGER)) = 1 THEN 'boundary'
    ELSE 'outside'
  END AS point_class,
  MAX(CAST(is_within AS INTEGER)) AS is_within,
  MAX(CAST(is_touching AS INTEGER)) AS is_touching
FROM candidates
GROUP BY point_id
")

dbGetQuery(con, "
SELECT
  point_class,
  COUNT(*) AS n_points,
  SUM(is_within) AS total_is_within,
  SUM(is_touching) AS total_is_touching
FROM classified_poi GROUP BY point_class
ORDER BY point_class
")

# point_class n_points total_is_within total_is_touching
# 1      inside   369697          369697                 0
# 2     outside   130156              NA                NA

dbGetQuery(con, "SELECT * FROM classified_poi LIMIT 10") #  No edge cases

# Assigning each POI point ID to the polygon ID that contains it
dbExecute(con, "
  CREATE OR REPLACE TABLE points_with_polygons AS
  SELECT
  p.*,
  l.LSOA11CD AS LSOA11CD
  FROM poi p
  INNER JOIN lsoa_boundaries l
  ON ST_Within(p.geom_1, l.geom_1)
")

DBI::dbGetQuery(con, "PRAGMA table_info('points_with_polygons')")

# Extract table to R,
points_with_polygons <- DBI::dbGetQuery(con, "SELECT * FROM points_with_polygons")

dbGetQuery(con, "SHOW TABLES")

dbGetQuery(con, "SELECT COUNT(*) FROM points_with_polygons")
# count_star()
# 1       369697

dbDisconnect(con, shutdown = TRUE)
gc()

# Reattach geometry
points_sf <- left_join(points_with_polygons[, !names(points_with_polygons) %in% c("id", "geom", "geom_1")],
  london2[c("LSOA11CD", "geometry")],
  by = "LSOA11CD"
) |>
  st_as_sf()

table(st_is_valid(points_sf))
# TRUE
# 369697

sum(sf::st_is_empty(points_sf$geometry))
sum(is.na(points_sf$LSOA11CD))
sum(points_sf$LSOA11CD == "")

# Review positional accuracy
table(points_sf$pos_accuracy) # Flag 366 points with value 4 as they are
# positioned in the geographic locality

# 3. Implement spatial point data analysis

# # Construct co-location indicators using leslieColocationQuotientNew2011 and
# sadahiroMethodEvaluatingPoint2025 and sadahiroNewStatisticalMethod2025

# 3.1. Spatial co-location and association mining (Point-to-Point)
# Asymmetric Colocation Quotient (CLQ): Using the sfdep package in
# Test whether independent specialty food stores (e.g. ethnic related if found in data)
# tend to colocate with specific food outlets

# 3.2. Point segregation and inhomogeneity
# Point Segregation Analysis: Using the spatstat package
# Test whether there is spatial segregation between corporate commercial food outlets
# (e.g. fast-food delivery) and independent specialty stores (retail shadow index)

# Weighted Random Labeling: POI distributions are heavily biased by spatial inhomogeneity
# (zoning laws) and aspatial inhomogeneity (differences in store floor size or
# local LSOA socio-demographic characteristics) LESS IMPORTANT

# 3.3. Spatial Proportionality (POIs vs LSOA Census Populations)
# Test if food outlets or social capital organisations are distributed evenly in relation to the population

# If time, replicate neural network embedding of niuDelineatingUrbanFunctional2021
# 3.4. Semantic Delineation and Topic Modelling (LDA)
# Functional Area Delineation: train a Doc2Vec model to vectorize both POI classes ("words")
# and LSOAs ("documents") directly
