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

# Create directories
dirs <- c("Output", "Figures", "Tables")
lapply(dirs, dir.create)

# Modify gitignore

## Runs the following --------
# 1. Load and clean POI data
# 2. Applies zonal algebra
# 3. Implement spatial point data analysis
# 4. Applies spatial aggregation measures
# 5. Exports spatial dataset
