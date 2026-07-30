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
  renv::install("here")
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
  "duckspatial",
  "ellmer"
)

renv::install(pkgs, verbose = TRUE)

## Load required packages ----
require(sf)
require(tidyverse)
require(duckdb)
require(duckspatial)
require(janitor)
require(tidyverse)
require(ellmer)
require(purrr)
require(jsonlite)
require(glue)

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

# Food outlets,
# Groupname [01], Categories [02],
# Classname [0013 Cafes, snack bars and tea rooms [X]
# 0018 Fast food and takeaway outlets
# 0019 Fast food delivery services
# 0020 Fish and chip shops
# 0034 Pubs, bars and inns
# 0043 Restaurants]

list(unique(poi_sf$qualifier_data[poi_sf$pointx_class == "01020043"]))
# [[1]]
# [1] "Italian Restaurant"          "Indian Restaurant"
# [3] "French Restaurant"           "Chinese Restaurant"
# [5] "Pizza Restaurant"            "English Restaurant"
# [7] "Restaurant"                  "Seafood Restaurant"
# [9] "Lebanese Restaurant"         "Thai Restaurant"
# [11] "Pub Food Restaurant"         "American Restaurant"
# [13] "Japanese Restaurant"         "Korean Restaurant"
# [15] "Turkish Restaurant"          "International Restaurant"
# [17] "British Restaurant"          "Brasserie Restaurant"
# [19] "Vegetarian Restaurant"       "Other Restaurant"
# [21] "Roadside"                    "Afghan Restaurant"
# [23] "Ethiopian Restaurant"        "Mediterranean Restaurant"
# [25] "Nepalese Restaurant"         "Spanish Restaurant"
# [27] "Oriental Restaurant"         "Portuguese Restaurant"
# [29] "Caribbean Restaurant"        "Mexican/Tex Mex Restaurant"
# [31] "Vietnamese Restaurant"       "European Restaurant"
# [33] "Mexican Restaurant"          "Greek Restaurant"
# [35] "Pizzeria Restaurant"         "Brazilian Restaurant"
# [37] "Belgian Restaurant"          "Asian Restaurant"
# [39] "Moroccan Restaurant"         "African Restaurant"
# [41] "Argentinian Restaurant"      "Austrian Restaurant"
# [43] "Middle Eastern Restaurant"   "South American Restaurant"
# [45] "Iraqi Restaurant"            "Iranian Restaurant"
# [47] "Restaurant Cruise"           "Egyptian Restaurant"
# [49] "Continental Restaurant"      "Malaysian Restaurant"
# [51] "Russian Restaurant"          "Indian/Asian Restaurant"
# [53] "Cuban Restaurant"            "Pakistani Restaurant"
# [55] "Creperie Restaurant"         "Swedish Restaurant"
# [57] "Bangladeshi Restaurant"      "Polish Restaurant"
# [59] "German Restaurant"           "Tunisian Restaurant"
# [61] "Philippine Restaurant"       "Eastern European Restaurant"
# [63] "Motorway Services"           "Jamaican Restaurant"
# [65] "Scottish Restaurant"         "Kosher Restaurant"
# [67] "Mauritian Restaurant"        "Mongolian Restaurant"
# [69] "Indonesian Restaurant"       "Colombian Restaurant"

# Logic
# PointX class > food candidate > food typology > brand refinement > cuisine/context refinement> measure flags
# Use of lookup tables to recover the full 8-digit PointX
# USe a set of alternative classification according to food environment lit
# USe of review_reason facilitates audit
# Matain foodservice and food retail separate
# food_outlet_base now reflects PointX's original coding; retail_format_typology
# reflects the corrected format after brand and express refinement.

# Single canonical hierarchy (poi_lookup).
# Classification mapping moved into a lookup table.
# Explicit classification ontology and more deterministic rules, added classification_version
# Traceability retained through pointx_class adn, trace_primary and trace_secondary for classification
# Food outlet + missing typology → review
# Food outlet + fully classified → not review
# Non-food outlet → not review

# Three layers:
#
# A) Core POI enrichment table:
# class_code, food_outlet_base, brand_match, UPRN, TOID, positional_accuracy, verified_address, site_conext.
# B) Measure lookup table:
# food_outlet_base, rfei_num_flag, rfei_den_flag, mrfei_healthy_flag, mrfei_less_health_flag.
# C) Context lookup table:
# brand_key, name cues, qualifier cues, format_refine.

# Useful functions, leave it here for visual inspection
read_poi_lookup <- function(filename) {
  read_delim(
    here("Data", "docs", filename),
    delim = "|",
    quote = "\"",
    trim_ws = TRUE,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    clean_names()
}

norm_chr <- function(x) {
  x %>%
    as.character() %>%
    replace_na("") %>%
    str_to_lower() %>%
    str_squish()
}

# CHANGED: replaced single classification_fields vector with domain-specific required field sets
required_fields_common <- c(
  "food_domain",
  "food_outlet_base",
  "public_health_typology",
  "public_health_subtype",
  "sociocultural_typology",
  "formality_typology"
)

# CHANGED: explicit required fields for food retail/foodservice QC
required_fields_food_retail <- c(
  required_fields_common,
  "retail_format_typology"
)

required_fields_foodservice <- c(
  required_fields_common,
  "foodservice_format_typology"
)

CLASSIFICATION_VERSION <- "2026-07-11"

brand_lookup <- tribble(
  ~brand_key, ~brand_group, ~format_refine,
  "aldi", "hard_discounter", "hard_discounter",
  "lidl", "hard_discounter", "hard_discounter",
  # CHANGED: frozen/value specialists separated from hard discounters
  "iceland", "frozen_value_specialist", "frozen_value_specialist",
  "farmfoods", "frozen_value_specialist", "frozen_value_specialist",
  "heron", "frozen_value_specialist", "frozen_value_specialist",
  "the food warehouse", "frozen_value_specialist", "frozen_value_specialist",
  "waitrose", "premium", "premium_convenience",
  # CHANGED: added wholesaler
  "marks & spencer simply food", "premium", "premium_convenience",
  "marks and spencer simply food", "premium", "premium_convenience",
  "m&s simply food", "premium", "premium_convenience",
  "marks & spencer", "premium", "premium_convenience",
  "m&s", "premium", "premium_convenience",
  "m & s simply food", "premium", "premium_convenience",
  "marks and spencer", "premium", "premium_convenience",
  "whole foods", "premium", "premium_convenience",
  "planet organic", "premium", "premium_convenience",
  "tesco", "mainstream_chain", "standard_supermarket_chain",
  "sainsbury's", "mainstream_chain", "standard_supermarket_chain",
  "sainsburys", "mainstream_chain", "standard_supermarket_chain",
  "asda", "mainstream_chain", "standard_supermarket_chain",
  "morrisons", "mainstream_chain", "standard_supermarket_chain",
  "co-op", "mainstream_chain", "standard_supermarket_chain",
  "coop", "mainstream_chain", "standard_supermarket_chain",
  "co-operative", "mainstream_chain", "standard_supermarket_chain",
  "spar", "symbol_group", "symbol_group_convenience",
  "budgens", "symbol_group", "symbol_group_convenience",
  "tfc", "mainstream_chain", "standard_supermarket_chain",
  "costcutter", "soft_franchise", "symbol_group_convenience",
  "londis", "soft_franchise", "symbol_group_convenience",
  "nisa", "soft_franchise", "symbol_group_convenience",
  "premier", "soft_franchise", "symbol_group_convenience",
  "best-one", "soft_franchise", "symbol_group_convenience",
  "mace", "soft_franchise", "symbol_group_convenience",
  "mccolls", "soft_franchise", "symbol_group_convenience",
  "day today", "soft_franchise", "symbol_group_convenience",
  "day-today", "soft_franchise", "symbol_group_convenience",
  "lifestyle express", "soft_franchise", "symbol_group_convenience",
  "simply fresh", "soft_franchise", "symbol_group_convenience",
  # CHANGED: forecourt and travel retained as format/context cues, not price cues
  "shell select", "forecourt", "forecourt_convenience",
  "esso", "forecourt", "forecourt_convenience",
  "on the run", "forecourt", "forecourt_convenience",
  "bp connect", "forecourt", "forecourt_convenience",
  "texaco", "forecourt", "forecourt_convenience",
  "murco", "forecourt", "forecourt_convenience",
  "wh smith", "travel_retail", "travel_convenience",
  "whistlestop", "travel_retail", "travel_convenience",
  "relay", "travel_retail", "travel_convenience",
  # CHANGED: added wholesaler
  "costco", "wholesaler", "bulk_wholesale",
  "makro", "wholesaler", "bulk_wholesale"
) %>%
  # CHANGED: enforce unique brand keys to avoid row multiplication in joins
  distinct(brand_key, .keep_all = TRUE)

poi_groups <- read_poi_lookup("POI GROUPS.txt") %>%
  transmute(
    group_number = str_pad(group_number, 2, pad = "0"),
    group_description
  )

poi_categories <- read_poi_lookup("POI CATEGORIES.txt") %>%
  transmute(
    category_number = str_pad(category_number, 2, pad = "0"),
    group_number = str_pad(group_number_foreign_key, 2, pad = "0"),
    category_description
  )

poi_classes <- read_poi_lookup("POI_CLASSIFICATIONS.txt") %>%
  transmute(
    class_number = str_pad(class_number, 4, pad = "0"),
    category_number = str_pad(category_number_foreign_key, 2, pad = "0"),
    poi_class_desc = classification_description
  )

poi_lookup <- poi_classes %>%
  left_join(poi_categories, by = "category_number") %>%
  left_join(poi_groups, by = "group_number") %>%
  mutate(
    class_code = paste0(group_number, category_number, class_number)
  ) %>%
  dplyr::select(
    class_code,
    group_number,
    group_description,
    category_number,
    category_description,
    class_number,
    poi_class_desc
  )

# CHANGED: explicit food_domain added so eating/drinking and food retail are separate throughout
food_typology_lookup <- tribble(
  ~class_code, ~food_domain, ~food_outlet_base, ~public_health_typology, ~public_health_subtype,
  "01020013", "foodservice", "cafe_snackbar_tearoom", "mixed_or_context_dependent", "cafe_snack_bar_tea_room",
  "01020018", "foodservice", "fastfood_takeaway", "unhealthy_or_risk", "fast_food_takeaway",
  "01020019", "foodservice", "fastfood_delivery_service", "unhealthy_or_risk", "fast_food_delivery_service",
  "01020020", "foodservice", "fish_chip_shop", "unhealthy_or_risk", "fish_and_chip_shop",
  # CHANGED: pubs/bars/inns added to food candidate lookup
  "01020034", "foodservice", "pub_bar_inn", "mixed_or_context_dependent", "pub_bar_inn",
  "01020043", "foodservice", "restaurant", "mixed_or_context_dependent", "restaurant",
  "09470661", "food_retail", "bakery", "mixed_or_context_dependent", "bakery",
  "09470662", "food_retail", "butcher", "healthy_or_supportive_fresh_food", "butcher",
  "09470663", "food_retail", "confectioner", "unhealthy_or_risk", "confectioner",
  "09470665", "food_retail", "delicatessen", "mixed_or_context_dependent", "delicatessen",
  "09470666", "food_retail", "fishmonger", "healthy_or_supportive_fresh_food", "fishmonger",
  "09470667", "food_retail", "frozen_food_retail", "mixed_or_context_dependent", "frozen_food_shop",
  "09470669", "food_retail", "grocer_farmshop_pyo", "healthy_or_supportive_fresh_food", "grocer_farmshop",
  # CHANGED: herbs/spices added as food candidate
  "09470670", "food_retail", "herbs_spices", "mixed_or_context_dependent", "herbs_spices",
  "09470671", "food_retail", "offlicence_alcohol_retail", "unhealthy_or_risk", "off_licence",
  "09470672", "food_retail", "organic_health_specialist", "healthy_or_supportive_fresh_food", "organic_health_food_shop",
  "09470699", "food_retail", "convenience_or_independent_supermarket", "mixed_or_context_dependent", "convenience_store_or_independent_supermarket",
  # CHANGED: livestock markets added as food candidate
  "09470703", "food_retail", "livestock_market", "mixed_or_context_dependent", "livestock_market",
  "09470705", "food_retail", "market", "potentially_supportive_fresh_food", "market",
  "09470768", "food_retail", "cash_and_carry", "mixed_or_context_dependent", "cash_and_carry",
  # CHANGED: tea/coffee merchants added as food candidate
  "09470798", "food_retail", "tea_coffee_merchant", "mixed_or_context_dependent", "tea_coffee_merchant",
  "09470819", "food_retail", "supermarket_chain", "healthy_or_supportive_fresh_food", "supermarket"
)

poi_food_lookup <- poi_lookup %>%
  left_join(food_typology_lookup, by = "class_code")

# CHANGED: explicit food candidate universe, separate from successful food typology mapping
food_candidate_lookup <- tribble(
  ~class_code,
  "01020013",
  "01020018",
  "01020019",
  "01020020",
  "01020034",
  "01020043",
  "09470661",
  "09470662",
  "09470663",
  "09470665",
  "09470666",
  "09470667",
  "09470669",
  "09470670",
  "09470671",
  "09470672",
  "09470699",
  "09470703",
  "09470705",
  "09470768",
  "09470798",
  "09470819"
)

## Match brand names
build_brand_patterns <- function(brand_keys) {
  paste0(
    "\\b",
    str_replace_all(brand_keys, "([[:punct:]])", "\\\\\\1"),
    "\\b"
  )
}

# CHANGED: longer brand strings are matched first so more specific labels take precedence
get_ordered_brand_keys <- function(brand_lookup) {
  brand_lookup %>%
    mutate(brand_key_nchar = str_length(brand_key)) %>%
    arrange(desc(brand_key_nchar), brand_key) %>%
    pull(brand_key)
}

match_brand_vectorised <- function(brand_std_vec, brand_keys) {
  patterns <- build_brand_patterns(brand_keys)
  result   <- rep(NA_character_, length(brand_std_vec))

  # CHANGED: first matching brand wins after ordering by descending brand length
  for (i in seq_along(brand_keys)) {
    unmatched <- is.na(result)
    if (!any(unmatched)) break
    hits <- str_detect(
      brand_std_vec,
      regex(patterns[i], ignore_case = TRUE)
    )
    result[unmatched & hits] <- brand_keys[i]
  }

  result
}

# CHANGED: supermarket and convenience/small supermarket brand matching only
assign_brand_matches <- function(
  df,
  brand_keys,
  supermarket_classes
) {

  supermarket_idx <-
    df$food_outlet_base %in% supermarket_classes

  df$matched_brand_key <- NA_character_

  df$matched_brand_key[supermarket_idx] <-
    match_brand_vectorised(
      df$brand_std[supermarket_idx],
      brand_keys
    )

  df
}

# PROBLEM: produces false positives "aldi local" vs "idealdistributors"
# SOLUTION: escapes special characters, adds word boundaries and matches whole brand names

classify_poi_food_typologies <- function(poi_sf) {

  supermarket_classes <- c(
    "supermarket_chain",
    "convenience_or_independent_supermarket"
  )

  # CHANGED: ordered keys ensure more specific brand aliases win before shorter aliases
  ordered_brand_keys <- get_ordered_brand_keys(brand_lookup)

  poi_sf %>%
    mutate(
      class_code = pointx_class %>%
        as.character() %>%
        str_extract("\\d{1,8}") %>%
        str_pad(width = 8, side = "left", pad = "0"),
      brand_std = norm_chr(brand),
      qualifier_type_std = norm_chr(qualifier_type),
      qualifier_data_std = norm_chr(qualifier_data),
      name_std = norm_chr(name),
      # CHANGED: combined text field for format/context detection
      brand_name_qualifier_std =
        str_squish(
          str_trim(
            paste(
              brand_std,
              name_std,
              qualifier_data_std
            )
          )
        ),
      poi_group = str_sub(class_code, 1, 2),
      poi_category = str_sub(class_code, 3, 4),
      poi_class = str_sub(class_code, 5, 8)
    ) %>%
    # hierarchy + food typology
    left_join(
      poi_food_lookup,
      by = "class_code"
    ) %>%
    # CHANGED: brand matching is only run for supermarket and convenience/small supermarket classes
    assign_brand_matches(
      brand_keys = ordered_brand_keys,
      supermarket_classes = supermarket_classes
    ) %>%
    left_join(
      brand_lookup,
      by = c("matched_brand_key" = "brand_key")
    ) %>%
    mutate(
      # CHANGED: explicit food candidate flag
      is_food_candidate =
        class_code %in% food_candidate_lookup$class_code,
      express_flag = str_detect(
        brand_name_qualifier_std,
        "\\bexpress\\b|\\blocal\\b|\\bmetro\\b|\\bsimply food\\b|\\blittle waitrose\\b|\\bdaily\\b|\\bfood to go\\b"
      ),
      # CHANGED: separate context cues from retail format
      forecourt_flag = str_detect(
        brand_name_qualifier_std,
        "\\bpfs\\b|\\bpetrol\\b|\\bfuel\\b|\\besso\\b|\\bshell\\b|\\bbp\\b|\\btexaco\\b"
      ),
      # REVIEWm circularity, source should not be Brand and category with 13 values
      travel_flag = str_detect(
        brand_name_qualifier_std,
        "\\bmsa\\b|\\bmotorway service\\b|\\bservice area\\b|\\btravel\\b|\\bstation\\b|\\brail\\b|\\bairport\\b|\\bwhistlestop\\b|\\brelay\\b"
      ),
      hospital_flag = str_detect(
        brand_name_qualifier_std,
        "\\bhospital\\b"
      ),
      outlet_flag = str_detect(
        brand_name_qualifier_std,
        "\\boutlet\\b"
      ),
      qualifier_is_cuisine =
        qualifier_type_std %in% c(
          "restaurant type",
          "restaurant_type"
        ),
      # CHANGED: cuisine subtype limited to foodservice records
      cuisine_subtype = case_when(
        food_domain == "foodservice" &
          qualifier_is_cuisine &
          qualifier_data_std != "" ~ qualifier_data_std,
        TRUE ~ NA_character_
      ),
      # CHANGED: retail format applies only to food retail, refined sub-divisions
      retail_format_typology = case_when(
        food_domain != "food_retail" ~ NA_character_,

        # supermarket_chain ──────────────────────────────────────────────────────
        food_outlet_base == "supermarket_chain" &
          format_refine == "hard_discounter" ~ "hard_discounter",

        food_outlet_base == "supermarket_chain" &
          format_refine == "frozen_value_specialist" ~ "frozen_value_specialist",

        # express before brand tier — catches Little Waitrose, Tesco Express, etc.
        food_outlet_base == "supermarket_chain" &
          express_flag ~ "local_express_topup",

        food_outlet_base == "supermarket_chain" &
          format_refine == "premium_supermarket" ~ "premium_supermarket",

        food_outlet_base == "supermarket_chain" &
          format_refine == "premium_convenience" ~ "premium_convenience",

        food_outlet_base == "supermarket_chain" ~ "superstore_or_full_line_chain",

        # convenience_or_independent_supermarket ─────────────────────────────────
        food_outlet_base == "convenience_or_independent_supermarket" &
          format_refine == "symbol_group_convenience" ~ "symbol_group_convenience",

        food_outlet_base == "convenience_or_independent_supermarket" &
          format_refine == "forecourt_convenience" ~ "forecourt_convenience",

        food_outlet_base == "convenience_or_independent_supermarket" &
          format_refine == "travel_convenience" ~ "travel_convenience",

        # premium brands PointX miscoded into convenience class
        food_outlet_base == "convenience_or_independent_supermarket" &
          format_refine == "premium_supermarket" ~ "premium_supermarket",

        # mainstream chains PointX miscoded into convenience class, no express flag
        food_outlet_base == "convenience_or_independent_supermarket" &
          format_refine == "standard_supermarket_chain" &
          !express_flag ~ "superstore_or_full_line_chain",

        # express — catches branded and unbranded convenience express
        food_outlet_base == "convenience_or_independent_supermarket" &
          express_flag ~ "local_express_topup",

        food_outlet_base == "convenience_or_independent_supermarket" ~
          "small_convenience_or_independent_supermarket",

        # other food retail ──────────────────────────────────────────────────────
        food_outlet_base == "cash_and_carry" ~ "bulk_wholesale",
        food_outlet_base == "market" ~ "market_retail",
        food_outlet_base == "livestock_market" ~ "livestock_market",
        food_outlet_base == "frozen_food_retail" ~ "frozen_food_specialist",

        food_outlet_base %in% c(
          "grocer_farmshop_pyo", "butcher", "fishmonger",
          "bakery", "delicatessen", "organic_health_specialist",
          "tea_coffee_merchant", "herbs_spices"
        ) ~ "specialist_food_retail",

        !is.na(food_outlet_base) ~ "other_food_retail",

        TRUE ~ NA_character_
      ),

      # CHANGED: foodservice format added so eating/drinking is not forced into retail typology
      foodservice_format_typology = case_when(
        food_domain != "foodservice" ~ NA_character_,
        food_outlet_base == "fastfood_delivery_service" ~ "delivery_focused_foodservice",
        food_outlet_base == "fastfood_takeaway" ~ "quick_service_takeaway",
        food_outlet_base == "fish_chip_shop" ~ "specialist_takeaway",
        food_outlet_base == "cafe_snackbar_tearoom" ~ "cafe_or_light_refreshment",
        food_outlet_base == "restaurant" ~ "full_service_restaurant",
        food_outlet_base == "pub_bar_inn" ~ "pub_bar_foodservice",
        !is.na(food_outlet_base) ~ "other_foodservice",
        TRUE ~ NA_character_
      ),
      # CHANGED: sociocultural typology now explicitly assigned
      sociocultural_typology = case_when(
        food_domain == "foodservice" &
          !is.na(cuisine_subtype) ~ "cuisine_identified",
        food_domain == "foodservice" ~ "cuisine_unspecified",
        food_domain == "food_retail" ~ "not_applicable_food_retail",
        TRUE ~ NA_character_
      ),
      # CHANGED: formality typology now explicitly assigned
      formality_typology = case_when(
        food_domain == "foodservice" &
          food_outlet_base %in% c(
            "fastfood_takeaway",
            "fastfood_delivery_service",
            "fish_chip_shop",
            "cafe_snackbar_tearoom"
          ) ~ "informal",
        food_domain == "foodservice" &
          food_outlet_base %in% c(
            "restaurant",
            "pub_bar_inn"
          ) ~ "mixed_or_context_dependent",
        food_domain == "food_retail" ~ "not_applicable_food_retail",
        TRUE ~ NA_character_
      ),

      trace_primary = case_when(
        is_food_candidate ~ "pointx_class",
        TRUE ~ NA_character_
      ),

      trace_secondary = case_when(
        !is.na(matched_brand_key) ~ "brand",
        food_domain == "foodservice" &
          qualifier_is_cuisine &
          !is.na(cuisine_subtype) ~ "qualifier_type_and_data",
        TRUE ~ NA_character_
      ),

      classification_rule = class_code,
      classification_version = CLASSIFICATION_VERSION,

      # CHANGED: mapped-food flag kept separate from candidate-food flag
      is_food_poi = !is.na(food_outlet_base),

      # CHANGED: ONE unresolved definition shared by classification_status and QC
      unresolved_flag = case_when(
        !is_food_candidate ~ FALSE,
        food_domain == "food_retail" ~
          if_any(all_of(required_fields_food_retail), is.na),
        food_domain == "foodservice" ~
          if_any(all_of(required_fields_foodservice), is.na),
        TRUE ~ TRUE
      ),

      # CHANGED: classification status now exactly matches unresolved logic
      classification_status = case_when(
        !is_food_candidate ~ "non_food",
        unresolved_flag ~ "review",
        TRUE ~ "classified"
      ),

      # CHANGED: review reason now matches unresolved definition
      review_reason = case_when(
        !is_food_candidate ~ NA_character_,
        is.na(food_domain) ~ "food_candidate_missing_domain",
        is.na(food_outlet_base) ~ "food_candidate_unmapped",
        food_domain == "food_retail" &
          is.na(public_health_typology) ~ "food_retail_missing_public_health_typology",
        food_domain == "food_retail" &
          is.na(public_health_subtype) ~ "food_retail_missing_public_health_subtype",
        food_domain == "food_retail" &
          is.na(sociocultural_typology) ~ "food_retail_missing_sociocultural_typology",
        food_domain == "food_retail" &
          is.na(formality_typology) ~ "food_retail_missing_formality_typology",
        food_domain == "food_retail" &
          is.na(retail_format_typology) ~ "food_retail_missing_format",
        food_domain == "foodservice" &
          is.na(public_health_typology) ~ "foodservice_missing_public_health_typology",
        food_domain == "foodservice" &
          is.na(public_health_subtype) ~ "foodservice_missing_public_health_subtype",
        food_domain == "foodservice" &
          is.na(sociocultural_typology) ~ "foodservice_missing_sociocultural_typology",
        food_domain == "foodservice" &
          is.na(formality_typology) ~ "foodservice_missing_formality_typology",
        food_domain == "foodservice" &
          is.na(foodservice_format_typology) ~ "foodservice_missing_format",
        TRUE ~ NA_character_
      )
    )
}

summarise_classification_qc <- function(restaurants_tagged) {

  n_total <- nrow(restaurants_tagged)
  food_only <- restaurants_tagged %>% filter(!is.na(food_outlet_base))
  n_food <- nrow(food_only)
  n_food_retail <- sum(food_only$food_domain == "food_retail")
  n_foodservice <- sum(food_only$food_domain == "foodservice")

  count_with_prop <- function(data, var, .n) {
    data %>%
      st_drop_geometry() %>%
      count({{ var }}, name = "n", sort = TRUE, .drop = FALSE) %>%
      mutate(prop = if (.n > 0) n / .n else NA_real_)
  }

  status_counts <- count_with_prop(restaurants_tagged, classification_status, n_total)
  domain_counts <- count_with_prop(food_only, food_domain, n_food)
  food_outlet_counts <- count_with_prop(food_only, food_outlet_base, n_food)
  public_health_counts <- count_with_prop(food_only, public_health_typology, n_food)
  retail_format_counts <- count_with_prop(
    food_only %>% filter(food_domain == "food_retail"),
    retail_format_typology,
    n_food_retail
  )
  # CHANGED: added foodservice format summary
  foodservice_format_counts <- count_with_prop(
    food_only %>% filter(food_domain == "foodservice"),
    foodservice_format_typology,
    n_foodservice
  )
  sociocultural_counts <- count_with_prop(food_only, sociocultural_typology, n_food)
  formality_counts <- count_with_prop(food_only, formality_typology, n_food)

  # CHANGED: Now uses classifier-generated unresolved_flag so review logic and QC are identical
  unresolved_rows <- restaurants_tagged %>%
    st_drop_geometry() %>%
    transmute(
      is_food_candidate,
      unresolved_flag,
      classification_status,
      review_reason
    )

  review_reason_counts <- restaurants_tagged %>%
    st_drop_geometry() %>%
    filter(classification_status == "review") %>%
    count(review_reason, sort = TRUE)

  # All count tables must sum to total; review logic must align with unresolved_flag
  stopifnot(
    sum(status_counts$n) == n_total,
    sum(domain_counts$n) == n_food,
    sum(food_outlet_counts$n) == n_food,
    sum(public_health_counts$n) == n_food,
    sum(retail_format_counts$n) == n_food_retail,
    sum(foodservice_format_counts$n) == n_foodservice,
    sum(sociocultural_counts$n) == n_food,
    sum(formality_counts$n) == n_food,
    all(unresolved_rows$classification_status[unresolved_rows$unresolved_flag] == "review"),
    !any(unresolved_rows$classification_status[!unresolved_rows$unresolved_flag & unresolved_rows$is_food_candidate] == "review"),
    !any(unresolved_rows$classification_status[!unresolved_rows$is_food_candidate] != "non_food")
  )

  list(
    total_n = n_total,
    total_food_n = n_food,
    classification_status = status_counts,
    food_domain = domain_counts,
    food_outlet_base = food_outlet_counts,
    public_health_typology = public_health_counts,
    retail_format_typology = retail_format_counts,
    foodservice_format_typology = foodservice_format_counts,
    sociocultural_typology = sociocultural_counts,
    formality_typology = formality_counts,
    review_reason = review_reason_counts
  )
}

# Action
restaurants_tagged <- classify_poi_food_typologies(poi_sf)

# Subset for review
restaurants_clean <- restaurants_tagged %>%
  filter(classification_status == "classified")

restaurants_flagged <- restaurants_tagged %>%
  filter(classification_status == "review")

# Summarise
qc <- summarise_classification_qc(restaurants_tagged)

# Save outputs
saveRDS(
  restaurants_tagged,
  here("Output", glue("restaurants_tagged_{CLASSIFICATION_VERSION}.rds"))
)
saveRDS(
  restaurants_clean,
  here("Output", glue("restaurants_clean_{CLASSIFICATION_VERSION}.rds"))
)
saveRDS(
  restaurants_flagged,
  here("Output", glue("restaurants_flagged_{CLASSIFICATION_VERSION}.rds"))
)
saveRDS(
  qc,
  here("Output", glue("qc_{CLASSIFICATION_VERSION}.rds"))
)

# CHANGED: validation rewritten as a valid object assignment
# identifies supermarket/convenience outlets where brand refinement failed

brand_match_validation <- restaurants_tagged %>%
  st_drop_geometry() %>%
  filter(
    food_outlet_base %in% c(
      "supermarket_chain",
      "convenience_or_independent_supermarket"
    ),
    is.na(matched_brand_key)
  ) %>%
  select(
    class_code,
    food_domain,
    food_outlet_base,
    brand,
    brand_std,
    name,
    classification_status,
    review_reason
  )

# CHANGED: this is descriptive reporting, not a validation failure table
brand_refinement_summary <- restaurants_tagged %>%
  st_drop_geometry() %>%
  filter(
    is_food_poi,
    food_outlet_base %in% c(
      "supermarket_chain",
      "convenience_or_independent_supermarket"
    )
  ) %>%
  count(
    matched_brand_key,
    retail_format_typology,
    sort = TRUE
  )

stopifnot(
  all(
    is.na(restaurants_tagged$matched_brand_key) |
      restaurants_tagged$food_outlet_base %in% c(
        "supermarket_chain",
        "convenience_or_independent_supermarket"
      )
  )
)
# All points results
# classification_status      n   prop
# 1              non_food 380737 0.8957
# 2            classified  44317 0.1043

# classification_status      n     prop
# 1              non_food 375386 0.883149
# 2            classified  49668 0.116851

# Additional visual checks of what is NOT classified as food
View(restaurants_tagged[restaurants_tagged$is_food_candidate == FALSE, ])
list(unique((restaurants_tagged$groupname[
  restaurants_tagged$is_food_candidate == FALSE
])))
# [[1]]
# [1] "Commercial Services",                "Transport"
# [3] "Public Infrastructure",              "Manufacturing and Production"
# [5] "Sport and Entertainment",            "Attractions"
# [7] "Retail",                             "Education and Health"
# [9] "Accommodation, Eating and Drinking"
list(unique((restaurants_tagged$categoryname[
  restaurants_tagged$groupname  == "Retail" &
    restaurants_tagged$is_food_candidate == FALSE
])))
# [[1]]
# [1] "Household, Office, Leisure and Garden", "Motoring"
# [3] "Clothing and Accessories" ,             "Food, Drink and Multi Item Retail
list(unique((restaurants_tagged$classname[
  restaurants_tagged$categoryname == "Food, Drink and Multi Item Retail" &
    restaurants_tagged$is_food_candidate == FALSE
])))
# [[1]]
# [1] "Green and New Age Goods"
View(as.data.frame(restaurants_tagged[
  restaurants_tagged$categoryname == "Food, Drink and Multi Item Retail" &
    restaurants_tagged$is_food_candidate == FALSE,
]))

list(unique((restaurants_tagged$classname[
  restaurants_tagged$categoryname  == "Eating and Drinking" &
    restaurants_tagged$is_food_candidate == FALSE
])))
# [[1]]
# [1] "Banqueting and Function Rooms" "Internet Cafes"

brand_refinement_summary

list(unique(restaurants_tagged$retail_format_typology))
# [[1]]
# [1] NA                                             "specialist_food_retail"
# [3] "superstore_or_full_line_chain"                "other_food_retail"
# [5] "small_convenience_or_independent_supermarket" "premium_convenience"
# [7] "local_express_topup"                          "frozen_food_specialist"
# [9] "symbol_group_convenience"                     "market_retail"
# [11] "bulk_wholesale"                               "hard_discounter"
# [13] "travel_convenience"                           "forecourt_convenience"
list(unique(restaurants_tagged$groupname[
  is.na(restaurants_tagged$retail_format_typology)  &
    restaurants_tagged$is_food_candidate  == FALSE
]))
# [[1]]
# [1] "Commercial Services"                "Transport"
# [3] "Public Infrastructure"              "Manufacturing and Production"
# [5] "Sport and Entertainment"            "Attractions"
# [7] "Retail"                             "Education and Health"
# [9] "Accommodation, Eating and Drinking"
list(unique(restaurants_tagged$categoryname[
  is.na(restaurants_tagged$retail_format_typology)  &
    restaurants_tagged$is_food_candidate == FALSE &
    restaurants_tagged$groupname == "Retail"
]))
# [[1]]
# [1] "Household, Office, Leisure and Garden", "Motoring"
# [3] "Clothing and Accessories" ,  "Food, Drink and Multi Item Retail"
list(unique(restaurants_tagged$classname[
  is.na(restaurants_tagged$retail_format_typology)  &
    restaurants_tagged$is_food_candidate == FALSE &
    restaurants_tagged$categoryname == "Food, Drink and Multi Item Retail"
]))
# [[1]]
# [1] "Green and New Age Goods"

list(unique(restaurants_tagged$categoryname[
  is.na(restaurants_tagged$retail_format_typology) &
    !restaurants_tagged$is_food_candidate
]))
# [[1]]
# [1] "It, Advertising, Marketing and Media Services" ,"Bus Transport"
# [3] "Infrastructure and Facilities"                 ,"Personal, Consumer and Other Services"
# [5] "Consultancies"                                 ,"Legal and Financial"
# [7] "Construction Services"                         ,"Industrial Products"
# [9] "Sports Complex"                                ,"Research and Design"
# [11] "Historical and Cultural"                      , "Household, Office, Leisure and Garden"
# [13] "Road and Rail"                                , "Health Practitioners and Establishments"
# [15] "Animal Welfare"                               , "Bodies Of Water"
# [17] "Industrial Features"                          , "Landscape Features"
# [19] "Primary, Secondary and Tertiary Education"    , "Education Support Services"
# [21] "Property and Development Services"            , "Water"
# [23] "Transport, Storage and Delivery"              , "Tourism"
# [25] "Recreational and Vocational Education"        , "Outdoor Pursuits"
# [27] "Sport and Entertainment Support Services"     , "Organisations"
# [29] "Accommodation"                                , "Contract Services"
# [31] "Repair and Servicing"                         , "Central and Local Government"
# [33] "Venues, Stage and Screen"                     , "Motoring"
# [35] "Recreational"                                 , "Consumer Products"
# [37] "Hire Services"                                , "Clothing and Accessories"
# [39] "Walking"                                      , "Foodstuffs"
# [41] "Botanical and Zoological"                     , "Employment and Career Agencies"
# [43] "Gambling"                                     , "Eating and Drinking"
# [45] "Farming"                                      , "Engineering Services"
# [47] "Air"                                          , "Public Transport, Stations and Infrastructure"
# [49] "Health Support Services"                      , "Recycling Services"
# [51] "Extractive Industries"
list(unique(restaurants_tagged$classname[
  is.na(restaurants_tagged$retail_format_typology) &
    !restaurants_tagged$is_food_candidate &
    restaurants_tagged$categoryname == "Food, Drink and Multi Item Retail"
]))
# [[1]]
# [1] "Banqueting and Function Rooms" "Internet Cafes
# [[1]]
# [1] "Green and New Age Goods"

# Additional visual checks of what IS classified as food
table(restaurants_tagged$retail_format_typology, restaurants_tagged$foodservice_format_typology)

View(restaurants_tagged[
  restaurants_tagged$retail_format_typology == "travel_convenience",
])



# As exurants (10,27rmarkets (8,691) dominate
# Fast food (8,106) and cafes (6,685) solid middle tier
# Specbutchers, fishmongers, confectioners) small but present

# To review 1209/66
# -------------------------
# POI-only model is therefore appropriate for commt as
# a direct measure of nutritional access or affordability.
# The circular is_food_poi definition, duplicate brand joins,
# retail-format conflation, and the need to separate  affordability.
# non-food POIsnmapped food POIs were mixed together.

# petrol and fuel stations separately and multiple activities can co-locate at one site.
#
# Travel should also be a site context, separate from retailer and format; motorway
# service stations and railway stations are distinct PointX transport classes.
#
# Frozen food chains such as Iceland or Farmfoods should not be grouped with
# Aldi/Lidl as the same format; they are better treated as frozen_specialist or value_frozen_specialist, not hard_discounter.
#
# Symbol groups such as SPAR should remain an operator model or banner field,
# because stock and pricing can vary materially by branch even within the same symbol group.

# Logic
# Run gemini-3.1-flash-lite on all 66
# Send only low-confidence cases to gemini-2.5-pro
# sequential batch processing is the safer choice, 50 rows per call
# Use ellmer batch helpers to manage multi-prompt workflows by ysing
# parallel_chat_structured() with an array-of-objects schema and voids fragile
# regex extraction of JSON tex

restaurants_tagged <- readRDS(here("Output", glue("restaurants_tagged_{CLASSIFICATION_VERSION}.rds")))
restaurants_clean <- readRDS(here("Output", glue("restaurants_clean_{CLASSIFICATION_VERSION}.rds")))
restaurants_flagged <- readRDS(here("Output", glue("restaurants_flagged_{CLASSIFICATION_VERSION}.rds")))
qc <- readRDS(here("Output", glue("qc_{CLASSIFICATION_VERSION}.rds")))

# AI classification only for flagged rows
Sys.getenv("GEMINI_API_KEY")

if (row(restaurants_flagged) > 0) {

  restauants_flagged <- restaurants_flagged |>
    mutate(row_id = row_number())

  result_type - type_array(
    type_object(
      row_id = type_integer(
        "Input row identifier."
      ),
      ethnic_label = type_enum(
        c("ethnic", "other", "unknown")
      ),
      chain_label = type_enum(
        c("chain", "independent", "unknown")
      ),
      confidence = type_number(
        "Confidence from 0 to 1.",
        required = FALSE
      ),
      reason = type_string(
        "Brief explanation.",
        required = FALSE
      )
    ),
    description =
      paste(
        "Return exactly one classification result for every input row_id.",
        "Do not omit any row."
      )
  )

  restaurants_per_prompt <- 20

  prompt_groups <- split(
    restauants_flagged |>
      st_drop_geomery(),
    ceiling(seq_len(nrow(restaurants_flagged)) /
      restaurants_per_prompt)
  )

  prompts <- lapply(
    prompt_groups,
    function(df) {

      rows_text <- paste0("row_id: ", df$row_id, "\n",
        "name: ", coalesce(df$ame, ""), "\n",
        "brand: ", coalesce(df$brand, ""), "\n",
        "qualifier: ", coalesce(df$qualifier_data, ""
        ),
        collapse = "\n\n"
      )

      paste(
        "Return an array of classification objects.",
        "",
        "Each object must contain",
        "- row_id", "",
        "- ethnic_label", "",
        "- chain_label", "",
        "Every input row_id must appear exactly once in the output.",
        "Do not omit rows.",
        "Do not create additional rows.",
        "",
        "Decision rules:",
        "1. Chain wins over ethnic if both appear.",
        "2. Generic-only qualifiers should usually be independent.",
        "3. Use unknown only when evidence is missing or contradictory.",
        "4. Keep reason brief.",
        "",
        rows_text,
        sep = "\n"
      )
    }
  )

  dir.create("chunk_results", showWarnings = FALSE)

  chat <- chat_google_gemini(
    model = "gemini-3.1-flash-lite"
  )

  message("Starting parallel_chat_tructured")

  ai_results <- parallel_chat_structured(
    chat = chat,
    prompts = prompts,
    type = result_type,
    max_active = 2,
    rpm = 12,
    on_error = "continue"
  ) |>
    as_tibble()

  class(ai_results)
  str(ai_results)

  saveRDS(
    ai_results,
    file.path(
      "chunk_results",
      "all_results.rds"
    )
  )


  if (!".error" %in% names(ai_results)) {
    ai_resuls$.error <- NA_character_
  }

  ai_results - ai_results |>
    mutate(
      had_error = !is.na(.error),
      ai_reason = if_else(
        had_error,
        "Sructured output failed",
        coalesce(
          as.character(reason),
          "No reasn returned"
        )
      ),
      ai_ethnic_label = coalesce(
        as.character(ethnic_label),
        "unknown"
      ),
      ai_chain_label = coalesce(
        as.character(chain_label),
        "unknown"
      ),
      ai_confidence = suppressWarnings(
        as.numeric(confidence)
      )
    ) |>
    dplyr::select(
      row_id,
      ai_ethnic_label,
      ai_chain_label,
      ai_confidence,
      ai_reason
    )

  message("Finished parallel_chat_structured")

  restaurants_ai <- restaurants_flagged |>
    left_join(
      ai_results,
      by = "row_id"
    )

  missing_ids <- setdiff(
    restaurants_flagged$row_id,
    restaurants_ai$row_id[
      !is.na(restaurants_ai$ai_ethnic_label)
    ]
  )

  if (length(missing_ids) > 0) {
    warning(
      length(missing_ids),
      " row_ids were not returned by the model."
    )
  }

  restaurants_final <- bind_ros(
    restaurants_clean |>
      mutate(
        row_id = NA_integer_,
        ai_ethnic_label = NA_character_,
        ai_chain_label = NA_character_,
        ai_confidence = NA_real_,
        ai_reason = NA_character_
      ),
    restaurants_ai
  )

} else {

  restaurants_final <- restaurants_clan |>
    mutate(
      row_id = NA_integer_,
      ai_ethnic_label = NA_character_,
      ai_chain_label = NA_character_,
      ai_confidence = NA_real_,
      ai_reason = NA_character_
    )
}

# Final classification
restaurants_final <- restaurants_final |>
  mutate(
    ethnic_final = coalesce(ethnic_rue, ai_ethnic_abel, "unknown"),
    chain_final = coalesce(chain_rule, ai_chain_label, "unknown"),
    confidence_final = coalesce(ai_confidence, 1.0)
  ) |>
  dplyr::select(
    geometry, name, brand, qualifier_data,
    ethnic_final, chain_final, confidence_final, i_reason,
    -any_of(c("ethnic_rule", "chain_rule", "ai_review_flag", "prompt"))
  )

# Verify
n_classified <- nrow(restaurants_final)
n_total <- nrow(restaurants_clean) + nrow(restaurants_flagged)

if (n_classified != n_total) {
  stop(
    paste0(
      "Final row count mismatch: ",
      n_classified, " rows in restaurants_final vs ",
      n_total, " expected rows."
    )
  )
}

# 09:57
# system("rundll32 user32.dll,MessageBeep")
# system.time()

# Groupname [09], Categories [47],
# Clasname [0671 Alcoholic drinks including off-licences and wholesalers [X]
# 0661 Bakeries
# 0662 Butchers
# 0768 Cash and crry [X]
# 0663 Confectioners [X]
# 0699 Convenience stores and independent supermarkets
# 0665 Delicatessens
# 0666 Fishmongers
# 0667 Frozen foods
# 0668 Green and new age goods
# 0669 Grocers, farm shops nd pick your own
# 0670 Herbs and spices
# 0703 Livestock markets [X]
# 0705 Markets
# 0672 Organic, health, gourmet and kosher foods
# 0819 Supermarket chains
# 0798 Tea and coffee merchants [X]

# Retail shadow index: independent specialty food stores (e.g., green grocers, Halal butchers, traditional markets)
# Core indicator = count of points in 0662, 0669, 0705, 0672.
# Expanded indicator = core plus 0665, 0666, 0661, 0670.

# Social capital Groupname [06], Categories [35],
#  Classname [ 0445 Animal welfare organisations
# 0816 Charitable organisations
# 0769 Community networks and projects
# 0446 Fan clubs and associations
# 0448 Institutes and professional organisations
# 0449 Political parties and related organisations
# 0450 Religious organisations
# 0447 Sports clubs and associations
# 0452 Youth organisations]

healthy_classes <- c(
  "090470661", # Bakeries
  "090470665", # Delicatessens
  "090470666", # Fishmongers
  "090470668", # Green and new age goods
  "090470669", # Grocers, farm shops and pick your own
  "090470670", # Herbs and spices
  "090470672", # Organic, health, gourmet and kosher foods
  "090470705"  # Markets
)

unhealthy_classes <- c(
  "010200018", # Fast food and takeaway outlets
  "010200019", # Fast food delivery services
  "010200020", # Fish and chip shops
  "010200034"  # Pubs, bars and inns
)

retail_shadow <- c(
  "090470662", # Butchers
  "090470669", # Grocers, farm shops and pick your own
  "090470672", # Organic, health, gourmet and kosher foods
  "090470705",  # Markets
  # REVIEW
  "090470661", # Bakeries
  "090470665", # Delicatessens
  "090470666", # Fishmongers
  "090470670"  # Herbs and spices
)

social_capital_classes <- c(
  "060350445", # Animal welfare organisations
  "060350447", # Sports clubs and associations
  "060350448", # Institutes and professional organisations
  "060350449", # Political parties and related organisations
  "060350450", # Religious organisations
  "060350452", # Youth organisations
  "060350769", # Community networks and projects
  "060350816"  # Charitable organisations
)

poi_sf <- poi_sf %>%
  mutate(
    healthy_retail = pointx_class %in% healthy_classes,
    unhealthy_retail = pointx_class %in% unhealthy_classes,
    retail_shadow = pointx_class %in% retail_shadow,
    social_capital = pointx_class %in% social_capital_classes
  )

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

# REVIEW
# duckspatial::ddbs_write_vector(
#   conn       = con,
#   x          = poi_sf,
#   name       = "poi",
#   schema     = "main",        # or another schema if you prefer
#   overwrite  = TRUE
# )
dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE poi AS
  dplyr::select ,
  geom::GEOMETRY AS geom
  FROM ST_Read('%s')
", path))
# [1] 499853

# Reviewing database
DBI::dbGetQuery(con, "PRAGMA table_info('poi')")

DBI::dbGetQuery(con, "
  dplyr::select
    COUNT() AS n_total,
    SUM(CASE WHEN ST_IsValid(geom) THEN 1 ELSE 0 END) AS n_valid,
    SUM(CASE WHEN NOT ST_IsValid(geom) THEN 1 ELSE 0 END) AS n_invalid
  FROM poi
")

DBI::dbGetQuery(con, "
  dplyr::select
    id,
    ST_GeometryType(geom) AS geom_type,
    ST_AsText(geom) AS wkt
  FROM poi
  LIMIT 10
")

DBI::dbGetQuery(con, "
  dplyr::select
    COUNT() AS n_total,
    SUM(CASE WHEN geom IS NULL THEN 1 ELSE 0 END) AS n_null_geom,
    COUNT(DISTINCT ST_AsText(geom)) AS n_unique_geoms
  FROM poi
")

DBI::dbGetQuery(con, "
  dplyr::select
    COUNT() AS n_mismatch
  FROM poi
  WHERE ABS(feature_easting - ST_X(geom)) > 0.001
     OR ABS(feature_northing - ST_Y(geom)) > 0.001
")

path <- here("Data", "Boundaries", "LSOA_2011_London_gen_MHW.shp")

dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE lsoa_boundaries AS
  dplyr::select ,
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
  dplyr::select
    p.id AS point_id,
    l.LSOA11CD AS lsoa_id,
    ST_Within(p.geom_1, l.geom_1) AS is_within,
    ST_Touches(p.geom_1, l.geom_1) AS is_touching
  FROM poi p
  LEFT JOIN lsoa_boundaries l
    ON ST_Intersects(p.geom_1, l.geom_1)
)
dplyr::select
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
dplyr::select
  point_class,
  COUNT() AS n_points,
  SUM(is_within) AS total_is_within,
  SUM(is_touching) AS total_is_touching
FROM classified_poi GROUP BY point_class
ORDER BY point_class
")

# point_class n_points total_is_within total_is_touching
# 1      inside   369697          369697                 0
# 2     outside   130156              NA                NA

dbGetQuery(con, "dplyr::select  FROM classified_poi LIMIT 10") #  No edge cases

# Assigning each POI point ID to the polygon ID that contains it
dbExecute(con, "
  CREATE OR REPLACE TABLE points_with_polygons AS
  dplyr::select
  p.,
  l.LSOA11CD AS LSOA11CD
  FROM poi p
  INNER JOIN lsoa_boundaries l
  ON ST_Within(p.geom_1, l.geom_1)
")

DBI::dbGetQuery(con, "PRAGMA table_info('points_with_polygons')")

# Extract table to R,
points_with_polygons <- DBI::dbGetQuery(con, "dplyr::select  FROM points_with_polygons")

dbGetQuery(con, "SHOW TABLES")

dbGetQuery(con, "dplyr::select COUNT() FROM points_with_polygons")
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

# Construct thematic subsets rather than analysing all POIs together.
# Avoid mixing food and non-food POIs because both CLQ and segregation statistics assume categories belong to a common parent population.
# Leslie & Kronenfeld explicitly recommend analysing categorical subsets within a meaningful joint population

food_points_sf <- points_sf %>%
  filter(
    classification_status == "classified",
    !is.na(food_domain)
  )

food_retail_sf <- food_points_sf %>%
  filter(food_domain == "food_retail")

foodservice_sf <- food_points_sf %>%
  filter(food_domain == "foodservice")

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

# 4. Implement geographic retail measures, see lytleMeasuresFoodEnvironment2017
# Food swamp indicators
## Retail Food Environment Index (RFEI)
# RFEI =  (Fast Food Restaurants + Convenience Stores)/
# (Supermarkets + Large Grocery Stores + Produce Markets)

## Modified Retail Food Environment Index (mRFEI)
# mRFEI = Healthy Food Retailers/
# (Healthy Food Retailers + Less Healthy Food Retailers) ×100

# Food mirage indicators
# FMI = Mean Cost of a Standardised Healthy Food Basket in Neighborhood i/
# Median Household Income of Neighborhood i

# Spatial Price Mismatch (SPM) Score
# food mirage is mathematically flagged when a geographic area falls simultaneously
# into the lowest 20% for median household income but ranks in the highest 20% for
# average food-store price tiering within a 15-minute walking catchment.


# Section 3 is currently a placeholder. The preceding classification workflow
# already gives you the structure needed to implement point-pattern analysis.
# The key design decision is that analyses should operate on classified food points only,
# not all POIs. The literature you attached supports two complementary approaches:
#
# 1. CLQ (Leslie & Kronenfeld, 2011) for asymmetric co-location between categorical food outlet types.
# 2. Sadahiro's segregation measure Ξ(k) for testing whether outlet categories are spatially segregated or mixed.
#
#
# Because your classification explicitly separates:
#
# `food_domain` → foodservice vs food\_retail
#  `food_outlet_base`
#  `retail_format_typology`
#  `foodservice_format_typology`
# `public_health_typology`
#
# the natural analytical unit is points\_sf filtered to classified food outlets.
#
#
# ## 3. Implement spatial point data analysis
#
# ### 3.0 Analytical datasets
#
# Construct thematic subsets rather than analysing all POIs together.
#
# ```r
# food_points_sf <- points_sf %>%
#   filter(
#     classification_status == "classified",
#     !is.na(food_domain)
#   )
#
# food_retail_sf <- food_points_sf %>%
#   filter(food_domain == "food_retail")
#
# foodservice_sf <- food_points_sf %>%
#   filter(food_domain == "foodservice")
# ```
#
# Avoid mixing food and non-food POIs because both CLQ and segregation statistics assume categories belong to a common parent population.
# Leslie & Kronenfeld explicitly recommend analysing categorical subsets within a meaningful joint population.
#
#
#
# ### 3.1 Spatial co-location and association mining (CLQ)
#
# #### Objective
#
# Test whether specific outlet categories preferentially locate near one another.
#
# Examples:
#
#  Fast-food takeaways → convenience stores
#  Supermarkets → markets
#  Restaurants → cafés
#  Premium convenience → restaurants
#  Ethnic grocers → restaurants of similar cuisine (if cuisine refinement added)
#
# #### Recommended category variable
#
# Start with:
#
# ```r
# food_outlet_base
# ```
#
# This is the most stable typology and avoids sparse categories.
#
# Later analyses can use:
#
# ```r
# retail_format_typology
# foodservice_format_typology
# public_health_typology
# ```
#
# #### Subsets
#
# Retail only
#
# ```r
# food_retail_sf
# ```
#
# Analyse:
#
# ```r
# retail_format_typology
# ```
#
# Examples:
#
#  hard\_discounter
#  premium\_convenience
#  symbol\_group\_convenience
#  market\_retail
#  frozen\_food\_specialist
#
# Foodservice only
#
# ```r
# foodservice_sf
# ```
#
# Analyse:
#
# ```r
# foodservice_format_typology
# ```
#
# Examples:
#
#  takeaway
#  chain\_restaurant
#  independent\_restaurant
#  café
#  pub\_bar
#
# Cross-domain
#
# ```r
# food_points_sf
# ```
#
# Analyse:
#
# ```r
# combined_type
# ```
#
# where:
#
# ```r
# combined_type = coalesce(
#   retail_format_typology,
#   foodservice_format_typology
# )
# ```
#
# #### Statistical model
#
# Apply global CLQ using nearest neighbours and random relabelling, preserving point locations. This follows Leslie & Kronenfeld's null hypothesis that observed point geometry remains fixed while category labels are permuted.
#
#
# Suggested neighbour sizes:
#
#   ```r
# k = c(1, 3, 5, 10)
# ```
#
# Sensitivity analysis is preferable because neighbourhood scale affects interpretation.
#
#
#  CLQ matrix
#  p-values
#  heatmap
#  network graph
#
# Interpretation:
#
#   ```text
# CLQ > 1 = attraction
# CLQ < 1 = avoidance
# ```
#
#
#
#   ### 3.2 Point segregation and inhomogeneity
#
#   #### Objective
#
#   Assess whether outlet categories form segregated clusters or mixed environments.
#
# Use Sadahiro's Ξ(k) framework. It was developed specifically for categorical point data and evaluates segregation relative to random relabelling.
#
#
# #### Segregation variables
#
# ##### Model 1: Retail formats
#
# ```r
# retail_format_typology
# ```
#
# Question:
#
# > Are hard discounters, premium retailers and convenience retailers spatially segregated?
#
# ##### Model 2: Foodservice formats
#
# ```r
# foodservice_format_typology
# ```
#
# Question:
#
# > Do takeaways cluster independently from restaurants and cafés?
#
# ##### Model 3: Public-health categories
#
# ```r
# public_health_typology
# ```
#
# Question:
#
# > Do unhealthy\_or\_risk outlets spatially cluster?
#
# This is probably the highest public-health value.
#
# #### Null model
#
# Use:
#
# ```r
# Random labelling
# ```
#
# not CSR.
#
# The literature and CLQ framework both emphasize preserving observed outlet locations while randomising category labels.
#
# #### Neighbourhood scale
#
# Sadahiro reports good performance across multiple k values and recommends examining several scales.
#
# Suggested:
#
# ```r
# k = c(5,10,15,20)
# ```
#
# Outputs:
#
#  Global Ξ(k)
#  Local Ψi(k)
#  Segregation maps
#  Significance surfaces
#
#
#
# ### 3.3 Retail shadow and corporate dominance analysis
#
# This emerges naturally from your classification.
#
# Create:
#
# ```r
# ownership_type
# ```
#
# For example:
#
# ```r
# chain
# franchise
# independent
# ```
#
# derived from:
#
# ```r
# matched_brand_key
# brand_group
# ```
#
# Then test:
#
# ```r
# independent vs chain
# ```
#
# using:
#
#  CLQ
#  Segregation index Ξ(k)
#
# Questions:
#
#  Are independent food retailers spatially isolated?
#  Do chains form exclusive retail clusters?
#  Are independents colocated with chains?
#
# This is a stronger implementation of the "retail shadow index" idea than comparing fast-food delivery versus specialists.
#
#
#  Potential weaknesses
#
#  Some typology classes may be sparse.
#  Cuisine subtype may create many low-frequency categories.
#  CLQ matrices become unstable when category counts are small (<10-20 observations). Leslie & Kronenfeld caution against very small categories.
