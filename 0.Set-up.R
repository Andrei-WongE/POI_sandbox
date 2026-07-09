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
# POI Class > Research Typology > Brand Refinement > Cuisine Refinement
# Use of lookup tables to recover the full 8-digit PointX
# USe a set of alternative classification according to food environment lit

# Single canonical hierarchy (poi_lookup).
# Classification mapping moved into a lookup table.
# Explicit classification ontology and more deterministic rules, added classification_version
# Traceability retained through pointx_class adn, trace_primary and trace_secondary for classification
# Food outlet + missing typology → review
# Food outlet + fully classified → not review
# Non-food outlet → not review

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

classification_fields <- c(
  "food_outlet_base",
  "public_health_typology",
  "public_health_subtype",
  "economic_typology",
  "sociocultural_typology",
  "formality_typology"
)

CLASSIFICATION_VERSION <- "2026-07-08"

brand_lookup <- tribble(
  ~brand_key, ~brand_group, ~economic_refine,
  "aldi", "hard_discounter", "hard_discounter",
  "lidl", "hard_discounter", "hard_discounter",
  "iceland", "hard_discounter", "hard_discounter",
  "waitrose", "premium", "premium_supermarket",
  "marks & spencer", "premium", "premium_supermarket",
  "m&s", "premium", "premium_supermarket",
  "m & s simply food", "premium", "premium_supermarket",
  "marks and spencer", "premium", "premium_supermarket",
  "whole foods", "premium", "premium_supermarket",
  "planet organic", "premium", "premium_supermarket",
  "tesco", "mainstream_chain", "standard_supermarket_chain",
  "sainsbury's", "mainstream_chain", "standard_supermarket_chain",
  "sainsburys", "mainstream_chain", "standard_supermarket_chain",
  "asda", "mainstream_chain", "standard_supermarket_chain",
  "morrisons", "mainstream_chain", "standard_supermarket_chain",
  "co-op", "mainstream_chain", "standard_supermarket_chain",
  "coop", "mainstream_chain", "standard_supermarket_chain",
  "co-operative", "mainstream_chain", "standard_supermarket_chain",
  "spar", "mainstream_chain", "standard_supermarket_chain",
  "budgens", "mainstream_chain", "standard_supermarket_chain",
  "tfc", "mainstream_chain", "standard_supermarket_chain",
  "costcutter", "soft_franchise", "symbol_group_convenience",
  "londis",     "soft_franchise", "symbol_group_convenience",
  "nisa",       "soft_franchise", "symbol_group_convenience",
  "premier",    "soft_franchise", "symbol_group_convenience",
  "best-one",   "soft_franchise", "symbol_group_convenience",
  "mace",       "soft_franchise", "symbol_group_convenience",
  "mccolls",    "soft_franchise", "symbol_group_convenience",
  "day today",  "soft_franchise", "symbol_group_convenience",
  "shell select", "forecourt",     "forecourt_convenience",
  "esso",       "forecourt",     "forecourt_convenience",
  "on the run", "forecourt",     "forecourt_convenience",
  "shell select",   "forecourt", "forecourt_convenience",
  "esso",           "forecourt", "forecourt_convenience",
  "on the run",     "forecourt", "forecourt_convenience",
  "wh smith",       "forecourt", "forecourt_convenience",  # station/travel retail
  "whistlestop",    "forecourt", "forecourt_convenience",
  "relay",          "forecourt", "forecourt_convenience",
  "bp connect",     "forecourt", "forecourt_convenience",
  "texaco",         "forecourt", "forecourt_convenience",
  "murco",          "forecourt", "forecourt_convenience",
  "martin's",     "soft_franchise", "symbol_group_convenience",
  "mccolls",      "soft_franchise", "symbol_group_convenience",
  "day today",    "soft_franchise", "symbol_group_convenience",
  "day-today",    "soft_franchise", "symbol_group_convenience",
  "lifestyle express", "soft_franchise", "symbol_group_convenience",
  "simply fresh", "soft_franchise", "symbol_group_convenience",
)

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

food_typology_lookup <- tribble(
  ~class_code, ~food_outlet_base, ~public_health_typology, ~public_health_subtype,
  "01020013", "cafe_snackbar_tearoom", "mixed_or_context_dependent", "cafe_snack_bar_tea_room",
  "01020018", "fastfood_takeaway", "unhealthy_or_risk", "fast_food_takeaway",
  "01020019", "fastfood_delivery_service", "unhealthy_or_risk", "fast_food_delivery_service",
  "01020020", "fish_chip_shop", "unhealthy_or_risk", "fish_and_chip_shop",
  "01020043", "restaurant", "mixed_or_context_dependent", "restaurant",
  "09470661", "bakery", "mixed_or_context_dependent", "bakery",
  "09470662", "butcher", "healthy_or_supportive_fresh_food", "butcher",
  "09470663", "confectioner", "unhealthy_or_risk", "confectioner",
  "09470665", "delicatessen", "mixed_or_context_dependent", "delicatessen",
  "09470666", "fishmonger", "healthy_or_supportive_fresh_food", "fishmonger",
  "09470667", "frozen_food_retail", "mixed_or_context_dependent", "frozen_food_shop",
  "09470669", "grocer_farmshop_pyo", "healthy_or_supportive_fresh_food", "grocer_farmshop",
  "09470671", "offlicence_alcohol_retail", "unhealthy_or_risk", "off_licence",
  "09470672", "organic_health_specialist", "healthy_or_supportive_fresh_food", "organic_health_food_shop",
  "09470699", "convenience_or_independent_supermarket", "mixed_or_context_dependent", "convenience_store_or_independent_supermarket",
  "09470705", "market", "potentially_supportive_fresh_food", "market",
  "09470768", "cash_and_carry", "mixed_or_context_dependent", "cash_and_carry",
  "09470819", "supermarket_chain", "healthy_or_supportive_fresh_food", "supermarket"
)

poi_food_lookup <- poi_lookup %>%
  left_join(food_typology_lookup, by = "class_code")

# Match brand names
build_brand_patterns <- function(brand_keys) {
  paste0(
    "\\b",
    str_replace_all(brand_keys, "([[:punct:]])", "\\\\\\1"),
    "\\b"
  )
}

match_brand_vectorised <- function(brand_std_vec, brand_keys) {
  patterns <- build_brand_patterns(brand_keys)
  result   <- rep(NA_character_, length(brand_std_vec))

  # Loop over brand keys (18 iterations), not over rows
  for (i in seq_along(brand_keys)) {
    unmatched <- is.na(result)
    if (!any(unmatched)) break
    hits          <- str_detect(brand_std_vec, regex(patterns[i], ignore_case = TRUE))
    result[unmatched & hits] <- brand_keys[i]
  }
  result
}

# PROBLEM: produces false positives"aldi local" vs "idealdistributors"
# SOLUTION: Now escapes special characters, wraps it with word boundaries and tests whether
# whole brand appears in brand_value. CAREFUL picks only first hit

# You are stupid 425,054 × 18 brand checks2≈ 7.7 million regex evaluations, subset to supermarkets

classify_poi_food_typologies <- function(poi_sf) {

  supermarket_classes <- c(
    "supermarket_chain",
    "convenience_or_independent_supermarket"
  )

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
      poi_group = str_sub(class_code, 1, 2),
      poi_category = str_sub(class_code, 3, 4),
      poi_class = str_sub(class_code, 5, 8)
    ) %>%
    # hierarchy + food typology
    left_join(poi_food_lookup, by = "class_code") %>%
    # Select only supermarkets to run brand matching
    mutate(
      # You are stupid 425,054 × 18 brand checks ≈ 7.7 million regex evaluations
      # Subset to supermarkets; vectorised over brand keys not rows
      matched_brand_key = if_else(
        food_outlet_base %in% supermarket_classes,
        match_brand_vectorised(brand_std, brand_lookup$brand_key),
        NA_character_
      )
    ) %>%
    left_join(brand_lookup, by = c("matched_brand_key" = "brand_key")) %>%
    mutate(
      express_flag = str_detect(name_std,
        "\\bexpress\\b|\\blocal\\b|\\bmetro\\b|\\bsimply food\\b|\\blittle waitrose\\b"),
      qualifier_is_cuisine = qualifier_type_std %in% c("restaurant type", "restaurant_type"),
      cuisine_subtype = case_when(
        qualifier_is_cuisine & qualifier_data_std != "" ~ qualifier_data_std,
        TRUE ~ NA_character_
      ),
      # REVIEW possible_poverty_premium price signal should probably only apply to true independents, not symbol groups.
      economic_typology = case_when(
        # Supermarket chain — brand-specific first, generic last
        food_outlet_base == "supermarket_chain" & economic_refine == "hard_discounter"     ~ "hard_discounter",
        food_outlet_base == "supermarket_chain" & economic_refine == "premium_supermarket" ~ "premium_supermarket",
        food_outlet_base == "supermarket_chain" & express_flag ~ "local_express_topup",
        food_outlet_base == "supermarket_chain" ~ "superstore_or_full_line_chain",
        # Convenience — brand-specific first, independent last
        food_outlet_base == "convenience_or_independent_supermarket" & economic_refine == "symbol_group_convenience" ~ "symbol_group_convenience",
        food_outlet_base == "convenience_or_independent_supermarket" & economic_refine == "forecourt_convenience"    ~ "forecourt_convenience",
        food_outlet_base == "convenience_or_independent_supermarket" ~ "small_convenience_or_independent_supermarket",
        # Everything else
        food_outlet_base == "cash_and_carry" ~ "bulk_value_wholesale",
        food_outlet_base == "market" ~ "market_retail",
        food_outlet_base == "fastfood_delivery_service" ~ "delivery_focused_foodservice",
        food_outlet_base %in% c(
          "grocer_farmshop_pyo", "butcher", "fishmonger",
          "bakery", "delicatessen", "organic_health_specialist"
        ) ~ "specialist_food_retail",
        !is.na(food_outlet_base) ~ "other_foodservice_or_retail",
        TRUE ~ NA_character_
      ),
      economic_price_signal = case_when(
        economic_typology == "hard_discounter" ~ "value",
        economic_typology == "premium_supermarket" ~ "premium",
        economic_typology == "local_express_topup" ~ "topup_premium_risk",
        economic_typology == "small_convenience_or_independent_supermarket" ~ "possible_poverty_premium",
        economic_typology == "market_retail" ~ "variable_often_low_cost",
        economic_typology == "bulk_value_wholesale" ~ "bulk_value",
        !is.na(economic_typology) ~ "unknown",
        TRUE ~ NA_character_
      ),
      sociocultural_typology = case_when(
        food_outlet_base == "market" ~ "informal_market_food_or_mixed_market",
        qualifier_is_cuisine & !is.na(cuisine_subtype) & food_outlet_base %in% c("restaurant", "cafe_snackbar_tearoom", "fastfood_takeaway", "fish_chip_shop", "fastfood_delivery_service") ~ "cuisine_specific_foodservice",
        qualifier_is_cuisine & !is.na(cuisine_subtype) & food_outlet_base %in% c("grocer_farmshop_pyo", "convenience_or_independent_supermarket", "supermarket_chain", "organic_health_specialist", "delicatessen") ~ "cuisine_or_ethnic_specialty_retail",
        food_outlet_base %in% c("grocer_farmshop_pyo", "butcher", "fishmonger", "convenience_or_independent_supermarket", "supermarket_chain", "bakery", "delicatessen", "organic_health_specialist", "offlicence_alcohol_retail", "cash_and_carry") ~ "grocery_retail_general",
        food_outlet_base %in% c("restaurant", "cafe_snackbar_tearoom", "fastfood_takeaway", "fish_chip_shop", "fastfood_delivery_service") ~ "foodservice_general",
        !is.na(food_outlet_base) ~ "other_or_unknown",
        TRUE ~ NA_character_
      ),
      formality_typology = case_when(
        food_outlet_base == "market" ~ "informal_or_mixed",
        food_outlet_base == "fastfood_delivery_service" ~ "delivery_service",
        food_outlet_base %in% c(
          "supermarket_chain", "grocer_farmshop_pyo", "butcher", "fishmonger",
          "convenience_or_independent_supermarket", "bakery", "confectioner",
          "delicatessen", "frozen_food_retail", "organic_health_specialist",
          "offlicence_alcohol_retail", "cash_and_carry",
          "restaurant", "cafe_snackbar_tearoom", "fastfood_takeaway", "fish_chip_shop"
        ) ~ "formal_brick_and_mortar",
        !is.na(food_outlet_base) ~ "unknown",
        TRUE ~ NA_character_
      ),
      trace_primary = case_when(
        !is.na(food_outlet_base) ~ "pointx_class",
        TRUE ~ NA_character_
      ),
      trace_secondary = case_when(
        !is.na(matched_brand_key) ~ "brand",
        qualifier_is_cuisine & !is.na(cuisine_subtype) ~ "qualifier_type_and_data",
        TRUE ~ NA_character_
      ),
      classification_rule = class_code,
      classification_version = CLASSIFICATION_VERSION,
      is_food_poi = !is.na(food_outlet_base),
      ai_review_flag = (
        is_food_poi &
          if_any(all_of(classification_fields), is.na)
      ),
      classification_status = case_when(
        ai_review_flag ~ "review",
        is_food_poi ~ "classified",
        TRUE ~ "non_food"
      )
    )
}

summarise_classification_qc <- function(restaurants_tagged) {

  n_total <- nrow(restaurants_tagged)
  food_only <- restaurants_tagged %>% filter(!is.na(food_outlet_base))
  n_food    <- nrow(food_only)

  count_with_prop <- function(data, var, .n) {
    data %>%
      st_drop_geometry() %>%
      count({{ var }}, name = "n", sort = TRUE) %>%
      mutate(prop = n / .n)
  }

  status_counts <- count_with_prop(restaurants_tagged, classification_status, n_total)
  food_outlet_counts <- count_with_prop(food_only, food_outlet_base, n_food)
  public_health_counts <- count_with_prop(food_only, public_health_typology, n_food)
  economic_counts <- count_with_prop(food_only, economic_typology, n_food)
  sociocultural_counts <- count_with_prop(food_only, sociocultural_typology, n_food)
  formality_counts <- count_with_prop(food_only, formality_typology, n_food)

  # Non-food POIs have NA across all classification_fields, exclude from unresolved check!
  unresolved_rows <- restaurants_tagged %>%
    st_drop_geometry() %>%
    mutate(unresolved = !is.na(food_outlet_base) & if_any(all_of(classification_fields), is.na))

  # All count tables must sum to total; review flags must align with unresolved fields
  stopifnot(
    sum(status_counts$n)        == n_total,
    sum(food_outlet_counts$n)   == n_food,
    sum(public_health_counts$n) == n_food,
    sum(economic_counts$n)      == n_food,
    sum(sociocultural_counts$n) == n_food,
    sum(formality_counts$n)     == n_food,
    all(unresolved_rows$ai_review_flag[unresolved_rows$unresolved]),
    # rows that are not unresolved should not have ai_review_flag = TRUE.
    !any(unresolved_rows$ai_review_flag[!unresolved_rows$unresolved])
  )

  list(
    total_n = n_total,
    total_food_n = n_food,
    classification_status = status_counts,
    food_outlet_base = food_outlet_counts,
    public_health_typology = public_health_counts,
    economic_typology = economic_counts,
    sociocultural_typology = sociocultural_counts,
    formality_typology = formality_counts
  )
}

# Action
restaurants_tagged <- classify_poi_food_typologies(poi_sf)

# Subset for review
restaurants_clean <- restaurants_tagged %>%
  filter(!ai_review_flag)

restaurants_flagged <- restaurants_tagged %>%
  filter(ai_review_flag)

# Summarise
qc <- summarise_classification_qc(restaurants_tagged)

# Save outputs

saveRDS(restaurants_tagged,  here("Output",
  glue("restaurants_tagged_{CLASSIFICATION_VERSION}.rds")))
saveRDS(restaurants_clean,   here("Output",
  glue("restaurants_clean_{CLASSIFICATION_VERSION}.rds")))
saveRDS(restaurants_flagged, here("Output",
  glue("restaurants_flagged_{CLASSIFICATION_VERSION}.rds")))
saveRDS(qc,                  here("Output",
  glue("qc_{CLASSIFICATION_VERSION}.rds")))

# Validate
restaurants_tagged %>%
  st_drop_geometry() %>%
  filter(!is.na(matched_brand_key)) %>%
  count(brand_std, matched_brand_key, sort = TRUE) %>%
  filter(n > 1)

# All points results
# classification_status      n   prop
# 1              non_food 380737 0.8957
# 2            classified  44317 0.1043

# As expected
# Restaurants (10,278) and convenience/independent supermarkets (8,691) dominate
# Fast food (8,106) and cafes (6,685) solid middle tier
# Specialist retail (butchers, fishmongers, confectioners) small but present

# To review 1209/66

# Logic
# Run gemini-3.1-flash-lite on all 66
# Send only low-confidence cases to gemini-2.5-pro
# sequential batch processing is the safer choice, 50 rows per call
# Use ellmer batch helpers to manage multi-prompt workflows by ysing
# parallel_chat_structured() with an array-of-objects schema and voids fragile
# regex extraction of JSON tex

# AI classification only for flagged rows
Sys.getenv("GEMINI_API_KEY")

if (nrow(restaurants_flagged) > 0) {

  restaurants_flagged <- restaurants_flagged |>
    mutate(row_id = row_number())

  result_type <- type_array(
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
    restaurants_flagged |>
      st_drop_geometry(),
    ceiling(seq_len(nrow(restaurants_flagged)) /
      restaurants_per_prompt)
  )

  prompts <- lapply(
    prompt_groups,
    function(df) {

      rows_text <- paste0(
        "row_id: ", df$row_id, "\n",
        "name: ", coalesce(df$name, ""), "\n",
        "brand: ", coalesce(df$brand, ""), "\n",
        "qualifier: ", coalesce(df$qualifier_data, ""
        ),
        collapse = "\n\n"
      )

      paste(
        "Return an array of classification objects.",
        "",
        "Each object must contain:",
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

  message("Starting parallel_chat_structured")

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
    ai_results$.error <- NA_character_
  }

  ai_results <- ai_results |>
    mutate(
      had_error = !is.na(.error),
      ai_reason = if_else(
        had_error,
        "Structured output failed",
        coalesce(
          as.character(reason),
          "No reason returned"
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

  restaurants_final <- bind_rows(
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

  restaurants_final <- restaurants_clean |>
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
    ethnic_final = coalesce(ethnic_rule, ai_ethnic_label, "unknown"),
    chain_final = coalesce(chain_rule, ai_chain_label, "unknown"),
    confidence_final = coalesce(ai_confidence, 1.0)
  ) |>
  dplyr::select(
    geometry, name, brand, qualifier_data,
    ethnic_final, chain_final, confidence_final, ai_reason,
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
system("rundll32 user32.dll,MessageBeep")
system.time()

# Groupname [09], Categories [47],
# Classname [0671 Alcoholic drinks including off-licences and wholesalers [X]
# 0661 Bakeries
# 0662 Butchers
# 0768 Cash and carry [X]
# 0663 Confectioners [X]
# 0699 Convenience stores and independent supermarkets
# 0665 Delicatessens
# 0666 Fishmongers
# 0667 Frozen foods
# 0668 Green and new age goods
# 0669 Grocers, farm shops and pick your own
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

dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE poi AS
  dplyr::select *,
  geom::GEOMETRY AS geom
  FROM ST_Read('%s')
", path))
# [1] 499853

# Reviewing database
DBI::dbGetQuery(con, "PRAGMA table_info('poi')")

DBI::dbGetQuery(con, "
  dplyr::select
    COUNT(*) AS n_total,
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
    COUNT(*) AS n_total,
    SUM(CASE WHEN geom IS NULL THEN 1 ELSE 0 END) AS n_null_geom,
    COUNT(DISTINCT ST_AsText(geom)) AS n_unique_geoms
  FROM poi
")

DBI::dbGetQuery(con, "
  dplyr::select
    COUNT(*) AS n_mismatch
  FROM poi
  WHERE ABS(feature_easting - ST_X(geom)) > 0.001
     OR ABS(feature_northing - ST_Y(geom)) > 0.001
")

path <- here("Data", "Boundaries", "LSOA_2011_London_gen_MHW.shp")

dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE lsoa_boundaries AS
  dplyr::select *,
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
  COUNT(*) AS n_points,
  SUM(is_within) AS total_is_within,
  SUM(is_touching) AS total_is_touching
FROM classified_poi GROUP BY point_class
ORDER BY point_class
")

# point_class n_points total_is_within total_is_touching
# 1      inside   369697          369697                 0
# 2     outside   130156              NA                NA

dbGetQuery(con, "dplyr::select * FROM classified_poi LIMIT 10") #  No edge cases

# Assigning each POI point ID to the polygon ID that contains it
dbExecute(con, "
  CREATE OR REPLACE TABLE points_with_polygons AS
  dplyr::select
  p.*,
  l.LSOA11CD AS LSOA11CD
  FROM poi p
  INNER JOIN lsoa_boundaries l
  ON ST_Within(p.geom_1, l.geom_1)
")

DBI::dbGetQuery(con, "PRAGMA table_info('points_with_polygons')")

# Extract table to R,
points_with_polygons <- DBI::dbGetQuery(con, "dplyr::select * FROM points_with_polygons")

dbGetQuery(con, "SHOW TABLES")

dbGetQuery(con, "dplyr::select COUNT(*) FROM points_with_polygons")
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
