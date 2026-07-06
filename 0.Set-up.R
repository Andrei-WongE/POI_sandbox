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

ethnic_keywords <- c(
  "afghan", "african", "albanian", "arab", "armenian", "argentinian", "asian",
  "asian fusion", "bangladeshi", "brazilian", "caribbean", "chinese",
  "colombian", "cuban", "egyptian", "ethiopian", "filipino", "georgian", "indian",
  "indian", "asian", "indonesian", "iraqi", "iranian", "irish", "jamaican", "japanese",
  "korean", "kosher", "latin", "lebanese", "malaysian", "mauritian",
  "mediterranean", "mexican", "middle eastern", "mongolian", "moroccan", "nepalese",
  "oriental", "pakistani", "peruvian", "philippine", "polish", "portuguese",
  "russian", "south american", "tex mex", "thai", "tunisian", "turkish",
  "vietnamese"
)

# NOT INCLUDED> Italian, French, British/english (what, this exists??), vegetarian,
# creperie, pizzeria, greek, scottish, seafood, american, international, european,
# continental, swedish, german

generic_non_ethnic <- c(
  "american", "austrian", "belgian", "brasserie restaurant", "british", "continental", "creperie",
  "english", "european", "french", "german", "greek", "international",
  "italian", "motorway services", "other restaurant", "pizzeria", "pizza",
  "pub food restaurant", "roadside", "scottish", "seafood", "spanish",
  "swedish", "vegetarian"
)

# NOT INCLUDED> "ask", "thyme", "aroma", and "poppins"

known_chain_brands <- c(
  "angus steak house", "ask italian", "aubaine", "balans", "basilico",
  "beefeater", "bella italia", "benugo", "bhs restaurant", "bill's",
  "big fernand", "black & blue", "bodean's bbq", "bonhams restaurant",
  "brasserie blanc", "brewers fayre", "brinkley's restaurants", "browns",
  "bubba gump shrimp co", "burger king", "burger & lobster", "byron",
  "cafe rouge", "chicago rib shack", "coast to coast", "costa", "cote",
  "corbin & king restaurants", "cosmo", "crown carveries", "d&d london",
  "del'aziz", "domino's", "ed's easy diner", "favourite chicken & ribs",
  "fatburger", "fire & stone", "firezza", "five guys", "franco manca",
  "frankie & benny's", "garfunkel's", "giraffe", "glendola leisure",
  "gordon ramsay", "gourmet burger kitchen", "greggs", "hard rock cafe",
  "handmade burger co.", "hawksmoor", "hache", "harvester",
  "heston blumenthal restaurants", "honest burgers", "hotel chocolat restaurants",
  "innfusion", "itsu", "jd wetherspoon", "jenny's restaurant", "jimmy spices",
  "joes kitchen & coffee house", "kebabish original", "kerbisher & malt",
  "la salle", "las iguanas", "leon", "loch fyne restaurants",
  "london steakhouse company", "mcdonald's", "mcmanus pub company", "meatliquor",
  "miller & carter", "miso noodle bar", "moto hospitality limited", "nando's",
  "old orleans", "papa john's", "perfect pizza", "peyton & byrne restaurants",
  "pho", "ping pong", "polpo", "pret a manger", "pizza express", "pizza hut",
  "rossopomodoro", "scoff & banter", "shake shack", "spudulike", "starbucks",
  "subway", "table table", "t g i friday's", "the diner", "the gaucho grill",
  "the living room", "tiger bills", "tinseltown", "toby carvery",
  "tom's kitchen", "tortilla", "tuttons", "vintage inns", "wagamama",
  "welcome break", "wetherspoon", "white brasserie", "wildwood",
  "wondertree", "yo! sushi", "young's", "zizzi"
)

ethnic_regex <- paste0(
  "\\b(",
  paste(str_replace_all(unique(ethnic_keywords),
    "([/ ])",
    "[ /]+"),
  collapse = "|"),
  ")\\b"
)

generic_regex <- paste0(
  "\\b(",
  paste(str_replace_all(unique(generic_non_ethnic),
    "([/ ])",
    "[ /]+"),
  collapse = "|"),
  ")\\b"
)

chain_regex <- paste0(
  "\\b(",
  paste(str_replace_all(unique(known_chain_brands),
    "([/ '&.-])",
    "\\\\W*"),
  collapse = "|"),
  ")\\b"
)

restaurants <- poi_sf |>
  filter(as.character(pointx_class) == "01020043")  |>
  mutate(
    name = coalesce(as.character(name), ""),
    brand = coalesce(as.character(brand), ""),
    qualifier_data = coalesce(as.character(qualifier_data), ""),
    name_l = str_squish(str_to_lower(name)),
    brand_l = str_squish(str_to_lower(brand)),
    qual_l  = str_squish(str_to_lower(qualifier_data))
  )

# Logic
# 1. qualifier_data decides ethnic_rule.
# 2. brand decides chain status.
# 3. name is a fallback only.
# 4. AI review only happens when both ethnicity and chain are still unresolved.

restaurants_tagged <- restaurants |>
  mutate(
    # Detect patterns
    has_ethnic_qual = str_detect(qual_l, ethnic_regex) & !str_detect(qual_l, generic_regex),
    has_ethnic_name = str_detect(name_l, ethnic_regex),
    has_chain_brand = brand_l != "" & str_detect(brand_l, chain_regex),
    has_chain_name = str_detect(name_l, chain_regex),
    has_generic_qual = str_detect(qual_l, generic_regex),

    # Apply source hierarchy: qualifier > name > brand, chain priority over ethnic, generic is independent
    classification = case_when(
      has_chain_brand | has_chain_name ~ "chain",
      has_ethnic_qual | has_ethnic_name ~ "ethnic",
      has_generic_qual ~ "independent",
      !has_chain_brand & !has_chain_name & !has_ethnic_qual & !has_ethnic_name & !has_generic_qual ~ "review",
      .default = "review"
    ),
    # Flag for AI review
    ai_review_flag = classification == "review"
  ) |> select(-contains("has_")) # drop intermediate detection flags

# Separate classified and clean
restaurants_clean <- restaurants_tagged |> filter(!ai_review_flag)
restaurants_flagged <- restaurants_tagged |> filter(ai_review_flag)

# Validate
restaurants_tagged |>
  st_drop_geometry() |>
  count(classification) |>
  mutate(prop = n / sum(n))

stopifnot(
  sum(table(restaurants_tagged$classification)) == nrow(restaurants_tagged)
)

# To review 1209

# Logic
# Run gemini-2.5-flash on all 1209
# Send only low-confidence cases to gemini-2.5-pro
# sequential batch processing is the safer choice, 50 rows per call
# Use ellmer batch helpers to manage multi-prompt workflows by ysing
# parallel_chat_structured() with an array-of-objects schema and voids fragile
# regex extraction of JSON tex

# AI classification only for flagged rows
Sys.getenv("GEMINI_API_KEY")

if (nrow(restaurants_flagged) > 0) {
  chat <- chat_google_gemini(model = "gemini-2.5-flash")

  restaurants_flagged <- restaurants_flagged |>
    mutate(global_row_id = row_number())

  result_type <- type_array(
    type_object(
      "One classification result for one restaurant row.",
      global_row_id = type_integer(
        "Row identifier copied exactly from the input row_id."
      ),
      ethnic_label = type_enum(
        c("ethnic", "other", "unknown"),
        "Use 'ethnic' when the name, brand, or qualifier strongly indicates a specific cuisine or ethnic food tradition; 'other' when there is no strong ethnic signal; 'unknown' when evidence is too ambiguous."
      ),
      chain_label = type_enum(
        c("chain", "independent", "unknown"),
        "Use 'chain' when the restaurant is part of a known multi-site brand or franchise; 'independent' when it is not a chain; 'unknown' when evidence is too ambiguous."
      ),
      confidence = type_number(
        "Confidence score from 0.0 to 1.0.",
        required = FALSE
      ),
      reason = type_string(
        "Very brief explanation.",
        required = FALSE
      )
    )
  )

  prompts <- restaurants_flagged |>
    mutate(
      prompt = purrr::pmap_chr(
        list(global_row_id, name, brand, qualifier_data),
        function(global_row_id, name, brand, qualifier_data) {
          paste0(
            "Classify this restaurant row.\n\n",
            "row_id: ", global_row_id, "\n",
            "name: ", coalesce(name, ""), "\n",
            "brand: ", coalesce(brand, ""), "\n",
            "qualifier: ", coalesce(qualifier_data, ""), "\n\n",
            "Decision rules:\n",
            "1. Chain wins over ethnic if both appear.\n",
            "2. Generic-only qualifiers should usually be independent, not unknown.\n",
            "3. Use unknown only when evidence is missing or contradictory.\n",
            "4. Keep the reason brief.\n"
          )
        }
      )
    )

  dir.create("chunk_results", showWarnings = FALSE)

  ai_results <- parallel_chat_structured(
    chat = chat,
    prompts = as.list(prompts$prompt),
    type = result_type,
    max_active = 10,
    rpm = 500,
    on_error = "continue"
  ) |>
    as_tibble() |>
    mutate(
      ai_ethnic_label = coalesce(as.character(ethnic_label), "unknown"),
      ai_chain_label = coalesce(as.character(chain_label), "unknown"),
      ai_confidence = suppressWarnings(as.numeric(confidence)),
      ai_reason = coalesce(as.character(reason), "Missing row in model output")
    ) |>
    select(global_row_id, ai_ethnic_label, ai_chain_label, ai_confidence, ai_reason, everything())

  restaurants_ai <- restaurants_flagged |>
    select(-prompt) |>
    left_join(ai_results, by = "global_row_id")

  restaurants_final <- bind_rows(
    restaurants_clean |>
      mutate(
        global_row_id = NA_integer_,
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
      global_row_id = NA_integer_,
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
  select(
    geometry, name, brand, qualifier_data,
    ethnic_final, chain_final, confidence_final, ai_reason,
    -ethnic_rule, -chain_rule, any_of("ai_review_flag")
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
