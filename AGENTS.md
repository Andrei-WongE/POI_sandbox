# Project context

I am an experienced R programmer (PhD level) working on spatial data science and spatial databases.
The code in this project is primarily R, with extensive use of sf objects and DuckDB / duckspatial for spatial queries.

The main goals are:
- Reproducible workflows (scripts and Quarto reports that can be re‑run end‑to‑end)
- Transparent data transformations (no hidden state, no “magic” side effects)
- Reasonable complexity without losing tractability and simplicity

Assume the person reading and maintaining the code has a high level of expertise in spatial data, R, and databases.

# Languages and environment

- Primary language: R (R 4.5.2 on Windows 11)
- Main libraries:
  - `sf`
  - `duckdb` and `duckspatial`
  - `janitor`
  - `ellmer`
  - `purrr`
  - `jsonlite`
  - `glue`
- Only use `tidyverse`/`dplyr` when it clearly improves clarity over base R and is easy to reason about.
- Prefer `require()` for loading packages only in examples; in real scripts, use `library()` and check dependencies explicitly.

When generating code:
- Always show which packages are required at the top of the script.
- Prefer explicit `package::function()` calls when using functions from many packages to avoid ambiguity.

# Coding style

General principles:
- Favour **base R** over tidyverse for data manipulation when it does not sacrifice clarity.
- Keep functions small and composable; avoid deeply nested pipes or control structures.
- Prefer explicit arguments over relying on defaults, especially for spatial and database operations.

Conventions:
- Use snake_case for object and function names.
- Use informative, domain‑specific names for sf objects (e.g. `admin_boundaries_sf`, `points_of_interest_sf`).
- Use `snake_case` column names (use `janitor::clean_names()` if needed, but be explicit about renaming).
- Limit pipe chains to a small number of steps; if a chain becomes long, break it into intermediate objects or helper functions.

Formatting:
- One logical operation per line when piping or chaining operations.
- Avoid overly clever one‑liners; prioritise readability and debuggability.
- Comment non‑obvious steps, especially spatial joins, projections, and database writes.

# Spatial data conventions

- Represent vector spatial data as `sf` objects.
- Be explicit about coordinate reference systems (CRS):
  - Always set CRS when creating `sf` objects.
  - Use `st_transform()` deliberately and document CRS changes in comments.
- For spatial joins:
  - Use `st_join()` and specify the `join` and `left` arguments explicitly.
  - Briefly explain the spatial relationship in a comment (e.g. “join points to polygons by containment”).

When generating code that reads or writes spatial data:
- Prefer standard formats (GeoPackage, Parquet with geometry support, GeoJSON) over ad‑hoc formats.
- Avoid hard‑coded absolute paths; prefer relative paths and a clear `data/` structure.

# DuckDB and spatial DuckDB conventions

- Use DuckDB as the main analytical database for tabular and spatial data.
- Use `duckspatial` for spatial support in DuckDB and prefer SQL that is:
  - Explicit about schemas and table names.
  - Clear about joins and spatial predicates.
- When creating spatial tables in DuckDB, prefer reading with `sf::st_read()` and writing
with `duckspatial::ddbs_write_vector()` so geometry columns are correctly bound as GEOMETRY
and work with spatial indexes.

When generating SQL:
- Use upper case for SQL keywords and lower case for table and column names.
- Prefer readable multi‑line SQL strings (e.g. with `glue::glue_sql()` or `glue::glue()`), not long single‑line strings.
- Avoid generating SQL that depends on implicit type casts or ambiguous joins.
- For spatial queries, be explicit about:
  - Geometry columns.
  - CRS assumptions.
  - Spatial predicates (e.g. `ST_Intersects`, `ST_Contains`, `ST_DWithin`).
  

# Reproducibility and project structure

- Assume an RStudio Project with a conventional layout:
  - `R/` for function scripts
  - `data_raw/` for original data
  - `data/` for processed data
  - `db/` or `data/duckdb/` for DuckDB files
  - `notebooks/` or `analysis/` for exploratory scripts and Quarto documents

When generating code:
- Avoid changing the working directory inside scripts; assume the project root is the working directory.
- Avoid interactive prompts; scripts should run non‑interactively.
- Prefer deterministic operations; if randomness is needed, set and document a seed.

Documentation:
- Encourage writing Quarto documents or R scripts that:
  - Load all required packages explicitly.
  - Read raw data.
  - Perform transformations and analysis.
  - Write outputs (tables, figures, models) to well‑defined locations.

# Preferred libraries and patterns

When you need to choose between options:
- Prefer `sf` for spatial vector data, not `sp` or older packages.
- Prefer DuckDB (+ `duckspatial`) for analytical database work instead of ad‑hoc in‑memory joins, when data size or complexity warrants it.
- Use `purrr` only where it clearly improves clarity over `lapply`/`Map` or base loops.
- Use `janitor` for:
  - Cleaning column names (`clean_names()`).
  - Simple summaries (`tabyl()`), when they improve clarity.

Avoid:
- Introducing additional large frameworks or DSLs without a strong justification.
- Over‑engineered class hierarchies or metaprogramming unless necessary.

# How to help

When responding to requests in this project:
- Assume the user understands advanced R and spatial concepts; keep explanations concise but precise.
- When suggesting code:
  - Explain non‑trivial spatial or database logic briefly in comments.
  - Highlight any assumptions about CRS, units, or spatial relationships.
- When refactoring:
  - Preserve semantics first, then improve readability and performance.
  - Avoid introducing hidden dependencies or state.

Testing and validation:
- Encourage basic sanity checks:
  - Row counts before/after joins.
  - CRS and bounding boxes after transformations.
  - Spot‑checks of spatial joins (e.g. plotting small samples).
- If you propose performance optimisations, mention trade‑offs (memory vs CPU, precision vs speed).

# Security and data sensitivity

- Assume datasets may contain sensitive information (e.g. household or firm locations).
- Never suggest exporting or logging full coordinates or IDs unnecessarily.
- Prefer aggregation or anonymisation when demonstrating analysis steps on potentially sensitive data.
