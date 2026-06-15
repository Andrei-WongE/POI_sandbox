# POI_sandbox: Data Wrangling of Greater London Area Points of Interest

This repository, **POI_sandbox**, serves as an environment for data wrangling, preprocessing, and spatial exploratory analysis of Points of Interest (POI) data for the Greater London Area. 
The repository manages large-scale geospatial vector records to facilitate spatial querying, categorization, and proximity analysis.

---

## 1. Dataset Citation and Technical Provenance

The primary geospatial dataset incorporated in this sandbox is derived from the Ordnance Survey (OS) Points of Interest product. Below is the formal citation and metadata profile:

*   **Dataset Name:** Points of Interest [GeoPackage Geospatial Data]
*   **Data Creator/Publisher:** Ordnance Survey (Great Britain) & PointX Ltd
*   **Scale:** 1:1250 (High-resolution positional accuracy)
*   **Geographic Coverage:** Great Britain (GB) (subsetted locally for the Greater London Area)
*   **Release Date / Last Update:** 1 December 2021
*   **Acquisition Platform:** EDINA Digimap Ordnance Survey Service (available at [digimap.edina.ac.uk](https://digimap.edina.ac.uk))
*   **Download Timestamp:** 2026-04-27 09:05:45.954

---

## 2. Geospatial and Database Structure

### 2.1 Classification Hierarchy
Features within the dataset are structured via a three-level hierarchical classification system:
1.  **Group (Level 1):** Broad thematic category. The dataset is partitioned into nine primary groups:
    *   `01` Accommodation, Eating and Drinking
    *   `02` Commercial Services
    *   `03` Attractions
    *   `04` Sport and Entertainment
    *   `05` Education and Health
    *   `06` Public Infrastructure
    *   `07` Manufacturing and Production
    *   `09` Retail
    *   `10` Transport
2.  **Category (Level 2):** Intermediate sub-classification (e.g., Category `0101` for Accommodation vs. `0102` for Eating and Drinking).
3.  **Class (Level 3):** Fine-grained industry/feature type identifiers (e.g., Specific types of restaurants or transport terminals).

### 2.3 Attributes and Identifiers
*   **TOID (Topographic Identifier):** A unique, 16-character alphanumeric reference code linking the spatial feature directly to Ordnance Survey's MasterMap Topography Layer.
*   **UPRN (Unique Property Reference Number):** A unique identifier for addressing and registry linkage, populated systematically to cross-reference with AddressBase layers.

---

## 3. Data Copyright, Licensing, and Restrictions

*   **Crown Copyright:** The digital map data is © Crown copyright 2012–2021. All rights reserved.
*   **PointX Copyright:** Intellectual property rights for the POI classification scheme and database compilation are owned by PointX Ltd and are reproduced under license.
*   **Conspicuous Acknowledgement:** Any cartographic plots, tables, or analytical outputs generated from this repository must carry a visible and legible attribution notice:
    > *"Contains Ordnance Survey data © Crown copyright and database right [Year]. Contains PointX database right [Year]."*


