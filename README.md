# MANAGE Database Explorer

**MANAGE Database Explorer** is an interactive R Shiny application for exploring the spatial distribution, metadata coverage, and paired soil measurements of agricultural metagenomic samples in the MANAGE database.

**MANAGE** stands for **Multi-Omics for ANalyzing AGricultural Ecosystems**. This app provides a map-based interface for visualizing agricultural metagenomic samples from multiple datasets collated into a single resource across the United States.

## Live App

[Launch the MANAGE Database Explorer](https://lmoore45.shinyapps.io/manage-database-explorer/)

## Overview

Predicting nutrient cycling, carbon transformation, and soil resilience under changing agricultural management and climate conditions requires a better understanding of soil microbiomes across landscapes. MANAGE was developed as a genome-resolved resource for agricultural soils, linking metagenomic samples with soil, management, system, and environmental metadata.

This Shiny app allows users to:

* Explore the geographic distribution of MANAGE metagenomic samples
* Filter samples by project category, state, agricultural system, SCORPAN zone, and available metadata
* Identify samples with paired soil measurements such as soil organic carbon, organic matter, total nitrogen, pH, bulk density, texture, and soil nutrients
* Compare metadata availability across mapped samples
* Summarize sample and MAG coverage across selected filters
* Download a filtered sample table for downstream use

## App Features

The current version includes:

* Interactive Leaflet map of MANAGE sample locations
* Toggleable terrain, light, and satellite map layers
* Public-facing MANAGE sample IDs
* Public-facing project categories
* Filters for project category, state, agricultural system, SCORPAN zone, and metadata availability
* Summary boxes for mapped samples, states represented, and MAGs from selected samples
* Bar plots showing samples by project and data availability across selected mapped samples
* Downloadable filtered sample table

## Public-Facing Sample IDs

Samples are displayed using arbitrary public MANAGE sample names rather than raw sample names.

Public sample names use the format:

```text
MANAGE_00001
MANAGE_00002
MANAGE_00003
```

## Project Categories

The current public-facing project categories are:

* **MANAGE Research Network**
* **Literature Sourced**
* **Joint Genome Institute (JGI)**

## Agricultural System Categories

Samples are classified by agricultural system where available:

* **Cropping**
* **Grazing**

The `System` field allows users to filter mapped samples by broad agricultural context.

## SCORPAN Zones

The app includes a `SCORPAN_Zone` field for filtering samples by environmental and soil-landscape zones. This provides an initial framework for linking MANAGE sample coverage to soil-forming factors and pedo-climatic gradients.

## Repository Structure

```text
MANAGE-database-explorer/
├── app.R
├── README.md
├── data_public/
│   └── MANAGE_metadata_public_v2.csv
├── www/
├── R/
└── MANAGE-database-explorer.Rproj
```

## Data Included in the App

The app uses the public metadata file:

```text
data_public/MANAGE_metadata_public_v2.csv
```

This file contains public-facing MANAGE sample IDs, public project categories, sample coordinates, agricultural system, SCORPAN zone, metagenome summary information, MAG counts, and selected soil metadata values.

The app includes measured values where available, including:

* total carbon
* soil organic carbon
* organic matter
* mineral-associated organic carbon
* particulate organic carbon
* total nitrogen
* nitrate
* ammonium
* potentially mineralizable nitrogen
* phosphorus
* potassium
* micronutrients
* pH
* bulk density
* texture class
* sand, clay, and silt percentages

## Version

**Version 2.0**

## Contact

Questions about the map or MANAGE Database Explorer can be directed to:

**Laura Moore**
[Laura.Moore@colostate.edu](mailto:Laura.Moore@colostate.edu)
