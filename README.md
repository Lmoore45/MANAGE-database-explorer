# MANAGE-database-explorer

**MANAGE Database Explorer** is an interactive R Shiny application for exploring the spatial distribution and metadata coverage of agricultural metagenomic samples in the MANAGE database.

**MANAGE** stands for **Multi-Omics for ANalyzing AGricultural Ecosystems**. This app provides a map-based interface for visualizing agricultural metagenomic samples from multiple datasets collated into a single resource across the United States.

## Overview

Predicting nutrient cycling, carbon transformation, and soil resilience under changing agricultural management and climate conditions requires a better understanding of soil microbiomes across landscapes. MANAGE was developed as a genome-resolved resource for agricultural soils, linking metagenomic samples with soil, management, and environmental metadata.

This Shiny app allows users to:

- Explore the geographic distribution of MANAGE metagenomic samples
- Filter samples by project category, state, and available metadata
- Identify samples with paired soil measurements such as soil organic carbon, total nitrogen, pH, bulk density, and texture
- Summarize data availability across projects
- Download a filtered sample table for downstream use

## App features

The current version includes:

- Interactive Leaflet map of MANAGE sample locations
- Toggleable terrain, light, and satellite map layers
- Public-facing project categories
- State-level and project-level filters
- Metadata availability filters
- Summary boxes for:
  - number of samples shown
  - number of states represented
  - number of MAGs from selected samples
- Bar plots showing:
  - samples by project
  - data availability across selected samples
- Downloadable filtered sample table

## Public-facing project categories

To protect collaborator and industry-sensitive information, the app uses grouped public-facing project categories rather than all internal dataset names.

The current project categories are:

- **DOE Joint Genome Institute (JGI)**
- **Literature Sourced**
- **Producer and Research**
- **Industry Collaborations**

## Repository structure

```text
MANAGE-database-explorer/
├── app.R
├── README.md
├── data_public/
│   └── MANAGE_sample_metadata_public.csv
├── data-raw/
│   └── create_public_app_data.R
├── www/
├── R/
└── MANAGE-database-explorer.Rproj
