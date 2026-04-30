library(tidyverse)
library(janitor)

# Read full local MANAGE sample file.
# This file is intentionally kept out of GitHub.
raw_data <- read_csv(
  "data/Sample_File_US.csv",
  show_col_types = FALSE,
  name_repair = "unique"
)

# Create public-facing app dataset.
# Sensitive/internal project labels are collapsed into broader public categories.
public_data <- raw_data %>%
  clean_names() %>%
  mutate(
    project_group = case_when(
      map_project %in% c("Wrighton Lab", "IN-RICHES") ~ "Producer and Research",
      map_project %in% c("Syngenta", "Nutrien") ~ "Industry Collaborations",
      map_project == "Literature Sourced" ~ "Literature Sourced",
      map_project == "DOE Joint Genome Institute (JGI)" ~ "DOE Joint Genome Institute (JGI)",
      TRUE ~ as.character(map_project)
    )
  ) %>%
  select(
    sample_name,
    manage_sample,
    project_group,
    state,
    latitude,
    longitude,
    meta_g_number_of_reads,
    meta_g_size_gbp,
    number_of_d_rep_99_bins,
    has_metadata,
    has_management,
    has_tc,
    has_soc,
    has_om,
    has_maoc,
    has_poc,
    has_tn,
    has_no3,
    has_nh4,
    has_pmn,
    has_p,
    has_k,
    has_micronutrients,
    has_p_h,
    has_bd,
    has_texture
  )

# Create public data folder if it does not exist.
dir.create("data_public", showWarnings = FALSE)

# Write public-safe CSV for the Shiny app.
write_csv(
  public_data,
  "data_public/MANAGE_sample_metadata_public.csv"
)
