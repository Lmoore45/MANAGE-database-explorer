# MANAGE Database Explorer
# R Shiny application for exploring MANAGE sample locations and metadata availability

library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(DT)
library(janitor)
library(scales)

# ---- Load public V2 data ----

sample_data <- read_csv(
  "data_public/MANAGE_metadata_public_v2.csv",
  show_col_types = FALSE,
  name_repair = "unique"
) %>%
  clean_names()

# ---- Helper functions ----

is_present <- function(x) {
  x_chr <- str_trim(as.character(x))
  !is.na(x) &
    x_chr != "" &
    !str_to_lower(x_chr) %in% c("na", "nan", "null", "none", "unknown")
}

safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

clean_choice_vector <- function(x) {
  x <- sort(unique(as.character(x)))
  x <- x[!is.na(x) & x != ""]
  c("All", x)
}

# ---- Privacy check ----
# These columns should never appear in the public app dataset.

private_cols <- c(
  "raw_sample_name",
  "server_sample",
  "project",
  "map_project",
  "contributing_pi"
)

private_cols_found <- intersect(private_cols, names(sample_data))

if (length(private_cols_found) > 0) {
  stop(
    paste(
      "Private/internal columns found in public app data:",
      paste(private_cols_found, collapse = ", ")
    )
  )
}

# ---- Basic data cleaning ----

sample_data <- sample_data %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    manage_sample = as.character(manage_sample),
    public_project = as.character(public_project),
    state = as.character(state),
    system = as.character(system),
    scorpan_zone = as.character(scorpan_zone)
  ) %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

# ---- Metadata value columns ----
# These are real measured metadata columns in V2.
# Data availability is calculated based on whether these columns have non-missing values.

metadata_lookup <- tibble::tribble(
  ~column, ~label,
  "total_c_mg_c_g_soil", "Total Carbon",
  "soc_mg_c_g_soil", "Soil Organic Carbon",
  "om_percent", "Organic Matter",
  "maoc_mg_c_g_soil", "Mineral-Associated Organic Carbon",
  "poc_mg_c_g_soil", "Particulate Organic Carbon",
  "total_n_percent", "Total Nitrogen",
  "no3_n_mg_kg", "Nitrate",
  "nh4_n_mg_kg", "Ammonium",
  "pmn_mg_kg", "Potentially Mineralizable Nitrogen",
  "p_mg_kg", "Phosphorus",
  "k_mg_kg", "Potassium",
  "mg_mg_kg", "Magnesium",
  "ca_mg_kg", "Calcium",
  "mn_mg_kg", "Manganese",
  "na_mg_kg", "Sodium",
  "zn_mg_kg", "Zinc",
  "fe_mg_kg", "Iron",
  "cu_mg_kg", "Copper",
  "s_mg_kg", "Sulfur",
  "b_mg_kg", "Boron",
  "p_h", "pH",
  "bulk_density_g_cm3", "Bulk Density",
  "texture_class", "Texture Class",
  "sand_percent", "Sand",
  "clay_percent", "Clay",
  "silt_percent", "Silt"
) %>%
  filter(column %in% names(sample_data))

metadata_cols <- metadata_lookup$column

metadata_choices <- setNames(
  metadata_lookup$column,
  metadata_lookup$label
)

# ---- Filter choices ----

project_choices <- clean_choice_vector(sample_data$public_project)
state_choices <- clean_choice_vector(sample_data$state)
system_choices <- clean_choice_vector(sample_data$system)
scorpan_choices <- clean_choice_vector(sample_data$scorpan_zone)

# ---- UI ----

ui <- page_sidebar(
  title = "MANAGE Database Explorer",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  sidebar = sidebar(
    width = 310,
    
    h4("Filters"),
    
    selectInput(
      inputId = "project_filter",
      label = "Project",
      choices = project_choices,
      selected = "All",
      multiple = FALSE
    ),
    
    selectInput(
      inputId = "state_filter",
      label = "State",
      choices = state_choices,
      selected = "All",
      multiple = FALSE
    ),
    
    selectInput(
      inputId = "system_filter",
      label = "System",
      choices = system_choices,
      selected = "All",
      multiple = FALSE
    ),
    
    selectInput(
      inputId = "scorpan_filter",
      label = "SCORPAN Zone",
      choices = scorpan_choices,
      selected = "All",
      multiple = FALSE
    ),
    
    checkboxGroupInput(
      inputId = "metadata_filter",
      label = "Required metadata availability",
      choices = metadata_choices,
      selected = NULL
    ),
    
    hr(),
    
    p(
      "Use the filters to explore where MANAGE metagenomes are located and which samples have paired soil, management, system, and SCORPAN metadata."
    )
  ),
  
  navset_tab(
    nav_panel(
      "Map Explorer",
      
      card(
        card_body(
          p(
            strong("Multi-Omics for ANalyzing AGricultural Ecosystems (MANAGE)"),
            " is an interactive map showcasing agricultural metagenomic samples from multiple datasets collated into a single public-facing resource across the United States."
          ),
          p(
            "This dashboard allows users to explore the spatial distribution of MANAGE samples, compare project-level data coverage, and identify samples with paired soil, management, SCORPAN, and system-level metadata relevant to agricultural microbiome research."
          ),
          p(
            strong("Questions about the map?"),
            " Contact Laura Moore at ",
            tags$a(
              href = "mailto:Laura.Moore@colostate.edu",
              "Laura.Moore@colostate.edu"
            )
          ),
          p(
            strong("Version 2.0")
          )
        )
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        value_box(
          title = "Samples shown",
          value = textOutput("n_samples"),
          showcase = NULL
        ),
        
        value_box(
          title = "States represented",
          value = textOutput("n_states"),
          showcase = NULL
        ),
        
        value_box(
          title = "MAGs from shown samples",
          value = textOutput("n_mags"),
          showcase = NULL
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("MANAGE sample locations"),
        leafletOutput("sample_map", height = "72vh")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Samples by project"),
          plotOutput("project_plot", height = 350)
        ),
        
        card(
          card_header("Data availability"),
          plotOutput("availability_plot", height = 350)
        )
      )
    ),
    
    nav_panel(
      "Filtered Data",
      
      card(
        card_header("Download filtered sample table"),
        p(
          "Download the currently filtered public MANAGE sample table. The file reflects the project, state, system, SCORPAN zone, and metadata availability filters selected in the sidebar."
        ),
        downloadButton(
          outputId = "download_filtered_data",
          label = "Download filtered data"
        )
      ),
      
      card(
        card_header("Filtered sample table"),
        DTOutput("sample_table")
      )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    dat <- sample_data
    
    if (!is.null(input$project_filter) && input$project_filter != "All") {
      dat <- dat %>%
        filter(public_project == input$project_filter)
    }
    
    if (!is.null(input$state_filter) && input$state_filter != "All") {
      dat <- dat %>%
        filter(state == input$state_filter)
    }
    
    if (!is.null(input$system_filter) && input$system_filter != "All") {
      dat <- dat %>%
        filter(system == input$system_filter)
    }
    
    if (!is.null(input$scorpan_filter) && input$scorpan_filter != "All") {
      dat <- dat %>%
        filter(scorpan_zone == input$scorpan_filter)
    }
    
    if (length(input$metadata_filter) > 0) {
      for (col in input$metadata_filter) {
        dat <- dat %>%
          filter(is_present(.data[[col]]))
      }
    }
    
    dat
  })
  
  output$n_samples <- renderText({
    comma(nrow(filtered_data()))
  })
  
  output$n_states <- renderText({
    comma(n_distinct(filtered_data()$state))
  })
  
  output$n_mags <- renderText({
    if ("number_of_d_rep_99_bins" %in% names(filtered_data())) {
      comma(safe_sum(filtered_data()$number_of_d_rep_99_bins))
    } else {
      "NA"
    }
  })
  
  output$sample_map <- renderLeaflet({
    
    dat <- filtered_data()
    
    validate(
      need(nrow(dat) > 0, "No samples match the selected filters.")
    )
    
    pal <- colorFactor(
      palette = "Set2",
      domain = sample_data$public_project
    )
    
    leaflet(dat) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Terrain"
      ) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light"
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      ) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("Terrain", "Light", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~pal(public_project),
        stroke = FALSE,
        fillOpacity = 0.75,
        popup = ~paste0(
          "<strong>MANAGE sample:</strong> ", manage_sample, "<br>",
          "<strong>Project:</strong> ", public_project, "<br>",
          "<strong>System:</strong> ", system, "<br>",
          "<strong>SCORPAN zone:</strong> ", scorpan_zone, "<br>",
          "<strong>State:</strong> ", state, "<br>",
          "<strong>Metagenome size:</strong> ", meta_g_size_gbp, " Gbp<br>",
          "<strong>dRep 99% MAGs:</strong> ", number_of_d_rep_99_bins, "<br>",
          "<strong>SOC:</strong> ", soc_mg_c_g_soil, " mg C/g soil<br>",
          "<strong>Organic matter:</strong> ", om_percent, " %<br>",
          "<strong>Total N:</strong> ", total_n_percent, " %<br>",
          "<strong>pH:</strong> ", p_h, "<br>",
          "<strong>Texture:</strong> ", texture_class
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~public_project,
        title = "Project"
      )
  })
  
  output$project_plot <- renderPlot({
    
    filtered_data() %>%
      count(public_project, sort = TRUE) %>%
      ggplot(aes(x = reorder(public_project, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Number of samples"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$availability_plot <- renderPlot({
    
    dat <- filtered_data()
    
    validate(
      need(nrow(dat) > 0, "No samples match the selected filters.")
    )
    
    dat %>%
      select(any_of(metadata_cols)) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "metadata_type",
        values_to = "value"
      ) %>%
      mutate(
        available = is_present(value)
      ) %>%
      left_join(
        metadata_lookup,
        by = c("metadata_type" = "column")
      ) %>%
      filter(available) %>%
      count(label, sort = TRUE) %>%
      ggplot(aes(x = reorder(label, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Number of samples"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$sample_table <- renderDT({
    
    dat <- filtered_data()
    
    validate(
      need(nrow(dat) > 0, "No samples match the selected filters.")
    )
    
    table_data <- dat %>%
      select(
        manage_sample,
        public_project,
        system,
        scorpan_zone,
        state,
        latitude,
        longitude,
        meta_g_number_of_reads,
        meta_g_size_gbp,
        number_of_d_rep_99_bins,
        any_of(metadata_cols)
      ) %>%
      rename(
        `MANAGE Sample` = manage_sample,
        Project = public_project,
        System = system,
        `SCORPAN Zone` = scorpan_zone,
        State = state,
        Latitude = latitude,
        Longitude = longitude,
        `Metagenome Reads` = meta_g_number_of_reads,
        `Metagenome Size (Gbp)` = meta_g_size_gbp,
        `dRep 99% MAGs` = number_of_d_rep_99_bins
      ) %>%
      rename_with(
        .fn = ~ metadata_lookup$label[match(.x, metadata_lookup$column)],
        .cols = any_of(metadata_lookup$column)
      )
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("MANAGE_filtered_samples_v2_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      filtered_data() %>%
        select(
          manage_sample,
          public_project,
          system,
          scorpan_zone,
          state,
          latitude,
          longitude,
          meta_g_number_of_reads,
          meta_g_size_gbp,
          number_of_d_rep_99_bins,
          any_of(metadata_cols)
        ) %>%
        rename(
          `MANAGE Sample` = manage_sample,
          Project = public_project,
          System = system,
          `SCORPAN Zone` = scorpan_zone,
          State = state,
          Latitude = latitude,
          Longitude = longitude,
          `Metagenome Reads` = meta_g_number_of_reads,
          `Metagenome Size (Gbp)` = meta_g_size_gbp,
          `dRep 99% MAGs` = number_of_d_rep_99_bins
        ) %>%
        rename_with(
          .fn = ~ metadata_lookup$label[match(.x, metadata_lookup$column)],
          .cols = any_of(metadata_lookup$column)
        ) %>%
        write_csv(file)
    }
  )
}

# ---- Run app ----

shinyApp(ui = ui, server = server)

