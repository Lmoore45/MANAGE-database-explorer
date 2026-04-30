# MANAGE Database Explorer
# Initial R Shiny application for exploring MANAGE sample locations and metadata availability

library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(DT)
library(janitor)
library(scales)

# ---- Load data ----

sample_data <- read_csv(
  "data/Sample_File_US.csv",
  show_col_types = FALSE,
  name_repair = "unique"
) %>%
  clean_names()

# ---- Helper functions ----

is_yes <- function(x) {
  str_to_lower(as.character(x)) %in% c("yes", "y", "true", "1")
}

safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

# ---- Basic data cleaning ----

sample_data <- sample_data %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    
    # Public-facing project categories
    project_group = case_when(
      map_project %in% c("Wrighton Lab", "IN-RICHES") ~ "Producer and Research",
      map_project %in% c("Syngenta", "Nutrien") ~ "Industry Collaborations",
      map_project == "Literature Sourced" ~ "Literature Sourced",
      map_project == "DOE Joint Genome Institute (JGI)" ~ "DOE Joint Genome Institute (JGI)",
      TRUE ~ as.character(map_project)
    ),
    
    project_group = as.character(project_group),
    state = as.character(state)
  ) %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

# ---- Metadata availability columns ----

availability_lookup <- tibble::tribble(
  ~column, ~label,
  "has_metadata", "Any Metadata",
  "has_management", "Management Data",
  "has_tc", "Total Carbon",
  "has_soc", "Soil Organic Carbon",
  "has_om", "Organic Matter",
  "has_maoc", "Mineral-Associated Organic Carbon",
  "has_poc", "Particulate Organic Carbon",
  "has_tn", "Total Nitrogen",
  "has_no3", "Nitrate",
  "has_nh4", "Ammonium",
  "has_pmn", "Potentially Mineralizable Nitrogen",
  "has_p", "Phosphorus",
  "has_k", "Potassium",
  "has_micronutrients", "Micronutrients",
  "has_p_h", "pH",
  "has_bd", "Bulk Density",
  "has_texture", "Texture"
) %>%
  filter(column %in% names(sample_data))

availability_cols <- availability_lookup$column

# For Shiny checkboxGroupInput:
# names = what the user sees
# values = actual column names used for filtering
availability_choices <- setNames(
  availability_lookup$column,
  availability_lookup$label
)

# For renaming table columns later
availability_rename_vector <- setNames(
  availability_lookup$column,
  availability_lookup$label
)

# ---- Filter choices ----

project_choices <- c(
  "All",
  sort(unique(sample_data$project_group))
)

state_choices <- c(
  "All",
  sort(unique(sample_data$state))
)

# ---- UI ----

ui <- page_sidebar(
  title = "MANAGE Database Explorer",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  sidebar = sidebar(
    width = 300,
    
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
    
    checkboxGroupInput(
      inputId = "availability_filter",
      label = "Required data availability",
      choices = availability_choices,
      selected = NULL
    ),
    
    hr(),
    
    p(
      "Use the filters to explore where MANAGE metagenomes are located and which samples have paired soil, management, and metadata measurements."
    )
  ),
  
  navset_tab(
    nav_panel(
      "Map Explorer",
      
      card(
        card_body(
          p(
            strong("Multi-Omics for ANalyzing AGricultural Ecosystems (MANAGE)"),
            " is an interactive map showcasing agricultural metagenomic samples from multiple datasets collated into a single resource across the United States."
          ),
          p(
            "This dashboard allows users to explore the spatial distribution of MANAGE samples, compare project-level data coverage, and identify samples with paired soil, management, and metadata measurements relevant to agricultural microbiome research."
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
            strong("Version 1.0")
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
          "Download the currently filtered MANAGE sample table. The file reflects the project, state, and data availability filters selected in the sidebar."
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
        filter(project_group == input$project_filter)
    }
    
    if (!is.null(input$state_filter) && input$state_filter != "All") {
      dat <- dat %>%
        filter(state == input$state_filter)
    }
    
    if (length(input$availability_filter) > 0) {
      for (col in input$availability_filter) {
        dat <- dat %>%
          filter(is_yes(.data[[col]]))
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
      domain = sample_data$project_group
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
        color = ~pal(project_group),
        stroke = FALSE,
        fillOpacity = 0.75,
        popup = ~paste0(
          "<strong>Sample:</strong> ", sample_name, "<br>",
          "<strong>MANAGE sample:</strong> ", manage_sample, "<br>",
          "<strong>Project:</strong> ", project_group, "<br>",
          "<strong>State:</strong> ", state, "<br>",
          "<strong>Metagenome size:</strong> ", meta_g_size_gbp, " Gbp<br>",
          "<strong>dRep 99% MAGs:</strong> ", number_of_d_rep_99_bins, "<br>",
          "<strong>Soil Organic Carbon:</strong> ", has_soc, "<br>",
          "<strong>Total Nitrogen:</strong> ", has_tn, "<br>",
          "<strong>pH:</strong> ", has_p_h, "<br>",
          "<strong>Texture:</strong> ", has_texture
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~project_group,
        title = "Project"
      )
  })
  
  output$project_plot <- renderPlot({
    
    filtered_data() %>%
      count(project_group, sort = TRUE) %>%
      ggplot(aes(x = reorder(project_group, n), y = n)) +
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
      select(any_of(availability_cols)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "metadata_type",
        values_to = "available"
      ) %>%
      mutate(
        available = is_yes(available)
      ) %>%
      left_join(
        availability_lookup,
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
    
    dat %>%
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
        any_of(availability_cols)
      ) %>%
      rename(
        Sample = sample_name,
        `MANAGE Sample` = manage_sample,
        Project = project_group,
        State = state,
        Latitude = latitude,
        Longitude = longitude,
        `Metagenome Reads` = meta_g_number_of_reads,
        `Metagenome Size (Gbp)` = meta_g_size_gbp,
        `dRep 99% MAGs` = number_of_d_rep_99_bins
      ) %>%
      rename_with(
        .fn = ~ availability_lookup$label[match(.x, availability_lookup$column)],
        .cols = any_of(availability_lookup$column)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("MANAGE_filtered_samples_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      filtered_data() %>%
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
          any_of(availability_cols)
        ) %>%
        rename(
          Sample = sample_name,
          `MANAGE Sample` = manage_sample,
          Project = project_group,
          State = state,
          Latitude = latitude,
          Longitude = longitude,
          `Metagenome Reads` = meta_g_number_of_reads,
          `Metagenome Size (Gbp)` = meta_g_size_gbp,
          `dRep 99% MAGs` = number_of_d_rep_99_bins
        ) %>%
        rename_with(
          .fn = ~ availability_lookup$label[match(.x, availability_lookup$column)],
          .cols = any_of(availability_lookup$column)
        ) %>%
        write_csv(file)
    }
  )
}

# ---- Run app ----

shinyApp(ui = ui, server = server)

