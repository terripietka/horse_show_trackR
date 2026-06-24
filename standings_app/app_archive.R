# ------------------- Load Libraries ------------------- #
library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(tidyr)

# ------------------- Load Data ------------------- #
division_data <- readRDS("data/division_standings.rds")
results_data <- readRDS("data/class_standings.rds")
cow_data <- readRDS("data/top_cow_horse.rds")
perf_data <- readRDS("data/top_performance_horse.rds")

# ------------------- User Interface ------------------- #
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(style = "text-align: center;", img(src = "ilrha.png", height = "200px")),
      br(),
      selectInput("division", "Select Division:", choices = c(
        "Jr Horse", "Sr Horse", "Ama", "Nov Ama", "Yth", "Nov Yth", "GAG", "WTRD"
      )),
      br(),
      br(),
      actionButton("return", label = "Return to ILRHA Website", class = "btn btn-pr"),
      tags$script(HTML("$(document).on('click', '#return', function() {
            window.open('https://www.illinoisranchhorse.com/', '_blank');
        });")),
      br(),
      br(),
      div(style = "text-align: center;", img(src = "gmp.png", height = "200px"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overall Standings",
          h3(textOutput("division_title")),
          h4("Overall Standings"),
          dataTableOutput("div_table"),
          h4("Top Cow Horse Standings"),
          dataTableOutput("cow_table"),
          h4("Top Performance Horse Standings"),
          dataTableOutput("perf_table")
        ),
        tabPanel(
          "Class Standings",
          h3("Class Standings"),
          uiOutput("class_tables")
        )
      )
    )
  )
)

# ------------------- Server Logic ------------------- #
server <- function(input, output, session) {
  
  # Check for duplicate horse/exhibitor combinations
  observe({
    dupes <- division_data %>%
      group_by(division, horse_name, exhibitor_name) %>%
      filter(n() > 1) %>%
      arrange(division, desc(total_points))
    
    if (nrow(dupes) > 0) {
      print("Duplicate rows found in division_data:")
      print(dupes)
    }
  })
  
  # ----------------- Reactive Filters ----------------- #
  
  filtered_division <- reactive({
    df <- division_data %>% filter(division == input$division)
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    if (is_jr_or_sr) {
      df %>% group_by(horse_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    } else {
      df %>% group_by(horse_name, exhibitor_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  filtered_cow <- reactive({
    df <- cow_data %>% filter(division == input$division)
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    if (is_jr_or_sr) {
      df %>% group_by(horse_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    } else {
      df %>% group_by(horse_name, exhibitor_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  filtered_perf <- reactive({
    df <- perf_data %>% filter(division == input$division)
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    if (is_jr_or_sr) {
      df %>% group_by(horse_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    } else {
      df %>% group_by(horse_name, exhibitor_name) %>%
        summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  filtered_results <- reactive({
    df <- results_data
    
    selected_div <- input$division
    if (selected_div %in% c("Jr Horse", "Sr Horse")) {
      df %>% filter(division %in% c(selected_div, "Open"))
    } else {
      df %>% filter(division == selected_div)
    }
  })
  
  # ----------------- Output Division Tables ----------------- #
  
  output$div_table <- renderDataTable({
    df <- filtered_division()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    df_display <- if (is_jr_or_sr) {
      df %>% select(Horse = horse_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    }
    
    datatable(df_display,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
              rownames = FALSE)
  })
  
  output$cow_table <- renderDataTable({
    df <- filtered_cow()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    df_display <- if (is_jr_or_sr) {
      df %>% select(Horse = horse_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    }
    
    datatable(df_display,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
              rownames = FALSE)
  })
  
  output$perf_table <- renderDataTable({
    df <- filtered_perf()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    df_display <- if (is_jr_or_sr) {
      df %>% select(Horse = horse_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points) %>%
        arrange(desc(`Total Points`))
    }
    
    datatable(df_display,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
              rownames = FALSE)
  })
  
  # ----------------- Output Class Tables ----------------- #
  
  output$class_tables <- renderUI({
    df <- filtered_results()
    if (nrow(df) == 0) return(tags$p("No matching class results."))
    
    selected_div <- input$division
    is_jr_or_sr <- selected_div %in% c("Jr Horse", "Sr Horse")
    
    output_sections <- list()
    
    if (is_jr_or_sr) {
      jr_sr_df <- df %>% filter(division == selected_div)
      open_df <- df %>% filter(division == "Open")
      
      # Jr/Sr class tables
      for (cls in unique(jr_sr_df$class_name)) {
        class_df <- jr_sr_df %>% filter(class_name == cls)
        output_sections <- append(output_sections, list(make_class_table_jr_sr(class_df, cls)))
      }
      
      # Open classes after Jr/Sr
      if (nrow(open_df) > 0) {
        output_sections <- append(output_sections, list(
          tags$h4("These open classes may also include junior or senior horses:"),
          tags$hr()
        ))
        
        for (cls in unique(open_df$class_name)) {
          class_df <- open_df %>% filter(class_name == cls)
          output_sections <- append(output_sections, list(make_class_table_jr_sr(class_df, cls)))
        }
      }
      
    } else {
      # All classes for non-Jr/Sr divisions
      for (cls in unique(df$class_name)) {
        class_df <- df %>% filter(class_name == cls)
        output_sections <- append(output_sections, list(make_class_table_standard(class_df, cls)))
      }
    }
    
    do.call(tagList, output_sections)
  })
  
  # ----------------- Helper Functions to Build Class Tables ----------------- #
  
  make_class_table_jr_sr <- function(class_df, cls) {
    class_df <- class_df %>%
      mutate(judge_name = paste0(show_name, "<br>", judge_name)) %>%
      group_by(horse_name, judge_name) %>%
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = judge_name, values_from = points, values_fill = 0)
    
    judge_cols <- setdiff(names(class_df), "horse_name")
    
    class_df <- class_df %>%
      mutate(`Total Points` = rowSums(across(all_of(judge_cols)), na.rm = TRUE)) %>%
      rename(Horse = horse_name) %>%
      arrange(desc(`Total Points`))
    
    output_id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", cls))
    output[[output_id]] <- renderDT({
      datatable(class_df, escape = FALSE, options = list(pageLength = 20, scrollX = TRUE, dom = 't'), rownames = TRUE)
    })
    
    tagList(h4(cls), DTOutput(output_id), tags$hr())
  }
  
  make_class_table_standard <- function(class_df, cls) {
    class_df <- class_df %>%
      mutate(judge_name = paste0(show_name, "<br>", judge_name)) %>%
      group_by(horse_name, exhibitor_name, judge_name) %>%
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = judge_name, values_from = points, values_fill = 0)
    
    judge_cols <- setdiff(names(class_df), c("horse_name", "exhibitor_name"))
    
    class_df <- class_df %>%
      mutate(`Total Points` = rowSums(across(all_of(judge_cols)), na.rm = TRUE)) %>%
      rename(Horse = horse_name, Exhibitor = exhibitor_name) %>%
      arrange(desc(`Total Points`))
    
    output_id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", cls))
    output[[output_id]] <- renderDT({
      datatable(class_df, escape = FALSE, options = list(pageLength = 20, scrollX = TRUE, dom = 't'), rownames = TRUE)
    })
    
    tagList(h4(cls), DTOutput(output_id), tags$hr())
  }
  
  # ----------------- Set Division Title ----------------- #
  
  output$division_title <- renderText({
    paste(input$division, "Composite Standings")
  })
}

# ------------------- Launch App ------------------- #
shinyApp(ui, server)
