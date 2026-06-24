# ------------------- Load Libraries ------------------- #
library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(tidyr)
library(stringr)
library(openxlsx)

# ------------------- Load Data ------------------- #
division_data <- readRDS("data/division_standings.rds")
results_data  <- readRDS("data/class_standings.rds")
cow_data      <- readRDS("data/top_cow_horse.rds")
perf_data     <- readRDS("data/top_performance_horse.rds")

# ------------------- Standard MoRHA Show Order ------------------- #
show_order <- c("March", "May", "August", "November")

# ------------------- UI ------------------- #
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      div(style = "text-align: center;", img(src = "morha.png", height = "180px")),
      br(),
      
      selectInput("division", "Select Division:", choices = c(
        "Junior Horse", "Senior Horse",
        "Ama", "Nov Ama",
        "Yth", "Nov Yth",
        "Novice W/T"
      )),
      
      br(), br(),
      
      actionButton("return", label = "Return to MoRHA Website", class = "btn btn-pr"),
      tags$script(HTML("$(document).on('click', '#return', function() {
            window.open('https://missouriranchhorse.com', '_blank');
      });")),
      
      br(), br(),
      div(style = "text-align: center;", img(src = "gmp.png", height = "180px"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overall Standings",
          h3(textOutput("division_title")),
          downloadButton("download_division_standings", "Download Division Standings (.xlsx)"),
          br(), br(),
          h4("Overall Standings"),
          dataTableOutput("div_table")
        ),
        
        tabPanel(
          "Class Standings",
          h3("Class Standings"),
          downloadButton("download_class_tables", "Download All Class Standings (.xlsx)"),
          uiOutput("class_tables")
        )
      )
    )
  )
)

# ------------------- SERVER ------------------- #
server <- function(input, output, session) {
  
  # ------------------ DIVISION SUMMARY ------------------ #
  filtered_division <- reactive({
    df <- division_data %>% filter(division == input$division)
    
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      df %>%
        transmute(
          Horse        = horse_name,
          Shows        = shows,
          `Total Points` = total_points
        ) %>%
        arrange(desc(`Total Points`), desc(Shows))
    } else {
      df %>%
        transmute(
          Horse        = horse_name,
          Exhibitor    = exhibitor_name,
          Shows        = shows,
          `Total Points` = total_points
        ) %>%
        arrange(desc(`Total Points`), desc(Shows))
    }
  })
  
  # ------------------ RESULTS FOR CLASS TABLES ------------------ #
  filtered_results <- reactive({
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      results_data %>% filter(division %in% c(input$division, "Open"))
    } else {
      results_data %>% filter(division == input$division)
    }
  })
  
  # ------------------ COW & PERFORMANCE STANDINGS ------------------ #
  filtered_cow <- reactive({
    df <- cow_data %>% filter(division == input$division)
    
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      df %>% 
        group_by(horse_name) %>% 
        summarise(total_points = sum(total_points), .groups="drop") %>%
        arrange(desc(total_points))
    } else {
      df %>%
        group_by(horse_name, exhibitor_name) %>% 
        summarise(total_points = sum(total_points), .groups="drop") %>%
        arrange(desc(total_points))
    }
  })
  
  filtered_perf <- reactive({
    df <- perf_data %>% filter(division == input$division)
    
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      df %>% 
        group_by(horse_name) %>% 
        summarise(total_points = sum(total_points), .groups="drop") %>%
        arrange(desc(total_points))
    } else {
      df %>%
        group_by(horse_name, exhibitor_name) %>% 
        summarise(total_points = sum(total_points), .groups="drop") %>%
        arrange(desc(total_points))
    }
  })
  
  # ------------------ DISPLAY TABLES ------------------ #
  output$div_table <- renderDataTable({
    df <- filtered_division()
    if (nrow(df) == 0) return(data.frame("No matching records." = ""))
    
    datatable(
      df,
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  
  # (cow & perf tables not in UI right now, but keeping helpers)
  output$cow_table <- renderDataTable({
    df <- filtered_cow()
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      df %>% select(Horse = horse_name, `Total Points` = total_points)
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points)
    }
  })
  
  output$perf_table <- renderDataTable({
    df <- filtered_perf()
    if (input$division %in% c("Junior Horse", "Senior Horse")) {
      df %>% select(Horse = horse_name, `Total Points` = total_points)
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points)
    }
  })
  
  # ------------------ CLASS TABLES UI ------------------ #
  output$class_tables <- renderUI({
    df <- filtered_results()
    if (nrow(df) == 0) return(tags$p("No matching class results."))
    
    tagList(lapply(unique(df$class_name), function(cls) {
      class_df <- df %>% filter(class_name == cls)
      make_class_table(class_df, cls)
    }))
  })
  
  # ------------------ CLASS TABLE RENDERER ------------------ #
  make_class_table <- function(class_df, cls) {
    
    class_df <- class_df %>%
      mutate(
        shows_entered = tidyr::replace_na(shows_entered, 0),
        EligibleSort = if_else(shows_entered >= 3, "YES", "NO"),
        Eligible = if_else(EligibleSort == "YES", "✓", "✗"),
        EligibleSort = factor(EligibleSort, levels = c("YES", "NO")),
        show_key = match(show_name, show_order),
        judge_label = paste0(show_key, " – ", toupper(show_name), " – ", judge_name)
      )
    
    class_out <- class_df %>%
      group_by(horse_name, exhibitor_name, owner_name, judge_label, EligibleSort, Eligible) %>%
      summarise(
        Points = sum(points),
        Shows = max(shows_entered),
        .groups="drop"
      ) %>%
      pivot_wider(
        names_from = judge_label,
        values_from = Points,
        values_fill = 0
      )
    
    judge_cols <- grep("^\\d+ –", names(class_out), value = TRUE)
    judge_cols <- judge_cols[order(as.numeric(sub(" –.*", "", judge_cols)))]
    
    class_out <- class_out %>%
      mutate(`Total Points` = rowSums(across(all_of(judge_cols)))) %>%
      arrange(
        EligibleSort,              # ✓ first, then ✗
        desc(`Total Points`),      # higher total points first
        desc(Shows)                # tie-breaker: more shows if points equal
      ) %>%
      select(-EligibleSort) %>%
      rename(
        Horse = horse_name,
        Exhibitor = exhibitor_name,
        Owner = owner_name
      )
    
    output_id <- paste0("class_", gsub("[^a-zA-Z0-9]", "_", cls))
    
    output[[output_id]] <- renderDT({
      datatable(class_out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })
    
    tagList(
      h4(cls),
      DTOutput(output_id),
      tags$hr()
    )
  }
  
  # ------------------ EXPORT HELPERS ------------------ #
  
  # Jr / Sr / Open class-export (by horse, with exhibitor list)
  make_export_table_jr_sr <- function(class_df) {
    
    class_df <- class_df %>%
      mutate(
        shows_entered = tidyr::replace_na(shows_entered, 0),
        show_grp = paste0(match(show_name, show_order), " – ", toupper(show_name)),
        Eligible = if_else(shows_entered >= 3, "✅", "❌")
      )
    
    exhibitors <- class_df %>%
      filter(!is.na(exhibitor_name), exhibitor_name != "") %>%
      group_by(horse_name, owner_name) %>%
      summarise(
        Exhibitor = paste(sort(unique(exhibitor_name)), collapse = ", "),
        .groups = "drop"
      )
    
    wide <- class_df %>%
      group_by(horse_name, owner_name, show_grp) %>%
      summarise(
        Points = sum(points, na.rm = TRUE),
        Shows = max(shows_entered, na.rm = TRUE),
        Eligible = first(Eligible),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = show_grp,
        values_from = Points,
        values_fill = 0
      )
    
    show_cols <- grep("^\\d+\\s*–", names(wide), value = TRUE)
    show_cols_sorted <- show_cols[order(as.numeric(sub(" *–.*$", "", show_cols)))]
    
    wide %>%
      mutate(`Total Points` = rowSums(across(all_of(show_cols_sorted)), na.rm = TRUE)) %>%
      left_join(exhibitors, by = c("horse_name", "owner_name")) %>%
      select(
        Horse = horse_name,
        Owner = owner_name,
        Exhibitor,
        Shows,
        Eligible,
        all_of(show_cols_sorted),
        `Total Points`
      ) %>%
      arrange(desc(`Total Points`))
  }
  
  make_export_table_standard <- function(class_df) {
    out <- class_df %>%
      mutate(
        shows_entered = tidyr::replace_na(shows_entered, 0),
        show_grp = paste0(match(show_name, show_order), " – ", toupper(show_name)),
        Eligible = if_else(shows_entered >= 3, "✅", "❌")
      ) %>%
      group_by(horse_name, exhibitor_name, owner_name, show_grp) %>%
      summarise(
        points = sum(points, na.rm = TRUE),
        shows_entered = max(shows_entered),
        Eligible = first(Eligible),
        .groups="drop"
      ) %>%
      pivot_wider(
        names_from = show_grp,
        values_from = points,
        values_fill = 0
      )
    
    judge_cols <- grep("^\\d+ –", names(out), value = TRUE)
    
    out %>%
      mutate(`Total Points` = rowSums(across(all_of(judge_cols)), na.rm = TRUE)) %>%
      arrange(desc(`Total Points`)) %>%
      rename(
        Horse = horse_name,
        Exhibitor = exhibitor_name,
        Owner = owner_name,
        Shows = shows_entered
      )
  }
  
  make_export_overall_jr_sr <- function(df) {
    df %>%
      group_by(horse_name) %>%
      summarise(
        `Total Points` = sum(total_points, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Points`)) %>%
      rename(Horse = horse_name)
  }
  
  make_export_overall_standard <- function(df) {
    df %>%
      group_by(horse_name, exhibitor_name) %>%
      summarise(
        `Total Points` = sum(total_points, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Points`)) %>%
      rename(
        Horse = horse_name,
        Exhibitor = exhibitor_name
      )
  }
  
  make_export_perf <- function(df, division) {
    if (nrow(df) == 0) {
      return(data.frame(Message = "No performance classes for this division."))
    }
    if (division %in% c("Junior Horse", "Senior Horse")) {
      df %>%
        group_by(horse_name) %>%
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(`Total Points`)) %>%
        rename(Horse = horse_name)
    } else {
      df %>%
        group_by(horse_name, exhibitor_name) %>%
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(`Total Points`)) %>%
        rename(Horse = horse_name, Exhibitor = exhibitor_name)
    }
  }
  
  make_export_cow <- function(df, division) {
    if (nrow(df) == 0) {
      return(data.frame(Message = "No cow horse classes for this division."))
    }
    if (division %in% c("Junior Horse", "Senior Horse")) {
      df %>%
        group_by(horse_name) %>%
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(`Total Points`)) %>%
        rename(Horse = horse_name)
    } else {
      df %>%
        group_by(horse_name, exhibitor_name) %>%
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(`Total Points`)) %>%
        rename(Horse = horse_name, Exhibitor = exhibitor_name)
    }
  }
  
  # ------------------ DOWNLOAD HANDLERS ------------------ #
  output$download_division_standings <- downloadHandler(
    
    filename = function() {
      paste0("MoRHA_", gsub(" ", "_", input$division), "_Overall_Standings.xlsx")
    },
    
    content = function(file) {
      wb <- createWorkbook()
      
      # Filter the division shown in the UI
      df <- division_data %>% filter(division == input$division)
      
      # Build the export table ---------------------------------------------------
      if (input$division %in% c("Junior Horse", "Senior Horse")) {
        
        export_df <- df %>%
          transmute(
            Horse         = horse_name,
            Shows         = shows,
            `Total Points` = total_points
          ) %>%
          arrange(desc(`Total Points`), desc(Shows))
        
      } else {
        
        export_df <- df %>%
          transmute(
            Horse         = horse_name,
            Exhibitor     = exhibitor_name,
            Shows         = shows,
            `Total Points` = total_points
          ) %>%
          arrange(desc(`Total Points`), desc(Shows))
      }
      
      # Write to workbook --------------------------------------------------------
      addWorksheet(wb, "Standings")
      header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2")
      writeData(wb, "Standings", export_df)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_class_tables <- downloadHandler(
    filename = function() {
      paste0("MoRHA_", gsub(" ", "_", input$division), "_Class_Standings.xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      df <- isolate(filtered_results())
      div <- input$division
      
      # Which classes to export
      if (div %in% c("Junior Horse", "Senior Horse")) {
        classes <- unique(df$class_name[df$division %in% c(div, "Open")])
      } else {
        classes <- unique(df$class_name[df$division == div])
      }
      
      for (cls in classes) {
        class_df <- df %>% filter(class_name == cls)
        
        if (div %in% c("Junior Horse", "Senior Horse")) {
          out <- make_export_table_jr_sr(class_df)
        } else {
          out <- make_export_table_standard(class_df)
        }
        
        sheet <- substr(cls, 1, 31)
        addWorksheet(wb, sheet)
        header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2")
        writeData(wb, sheet, out)
        freezePane(wb, sheet, firstActiveRow = 2)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ------------------ TITLE ------------------ #
  output$division_title <- renderText({
    paste(input$division, "Standings")
  })
}

# ------------------- RUN APP ------------------- #
shinyApp(ui, server)

