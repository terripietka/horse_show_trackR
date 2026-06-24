# ------------------- Server Logic ------------------- #
server <- function(input, output, session) {
  
  # ----------------- Show Order Setup ----------------- #
  show_order <- c("March", "April", "October")
  
  # Build consistent judge order based on show order
  judge_levels <- results_data %>%
    mutate(
      show_name = factor(show_name, levels = show_order),
      judge_name = paste0(show_name, "<br>", judge_name)
    ) %>%
    arrange(show_name, judge_name) %>%
    distinct(judge_name) %>%
    pull(judge_name)
  
  # ----------------- Reactive Filters ----------------- #
  
  filtered_division <- reactive({
    df <- division_data %>% filter(division == input$division)
    
    if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% 
        group_by(horse_name) %>% 
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
        mutate(Exhibitor = NA_character_)
    } else {
      df %>% 
        group_by(horse_name, exhibitor_name) %>% 
        summarise(`Total Points` = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  filtered_results <- reactive({
    # Fix AA class naming → merge into Jr/Sr
    df <- results_data %>%
      mutate(
        division = case_when(
          str_detect(class_name, regex("^AA\\s*Sr|AA\\s*Senior|AA\\s*SR", ignore_case = TRUE)) ~ "Sr Horse",
          str_detect(class_name, regex("^AA\\s*Jr|AA\\s*Junior|AA\\s*JR", ignore_case = TRUE)) ~ "Jr Horse",
          TRUE ~ division
        )
      )
    
    if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% filter(division %in% c(input$division, "Open"))
    } else {
      df %>% filter(division == input$division)
    }
  })
  
  filtered_cow <- reactive({
    df <- cow_data %>% filter(division == input$division)
    
    if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% group_by(horse_name) %>% summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    } else {
      df %>% group_by(horse_name, exhibitor_name) %>% summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  filtered_perf <- reactive({
    df <- perf_data %>% filter(division == input$division)
    
    if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% group_by(horse_name) %>% summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    } else {
      df %>% group_by(horse_name, exhibitor_name) %>% summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")
    }
  })
  
  # ----------------- Overall Division Table ----------------- #
  
  output$div_table <- renderDataTable({
    df <- filtered_division()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    df_display <- if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% select(Horse = horse_name, `Total Points`) %>% arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points`) %>% arrange(desc(`Total Points`))
    }
    
    datatable(df_display, options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'), rownames = FALSE)
  })
  
  output$cow_table <- renderDataTable({
    df <- filtered_cow()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    df_display <- if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% select(Horse = horse_name, `Total Points` = total_points) %>% arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points) %>% arrange(desc(`Total Points`))
    }
    
    datatable(df_display, options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'), rownames = FALSE)
  })
  
  output$perf_table <- renderDataTable({
    df <- filtered_perf()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    
    df_display <- if (input$division %in% c("Jr Horse", "Sr Horse")) {
      df %>% select(Horse = horse_name, `Total Points` = total_points) %>% arrange(desc(`Total Points`))
    } else {
      df %>% select(Horse = horse_name, Exhibitor = exhibitor_name, `Total Points` = total_points) %>% arrange(desc(`Total Points`))
    }
    
    datatable(df_display, options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'), rownames = FALSE)
  })
  
  # ----------------- Class Tables ----------------- #
  output$class_tables <- renderUI({
    df <- filtered_results()
    if (nrow(df) == 0) return(tags$p("No matching class results."))
    
    out <- list()
    is_jr_or_sr <- input$division %in% c("Jr Horse", "Sr Horse")
    
    if (is_jr_or_sr) {
      jr_sr_df <- df %>% filter(division == input$division)
      open_df <- df %>% filter(division == "Open")
      
      for (cls in unique(jr_sr_df$class_name)) {
        out <- append(out, list(make_class_table_jr_sr(jr_sr_df %>% filter(class_name == cls), cls)))
      }
      
      if (nrow(open_df) > 0) {
        out <- append(out, list(tags$h4("These open classes may also include junior or senior horses:"), tags$hr()))
        for (cls in unique(open_df$class_name)) {
          out <- append(out, list(make_class_table_jr_sr(open_df %>% filter(class_name == cls), cls)))
        }
      }
    } else {
      for (cls in unique(df$class_name)) {
        out <- append(out, list(make_class_table_standard(df %>% filter(class_name == cls), cls)))
      }
    }
    
    do.call(tagList, out)
  })
  
  # ----------------- Helper Functions ----------------- #
  
  make_class_table_jr_sr <- function(class_df, cls) {
    class_df <- class_df %>%
      mutate(judge_name = paste0(show_name, "<br>", judge_name),
             judge_name = factor(judge_name, levels = judge_levels),
             Eligible = if_else(shows_entered >= 3, "✅", "❌")) %>%
      group_by(horse_name, judge_name, shows_entered, Eligible) %>%
      summarise(points = sum(points, na.rm = TRUE)) %>%
      pivot_wider(names_from = judge_name, values_from = points, values_fill = 0) %>%
      mutate(`Total Points` = rowSums(across(where(is.numeric)))) %>%
      rename(Horse = horse_name, Shows = shows_entered) %>%
      arrange(desc(Shows), desc(`Total Points`))
    
    id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", cls))
    output[[id]] <- renderDT(datatable(class_df, escape = FALSE,
                                       options = list(pageLength = 20, scrollX = TRUE, dom = 't'),
                                       rownames = FALSE))
    tagList(h4(cls), DTOutput(id), tags$hr())
  }
  
  make_class_table_standard <- function(class_df, cls) {
    class_df <- class_df %>%
      mutate(judge_name = paste0(show_name, "<br>", judge_name),
             judge_name = factor(judge_name, levels = judge_levels),
             Eligible = if_else(shows_entered >= 3, "✅", "❌")) %>%
      group_by(horse_name, exhibitor_name, judge_name, shows_entered, Eligible) %>%
      summarise(points = sum(points, na.rm = TRUE)) %>%
      pivot_wider(names_from = judge_name, values_from = points, values_fill = 0) %>%
      mutate(`Total Points` = rowSums(across(where(is.numeric)))) %>%
      rename(Horse = horse_name, Exhibitor = exhibitor_name, Shows = shows_entered) %>%
      arrange(desc(Shows), desc(`Total Points`))
    
    id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", cls))
    output[[id]] <- renderDT(datatable(class_df, escape = FALSE,
                                       options = list(pageLength = 20, scrollX = TRUE, dom = 't'),
                                       rownames = FALSE))
    tagList(h4(cls), DTOutput(id), tags$hr())
  }
  
  output$division_title <- renderText({
    paste(input$division, "Composite Standings")
  })
}
