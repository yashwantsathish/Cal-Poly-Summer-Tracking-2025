library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)

# List of offensive stats from the original with correct prefixes

source("stat_definitions.R", local = TRUE)

offensive_stat_map <- generate_stat_map(stat_definitions$offensive)
defensive_stat_map <- generate_stat_map(stat_definitions$defensive)

offensive_groups <- generate_stat_groups(stat_definitions$offensive)
defensive_group <- generate_stat_groups(stat_definitions$defensive)

# Merge both offensive and defensive maps for use in the Trends tab
stat_map <- c(offensive_stat_map, defensive_stat_map)

server <- function(input, output, session) {
  
  # Function to process stats (works for both offensive and defensive)
  process_stats <- function(files, stat_map) {
    print("🚀 Entered process_stats()")
    print("Files to process:")
    print(files)
    
    if (is.null(files) || nrow(files) == 0 || !"datapath" %in% names(files)) {
      message("process_stats() received invalid or empty files input.")
      return(data.frame())
    }
    
    if (any(is.na(files$datapath))) {
      message("Some files have NA paths; removing those.")
      files <- files[!is.na(files$datapath), ]
    }
    
    if (nrow(files) == 0) {
      message("All files removed due to NA paths.")
      return(data.frame())
    }
    
    # Initialize combined data frame
    combined_stats <- NULL
    
    # Process each file
    for (i in 1:nrow(files)) {
      file_path <- files$datapath[i]
      if (is.null(file_path) || is.na(file_path) || identical(file_path, "") || !file.exists(file_path)) {
        message(paste("Skipping bad file at index", i, ":", file_path))
        next
      }
      
      # Validate file path
      if (is.null(file_path) || is.na(file_path) || identical(file_path, "") || !file.exists(file_path)) {
        next
      }
      df <- read_csv(files$datapath[i], show_col_types = FALSE)
      
      # Get player/team stat rows
      name_col <- names(df)[1]
      df_stat <- df %>%
        filter(grepl("Stat$", .[[name_col]]) | .[[name_col]] == "Team")
      
      # Optional but recommended for consistency:
      df_stat[[name_col]][df_stat[[name_col]] == "Team"] <- "Team Total"
      colnames(df_stat)[1] <- "Player"
      
      # Extract plus/minus values for each stat
      practice_stats <- data.frame(Player = df_stat$Player, stringsAsFactors = FALSE)
      
      for (stat in names(stat_map)) {
        plus_col <- stat_map[[stat]]["plus"]
        minus_col <- stat_map[[stat]]["minus"]
        
        plus_vals <- if (!is.na(plus_col) && plus_col %in% names(df_stat)) {
          as.numeric(df_stat[[plus_col]])
        } else {
          rep(0, nrow(df_stat))
        }
        
        minus_vals <- if (!is.na(minus_col) && minus_col %in% names(df_stat)) {
          as.numeric(df_stat[[minus_col]])
        } else {
          rep(0, nrow(df_stat))
        }
        
        practice_stats[[paste0(stat, " +")]] <- plus_vals
        practice_stats[[paste0(stat, " -")]] <- minus_vals
      }
      
      # Combine with previous practices
      if (is.null(combined_stats)) {
        combined_stats <- practice_stats
      } else {
        # Merge by player name, summing stats
        combined_stats <- combined_stats %>%
          full_join(practice_stats, by = "Player", suffix = c("", ".new")) %>%
          mutate(across(ends_with(".new"), ~ ifelse(is.na(.), 0, .))) %>%
          mutate(across(ends_with(" +") & !ends_with(".new"), ~ ifelse(is.na(.), 0, .))) %>%
          mutate(across(ends_with(" -") & !ends_with(".new"), ~ ifelse(is.na(.), 0, .)))
        
        # Sum the stats
        for (stat in names(stat_map)) {
          plus_col <- paste0(stat, " +")
          minus_col <- paste0(stat, " -")
          plus_new_col <- paste0(stat, " +.new")
          minus_new_col <- paste0(stat, " -.new")
          
          if (plus_new_col %in% names(combined_stats)) {
            combined_stats[[plus_col]] <- combined_stats[[plus_col]] + combined_stats[[plus_new_col]]
            combined_stats[[plus_new_col]] <- NULL
          }
          
          if (minus_new_col %in% names(combined_stats)) {
            combined_stats[[minus_col]] <- combined_stats[[minus_col]] + combined_stats[[minus_new_col]]
            combined_stats[[minus_new_col]] <- NULL
          }
        }
      }
    }
    
    # Calculate success rates and opportunities for combined data
    final_stats <- data.frame(Player = combined_stats$Player, stringsAsFactors = FALSE)
    
    for (stat in names(stat_map)) {
      plus_col <- paste0(stat, " +")
      minus_col <- paste0(stat, " -")
      
      plus_vals <- if (plus_col %in% names(combined_stats)) {
        combined_stats[[plus_col]]
      } else {
        rep(0, nrow(combined_stats))
      }
      
      minus_vals <- if (minus_col %in% names(combined_stats)) {
        combined_stats[[minus_col]]
      } else {
        rep(0, nrow(combined_stats))
      }
      
      total <- plus_vals + minus_vals
      success_rate <- ifelse(total > 0, round(plus_vals / total * 100, 2), 0)
      
      final_stats[[paste0(stat, " Success Rate")]] <- success_rate
      final_stats[[paste0(stat, " Opportunities")]] <- total
    }
    
    # Calculate team totals
    team_final <- data.frame(Player = "Team", stringsAsFactors = FALSE)
    
    for (stat in names(stat_map)) {
      plus_col <- paste0(stat, " +")
      
      total <- sum(combined_stats[[paste0(stat, " +")]], combined_stats[[paste0(stat, " -")]], na.rm = TRUE)
      plus <- sum(combined_stats[[plus_col]], na.rm = TRUE)
      
      success_rate <- if (total > 0) round(plus / total * 100, 2) else 0
      
      team_final[[paste0(stat, " Success Rate")]] <- success_rate
      team_final[[paste0(stat, " Opportunities")]] <- total
    }
    
    # Combine player and team data - ensure Team is always last
    player_stats <- final_stats
    final_result <- bind_rows(player_stats, team_final)
    
    return(final_result)
  }
  
  # COUNTING STATS TAB LOGIC
  
  counting_stats_data <- reactive({
    file_names <- input$counting_stats_off_files
    files_to_use <- input$offensive_files
    
    if (is.null(files_to_use)) {
      all_files <- list.files("Offense", pattern = "\\.csv$", full.names = TRUE)
      selected <- all_files[basename(all_files) %in% file_names]
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    } else {
      selected <- files_to_use$datapath[files_to_use$name %in% file_names]
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    }
    
    if (nrow(files_to_use) == 0) return(data.frame())
    
    # Read and combine all files
    combined <- lapply(files_to_use$datapath, function(path) {
      read_csv(path, show_col_types = FALSE)
    }) %>% bind_rows()
    
    name_col <- names(combined)[1]
    pt_col <- "player stats:PT +"
    
    if (!(pt_col %in% names(combined)) || !"offense_lab" %in% names(combined)) return(data.frame())
    
    players <- combined[[name_col]]
    player_names <- unique(gsub(" Stat$", "", players[grepl(" Stat$", players)]))
    
    result <- lapply(player_names, function(p) {
      row_stat <- combined[combined[[name_col]] == paste(p, "Stat"), ]
      row_main <- combined[combined[[name_col]] == p, ]
      
      pt <- sum(as.numeric(row_stat[["player stats:PT +"]]), na.rm = TRUE)
      poss <- sum(as.numeric(row_main[["offense_lab"]]), na.rm = TRUE)
      
      pt_per_poss <- if (poss > 0) pt / poss else NA
      
      data.frame(
        Player = p,
        PaintTouches = pt,
        Possessions = poss,
        PaintperPoss = round(pt_per_poss, 2)
      )
    })
    
    bind_rows(result) %>%
      arrange(desc(PaintperPoss))    
  })
  
  output$counting_stats_table <- renderDT({
    req(counting_stats_data())
    datatable(counting_stats_data(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # OFFENSIVE TAB LOGIC
  output$offensive_files_uploaded <- reactive({
    has_upload <- !is.null(input$offensive_files)
    has_default <- length(list.files("Offense", pattern = "\\.csv$")) > 0
    has_upload || has_default
  })
  outputOptions(output, "offensive_files_uploaded", suspendWhenHidden = FALSE)
  
  output$offensive_file_list <- renderText({
    if (!is.null(input$offensive_files)) {
      paste(input$offensive_files$name, collapse = "\n")
    }
  })
  
  offensive_processed_data <- reactive({
    file_names <- input$display_off_files
    files_to_use <- input$offensive_files
    
    if (is.null(files_to_use)) {
      all_files <- list.files("Offense", pattern = "\\.csv$", full.names = TRUE)
      display_files <- input$display_off_files
      if (is.null(display_files)) {
        display_files <- basename(all_files)
      }
      selected <- all_files[basename(all_files) %in% display_files]
      
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    } else {
      selected <- files_to_use$datapath[files_to_use$name %in% file_names]
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    }
    
    process_stats(files_to_use, offensive_stat_map)
  })
  
  output$offensive_table <- renderDT({
    data <- offensive_processed_data() %>% arrange(ifelse(Player == "Team", 1, 0))
    
    
    datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        order = list(list(0, 'asc')),  # Default sort by Player column
        columnDefs = list(
          list(
            targets = 0,  # Player column
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'type' || type === 'sort') {",
              "    if (data === 'Team') {",
              "      return 'zzzzzzzzz';",
              "    }",
              "  }",
              "  return data;",
              "}"
            )
          ),
          list(
            targets = 1,  # Apply to all other columns (numeric)
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'type' || type === 'sort') {",
              "    if (row[0] === 'Team') {",
              "      return 999999999;",
              "    }",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      class = 'stripe hover order-column',
      extensions = c('FixedColumns', 'Buttons')
    ) %>%
      formatStyle(
        'Player',
        target = 'row',
        backgroundColor = styleEqual('Team', '#f0f0f0')
      )
  })
  
  # DEFENSIVE TAB LOGIC
  output$defensive_files_uploaded <- reactive({
    has_upload <- !is.null(input$defensive_files)
    has_default <- length(list.files("Defense", pattern = "\\.csv$")) > 0
    has_upload || has_default
  })
  outputOptions(output, "defensive_files_uploaded", suspendWhenHidden = FALSE)
  
  output$defensive_file_list <- renderText({
    if (!is.null(input$defensive_files)) {
      paste(input$defensive_files$name, collapse = "\n")
    }
  })
  
  defensive_processed_data <- reactive({
    file_names <- input$display_def_files
    files_to_use <- input$defensive_files
    
    if (is.null(files_to_use)) {
      all_files <- list.files("Defense", pattern = "\\.csv$", full.names = TRUE)
      selected <- all_files[basename(all_files) %in% file_names]
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    } else {
      selected <- files_to_use$datapath[files_to_use$name %in% file_names]
      files_to_use <- data.frame(datapath = selected, stringsAsFactors = FALSE)
    }
    
    process_stats(files_to_use, defensive_stat_map)
  })
  
  output$defensive_table <- renderDT({
    data <- defensive_processed_data()
    
    datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        order = list(list(0, 'asc')),  # Default sort by Player column
        columnDefs = list(
          list(
            targets = 0,  # Player column
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'type' || type === 'sort') {",
              "    if (data === 'Team') {",
              "      return 'zzzzzzzzz';",
              "    }",
              "  }",
              "  return data;",
              "}"
            )
          ),
          list(
            targets = 1,  # Apply to all other columns (numeric)
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'type' || type === 'sort') {",
              "    if (row[0] === 'Team') {",
              "      return 999999999;",
              "    }",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      class = 'stripe hover order-column',
      extensions = c('FixedColumns', 'Buttons')
    ) %>%
      formatStyle(
        'Player',
        target = 'row',
        backgroundColor = styleEqual('Team', '#f0f0f0')
      )
  })
  
  # --- DISPLAY TAB GROUPED CHECKBOX LOGIC ---
  
  # Track whether group checkbox toggle was user-initiated
  user_group_trigger <- reactiveValues()
  for (group in names(offensive_groups)) {
    user_group_trigger[[group]] <- FALSE
  }
  
  # 1. When a group checkbox is clicked, select or deselect its group of stats
  lapply(names(offensive_groups), function(group) {
    group_id <- paste0("group_", group)
    stat_id <- paste0("stats_", group)
    
    observeEvent(input[[group_id]], {
      user_group_trigger[[group]] <- TRUE
      
      updateCheckboxGroupInput(
        session,
        inputId = stat_id,
        selected = if (isTRUE(input[[group_id]])) offensive_groups[[group]] else character(0)
      )
    }, ignoreInit = TRUE)
  })
  
  # 2. When stat selections change, update corresponding group checkbox
  lapply(names(offensive_groups), function(group) {
    stat_id <- paste0("stats_", group)
    group_id <- paste0("group_", group)
    
    observe({
      selected <- input[[stat_id]]
      all_choices <- offensive_groups[[group]]
      
      # Determine if all are selected
      is_all_selected <- setequal(selected, all_choices)
      
      # If not triggered by group checkbox, sync group checkbox
      if (!isTRUE(user_group_trigger[[group]])) {
        updateCheckboxInput(session, group_id, value = is_all_selected)
      }
      
      user_group_trigger[[group]] <- FALSE  # reset
    })
  })
  
  # Observe select/deselect buttons for each offensive group
  lapply(names(offensive_groups), function(group) {
    stat_id <- paste0("stats_", group)
    observeEvent(input[[paste0("select_", group)]], {
      updateCheckboxGroupInput(session, stat_id, selected = offensive_groups[[group]])
    })
    observeEvent(input[[paste0("deselect_", group)]], {
      updateCheckboxGroupInput(session, stat_id, selected = character(0))
    })
  })
  
  # 3. Unified input for table filtering
  observe({
    selected_stats <- unlist(lapply(names(offensive_groups), function(group) {
      input[[paste0("stats_", group)]]
    }))
    updateCheckboxGroupInput(
      session,
      inputId = "offensive_display_stats",
      choices = unlist(offensive_groups),
      selected = selected_stats
    )
  }, priority = 10)
  
  
  # Handle defense select/deselect
  observeEvent(input$select_defense, {
    user_group_trigger$defense <- TRUE
    updateCheckboxGroupInput(
      session,
      inputId = "defensive_display_stats",
      selected = defensive_group[["Overall Defense"]]
    )
  })
  
  
  observeEvent(input$deselect_defense, {
    user_group_trigger$defense <- TRUE
    updateCheckboxGroupInput(
      session,
      inputId = "defensive_display_stats",
      selected = character(0)
    )
  })
  
  
  output$display_data_available <- reactive({
    offense_available <- !is.null(input$offensive_files) || length(list.files("Offense", pattern = "\\.csv$")) > 0
    print("Offense Available")
    print(offense_available)
    defense_available <- !is.null(input$defensive_files) || length(list.files("Defense", pattern = "\\.csv$")) > 0
    
    (offense_available && length(input$offensive_display_stats) > 0) ||
      (defense_available && length(input$defensive_display_stats) > 0)
  })
  outputOptions(output, "display_data_available", suspendWhenHidden = FALSE)
  
  display_processed_data <- reactive({
    offense_available <- !is.null(input$offensive_files) || length(list.files("Offense", pattern = "\\.csv$")) > 0
    defense_available <- !is.null(input$defensive_files) || length(list.files("Defense", pattern = "\\.csv$")) > 0
    
    req(
      (offense_available && length(input$offensive_display_stats) > 0 && length(input$display_off_files) > 0) ||
        (defense_available && length(input$defensive_display_stats) > 0 && length(input$display_def_files) > 0)
    )
    
    display_data <- NULL
    
    # Get offensive data if available and files are selected
    if (offense_available && length(input$offensive_display_stats) > 0 && length(input$display_off_files) > 0) {
      off_data <- offensive_processed_data()
      selected_off_cols <- c("Player")
      
      for (stat in input$offensive_display_stats) {
        selected_off_cols <- c(selected_off_cols, stat)
        if (input$show_opportunities) {
          opp_col <- gsub("Success Rate", "Opportunities", stat)
          if (opp_col %in% names(off_data)) {
            selected_off_cols <- c(selected_off_cols, opp_col)
          }
        }
      }
      
      display_data <- off_data %>% select(any_of(selected_off_cols))
    }
    
    # Get defensive data if available and files are selected
    if (defense_available && length(input$defensive_display_stats) > 0 && length(input$display_def_files) > 0) {
      def_data <- defensive_processed_data()
      selected_def_cols <- c("Player")
      
      for (stat in input$defensive_display_stats) {
        selected_def_cols <- c(selected_def_cols, stat)
        if (input$show_opportunities) {
          opp_col <- gsub("Success Rate", "Opportunities", stat)
          if (opp_col %in% names(def_data)) {
            selected_def_cols <- c(selected_def_cols, opp_col)
          }
        }
      }
      
      def_display <- def_data %>% select(any_of(selected_def_cols))
      
      if (!is.null(display_data)) {
        display_data <- display_data %>%
          full_join(def_display, by = "Player")
      } else {
        display_data <- def_display
      }
    }
    
    # Final safeguard: ensure display_data has a Player column even if empty
    if (is.null(display_data)) {
      display_data <- data.frame(Player = character(0))
    }
    
    # Separate player rows and team row
    team_row <- display_data %>% filter(Player == "Team")
    players_data <- display_data %>% filter(Player != "Team")
    
    return(list(players = players_data, team = team_row))
  })
  
  
  
  display_file_choices <- reactive({
    off_files <- if (!is.null(input$offensive_files)) {
      input$offensive_files$name
    } else {
      list.files("Offense", pattern = "\\.csv$")
    }
    
    def_files <- if (!is.null(input$defensive_files)) {
      input$defensive_files$name
    } else {
      list.files("Defense", pattern = "\\.csv$")
    }
    
    list(off = off_files, def = def_files)
  })
  
  output$missing_file_warning <- renderUI({
    if (!is.null(input$offensive_display_stats) &&
        length(input$offensive_display_stats) > 0 &&
        (is.null(input$offensive_files) && length(list.files("Offense", pattern = "\\.csv$")) == 0)) {
      return(tags$div(style = "color: red;", "⚠️ You selected offensive stats, but no offensive file is selected."))
    }
    
    if (!is.null(input$defensive_display_stats) &&
        length(input$defensive_display_stats) > 0 &&
        (is.null(input$defensive_files) && length(list.files("Defense", pattern = "\\.csv$")) == 0)) {
      return(tags$div(style = "color: red;", "⚠️ You selected defensive stats, but no defensive file is selected."))
    }
    
    return(NULL)
  })
  
  
  output$display_file_selector <- renderUI({
    choices <- display_file_choices()
    tagList(
      h5("Select Offensive Files to Use:"),
      checkboxGroupInput("display_off_files", NULL, choices = choices$off, selected = choices$off),
      h5("Select Defensive Files to Use:"),
      checkboxGroupInput("display_def_files", NULL, choices = choices$def, selected = choices$def)
    )
  })
  
  output$display_file_selector_off <- renderUI({
    checkboxGroupInput("display_off_files", NULL, 
                       choices = display_file_choices()$off, 
                       selected = display_file_choices()$off)
  })
  
  output$counting_file_selector_off <- renderUI({
    checkboxGroupInput("counting_stats_off_files", NULL, 
                       choices = display_file_choices()$off, 
                       selected = display_file_choices()$off)
  })
  
  output$display_file_selector_def <- renderUI({
    checkboxGroupInput("display_def_files", NULL, 
                       choices = display_file_choices()$def, 
                       selected = display_file_choices()$def)
  })
  
  output$team_summary_row <- renderUI({
    team_data <- display_processed_data()$team
    if (nrow(team_data) == 0) return(NULL)
    
    low <- input$low_threshold
    high <- input$high_threshold
    
    # Build HTML table
    header <- paste0("<tr>", paste0("<th>", colnames(team_data), "</th>", collapse = ""), "</tr>")
    
    team_row <- apply(team_data, 1, function(row) {
      paste0(
        "<tr>",
        paste0(
          mapply(function(val, colname) {
            if (colname == "Player") {
              return(paste0("<td style='background-color:#e6e6e6; font-weight:bold;'>", val, "</td>"))
            }
            if (grepl("Success Rate$", colname)) {
              val_num <- suppressWarnings(as.numeric(val))
              
              bg <- if (is.na(val_num)) {
                "#ffffff"
              } else if (val_num < input$low_threshold) {
                "#ffcccb"
              } else if (val_num < input$high_threshold) {
                "#ffe4b5"
              } else {
                "#90ee90"
              }
              
              display_val <- if (is.na(val_num)) {
                ""
              } else {
                paste0(round(val_num), "%")
              }
              
              return(paste0("<td style='background-color:", bg, "; font-weight:bold;'>", display_val, "</td>"))
            }
            return(paste0("<td>", val, "</td>"))
          }, row, colnames(team_data)),
          collapse = ""
        ),
        "</tr>"
      )
    })
    
    table_html <- paste0(
      "<div style='margin-top: 20px;'>",
      "<h4><strong><u>Team Summary</u></strong></h4>",
      "<table class='table table-bordered table-striped' style='width:100%; text-align:center;'>",
      header,
      team_row,
      "</table>",
      "</div>"
    )
    
    HTML(table_html)
  })
  
  
  output$display_table <- renderDT({
    raw_data <- display_processed_data()$players %>% arrange(Player)
    success_rate_cols <- grep("Success Rate$", names(raw_data), value = TRUE)
    
    dt <- datatable(
      raw_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        order = list(),
        columnDefs = lapply(which(names(raw_data) %in% success_rate_cols), function(i) {
          list(
            targets = i - 1,  # 0-based indexing in JS
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    if (data === null || data === '') return '';",
              "    var rounded = Math.round(parseFloat(data));",
              "    return isNaN(rounded) ? '' : rounded + '%';",
              "  }",
              "  return data;",  # for filtering/sorting use raw value
              "}"
            )
          )
        })
      ),
      rownames = FALSE,
      class = 'stripe hover order-column',
      extensions = c('FixedColumns', 'Buttons')
    )
    
    # Apply coloring on raw numeric values
    for (col in success_rate_cols) {
      dt <- formatStyle(
        dt,
        columns = col,
        backgroundColor = styleInterval(
          c(input$low_threshold, input$high_threshold),
          c('#ffcccb', '#ffe4b5', '#90ee90')
        ),
        color = 'black',
        fontWeight = 'bold'
      )
    }
    
    return(dt)
  })
  
  
  # Download handler for display table
  output$download_display <- downloadHandler(
    filename = function() {
      paste("team_performance_display_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(display_processed_data(), file, row.names = FALSE)
    }
  )
  
  # Trends Tab Logic
  
  trend_data <- reactive({
    if (input$trend_type == "Offensive") {
      files <- input$offensive_files
      stat_map <- offensive_stat_map
      if (is.null(files)) {
        default_files <- list.files("Offense", pattern = "\\.csv$", full.names = TRUE)
        files <- data.frame(datapath = default_files, name = basename(default_files), stringsAsFactors = FALSE)
      }
    } else {
      files <- input$defensive_files
      stat_map <- defensive_stat_map
      if (is.null(files)) {
        default_files <- list.files("Defense", pattern = "\\.csv$", full.names = TRUE)
        files <- data.frame(datapath = default_files, name = basename(default_files), stringsAsFactors = FALSE)
      }
    }
    
    all_data <- list()
    for (i in 1:nrow(files)) {
      df <- process_stats(data.frame(datapath = files$datapath[i]), stat_map)
      df$Date <- basename(files$name[i])
      all_data[[i]] <- df
    }
    
    full_data <- bind_rows(all_data)
    full_data$Date <- factor(full_data$Date, levels = unique(full_data$Date))
    return(full_data)
  })
  
  # Trend Plot User Interaction Detection
  observe({
    df <- trend_data()
    stat_choices <- grep("Success Rate$", names(df), value = TRUE)
    updateSelectInput(inputId = "trend_stat", choices = stat_choices)
  })
  
  observe({
    df <- trend_data()
    players <- unique(df$Player)
    
    # Move "Team" to the end
    if ("Team" %in% players) {
      ordered_players <- c(setdiff(players, "Team"), "Team")
    } else {
      ordered_players <- players
    }
    
    default_selection <- if ("Team" %in% ordered_players) "Team" else ordered_players[1]
    
    updateCheckboxGroupInput(
      inputId = "trend_players",
      choices = ordered_players,
      selected = default_selection
    )
  })
  
  
  output$trend_plot <- renderPlot({
    df <- trend_data()
    req(input$trend_stat, input$trend_metric)
    
    selected_players <- input$trend_players
    metric_type <- input$trend_metric  # "Success Rate" or "Opportunities"
    
    # Build correct column names based on selected metric
    selected_stats <- input$trend_stat
    stat_cols <- gsub(" Success Rate$| Opportunities$", "", selected_stats)  # get base stat names
    full_stat_names <- paste0(stat_cols, " ", metric_type)
    
    # Filter and reshape to long format
    plot_df <- df %>%
      filter(Player %in% selected_players) %>%
      select(Player, Date, all_of(full_stat_names)) %>%
      tidyr::pivot_longer(
        cols = all_of(full_stat_names),
        names_to = "Stat",
        values_to = "Value"
      )
    
    ggplot(plot_df, aes(x = Date, y = Value, color = Stat, linetype = Player, group = interaction(Stat, Player))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = paste("Trend:", metric_type),
        x = "Date",
        y = metric_type
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  
  
  
  
}