library(shiny)
library(bs4Dash)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(bslib)
library(shinyWidgets)

source("stat_definitions.R", local = TRUE)

checkboxGroupInput("offensive_display_stats", label = NULL, choices = NULL)

# List of offensive stats from the original with correct prefixes
offensive_stat_map <- generate_stat_map(stat_definitions$offensive)
defensive_stat_map <- generate_stat_map(stat_definitions$defensive)

offensive_groups <- generate_stat_groups(stat_definitions$offensive)
defensive_group <- generate_stat_groups(stat_definitions$defensive)

polypal <- bs_theme(
  version = 5,
  primary = "#154734",    # Dark green
  secondary = "#FDB827",  # Gold
  bg = "#ffffff",
  fg = "#212529",
  base_font = font_google("Montserrat")
)

# Merge both offensive and defensive maps for use in the Trends tab
stat_map <- c(offensive_stat_map, defensive_stat_map)

ui <- fluidPage(
  titlePanel("Cal Poly MBB Summer Tracking: 5v5 Live Play"),
  
  tabsetPanel(
    # Display Tab
    tabPanel("Stats",
             sidebarLayout(
               sidebarPanel(
                 h4(tags$strong("Select Stats to Display:", style = "margin-bottom: 15px;")),
                 #tags$div(style = "margin-bottom: 30px;",
                 #         checkboxInput("group_all_offense", label = strong("All Offensive Stats"), value = TRUE)
                 # ),
                 # Group + individual stat checkboxes
                 h5(tags$strong(tags$u("Offensive Stats:"))),
                 tags$div(style = "margin-left: 15px;",
                          lapply(names(offensive_groups), function(group) {
                            stat_id <- paste0("stats_", group)
                            
                            tagList(
                              tags$div(
                                (tags$u(group)),
                                actionButton(inputId = paste0("select_", group), label = "Select All", class = "btn-xs btn-link"),
                                actionButton(inputId = paste0("deselect_", group), label = "Deselect All", class = "btn-xs btn-link")
                              ),
                              tags$div(
                                style = "margin-left: 15px;",
                                checkboxGroupInput(stat_id, label = NULL,
                                                   choices = offensive_groups[[group]],
                                                   selected = intersect(offensive_groups[[group]], c("EPA Success Rate", "D2 Success Rate", "P5 Success Rate"))
                                )
                              )
                            )
                          })
                 ),
                 tags$div(style = "display: none;",
                          checkboxGroupInput("offensive_display_stats", label = NULL, choices = NULL)
                 ),
                 tags$div(
                   tags$strong(tags$u("Defensive Stats:")),
                   actionButton(inputId = "select_defense", label = "Select All", class = "btn-xs btn-link"),
                   actionButton(inputId = "deselect_defense", label = "Deselect All", class = "btn-xs btn-link")
                 ),
                 tags$div(
                   style = "margin-left: 15px;",
                   checkboxGroupInput("defensive_display_stats", label = NULL,
                                      choices = defensive_group[["Overall Defense"]],
                                      selected = c("Stick Hand Success Rate", "Sprint to Gap Success Rate", "MIG Success Rate")
                   )
                 )
                 ,
                 br(),
                 checkboxInput("show_opportunities", "Show Frequency (Count) Columns", value = FALSE),
                 
                 br(),
                 h4(tags$strong(("Filter Data by Date:"))),
                 h5(tags$u(style = "margin-bottom: 20px;","Offensive Files:")),
                 uiOutput("display_file_selector_off"),
                 h5(tags$u("Defensive Files:")),
                 uiOutput("display_file_selector_def"),
                 
                 br(),
                 h5(tags$u("Color Coding:")),
                 numericInput("low_threshold", "Low (Red) Threshold:", value = 50, min = 0, max = 100),
                 numericInput("high_threshold", "High (Green) Threshold:", value = 80, min = 0, max = 100),
                 
                 br()
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "output.display_data_available",
                   h3("Player Performance"),
                   DTOutput("display_table"),
                   uiOutput("team_summary_row")
                 ),
                 conditionalPanel(
                   condition = "!output.display_data_available",
                   h3("Please select a stat/category in the side panel"),
                   uiOutput("missing_file_warning")
                 ),
               )
             )
    ),
    # Counting Stats Tab
    tabPanel("Counting Stats",
             sidebarLayout(
               sidebarPanel(
                 h5("Choose offensive files:"),
                 uiOutput("counting_file_selector_off")
               ),
               mainPanel(
                 h3("Paint Touches per Possession"),
                 DTOutput("counting_stats_table")
               )
             )
    ),
    #Trends Tab
    # Trends Tab UI
    tabPanel("Trends",
             sidebarLayout(
               sidebarPanel(
                 helpText("Trends calculated from uploaded files in Offensive and Defensive tabs."),
                 selectizeInput("trend_stat", "Select Stats to Plot:", choices = NULL, multiple = TRUE),
                 checkboxGroupInput("trend_players", "Select Players/Team", choices = NULL),
                 radioButtons("trend_metric", "Metric:", choices = c("Success Rate", "Opportunities"), inline = TRUE),
                 radioButtons("trend_type", "Stat Type:", choices = c("Offensive", "Defensive"), inline = TRUE),
                 helpText("Note: File name is used as proxy for date.")
               ),
               mainPanel(
                 plotOutput("trend_plot", height = "500px")
               )
             )
    )
    
  )
)