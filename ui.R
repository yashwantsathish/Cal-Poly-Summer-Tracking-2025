library(shiny)
library(bs4Dash)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyWidgets)

offensive_groups <- list(
  "Cognition" = c("B1 Success Rate", "D2 Success Rate", "EPA Success Rate", 
                  "P5 Success Rate", "Screener Success Rate", "Handler Success Rate"),
  "Spacing" = c("Cut Success Rate", "Pull Behind Success Rate", 
                "Empty Out Success Rate", "Squeeze Success Rate"),
  "Process" = c("Outside Foot Success Rate", "JJ Success Rate", "Stampede Success Rate")
)
defensive_group <- list(
  "Overall Defense" = c("Stick Hand Success Rate", "Sprint to Gap Success Rate", "MIG Success Rate", 
                        "X Out Success Rate", "Crackdown Success Rate", "Attack the Back Success Rate")
)

checkboxGroupInput("offensive_display_stats", label = NULL, choices = NULL)

# List of offensive stats from the original with correct prefixes
offensive_stat_map <- list(
  "B1" = c(plus = "player stats:B1 +", minus = "player stats:B1 -"),
  "Cut" = c(plus = "player stats:Cut +", minus = "player stats:Cut -"),
  "D2" = c(plus = "player stats:D2 +", minus = "player stats:D2 -"),
  "EPA" = c(plus = "player stats:EPA +", minus = "player stats:EPA -"),
  "Empty Out" = c(plus = "player stats:Empty Out +", minus = "player stats:Empty Out -"),
  "Exits" = c(plus = "player stats:Exits +", minus = "player stats:Exits -"),
  "Handler" = c(plus = "player stats:Handler +", minus = "player stats:Handler -"),
  "Hot Stove" = c(plus = "player stats:Hot Stove +", minus = "player stats:Hot Stove -"),
  "JJ" = c(plus = "player stats:JJ +", minus = "player stats:JJ -"),
  "Outside Foot" = c(plus = "player stats:Outside Foot +", minus = "player stats:Outside Foot -"),
  "P5" = c(plus = "player stats:P5 +", minus = "player stats:P5 -"),
  "PT" = c(plus = "player stats:PT +", minus = "player stats:PT -"),
  "Pull Behind" = c(plus = "player stats:Pull Behind +", minus = "player stats:Pull Behind -"),
  "Screener" = c(plus = "player stats:Screener +", minus = "player stats:Screener -"),
  "Squeeze" = c(plus = "player stats:Squeeze +", minus = "player stats:Squeeze -"),
  "Stampede" = c(plus = "player stats:Stampede +", minus = "player stats:Stampede -"),
  "IO3" = c(plus = "shot chart:IO3 +", minus = "shot chart:IO3 -"),
  "NIO3" = c(plus = "shot chart:NIO3 +", minus = "shot chart:NIO3 -"),
  "RZ" = c(plus = "shot chart:RZ +", minus = "shot chart:RZ -")
)

# List of defensive stats based on your data
defensive_stat_map <- list(
  "Stick Hand" = c(plus = "player stats:StickHand +", minus = "player stats:StickHand -"),
  "Sprint to Gap" = c(plus = "player stats:SprintToGap +", minus = "player stats:SprintToGap -"),
  "MIG" = c(plus = "player stats:MIG +", minus = "player stats:MIG -"),
  "X Out" = c(plus = "player stats:X Out +", minus = "player stats:X Out -"),
  "Crackdown" = c(plus = "player stats:Crackdown +", minus = "player stats:Crackdown -"),
  "Attack the Back" = c(plus = "player stats:AttackTheBack +", minus = "player stats:AttackTheBack -")
)

# Merge both offensive and defensive maps for use in the Trends tab
stat_map <- c(offensive_stat_map, defensive_stat_map)

ui <- fluidPage(
  titlePanel("Cal Poly MBB Summer Tracking"),
  
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
    
    #Trends Tab
    # Trends Tab UI
    tabPanel("Trends",
             sidebarLayout(
               sidebarPanel(
                 helpText("Trends calculated from uploaded files in Offensive and Defensive tabs."),
                 selectInput("trend_stat", "Select Stat to Plot:", choices = NULL),
                 checkboxGroupInput("trend_players", "Select Players (or leave blank for Team Total):", choices = NULL),
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