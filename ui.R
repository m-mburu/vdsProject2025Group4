library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(tidyverse)
library(data.table)
library(DT)
library(sf)

load("data/all_obj_shiny.rda")
list2env(all_obj_shiny, envir =.GlobalEnv)
ui <- dashboardPage(
    skin = "blue",

    ## HEADER
    dashboardHeader(title = "FC Barcelona – 2008‑2016 Analysis"),

    ## SIDEBAR
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview map",    tabName = "map",       icon = icon("map")),
            menuItem("Team trends",     tabName = "improve",   icon = icon("chart-line")),
            menuItem("Possession",      tabName = "possess",   icon = icon("percent")),
            menuItem("Opponents",       tabName = "barcaOpp",  icon = icon("futbol")),
            menuItem("Young talents",   tabName = "talents",   icon = icon("user-graduate"))
        )
    ),

    ## BODY
    dashboardBody(

        ## MAP TAB
        tabItems(
          tabItem(tabName = "map",

                  ## Row 1: Map & Intro
                  fluidRow(
                    column(width = 6,
                           box(
                             width       = NULL,
                             title       = "European Countries with Football Leagues in the data",
                             solidHeader = TRUE,
                             status      = "primary",
                             height      = "600px",
                             leafletOutput("countryMap", height = "600px")
                           )
                    ),
                    column(width = 6,
                           box(
                             width       = NULL,
                             title       = "Project Description",
                             solidHeader = TRUE,
                             status      = "primary",
                             height      = "600px",

                             div(style = "height:100%; overflow-y:auto; padding-right:10px;",
                                 uiOutput("introSection")
                             )
                           )
                    )
                  ),

                  ## Row 2: Teams table below
                  fluidRow(
                    box(
                      width       = 12,
                      title       = "Clubs in Selected Country",
                      solidHeader = TRUE,
                      status      = "primary",
                      DTOutput("countryTeams", height = "450px")
                    )
                  )
          ),

            ## IMPROVED TEAMS
            tabItem(tabName = "improve",
                    fluidRow(
                        box(width = 10, title = "Most‑Improved Teams (2008‑2016)",
                            plotlyOutput("trendPlot", height = 500),


                            helpText("Hover a line to see rating change; use legend to toggle teams.")),
                        box(width = 2, title = "Filters",
                            selectInput("homeAwayChoice", "Home/Away",
                                        choices = c("both","home","away"), selected = "Both"),
                            helpText("Select home/away or both to filter the teams.")
                        )
                    )
            ),

            ## 3.3 POSSESSION
            tabItem(tabName = "possess",
                    fluidRow(
                        column(width = 3,
                               selectInput("teamSelect", "Choose team", choices = c("FC Barcelona","All teams")),
                               checkboxInput("showPoints", "Show raw points", TRUE)
                        ),
                        column(width = 9,
                               box(width = 12, title = "Possession by Match Outcome",
                                   plotlyOutput("possBox", height = 500))
                        )
                    )
            ),

            # NEW:INTEGRATION OF POSSESSION APP
            tabItem(tabName = "possess",
                    titlePanel("Possession vs. Match Outcome"),
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("league_possession", "Select League:", # Renamed to avoid conflict if 'league' is used elsewhere
                              # choices should be dynamically populated from available_leagues_with_possession
                                    choices = c("All", if(length(available_leagues_with_possession) > 0) available_leagues_with_possession else "No Leagues Available"),
                                    selected = "All"
                  ),
                  # Check if match_table_possession and season_year exist and are not all NA
                  if (exists("match_table_possession") && "season_year" %in% names(match_table_possession) && !all(is.na(match_table_possession$season_year))) {
                    sliderInput("season_range_possession", "Select Season Range:", # Renamed
                                min = min(match_table_possession$season_year, na.rm = TRUE),
                                max = max(match_table_possession$season_year, na.rm = TRUE),
                                value = c(min(match_table_possession$season_year, na.rm = TRUE), max(match_table_possession$season_year, na.rm = TRUE)),
                                sep = "" # Corrected from ...
                    )
                  } else {
                    p("Season data for possession analysis not available.")
                  },
                  if (exists("match_table_possession") && "win_margin" %in% names(match_table_possession) && !all(is.na(match_table_possession$win_margin))) {
                      sliderInput("win_margin_possession", "Select Win Margin:", # Renamed
                                min = min(match_table_possession$win_margin, na.rm = TRUE),
                                max = max(match_table_possession$win_margin, na.rm = TRUE),
                                value = c(min(match_table_possession$win_margin, na.rm = TRUE), max(match_table_possession$win_margin, na.rm = TRUE)),
                                step = 1
                    )
                  } else {
                    p("Win margin data for possession analysis not available.")
                  },
                  selectInput("plot_type_possession", "Select Plot Type:", # Renamed
                              choices = c("Violin Plot", "Beeswarm Plot"),
                              selected = "Violin Plot"
                  )
                ),
                mainPanel(
                    plotlyOutput("possessionOutcomePlot", height = "500px") # Changed ID from possBox
                )
              )
        ),
        # END OF MODIFICATION  
                    
            ## OPPONENTS (faceted bars/waffle)
            tabItem(tabName = "barcaOpp",
                    fluidRow(
                        box(width = 9, title = "Barça’s Best & Worst Opponents",
                            plotlyOutput("oppFacet", height = 550)),
                        box(width = 3, title = "Filters",
                            # home/away choice
                            radioButtons("homeAwayOpp", "Home/Away",
                                         choices = c("Both" = "both", "Home" = "home", "Away" = "away"),
                                         selected = "both"),
                            # opponent sringest weakest teams
                            selectInput("oppStrength", "Opponent Strength",
                                        choices = c("Strongest","Weakest"), selected = "Strongest")
                    )
            )),

            ## 3.5 YOUNG TALENTS (lollipop + filters)
            tabItem(tabName = "talents",
                    fluidRow(

                        column(width = 9,
                               box(width = 12, title = "Top Young Players",
                                   plotlyOutput("talentPlot", height = 550))
                        ),
                        column(width = 3,
                               sliderInput("maxAge",
                                           "Maximum age",
                                           min = 16, max = 23,
                                           value = 21),
                               ## NEW — mean-rating range
                               sliderInput("ratingRange",
                                           "Mean rating range",
                                           min   = 0,  max = 100,
                                           value = c(50, 100),
                                           step  = 1),
                               radioButtons("posFilter", "Position", choices = c("Overall","GK","DEF","MID","FWD"),
                                            inline = TRUE)
                        ),
                        column(width = 9,
                               box(width = 12, title = "Top Young Players",
                                   plotlyOutput("radarTalentPlot", height = 550))
                        )
                    )
            )
        )
    )
)
