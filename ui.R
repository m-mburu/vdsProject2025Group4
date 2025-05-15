library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(tidyverse)
library(data.table)

load("data/all_obj_shiny.rda")
list2env(all_obj_shiny, envir =.GlobalEnv)
ui <- dashboardPage(
    skin = "blue",

    ## ----- 1  HEADER -----
    dashboardHeader(title = "FC Barcelona – 2008‑2016 Analysis"),

    ## ----- 2  SIDEBAR -----
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview map",    tabName = "map",       icon = icon("map")),
            menuItem("Team trends",     tabName = "improve",   icon = icon("chart-line")),
            menuItem("Possession",      tabName = "possess",   icon = icon("percent")),
            menuItem("Opponents",       tabName = "barcaOpp",  icon = icon("futbol")),
            menuItem("Young talents",   tabName = "talents",   icon = icon("user-graduate"))
        )
    ),

    ## ----- 3  BODY -----
    dashboardBody(

        ## 3.1 MAP TAB (full‑width leaflet)
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(
                        box(width = 12, title = "European Clubs – Stadium Locations",
                            leafletOutput("euroMap", height = 600))
                    )
            ),

            ## 3.2 IMPROVED TEAMS
            tabItem(tabName = "improve",
                    fluidRow(
                        box(width = 12, title = "Most‑Improved Teams (2008‑2016)",
                            plotlyOutput("trendPlot", height = 500),
                            helpText("Hover a line to see rating change; use legend to toggle teams."))
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

            ## 3.4 OPPONENTS (faceted bars/waffle)
            tabItem(tabName = "barcaOpp",
                    fluidRow(
                        box(width = 12, title = "Barça’s Best & Worst Opponents",
                            plotlyOutput("oppFacet", height = 550))
                    )
            ),

            ## 3.5 YOUNG TALENTS (lollipop + filters)
            tabItem(tabName = "talents",
                    fluidRow(

                        column(width = 9,
                               box(width = 12, title = "Top Young Players",
                                   plotlyOutput("talentPlot", height = 550))
                        ),
                        column(width = 3,
                               sliderInput("maxAge", "Maximum age", min = 16, max = 23, value = 21),
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
