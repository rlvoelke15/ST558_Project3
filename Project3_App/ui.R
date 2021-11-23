
library(shiny)
library(shinythemes)

# Define UI for Shiny Application
shinyUI(fluidPage(theme = shinytheme("united"),

    # Application Title with Image Embedded
    titlePanel(title=div(img(src = "nba-logo.jpg", height = 102, width = 102), "2020-2021 NBA Player Stats - Regular Season")),

    tabsetPanel(
        tabPanel("About", fluid = TRUE,
                 mainPanel(
                     br(),
                     wellPanel(h4("This application is designed to allow users to explore patterns and relationships across a variety of aggregated statistics on NBA Players for the 2020-2021 Regular Season."),
                     em("Note: This does not include Playoff Game Data."),
                     br(),
                     h4(uiOutput("homePage")),
                     "A variety of cumulative statistics, including...[insert select variables here]... is provided across 600+ active NBA players. This data will support the interactive analyses, in this application.",
                     uiOutput("dataSource"),
                     br(),
                     h4("Data Exploration Page"),
                     "This page allows the user to create various numerical and graphical summaries using the data of their choosing.",
                     h4("Modeling Page"),
                     "This page, which has multiple subsetted tabs, allows the user to model and fit the data of their choosing. It alsow allows the user to obtain a prediction for a response variable against any of their fitted models.",
                     h4("Data Page"),
                     "This page allows the user to explore the raw data set and download it (or a filtered version of it) to their local desktop."
                ))),

        tabPanel("Data Exploration", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    radioButtons("Y1", "Select a Response Variable of Interest", choices = c("Points per Game", "Offensive Rating", "Defensive Rating")),
                    selectizeInput("Team", "Select a Team", choices = c("choose" = "", levels(rawData2$TEAM)), select = "Atl"),
                    # Conditional Paneling 
                    conditionalPanel(condition = "input.Y1 != 'Points per Game'", checkboxGroupInput("Position", "Select a Position", choices = c("G", "G-F", "F", "F-C", "C"), selected = "G")),
                    conditionalPanel(condition = "input.Y1 == 'Points per Game'", selectInput("X1", "Select the Variable of Interest for X-Axis", choices = list("Rebounds per Game", "Assists per Game", "Steals per Game", "Blocks per Game", "Turnovers per Game")))),
                    mainPanel(
                        conditionalPanel(condition = "input.Y1 == 'Points per Game'", plotOutput("scatterPlot")),
                        conditionalPanel(condition = "input.Y1 != 'Points per Game'", plotOutput("barPlot"))
                        
    ))),
        tabPanel("Modeling", fluid = TRUE,
                 tabsetPanel(
                     tabPanel("Modeling Info", fluid = TRUE),
                     tabPanel("Model Fitting", fluid = TRUE),
                     tabPanel("Prediction", fluid = TRUE)
                 )),
        tabPanel("Data", fluid = TRUE)
    )
))
