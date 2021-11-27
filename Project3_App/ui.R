
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
                     wellPanel(h3("About Section"),
                        "This application is designed to allow users to explore patterns and relationships across a variety of aggregated statistics on NBA Players for the 2020-2021 Regular Season.",
                     em("(Note: This does not include Playoff Game Data.)"),
                     br(),
                     br(),
                     uiOutput("homePage"),
                     br(),
                     "A variety of cumulative statistics, including Points (PPG), Rebounds (RPG), Assists (APG), Steals (SPG), Blocks (BPG), and Turnovers (TOPG), per game, as well as Team Affiliation, Position, etc. are provided across 600+ active NBA players. This data will support the interactive analyses, in this application.",
                     br(),
                     br(),
                     uiOutput("dataSource")),
                     wellPanel(h3("Navigation Section"),
                         strong("Data Exploration Page"),
                         br(),
                     "This page allows the user to create various numerical and graphical summaries using the data of their choosing.",
                     br(),
                     br(),
                     strong("Modeling Page"),
                     br(),
                     "This page, which has multiple subsetted tabs, allows the user to model and fit the data of their choosing. It also allows the user to obtain predictions for their response variable against any of their fitted models.",
                     br(),
                     br(),
                     strong("Data Page"),
                     br(),
                     "This page allows the user to explore the raw data set and download it (or a filtered version of it) to their local desktop."
                ))),

        tabPanel("Data Exploration", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    strong("Check this Box to View Statistics Across All NBA Teams"),
                    br(),
                    em("If unchecked, select an NBA Team from the dropdown"),
                    checkboxInput("AllTeams", "All NBA Teams"),
                    conditionalPanel(condition = "input.AllTeams == false", selectizeInput("Team", "Select a Team", choices = c("choose" = "", levels(rawData2$TEAM)), select = "Atl")),
                    radioButtons("Y1", "Select a Response Variable of Interest", choices = c("Points per Game", "Offensive Rating", "Defensive Rating")),
                    conditionalPanel(condition = "input.Y1 != 'Points per Game'", radioButtons("Position", "Select a Position", choices = c("Guard" = "G", "Forward" = "F", "Center" = "C"), selected = "G")),
                    conditionalPanel(condition = "input.Y1 != 'Points per Game'", sliderInput("Slider", "Minutes per Game", min = 1, max = 30, value = 1, step = 5)),
                    conditionalPanel(condition = "input.Y1 != 'Points per Game'", uiOutput("minPlayed")),
                    conditionalPanel(condition = "input.Y1 == 'Points per Game'", selectInput("X1", "Select a Predictor Variable of Interest (X-Axis)", choices = list("Rebounds per Game", "Assists per Game", "Steals per Game", "Blocks per Game", "Turnovers per Game")))
                    ),
                    mainPanel(
                        conditionalPanel(condition = "input.AllTeams == false & input.Y1 == 'Points per Game'", plotOutput("scatterPlot")),
                        conditionalPanel(condition = "input.AllTeams == true & input.Y1 == 'Points per Game'", plotOutput("scatterPlotAll")),
                        conditionalPanel(condition = "input.AllTeams ==false & input.Y1 != 'Points per Game'", plotOutput("barPlot")),
                        conditionalPanel(condition = "input.AllTeams ==false & input.Y1 != 'Points per Game'", uiOutput("avgMinPlayed")),
                        conditionalPanel(condition = "input.AllTeams == true & input.Y1 != 'Points per Game'", plotOutput("barPlotAll")),
                        br(),
                        dataTableOutput("table")
    ))),
        tabPanel("Modeling", fluid = TRUE,
                 tabsetPanel(
                     tabPanel("Modeling Info", fluid = TRUE),
                     tabPanel("Model Fitting", fluid = TRUE,
                              sidebarLayout(
                                  sidebarPanel(
                              wellPanel(radioButtons("Y2", "Select a Response Variable of Interest", choices = c("Points per Game", "Offensive Rating", "Defensive Rating")),
                              conditionalPanel(condition = "input.Y2 == 'Points per Game'", checkboxGroupInput("Preds1", "Select Predictor Variables",c("RPG", "APG", "BPG", "SPG", "TOPG"))),
                              conditionalPanel(condition = "input.Y2 != 'Points per Game'", checkboxGroupInput("Preds2", "Select Predictor Variables", c("Postion", "MPG", "PPG")))),
                              br(),
                              wellPanel(h4("Select the Size of Your Training Set:"),
                              numericInput("NI","Enter a Number between 0 and 1", value = 0.8, min = 0, max = 1, step = 0.05),
                              em("Note: Your Test Set will Default to the Remaining Obs. not used in your Training Set"),
                              br(),
                              numericInput("CV", "Select the Number of Cross Validation Folds", value = 5, min = 1, max =10, step = 1),
                              numericInput("Repeats", "Select the Number of Repetitions for Cross Validation", value = 3, min = 1, max =10, step = 1)),
                              ),
                              mainPanel(
                                  h3("Training Data Set"),
                                  dataTableOutput("training")
                              ))),
                     tabPanel("Prediction", fluid = TRUE)
                 )),
        tabPanel("Data", fluid = TRUE,
            # App title
            titlePanel("Download Data"),
                 
            # Sidebar Layout
                 sidebarLayout(
                    sidebarPanel(
                        
                         # Input: Choose a Data Set
                         checkboxInput("fulldata", "Select Full Data Set", value = TRUE),
                         conditionalPanel(condition = "input.fulldata == false", selectizeInput("dataset", "Choose a Data Set:", choices = c("choose" = "", levels(rawData2$TEAM)), select = "Atl"),
                         # Button
                         actionButton("downloadData", "Download")
                     )),
                     # Main panel for displaying outputs
                     mainPanel(
                         conditionalPanel(condition = "input.fulldata == true", dataTableOutput("fullDownload")),
                         conditionalPanel(condition = "input.fulldata == false", dataTableOutput("download"))
    )
)))))
