
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
          uiOutput("dataSource")
        ),
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
        )
      )
    ),
    tabPanel("Data Exploration", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          strong("Check this Box to View Statistics Across All NBA Teams"),
            br(),
            em("If unchecked, select an NBA Team from the dropdown"),
            checkboxInput("AllTeams", "All NBA Teams"),
            conditionalPanel(condition = "input.AllTeams == false", selectizeInput("Team", "Select a Team", choices = c("choose" = "" , levels(rawData2$TEAM)), selected = "Atl")),
            radioButtons("Y1", "Select a Response Variable of Interest", choices = c("Points per Game", "Offensive Rating", "Defensive Rating")),
            conditionalPanel(condition = "input.Y1 != 'Points per Game'", radioButtons("Position", "Select a Position", choices = c("Guard" = "G", "Forward" = "F", "Center" = "C"), selected = "G")),
            conditionalPanel(condition = "input.Y1 != 'Points per Game'", sliderInput("Slider", "Minutes per Game", min = 1, max = 30, value = 1, step = 5)),
            conditionalPanel(condition = "input.Y1 != 'Points per Game'", uiOutput("minPlayed")),
            conditionalPanel(condition = "input.Y1 == 'Points per Game'", selectInput("X1", "Select a Predictor Variable of Interest (X-Axis)", choices = list("Rebounds per Game", "Assists per Game", "Steals per Game", "Blocks per Game", "Turnovers per Game")))
        ),
        mainPanel(
          conditionalPanel(condition = "input.AllTeams == false & input.Y1 == 'Points per Game'", plotOutput("scatterPlot")),
          conditionalPanel(condition = "input.AllTeams == false & input.Y1 == 'Points per Game'", uiOutput("scatterPoint")),
          conditionalPanel(condition = "input.AllTeams == true & input.Y1 == 'Points per Game'", plotOutput("scatterPlotAll")),
          conditionalPanel(condition = "input.AllTeams == true & input.Y1 == 'Points per Game'", "Each Point Represents an Individual Player in the NBA"),
          conditionalPanel(condition = "input.AllTeams ==false & input.Y1 != 'Points per Game'", plotOutput("barPlot")),
          conditionalPanel(condition = "input.AllTeams ==false & input.Y1 != 'Points per Game'", uiOutput("avgMinPlayed")),
          conditionalPanel(condition = "input.AllTeams == true & input.Y1 != 'Points per Game'", plotOutput("barPlotAll")),
          conditionalPanel(condition = "input.AllTeams == true & input.Y1 != 'Points per Game'", uiOutput("barSum")),
          br(),
          DT::dataTableOutput("table")
        )
      )
    ),
    tabPanel("Modeling", fluid = TRUE,
      tabsetPanel(
        tabPanel("Modeling Info", fluid = TRUE,
          mainPanel(
            br(),
            wellPanel(
              withMathJax(),
              h3("Multiple Linear Regression Model"),
              h4("Benefits"),
              "A key benefit of using Mulitiple Linear Regression is that the user has the ability to include multiple predictor variables and/or higher order terms in their model. This provides us further insight into our data that we might not have with a Single Multiple Linear Regression Model.",
              uiOutput("math"),
              h4("Drawbacks"),
              "Multiple Linear Regression is a great starting point, but it doesn't allow us to look at response variables that are non-linear in nature. A variety of assumptions must be met for us to be able to infer meaning from our Multiple Linear Regression Model. If those assumptions cannot be met, a Multiple Linear Regression Model, may not be appropriate."
            ),
            wellPanel(
              h3("Regression Tree Model"),
              h4("Benefits"),
              "A regression tree model allows us to predict a continuous response variable using the mean of observations as our prediction and easily interpret our output. Unlike MLR, statistical assumptions don't need to be met when drawing inference from your model. ",
              h4("Drawbacks"),
              "When using a Regression Tree Model, small changes in our data can vastly our results, which you might see when playing around on the Model Fitting Tab. Our trees usually require pruning, as well, so as not to overfit out data to training set, but this may increase our bias. "
            ),
            wellPanel(
              h3("Random Forest Tree Model"),
              h4("Benefits"),
              "A Random Forest Tree Model is a great model to use when a strong predictor variable exists. Rather than relying on the strongest predictor variable in every model, a Random Forest Tree Model will randomly select the variables used in every iteration of itself.",
              h4("Drawbacks"),
              "One of the drawbacks of the Random Forest Tree Model is that it can take a significant amount of time to run and a significant amount of memory on your operating system. You will notice this as well, that it may take a couple minutes for your models to run, when playing with them on the Model Fitting Tab. "
            )
          )
        ),
        tabPanel("Model Fitting", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
              wellPanel(h4("Select the Size of Your Training Set:"),
                numericInput("NI","Enter a Value between 0 and 1", value = 0.8, min = 0, max = 1, step = 0.05),
                em("Note: Your Test Set will Default to the Remaining Obs. not used in your Training Set")
              ),
              wellPanel(h4("Select Variables of Interest:"),
                radioButtons("Y2", "Select a Response Variable", choices = c("Points per Game" = "PPG", "Offensive Rating" = "ORTG", "Defensive Rating" = "DRTG")),
              checkboxGroupInput("Preds1", "Select Predictor Variables",c("Rebouds per Game" = "RPG", "Assists per Game" = "APG", "Blocks per Game" = "BPG", "Steals per Game" = "SPG", "Turnovers per Game" = "TOPG", "Postion" = "Position_New", "Minutes per Game" = "MPG"), selected = "RPG"),
              ),
              wellPanel(h4("Select Tree Model Criterion:"),
                h5("Regression Tree Criterion:"),
                numericInput("CV1", "Select the Number of Cross Validation Folds", value = 5, min = 1, max =10, step = 1),
                numericInput("Repeats1", "Select the Number of Repetitions for Cross Validation", value = 3, min = 1, max =10, step = 1),
                br(),
                h5("Random Forest Tree Criterion:"),
                numericInput("CV2", "Select the Number of Cross Validation Folds", value = 5, min = 1, max =10, step = 1),
                numericInput("Repeats2", "Select the Number of Repetitions for Cross Validation", value = 3, min = 1, max =10, step = 1)
              ),
              wellPanel(actionButton("Run", "Fit Models")
              )
            ),
            mainPanel(
              em("Define Model Criterion and hit 'Fit Models' Button to Generate Model Fit Statistics Below"),
              h4("Multiple Linear Regression Summary Statistics - Training Data"),
              verbatimTextOutput("MLR"),
              h4("Multiple Linear Regression Summary Statistics - Test Data"),
              verbatimTextOutput("MLRTest"),
              h4("Regression Tree Summary Statistics - Training Data"),
              verbatimTextOutput("regTrain"),
              h4("Regression Tree Summary Statistics - Test Data"),
              verbatimTextOutput("regTest"),
              h4("Random Forest Tree Summary Statistics - Training Data"),
              verbatimTextOutput("rfTree"),
              h4("Random Forest Tree Summary Statistics - Test Data"),
              verbatimTextOutput("rfTreeTest")
            )
          )
        ),
        tabPanel("Prediction", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
              em("Please enter a value between 0 and 20 in each of the fields below and press 'Predict' Button to generate the estimated Points per Game (PPG) for a given player."),
              br(),
              br(),
              numericInput("Pred1", "Select the Value of RPG", value = 0, min = 0, max = 20, step = 1),
              numericInput("Pred2", "Select the Value of APG", value = 0, min = 0, max = 20, step = 1),
              numericInput("Pred3", "Select the Value of SPG", value = 0, min = 0, max = 20, step = 1),
              numericInput("Pred4", "Select the Value of BPG", value = 0, min = 0, max = 20, step = 1),
              numericInput("Pred5", "Select the Value of TOPG", value = 0, min = 0, max = 20, step = 1),
              actionButton("Predict", "Predict")
            ),
            mainPanel(
              wellPanel(h4("Prediction Model"),
                        "For this prediction, we will be looking at a version the Multiple Linear Regression Model that uses the following predictor variables:",
                        br(),
                        em("Reboounds per Game, Assists per Game, Blocks per Game, Steals per Game, and Turnovers per Game"),
                        br(),
                        br(),
                        em("Refer to Below Coefficients Used for this Calculation"),
                        br(),
                        br(),
                        verbatimTextOutput("coefficients")
              ),
              wellPanel(h4(uiOutput("prediction"))
              )
            )
          )
      )
    )
  ),
    tabPanel("Data", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          strong("Click this box if you would like to download full, unfiltered data set."),
          checkboxInput("Unfiltered", "Unfiltered Data"),
          conditionalPanel(condition = "input.Unfiltered == false", strong("If you would like to subset your data, filter it on below criteria, as desired.")),
          br(),
          conditionalPanel(condition = "input.Unfiltered == false", selectizeInput("TeamFilt", "Filter for a Specific NBA Team", choices = c("choose" = "" , levels(rawData2$TEAM)))),
          conditionalPanel(condition = "input.Unfiltered == false", radioButtons("PosFilt", "Filter for a Specific Position", choices = c("Guard" = "G", "Forward" = "F", "Center" = "C"))),
          strong("Click on the Button Below to Download your desired data set."),
          br(),
          br(),
          actionButton("Download", "Download"),
        ),
        mainPanel(
          conditionalPanel(condition = "input.Unfiltered == true", DT::dataTableOutput("downloadTable")),
          conditionalPanel(condition = "input.Unfiltered == false", DT::dataTableOutput("downloadTableFilt"))
        )
      ))
      )
))
