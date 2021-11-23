
library(shiny)

# Define Server Logic Required to Build Application
shinyServer(function(input, output, session) {
    
    # Create Hyperlinks for About Page
    output$dataSource <- renderUI({
        url <- a(href="https://www.nbastuffer.com/2020-2021-nba-player-stats/", target="_blank","Click Here")
        tagList(url, " to access the Source Data")
    })
    
    output$homePage <- renderUI({
        url <- a(href="https://www.nbastuffer.com/", target="_blank","NBAStuffer Website,")
        tagList("This particular dataset comes from the ", url, " a reference hub that provides 'unique metrics and NBA analytics content'.")
    })

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
