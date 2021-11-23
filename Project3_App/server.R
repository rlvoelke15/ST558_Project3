
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

    output$scatterPlot <- renderPlot({
        
        filteredData <- rawData2 %>% filter(TEAM == input$Team)
        
        if(input$X1 == "Rebounds per Game"){
            g <- ggplot(filteredData, aes(x= RPG, y = PPG))
            g + geom_point()} else if(input$X1 == "Assists per Game"){
                g <- ggplot(filteredData, aes(x= APG, y = PPG))
                g + geom_point()} else if(input$X1 == "Steals per Game"){
        g <- ggplot(filteredData, aes(x= SPG, y = PPG))
        g + geom_point()} else if(input$X1 == "Blocks per Game"){
                    g <- ggplot(filteredData, aes(x= BPG, y = PPG))
                    g + geom_point()} else{
                    g <- ggplot(filteredData, aes(x= TOPG, y = PPG))
                    g + geom_point()}
    })
    
    output$barPlot <- renderPlot({
        
        filteredData <- rawData2 %>% filter(TEAM == input$Team) %>% filter(POS == input$Position)
        
        if(input$Y1 == "Offensive Rating"){
            g <- ggplot(filteredData, aes(x= `FULL NAME`, y = ORTG))
                g + geom_bar(stat = "identity")} else {
                    g <- ggplot(filteredData, aes(x= `FULL NAME`, y = DRTG))
                    g + geom_bar(stat = "identity")
                }
        
    })
})

