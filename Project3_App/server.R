
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
        tagList("This dataset comes from the ", url, " a reference hub that provides 'unique metrics and NBA analytics content'.")
    })
    
    output$scatterPlot <- renderPlot({
        
        filteredData <- rawData2 %>% filter(TEAM == input$Team)
        
        if(input$X1 == "Rebounds per Game"){
            g <- ggplot(filteredData, aes(x= RPG, y = PPG))
            g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Assists per Game"){
                g <- ggplot(filteredData, aes(x= APG, y = PPG))
                g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Steals per Game"){
        g <- ggplot(filteredData, aes(x= SPG, y = PPG))
        g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Blocks per Game"){
                    g <- ggplot(filteredData, aes(x= BPG, y = PPG))
                    g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} else{
                    g <- ggplot(filteredData, aes(x= TOPG, y = PPG))
                    g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)}
    })
    
    output$scatterPlotAll <- renderPlot({
        
        if(input$X1 == "Rebounds per Game"){
            g <- ggplot(rawData2, aes(x= RPG, y = PPG, col = TEAM))
            g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Assists per Game"){
                g <- ggplot(rawData2, aes(x= APG, y = PPG, col = TEAM))
                g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Steals per Game"){
                    g <- ggplot(rawData2, aes(x= SPG, y = PPG, col = TEAM))
                    g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} else if(input$X1 == "Blocks per Game"){
                        g <- ggplot(rawData2, aes(x= BPG, y = PPG, col = TEAM))
                        g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} else{
                            g <- ggplot(rawData2, aes(x= TOPG, y = PPG, col = TEAM))
                            g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)}
    })
    
    output$barPlot <- renderPlot({
        
        filteredData <- rawData2 %>% filter(TEAM == input$Team) %>% filter(Position_New == input$Position) %>% filter(MPG >= input$Slider)
        
        if(input$Y1 == "Offensive Rating"){
            g <- ggplot(filteredData, aes(x= FullName, y = ORTG))
                g + geom_bar(stat = "identity", fill = "#FF6633")+labs(y= input$Y1, x = "Player Name")} else {
                    g <- ggplot(filteredData, aes(x= FullName, y = DRTG))
                    g + geom_bar(stat = "identity", fill = "#FF6633")+labs(y= input$Y1, x = "Player Name")
                }
    })

    output$barPlotAll <- renderPlot({
        
        sumData <- rawData2 %>% group_by(TEAM, Position_New, MPG) %>% summarise(ORTGSum = sum(ORTG, na.rm = TRUE), DRTGSum = sum(DRTG, na.rm = TRUE)) %>% filter(Position_New == input$Position) %>% filter(MPG >= input$Slider)
        
        if(input$Y1 == "Offensive Rating"){
            g <- ggplot(sumData, aes(x= TEAM, y = ORTGSum))
            g + geom_bar(stat = "identity", aes(fill = TEAM))+labs(y= input$Y1, x = "Team")} else {
                g <- ggplot(sumData, aes(x= TEAM, y = DRTGSum))
                g + geom_bar(stat = "identity", aes(fill = TEAM))+labs(y= input$Y1, x = "Team")}
    })
    
    output$minPlayed <- renderUI({
        
        text1 <- "Note: Bar Chart includes players that average "
        text2 <- " minute(s) per game or more."
        paste(text1, input$Slider, text2)
        
    })
    
    output$avgMinPlayed <- renderUI({
        
        filteredData <- rawData2 %>% filter(TEAM == input$Team) %>% filter(Position_New == input$Position)
        
        text1 <- "The average minutes played by "
        text2 <- "s on "
        text3 <- " is "
        average <- round(mean(filteredData$MPG), digits = 2)
        paste(text1, input$Position, text2, input$Team, text3, average)
    })
    
    # Create the Data Table
    library(DT)
    
    output$table <- renderDataTable({
        
        tab <- rawData2[ , c("FullName", "TEAM", "Position_New", "PPG", "RPG", "APG", "SPG", "BPG", "TOPG", "ORTG", "DRTG")]
        datatable(tab)
    })
})
