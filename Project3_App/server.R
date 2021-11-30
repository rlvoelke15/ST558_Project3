
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
        g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} 
      else if(input$X1 == "Assists per Game"){
        g <- ggplot(filteredData, aes(x= APG, y = PPG))
          g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} 
        else if(input$X1 == "Steals per Game"){
          g <- ggplot(filteredData, aes(x= SPG, y = PPG))
            g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)} 
          else if(input$X1 == "Blocks per Game"){
            g <- ggplot(filteredData, aes(x= BPG, y = PPG))
              g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)}  else{
              g <- ggplot(filteredData, aes(x= TOPG, y = PPG))
                g + geom_point(color = "#FF6633")+labs(y= "Points per Game", x = input$X1)}
  })
    
  output$scatterPlotAll <- renderPlot({
        
    if(input$X1 == "Rebounds per Game"){
      g <- ggplot(rawData2, aes(x= RPG, y = PPG, col = TEAM))
        g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} 
      else if(input$X1 == "Assists per Game"){
        g <- ggplot(rawData2, aes(x= APG, y = PPG, col = TEAM))
          g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} 
        else if(input$X1 == "Steals per Game"){
          g <- ggplot(rawData2, aes(x= SPG, y = PPG, col = TEAM))
            g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} 
          else if(input$X1 == "Blocks per Game"){
            g <- ggplot(rawData2, aes(x= BPG, y = PPG, col = TEAM))
              g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)} 
            else{
              g <- ggplot(rawData2, aes(x= TOPG, y = PPG, col = TEAM))
                g + geom_point(aes(fill = TEAM))+labs(y= "Points per Game", x = input$X1)}
  })
    
  output$barPlot <- renderPlot({
    filteredData <- rawData2 %>% filter(TEAM == input$Team) %>% filter(Position_New == input$Position) %>% filter(MPG >= input$Slider)
        
    if(input$Y1 == "Offensive Rating"){
      g <- ggplot(filteredData, aes(x= FullName, y = ORTG))
        g + geom_bar(stat = "identity", fill = "#FF6633")+labs(y= input$Y1, x = "Player Name")} 
      else {
        g <- ggplot(filteredData, aes(x= FullName, y = DRTG))
          g + geom_bar(stat = "identity", fill = "#FF6633")+labs(y= input$Y1, x = "Player Name")}
  })

  output$barPlotAll <- renderPlot({
    sumData <- rawData2 %>% group_by(TEAM, Position_New, MPG) %>% summarise(ORTGSum = sum(ORTG, na.rm = TRUE), DRTGSum = sum(DRTG, na.rm = TRUE)) %>% filter(Position_New == input$Position) %>% filter(MPG >= input$Slider)
        
    if(input$Y1 == "Offensive Rating"){
      g <- ggplot(sumData, aes(x= TEAM, y = ORTGSum))
        g + geom_bar(stat = "identity", aes(fill = TEAM))+labs(y= input$Y1, x = "Team")} 
      else {
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
  output$table <- renderDataTable({
    tab <- rawData2[ , c("FullName", "TEAM", "Position_New", "PPG", "RPG", "APG", "SPG", "BPG", "TOPG", "ORTG", "DRTG")]
    datatable(tab)
  })
  
  output$MLR <- renderPrint({
    rawData3 <- rawData2 %>% mutate_at(vars(ORTG:DRTG), ~replace(., is.na(.), 0))  
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      trainData <- rawData3[trainIndex,]
      testData <- rawData3[-trainIndex,]
    
      # Multiple Linear Regression Fit
      mlrFit <- lm(PPG ~ input$Preds1, data = trainData)
      mlrFit
      summary(mlrFit)}
      else if (input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        trainData <- rawData3[trainIndex,]
        testData <- rawData3[-trainIndex,]
        
        # Multiple Linear Regression Fit
        mlrFit <- lm(ORTG ~ Position_New+MPG+PPG, data = trainData)
        mlrFit
        summary(mlrFit)}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          trainData <- rawData3[trainIndex,]
          testData <- rawData3[-trainIndex,]
          
          # Multiple Linear Regression Fit
          mlrFit <- lm(DRTG ~ Position_New+MPG+PPG, data = trainData)
          mlrFit
          summary(mlrFit)
        }
  })
  
  output$MLRTest <- renderPrint({
    rawData3 <- rawData2 %>% mutate_at(vars(ORTG:DRTG), ~replace(., is.na(.), 0))  
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      trainData <- rawData3[trainIndex,]
      testData <- rawData3[-trainIndex,]
      
      # Multiple Linear Regression Fit
      mlrFit <- lm(PPG ~ RPG+APG+BPG+SPG+TOPG, data = testData)
      mlrFit
      summary(mlrFit)}
      else if (input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        trainData <- rawData3[trainIndex,]
        testData <- rawData3[-trainIndex,]
      
        # Multiple Linear Regression Fit
        mlrFit <- lm(ORTG ~ Position_New+MPG+PPG, data = testData)
        mlrFit
        summary(mlrFit)}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          trainData <- rawData3[trainIndex,]
          testData <- rawData3[-trainIndex,]
      
          # Multiple Linear Regression Fit
          mlrFit <- lm(DRTG ~ Position_New+MPG+PPG, data = testData)
          mlrFit
          summary(mlrFit)}
  })
  
  output$regTree <- renderPrint({
  # Define Tuning Parameters for Regression Tree
    cp <- 0:0.1
    df <- expand.grid(cp = cp)
        
  # Fit Regression Tree Model
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      regFit <- train(PPG ~ RPG+APG+BPG+SPG+TOPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
      regFit}
      else if(input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        regFit <- train(ORTG ~ Position_New+MPG+PPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
        regFit}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          regFit <- train(DRTG ~ Position_New+MPG+PPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
          regFit}
  })
  
  output$regTreeTest <- renderPrint({
    # Define Tuning Parameters for Regression Tree
    cp <- 0:0.1
    df <- expand.grid(cp = cp)
    
    # Fit Regression Tree Model
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      regFit <- train(PPG ~ RPG+APG+BPG+SPG+TOPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
      
      # Check Regression Tree Model on Test Data
      predReg <- predict(regFit, newdata = testData)
      postResample(testData$PPG, predReg)}
      else if(input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        regFit <- train(ORTG ~ Position_New+MPG+PPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
      
        # Check Regression Tree Model on Test Data
        predReg <- predict(regFit, newdata = testData)
        postResample(testData$PPG, predReg)}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          regFit <- train(DRTG ~ Position_New+MPG+PPG, data = trainData, method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$CV1, repeats = input$Repeats1), tuneGrid = df)
          # Check Regression Tree Model on Test Data
          predReg <- predict(regFit, newdata = testData)
          postResample(testData$PPG, predReg)}
  })
  
  output$rfTree <- renderPrint({
    
    withProgress(message = "Modeling in Progress", detail = "Please be Patient", value = 0, {
      for (i in 1:10) {
        incProgress(1/15)
        Sys.sleep(2)
      }
    })
    
    # Define Tuning Parameters for Random Forest Tree
    mtry <- 1:15
    df <- expand.grid(mtry = mtry)
    
    # Fit Random Forest Tree Model
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      rfFit <- train(PPG ~ RPG+APG+BPG+SPG+TOPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
      rfFit}
      else if(input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        rfFit <- train(ORTG ~ Position_New+MPG+PPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
        rfFit}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          rfFit <- train(DRTG ~ Position_New+MPG+PPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
          rfFit}
  })
    
  output$rfTreeTest <- renderPrint({
    # Define Tuning Parameters for Random Forest Tree
    mtry <- 1:15
    df <- expand.grid(mtry = mtry)
      
    # Fit Random Forest Tree Model
    if(input$Y2 == "PPG"){
      trainIndex <- createDataPartition(rawData3$PPG, p = input$NI, list = FALSE)
      rfFit <- train(PPG ~ RPG+APG+BPG+SPG+TOPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
      # Check Random Forest Model on Test Data
      predRF <- predict(rfFit, newdata = testData)
      postResample(testData$PPG, predRF)}
      else if(input$Y2 == "ORTG"){
        trainIndex <- createDataPartition(rawData3$ORTG, p = input$NI, list = FALSE)
        rfFit <- train(ORTG ~ Position_New+MPG+PPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
        # Check Random Forest Model on Test Data
        predRF <- predict(rfFit, newdata = testData)
        postResample(testData$PPG, predRF)}
        else{
          trainIndex <- createDataPartition(rawData3$DRTG, p = input$NI, list = FALSE)
          rfFit <- train(DRTG ~ Position_New+MPG+PPG, data = trainData, method = "rf", trControl = trainControl(method = "repeatedcv", number = input$CV2, repeats = input$Repeats2), tuneGrid = df)
          # Check Random Forest Model on Test Data
          predRF <- predict(rfFit, newdata = testData)
          postResample(testData$PPG, predRF)}
  })
})
