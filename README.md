# ST558_Project3

This was was built to allow users to explore the 2020-2021 NBA Regular Season Player Statistics. As a passion of mine, I find it quite enjoyable as an end-user to be able to explore metrics of my own interest, and dynamically investigate how they are related to one another. This application allows you to do just that!

Users of this application can explore a variety of metrics across 30+ NBA Teams including, but not limited to, Points per Game, Minutes per Game, Turnovers per Game, etc. Users can also explore the relationships between these variables by modeling them across three different statistical mdoels: A multiple linear regression model, a regression tree model, and a random forest model.

In order to use this app, please make sure you install the following packages in R: 

    1. shiny 
    2. shinythemes
    3. tidyverse
    4. DT
    5. caret
    
The user should also download the rawData2.csv file to their Working Directory in order to successfully run this application and run the following code: 

`library(shiny)`
`library(shinythemes)`
`library(tidyverse)`
`library(DT)`
`library(caret)`

`rawData2$TEAM <- as.factor(rawData2$TEAM)`
`rawData2$Position_New <- as.factor(rawData2$Position_New)`

Once the above code has been run, the user can run the following code in their R console to access this application!

`shiny::runGitHub("ST558_Project3", "rlvoelke15", ref = "main", subdir = "Project3_App")`

Please note I have also uploaded a separate R Markdown File with the Data Cleansing performed to clean up the original raw data (i.e. rename variables appropriately and create additional variable(s) to support more in-depth analysis. 
