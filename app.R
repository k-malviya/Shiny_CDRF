library(shiny)
library(shinydashboard)
library(maps)
library(DT)
library(googleVis)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(mapproj)

## global.R ##
## Here we pull the data ##
inputData=read.csv("./dataset.csv")
inputData

inputData <- rename(inputData, Location="LocationDesc")
inputData

inputData<-inputData[, -c(4, 8, 16,17)]
inputData

inputDataBO<-inputData %>%
  spread(Break_Out_Category, Break_out) %>%
  spread(CategoryID, Category)

# Create Age Data Frames
ageData <- inputDataBO %>% filter(!is.na(Age))
ageData = ageData[, -c(23, 24, 25)]
ageData <- ageData %>% filter(!is.na(Data_Value))
ageDataCD = ageData %>% filter(!is.na(C1))
ageDataRF = ageData %>% filter(!is.na(C2))

# Create Gender Data Frames
genderData <- inputDataBO %>% filter(!is.na(Gender))
genderData = genderData[, -c(22, 24, 25)]
genderData = genderData %>% filter(!is.na(Data_Value))
genderDataCD = genderData %>% filter(!is.na(C1))
genderDataRF = genderData %>% filter(!is.na(C2))

# Create Race Data Frames
raceData <- inputDataBO %>% filter(!is.na(Race))
raceData = raceData[, -c(22, 23, 24)]
raceData = raceData %>% filter(!is.na(Data_Value))
raceDataCD = raceData %>% filter(!is.na(C1))
raceDataRF = raceData %>% filter(!is.na(C2))

# Create Overall Data Frames
overallData <- inputDataBO %>% filter(!is.na(Overall))
overallData = overallData[, -c(22, 23, 25)]
overallData = overallData %>% filter(!is.na(Data_Value))
overallDataCD = overallData %>% filter(!is.na(C1))
overallDataRF = overallData %>% filter(!is.na(C2))

## app.R ##

ui <- dashboardPage(
  dashboardHeader(title = span("Heart Disease and Stroke Prevention", style = "color: white; font-weight: bold; font-size: 18px"), titleWidth = 350),
  
  ## Sidebar content
  
  dashboardSidebar(
    width = 350,
    # User Panel
    sidebarUserPanel(
      name = span("By: Kranti Malviya", style = "color: white; font-weight: bold; font-size: 14px"),
      subtitle = span("Comprehensive View of Cardiovascular Diseases & Related Prevention", style = "color: white; white-space: normal")
    ),
    sidebarMenu(
      menuItem("Age",
               tabName = "tbage",
               icon = shiny::icon('sort-numeric-up')
      ),
      menuItem("Gender",
               tabName = "tbgender",
               icon = shiny::icon('user-alt')
      ),
      menuItem("Ethnicity",
               tabName = "tbrace",
               icon = shiny::icon('globe-americas')
      ),
      menuItem("Overall",
               tabName = "tboverall",
               icon = shiny::icon('tachometer-alt')
      ),
      menuItem("Statewise Risk by Age",
               tabName = "tbStateRisk",
               icon = shiny::icon('globe-americas')
               ),
      menuItem("Compare States",
                tabName = "tbComparestates",
                icon = shiny::icon('globe-americas')
                )
      )
    ),

  ## Body content
  dashboardBody(
    # Age Tab here
    tabItems(
      tabItem(
        tabName = "tbage",
        span("Age Based Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
        fluidRow(
          box(selectizeInput( inputId = "inAge1", "Pick an age range", choices = unique(ageData$Age)), height = 120),
          box(sliderInput(inputId = "inYear1", label = "Slide to select a year", 2011, 2015, step = 1, value = 2011)),
          box(selectizeInput( inputId = "inCD1", "Cardiovascular Diseases", choices = unique(ageDataCD$Topic)), height = 120),
          box(selectizeInput( inputId = "inRF1", "Risk Factor", choices = unique(ageDataRF$Topic)), height = 120),
          box(title = span("Diseases", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("agemapCD")),
          box(title = span("Risk Factors", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("agemapRF"))
        )
      ),

    tabItem(
      tabName = "tbgender",
      span("Gender Based Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
      fluidRow(
        box(selectizeInput(inputId = "inGender2", "Pick a Gender", choices = unique(genderData$Gender)), height = 120),
        box(sliderInput(inputId = "inYear2", label = "Slide to select a year", 2011, 2015, step = 1, value = 2011)),
        box(selectizeInput( inputId = "inASCR2", "Age Standardized or Crude data", choices = unique(genderData$Data_Value_Type)), height = 120),
        box(selectizeInput( inputId = "inRF2", "Risk Factor", choices = unique(genderDataRF$Topic)), height = 120),
        box(selectizeInput( inputId = "inCD2", "Cardiovascular Diseases", choices = unique(genderDataCD$Topic)), height = 120)
      ),
      fluidRow(
        box(title = span("Diseases", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("gendermapCD")),
        box(title = span("Risk Factors", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("gendermapRF"))
        )
      ),

    tabItem(
      tabName = "tbrace",
      span("Ethnicity Based Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
      fluidRow(
        box(selectizeInput( inputId = "inRace3", "Pick Ethnicity", choices = unique(raceData$Race)), height = 120),
        box(sliderInput(inputId = "inYear3", label = "Slide to select a year", 2011, 2015, step = 1, value = 2011)),
        box(selectizeInput( inputId = "inASCR3", "Age Standardized or Crude data", choices = unique(raceData$Data_Value_Type)), height = 120),
        box(selectizeInput( inputId = "inRF3", "Risk Factor", choices = unique(raceDataRF$Topic)), height = 120),
        box(selectizeInput( inputId = "inCD3", "Cardiovascular Diseases", choices = unique(raceDataCD$Topic)), height = 120)
      ),
      fluidRow(
        box(title = span("Diseases", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("racemapCD")),
        box(title = span("Risk Factors", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("racemapRF"))
        )
      ),

    tabItem(
      tabName = "tboverall",
      span("Overall Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
      fluidRow(
        box(selectizeInput( inputId = "inASCR4", "Age Standardized or Crude data", choices = unique(overallData$Data_Value_Type)), height = 120),
        box(sliderInput(inputId = "inYear4", label = "Slide to select a year", 2011, 2015, step = 1, value = 2011)),
        box(selectizeInput( inputId = "inCD4", "Cardiovascular Diseases", choices = unique(overallDataCD$Topic)), height = 120),
        box(selectizeInput( inputId = "inRF4", "Risk Factor", choices = unique(overallDataRF$Topic)), height = 120)
        ),
      fluidRow(
          box(title = span("Diseases", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("overallmapCD")),
          box(title = span("Risk Factors", style = "color: blue; font-weight: bold; font-size: 14px"), htmlOutput("overallmapRF"))
          )
      ),
    
    # States Risk by Age tab here
    tabItem(
      tabName = "tbStateRisk",
      span("Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
      fluidRow(
        box(selectizeInput(inputId = "inLoca","Location",choices = unique(ageData$Location))),
        box(selectizeInput(inputId = "inTopic", "Risk Factors",choices = unique(ageData$Topic))),
        box(selectizeInput(inputId = "inAge", "Age",choices = unique(ageData$Age))),
        box(title=span("Risk factors",style= "color: blue; font-weight: bold; font-size: 14px"), plotOutput("count"))
        )
      ),
    
    # States Risk by Age tab here
    tabItem(
      tabName = "tbComparestates",
      span("Graphs", style = "color: green; font-weight: bold; font-size: 18px"),
      fluidRow(
        box(selectizeInput(inputId = "Location1", label = "Select State", choices = unique(genderData$Location))),
        box(selectizeInput(inputId = "Location2", label = "Select State to compare", choices = unique(genderData$Location))),
        box(selectizeInput(inputId = "Indicator", label = "Indicator", choices = unique(genderData$Indicator))),
        box(selectizeInput(inputId = "Gender", label = "Gender", choices = unique(genderData$Gender))),
        box(title=span("Compare states",style= "color: blue; font-weight: bold; font-size: 14px"), plotOutput("compareStates"))
        )
      )
    )
    )
  )

##################################################
# Server.R
##################################################
  server <- function(input, output, session) {

  # Age Data - Diseases
  ageDataCDSel1 <- reactive({
    ageDataCD %>% filter(ageDataCD$Age == input$inAge1 & ageDataCD$Year == input$inYear1 & ageDataCD$Topic == input$inCD1) %>% select(., c(Location, Data_Value ))
  })
  
  output$agemapCD <- renderGvis({
    df1a <- ageDataCDSel1()
    a1a <- gvisGeoChart(
      df1a,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto")
    )
  }
  )
  # Age Data - Risk Factors  
  ageDataRFSel2 <- reactive({
    ageDataRF %>% filter(ageDataRF$Age == input$inAge1 & ageDataRF$Year == input$inYear1 & ageDataRF$Topic == input$inRF1) %>% select(., c(Location, Data_Value ))
  })
  
  output$agemapRF <- renderGvis({
    df1b <- ageDataRFSel2()
    a1b <- gvisGeoChart(
      df1b,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto"))
  }
  )

  # Gender Data - Diseases
  genderDataCDSel1 <- reactive({
    genderDataCD %>% filter(genderDataCD$Gender == input$inGender2 & genderDataCD$Year == input$inYear2 & genderDataCD$Topic == input$inCD2 & genderDataCD$Data_Value_Type == input$inASCR2) %>% select(., c(Location, Data_Value ))
  })
  
  output$gendermapCD <- renderGvis({
    df2a <- genderDataCDSel1()
    a2a <- gvisGeoChart(
      df2a,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto")
    )
  }
  )
  # Gender Data - Risk Factors  
  genderDataRFSel2 <- reactive({
    genderDataRF %>% filter(genderDataRF$Gender == input$inGender2 & genderDataRF$Year == input$inYear2 & genderDataRF$Topic == input$inRF2 & genderDataRF$Data_Value_Type == input$inASCR2) %>% select(., c(Location, Data_Value ))
  })
  
  output$gendermapRF <- renderGvis({
    df2b <- genderDataRFSel2()
    a2b <- gvisGeoChart(
      df2b,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto"))
  }
  )
  
  # Ethnicity Data - Diseases
  raceDataCDSel1 <- reactive({
    raceDataCD %>% filter(raceDataCD$Race == input$inRace3 & raceDataCD$Year == input$inYear3 & raceDataCD$Topic == input$inCD3 & raceDataCD$Data_Value_Type == input$inASCR3) %>% select(., c(Location, Data_Value ))
  })
  
  output$racemapCD <- renderGvis({
    df3a <- raceDataCDSel1()
    a3a <- gvisGeoChart(
      df3a,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto")
    )
  }
  )
  # Ethnicity Data - Risk Factors  
  raceDataRFSel2 <- reactive({
    raceDataRF %>% filter(raceDataRF$Race == input$inRace3 & raceDataRF$Year == input$inYear3 & raceDataRF$Topic == input$inRF3 & raceDataRF$Data_Value_Type == input$inASCR3) %>% select(., c(Location, Data_Value ))
  })
  
  output$racemapRF <- renderGvis({
    df3b <- raceDataRFSel2()
    a3b <- gvisGeoChart(
      df3b,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto"))
  }
  )
  
  # Overall Data - Diseases
  overallDataCDSel1 <- reactive({
    overallDataCD %>% filter(overallDataCD$Year == input$inYear4 & overallDataCD$Topic == input$inCD4 & overallDataCD$Data_Value_Type == input$inASCR4) %>% select(., c(Location, Data_Value ))
  })
  
  output$overallmapCD <- renderGvis({
    df4a <- overallDataCDSel1()
    a4a <- gvisGeoChart(
      df4a,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto")
    )
  }
  )
  # Overall Data - Risk Factors  
  overallDataRFSel2 <- reactive({
    overallDataRF %>% filter(overallDataRF$Year == input$inYear4 & overallDataRF$Topic == input$inRF4 & overallDataRF$Data_Value_Type == input$inASCR4) %>% select(., c(Location, Data_Value ))
  })
  
  output$overallmapRF <- renderGvis({
    df4b <- overallDataRFSel2()
    a4b <- gvisGeoChart(
      df4b,
      locationvar = "Location",
      colorvar = "Data_Value",
      options=list(region="US", displayMode="regions", resolution="provinces", width="auto", height="auto"))
  }
  )
  # #Plot the graph for risk factors according to Location and year
  
  output$count <- renderPlot(
   ageData %>%
      filter(Location == input$inLoca,Topic == input$inTopic & Age == input$inAge) %>%
      group_by(Year) %>%
      ggplot(aes(x =Year, y = (Data_Value))) +
      geom_col(fill = "lightblue") +
      ggtitle("Cardiovascular disease")
  )
  
  output$compareStates <- renderPlot(
    genderData %>%
      filter(Location == input$Location1 | Location==input$Location2 ,Indicator==input$Indicator,Gender == input$Gender) %>%
      group_by(Year) %>%
      ggplot(aes(x =Year,y=Data_Value)) +
      geom_col(aes(fill=Location) ,position="dodge")+
      ggtitle("Cardiovascular disease")
  )
  }

shinyApp(ui, server)
