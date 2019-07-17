library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(tidyr)
library(plotly)
library(magrittr)
library(dplyr)
library(pillar)
library(tidyverse)
library(ggplot2)
library(readr)
library(data.table)
library(parallel)
library(raster)
library(leaflet)
library(viridis)
library(zoo)
library(tigris)
library(geosphere)

#According to year
df <- read.csv(file = "1950-2017_all_tornadoes.csv",header = TRUE, sep = ",")
# mag<- rowsum(df$mag, group= df$yr, reorder = TRUE)
# inj <-rowsum(df$inj, group= df$yr, reorder = TRUE)
# fat <-rowsum(df$fat, group= df$yr, reorder = TRUE)
# loss <-rowsum(df$loss, group= df$yr, reorder = TRUE)
# df1 <- data.frame(mag,inj,fat,loss)
# df1$year <- unique(df$yr)
# magt <- colSums(mag)
# injt <- colSums(inj)
# fatt <- colSums(fat)
# losst <- colSums(loss)
# df1$mag1 <- (df1$mag / magt)* 100
# df1$inj1 <- (df1$inj / injt)* 100
# df1$fat1 <- (df1$fat / fatt)* 100
# df1$loss1 <- (df1$loss / losst)* 100


#According to month
# mag<- rowsum(df$mag, group= df$mo, reorder = TRUE)
# inj <-rowsum(df$inj, group= df$mo, reorder = TRUE)
# fat <-rowsum(df$fat, group= df$mo, reorder = TRUE)
# loss <-rowsum(df$loss, group= df$mo, reorder = TRUE)
# df2 <- data.frame(mag,inj,fat,loss)
# df2$month <- unique(df$mo)
# mymonths <- c("Jan","Feb","Mar",
#               "Apr","May","Jun",
#               "Jul","Aug","Sep",
#               "Oct","Nov","Dec")
# df2$month <- mymonths[ df2$month ]
# magt <- colSums(mag)
# injt <- colSums(inj)
# fatt <- colSums(fat)
# losst <- colSums(loss)
# df2$mag1 <- (df2$mag / magt)* 100
# df2$inj1 <- (df2$inj / injt)* 100
# df2$fat1 <- (df2$fat / fatt)* 100
# df2$loss1 <- (df2$loss / losst)* 100

dfIL <- df %>% filter(st == "IL")
years <-unique(df$yr)
states <- unique(df$st)


ui <-fluidPage(
  dashboardPage(skin= "red",
                dashboardHeader(title = "You Spin Me Round", titleWidth = 450),
                dashboardSidebar(disable = FALSE, collapsed = FALSE,
                                 
                                 selectInput("Year", "Select the year to visualize", years, selected = 1950),
                                 selectInput("State", "Select the State to visualize", states, selected = "IL"),
                                 selectInput("State2", "Select the State to visualize (For New Map)", states, selected = "AR"),
                                 
                sidebarMenu(menuItem("About",tabName = "about"),
                            menuItem("Tornadoes- Yearly",tabName = "tyearly"),
                            menuItem("Tornadoes- Monthly",tabName = "tmontly"),
                            menuItem("Tornadoes- StateWise", tabName= "stornadoes"),
                            menuItem("Tornadoes- CountyWise", tabName= "ctornadoes"),
                            menuItem("Maps- State Wise", tabName = "maps"),
                            menuItem("Map- Tornadoes", tabName= "mapT"),
                            menuItem("Map- New State", tabName = "mapT2")
                            )),
               
                 dashboardBody(
                  tabItems(
                    tabItem("about", includeHTML("about.html")),
                    tabItem("tyearly", 
                            fluidRow(
                              column(12,
                                     fluidRow( 
                                       box(width = 14,status = "info",dataTableOutput("TornadoesTable")),
                                       fluidRow(box(width = 5, status = "info",plotlyOutput("TornadoesPieM")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesPieI")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesPieF")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesPieL"))))))
                            ),
                    tabItem("tmontly", 
                            fluidRow(
                              column(12,fluidRow(box(width = 14,status = "info",dataTableOutput("TornadoesMonthTable")),
                                       fluidRow(box(width = 5, status = "info",plotlyOutput("TornadoesMonthBar")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesMonthInj")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesMonthfat")),
                                                box(width=5, status= "info", plotlyOutput("TornadoesMonthloss"))),
                                       box(width=15,status = "info", plotlyOutput("TornadoesMonthPie")))))
                            ),
                tabItem("stornadoes", 
                        fluidRow(title = "STATE WISE BAR GRAPHS",column(12, fluidRow(box(width = 5, status = "info",plotlyOutput("statewisemag")),
                                                     box(width=5, status= "info", plotlyOutput("statewiseinj")),
                                                     box(width=5, status= "info", plotlyOutput("statewisefat")),
                                                     box(width=5, status= "info", plotlyOutput("statewiseloss")))
                                        ))),
                tabItem("ctornadoes",fluidRow(box(width=15, status= "info", dataTableOutput("countyDataTable"))),
                        box(width=15, status= "info", plotlyOutput("ILStateComparison"))),
                tabItem("maps", fluidRow(box(title = "Deaths", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("mapDeaths")),
                                         box(title = "Injuries", solidHeader= TRUE, status = "primary", width = 12, leafletOutput("mapInjuries")),
                                         box(title = "Loss", solidHeader= TRUE, status = "primary", width = 12, leafletOutput("mapLoss")),
                                         box(title = "Total Tornadoes", solidHeader= TRUE, status = "primary", width = 12, leafletOutput("mapTornadoes")))),
                tabItem("mapT", fluidRow(box(width=4, radioButtons("selectdamage",h3("Damage Selected : "), choices=c("magnitude" = "mag", "length" = "len", "width" = "wid",
                                                                    "loss" = "loss", "injuries" = "inj", "fatalities" = "fat")), 
                                             checkboxGroupInput("magnitudes", h3("Magnitudes amount:"), choices=c(0,1,2,3,4,5,"unknown" = -9), 
                                                                inline = TRUE,selected = c(0,1,2,3,4,5)), 
                                             selectInput("maplayout", label = h3("Choose a base map:"), choices = list("Base Layout" = 1, "Satellite " = 2, "Transport Lines" = 3, "Stamen Terrain" = 4, "Hydda" = 5))),
                                          box(width = 4, sliderInput("mapLength", label = h3("Length Range:"), min = 0, 
                                                     max = 240, value = c(0, 240)),sliderInput("mapWidth", label = h3("Width Range:"), min = 0, 
                                                                                              max = 4600, value = c(0, 4600)), 
                                              sliderInput("mapYear", label = h3("Year Select"), min = 1950, max = 2018, value = 2015,sep="",step = 1)),
                                        box(width = 4, sliderInput("mapLoss", label = h3("Loss Range:"), min = 0, 
                                                     max = 1000, value = c(0, 1000),step = 1), sliderInput("mapInjury", label = h3("Injury Range:"), min = 0, 
                                                     max = 1800, value = c(0, 1750)), sliderInput("mapDeaths", label = h3("Deaths Range:"), min = 0, 
                                                     max = 160, value = c(0, 170))),
                         fluidRow(column(12, box(title = "Map", solidHeader = FALSE, status = "primary", width = 24,
                                         leafletOutput("map")))))),
                tabItem("mapT2", fluidRow(box(width=4, radioButtons("selectdamage2",h3("Damage Selected : "), choices=c("magnitude" = "mag", "length" = "len", "width" = "wid",
                                                                                                                      "loss" = "loss", "injuries" = "inj", "fatalities" = "fat")), 
                                             checkboxGroupInput("magnitudes2", h3("Magnitudes amount:"), choices=c(0,1,2,3,4,5,"unknown" = -9), 
                                                                inline = TRUE,selected = c(0,1,2,3,4,5)), 
                                             selectInput("maplayout2", label = h3("Choose a base map:"), choices = list("Base Layout" = 1, "Satellite " = 2, "Transport Lines" = 3, "Stamen Terrain" = 4, "Hydda" = 5))),
                                         box(width = 4, sliderInput("mapLength2", label = h3("Length Range:"), min = 0, 
                                                                    max = 240, value = c(0, 240)),sliderInput("mapWidth2", label = h3("Width Range:"), min = 0, 
                                                                                                              max = 4600, value = c(0, 4600)), 
                                             sliderInput("mapYear2", label = h3("Year Select"), min = 1950, max = 2018, value = 2015,sep="",step = 1)),
                                         box(width = 4, sliderInput("mapLoss2", label = h3("Loss Range:"), min = 0, 
                                                                    max = 1000, value = c(0, 1000),step = 1), sliderInput("mapInjury2", label = h3("Injury Range:"), min = 0, 
                                                                    max = 1800, value = c(0, 1750)), sliderInput("mapDeaths2", label = h3("Deaths Range:"), min = 0,max = 160, value = c(0, 170))),
                                         fluidRow(column(12, box(title = "New Map State", solidHeader = FALSE, status = "primary", width = 24,
                                                                 leafletOutput("map2"))))))
                
                
                )
                )))

server <- function(input,output,session)
  {
  #............................ FUNCTIONS.........................
  yearlydata <- function()
  {
    mag<- rowsum(df$mag, group= df$yr, reorder = TRUE)
    inj <-rowsum(df$inj, group= df$yr, reorder = TRUE)
    fat <-rowsum(df$fat, group= df$yr, reorder = TRUE)
    loss <-rowsum(df$loss, group= df$yr, reorder = TRUE)
    
    yearlydata <- data.frame(mag,inj,fat,loss)
    yearlydata$year <- unique(df$yr)
    
    magt <- colSums(mag)
    injt <- colSums(inj)
    fatt <- colSums(fat)
    losst <- colSums(loss)
    
    yearlydata$mag1 <- (yearlydata$mag / magt)* 100
    yearlydata$inj1 <- (yearlydata$inj / injt)* 100
    yearlydata$fat1 <- (yearlydata$fat / fatt)* 100
    yearlydata$loss1 <- (yearlydata$loss / losst)* 100
    yearlydata
  }
  monthlydata <- function()
  {
    mag<- rowsum(df$mag, group= df$mo, reorder = TRUE)
    inj <-rowsum(df$inj, group= df$mo, reorder = TRUE)
    fat <-rowsum(df$fat, group= df$mo, reorder = TRUE)
    loss <-rowsum(df$loss, group= df$mo, reorder = TRUE)
    
    monthlydata <- data.frame(mag,inj,fat,loss)
    monthlydata$month <- unique(df$mo)
    
    mymonths <- c("Jan","Feb","Mar",
                  "Apr","May","Jun",
                  "Jul","Aug","Sep",
                  "Oct","Nov","Dec")
    monthlydata$month <- mymonths[ monthlydata$month ]
    
    magt <- colSums(mag)
    injt <- colSums(inj)
    fatt <- colSums(fat)
    losst <- colSums(loss)
    
    monthlydata$mag1 <- (monthlydata$mag / magt)* 100
    monthlydata$inj1 <- (monthlydata$inj / injt)* 100
    monthlydata$fat1 <- (monthlydata$fat / fatt)* 100
    monthlydata$loss1 <- (monthlydata$loss / losst)* 100
    monthlydata
  }
    
  #get total of each damage - inj, loss and fatalities
  damage <- function( Data1, type, columnName)
  {
    damagedone <- Data1 %>% group_by_(columnName, type) %>% 
      summarise(n())
    names(damagedone) <- c(columnName, "a", "count")
    damagedoneCount <- aggregate(damagedone$count * damagedone$a, by=list(Category=damagedone[[1]]), FUN=sum)
    names(damagedoneCount) <- c(columnName, type)
    return(damagedoneCount)
  }
  
  #combine all damages into one dataframe
  fulldamage <- function( Data1, columnName)
  {
    deaths <- damage(Data1, "fat", columnName)
    losses <- damage(Data1, "loss", columnName)
    injuries <- damage(Data1, "inj", columnName)
    
    names(deaths) <- c(columnName, "Deaths")
    names(injuries) <- c(columnName, "Injuries")
    names(losses) <- c(columnName, "Loss")
    
    totalDamages <- merge(deaths,losses,by=columnName)
    totalDamages <- merge(totalDamages, injuries, by=columnName)
  }
  
  #get tornado info based on county
  tornadoinfo <- function(Data1)
    {
    c1 <- Data1 %>% group_by(f1) %>% 
      summarise(n())
    c2 <- Data1 %>% group_by(f2) %>% 
      summarise(n())
    c3 <- Data1 %>% group_by(f3) %>% 
      summarise(n())
    c4 <- Data1 %>% group_by(f4) %>% 
      summarise(n())
    
    names(c1) <- c("County", "Count1")
    names(c2) <- c("County", "Count2")
    names(c3) <- c("County", "Count3")
    names(c4) <- c("County", "Count4")
    
    county <- merge(c1, c2, by="County", all.x = TRUE, all.y = TRUE)
    county1 <- merge(c3, c4, by="County", all.x = TRUE, all.y = TRUE)
    county <- merge(county, county1, by="County", all.x = TRUE, all.y = TRUE)
    county$sum <- rowSums( county[,2:5] )
    county[is.na(county)] <- 0
  
    county <- subset(county, select = c(County,sum))
    
    #order descending according to County Name
    county <- county[order(county$County),]
    
    return(county)
  }
  
  damageDataCounty <- function(Data1, county)
    {
    # get injuries, deaths, and losses on a per county basis
    c1 <- fulldamage(Data1, "f1")
    c2 <- fulldamage(Data1, "f2")
    c3 <- fulldamage(Data1, "f3")
    c4 <- fulldamage(Data1, "f4")
    
    names(c1) <- c("County", "Deaths1", "Loss1", "Injuries1")
    names(c2) <- c("County", "Deaths2", "Loss2", "Injuries2")
    names(c3) <- c("County", "Deaths3", "Loss3", "Injuries3")
    names(c4) <- c("County", "Deaths4", "Loss4", "Injuries4")
    
    county <- merge(c1, c2, by="County", all.x = TRUE, all.y = TRUE)
    county1 <- merge(c3, c4, by="County", all.x = TRUE, all.y = TRUE)
    county <- merge(county, county1, by="County", all.x = TRUE, all.y = TRUE)
    
    county$Deaths <- rowSums(county[, c(2, 5, 8, 11)])
    county$Injuries <- rowSums(county[, c(4, 7, 10, 13)])
    county$Losses <- rowSums(county[, c(3, 6, 9, 12)])
    county$Tornadoes <- county[[2]]
    overallcounty <- subset(county, select = c(County,Tornadoes, Deaths,Injuries,Losses))
    
    return(overallcounty)
  }
  
  magData <- function(Data1, columnName)
    {
    mag1 <- Data1 %>% group_by_(columnName, "mag") %>% 
      summarise(Count = n())
    names(mag1) <- c(columnName, "Magnitude", "Count")
  
    mag2 <- Data1 %>% group_by_(columnName) %>% 
      summarise(n())
    names(mag2) <- c(columnName, "Total")
    
    mag1 <- merge(mag1,mag2,by=columnName)
    # take note this data frame has the counts and totals of a specific tornado... not actual percantage
    
    return(mag1)
  }
  
  magDataCounty <- function(Data1)
    {
    c1 <- magData(Data1, "f1")
    c2 <- magData(Data1, "f2")
    c3 <- magData(Data1, "f3")
    c4 <- magData(Data1, "f4")
    
    names(c1) <- c("County", "Magnitude", "Count1", "Total1")
    names(c2) <- c("County", "Magnitude", "Count2", "Total2")
    names(c3) <- c("County", "Magnitude", "Count3", "Total3")
    names(c4) <- c("County", "Magnitude", "Count4", "Total4")
    
    county <- merge(c1, c2, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    county1 <- merge(c3, c4, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    county <- merge(county, county1, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    
    county$TornadoCount <- rowSums(county[, c(3, 5, 7, 9)])
    county$TotalTornado <- rowSums(county[, c(4, 6, 8, 10)])
    
    county <- subset(county, select = c(County, Magnitude, TornadoCount, TotalTornado))
    
    return(county)
  }
  
  #.................... REACTIVES ..................................
  countyData <- reactive({
      state <- df %>% filter(st == input$State)
      tornadoes <- tornadoinfo(state)
      county <- data.frame(damageDataCounty(state, tornadoes),
        mag0 = 0, mag1 = 0, mag2 = 0, mag3 = 0, mag4 = 0, mag5 = 0, magUnknown = 0)
      countyDMagnitude <- magDataCounty(state)
       for(i in 1:length(county$County))
         {
        currentCounty <- county$County[i]
        x <- as.numeric(currentCounty)
        if (x < 100 & x > 10)
          {
          county$County[i] <- paste("0", currentCounty, sep = "")
        } 
        else if (x < 10)
          {
          county$County[i] <- paste("0", currentCounty, sep = "")
        }
      }
      county[is.na(county)] <- 0
      county <- county[-1,]
    county
  })
  
  #joinng all counties into one
  mergedcountyData <- reactive({
    df3 <- counties(state = input$State, cb = TRUE, resolution = '5m')
    tornadoData <- countyData()
    colnames(tornadoData) <- c("County", "Tornadoes", "Deaths", "Losses", "Injuries", "mag0", "mag1","mag2", "mag3", "mag4", "mag5", "magU")
    county <- geo_join(df3, tornadoData, "COUNTYFP", "County")
    county
    
  })

  mapTornadoes <- reactive({
    
    tmap <- df %>% filter(st == "IL" & mag %in% input$magnitudes & yr == input$mapYear& 
                            len >= input$mapLength[1] & len <= input$mapLength[2] &
                            wid >= input$mapWidth[1] & wid <= input$mapWidth[2] &
                             fat >= input$mapDeaths[1] & fat <= input$mapDeaths[2] &
                            inj >= input$mapInjury[1] & inj <= input$mapInjury[2] &
                            loss >= ( input$mapLoss[1] * 1000000) & loss <= (input$mapLoss[2] * 1000000)  
                                             )
    tmap
  })
  mapTornadoes2 <- reactive ({
    tmap2 <- df %>% filter (st == input$State2 & mag %in% input$magnitudes2 & len >= input$mapLength2[1] & len <= input$mapLength2[2] &
                             wid >= input$mapWidth2[1] & wid <= input$mapWidth2[2] &
                             yr == input$mapYear2&
                             fat >= input$mapDeaths2[1] & fat <= input$mapDeaths2[2] &
                             loss >= ( input$mapLoss2[1] * 1000000) & loss <= (input$mapLoss2[2] * 1000000)  &
                             inj >= input$mapInjury2[1] & inj <= input$mapInjury2[2])
    tmap2
  })
  

  
  #......................  OUTPUTS ................
  
  output$TornadoesTable <- renderDataTable(yearlydata()[ , c(1,2,3,4,5) ], options = list(columnDefs = list(list(
    targets = 1:5 , className = "right"
  )), pageLength = 5))
  
  output$TornadoesPieM <- renderPlotly ({
    
    plot_ly(yearlydata(), labels = yearlydata()$year, values = yearlydata()$mag1, type = 'pie') %>%
      layout(title = 'Tornadoes by year (MAGNITUDE)')
  })
  
  output$TornadoesPieI <- renderPlotly ({
    
    plot_ly(yearlydata(), labels = yearlydata()$year, values = yearlydata()$inj1, type = 'pie') %>%
      layout(title = 'Tornadoes by year (INJURIES) ')
  })
  output$TornadoesPieF <- renderPlotly ({
    
    plot_ly(yearlydata(), labels = yearlydata()$year, values = yearlydata()$fat1, type = 'pie') %>%
      layout(title = 'Tornadoes by year (DEATHS)')
  })
  output$TornadoesPieL <- renderPlotly ({
    
    plot_ly(yearlydata(), labels = yearlydata()$year, values = yearlydata()$loss1, type = 'pie') %>%
      layout(title = 'Tornadoes by year(LOSSES) ')
  })
  
  output$TornadoesMonthTable <- renderDataTable(monthlydata()[ , c(1,2,3,4,5) ], options = list(columnDefs = list(list(
    targets = 1:5 , className = "right"
  )), pageLength = 5))
  
  output$TornadoesMonthBar <- renderPlotly({
    ggplot(monthlydata(), aes(x=month, y=mag)) + geom_bar(stat = "identity", fill = "blue") + ggtitle("Monthly Data (Magnitude)") 
  })
  
  output$TornadoesMonthPie <- renderPlotly({
     plot_ly(monthlydata(), labels = monthlydata()$month, values = monthlydata()$mag1, type = 'pie') %>%
      layout(title = 'Magnitude of Tornadoes by month')
  })
  
  output$TornadoesMonthInj <- renderPlotly({
    ggplot(monthlydata(), aes(x=month, y=inj)) + geom_bar(stat = "identity", fill = "blue") + ggtitle("Monthly Data (Injuries)") 
  })
  
  output$TornadoesMonthfat <- renderPlotly({
    ggplot(monthlydata(), aes(x=month, y=fat)) + geom_bar(stat = "identity", fill="blue") + ggtitle("Monthly Data (Deaths)") 
  })
  output$TornadoesMonthloss <- renderPlotly({
    ggplot(monthlydata(), aes(x=month, y=loss)) + geom_bar(stat = "identity", fill= "blue") + ggtitle("Monthly Data (Losses)") 
  })

  output$statewisemag <- renderPlotly({
    p <- ggplot(df[df$yr == input$Year, ], aes(x= st, y= mag )) + geom_bar(stat= "identity", fill = "red") + ggtitle("State Wise Data (Magnitude)") 
    ggplotly(p)
    
  })
  
  output$statewiseinj <- renderPlotly({
    p <- ggplot(df[df$yr == input$Year, ], aes(x= st, y= inj )) + geom_bar(stat= "identity", fill = "red") + ggtitle("State Wise Data (Injuries)") 
    ggplotly(p)
    
  })
  output$statewisefat <- renderPlotly({
    p <- ggplot(df[df$yr == input$Year, ], aes(x= st, y= fat )) + geom_bar(stat= "identity", fill = "red") + ggtitle("State Wise Data (Deaths)") 
    ggplotly(p)
    
  })
  output$statewiseloss <- renderPlotly({
    p <- ggplot(df[df$yr == input$Year, ], aes(x= st, y= loss )) + geom_bar(stat= "identity", fill = "red") + ggtitle("State Wise Data (Losses)") 
    ggplotly(p)
    
  })
  output$ILStateComparison <- renderPlotly({
    p <- ggplot(as.data.frame(subset(mergedcountyData(), select = c(NAME, County, Tornadoes, Deaths, Injuries, Losses, mag0, mag1,mag2,mag3,mag4,mag5, magU))), aes(x= County, y= Injuries)) + geom_bar(stat= "identity", fill= "blue")
  })
  output$countyDataTable <- renderDataTable(as.data.frame(subset(mergedcountyData(), select = c(NAME, County, Tornadoes, Deaths, Injuries, Losses, mag0, mag1,mag2,mag3,mag4,mag5, magU))),options = list(pageLength = 5) 
  )
  
  output$mapDeaths <- renderLeaflet({
      county <- mergedcountyData()
      deaths <- county$Deaths
      
      pal <- colorNumeric("inferno", NULL)
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = county,fillColor = ~pal(as.numeric(deaths)),fillOpacity = 1.0,
                    color = "#444444",opacity = 1.5,weight = 1.0,
                    label = ~paste("Name: ", county$NAME, deaths )) %>%
        addLegend(pal = pal,
                  values = deaths,
                  position = "topright",
                  title = "Number of Deaths")
    
  })
  output$mapInjuries <- renderLeaflet({
    county <- mergedcountyData()
    inj <- county$Injuries
    
    pal <- colorNumeric("inferno", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = county, fillColor = ~pal(as.numeric(inj)), fillOpacity = 1.0, color = "#444444",
                  opacity = 1.5, weight = 1.0, label = ~paste("Name: ",county$NAME, inj)) %>%
      addLegend(pal = pal,
                values = inj,
                position = "topright",
                title = "Number of Injuries")
    
  })
  output$mapLoss <- renderLeaflet({
    county <- mergedcountyData()
    loss <- county$Losses
    pal <- colorNumeric("inferno", NULL)
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = county,
                  fillColor = ~pal(as.numeric(loss)), fillOpacity = 1.5,color = "#444444",opacity = 1.5, weight = 1.0,
                  label = ~paste("Name: ", county$NAME,  loss)) %>%
      addLegend(pal = pal,
                values = loss,
                position = "topright",
                title = "Number of Losses")
    
  })
  output$mapTornadoes <- renderLeaflet({
    county <- mergedcountyData()
    tornadoes <- county$Tornadoes
    pal <- colorNumeric("inferno", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = county, fillColor = ~pal(as.numeric(tornadoes)), fillOpacity = 1.5, color = "#444444", opacity = 1.5,
                  weight = 1.0,
                   label = ~paste("Name: ", county$NAME, tornadoes)) %>%
      addLegend(pal = pal,
                values = tornadoes,
                position = "topright",
                title = "Number of Tornadoes")
  })
  
  output$map <- renderLeaflet({
    tmap <- mapTornadoes()
    choice <- input$maplayout
    if(choice == 1){
      selected <- "CartoDB.Positron"
    }
    else if(choice == 2) {
      selected <- "Esri.NatGeoWorldMap"
    }
    else if(choice == 3) {
      selected <- "Thunderforest.TransportDark"
    }
    else if(choice == 4){
      selected <- "Stamen.Toner"
    }
    else{
      selected <- "Hydda.Full"
    }
    pal <- colorNumeric(c("blue", "red"),domain =tmap[,input$selectdamage])
    m <-leaflet(tmap) %>%
      setView(lat = 40.6331, lng =-89.399, zoom = 8) %>% 
      addTiles()
    m<- addLegend(m,pal = pal, values = tmap[,input$selectdamage],position = "topright",title = "Path color")
    m = addProviderTiles(map = m, provider = selected)
    #denotes start of tornadoes
    m <- addCircleMarkers(m, lng = tmap$slon , lat = tmap$slat, radius = 8, color = "red", fillColor = "red",opacity = 1, 
                          popup  =  paste("Date & Time: ", tmap[, "date"],"&",tmap[, "time"], '<p></p>',"Starting position :" ,"<br>Lon: ",
                                          tmap[, "slon"]," Lat: ",tmap[, "slat"],'</p><p>', "Ending position","<br>Lon:",tmap[, "elon"]," 
                                          Lat: ",tmap[, "elat"],'</p><p>',"Magnitude: ",tmap[, "mag"], "&","Injures: ",tmap[, "inj"], '<p></p>',
                                          "Deaths: ",tmap[, "fat"], "&","Loss (in $) : $",tmap[, "loss"]  ))
    #denotes end of tornadoes
    m <- addCircleMarkers(m, lng = tmap$elon , lat = tmap$elat , radius = 8, color = "blue", fillColor = "blue",opacity = 1, 
                          popup  =  paste("Date & Time: ", tmap[, "date"],"&", tmap[, "time"], '<p></p>', "Starting  :" ,"<br>Lon: ",
                                          tmap[, "slon"]," Lat: ",tmap[, "slat"],'</p><p>',"Ending :","<br>Lon:",tmap[, "elon"]," 
                                          Lat: ",tmap[, "elat"],'</p><p>',"Magnitude: ",tmap[, "mag"], "&","Injures: ",
                                  tmap[, "inj"], '<p></p>',"Deaths: ",tmap[, "fat"], "&","Loss (in $) : $",tmap[, "loss"] ))
    #add a line connecting the circles
    
    for(i in 1:nrow(tmap))
    {
      la <- as.numeric(tmap[i, c('slat','elat' )])
      ln <- as.numeric(tmap[i, c('slon', 'elon')])
      m <-  addPolylines(m,data = tmap,lat = la , lng = ln, color = "black")
    }
   
    m
  })
  output$map2 <- renderLeaflet({
    tmap <- mapTornadoes2()
    choice <- input$maplayout
    if(choice == 1){
      selected <- "CartoDB.Positron"
    }
    else if(choice == 2) {
      selected <- "Esri.NatGeoWorldMap"
    }
    else if(choice == 3) {
      selected <- "Thunderforest.TransportDark"
    }
    else if(choice == 4){
      selected <- "Stamen.Toner"
    }
    else{
      selected <- "Hydda.Full"
    }
    pal <- colorNumeric(c("blue", "red"),domain =tmap[,input$selectdamage])
    m <-leaflet(tmap) %>%
      setView(lat = 40.6331, lng =-89.399, zoom = 4) %>% 
      addTiles()
    m<- addLegend(m,pal = pal, values = tmap[,input$selectdamage],position = "topright",title = "Path color")
    m = addProviderTiles(map = m, provider = selected)
    #denotes start of tornadoes
    m <- addCircleMarkers(m, lng = tmap$slon , lat = tmap$slat, radius = 8, color = "red", fillColor = "red",opacity = 1, 
                          popup  =  paste("Date & Time: ", tmap[, "date"],"&",tmap[, "time"], '<p></p>',"Starting position :" ,"<br>Lon: ",
                                          tmap[, "slon"]," Lat: ",tmap[, "slat"],'</p><p>', "Ending position","<br>Lon:",tmap[, "elon"]," 
                                          Lat: ",tmap[, "elat"],'</p><p>',"Magnitude: ",tmap[, "mag"], "&","Injures: ",tmap[, "inj"], '<p></p>',
                                          "Deaths: ",tmap[, "fat"], "&","Loss (in $) : $",tmap[, "loss"]  ))
    #denotes end of tornadoes
    m <- addCircleMarkers(m, lng = tmap$elon , lat = tmap$elat , radius = 8, color = "blue", fillColor = "blue",opacity = 1, 
                          popup  =  paste("Date & Time: ", tmap[, "date"],"&", tmap[, "time"], '<p></p>', "Starting  :" ,"<br>Lon: ",
                                          tmap[, "slon"]," Lat: ",tmap[, "slat"],'</p><p>',"Ending :","<br>Lon:",tmap[, "elon"]," 
                                          Lat: ",tmap[, "elat"],'</p><p>',"Magnitude: ",tmap[, "mag"], "&","Injures: ",
                                          tmap[, "inj"], '<p></p>',"Deaths: ",tmap[, "fat"], "&","Loss (in $) : $",tmap[, "loss"] ))
    #add a line connecting the circles
    
    for(i in 1:nrow(tmap))
    {
      la <- as.numeric(tmap[i, c('slat','elat' )])
      ln <- as.numeric(tmap[i, c('slon', 'elon')])
      m <-  addPolylines(m,data = tmap,lat = la , lng = ln, color = "black")
    }
    
    m
  })
  
}


shinyApp(server = server, ui = ui)



# ggplot(df1, aes(x=year, y=mag)) + geom_bar(stat = "identity")
# ggplot(df1, aes(x=year, y=inj)) + geom_bar(stat = "identity")
# ggplot(df1, aes(x=year, y=fat)) + geom_bar(stat = "identity")
# ggplot(df1, aes(x=year, y=loss)) + geom_bar(stat = "identity")
# 
# ggplot(df2, aes(x=month, y=mag)) + geom_bar(stat = "identity")
# ggplot(df2, aes(x=month, y=inj)) + geom_bar(stat = "identity")
# ggplot(df2, aes(x=month, y=fat)) + geom_bar(stat = "identity")
# ggplot(df2, aes(x=month, y=loss)) + geom_bar(stat = "identity")
# 
# plot_ly(df1, labels = df1$year, values = df1$mag1, type = 'pie') %>%
# layout(title = 'Tornadoes by year')
# plot_ly(df1, labels = df1$year, values = df1$inj1, type = 'pie') %>%
#   layout(title = 'Tornadoes by year')
# plot_ly(df1, labels = df1$year, values = df1$fat1, type = 'pie') %>%
#   layout(title = 'Tornadoes by year')
# plot_ly(df1, labels = df1$year, values = df1$loss1, type = 'pie') %>%
#   layout(title = 'Tornadoes by year')
# 
# plot_ly(df2, labels = df2$month, values = df2$mag1, type = 'pie') %>%
# layout(title = 'Magnitude of Tornadoes by month')
# plot_ly(df2, labels = df2$month, values = df2$fat1, type = 'pie') %>%
#   layout(title = 'Fatalities of Tornadoes by month')
# plot_ly(df2, labels = df2$month, values = df2$inj1, type = 'pie') %>%
#   layout(title = 'Injuries of Tornadoes by month')
# plot_ly(df2, labels = df2$month, values = df2$loss1, type = 'pie') %>%
#   layout(title = 'Loss of Tornadoes by month')
# 
# 
# 
# p <- ggplot(df[df$yr == "2010", ], aes(x= st, y= mag )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$yr == "2010", ], aes(x= st, y= inj )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$yr == "1995", ], aes(x= st, y= fat )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$yr == "2010", ], aes(x= st, y= loss )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$sst == "AZ" , ], aes(x= time, y= mag )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$st == "AZ" , ], aes(x= time, y= inj )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# p <- ggplot(df[df$st == "AZ" , ], aes(x= time, y= fat )) + geom_bar(stat= "identity", fill = "red")
# ggplotly(p)
# 
# map <- leaflet()
# map <- addTiles(map)
# map <- setView(map, lng = df[df$yr == "1950",17][1], lat = df[df$yr == "1950" ,16][1], zoom = 2)
# map <- addCircleMarkers(map, lng = df[df$yr == "1950",17][1], lat = df[df$yr == "1950" ,16][1], popup= df$st)
# mypal <- colorNumeric(palette = "viridis", domain = df[df$yr=="1950",8], na.color = "grey", reverse = TRUE)
# map <- addCirclesMarkers(data = df[df$yr == "1950", ], stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,fillColor = ~mypal(df$mag))
#map

