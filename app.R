#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(leaflet)
#library(ggthemes)
library(rgdal)
library(plotly)
Sys.setenv("plotly_username"="mrfuller460")
Sys.setenv("plotly_api_key"="MQmwmskDMq39EWqcCr19")
plotly2000pyramid <- api_download_plot(15,"mrfuller460")
plotly2010pyramid <- api_download_plot(17,"mrfuller460")
#source("popPyramid2000.r", local = TRUE)
#source("popPyramid2010.r", local = TRUE)
#source("acs2016demographics.R")
#source("census2010demographics.R")
#source("census2000demographics.R")
#source("racechart.R")
#source("unemploymentchart.R")
#source("povertychart.R")
#source("edchart.R")
raceplotly <- api_download_plot(7,"mrfuller460")
unemploymentplotly <-  api_download_plot(9,"mrfuller460")
povertyplotly <- api_download_plot(11,"mrfuller460")
edplotly <- api_download_plot(13,"mrfuller460")
blk2000 <- readOGR("data/middle_grounds_blk_2000.gpkg")
blk2000 <- spTransform(blk2000, CRS("+init=epsg:4326"))
#pyramid <- pyramid
ui <- navbarPage("Middle Grounds District Demographics",  
  tabPanel("Home",
  sidebarLayout(
    sidebarPanel(div(img(src = "middle-grounds-aerial.jpg", width = "100%")),
      p(" This website provides demographic data in support of development efforts outlined in the ", 
               a(href="http://toledo.oh.gov/media/4015/middle-grounds-district-plan-2015-reduced.pdf", 
                 "Middle Grounds District Plan")),
                 
                 p("All data from the Census Bureau"),
                 p("Most data aggregated at the block group level, in which case the study area includes more than
                  just the Middle Grounds District"),
                 img(src = "TMACOGlogo.jpg"),
                 p("Built with R, Shiny, Plotly, ggmap, leaflet, and ggplot"),
                 p("Data prepared by ",a(href = "http://www.tmacog.org","TMACOG")),
                 p("Source code at ",a(href = "https://github.com/mr-fuller/middle-grounds","GitHub"))
                ),
    mainPanel(
      leafletOutput("map"),
      h2("Middle Grounds District Legal Description"),
      p("The Middle Grounds District boundary is as follows: Commencing at the intersection of 
        the centerline of Clayton Street and the centerline of South St. Clair Street,
        thence southwest along the centerline of South St. Clair Street to its intersection with 
        the centerline of Newton Street,
        thence west along the centerline of Newton Street to its intersection with the centerline 
        of Collingwood Boulevard,
        thence west along the centerline of Collingwood Boulevard to its intersection with the 
        east right-of-way line of I-75,
        thence southeast along the east right
        -of-way of I-75 to a point on the centerline of the 
        Norfolk Southern Railroad right-of-way, 
        thence southeast along the centerline of the Norfolk Southern Railroad right-
        of-way to a point being the centerline of the Maumee River,
        thence downstream, north, along the centerline of the Maumee River to a point on the 
        centerline of the Anthony Wayne Bridge (State Routes 2, 51,and 65),
        thence northwest along the centerline of the Anthony Wayne Bridge westward to its 
        intersection with the centerline of  Clayton Street,
        thence west along the centerline of Clayton Street to its intersection the centerline of 
        South St. Clair Street, the point of beginning.")))),
  tabPanel("Population",fluidRow(
    column(6,
           plotlyOutput("pyramid")
           ),
    column(6,
           plotlyOutput("pyramid2")
           )
        )
      ),
  tabPanel("Race",fluidRow(
    plotlyOutput("racechart")),
    #column(4,plotlyOutput("race2010")),
    #column(4,plotlyOutput("race2016"))),
    br(),
    
    p("Note that the term 'People of Color' is used to include any person not identified as 'White Alone, not Hispanic'.")
    ),
  tabPanel("Unemployment",fluidRow(
    plotlyOutput("unemploymentchart")),
    #column(6,plotlyOutput("unemployment2016")),
    br(),
    p("Percentages reflect those for the population age 16 and over.")
    ),
  tabPanel("Poverty", fluidRow(
    plotlyOutput("povertychart")),
    #column(6,plotlyOutput("poverty2016")),
    br(),
    p("Poverty rates reflect those of households. The district is made up of areas with low-moderate income percentages 
        of over 51 percent, making it eligible for the City of Toledo's CDBG entitlement from HUD.")
    ),
  tabPanel("Education", fluidRow(
    plotlyOutput("edchart")),
    #column(6,plotlyOutput("education2016")),
    p("Percentages reflect those for the population age 25 and over."))
    

)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolygons(data = blk2000)
  })
    
  
  output$pyramid <- renderPlotly({
    print(plotly2000pyramid)
  })
  output$pyramid2 <- renderPlotly({
    print(plotly2010pyramid)
  })
  output$racechart <- renderPlotly({
    print(raceplotly)
  })
  
  output$unemploymentchart <- renderPlotly({
    print(unemploymentplotly)
  })
  
  output$povertychart <- renderPlotly({
    print(povertyplotly)
  })
  
  output$edchart <- renderPlotly({
    print(edplotly)
  })
  
}


shinyApp(ui, server)

