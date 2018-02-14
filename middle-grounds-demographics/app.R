#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(rgdal)
blk2000 <- readOGR("data/middle_grounds_blk_2000.gpkg")
blk2000 <- spTransform(blk2000, CRS("+init=epsg:4326"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  #fluidPage(
   
   # Application title
   dashboardHeader(title = #div(img(src = "middle-grounds-aerial.jpg"),
                               "Middle Grounds District Demographic Data and Trends") ,
                               #img(src = "TMACOGlogo.jpg") )),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(   
     sidebarMenu(
     
      menuItem("Interactive Map", tabName = "Map"),
      menuItem("Summary",tabName = "Summary"),
      menuItem("Trends", tabName = "Trends"),
         box(sliderInput("bins",
                     "Number of bins:",
                     min = 5,
                     max = 50,
                     value = 30)),
         box(selectInput("select",h3("Select Data to view on the map"), 
                     choices = list("1990 Census" ,
                                    "2000 Census" ,
                                    "2010 Census" ,
                                    "2012-2016 American Community Survey",
                                    "2007-2011 American Community Survey" ), selected = NULL
                     )),
         box(img(src = "TMACOGlogo.jpg"),
         p("Data complied by ",a(href = "http://www.tmacog.org","TMACOG")),
         
         p("p creates a paragraph of text.And I like the option to put text to the side of the graphic"),
         p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
         strong("strong() makes bold text."),
         em("em() creates italicized (i.e, emphasized) text."),
         br(),
         code("code displays your text similar to computer code"),
         div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
         br(),
         p("span does the same thing as div, but it works with",
           span("groups of words", style = "color:blue"),
           "that appear inside a paragraph."),
         p("Data from US Census Bureau"))
         
      )),
      
      # Show a plot of the generated distribution
      dashboardBody(
        #tags$style(type = "text/css", "#map {height: calc(100vh - 80 px) !important;}"),
        #mainPanel(
        #tabItems(
          #tabItem(div(class = "outer",
            #tags$style(type = "text/css", "#map, .leaflet-zoom-animated, html, body {width: 100%;height:800}"),
                  #tabName = "Map",
            div(class = "map",leafletOutput("map",height = 800))
          #tabItem(tabName = "Summary", h1("First level title"),
            #h2("Second level title"), h3("Third level title"), h4("Fourth level title"),  
            #h5("Fifth level title"),h6("Sixth level title")),
         #tabItem(tabName = "Trends",plotOutput("distPlot"))
        
      )
   #)
   )
  #)
#))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'orange')
   })
   # this I made to test basic functionality, print variable selected
   output$selection <- renderText({
     input$select
   })
   output$map <- renderLeaflet({
     leaflet() %>% addTiles() %>% addPolygons(data = blk2000)  
     #%>% setView() # do I need setView or will the map zoom to data automatically?
     
   })
   #leafletOutput('map',height = 1000)
   # put ouput maps and plots here
   #output$selection <- renderPlot({
   
   # put code or geographic data here
   'data <- switch(input$selection,
                  "1990 Census" = blk21990data,
                  "2000 Census" = blk2000data,
                  "2010 Census" = blk2010data,
                  "2007-2011 American Community Survey" = ,
                  "2012-2016 American Community Survey" = )
   
   })'
}

# Run the application 
shinyApp(ui = ui, server = server)

