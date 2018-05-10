library(tidyverse)
library(plotly)
library(ggplot2)
source("acs2016demographics.R")
#source("census2010demographics.R")
source("census2000demographics.R")
#source(years2)
years2 <- c("2016","2000")
poverty <- c(round((sum(blkgrp2016data$B17017_002E)/sum(blkgrp2016data$B17017_001E)*100)),
             round(((sum(blkgrp2000data$P092002)/sum(blkgrp2000data$P092001)*100)))
             )
povertychartdata <- data.frame(years2,poverty)

povertychart <- plot_ly(povertychartdata,
                        x = ~years2,
                       y= ~poverty,
                       name = "Poverty Rate", 
                       type = "bar",
                       hoverinfo = 'y')%>%
  #add_trace(y = ~(sum(blkgrp2016data$B23025_007E)/sum(blkgrp2016data$B23025_001E)*100),name = "Not in Labor Force")%>%
  layout(xaxis = list(title = "Year"),
                      yaxis = list(title = 'Percentage', range = c(0,100)))

