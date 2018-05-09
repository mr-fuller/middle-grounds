library(tidyverse)
library(plotly)
library(ggplot2)
source("acs2016demographics.R")
source("census2010demographics.R")
source("census2000demographics.R")

# race plot
years = c("2016","2010","2000")
wanh <- c((sum(blkgrp2016data$B03002_003E)/sum(blkgrp2016data$B01001_001E))*100,
          (sum(blk2010data$`White alone`)/ sum(blk2010data$`Total Population`)*100),
          (sum(blkgrp2000data$P007003)/ sum(blkgrp2000data$P001001 )*100)
          )

poc <- c((sum(blkgrp2016data$B01001_001E)-sum(blkgrp2016data$B03002_003E))/sum(blkgrp2016data$B01001_001E)*100,
         (sum(blk2010data$`Total Population`) - sum(blk2010data$`White alone`))/sum(blk2010data$`Total Population`)*100,
         (sum(blkgrp2000data$P001001) - sum(blkgrp2000data$P007003))/sum(blkgrp2000data$P001001)*100)
racechartdata <- data.frame(years,wanh,poc)
racechart <- plot_ly(racechartdata,
                    x = ~years,
                    y = ~wanh, 
                    name = "White Alone, non-Hispanic", 
                    type = "bar",
                    hoverinfo = 'y')%>% 
  add_trace(y = ~poc, 
            name = "People of Color",
            hoverinfo = 'y')%>%
  layout(xaxis = list(title = 'Year'),
    yaxis = list(title = 'Percentage'),barmode = 'stack')

"
add_trace(blk2010data,x = '2010', 
                    y = ~,
                    name = 'White Alone, not Hispanic', type = 'bar',
                    hoverinfo = 'y')%>%
  add_trace(x = '2010',y = ~
            name = 'People of Color',
            hoverinfo = 'y')%>%
  #layout(yaxis = list(title = 'Percentage'),barmode = 'stack', showlegend = FALSE)%>%

  add_trace(blkgrp2000data,
                    x = '2000', 
                    y = ~,
                    name = 'White Alone, not Hispanic', type = 'bar',
                    hoverinfo = 'y')%>%
  add_trace(x = '2000', y = ~,
            name = 'People of Color',
            hoverinfo = 'y')%>%
  layout(yaxis = list(title = 'Percentage'),barmode = 'stack',showlegend = FALSE)"