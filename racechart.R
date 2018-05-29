library(tidyverse)
library(plotly)
library(ggplot2)
source("acs2016demographics.R")
source("census2010demographics.R")
source("census2000demographics.R")

# race plot
years = c("2000","2010","2016")
wanh <- c(round(sum(blkgrp2000data$P007003)/ sum(blkgrp2000data$P001001 )*100),
          round((sum(blk2010data$`White alone`)/ sum(blk2010data$`Total Population`)*100)),
          round((sum(blkgrp2016data$B03002_003E)/sum(blkgrp2016data$B01001_001E))*100)
          
          )

poc <- c(round((sum(blkgrp2000data$P001001) - sum(blkgrp2000data$P007003))/sum(blkgrp2000data$P001001)*100),
         round((sum(blk2010data$`Total Population`) - sum(blk2010data$`White alone`))/sum(blk2010data$`Total Population`)*100),
         round((sum(blkgrp2016data$B01001_001E)-sum(blkgrp2016data$B03002_003E))/sum(blkgrp2016data$B01001_001E)*100)
         )
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

