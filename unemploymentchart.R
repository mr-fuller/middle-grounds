library(tidyverse)
library(plotly)
library(ggplot2)
source("acs2016demographics.R")
#source("census2010demographics.R")
source("census2000demographics.R")

years2 <- c("2000","2016")
nilf <- c(
          round((sum(blkgrp2000data$P043008) + sum(blkgrp2000data$P043015))/sum(blkgrp2000data$P043001)*100),
          round((sum(blkgrp2016data$B23025_007E)/sum(blkgrp2016data$B23025_001E)*100))
          )
unemployed <- c(
                round(((sum(blkgrp2000data$P043007)+sum(blkgrp2000data$P043014))/ sum(blkgrp2000data$P043001 )*100)),
                round((sum(blkgrp2016data$B23025_005E)/sum(blkgrp2016data$B23025_001E)*100))
                )
unemploymentchartdata <- data.frame(years2,unemployed,nilf)
unemploymentchart <- plot_ly(unemploymentchartdata,
                             x = ~years2,
                            y= ~unemployed,
                            name = "Unemployed", 
                            type = "bar",
                            hoverinfo = 'y')%>%
  add_trace(y = ~nilf,
            name = "Not in Labor Force",
            hoverinfo = 'y')%>%
  layout(xaxis = list(title = "Year"),
         yaxis = list(title = 'Percentage',range = c(0,60)), barmode = 'group')


