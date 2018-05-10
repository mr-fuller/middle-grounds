library(tidyverse)
library(plotly)
library(ggplot2)
source("acs2016demographics.R")
#source("census2010demographics.R")
source("census2000demographics.R")
source(years2)

hsoe <- c(round(((sum(blkgrp2016data$B15003_017E)+sum(blkgrp2016data$B15003_018E))/sum(blkgrp2016data$B15003_001E)*100)),
  round(((sum(blkgrp2000data$P037011)+sum(blkgrp2000data$P037028))/sum(blkgrp2000data$P037001)*100))
)
bd <- c(round((sum(blkgrp2016data$B15003_022E)/sum(blkgrp2016data$B15003_001E)*100)),
        round(((sum(blkgrp2000data$P037015)+sum(blkgrp2000data$P037032))/sum(blkgrp2000data$P037001)*100)))
edchartdata <- data.frame(years2,hsoe,bd)
edchart <- plot_ly(edchartdata,
                   x = ~years2,
                   y= ~hsoe,
                   name = "High School or Equivalent",
                   type = "bar",
                   hoverinfo = 'y')%>%
  add_trace(y = ~bd,
            name = "Bachelor's Degree",
            hoverinfo = 'y')%>%
  layout(xaxis = list(title = 'Year'),
          yaxis = list(title = 'Percentage',range = c(0,50)), barmode = 'group')
