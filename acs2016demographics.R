library(censusapi)
library(tidyverse)
library(stringr)
library(plotly)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

blk_grps_2010 <- c(2,3)

listCensusMetadata(name = "acs/acs5",vintage = 2016, type = "variables")
#What variables do I want?
#race
vars <- c("B01001_001E","B01001_001M", #total pop
          "B03002_003E","B03002_003M", #white alone, non-hispanic
          "B06011_001E","B06011_001M", #median income
          "B15003_001E","B15003_001M", #population 25+
          "B15003_017E","B15003_017M", #high school diploma 25+
          "B15003_018E","B15003_018M", #GED 25+
          "B15003_022E","B15003_022M", #bachelor's degree 25+
          "B17017_001E","B17017_001M", #households
          "B17017_002E","B17017_002M", #households below poverty level
          "B25001_001E","B25001_001M", #housing units
          "B25002_003E","B25002_003M", #vacant housing units
          "B25003_003E","B25003_003M", #renter occupied housing units
          "B25031_001E","B25031_001M", #median gross rent
          "B25010_002E","B25010_001M", #average household size 
          "B25010_002E","B25010_002M", #average household size for owner occupied
          "B25010_003E","B25010_003M", #average household size for renter occupied
          "B23025_001E","B23025_001M", #16 and older population 
          "B23025_005E","B23025_005M", #unemployed
          "B23025_007E","B23025_007M", #not in labor force
          "B23025_005E","B23025_005M" #unemployed
          
)
#income
#pop pyramid of  2016 data?
#  B01001_003E:B01001_049E
#poverty
#home ownership
#vehicle ownership
#median gross rent
#education
#household size

## Acquire and tidy 2000 block data
blkgrp2016data <- tibble()
#for (i in unique(substr(blks_2010$fips,6,11))){
temp <-as_tibble(getCensus(name="acs/acs5", 
                             vintage = 2016, 
                             vars =  vars,
                             region = "block group:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = "state:39+county:095+tract:010300",                        
                             key = api_key ))
  blkgrp2016data <-rbind(blkgrp2016data,temp)
  
#}
  blkgrp2016data <- blkgrp2016data %>% filter(block.group>1)
  #white_pct <- (blkgrp2016data$B03002_003E/blkgrp2016data$B03001_001E)*100
  #poc_pct <- (blkgrp2016data$B03001_001E-blkgrp2016data$B03002_003E)/blkgrp2016data$B03001_001E*100
  
race2016 <- plot_ly(blkgrp2016data,
                    x = "2016",
                    y = ~(sum(blkgrp2016data$B03002_003E)/sum(blkgrp2016data$B01001_001E))*100, 
                    name = "White Alone, non-Hispanic", 
                    type = "bar",
                    hoverinfo = 'y')%>% 
  add_trace(y = ~(sum(blkgrp2016data$B01001_001E)-sum(blkgrp2016data$B03002_003E))/sum(blkgrp2016data$B01001_001E)*100, 
            name = "People of Color",
            hoverinfo = 'y')%>%
  layout(yaxis = list(title = 'Percentage'),barmode = 'stack',legend = list(x=100,y = 0.5))

unemployment2016 <- plot_ly(blkgrp2016data,x = "2016",
                            y= ~(sum(blkgrp2016data$B23025_005E)/sum(blkgrp2016data$B23025_001E)*100),
                                name = "Unemployed", type = "bar",
                            hoverinfo = 'y')%>%
  add_trace(y = ~(sum(blkgrp2016data$B23025_007E)/sum(blkgrp2016data$B23025_001E)*100),
            name = "Not in Labor Force",
            hoverinfo = 'y')%>%
    layout(yaxis = list(title = 'Percentage',range = c(0,60)), barmode = 'group')

poverty2016 <- plot_ly(blkgrp2016data,x = "2016",
                            y= ~(sum(blkgrp2016data$B17017_002E)/sum(blkgrp2016data$B17017_001E)*100),
                            name = "Poverty Rate", type = "bar",
                       hoverinfo = 'y')%>%
  #add_trace(y = ~(sum(blkgrp2016data$B23025_007E)/sum(blkgrp2016data$B23025_001E)*100),name = "Not in Labor Force")%>%
  layout(yaxis = list(title = 'Percentage', range = c(0,100)))

education2016 <- plot_ly(blkgrp2016data,x = "2016",
                            y= ~((sum(blkgrp2016data$B15003_017E)+sum(blkgrp2016data$B15003_018E))/sum(blkgrp2016data$B15003_001E)*100),
                            name = "High School or Equivalent", type = "bar",
                         hoverinfo = 'y')%>%
  add_trace(y = ~(sum(blkgrp2016data$B15003_022E)/sum(blkgrp2016data$B15003_001E)*100),
            name = "Bachelor's Degree",
            hoverinfo = 'y')%>%
  layout(yaxis = list(title = 'Percentage',range = c(0,50)), barmode = 'group')

