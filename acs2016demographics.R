library(censusapi)
library(tidyverse)
library(stringr)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

blk_grps_2010 <- c(2,3)

listCensusMetadata(name = "acs/acs5",vintage = 2016, type = "variables")
#What variables do I want?
#race
vars <- c("B03002_003E","B03002_003M", #white alone, non-hispanic
          "B06011_001E","B06011_001M", #median income
          "B15003_017E","B15003_017M", #high school diploma
          "B15003_018E","B15003_018M", #GED
          "B15003_022E","B15003_022M", #bachelor's degree
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
          "B23025_005E","B23025_005M", #unemployed
          
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
                             vars =  c("B01001_003E", "B01001_003M"),
                             region = "block group:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = "state:39+county:095+tract:010300",                        
                             key = api_key ))
  blkgrp2016data <-rbind(blkgrp2016data,temp)
  
#}
