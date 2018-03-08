library(censusapi)
library(tidyverse)
library(stringr)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

blk_grps_2010 <- c(2,3)

#What variables do I want?
#race
#income
#pop pyramid of  2016 data?
#  B01001_003E:B01001_049E
#poverty
#home ownership
#vehicle ownership

## Acquire and tidy 2000 block data
blkgrp2016data <- tibble()
#for (i in unique(substr(blks_2010$fips,6,11))){
temp <-as_tibble(getCensus(name="acs/acs5", 
                             vintage = 2016, 
                             vars =  "B01001_003E", 
                             region = "block group:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = "state:39+county:095+tract:010300",                        
                             key = api_key ))
  blkgrp2016data <-rbind(blkgrp2016data,temp)
  
#}
