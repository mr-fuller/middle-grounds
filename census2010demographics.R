library(censusapi)
library(tidyverse)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

blks_2010 <- tribble(~fips,
                     390950103002009,
                     390950103002010,
                     390950103002030,
                     390950103002039,
                     390950103002040,
                     390950103002041,
                     390950103002042,
                     390950103002043,
                     390950103002044,
                     390950103002045,
                     390950103002046,
                     390950103002049,
                     390950103002050,
                     390950103002051,
                     390950103002052,
                     390950103002053,
                     390950103002054,
                     390950103002055,
                     390950103003001,
                     390950103003002,
                     390950103003003
)

blk_grps_2010 <- c(2,3)

vars2010 <- tribble(~name,
                    "P0010001", #Total population
                    "P0030002", #Total Population
                    "H0030001", #Housing Units
                    "H0030002", #occupied housing units
                    "H0030003", #vacant housing units
                    "H0040004", #renter occupied units
                    "H0070003", #householder not hispanic or latino who is white alone
                    "H0110004", #population in renter occupied housing units
                    "H0120001", #average household size
                    "H0120002", #average household size for owner-occupied
                    "H0120003", #average household size for renters
                    "P0050003", #population not hispanic or latino, white alone
                  
                    "P0130001", #median age both sexes
                    "P0130002", #median age male
                    "P0130003", #median age female
                    "P0180001", #households
                    "P0180002", #family households
                    "P0290020", #male householder living alone 
                    "P0290023", #female householder living alone 
                    "P0420001" #pop in group quarters?
                    
)
getDemoVars <- function(year){
  #retrieve all variables
  vars <- as_tibble(listCensusMetadata(name = "sf1",
                                       vintage = year,
                                       type = "variables"))
  #filter to sex by age variables
  vars <- filter(vars,vars$name %in% vars2010$name)
  #remove all 'total' variables
  vars <- arrange(vars,name)#[-c(1:2,26),]
  return(vars)
}
demoVars2010 <- getDemoVars(2010)

blk2010data <- tibble()
for (i in unique(substr(blks_2010$fips,6,11))){
  temp <-as_tibble(getCensus(name="sf1", 
                             vintage = 2010, 
                             vars =  vars2010$name, 
                             region = "block:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = paste("state:39+county:095+tract:",i,sep = ""),                        
                             key = api_key ))
  blk2010data <-rbind(blk2010data,temp)
  
}

## filter blocks to only those in Middle Grounds District 
blk2010data <- filter(unite(blk2010data, state, county, tract, block, col="GEOID",sep = "",remove = FALSE), GEOID %in% blks_2010$fips)
#rename
setnames(blk2010data, old = as.character(demoVars2010$name), new = as.character(demoVars2010$label))

#read in a spatial layer

#join data to spatial layer on GEOID

#put spatial layer on interactive map