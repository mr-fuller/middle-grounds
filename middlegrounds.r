library(censusapi)
library(tidyverse)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

# list of blocks
blks_2000 <- tribble(~fips,
              390950038001036,
              390950038001037,
              390950038001038,
              390950038001039,
              390950038001040,
              390950038001041,
              390950038001042,
              390950038001043,
              390950038001044,
              390950038001045,
              390950038001046,
              390950038001009,
              390950038001014,
              390950038001015,
              390950038001016,
              390950038001017,
              390950038001018,
              390950038001019,
              390950041001000,
              390950041001001,
              390950041001002)

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
blks_1990 <- tribble(~fips,
             39095003800201,
             39095003800202,
             39095003800203,
             39095003800204,
             39095003800206,
             39095003800207,
             39095003800208,
             39095003800209,
             39095003800210,
             39095003800211,
             39095003800213,
             39095003800214,
             39095003800219,
             39095003800220,
             39095003800221,
             39095003800401,
             39095003800402,
             39095003800403,
             39095003800404,
             39095004100101,
             39095004100102,
             39095004100103
             )

#list of block groups
blk_grps_1990 <- c(390950038002,390950038004,390950041001)
blk_grps_2000 <- c(390950041001,390950038001)
blk_grps_2010 <- c(2,3)

#list of variables
vars1990 <- as_tibble(listCensusMetadata(name = "sf1",
                                         vintage = 1990,
                                         type = "variables")
)
vars2000 <- as_tibble(listCensusMetadata(name = "sf1",
                                         vintage = 2000,
                                        type = "variables")
)

vars2010 <- tribble(~name,
                      "P0010001", #Total population
                      "P0030001", #Total Population
                      "H0030001", #Housing Units
                      "H0030002", #occupied housing units
                      "H0030003", #vacant housing units
                      "H0040004", #renter occupied units
                      "H0070003", #householder not hispanic or latino who is white alone
                      "H0110004" #population in renter occupied housing units
                      )
## Acquire and tidy 1990 block data
blk1990data <- tibble()
for (i in unique(substr(blks_1990$fips,6,11))){
  temp <-as_tibble(getCensus(name="sf1", 
                             vintage = 1990, 
                             vars =  c("P0010001","P0030001"), 
                             region = "block:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = paste("state:39+county:095+tract:",i,sep = ""),                        
                             key = api_key ))
  blk2000data <-rbind(blk2000data,temp)
  
}
## filter blocks to only those in Middle Grounds District 
filter(unite(blk2000data, state, county, tract, block, col="GEOID",sep = "",remove = FALSE), GEOID %in% blks_2000$fips)


## Acquire and tidy 2000 block data
blk2000data <- tibble()
for (i in unique(substr(blks_2000$fips,6,11))){
  temp <-as_tibble(getCensus(name="sf1", 
                             vintage = 2000, 
                             vars =  c("P001001","P003001"), 
                             region = "block:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = paste("state:39+county:095+tract:",i,sep = ""),                        
                             key = api_key ))
              blk2000data <-rbind(blk2000data,temp)
  
}
## filter blocks to only those in Middle Grounds District 
filter(unite(blk2000data, state, county, tract, block, col="GEOID",sep = "",remove = FALSE), GEOID %in% blks_2000$fips)

## Acquire and tidy 2010 block data
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
