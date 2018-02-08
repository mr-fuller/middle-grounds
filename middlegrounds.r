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
                      "P0120003", #male 0-5
                      "P0120004", #male 5-9
                      "P0120005", #male 10-14
                      "P0120006", #male 15-17
                      "P0120007", #male 18-19
                      "P0120008", #male 20
                      "P0120009", #male 21
                      "P0120010", #male 22-24
                      "P0120011", #male 25-29
                      "P0120012", #male 30-34
                      "P0120013", #male 35-39
                      "P0120014", #male 40-44
                      "P0120015", #male 45-49
                      "P0120016", #male 50-54
                      "P0120017", #male 55-59
                      "P0120018", #male 60-61
                      "P0120019", #male 62-64
                      "P0120020", #male 65-66
                      "P0120021", #male 67-69
                      "P0120022", #male 70-74
                      "P0120023", #male 75-79
                      "P0120024", #male 80-84
                      "P0120025", #male 85+
                      "P0120027", #female 0-5
                      "P0120028", #female 5-9
                      "P0120029", #female 10-14
                      "P0120030", #female 15-17
                      "P0120031", #female 18-19
                      "P0120032", #female 20
                      "P0120033", #female 21
                      "P0120034", #female 22-24
                      "P0120035", #female 25-29
                      "P0120036", #female 30-34
                      "P0120037", #female 35-39
                      "P0120038", #female 40-44
                      "P0120039", #female 45-49
                      "P0120040", #female 50-54
                      "P0120041", #female 55-59
                      "P0120042", #female 60-61
                      "P0120043", #female 62-64
                      "P0120044", #female 65-66
                      "P0120045", #female 67-69
                      "P0120046", #female 70-74
                      "P0120047", #female 75-79
                      "P0120048", #female 80-84
                      "P0120049", #female 85+
                    "P0130001", #median age both sexes
                    "P0130002", #median age male
                    "P0130003", #median age female
                    "P0180001", #households
                    "P0180002", #family households
                    "P0290020", #male householder living alone 
                    "P0290023", #female householder living alone 
                    "P0420001" #pop in group quarters?
                    
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
