library(censusapi)
library(tidyverse)
#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

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

blk_grps_2000 <- c(390950041001,390950038001)

vars2000 <- tribble(~name,
                    "P001001", #Total population
                    
                    "H003001", #Housing Units
                    "H003002", #occupied housing units
                    "H003003", #vacant housing units
                    "H004003", #renter occupied units
                    "H007001", #total householders
                    "H007003", #householder not hispanic or latino who is white alone
                    "H011003", #population in renter occupied housing units
                    "H012001", #average household size
                    "H012002", #average household size for owner-occupied
                    "H012003", #average household size for renters
                    "P008003", #population not hispanic or latino, white alone
                    
                    "P013001", #median age both sexes
                    "P013002", #median age male
                    "P013003", #median age female
                    "P027002", #households
                    "P027003", #family households
                    "P027018", #male householder living alone 
                    "P027021", #female householder living alone 
                    "P027024" #pop in group quarters? institutionalized 
                    
)
getDemoVars <- function(year){
  #retrieve all variables
  vars <- as_tibble(listCensusMetadata(name = "sf1",
                                       vintage = year,
                                       type = "variables"))
  #filter to sex by age variables
  vars <- filter(vars,vars$name %in% vars2000$name)
  #remove all 'total' variables
  vars <- arrange(vars,name)#[-c(1:2,26),]
  return(vars)
}
demoVars2000 <- getDemoVars(2000)

blk2000data <- tibble()
for (i in unique(substr(blks_2000$fips,6,11))){
  temp <-as_tibble(getCensus(name="sf1", 
                             vintage = 2000, 
                             vars =  demoVars2000$name, 
                             region = "block:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = paste("state:39+county:095+tract:",i,sep = ""),                        
                             key = api_key ))
  blk2000data <-rbind(blk2000data,temp)
  
}

## filter blocks to only those in Middle Grounds District 
blk2000data <- filter(unite(blk2000data, state, county, tract, block, col="GEOID",sep = "",remove = FALSE), GEOID %in% blks_2000$fips)
#rename
setnames(blk2000data, old = as.character(demoVars2000$name), new = as.character(demoVars2000$label))

race2000 <- plot_ly(blk2000data,
                    x = "2000", 
                    y = ~(sum(blk2000data$`HISPANIC:NotHisp:White alone`)/ sum(blk2000data$`Population:Total [1` )*100),
                    name = 'White Alone, not Hispanic', type = 'bar')%>%
  add_trace(y = ~(sum(blk2000data$`Population:Total [1`) - sum(blk2000data$`HISPANIC:NotHisp:White alone`))/sum(blk2000data$`Population:Total [1`)*100,
            name = "People of Color")%>%
  layout(yaxis = list(title = "Percentage"),barmode = 'stack')

#read in a spatial layer

#join data to spatial layer on GEOID

#put spatial layer on interactive map