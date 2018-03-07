library(censusapi)
library(tidyverse)
library(stringr)
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

sexbyageorder = c(
  "Male:0-4" ,
  "Male:5-9" ,
  "Male:10-14" ,
  "Male:15-19" ,
  
  "Male:20-24" ,
  
  "Male:25-29" ,
  "Male:30-34" ,
  "Male:35-39" ,
  "Male:40-44" ,
  "Male:45-49" ,
  "Male:50-54" ,
  "Male:55-59" ,
  "Male:60-64" ,
  
  "Male:65-69" ,
  
  "Male:70-74" ,
  "Male:75-79" ,
  "Male:80-84" ,
  "Male:85+" ,
  "Female:0-4" ,
  "Female:5-9" ,
  "Female:10-14" ,
  "Female:15-19" ,
  "Female:20-24" ,
  "Female:25-29",
  "Female:30-34",
  "Female:35-39",
  "Female:40-44",
  "Female:45-49",
  "Female:50-54",
  "Female:55-59",
  "Female:60-64",
  "Female:65-69",
  "Female:70-74",
  "Female:75-79",
  "Female:80-84",
  "Female:85+" 
)

getPyramidVars <- function(year){
  #retrieve all variables
  vars <- as_tibble(listCensusMetadata(name = "sf1",
                                       vintage = year,
                                       type = "variables"))
  #filter to sex by age variables
  vars <- filter(vars,str_detect(concept,"P12. Sex By Age"))
  #remove all 'total' variables
  vars <- arrange(vars,name)#[-c(1:2,26),]
  return(vars)
}
vars2000 <- getPyramidVars(2000)

## Acquire and tidy 2000 block data
blk2000data <- tibble()
for (i in unique(substr(blks_2000$fips,6,11))){
  temp <-as_tibble(getCensus(name="sf1", 
                             vintage = 2000, 
                             vars =  vars2000$name, 
                             region = "block:*",##,paste(substr(blks_2000$fips,12,15),collapse = ','),sep = ""), 
                             regionin = paste("state:39+county:095+tract:",i,sep = ""),                        
                             key = api_key ))
  blk2000data <-rbind(blk2000data,temp)
  
}

## filter blocks to only those in Middle Grounds District
blk2000data <- filter(unite(blk2000data, state, county, tract, block, col="GEOID",sep = "",remove = FALSE), GEOID %in% blks_2000$fips)
#reformat names for columns
li <- arrange(li,name)
li$label2 <- gsub(" to ", "-",substring(as.character(li$label),12))
li$label2 <- gsub("'&'","-",li$label2)
li$label2 <- gsub("'<'5","0-4",li$label2)
li$label2 <- gsub(" yrs-over","+",li$label2)
#rename columns
setnames(blk2000data, old = as.character(li$name), new = as.character(li$label2))

#function to combine cohorts in a census dataset
combineCohorts <- function(data){
  data <- data %>%
    #combine 15-17 and 18-19 cohorts
    mutate("Male:15-19" = (data$'Male:15-17'+data$"Male:18-19"))%>%
    mutate("Female:15-19" = (data$'Female:15-17'+data$"Female:18-19"))%>%
    #combine 20,21 and 22-24 cohorts
    mutate("Male:20-24" = (data$'Male:20'+data$'Male:21'+data$'Male:22-24'))%>%
    mutate("Female:20-24" = (data$'Female:20'+data$'Female:21'+data$'Female:22-24'))%>%
    #combine 60-61 and 62-64 cohorts
    mutate("Male:60-64" = (data$"Male:60-61"+data$'Male:62-64'))%>%
    mutate("Female:60-64" = (data$"Female:60-61"+data$'Female:62-64'))%>%
    #combine 65-66 and 67-69 cohorts
    mutate("Male:65-69" = (data$"Male:65-66"+data$'Male:67-69'))%>%
    mutate("Female:65-69" = (data$"Female:65-66"+data$'Female:67-69'))
  
  
}
blk2000data <- combineCohorts(blk2000data)




#drop unnecessary columns
blk2000data <- select(blk2000data,-contains("15-17"),-contains("18-19"),
                      -one_of(c('Male:20','Female:20')),
                      -matches("Male:21"),-matches("Female:21"),-contains("22-24"),
                      -contains("60-61"),-contains("62-64"),
                      -contains("65-66"),-contains("67-69"),
                      -contains("Total"))


blk2000data <- gather(blk2000data,sexbyageorder,key = "cohort", value = "pop")  


#sum by age and sex cohort
blk2000data <- blk2000data %>%
  group_by(cohort) %>%
  mutate(group_est = sum(pop)) %>%
  distinct(cohort, .keep_all = TRUE) %>%
  ungroup() %>%
  #arrange(cohort)%>%
  separate(cohort, into = c("Sex","Age"),sep = ":") %>%
  mutate(Age = factor(Age, levels = unique(Age)),
                        #C("0-4","5-9","10-14","15-19","20-24","25-29",
                         #             "30-34","35-39","40-44","45-49","50-54","55-59",
                          #            "60-64","65-69","70-74","75-79","80-84","85+"),ordered = TRUE),
         group_est = ifelse(Sex == "Male",-group_est,group_est))
  

#head(blk2000data)

#View(blk2000data[,c(1:5,25:28)])

pyramid <- ggplot(data = blk2000data, aes(x = Age, y = group_est, fill = Sex))+
  geom_bar(stat = "identity")+ #note that the options for geom_bar are 'identity' or 'count'
  #geom_text(aes(x=Age, y = pop, label = group_est,hjust="outward"))+#, position = position_dodge(width = 0.9))+
  scale_y_continuous(breaks = c(-5,0,5),labels = c("5","0","5"))+
  coord_flip()+
  
  scale_fill_manual(values = c("red","navy"))+
  annotate("text", x = 17, y = -5, label = "Total Population: 107")+
  labs(y = "Population",
       title = "Middle Grounds District 2000 Population",
       caption = "Source: US Census Bureau")
pyramid
