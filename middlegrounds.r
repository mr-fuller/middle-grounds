library(censusapi)

#api key
api_key = "b7da053b9e664586b9e559dba9e73780602f0aab"

# list of blocks
blks <- c(3001,xxxx)

#list of block groups
blk_grps <- c(2,3)

#list of variables
vars <- c("P0010001","P0030001")

blk2010data <- getCensus(name="sf1", vintage = 2010, vars = vars, region = "block:"+','.join(blks), 
  regionin = "state:39+county:095+tract:010300", key= api_key)
