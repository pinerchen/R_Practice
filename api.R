################################################
#### API Getting data from SBA ####
################################################
install.packages("httr")
install.packages("jsonlite")
install.packages("magrittr")
install.packages("gtools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("rjson")
library(httr)
library(jsonlite)
library(magrittr)
library(gtools)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(rjson)

# verb (method) = GET
# SAM URL (endpoint) API KEY = https://api.data.gov/sam/v1/registrations/1459697830000&api_key=2fUK4nRTlChuw4TohSL6JiE1KOiTpqF9B6IvHXuL
# parameter = search

# search by customized header
key <- "2fUK4nRTlChuw4TohSL6JiE1KOiTpqF9B6IvHXuL"
test <- GET("https://api.data.gov/sam/v1/registrations?qterms=sba8AProgram:true&start=1&length=500" , query = list( api_key = key))
test <- GET("https://api.data.gov/sam/v1/registrations" , query = list(qterms=(duns=169828675),api_key = key))
testv4 <- GET("https://api.data.gov/samdata/v3/registrations?qterms=sba8AProgram:true" , query = list( api_key = key))%>% 
  stop_for_status() %>% 
  json_parse
# request status
test$status_code
test$headers$`content-type`
names(test)


# -------------------------------

# Helper function
json_parse <- function(req) {
  text <- content(req, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warn("No output to parse.")
  fromJSON(text)
}

# List results
eightA <- GET("https://api.data.gov/sam/v1/registrations?qterms=sba8AProgram:true&start=1&length=10" , query = list( api_key = key)) %>% stop_for_status()
json_eightA <- json_parse(eightA)

# The response includes metadata as well as results
names(json_eightA)
json_eightA$results
length(json_eightA$results)
next500 <- json_eightA[["links"]][["href"]][[2]]

api_eightA <- json_eightA$results
api_eightA$duns

# Get the next page of results based on the content of the `next` field
next_page <- GET(next500) %>% stop_for_status()

# Use a function to parse the results
parsed_next_page <- json_parse(next_page)
parsed_next_page$results$duns

# If the API results come back paged like this, you can write a loop to follow the next URL 
# until the there are no more pages, and rbind all the data into a single dataframe.

# Grab data on all of the Star Wars planets
request <- GET("https://api.data.gov/sam/v1/registrations?qterms=sba8AProgram:true&start=1&length=500" , query = list( api_key = key)) %>% 
  stop_for_status() %>% 
  json_parse

data <- flatten(request$results$links)
strindex <- which(request$links$rel== 'next')
next500 <- request$links[index,2]


while(length(index) != 0 ) {
  more_request <- GET(next500, query = list( api_key = key)) %>% 
    stop_for_status() %>% 
    json_parse()
  split_more <- flatten(more_request$results)
  #data <- rbind(data, split_more)
  data <- smartbind(data,split_more,fill=NA)
  #data <- smartbind(data, more_request$results,fill=0)
  #data$samAddress <- cleansamaddress
  index <- which(more_request$links$rel== 'next')
  next500<- more_request$links$href[index]
}

#######################################
### Data wrangling ####
########################
library(stringr)

# select desire columns, expirationDate(2018,2017), seperate
# select expiration data is 2017 & 2018
data2 <- data %>% tidyr::separate(expirationDate, c("exp.year", "exp.month","exp.date"), extra = "drop", fill = "right")

# make sure # of company expire between 2017 and 2018
table(data2$exp.year) #280+5595=5875
expir2018 <- dplyr::filter(data2, exp.year > 2016)

# load commercial dataset
commercial <- read.csv("commercial.csv",header = TRUE)
commercial <- as.data.frame(commercial) %>% select(1,3) 
colnames(commercial) <- c("legalBusinessName","categories") 
com8a <- commercial %>% filter(str_detect(categories,"8")) # all 8(a) data
com8a$legalBusinessName <- as.character(com8a$legalBusinessName)

diff <- anti_join(com8a, expir2018, by = "legalBusinessName")

####################
###NAICs search & 8a companies#####
####################
naics <- GET("https://api.data.gov/sam/v1/registrations?qterms=sba8AProgram:true+AND+naicsLimitedSB:(541110,541211,541330,541380,541511,541512,541513,541519,541611,541614,541618,541620,541690,541712,541930,541990)&start=1&length=500&api_key=2fUK4nRTlChuw4TohSL6JiE1KOiTpqF9B6IvHXuL") %>% 
  stop_for_status() %>% 
  json_parse
naicsdata <- naics$results
index <- which(naics$links[[2]]$rel== 'next')
next500 <- naics$links[index,2]
detail <- naics[["results"]][[1]][["links"]][[1]][["href"]]

while(length(index) != 0 ) {
  more_request <- GET(next500, query = list( api_key = key)) %>% 
    stop_for_status() %>% 
    json_parse()
  split_more <- flatten(more_request$results)
  #data <- rbind(data, split_more)
  naicsdata <- smartbind(naicsdata,split_more,fill=NA)
  #data <- smartbind(data, more_request$results,fill=0)
  #data$samAddress <- cleansamaddress
  index <- which(more_request$links$rel== 'next')
  next500<- more_request$links$href[index]
}

#################################################
############## USAspending ####################
############################################
#POST request
json_file <- upload_file("body.json")
json_arg <- toJSON(json_file)

test <- POST(
  url = "https://api.usaspending.gov/api/v1/awards/",
  body = json_arg,
  add_headers (
    "Content-Type" = "application/json")) %>% stop_for_status() %>% json_parse

test$results

