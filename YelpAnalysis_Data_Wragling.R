library(sparklyr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(foreach)
library(doMC)
library(stringr)
library(tidyr)
library(lubridate)

install.packages('lubridate')

Sys.setenv(SPARK_HOME = "/usr/lib/spark")
config <- spark_config()
sc <-
  spark_connect(master = "yarn-client",
                config = config,
                version = '2.2.0')

businessDF <- as.data.frame(business)


for (i in colnames(businessDF)) {
  if (max(sapply(businessDF[[i]], length)) == 1) {
    businessDF[[i]] <- as.character(businessDF[[i]])
  }
}

## Attributes as kind dictionay format, not sure how else to parse in R, running a for loop

d <- vector('list')
n <- 0
k <- 0
for (z in businessDF$attributes) {
  k <- k + 1
  businessId <- businessDF$business_id[[k]]
  #print(paste0("File Processed ", businessId))
  for (i in z) {
    x <-
      str_split(gsub('(([a-z]*?:[[:space:]]\\{)|\\})', '', i, ignore.case = TRUE),
                ',')
    for (y in x) {
      n = n + 1
      s <- str_split(y, ':')
      d[[n]] <-  data.frame(businessId, s[[1]][1], s[[1]][2])
      
    }
    #print(x)
  }
}


## Binding all the dataframes together

f <- bind_rows(d)
colnames(f) <- c("businessId", "Attribute", "Value")

## Spreading the attributes to there own column

attibutesDF <- spread(f, key = Attribute, value = Value)


## Next step categories, a resturant belongs to multiple categories
c <- vector('list')
n <- 0
k <- 0
for (z in businessDF$categories) {
  k <- k + 1
  businessId <- businessDF$business_id[[k]]
  #print(paste0("File Processed ", businessId))
  for (i in z) {
    x <-
      str_split(gsub('(([a-z]*?:[[:space:]]\\{)|\\})', '', i, ignore.case = TRUE),
                ',')
    for (y in x) {
      n = n + 1
      s <- str_split(y, ':')
      c[[n]] <-  data.frame(businessId, s[[1]][1], s[[1]][2])
      
    }
    #print(x)
  }
}

### Category is unique to business, No Need to spread
fc <- bind_rows(c)
colnames(fc) <- c("businessId", "Attribute", "Value")


fcRest <-
  fc[fc$businessId %in% fc$businessId[fc$Attribute == "Restaurants"], ]
fcRest$Value <- TRUE

fcRest <-
  fcRest[fcRest$Attribute %in% c(
    "Sandwiches",
    "Fast Food",
    "Nightlife",
    "Pizza",
    "Bars",
    "Mexican",
    "Food",
    "American (Traditional)",
    "Burgers",
    "Chinese",
    "Italian",
    "American (New)",
    "Breakfast & Brunch",
    "Thai",
    "Indian",
    "Sushi Bars",
    "Korean",
    "Mediterranean",
    "Japanese",
    "Seafood",
    "Middle Eastern",
    "Pakistani" ,
    "Barbeque",
    "Vietnamese",
    "Asian Fusion",
    "Diners",
    "Greek",
    "Vegetarian"
  ), ]

## categoriesDF has businessid of all the resturants
categoriesDF <- spread(fcRest, key = Attribute, value = Value)



u <- vector('list')
n <- 0
k <- 0
for (z in businessDF$hours) {
  k <- k + 1
  businessId <- businessDF$business_id[[k]]
  #print(paste0("File Processed ", businessId))
  for (i in z) {
    x <-
      str_split(gsub('(([a-z]*?:[[:space:]]\\{)|\\})', '', i, ignore.case = TRUE),
                ',')
    for (y in x) {
      n = n + 1
      s <- str_split(y, ' ')
      time <- str_split(s[[1]][2], '-')
      u[[n]] <-
        data.frame(businessId,
                   Attribute = paste0(s[[1]][1], '-open'),
                   Value = time[[1]][1])
      n = n + 1
      u[[n]] <-
        data.frame(businessId,
                   Attribute = paste0(s[[1]][1], '-close'),
                   Value = time[[1]][2])
      
    }
    #print(x)
  }
}

## Binding all the dataframes together

fh <- bind_rows(u)
#sapply(fh$Value , function(x){str_split(x,":")[[1]][1]})

#colnames(fh) <- c("businessId","Attribute","Value")


fhRestaurants <- fh[fh$businessId %in% categoriesDF$businessId, ]

fhRestaurants$hour <-
  as.numeric(str_replace(fhRestaurants$Value, ":", '.'))



moreTimingsPerday <-
  fhRestaurants %>% group_by(businessId, Attribute) %>% summarise(n = n(),
                                                                  max = max(hour),
                                                                  min = min(hour)) %>% filter(n > 1)


finalTiming <-
  bind_rows(
    moreTimingsPerday[grep('-close', moreTimingsPerday$Attribute), c(1, 2, 4)] %>% inner_join(
      fhRestaurants,
      by = c(
        "businessId" = "businessId",
        "Attribute" = "Attribute",
        "max" = "hour"
      )
    ) %>% select(businessId, Attribute, Value)
    ,
    moreTimingsPerday[grep('-open', moreTimingsPerday$Attribute), c(1, 2, 5)] %>% inner_join(
      fhRestaurants,
      by = c(
        "businessId" = "businessId",
        "Attribute" = "Attribute",
        "min" = "hour"
      )
    ) %>% select(businessId, Attribute, Value)
    ,
    fhRestaurants %>% anti_join(
      moreTimingsPerday,
      by = c("businessId" = "businessId", "Attribute" = "Attribute")
    ) %>% select(businessId, Attribute, Value)
  )
## Spreading the attributes to there own column

openDF <- spread(unique(finalTiming), key = Attribute, value = Value)


### joinin all the intermeditae tables


yelp_restaurants <-
  select(businessDF,-attributes,-categories,-hours) %>% inner_join(openDF, by =
                                                                     c("business_id" = "businessId")) %>% inner_join(categoriesDF, by = c("business_id" =
                                                                                                                                            "businessId")) %>% inner_join(attibutesDF, by = c("business_id" =


colnames(yelp_restaurants) <- make.names(trimws(sub("\'",'',colnames(yelp_restaurants))),unique = TRUE,allow_ = FALSE)
                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                "businessId"))

#### Tips table
tipDF <- as.data.frame(apply(tip, 2, unlist))
class(tipDF$text)

for (i in colnames(tipDF)) {
  if (max(sapply(tipDF[[i]], length)) == 1) {
    tipDF[[i]] <- as.character(tipDF[[i]])
  }
}

checkinDF <- as.data.frame(checkin)

getTotalCheckins <- function(y) {
  sum(sapply(str_split(y, ":")
             , function(x) {
               as.numeric(x[[2]])
             }))
}

checkinRealDataFrame <-
  as.data.frame(cbind(
    unlist(checkinDF$business_id),
    sapply(checkinDF$time, getTotalCheckins),
    unlist(checkinDF$type)
  ))

colnames(checkinRealDataFrame) <-
  c("business_id", "Num_of_Checkins", "Type_of_Checkins")


### Joing tips, checking and business data

tipCheckinData <- tipDF %>% group_by(business_id) %>% summarise(total_num_tip_likes=sum(as.numeric(likes)),total_num_tips=n()) %>% full_join(checkinRealDataFrame, by = "business_id")

yelpFinaleSet <- yelp_restaurants %>% left_join(tipCheckinData, by = c("business.id"="business_id"))

colnames(yelpFinaleSet) <- make.names(trimws(sub("\'",'',colnames(yelpFinaleSet))),unique = TRUE,allow_ = FALSE)

colnames(yelpFinaleSet)

write.csv(yelpFinaleSet,file = "yelp_restaurants.txt",fileEncoding = "UTF-8")
