library(dplyr)
library(tidytext)
library(ggmap)
library(leaflet)
library(tidytext)
library(stringr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(htmlwidgets)
library(gridExtra)
library(knitr)
library(wordcloud)
library(lubridate)
library(stringr)
library(ggmap)
library(tidyverse)
library(statar)
yelp_data <- read.csv("./data/yelp_restaurants.txt")
tips <- readRDS('./data/tip.rds')
tipDF <- as.data.frame(apply(tips, 2, unlist))
vegas <- yelp_data %>% 
  filter(city=="Las Vegas")
result_yelp <- vegas %>% 
  select(name,stars,review.count,latitude,longitude)

## Load the inspection data and select useful variables
inspection <- read.csv("./data/Restaurant_Inspections.csv") %>% 
  filter(City=="Las Vegas") %>% 
  filter(Inspection.Type=="Routine Inspection") %>% 
  filter(nchar(as.character(Location.1))==25) %>% 
  select("Restaurant.Name","Location.Name","Category.Name","Address","City","Zip","Zip.Codes","Location.1","Current.Demerits","Current.Grade","Employee.ID","Inspection.Type","Inspection.Demerits",'Inspection.Grade',"Inspection.Result","Permit.Status","Violations","Inspection.Date")

inspection <- inspection[!duplicated(inspection),]

inspection$latitude=substr(inspection$Location.1,2,stop = 10)
inspection$longitude=str_trim(substr(inspection$Location.1,13,22))
inspection$longitude=ifelse(substr(inspection$longitude,1,1)!="-",paste0('-',inspection$longitude),inspection$longitude)


result_inspection <- inspection %>%
  group_by(Restaurant.Name) %>% 
  summarize(nr_A=sum(Inspection.Grade=='A'|Inspection.Grade=="a"),
            nr_B=sum(Inspection.Grade=="B"|Inspection.Grade=="b"),
            nr_C=sum(Inspection.Grade=='C'),
            nr_O=sum(Inspection.Grade=="O"),
            nr_P=sum(Inspection.Grade=="P"),
            nr_S=sum(Inspection.Grade=="S"),
            nr_X=sum(Inspection.Grade=="X"),
            nr_inspection=n(),
            A_rate=round(nr_A/nr_inspection,2),
            lat=latitude[1],
            lon=longitude[1]) %>% 
  mutate(A_dec=xtile(A_rate,5))


result_inspection$lat <- as.numeric(result_inspection$lat)
result_inspection$lon <- as.numeric(result_inspection$lon)
result_inspection <- na.omit(result_inspection)

result_yelp$latitude <- as.numeric(result_yelp$latitude)
result_yelp$longitude<- as.numeric(result_yelp$longitude)

vegas.map <- get_map("las vegas")
ggmap(vegas.map)+
  geom_point(data = result_inspection,
             aes(x=lon,y=lat,col=A_rate),
             size=1,alpha=.1)+
  facet_wrap(~A_dec) + 
  theme(legend.position="none")

ggmap(vegas.map)+
  geom_point(data = result_yelp,
             aes(x=longitude,y=latitude,col=stars),
             size=1,alpha=.1)+
  facet_wrap(~stars) + 
  theme(legend.position="none")

ggmap(vegas.map)+
  geom_point(data = result_yelp,
             aes(x=longitude,y=latitude,col=review.count),
             size=1,alpha=.1)


unique(result_inspection_geog$Restaurant.Name)

result_yelp$name <- as.character(result_yelp$name)
result_inspection$Restaurant.Name <- as.character(result_inspection$Restaurant.Name)

combined <- inner_join(result_yelp,result_inspection,by=c("name"="Restaurant.Name")) %>% 
  mutate(A_dec=xtile(A_rate,10,rev=TRUE)) 

lm(combined$stars~combined$review.count+combined$A_rate)  
lm(combined$review.count~combined$A_rate)  

