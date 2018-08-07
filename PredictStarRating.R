# Goal: Predict star ratings given all the amenities that the business offers

yelp_data <- read.csv("yelp_restaurants.txt", stringsAsFactors = FALSE)

# columns needed:
colnames(yelp_data)
df <- yelp_data[, !names(yelp_data) %in% c('X','SrNo','business.id', 'name', 'address', 'neighborhood', 
                                           'postal.code','latitude', 'longitude', 'review.count', 'is.open', 'type', 
                                           'total.num.tip.likes','total.num.tips', 'Num.of.Checkins', 'Type.of.Checkins')]

# as we don't have all states and cities - let's remove
# or we could do this analysis for just 1 state / city
library(dplyr)
#install.packages("VIM")
library(VIM)

df %>% group_by(city) %>% summarise(n = n()) %>% arrange(desc(n))
df <- filter(df, city == 'Las Vegas')
df$state <- NULL 
df$city <- NULL
str(df)

## CLEANING TIMINGS COLUMN - Close hours - reduce to hour level from hour-min level
extract_hour <- function(x) { 
  return (as.numeric(strsplit(x, ":")[[1]][1]))
}

timing_cols <- c("Friday.close","Friday.open","Monday.close","Monday.open",
                 "Saturday.close","Saturday.open","Sunday.close","Sunday.open",
                 "Thursday.close","Thursday.open","Tuesday.close","Tuesday.open",
                 "Wednesday.close","Wednesday.open")
timings <- df[,timing_cols]
timings[is.na(timings) == TRUE] <- 99

#timings <- kNN(timings, k=5)
#timings <- timings[1:14]

colSums(is.na(timings))
colnames(timings)

# for all timing columns
timings_cleaned <- as.data.frame(apply(timings, 2, function(x) 
  as.data.frame(apply(as.array(x), 1, extract_hour))))
colnames(timings_cleaned) <- timing_cols
# timings_cleaned[is.na(timings_cleaned) == TRUE] <- 'NA'
timings_cleaned <- as.data.frame(apply(timings_cleaned, 2, as.numeric))

# new features wrt timing
df <- df[,!names(df) %in% timing_cols]
df$openhours_per_week <- 
  abs(timings_cleaned$Friday.close - timings_cleaned$Friday.open) +
  abs(timings_cleaned$Saturday.close - timings_cleaned$Saturday.open) +
  abs(timings_cleaned$Sunday.close - timings_cleaned$Sunday.open) +
  abs(timings_cleaned$Monday.close - timings_cleaned$Monday.open) +
  abs(timings_cleaned$Tuesday.close - timings_cleaned$Tuesday.open) +
  abs(timings_cleaned$Wednesday.close - timings_cleaned$Wednesday.open) +
  abs(timings_cleaned$Thursday.close - timings_cleaned$Thursday.open) 
df$Friday.open <- ifelse(timings_cleaned$Friday.open != 99,1,0)
df$Saturday.open <- ifelse(timings_cleaned$Saturday.open != 99,1,0)
df$Sunday.open <- ifelse(timings_cleaned$Sunday.open != 99,1,0)
#df <- cbind(df, timings_cleaned)

## CHUCK COLS WITH SPARCITY
df$coloring. <- NULL
df$perms. <- NULL
df$monday. <- NULL
df$dairy.free. <- NULL
df$AcceptsInsurance <- NULL
df$AgesAllowed <- NULL
df$BusinessAcceptsBitcoin <- NULL
df$BYOB <- NULL
df$BYOBCorkage <- NULL
df$ByAppointmentOnly <- NULL
df$Open24Hours <- NULL
df$RestaurantsCounterService <- NULL
df$Corkage <- NULL
df$Smoking <- NULL
df$Food <- NULL

## CLEANING CUISINE COLUMNS - Make them 1/0
cuisine_cols <- c("American..New.","American..Traditional.","Asian.Fusion","Barbeque","Bars",
                  "Breakfast...Brunch","Burgers","Chinese","Diners","Fast.Food","Greek",
                  "Indian","Italian","Japanese","Korean","Mediterranean","Mexican","Middle.Eastern",
                  "Nightlife","Pakistani","Pizza","Sandwiches","Seafood","Sushi.Bars","Thai",
                  "Vegetarian","Vietnamese")
cuisine <- df[,cuisine_cols]

cuisine[is.na(cuisine) == TRUE] <- 0
cuisine <- as.data.frame(apply(cuisine, 2, as.numeric))
colSums(is.na(cuisine))
str(cuisine)

df <- df[,!names(df) %in% cuisine_cols]
df <- cbind(df, cuisine)

## CLEANING AMENITIES COLUMNS
amenities_cols1 <- c("dessert.","dj.","garage.","romantic.","BikeParking",
                     "BusinessAcceptsCreditCards","Caters","CoatCheck","DogsAllowed",
                     "DriveThru","GoodForDancing","GoodForKids","HappyHour","HasTV",
                     "OutdoorSeating","RestaurantsDelivery","RestaurantsGoodForGroups",
                     "RestaurantsReservations","RestaurantsTableService",
                     "RestaurantsTakeOut","WheelchairAccessible")
amenities_cols2 <- c("Alcohol","NoiseLevel","RestaurantsAttire",
                     "RestaurantsPriceRange2","WiFi")
amenities_cols <- c(amenities_cols1, amenities_cols2)
amenities1 <- df[,amenities_cols1]
amenities1[is.na(amenities1) == TRUE] <- 0

for (col in colnames(amenities1))
{
  amenities1[,col] <- ifelse(amenities1[,col] == " True", 1, 0)
}

amenities2 <- df[,amenities_cols2]
amenities2[is.na(amenities2) == TRUE] <- 0

table(amenities2$Alcohol)
amenities1$Alcohol_beer_wine <- ifelse(amenities2$Alcohol == " beer_and_wine", 1, 0)
amenities1$Alcohol_fullbar <- ifelse(amenities2$Alcohol == " full_bar", 1, 0)
#amenities2$Alcohol_none <- ifelse(amenities2$Alcohol == " none" | amenities2$Alcohol == 0, 1, 0)

table(amenities2$NoiseLevel)
amenities1$noise_loud <- ifelse(amenities2$NoiseLevel == " loud" | amenities2$NoiseLevel == " very_loud", 1, 0)
amenities1$noise_quiet <- ifelse(amenities2$NoiseLevel == " quiet", 1, 0)
#amenities2$noise_avg <- ifelse(amenities2$NoiseLevel == " average" | amenities2$NoiseLevel == 0, 1, 0)

table(amenities2$RestaurantsAttire)
amenities1$attire_casual <- ifelse(amenities2$RestaurantsAttire == " casual" | amenities2$RestaurantsAttire == 0, 1, 0)
#amenities2$attire_dressy <- ifelse(amenities2$RestaurantsAttire == " dressy" | amenities2$RestaurantsAttire == " formal", 1, 0)

amenities1$RestaurantsPriceRange2 <- as.numeric(amenities2$RestaurantsPriceRange2)

table(amenities2$WiFi)
amenities1$wifi <- ifelse(amenities2$WiFi == " free" | amenities2$RestaurantsAttire == " paid", 1, 0)

#amenities <- as.data.frame(apply(amenities, 2, as.factor))
colSums(is.na(amenities1))

str(amenities)

df <- df[,!names(df) %in% amenities_cols]
df <- cbind(df, amenities1)

str(df)

### stars > 4.5
df$stars <- ifelse(df$stars >= 4,"high","low")
df$stars <- as.factor(df$stars)

table(df$stars) *100 / nrow(df)
str(df)
# df <- as.data.frame(apply(df, 2, as.factor))

#### LOGISTIC MODEL to predict star (factor)

##### GLM
str(trainSet)

# removing insignificant ones
df$Saturday.open <- NULL
df$dj. <- NULL
df$romantic. <- NULL
df$BusinessAcceptsCreditCards <- NULL
df$CoatCheck <- NULL
df$GoodForDancing <- NULL
df$HappyHour <- NULL
df$OutdoorSeating <- NULL
df$RestaurantsDelivery <- NULL
df$RestaurantsGoodForGroups <- NULL
df$Alcohol_beer_wine <- NULL
df$noise_quiet <- NULL
df$RestaurantsPriceRange2 <- NULL
df$RestaurantsTakeOut <- NULL
df$RestaurantsReservations <- NULL
df$Vietnamese <- NULL
df$Sandwiches <- NULL
df$Seafood <- NULL
df$Sushi.Bars <- NULL  
df$Thai <- NULL  
df$Middle.Eastern <- NULL   
df$Nightlife <- NULL 
df$Korean <- NULL  
df$Mediterranean <- NULL
df$Italian <- NULL
df$Greek <- NULL
df$Diners <- NULL
df$Bars <- NULL   
df$Breakfast...Brunch <- NULL  
df$Burgers <- NULL

temp <- df[,c(1,2)]
df <- df[,-c(1,2)]
df <- as.data.frame(apply(df, 2, as.factor))
str(df)
df <- cbind(temp, df)

library(caret)
set.seed(7)
index <- createDataPartition(df$stars, p = .70, list = FALSE)
trainSet <- df[index,]
vSet <- df[-index,]

##### Ranger model 1
# objControl <- trainControl(classProbs = TRUE)
# metric = 'Accuracy'
# 
# #colnames(df) <- make.names(colnames(df))
# #trainSet<- trainSet[,1:42]
# colSums(is.na(trainSet))
# str(df)
# 
# set.seed(7)
# ranger_model <- caret::train(stars~., data = trainSet, method = "ranger", metric = metric,
#                              trControl = objControl, importance = "impurity")
# ranger_model
# 
# pred <- predict(ranger_model, trainSet)
# table(pred, trainSet$stars)
# (1851+3233)/nrow(trainSet)
# 
# pred <- predict(ranger_model, vSet)
# table(pred, vSet$stars)
# (327+1146)/nrow(vSet)
# 
# ##### Ranger model 2
# objControl <- trainControl(method = "repeatedcv",
#                            number = 5,
#                            repeats = 3,
#                            verboseIter = FALSE,
#                            classProbs = TRUE)
# metric = 'Accuracy'
# Grid <- expand.grid(mtry = c(7,9,11), 
#                     splitrule = c("gini"),
#                     min.node.size = 5)
# 
# set.seed(7)
# ranger_model2 <- caret::train(stars~., data = trainSet, method = "ranger", metric = metric,
#                              trControl = objControl, tuneGrid = Grid, importance = 'impurity')
# ranger_model2 <- ranger_model
# ranger_model2
# 
# ## min.node.size = 1
# pred <- predict(ranger_model2, trainSet)
# table(pred, trainSet$stars)
# (1822+3232)/nrow(trainSet)
# 
# pred <- predict(ranger_model2, vSet)
# table(pred, vSet$stars)
# (316+1171)/nrow(vSet)

glm_model <- caret::train(stars~., data = trainSet, method = "glm", preProcess=c("center","scale"))
summary(glm_model)
glm_coefs <- as.data.frame(summary(glm_model)$coefficients)
glm_coefs$OR <- exp(glm_coefs$Estimate)
glm_coefs$name <- rownames(glm_coefs)
glm_coefs <- glm_coefs[-1,]
library(ggplot2)
glm_coefs %>% arrange(desc(OR)) %>% 
  ggplot(aes(y=OR, x=reorder(name, -OR))) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs("Coeff")

pred <- predict(glm_model, trainSet)
table(pred, trainSet$stars)
(714+1778)/nrow(trainSet)

pred <- predict(glm_model, vSet)
table(pred, vSet$stars)
(307+745)/nrow(vSet)

Set <- c('Train', 'Validation', 'AUC')
Logistic <- c('70.7%', '69.8%', '0.66')

acc_table <- data.frame(rbind(Set, Accuracy))
library(pROC)
roc(as.numeric(stars)~as.numeric(pred),data=vSet)

##### Ranger model 2
objControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           verboseIter = FALSE,
                           classProbs = TRUE)
metric = 'Accuracy'
Grid <- expand.grid(mtry = c(7,9,11),
                    splitrule = c("gini"),
                    min.node.size = 5)

set.seed(7)
ranger_model2 <- caret::train(stars~., data = trainSet, method = "ranger", metric = metric,
                              trControl = objControl, tuneGrid = Grid, importance = 'impurity')
ranger_model2

pred <- predict(ranger_model2, trainSet)
table(pred, trainSet$stars)
(1086+2059)/nrow(trainSet)

pred <- predict(ranger_model2, vSet)
table(pred, vSet$stars)
(320+753)/nrow(vSet)

Set <- c('Train', 'Validation', 'AUC')
RF <- c('89.3%', '71.2%', '0.68')

acc_table <- data.frame(rbind(Set, Logistic, RF))

roc(as.numeric(stars)~as.numeric(pred),data=vSet)

## graphs
# library(ggplot2)
# df %>% group_by(openhours_per_week, stars) %>% summarise(n = n()) %>% 
#   ggplot(aes(x = stars, y = n)) + geom_bar(stat = "identity") + 
#   facet_grid(.~openhours_per_week)

df %>% group_by(stars) %>% summarise(n = n()) %>% 
  ggplot(aes(x = stars, y = n)) + geom_bar(stat = "identity")

auc
