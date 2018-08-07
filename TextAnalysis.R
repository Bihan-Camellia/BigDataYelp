## @knitr loadfilelib
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

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

yelp_data <- read.csv("./data/yelp_restaurants.txt")
tips <- readRDS('./data/tip.rds')
tipDF <- as.data.frame(apply(tips, 2, unlist))

for (i in colnames(tipDF)) {
  if (max(sapply(tipDF[[i]], length)) == 1) {
    tipDF[[i]] <- as.character(tipDF[[i]])
  }
}

checkin <- readRDS("./data/checkin.rds")
checkinDF <- as.data.frame(checkin)

for (i in colnames(checkinDF)) {
  if (max(sapply(checkinDF[[i]], length)) == 1) {
    checkinDF[[i]] <- as.character(checkinDF[[i]])
  }
}

resttipDF <- tipDF[tipDF$business_id %in% yelp_data$business.id, ]



## @knitr wherearerest


usa_map <- get_map(location='united states',source = "google",zoom=4,maptype = "terrain",color = "bw")


usa1 <-  ggmap(usa_map) + geom_point(data = yelp_data,size=8,alpha=0.5,
                                     mapping = aes(
                                       x = longitude,
                                       y = latitude,
                                       color = factor(stars)
                                     ))+theme(legend.position = "none")


euro_map <-
  get_map(
    location = 'europe',
    source = "google",
    zoom = 4,
    maptype = "terrain",
    color = "bw"
  )

euro2 <- ggmap(euro_map) + geom_point(data = yelp_data,size=8,alpha=0.5,
                                      mapping = aes(
                                        x = longitude,
                                        y = latitude,
                                        color = factor(stars)
                                      ))+theme(legend.position = "none")

grid.arrange(usa1,euro2,nrow=1,ncol=2)

## @knitr RestaurantsPerStars

yelp_data %>% group_by(stars) %>% summarise(n=n()) %>% ggplot(aes(x=factor(stars),y=n,label=n,fill=factor(stars)))+geom_bar(stat = "identity")+geom_label(vjust=-.15)+theme(legend.position = "none",panel.background = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank())+labs(x="Star Rating of Restautrants",y="Total Restaurants",title="Number of resturants per rating")

## @knitr BestResaturantperCuisine

cityCusineStars <- select(filter(yelp_data,review.count>200),"American..New.","American..Traditional.","Asian.Fusion","Burgers","Chinese","Greek","Indian","Italian","Japanese","Korean","Mediterranean","Mexican","Middle.Eastern","Pakistani","Pizza","Sandwiches","Seafood","Sushi.Bars","Thai","Vegetarian","Vietnamese","stars","city","name","review.count") %>% gather(key="Cuisine",value="Value",-stars,-city,-name,-review.count) %>% filter(!is.na(Value))

cityCusineStars %>% group_by(Cuisine) %>% summarise(maxStars=max(stars)) %>% inner_join(cityCusineStars,by=c("maxStars"="stars","Cuisine"="Cuisine")) %>% group_by(Cuisine) %>% mutate(rank=rank(desc(review.count))) %>% filter(rank==1) %>% select("Cuisine","name","city","maxStars","review.count")%>% grid.table()

## @knitr wheredoesmycityeat

cityCusineStars %>% filter(city %in% c(  "Las Vegas","Phoenix","Toronto","Tempe","Pittsburgh","Cleveland","Scottsdale","Charlotte","Henderson")) %>% group_by(city) %>% summarise(totalRest=sum(review.count)) %>% inner_join(cityCusineStars) %>% group_by(city,Cuisine,totalRest) %>%  summarise(totalRestCui=sum(review.count)) %>%ungroup() %>% group_by(city) %>%  mutate(rank=rank(desc(totalRestCui),ties.method = "random"),rank=rank(desc(totalRestCui),ties.method = "random"),topRest=ifelse(rank==1,Cuisine,"")) %>% ggplot(aes(x=city,y=totalRestCui/totalRest,color=Cuisine,label=topRest))+geom_line(aes(group=Cuisine))+geom_text()+theme(legend.position = "none",panel.background = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank())+labs(x="City",y="Percentage of total Restaurants",title="Most Restaurants (More than 100 Restaurants in a city)")


## @knitr textAnalysisSmiley

extractSmileys <-
  str_match_all(tolower(resttipDF), "(?::|;|=)(?:\\)|\\(|D|P|\\|)")

t <- data.frame(extractSmileys[[1]])
colnames(t) <- c("smiley")

t <- t %>% group_by(smiley) %>% summarise(n = n())

ggplot(t, aes(smiley, n, label = smiley, fill = smiley)) +
  geom_bar(stat = "identity") +
  geom_text(aes(color = smiley), vjust = -.5) +
  scale_x_discrete(breaks = t$smiley, labels = t$smiley) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

## @knitr MostfamousWords

resttipDF$text <- gsub("[[:punct:]|0-9]", "", resttipDF$text)
resttipDF$id <- rownames(resttipDF)

unnestReviews <- resttipDF %>% unnest_tokens(word, text)

unnestReviewsT <- unnestReviews %>% anti_join(stop_words)%>%  count(word, sort = TRUE)


## @knitr BestWorstReview

bingLexicons <-
  sentiments %>% filter(lexicon == "bing") %>% select(-score)

reviewScore <-
  unnestReviews %>% inner_join(bingLexicons) %>% count(id, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment =
                                                                                                                      positive - negative)

overallReviewSentimentNoWord <-
  reviewScore %>% inner_join(resttipDF, by = "id")

overallReviewSentiment <-
  reviewScore %>% inner_join(resttipDF, by = "id") %>% inner_join(select(unnestReviews, id, word), by =
                                                                    "id") %>% arrange(desc(sentiment))

s <- overallReviewSentimentNoWord %>% arrange(desc(sentiment)) %>% top_n(3) %>% select(text) %>% slice(1:6)

n <- overallReviewSentimentNoWord %>% arrange(sentiment) %>% top_n(3) %>% select(text)%>% slice(1:6)

## @knitr BestandworstWords

unnestReviewsPost <-
  overallReviewSentiment[overallReviewSentiment$sentiment > 0, ] %>% count(word, sort = TRUE) %>% anti_join(stop_words)

pal = brewer.pal(9, "BrBG")
wordcloud(
  unnestReviewsPost$word[1:100],
  unnestReviewsPost$n[1:100],
  scale = c(5, .5),
  colors = pal
)


unnestReviewsNeg <-
  overallReviewSentiment[overallReviewSentiment$sentiment < 0, ] %>% count(word, sort = TRUE) %>% anti_join(stop_words)

pal = brewer.pal(9, "BrBG")
wordcloud(
  unnestReviewsNeg$word[1:100],
  unnestReviewsNeg$n[1:100],
  scale = c(5, .5),
  colors = pal
)


## @knitr topbottom5RestaurantBasedonAverageSentiment2016
overallReviewSentimentNoWord$month <- month(overallReviewSentimentNoWord$date)
overallReviewSentimentNoWord$day <- day(overallReviewSentimentNoWord$date)
overallReviewSentimentNoWord$year <- year(overallReviewSentimentNoWord$date) 

yearWiseAvergae <- overallReviewSentimentNoWord %>%filter(year<=2016) %>% group_by(business_id,year) %>% summarise(n=mean(sentiment),reviewCount=n()) %>% inner_join(select(yelp_data,business.id,name,stars,city),by=c("business_id"="business.id"))

businessin2010 <-  yearWiseAvergae %>% group_by(business_id) %>% filter(year==2010)

topRest <-   yearWiseAvergae  %>%  filter(year==2016 & business_id %in% businessin2010$business_id & reviewCount>10) %>% arrange(desc(n)) %>% top_n(5)

yearWiseAvergae %>% filter(business_id %in% topRest$business_id[1:5])%>% ggplot(aes(x=year,y=n)) + geom_line(aes(group=name,color=name)) + labs(y="Average Sentiment Score",title="Top 5 Restaurants with positive sentiments in 2016")+theme(panel.background = element_blank(),axis.ticks = element_blank())

bottomRest <-   yearWiseAvergae  %>%   filter(year==2016 & business_id %in% businessin2010$business_id & reviewCount>10) %>% arrange((n)) %>% top_n(5)

yearWiseAvergae %>% filter(business_id %in% bottomRest$business_id[1:5])%>% ggplot(aes(x=year,y=n)) + geom_line(aes(group=name,color=name))+ labs(y="Average Sentiment Score",title="Bottom 5 Restaurants with positive sentiments in 2016")+theme(panel.background = element_blank(),axis.ticks = element_blank())



## @knitr identifySpammer

yearlyAvgSentimentScorex <- overallReviewSentimentNoWord %>% group_by(business_id,year) %>% summarise(avgSentimentScore= mean(sentiment))

sentimentDevScore <- overallReviewSentimentNoWord %>% inner_join(yearlyAvgSentimentScorex) %>% mutate(sentimentDeviation=(sentiment-avgSentimentScore)**2)


finelDevation <- sentimentDevScore %>% group_by(user_id,business_id,year) %>% summarise(avgSentDev=sqrt(mean(sentimentDeviation)),n=n(),avgSentimentScore= mean(sentiment)) %>% filter(n>10)

colnames(finelDevation) <- c("User Id","Business Id","Year","Average Deviaton for Avg sentiment Score","Total Review","Avg Sentiment Score")


colnames(overallReviewSentimentNoWord)

finalReviewsSpammers <- finelDevation %>% ungroup() %>% inner_join(overallReviewSentimentNoWord %>% ungroup(), by = c("User Id"="user_id","Business Id"="business_id","Year"="year")) %>% select("User Id","Business Id","Year","text","sentiment")

crossJoinedData <- finalReviewsSpammers %>% group_by(`User Id`,`Business Id`,Year) %>% expand(text,text)

library(stringdist)

crossJoinedData <- crossJoinedData %>% mutate(lv =stringdist(text,text1,method="lv")) %>% group_by(`User Id`,`Business Id`,Year) %>% summarise(meanLV=mean(lv),sdLV=sd(lv))

overallReviewSentimentNoWord$text[overallReviewSentimentNoWord$user_id=='b67pD_AG3HnMLsoYqAy7ag' & overallReviewSentimentNoWord$business_id=='1CR2ddUcjYYwRd5JLtiRrw' & overallReviewSentimentNoWord$year=='2011']


overallReviewSentimentNoWord$text[overallReviewSentimentNoWord$user_id=='JAvxtY6z-ekLUqnNqAizXQ' & overallReviewSentimentNoWord$business_id=='KkmPDIWzvwbBpyqOHT6pcQ' & overallReviewSentimentNoWord$year=='2012']

### Are there customers who consistently give bad reviews


## Clustering
for (i in c(29:66,68:72,74:82,84:89,91:93,95)) {
  yelp_data[,i] = ifelse(is.na(yelp_data[,i]), 0,
                         ifelse(yelp_data[,i] == TRUE, 1,
                                ifelse(yelp_data[,i] == "True", 1, 0)))
  #yelp_data[,i] = ifelse(is.na(yelp_data[,i]), paste("Is Not", names(yelp_data)[i]),
  #                       ifelse(yelp_data[,i] == TRUE, paste("Is", names(yelp_data)[i]),
  #                              ifelse(yelp_data[,i] == "True", paste("Is", names(yelp_data)[i]), paste("Is Not", names(yelp_data)[i]))))
  yelp_data[,i] = as.factor(yelp_data[,i])
}

yelp_data$Alcohol[is.na(yelp_data$Alcohol)] = " none"
yelp_data$BYOBCorkage[is.na(yelp_data$BYOBCorkage)] = " no"
levels(yelp_data$NoiseLevel) = c(levels(yelp_data$NoiseLevel), "unknown")
yelp_data$NoiseLevel[is.na(yelp_data$NoiseLevel)] = "unknown"
yelp_data$Smoking[is.na(yelp_data$Smoking)] = " no"
yelp_data$WiFi[is.na(yelp_data$WiFi)] = " no"
yelp_data$total.num.tip.likes[is.na(yelp_data$total.num.tip.likes)] = 0
yelp_data$total.num.tips[is.na(yelp_data$total.num.tips)] = 0

#cluster_data = yelp_data[,c(7,11,29:32,34:36,38,40:47,49:56)]
#cluster_data$stars = as.factor(cluster_data$stars)
#stars_model = as.data.frame(model.matrix( ~ stars - 1, data = cluster_data))
#state_model = as.data.frame(model.matrix( ~ state - 1, data = cluster_data))
#stars_model = apply(stars_model, 2, as.factor)
#state_model = apply(state_model, 2, as.factor)
#cluster_data = cbind(state_model, cluster_data[,-c(1)])
# summary(cluster_data)
# #
# xquant <- cluster_data[,which(names(cluster_data) == "stars")] # Numeric variables
# xqual <- cluster_data[,which(names(cluster_data) != "stars")] 
# tree <- hclustvar(xquant, xqual)
# plot(tree)
# 
# k.means <- kmeansvar(matrix(xquant), xqual, init=6)
# summary(k.means)
# 
# cluster.model = daisy(cluster_data, metric = "gower")
# fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
# plot.new()
# plot(fit, hang=-1)
# groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
# rect.hclust(fit, k=6, border="red")

## Regression
library(radiant)
gdp = xlsx::read.xlsx("data/gdp_data.xlsx", 1)
names(gdp)[1] = "state"
population = read.csv("data/population.csv")
population$state = gsub("\\.", "", population$state)
gdp_population = inner_join(gdp, population, "state")
gdp_population = gdp_population[,-1]
names(gdp_population)[2] = "state"
reg_data = yelp_data[,c(7,11,29:32,34:36,38,40:47,49:56,90)]
reg_data = reg_data %>%
  inner_join(gdp_population, "state")
reg_data$state = as.factor(reg_data$state)
names(reg_data)[27] = "PriceRange"
reg_data$PriceRange = as.factor(as.character(reg_data$PriceRange))
ggplot(data=reg_data) + 
  geom_histogram(breaks=seq(1,5,by=0.5),aes(x=stars,y=..density..,fill = PriceRange), position="identity", alpha = 0.5)

ggplot(data = reg_data) + geom_histogram(aes(x=census), breaks = seq(2000000, 20000000, by = 2000000))
ggplot(data = reg_data)+ geom_histogram(aes(x=GDP), breaks = seq(35000, 65000, by = 5000))

reg_data$census_quantile = xtile(reg_data$census, 5, rev = FALSE)
reg_data$gdp_quantile = xtile(reg_data$GDP, 5, rev = FALSE)
reg_data = reg_data[,-c(28:29)]

train_ind = sample(nrow(reg_data)*0.8)
train = reg_data[train_ind,]
test = reg_data[-train_ind,]
## Linear Model
linear.model = lm(stars ~ . -state + gdp_quantile*PriceRange, data = train)
summary(linear.model)

linear.train.preds = predict(linear.model, train)
linear.train.MSE = mean(square(train$stars - linear.train.preds), na.rm = TRUE)
linear.test.preds = predict(linear.model, test)
linear.test.MSE = mean(square(test$stars - linear.test.preds), na.rm = TRUE)

coef.linear = tidy(linear.model)

set.seed(1234)

library(randomForest)
library(reprtree)
rf.model = randomForest(stars ~ . -state, data = train, na.action = na.omit)

rf.train.preds = predict(rf.model, train)
rf.train.MSE = mean(square(train$stars - rf.train.preds), na.rm = TRUE)
rf.test.preds = predict(rf.model, test)
rf.test.MSE = mean(square(test$stars - rf.test.preds), na.rm = TRUE)

importance = rf.model$importance

reprtree:::plot.getTree(rf.model)

#library(caret)
#numFolds <- trainControl(method = 'cv', number = 10)
#nn.model <- train(stars ~ . -state, data = train, method = 'nnet', na.action = na.omit, 
#              trControl = numFolds, tuneGrid=expand.grid(size=c(20:50), decay=c(0.01,0.05,0.1,0.15)))
#nn.model.final <- nnet(stars ~ . -state, data = train, size = 32, decay = 0.01)

#nn.train.preds = predict(nn.model.final, train)
#nn.test.preds = predict(nn.model.final, test)

RMSE = data.frame(model = c("Linear", "Linear", "Random Forest", "Random Forest"),
                  data = c("Train", "Test", "Train", "Test"),
                  RMSE = c(linear.train.MSE,linear.test.MSE,
                           rf.train.MSE,rf.test.MSE))

ggplot(data = yelp_data, aes(x = stars)) + geom_histogram(breaks = seq(1,5, by = 0.5)) +
  xlab("Star Rating") + ylab("Number of Restaurants")

ggplot(data = RMSE, aes(x = model, y = RMSE, fill = model)) +geom_bar(stat = "identity") + 
  facet_wrap(~data) + ggtitle("RMSE of Models predicting Star Rating") + 
  xlab("Model") + ylab("RMSE") 

