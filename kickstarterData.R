#data cleaning, EDA, data visualization
#data is from kickstarter projects dataset on Kaggle by Mickaël Mouillé

library(tidyverse)
library(dplyr)
library(shiny)
library(ggthemes)
library(tree)
library(pROC)

data <- read_csv("ks-projects-201801.csv")

modData <- data

table(data$state)
class(data)
str(data)
summary(data)
head(data)

#add a col for differences between pledge/goal
modData <- mutate(modData, residual = modData$usd_pledged_real-modData$usd_goal_real)

#checklist - general analysis
#1 - check pledged for USD = USD_real, check goal for USD = USD_real
#2 - check state when backers is 0
#3 - find a state for the undefined using residuals
#4 - successful ones have positive residuals, failed ones have negative residuals

table(modData$state)
#1 - the same
filter(modData, pledged != usd_pledged_real & currency == "USD")
filter(modData, goal != usd_goal_real & currency == "USD")

#2 - backers is 0, but successful - should not be used in dataset
modData <- filter(modData, backers != 0 | state != "successful")

#3 - use residuals to replace undefined states
undefined <- filter(modData, state == "undefined")
others <- filter(modData, state != "undefined")

for (i in 1:nrow(undefined)) {
  if (undefined[i,]$residual > 0) {
    undefined[i,]$state <- "successful"}
  if (undefined[i,]$residual <= 0) {
    undefined[i,]$state <- "failed"}
}

modData <- rbind(undefined, others)

#4 - look further into the individual kickstarters
filter(modData, residual > 0, state == "failed") #should be successful
filter(modData, residual < 0, state == "successful") #these are correct

#new dataframes for failed and successful kickstarters
failed <- filter(modData, state =="failed")
successful <- filter(modData, state == "successful")

for (i in 1:nrow(failed)) {
  if (failed[i,]$state == "failed" & failed[i,]$residual > 0)
    failed[i,]$state <- "successful"
}

modData <- rbind(failed, successful)

#look at NA values - we don't need usd pledged
sapply(modData, function(x) sum(is.na(x)))
modData <- modData[,-13]

finalData <- modData

#end of wrangling - this is our final dataset used - look at the 3 name NA's if possible

#bar graph of state by num/by proportion
ggplot(finalData, aes(state)) +
  geom_bar()

ggplot(finalData, aes(state, ..prop.., group = 1)) +
  geom_bar()

#successful projects
successful <- filter(finalData, state == "successful")
ggplot(successful, aes(main_category)) +
  geom_bar()

ggplot(successful)+
  geom_smooth(aes(launched, residual))

#which type of proj are the most popular?
categoryfreq <- finalData %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

categoryfreq$main_category <- factor(categoryfreq$main_category, levels=categoryfreq$main_category)

ggplot(categoryfreq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Project Popularity by Category") + xlab("Category") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.3) + theme_economist() +
  theme(plot.title=element_text(hjust=0.3), axis.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=8, angle=90), legend.position="null") + 
  scale_fill_gradient(low="white", high="black")

#same for subcategory
subcat.freq <- finalData %>%
  group_by(category) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

subcat.freq$category <- factor(subcat.freq$category, levels=subcat.freq$category)

ggplot(head(subcat.freq,10), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Project Popularity by Subcategory") + xlab("Subcategory") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.3) + theme_economist() +
  theme(plot.title=element_text(hjust=0.3), axis.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=8, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")


#amount pledged by category

#Predictive Analytics
finalData$contrib <- ifelse(finalData$backers > 0, (finalData$pledged / finalData$backers), 0)

#reach ratio is the percentage of completion of the project
finalData$reach_ratio <- ifelse( finalData$contrib != 0, ((finalData$contrib / finalData$goal)*100), 0)
finalData$launch_year <- substr(finalData$launched, 1,4)

hist(finalData$reach_ratio, main = "Histogram of average reach_ratio")

finalData$status = ifelse(finalData$state == 'failed', 0, 1)

## 70% of the sample size
smp_size <- floor(0.7 * nrow(finalData))

## set the seed to make partition reproductible
set.seed(1024)
train_ind <- sample(seq_len(nrow(finalData)), size = smp_size)

train <- finalData[train_ind, ]
test <- finalData[-train_ind, ]


tree1 <- tree(status ~ goal + reach_ratio + category + backers + country + launch_year , data = train)

summary(tree1)

#decision tree rules
plot(tree1)
text(tree1 ,pretty =0)

#applying to test data
Pred <- predict(tree1, test)
validf <- data.frame( kickstarter_id = test$ID, orig_status = test$status, new_status = Pred)
validf$new = ifelse(validf$new_status < 0.5, 0, 1)

table(validf$orig_status, validf$new)


auc(validf$orig_status, validf$new)
#area under the curve is 0.971
#most important factors are backers and reach_ratio