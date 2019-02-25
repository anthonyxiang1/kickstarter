#data cleaning, EDA, data visualization
#data is from kickstarter projects dataset on Kaggle by Mickaël Mouillé

library(tidyverse)
library(dplyr)
library(shiny)
library(ggthemes)
library(tree)
library(pROC)
library(gplots)
library(rworldmap)

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

pledged.total <- finalData %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged_real)) %>%
  arrange(desc(total))

pledged.total$main_category <- factor(pledged.total$main_category, levels=pledged.total$main_category)

ggplot(pledged.total, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)") + 
  geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue2", high="royalblue2")

#backer contribution
pledged.avg <- finalData %>%
  group_by(main_category) %>%
  summarize(pledged=sum(usd_pledged_real), backers=sum(backers)) %>%
  mutate(avg=pledged/backers) %>%
  arrange(desc(avg))

pledged.avg$main_category <- factor(pledged.avg$main_category, levels=pledged.avg$main_category)

ggplot(pledged.avg, 
       aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Amount Pledged per Backer") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,2))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")
#technology has the most amount per backer

#average project goal
goal.avg <- finalData %>%
  group_by(main_category) %>%
  summarize(goal=sum(usd_goal_real), projects=n()) %>%
  mutate(avg=goal/projects) %>%
  arrange(desc(avg))

goal.avg$main_category <- factor(goal.avg$main_category, levels=goal.avg$main_category)

ggplot(goal.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Project Goal") + xlab("Project Category") + ylab("Project Goal (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,0))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")
#technology has the highest 

#boxplots as well
ggplot(finalData, aes(main_category, usd_goal_real, fill=main_category)) + geom_boxplot() + 
  ggtitle("Project Goal vs. Project Category") + xlab("Project Category") + 
  ylab("Project Goal (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,60000))

#distribution of usd pledged and usd goal
usd.amounts <- gather(finalData, type, amount, usd_pledged_real, usd_goal_real, factor_key=T)

ggplot(usd.amounts, aes(log(amount+1), fill=type)) + 
  geom_histogram(alpha=0.5, position="identity") + 
  ggtitle("Distribution of log(USD Pledged) vs. log(USD Goal)") + xlab("log(USD + 1)") + 
  ylab("Frequency") + scale_fill_discrete("Type", labels=c("USD Pledged", "USD Goal"))
#usd goal looks relatively normally distributed
#usd pledged bimodal - one very left and the other around 8ish log(USD+1)

#Success vs failure by project category
state.pct <- finalData %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)

state.pct$main_category <- factor(state.pct$main_category, 
                                  levels=state.pct$main_category[1:(nrow(state.pct)/2)])

ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()

#per year
projYear <- finalData %>%
  filter(launch_year != "1970") %>%
  group_by(launch_year) %>%
  summarize(count = n())

ggplot(projYear, aes(launch_year, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects per Year") + xlab("Year") + 
  ylab("# of Projects") + scale_x_discrete(limits=c(2009:2018)) + 
  geom_text(aes(label=paste0(count)), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")
#x labels not present, but from 2009 - 2018

#success and failure by year launched
state.pct2 <- finalData %>%
  filter(launch_year!="1970", state %in% c("successful", "failed")) %>%
  group_by(year=launch_year, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state))

ggplot(state.pct2, aes(year, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Year Launched") + 
  xlab("Year") + ylab("Percentage") + scale_x_discrete(limits=c(2009:2017)) + 
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))
#from 2009 - 2018

#heatmap of year by category
cat.year <- finalData %>%
  filter(!launch_year %in% c("1970", "2018")) %>%
  group_by(main_category, year=launch_year) %>%
  summarize(count=n())

cat.year2 <- t(matrix(cat.year$count, nrow=9))
colnames(cat.year2) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
rownames(cat.year2) <- c("Food", "Fashion", "Art","Publishing", "Games",
                          "Design","Technology","Theater","Photography",
                         "Comics","Crafts","Journalism","Dance","Film & Video","Music")

heatmap.2(cat.year2, dendrogram="row", Colv=F, trace="none", margins=c(10,10))

#by country
countries.freq <- finalData %>%
  filter(country!='N,0"') %>%
  group_by(country) %>%
  summarize(count=n())

countries.match <- joinCountryData2Map(countries.freq, joinCode="ISO2", nameJoinColumn="country")

mapCountryData(countries.match, nameColumnToPlot="count", 
               mapTitle="Number of Projects by Country", catMethod="logFixedWidth", 
               colourPalette="heat")

#Predictive Analytics
finalData$contrib <- ifelse(finalData$backers > 0, (finalData$pledged / finalData$backers), 0)

#completion ratio is the percentage of completion of the project
finalData$comp_ratio <- ifelse( finalData$contrib != 0, ((finalData$contrib / finalData$goal)*100), 0)
finalData$launch_year <- substr(finalData$launched, 1,4)

hist(finalData$comp_ratio, main = "Histogram of average comp_ratio")

finalData$status = ifelse(finalData$state == 'failed', 0, 1)

## 70% of the sample size
smp_size <- floor(0.7 * nrow(finalData))

## set the seed to make partition reproductible
set.seed(1024)
trainIndex <- sample(seq_len(nrow(finalData)), size = smp_size)

train <- finalData[trainIndex, ]
test <- finalData[-trainIndex, ]


tree1 <- tree(status ~ goal + comp_ratio + category + backers + country + launch_year , data = train)

summary(tree1)

#decision tree rules
plot(tree1)
text(tree1 ,pretty =0)

#applying to test data
prediction <- predict(tree1, test)
check <- data.frame( kickstarter_id = test$ID, orig_status = test$status, new_status = prediction)
check$new = ifelse(check$new_status < 0.5, 0, 1)

table(check$orig_status, check$new)


auc(check$orig_status, check$new)
#area under the curve is 0.971
#most important factors are backers and comp_ratio