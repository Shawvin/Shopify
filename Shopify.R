setwd("C:/Users/Wang Xiaoyuan/Desktop/Shopify")

dir()
library(tidyverse)

apps<-read.csv("apps.csv")
## 4750 apps with 12 columns

colnames(apps)

print(object.size(apps),units="Mb")

apps_categories<-read.csv("apps_categories.csv")
length(unique(apps_categories$app_id))
##7376 rows with 4750 unique app_ids
##some apps belong to more than one categories

categories<-read.csv("categories.csv")
##12 categories

key_benefits<-read.csv("key_benefits.csv")
length(unique(key_benefits$app_id))
length(unique(key_benefits$title))
##12927 rows with 4309 unique app_ids
##some apps are not shown
##some apps have more benefits

price_plan_features<-read.csv("pricing_plan_features.csv")
length(unique(price_plan_features$app_id))
length(unique(price_plan_features$pricing_plan_id))
length(unique(price_plan_features$feature))
##23990 rows with 3 columns

##6087 unique apps id, more than 4750 in apps.csv dataset
##2401 unique pricing_plan_id
##The app id and price plan id are mixded up in this dataset

##15746 unique feature

price_plan<-read.csv("pricing_plans.csv")
length(unique(price_plan$app_id))
length(unique(price_plan$id))
##4750 app id
##8514 price id

reviews<-read.csv("reviews.csv")
print(object.size(reviews),units="Mb")
colnames(reviews)
##447317 rows with 8 column
length(unique(reviews$app_id))
##only 3733 apps have reviews

length(unique(reviews$author))
##299316 user have rated 

dup_index<-which(duplicated(reviews[,1:3]))
df<-reviews[-dup_index,1:3] %>% spread(author, rating) 
dim(df)
head(reviews[,1:3])
