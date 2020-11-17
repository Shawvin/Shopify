library(tidyverse)

##The file used to generate the R object to run shiny app
##Only need to run once

#***********************
#1. The main reference for display
#***********************

##read the apps csv
dat<-read.csv("apps.csv", encoding="UTF-8")

##obtain the columns we need
dat2<-dat[,c(1,3,6,7,8,11)] %>% unique()

##paste the title with ratings
dat2$titles<-paste(dat2$title, "\t(",dat2$rating, ")", sep="")
app_icons<-dat2
#write.csv(dat2, "app_icon.csv", row.names = FALSE)
#save(app_icons, file="app_icons.Rdata")


#***********************
#2. new user recommendation
#***********************

##read the newuser recom csv
newuser_recom<-read.csv("recom_newuser.csv", stringsAsFactors = FALSE)

##convert to list of vector
newuser_recom_list<-split(newuser_recom$app_title2, ceiling((1:23740)/5))

##each element of list is 5 apps recommendation based on the app new user choose
names(newuser_recom_list)<-unique(newuser_recom$app_title1)

#save(newuser_recom_list, file="recom_newuser.Rdata")


#***********************
#3. active user list
#***********************
#read the csv file
top32<-read.csv("top32.csv", stringsAsFactors = FALSE)

#generate active user list
activeuser_list<-unique(top32$user_id)

#save(activeuser_list, file="activeuser_list.Rdata")


#***********************
#4. generate active user browsing history
#***********************
events<-read.csv("reviews.csv", encoding="UTF-8", stringsAsFactors = FALSE)

ucnts = aggregate(app_id ~ author, data = events, FUN=length)
colnames(ucnts) = c("author","review_count") 
activeusers = ucnts$author[ucnts$review_count >= 3]
length(activeusers) # 29,631 active reviewers with >= 3 reviews
active_events = events[events$author %in% activeusers,]


##average the rating of same user on same item
Mean_review<-active_events %>% group_by(author, app_id) %>% summarize(rating=mean(rating))

##15 history app for each user
history_table<-Mean_review %>% group_by(author) %>% top_n(n=15, wt=app_id)

##remove empty author
history_table<-history_table[-c(1:15),]

#save(history_table, file="history_table.Rdata")


#***********************
#5. generate top32 recommendation for each user
#***********************
##read content_based filtering top100 recommendation for each user
cbf_top100<-read.csv("CBF_recommendations_top100.csv", stringsAsFactors = FALSE)

#the number of unique users
length(unique(cbf_top100$user_id))
#29630

##read item-based collaborative filtering top10 recommendation for each user
cf<-read.csv("CF_recommendations_top10.csv")

#the number of unique users
length(unique(cf$user_id))
#29599

##the intersect of user_id
common_user<-intersect(unique(cbf_top100$user_id),unique(cf$user_id))
##number of users in both recommendation
length(unique(common_user))
#29599

##clean cbf file
cbf<-cbf_top100[cbf_top100$user_id %in% common_user,]
cbf$approch<-rep("cbf", nrow(cbf))

cbf<-cbf[, c(2,3,4,5)]
cbf<-cbf %>% group_by(user_id) %>% top_n(n=32, wt=similarity) %>% arrange(user_id, similarity)

##clean cf file
cf$approach<-rep("ibcf", nrow(cf))
cf<-cf[,c(2,3,4,8)]

colnames(cbf)<-colnames(cf)
combined<-rbind(data.frame(cbf), cf) 

combined<- combined %>% arrange(user_id, desc(approach), -similarity)
test<-combined[!duplicated(combined[1:2]),]

test2<-test %>% group_by(user_id) %>% top_n(32, wt=similarity)

top32<-test2[,c(1,2)]

#save(top32, file="top32.Rdata")
