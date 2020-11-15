library(shiny)
library(shinyjs)
library(tidyverse)

dat<-read.csv("apps.csv", encoding="UTF-8")

dat2<-dat[,c(3,6,7,11)] %>% unique() 
dat2$titles<-paste(dat2$title, "\t(",dat2$rating, ")", sep="")
write.csv(dat2, "app_icon.csv", row.names = FALSE)

# newuser_recom<-read.csv("recom_newuser.csv", stringsAsFactors = FALSE)
# newuser_recom_list<-split(newuser_recom$app_title2, ceiling((1:23740)/5))
# names(newuser_recom_list)<-unique(newuser_recom$app_title1)

#save(newuser_recom_list, file="recom_newuser.Rdata")