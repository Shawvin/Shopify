library(shiny)
library(shinyjs)
library(tidyverse)

dat<-read.csv("apps.csv", encoding="UTF-8")

dat2<-dat[,c(3,6,7,11)] %>% unique() 
dat2$titles<-paste(dat2$title, "\t(",dat2$rating, ")", sep="")
write.csv(dat2, "app_icon.csv", row.names = FALSE)
