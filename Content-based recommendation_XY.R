library(tm)
library(tidyverse)

setwd("C:/Users/Wang Xiaoyuan/Desktop/Shopify")

#convert the app id and app category to wide format
apps_cat<-read.csv("apps_categories.csv")
cat<-read.csv("categories.csv")

app_cat_comb<-full_join(apps_cat, cat, by=c("category_id"="id"))
app_cat_comb$value<-1

app_cat_wide<-app_cat_comb[,c(1,3,4)] %>% spread(title, value, fill=0)

head(app_cat_wide)

app_cat_wide2<-left_join(app_cat_wide, apps[,c(1,3)], "app_id"="app_id")


##price plan
price_plan<-read.csv("pricing_plans.csv")
head(price_plan)

length(unique(price_plan$id))
length(unique(price_plan$app_id))
length(unique(price_plan$price))

price_plan$value<-1

price_plan_wide<-price_plan[,c(2,4,5)] %>% distinct() %>% spread(price, value, fill=0)
price_plan_wide2<-left_join(price_plan_wide, apps[,c(1,3)], "app_id"="app_id")



##price plan features
price_plan_feature<-read.csv("pricing_plan_features.csv")
head(price_plan_feature)
length(unique(price_plan_feature$app_id))
length(unique(price_plan_feature$pricing_plan_id))


##convert dtm-ti to dense matrix
dtm_ti_matrix<-Matrix(dtm_ti, sparse = TRUE)
remove(dtm_ti_matrix)
dtm_ti_desematrix<-as.matrix(dtm_ti)
dim(dtm_ti_desematrix)

combined<-cbind(app_cat_wide, dtm_ti)

app_cat_wide_sparse<-as(app_cat_wide, "sparseMatrix")


