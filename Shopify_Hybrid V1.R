# Shopify_Hybrid V1
# Hybrid approach combining results of Item-Item Collaborative Filtering and Content Based results


library(tictoc)
library(dplyr)

# update directory before running
setwd("D:/NUS/ISS-EBAG/EBA5002 - Core Analytics Techniques/CA 2/Shopify Dataset")
# Import results of IBCF and Content Based
# need to amend the name of IBCF output file
result_IBCF_raw <- read.csv("CF_recommendations_top10.csv", header=TRUE, sep=",") 
names(result_IBCF_raw)[names(result_IBCF_raw) == "testuser"] <- "user_id"
names(result_IBCF_raw)[names(result_IBCF_raw) == "itemSim"] <- "similarity"
result_IBCF = result_IBCF_raw[1:4]
result_IBCF$method = "IBCF"
head(result_IBCF)

result_CBF <- read.csv("CBF_recommendations_top10.csv", header=TRUE, sep=",") 
result_CBF$method = "CBF"
head(result_CBF)

# length(unique(result_IBCF$user_id)) # 1337 using cutoff at 10 -> use change cutoff at 3, this should give same users as content based
# length(unique(result_CBF$user_id)) # 29630 using cutoff at 3 

# Extract list of unique users
userList = union(unique(result_IBCF$user_id),unique(result_CBF$user_id))
length(userList) # 29630 unique users


hybrid = data.frame()

tic("Find Hybrid")
for (target in 1:length(userList)) {
  
  Top10_IBCF = result_IBCF[result_IBCF$user_id == userList[target],]
  Top10_CBF = result_CBF[result_CBF$user_id == userList[target],]
  head(Top10_CBF)
  
  # create a dataframe with Top 3 results from each recommendation algorithm
 
  
  
  # add the row iteratively to the hybrid result, only if the app_id has not been picked yet (to avoid duplicated result)
  j_CBF = 1
  i_IBCF = 1
  for(i_IBCF in 1: max(nrow(Top10_IBCF),nrow(Top10_CBF))){
    if(i_IBCF == 1){
      hybrid = rbind(hybrid,Top10_IBCF[i_IBCF,])
    }else{
      if(!(Top10_IBCF[i_IBCF,] %in% hybrid$app_id)){
        hybrid = rbind(hybrid,Top10_IBCF[i_IBCF,])
      }
    }
    i_IBCF = i_IBCF + 1
    
    if(j_CBF==1) {
      hybrid = rbind(hybrid,Top10_CBF[j_CBF,])
    } else{
      if(!(Top10_CBF[j_CBF,] %in% hybrid$app_id)){
        hybrid = rbind(hybrid,Top10_CBF[j_CBF,])
      }
    }
    j_CBF = j_CBF + 1
    
    if(nrow(hybrid)>=6){
      break
    }
  }
}

toc()

# Add in app description and other details to the final list
appsList <- read.csv("apps.csv",header=TRUE, sep=",")
appsList = appsList[,c(1,3,4,7)]
head(appsList)
colnames(appsList) = c("app_id", "app_title","developer", "average_rating")

hybrid_result = merge(x = hybrid, y = appsList, by = 'app_id', all.x = TRUE)
names(hybrid_result)[names(hybrid_result) == 'X'] <- "SN"

names(result_IBCF_raw)[names(result_IBCF_raw) == "itemSim"] <- "similarity"


# Output the table
write.csv(hybrid_result,"hybrid_recommendation_top6.csv")



