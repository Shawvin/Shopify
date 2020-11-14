
library(tictoc)

# please update directory before running the code
#setwd("D:/NUS/ISS-EBAG/EBA5002 - Core Analytics Techniques/CA 2/Shopify Dataset")
raw_events <- read.csv("reviews.csv", header=TRUE, sep=",") # transaction format!
raw_events[1,]
names(raw_events)
events = raw_events[c('author','app_id','rating')]
# names(events) = c("author", "app_i", "rating")
length(unique(events$app_id)) # show #books  
#3733
length(unique(events$author)) # show #users 
#299,316

# extract only the explicit ratings
events = events[events$rating > 0,]
nb = length(unique(events$app_id)); nb # 3733, 
nu = length(unique(events$author)); nu # 299316

# eliminate users with too few ratings
ucnts = aggregate(app_id ~ author, data = events, FUN=length)
head(ucnts)
colnames(ucnts) = c("author","numitems") 
activeusers = ucnts$author[ucnts$numitems >= 2] ; length(activeusers) ; length(activeusers)/nu#1338

# active users 
# > 20, count = 172
# > 10, count = 1338
# > 5,  count = 8547
# > 2, count = 74582
ev = events[events$author %in% activeusers,]

dim(ev); nrow(ev)/ nrow(raw_events)
# users count
# > 10, 19418, 3
# > 2, 222583, 3, 49.7%

head(ev)

# eliminate apps with too few ratings
bcnts = aggregate(author ~ app_id, data = events, FUN=length)
colnames(bcnts) = c("app_id","numusers") 
popularapps = bcnts$app_id[bcnts$numusers >= 2] ; length(popularapps); length(popularapps)/nb #1957

# apps count
# > 15, 1670, 0.4473614
# > 10, 1957, 0.5242432
# > 5, 2513, 0.6731851
# > 2, 3237, 0.867131

ev = ev[ev$app_id %in% popularapps,]

dim(ev) 
# 18890     3
# 222294      3

# free up data
rm(ucnts)
rm(bcnts)
rm(events)

#  create the ratings matrix from the raw events
tic("Rating Matrix Creation Runtime")
users = acast(ev, author ~ app_id, value.var = "rating", fun.aggregate = mean) # we use an aggregation function since some users have rated the same book more than once
users = sweep(users, 1, rowMeans(users, na.rm=TRUE))  # normalise the data
toc()
dim(users)
#1338 1549 (users, apps)
# colnames(users)
head(users)


# compute item-item similarity matrix (to prepare for Item-Item CF)
# there are many simiarity measures to pick from
itemsims   = cor(users, use="pairwise.complete.obs") #; itemsims  # pearson
itemsimsP  = getitemsimsmatrix(users, simfun=pearsonsim) #; itemsimsP  # same as above
itemsimsC  = getitemsimsmatrix(users, simfun=cosinesim) #; itemsimsC
itemsimsE  = getitemsimsmatrix(users, simfun=euclidsim) #; itemsimsE

fillrate(itemsims) #1.790155
fillrate(itemsimsP) #1.740351
fillrate(itemsimsC) #5.148452
fillrate(itemsimsE) #6.843125

# setup the train/test scheme
numtestusers = 40 
test  = sample(rownames(users), min(numtestusers,nrow(users)))
train = setdiff(rownames(users),test)

#test IBCF (try different similarity metrics)
preds = predictCF(users[test,], itemsims=itemsimsC, numtestitems=10, random=FALSE) ; preds
cat("avg MAE =",avgMAE(preds), "from", validcnt(listpreds(preds)), "tests")
#itemsims: avg MAE = 0.2594605 from 368 tests
#itemsimsP: avg MAE = 0.2594605 from 368 tests
#itemsimsC: avg MAE = 0.2388916 from 384 tests
#itemsimsE: avg MAE = 0.3561233 from 393 tests

# viewing a histogram of the ratings helps gauge the impact of different MAE's and decide what the likethresh should be
hist(unlist(users[train,])) 
#Error in users[train, ] : subscript out of bounds
mean(users,na.rm=TRUE) #-1.980623e-17
likethresh = 0 # 
showCM(preds,likethresh)

# Setting up test cases to visually validate the recommendations
# get recommendation matrix: default is top 5 results

testUserName = sample(test,size = 5)

for(name in testUserName){
  
  target=users[name,]
  reconItems = getrecommendations_II(target, itemsimsC)
  reconItems = as.data.frame(reconItems)
  reconAppName = rownames(reconItems)
  reconItemsTable = data.frame(reconAppName,reconItems$reconItems)
  colnames(reconItemsTable) = c( "app_id","itemSim")
  reconItemsTable
  # left join to get the app name and developer details
  appsList <- read.csv("apps.csv",header=TRUE, sep=",")
  appsList = appsList[,c(1,3,4,7,8)]
  colnames(appsList) = c("app_id", "title","developer", "rating", "reviews_count")
  # head(appsList)
  reconItemsTable = merge(x = reconItemsTable, y = appsList, by = 'app_id', all.x = TRUE)
  reconItemsTable <- reconItemsTable[order(reconItemsTable$itemSim,decreasing = TRUE),]
  sprintf("Test User: %s", name)
  print(reconItemsTable)
}

