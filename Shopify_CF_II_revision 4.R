
library(tictoc)
library(dplyr)

# please update directory before running the code
# setwd("D:/NUS/ISS-EBAG/EBA5002 - Core Analytics Techniques/CA 2/Shopify Dataset")
#setwd("C:/Users/sophi/Desktop/NUS Master/S2-01 CA2/shopify")
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
activeusers = ucnts$author[ucnts$numitems >= 3 & ucnts$author!=""] ; length(activeusers) ; length(activeusers)/nu#29631

# active users 
# > 20, count = 172
# > 10, count = 1338
# > 5,  count = 8547
# > 4, count = 14903
# > 3, count = 29631
# > 2, count = 74582
ev = events[events$author %in% activeusers,]

dim(ev); nrow(ev)/ nrow(raw_events)
# users count
# > 10, 19418, 3
# > 5, 63073     3, 0.1410029
# > 4, 88497     3
# > 3, 132681, 3, 29.66152%
# > 2, 222583, 3, 49.7%

head(ev)

# eliminate apps with too few ratings
bcnts = aggregate(author ~ app_id, data = events, FUN=length)
colnames(bcnts) = c("app_id","numusers") 
popularapps = bcnts$app_id[bcnts$numusers >= 1] ; 
length(popularapps); 
length(popularapps)/nb #3237

# apps count
# >= 15, 1670, 0.4473614
# >= 10, 1957, 0.5242432
# >= 5, 2513, 0.6731851
# >= 3, 3237, 0.867131
# >= 2, 3237, 0.867131
# <= 1, 3733, 1

# events["author"][events["author"]==""]
head(events)

ev = ev[ev$app_id %in% popularapps,]

dim(ev) 
# 18890     3
# 222294      3
# 132681 3
# 88497     3
# free up data
# rm(ucnts)
# rm(bcnts)
# rm(events)

#  create the ratings matrix from the raw events
tic("Rating Matrix Creation Runtime")
users = acast(ev, author ~ app_id, value.var = "rating", fun.aggregate = mean) # we use an aggregation function since some users have rated the same book more than once
# users = sweep(users, 1, rowMeans(users, na.rm=TRUE))  # normalise the data
toc()
dim(users) #
# (users, apps)
# colnames(users)
head(users)

# compute item-item similarity matrix (to prepare for Item-Item CF)
# there are many simiarity measures to pick from
# tic("Rating Matrix Creation Runtime")
# itemsims   = cor(users, use="pairwise.complete.obs") #; itemsims  # pearson
# toc() #Rating Matrix Creation Runtime: 35.14 sec elapsed
# range: [-1,1]

# same as above
# tic("Rating Matrix Creation Runtime")
# itemsimsP  = getitemsimsmatrix(users, simfun=pearsonsim) #; itemsimsP  # same as above
# toc()
# range: [-1,1]

# tic("Rating Matrix Creation Runtime")
# itemsimsC  = getitemsimsmatrix(users, simfun=cosinesim) #; itemsimsC
# toc()
# # range: [-1,1]
# 
# tic("Rating Matrix Creation Runtime")
# itemsimsE  = getitemsimsmatrix(users, simfun=euclidsim) #; itemsimsE
# toc()
# range: [0,1]

# we can also try pre-normalising the data and then using cosine similarity
meanusers = rowMeans(users, na.rm=TRUE)
normalizedusers = sweep(users, 1, meanusers)   # subtract user (row) means

tic("Rating Matrix Creation Runtime")
itemsimsNC = getitemsimsmatrix(normalizedusers , simfun=cosinesim)
toc()
itemsimsNC
# range: [-1,1]
# Rating Matrix Creation Runtime: 7681.92 sec elapsed

# tic("Rating Matrix Creation Runtime")
# itemsimsNE  = getitemsimsmatrix(normalizedusers, simfun=euclidsim) #; itemsimsE
# toc()
# range: [0,1]

# fillrate(itemsims) #0.2236306 %
# fillrate(itemsimsP) #4.863462 %
# fillrate(itemsimsC) #4.863462 %
# fillrate(itemsimsE) #4.863462 %
fillrate(itemsimsNC) #3.700213 % #2.189137
# fillrate(itemsimsNE) #4.863462 %

# setup the train/test split ready for testing
dim(users) #1338 1946 #29630  3074
numtestusers = 30 # 100
testnames  = sample(rownames(users), min(numtestusers,nrow(users))) # identify N users randomly for testing
trainnames = setdiff(rownames(users),testnames) # take remaining users for training
trainusers = users[trainnames,]
testusers  = users[testnames,]

# dim(users)
# users[1:2,1:2]

#normalize the users
trainusers = sweep(trainusers, 1, rowMeans(trainusers, na.rm=TRUE) )
testusers = sweep(testusers, 1, rowMeans(testusers, na.rm=TRUE) )

# testusers[1:3,1:3]

#test IBCF
preds = predictCF(testusers, itemsims=itemsimsNC, numtestitems=40, random=FALSE) ; preds
cat("avg MAE =",avgMAE(preds), "from", validcnt(listpreds(preds)), "tests")
#itemsims: avg MAE = 0.3373857 from 353 tests
#itemsimsP:
#itemsimsC: avg MAE = 0.4354971 from 579 tests
#itemsimsE: avg MAE = 0.2698382 from 579 tests
#itemsimsNC: avg MAE = 0.2202145 from 566 tests
#itemsimsNE: avg MAE = 0.2698382 from 579 tests

# full matrix: avg MAE = 0.3212429 from 100 tests

likethresh = 0 # 
showCM(preds, like=likethresh)
# TN=   28 FP=   17
# FN=    4 TP=  381  (total=430)
# accuracy  = 95.1%
# precision = 95.7%
# recall    = 99.0%

# full matrix
# TN=   10 FP=    0
# FN=    6 TP=   84  (total=100)
# accuracy  = 94.0%
# precision = 100.0%
# recall    = 93.3%


# get recommendation matrix: default is top 5 results

testUserName = sample(testnames,size = 20) 
length(unique(rownames(users))) #29630
username = unique(rownames(users))
# head(username)
# length(username)
                  # itemsimsNC[1:10,1:10]
# target[,c('837308dd-5304-4b82-b319-290b251f282c',
#          '61ef1097-7150-409d-97ca-fbe178d5b80f',
#          '0c29ae5e-01e0-4efb-959f-40d9041db6fa')]

appsList <- read.csv("apps.csv",header=TRUE, sep=",")
appsList = appsList[,c(1,3,4,7,8)]
colnames(appsList) = c("app_id", "app_title","developer", "average_rating", "reviews_count")

tic("prediction Runtime")
table = data.frame()
file.remove("result.csv")
count=0
for(name in username){
  count = count+1
  if(count%%100==0) 
    cat(paste0("count:",count))
  # name="Elevated Vaping"
  # name = "HURLAXE"
  target=users[name,]
  reconItems = getrecommendations_II(target, itemsimsNC, topN=100)
  reconItems = as.data.frame(reconItems)
  reconAppName = rownames(reconItems)
  reconItemsTable = data.frame(reconAppName,reconItems$reconItems)
  colnames(reconItemsTable) = c( "app_id","itemSim")
  reconItemsTable
  # left join to get the app name and developer details
  testuser = list(testuser =name)
  # head(appsList)
  reconItemsTable = merge(x = reconItemsTable, y = appsList, by = 'app_id', all.x = TRUE)
  reconItemsTable = merge(x = reconItemsTable, y = testuser, all = TRUE)
  # reconItemsTable = reconItemsTable[reconItemsTable$itemSim<5]

  reconItemsTable = reconItemsTable %>%
    filter(average_rating>=3, reviews_count>=10) %>%
    arrange(desc(itemSim),desc(reviews_count),desc(average_rating)) %>%
    slice_head(n=10)
    # cat(paste0("Test User: ", name))
  table = rbind(table, reconItemsTable)
  # print(table)
  # append(reconItemsTable)
  if (count==length(username)) 
    write.csv(table[c("testuser", "app_id","itemSim", "app_title","average_rating","reviews_count")],"result.csv")
}
toc()

# Test User: Fazbima                                  app_id itemSim                          title            developer average_rating reviews_count
# 254 da2ffea8-801c-46e5-aba6-03568e5aec86       5   Facebook & Twitter Auto Post           Socialhead              5           368
# 104 522a5be2-ce31-474d-b936-e312007a07cb       5 Store Pickup Click and Collect            CreativeR              5           216
# 295 fb9503ad-cc0c-4228-8421-29c0c75ebf2b       5                 Invoice Falcon        622 Solutions              5           210
# 257 dd74088c-c6ee-40ba-889e-7ef0eb62eecf       5        Simplio: Simple Invoice              Simplio              5           151
# 136 717bbc13-7ed5-4c0c-b8ab-5cc5714b2f4b       5 Messenger Chat+ Abandoned Cart            BiteSpeed              5           148
# 76  42cef819-bfa5-4618-a0d7-5da6c64c6fae       5    Ultimate Sticky Add to Cart      Conversion Bear              5           125
# 176 93d20e57-6234-4964-81e4-2493210a165d       5              Postcode Shipping             Addition              5            96
# 212 ade95e63-867f-482e-b953-9636dfaa1bea       5                   Easy Invoice           Softify OU              5            76
# 169 8cabaf0b-d110-4bbb-987c-91fbeb6f968c       5                      Sky Pilot Corknine Development              5            75
# 103 51d2d3c0-ea11-4130-bf00-7d3074ea97f1       5                     Recently 3             Appifiny              5            72

# check for Test User: Fazbima
# itemsimsNC[c('0ec41a8a-a698-4b59-942d-1c4f4fbd1d66',
#              '1173b33a-301b-4051-acdc-b6b02d6cdbd6',
#              'a789fb17-fbdb-4c7d-a613-4e82badc821d',
#              '51bf2ba0-4e32-402f-9157-3c4d4013376a',
#              'c98ff6fc-7f5f-441f-b4ac-6271f923ad3f',
#              '59eda690-e12c-4223-bfeb-ded702d258f9',
#              '03246394-bf13-4bbd-b220-8f1c77d15416',
#              '11a394a0-1210-479c-a17d-5407e76feac0',
#              'e38aac12-0ad1-4145-ac0e-77e291f8c5fa',
#              '8d28ced2-f6b9-4884-8f77-37b2f2253d94',
#              '0c29ae5e-01e0-4efb-959f-40d9041db6fa',
#              'f9e45fcc-4d6a-4dbe-8e3f-7f7c22398ee1',
#              '1a4accf8-b57e-4c08-a07c-fe5db0f0a0bc'),
#           c('da2ffea8-801c-46e5-aba6-03568e5aec86',
#             '522a5be2-ce31-474d-b936-e312007a07cb',
#             'fb9503ad-cc0c-4228-8421-29c0c75ebf2b',
#             'dd74088c-c6ee-40ba-889e-7ef0eb62eecf',
#             '717bbc13-7ed5-4c0c-b8ab-5cc5714b2f4b',
#             '42cef819-bfa5-4618-a0d7-5da6c64c6fae',
#             '93d20e57-6234-4964-81e4-2493210a165d',
#             'ade95e63-867f-482e-b953-9636dfaa1bea',
#             '8cabaf0b-d110-4bbb-987c-91fbeb6f968c',
#             '51d2d3c0-ea11-4130-bf00-7d3074ea97f1')
# ]

