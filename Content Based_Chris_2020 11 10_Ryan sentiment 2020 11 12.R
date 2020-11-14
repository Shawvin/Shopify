
# Install Packages
install.packages("tm")
install.packages("SnowballC")
install.packages("slam")
install.packages("dplyr")
install.packages("Matrix")
library(tm)
library(stringr)
library(SnowballC)
library(slam)
library(dplyr)
library(Matrix)
library(tidyr)

# set working directory
#setwd("C:/Users/ryans/Dropbox/My PC (DESKTOP-UG4152D)/Desktop/EBAC/Courseware/EBA 5002 Business analytics practice/CA/CA2 RCS/Content Based")

########### To create the User-Ratings Matrix
# Import Reviews.csv
raw_events <- read.csv("reviews.csv", header=TRUE, sep=",") # transaction format!
raw_events[1,]
names(raw_events)
events = raw_events[c('author','app_id','rating')]

# Examine events dataframe
head(events) # Author, app_id, rating
summary(events) # ratings between 1-5

# Count the number of unique apps and reviewers
length(unique(events$app_id)) # 3733 apps
length(unique(events$author)) # 299,316 reviewers

# Select only the active reviewers (>= 10 reviews)
ucnts = aggregate(app_id ~ author, data = events, FUN=length)
colnames(ucnts) = c("author","numitems") 
activeusers = ucnts$author[ucnts$numitems >= 10]
length(activeusers) # 1338 active reviewers
active_events = events[events$author %in% activeusers,]

length(unique(active_events$app_id)) # 1946 apps
dim(active_events) # Remaining no of reviews = 19418
head(active_events)

# Create User-Ratings Matrix for active reviewers
active_events <- active_events %>% group_by(author,app_id) %>%
  summarize(rating=mean(rating))

users <- spread(active_events,app_id,rating,fill=0)
users <- users[-1,]
users_matrix <- as.matrix(users[,-1]) # user-rating matrix is a large matrix, 20.1Mb
rownames(users_matrix)<-users$author

###########


# Read Apps Data
apps <- read.csv("apps.csv", stringsAsFactor=FALSE, encoding="UTF-8")
apps <- apps %>% rename(app_id = id)
head(apps) # 12 variables - description and tagline contain most text data
dim(apps)

# Read key_benefits Data, and join text from title and description by app_id
key_benefits <- read.csv("key_benefits.csv", stringsAsFactor=FALSE, encoding="UTF-8") # 3 variables - appID, title, description
key_benefits$titledescription <- paste(key_benefits$title, key_benefits$description,sep=" ")
key_benefits2 <- aggregate(titledescription ~ app_id, data = key_benefits, paste, collapse = " ")

# Read apps_categories Data
apps_categories <- read.csv("apps_categories.csv", stringsAsFactor=FALSE, encoding="UTF-8") # 2 variables - appID, catID

# Read categories Data
categories <- read.csv("categories.csv", stringsAsFactor=FALSE, encoding="UTF-8") # 2 variables - catID, cat_name
categories <- categories %>% rename(category_id = id, app_category = title)

# Add key_benefits text to apps dataframe
apps <- left_join(apps,key_benefits2, by="app_id")
dim(apps)
apps$text <- paste(apps$description, apps$tagline, apps$titledescription, sep=" ")

# Add category feature to apps dataframe
# apps <- inner_join(apps,apps_categories, by="app_id")
# apps <- inner_join(apps,categories, by="category_id")
# head(apps)

# Keep Title and Description Columns
apps_subset <- apps[, c('app_id','title', 'text')]
head(apps_subset)

# Filter this apps_subset with the truncated apps list - only active users
apps_subset <- apps_subset[apps_subset$app_id %in% unique(active_events$app_id),]
head(apps_subset)
dim(apps_subset) # again left 1946

# Remove non-ASCII characters with space, replace \n with space
apps_subset$title <- iconv(apps_subset$title, "UTF-8", "ASCII",sub='')
apps_subset$text <- iconv(apps_subset$text, "UTF-8", "ASCII",sub='')
apps_subset$text <- str_replace_all(apps_subset$text, "[\n]" , "")
apps_subset[10,] # Test case to check if replace was successful

# Text Pre-processing
corpus <- VCorpus(VectorSource(apps_subset$text))

for(i in 1:5){
  print(corpus[[i]][1])
} 

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stripWhitespace)

# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

# Convert to Dense Matrix, add app_id column, sort by app_id
dtm_ti_densematrix<-as.matrix(dtm_ti) # 169.4Mb - matrix of all the term frequencies in the DTM by app_id(rows)
rownames(dtm_ti_densematrix) <- apps_subset$app_id
itemmatrix <- dtm_ti_densematrix[order(rownames(dtm_ti_densematrix)),]

dim(itemmatrix)

# the 1946 columns in user rating matrix match the 1946 rows in item matrix (app IDs already sorted)
# user(row) terms (columns) matrix which is m x p (1336 x 11349)
userprofile <- users_matrix %*% itemmatrix # %*% = matrix multiplication. Need m x n by n x p

##simplify the matrix dot product above

# To get similarity matrix by dot product(WORK IN PROGRESS) - to find similar users
sim_mat <- userprofile %*% t(itemmatrix)

# To get the consine similarity: cosine(user, item)=(user %*% item) /(|user|*|item|)
denominator<-sqrt(rowSums(userprofile^2)) %*% t(sqrt(rowSums(itemmatrix^2)))
cosine_sim_mat<-sim_mat/denominator

# Calculate Cosine Similarity
sim_mat_cos <- crossprod_simple_triplet_matrix(t(dtm_ti))/(sqrt(col_sums(t(dtm_ti)^2) %*% t(col_sums(t(dtm_ti)^2))))
result <- sort(sim_mat_cos[, 1], decreasing = T)[1:5]
result

# Get the index of the most similar movies in 'result'
result_i <- as.integer(names(result))

# Print out Description for viewing/checking
for (i in 1:5) {
  cat(paste0("\n",i, "- ", result_i[i], " <Title>", apps_subset[result_i[i], 1], "\n<Description>", apps_subset[result_i[i], 2]))
}

# Define function for app recommendation
AppRec = function (simm, app, k) {
  found <- sort(simm[, app], decreasing = T)[2:(k+1)]
  print(found)
  cat(paste0("\nSelected App: ", apps_subset[app, 1], "\n Description: ", apps_subset[app, 2]),"\n")
  cat("\nRecommended Apps:\n")
  resindex <- as.integer(names(found))
  print(resindex)
  for (i in 1:k) {
    cat(paste0("\n", "Recommended App No. ", i, " - ",apps_subset[resindex[i], 1], " (", resindex[i], ")", "\nDescription: ", apps_subset[resindex[i], 2]),"\n")
  }
}


# Find App Recommendations
AppRec(sim_mat_cos, 50, 5)


###########
# Incorportae Sentiment Analysis into product filtering - only recommend those with positive sentiments
###########

# Approach
# Create list of words associated with strongly positive or negative sentiment - use tidytext package's "bing" list
# - this "bing" list was developed by Bing Liu and coauthors: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
# Count the number of positive and negative words
# Analyze the mix of positive to negative words for each app
# - useful processing of text found here: https://www.tidytextmining.com/sentiment.html?

# Use Processed Apps Data - use apps_subset
install.packages("tidytext")
install.packages("tidyverse")
library(tidytext)
library(tidyverse)
# library(dplyr)
# library(stringr)

# Use apps_subset but Keep app_id and text Columns only to suit format this tokenization script of tidytext needs
apps_subset2 <- apps[, c('app_id', 'text')]
apps_subset2 <- apps_subset2 %>% rename(doc_id = app_id)
head(apps_subset2)

# tokenize entire apps_subset2
# tokens <- data_frame(text = apps_subset2) %>% unnest_tokens(word, text)

# apps_subset2 %>%
#  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
#  count(doc_id, sentiment) %>% # count the # of positive & negative words
#  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
#  mutate(sentiment = positive - negative) # # of positive words - # of negative words
# overall result should show 13412 negative, 78040 positive, 64628 sentiments

# tokenize apps_subset2 by individual app

apps_subset3 <- apps_subset2 %>%
  group_by(doc_id) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# get sentiments for each app_id - renamed doc_id. Sentiment score is reflected under variable sentiment
apps_subset_with_sentiment <- apps_subset3 %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(doc_id, sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

apps_subset_with_sentiment <- apps_subset_with_sentiment %>% rename(app_id = doc_id) # rename doc_id back to app_id

# save apps_subset_with_sentiment

write.csv(apps_subset_with_sentiment,'apps_subset_with_sentiment.csv') # app (row) positive, negative and overall sentiment score (columns)

