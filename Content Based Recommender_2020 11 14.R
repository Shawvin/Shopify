
# Install and Load Packages
pacman::p_load(tm,SnowballC,slam,dplyr,Matrix,stringr,tidyr,tidytext,tidyverse,tictoc)

# Set Working Directory
# setwd("C:/Users/christopher.chang/OneDrive - National University of Singapore/NUS EBAC/2 Business Analytics Practice/CA/CA 2 - TA & RS/Datasets/Shopify")

############### Create User-Ratings Matrix ###############

# Import reviews.csv
tic()

raw_events <- read.csv("reviews.csv", header=TRUE, sep=",")
events = raw_events[c('author','app_id','rating')]
head(events)
summary(events)
table(events$rating)
dim(events) # 447,317 review records

# Count number of unique apps and reviewers
length(unique(events$app_id)) # 3,733 apps
length(unique(events$author)) # 299,316 reviewers

# Select active reviewers (>= 2 reviews)
ucnts = aggregate(app_id ~ author, data = events, FUN=length)
colnames(ucnts) = c("author","review_count") 
activeusers = ucnts$author[ucnts$review_count >= 3]
length(activeusers) # 29,631 active reviewers with >= 3 reviews
active_events = events[events$author %in% activeusers,]
dim(active_events) # 132,681 reviews left (out of initial count of 447,317)
length(unique(active_events$app_id)) # 3,075 apps
head(active_events)

# Create User-Ratings Matrix for active reviewers
active_events <- active_events %>% group_by(author,app_id) %>%
  summarize(rating=mean(rating)) # To get mean rating for reviews by same author for same app_id
users <- spread(active_events,app_id,rating,fill=0)
users <- users[-1,] # remove 1st row where author is blank
users_mat <- as.matrix(users[,-1]) 
rownames(users_mat) <- users$author

toc()

############### Create DTM out of App Text ###############

tic()

# Read Apps Data
apps <- read.csv("apps.csv", stringsAsFactor=FALSE, encoding="UTF-8")
apps <- apps %>% rename(app_id = id)
head(apps) # 12 variables - description and tagline contain most text data
dim(apps)

# Read key_benefits Data, and join text from title and description by app_id
key_benefits <- read.csv("key_benefits.csv", stringsAsFactor=FALSE, encoding="UTF-8") # 3 variables - appID, title, description
key_benefits$titledescription <- paste(key_benefits$title, key_benefits$description,sep=" ")
key_benefits2 <- aggregate(titledescription ~ app_id, data = key_benefits, paste, collapse = " ")

# Add key_benefits text to apps dataframe
apps <- left_join(apps,key_benefits2, by="app_id")
dim(apps)
apps$text <- paste(apps$description, apps$tagline, apps$titledescription, sep=" ")

# Keep Title and Description Columns
apps_subset <- apps[, c('app_id','title', 'text')]
head(apps_subset)

# Filter this apps_subset with the truncated apps list - only active users
apps_subset <- apps_subset[apps_subset$app_id %in% unique(active_events$app_id),]
head(apps_subset)
dim(apps_subset) # Remaining apps: 3,395

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
dtm_ti_densematrix <- as.matrix(dtm_ti) # 169.4Mb - matrix of all the term frequencies in the DTM by app_id(rows)
rownames(dtm_ti_densematrix) <- apps_subset$app_id
item_mat <- dtm_ti_densematrix[order(rownames(dtm_ti_densematrix)),]
dim(item_mat)

toc()

############### Create User Profile ###############

# the 3,075 columns in user_mat matches the 3,075 rows in item_mat (app IDs already sorted)
# %*% = matrix multiplication. Need m x n by n x p
# user(row) terms (columns) matrix which is m x p (29,630 x 16,051)
# Note: This step below takes about 20 mins to run

tic()

userprofile <- users_mat %*% item_mat 

toc()

tic()
# Similarity Matrix 1: By dot product
sim_mat <- userprofile %*% t(item_mat)

# Similarity Matrix 2: By Cosine Similarity (cosine(user, item)=(user %*% item) /(|user|*|item|)
denominator <- sqrt(rowSums(userprofile^2)) %*% t(sqrt(rowSums(item_mat^2)))
sim_mat_cos <- sim_mat/denominator

toc()

# Export sim_mat_cos
write.csv(sim_mat_cos,'CBF_sim_mat_cos.csv')
sim_mat_cos <- read.csv('CBF_sim_mat_cos.csv')

############### Use Sentiment Analysis on Reviews for Filtering ###############

# Use apps_subset but Keep app_id and text Columns only to suit format this tokenization script of tidytext needs
tic()
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

toc()

############### Generate Filtered Recommendations ###############

tic()
# Create a list of apps with sentiment score > 0
positive_sentiment <- filter(apps_subset_with_sentiment, sentiment > 0)

# Select columns from sim_mat_cos with positive sentiment
# colnames(sim_mat_cos) <- c('user', rownames(item_mat))

sim_mat_cos_positive <- sim_mat_cos[,colnames(sim_mat_cos) %in% positive_sentiment$app_id]
sim_mat_cos_positive <- as.data.frame(sim_mat_cos_positive)
sim_mat_cos_positive <- cbind(rownames(sim_mat_cos_positive),sim_mat_cos_positive)
row.names(sim_mat_cos_positive) <- NULL
sim_mat_cos_positive <- sim_mat_cos_positive %>% rename(user_id = "rownames(sim_mat_cos_positive)")

# sim_mat_cos_positive$user_id <- sim_mat_cos$user

# Convert from wide to long format
df_long <- gather(sim_mat_cos_positive, "app_id", "similarity", -user_id)
dim(df_long)

# Group by User, display only top 10 apps by similarity 
df_long_top10 <- df_long %>% group_by(user_id) %>% slice_max(order_by=similarity,n=10)
dim(df_long_top10)
head(df_long_top10)

toc()

# Export Recommendations to CSV
write.csv(df_long_top10,'CBF_recommendations_top10.csv')
# write.csv(df_long,'CBF_recommendations_ALL.csv') # This file is 7.5GB file
