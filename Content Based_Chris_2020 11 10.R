########## Install Packages
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
library(caret)
library(tibble)

########## Create User-Ratings Matrix
# Import reviews.csv
raw_events <- read.csv("reviews.csv", header=TRUE, sep=",")
events = raw_events[c('author','app_id','rating')]
head(events)
dim(events) # 447,317 review records

# Count number of unique apps and reviewers
length(unique(events$app_id)) # 3733 apps
length(unique(events$author)) # 299,316 reviewers

# Select active reviewers (>= 10 reviews)
ucnts = aggregate(app_id ~ author, data = events, FUN=length)
colnames(ucnts) = c("author","review_count") 
activeusers = ucnts$author[ucnts$review_count >= 10]
length(activeusers) # 1,338 active reviewers with >= 10 reviews
active_events = events[events$author %in% activeusers,]

dim(active_events) # 19,418 reviews records left
length(unique(active_events$app_id)) # 1,946 apps
head(active_events)

# Create User-Ratings Matrix for active reviewers
active_events <- active_events %>% group_by(author, app_id) %>%
  summarize(rating = mean(rating)) # To get rid of duplicate reviews by same author on same app_id
users <- spread(active_events, app_id, rating, fill = 0)
users <- users[-1,] # remove 1st row where author is blank
users_rat_mat <- as.matrix(users)

##########

# Import apps.csv
apps <- read.csv("apps.csv", stringsAsFactor=FALSE, encoding="UTF-8")
apps <- apps %>% rename(app_id = id)
dim(apps)
head(apps)

# Read key_benefits.csv, join text from title and description by app_id
key_benefits <- read.csv("key_benefits.csv", stringsAsFactor=FALSE, encoding="UTF-8")
key_benefits$text <- paste(key_benefits$title, key_benefits$description,sep=" ")
key_benefits <- aggregate(text ~ app_id, data = key_benefits, paste, collapse = " ")

# Join key_benefits text to apps
apps <- left_join(apps,key_benefits, by="app_id")
dim(apps)
apps$text <- paste(apps$description, apps$tagline, apps$text, sep=" ")

# Create apps_subset containing only app_id, title and text
apps_subset <- apps[, c('app_id','title','text')]
head(apps_subset)
dim(apps_subset) # 4,750 rows

# Filter apps_subset with apps list from active_events
apps_subset <- apps_subset[apps_subset$app_id %in% unique(active_events$app_id),]
dim(apps_subset) # 1,946 rows

# Remove non-ASCII characters with space, replace \n with space
apps_subset$title <- iconv(apps_subset$title, "UTF-8", "ASCII", sub='')
apps_subset$text <- iconv(apps_subset$text, "UTF-8", "ASCII", sub='')
apps_subset$text <- str_replace_all(apps_subset$text, "[\n]" , "")
apps_subset[1,] # Test case to check if replace was successful

# Text Pre-processing
corpus <- VCorpus(VectorSource(apps_subset$text))

# Print Corpus (before pre-processing)
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

# Print Corpus (after pre-processing)
for(i in 1:5){
  print(corpus[[i]][1])
} 

# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm_ti <- weightTfIdf(dtm)
dtm_ti

# Convert Sparse DTM Matrix into Dense Matrix, add app_id column, sort by app_id
dtm_ti_densematrix <- as.matrix(dtm_ti)
dtm_ti_densematrix <- cbind(apps_subset$app_id, dtm_ti_densematrix)
item_mat <- dtm_ti_densematrix[order(dtm_ti_densematrix[,1]),] # sort by app_id
dim(item_mat) # 1946r x 11,351c

# Save User-Ratings and Item Matrices
# Note: We weren't able to get the data to be in the correct type, but it worked
# when we exported the matrix and imported it again.
write.csv(users_rat_mat,'user_matrix.csv')
write.csv(item_mat,'item_matrix.csv')

user_mat <-read.csv("user_matrix.csv")
item_mat <-read.csv("item_matrix.csv")

# To create user profiles (dot product of user_mat and item_mat)
dim(user_mat) # 1337 x 1948
dim(item_mat) # 1946 x 11352
userprofile <- as.matrix(user_mat[,3:ncol(user_mat)]) %*% as.matrix(item_mat[,3:ncol(item_mat)])

# To get similarity matrix
sim_mat <- as.matrix(userprofile) %*% as.matrix(t(item_mat[,3:ncol(item_mat)]))
rownames(sim_mat) <- user_mat[,2]
colnames(sim_mat) <- item_mat[,2]

# Min-Max Scaling by Column
# Chris: Not sure if we should min-max scale by column actually, perhaps should
# just compare similarity score directly
preproc2 <- preProcess(sim_mat[,2:ncol(sim_mat)], method=c("range"))
sim_mat_norm <- predict(preproc2, sim_mat[,2:ncol(sim_mat)])

# Generate App Recommendations
AppRec = function(simm,user,k){
  cat(paste0("\nSelected User: ",row.names(simm)[user]))
  similarity <- sort(simm[user,], decreasing = TRUE)[1:k]
  df <- as.data.frame(similarity) %>% rownames_to_column("app_id")
  df <- left_join(df,apps_subset,by="app_id")
  for (i in 1:k) {
    cat(paste0("\n", "\n", "Recommended App ", i, " - ",df[i, 3],"           Similarity: ", round(df[i,2],digits=3), "\nDescription: ", df[i, 4],  "\n"))
  }
}

# Test out AppRec on sim_mat and sim_mat_norm
AppRec(sim_mat,1,5)
AppRec(sim_mat_norm,1,5)

# The recommendations produced by sim_mat and sim_mat_norm are different

# Questions: How do we know if our recommendations make sense? Do we have to compare
# with the apps that a user has rated before?



################ Old Code Below

# Read apps_categories.csv
apps_categories <- read.csv("apps_categories.csv", stringsAsFactor=FALSE, encoding="UTF-8")

# Read categories.csv
categories <- read.csv("categories.csv", stringsAsFactor=FALSE, encoding="UTF-8")
categories <- categories %>% rename(category_id = id, app_category = title)

# Add category feature to apps dataframe
# apps <- inner_join(apps,apps_categories, by="app_id")
# apps <- inner_join(apps,categories, by="category_id")
# head(apps)


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

