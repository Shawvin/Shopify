
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

########### To create the User-Ratings Matrix
# Import Reviews.csv
raw_events <- read.csv("reviews.csv", header=TRUE, sep=",") # transaction format!
raw_events[1,]
names(raw_events)
events = raw_events[c('author','app_id','rating')]

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
users_matrix <- as.matrix(users)

###########


# Read Apps Data
apps <- read.csv("apps.csv", stringsAsFactor=FALSE, encoding="UTF-8")
apps <- apps %>% rename(app_id = id)
head(apps)
dim(apps)

# Read key_benefits Data, and join text from title and description by app_id
key_benefits <- read.csv("key_benefits.csv", stringsAsFactor=FALSE, encoding="UTF-8")
key_benefits$titledescription <- paste(key_benefits$title, key_benefits$description,sep=" ")
key_benefits2 <- aggregate(titledescription ~ app_id, data = key_benefits, paste, collapse = " ")

# Read apps_categories Data
apps_categories <- read.csv("apps_categories.csv", stringsAsFactor=FALSE, encoding="UTF-8")

# Read categories Data
categories <- read.csv("categories.csv", stringsAsFactor=FALSE, encoding="UTF-8")
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

# Filter this apps_subset with the truncated apps list
apps_subset <- apps_subset[apps_subset$app_id %in% unique(active_events$app_id),]
head(apps_subset)
dim(apps_subset)

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
dtm_ti_densematrix<-as.matrix(dtm_ti)
dtm_ti_densematrix <- cbind(apps_subset$app_id,dtm_ti_densematrix)
itemmatrix <- dtm_ti_densematrix[order(dtm_ti_densematrix[,1]),]
itemmatrix[,-1] <- sapply(itemmatrix[,-1],as.numeric)

dim(itemmatrix)

# Save Matrixes
write.csv(users_matrix,'users_matrix.csv')
write.csv(itemmatrix,'item_matrix.csv')

# To create the user profile
user_matrix <-read.csv("users_matrix.csv")
item_matrix <-read.csv("item_matrix.csv")

userprofile <- as.matrix(user_matrix[,3:1948]) %*% as.matrix(item_matrix[,3:11352])
userprofile <- cbind(users_matrix[,1],userprofile)

# To get similarity matrix (WORK IN PROGRESS)
sim_mat <- as.matrix(userprofile) %*% as.matrix(t(item_matrix[,3:11352]))
rownames(sim_mat) <- users_matrix[,1]
colnames(sim_mat) <- item_matrix[,2]

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

