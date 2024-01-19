library(tm)
library(caret)
library(e1071)
library(wordcloud)
library(quanteda)

# Set working directory
setwd("C:/Users/coded/Desktop/R Demo Projects/Sentiment Analysis/archive")

# Import CSV file
data <- read.csv("IMDB Dataset.csv")

# Cleaning data
corpus <- Corpus(VectorSource(data$review))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Sample a subset of data
set.seed(123)
data_subset <- data[sample(nrow(data), 1000), ]

# Create a document-feature matrix (DFM) using quanteda
dfm <- dfm(data_subset$review, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stopwords = stopwords("en"))

# Convert DFM to a data frame
dtm_df <- as.data.frame(as.matrix(dfm))

# Create a binary sentiment label (1 for positive, 0 for negative)
data$sentiment_label <- ifelse(data$sentiment == "positive", 1, 0)

# Train-test split
set.seed(123)
splitIndex <- createDataPartition(data$sentiment_label, p = 0.7, list = FALSE)
train_data <- dtm_df[splitIndex, ]
test_data <- dtm_df[-splitIndex, ]
train_labels <- data$sentiment_label[splitIndex]
test_labels <- data$sentiment_label[-splitIndex]

# Visualize sentiment distribution
barplot(table(data$sentiment), main = "Sentiment Distribution", col = "skyblue", xlab = "Sentiment", ylab = "Count")

# Word clouds for positive and negative sentiments
positive_reviews <- data$review[data$sentiment == "positive"]
negative_reviews <- data$review[data$sentiment == "negative"]

wordcloud(words = unlist(strsplit(positive_reviews, " ")), min.freq = 5, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), main = "Word Cloud - Positive Sentiment")
wordcloud(words = unlist(strsplit(negative_reviews, " ")), min.freq = 5, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), main = "Word Cloud - Negative Sentiment")
