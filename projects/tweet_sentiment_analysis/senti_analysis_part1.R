
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

# Load packages
library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

# Function to compute accuracy of a classification model
tableAccuracy <- function(test, pred) {
  t = table(test, pred)
  a = sum(diag(t))/length(test)
  return(a)
}


# Load the data set
Tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(Tweets)

# We want to predict negative sentiment
# Lets create a new variable called "Negative" that converts the 
# "sentiment" number to negative (or not negative)
# anything less than or equal to -1 is negative
Tweets$Negative = as.factor(as.numeric(Tweets$Avg <= -1))
# And remove the old "Avg" column - we won't use it anymore
Tweets$Avg <- NULL

str(Tweets)

# Before going any further, lets understand the rough distribution of
# negative tweets in our data set
table(Tweets$Negative)

corpus = Corpus(VectorSource(Tweets$Tweet))
# The tweets are now "documents"
corpus[[1]]
strwrap(corpus[[1]])

# Step 2: Change all the text to lower case.

corpus = tm_map(corpus, tolower)
# Lets check:
strwrap(corpus[[1]])


# Step 3: Remove all punctuation
corpus = tm_map(corpus, removePunctuation)

strwrap(corpus[[1]])

# Step 4: Remove stop words

stopwords("english")[1:10]
length(stopwords("english"))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# Take a look:
strwrap(corpus[[1]])

# Step 5: Stem our document

corpus = tm_map(corpus, stemDocument)

strwrap(corpus[[1]])



# Step 6: Create a word count matrix (rows are tweets, columns are words)

frequencies = DocumentTermMatrix(corpus)
# We can get some summary information by looking at this structure
frequencies


# Step 7: Account for sparsity
# We currently have way too many words, which will make it hard to train
# our models and may even lead to overfitting.

# Words that appear at least 50 times:
findFreqTerms(frequencies, lowfreq=50)
# Words that appear at least 20 times:
findFreqTerms(frequencies, lowfreq=20)

# Our solution to the possibility of overfitting is to only keep terms
# that appear in x% or more of the tweets. For example:
# 1% of the tweets or more (= 12 or more)
sparse = removeSparseTerms(frequencies, 0.99)

# 0.5% of the tweets or more (= 6 or more)
sparse = removeSparseTerms(frequencies, 0.995)
# How many did we keep?
sparse

# Let's keep it at the 1%
sparse = removeSparseTerms(frequencies, 0.99)


# Step 8: Create data frame from the document-term matrix
TweetsTM = as.data.frame(as.matrix(sparse))
# We have some variable names that start with a number, 
# which can cause R some problems. Let's fix this before going
# any further
colnames(TweetsTM) = make.names(colnames(TweetsTM))
# This isn't our original dataframe, so we need to bring that column
# with the dependent variable into this new one
TweetsTM$Negative = Tweets$Negative


wordcloud(corpus, max.words = 200, random.order = FALSE, rot.per = .1, 
          colors = brewer.pal(8, "Dark2"))
