# Title: NLP HW 1
# NAME: Mihir Sachdeva
# Date: March 06 2022

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

#### 
# set your working directory
setwd("~/Desktop/Text Analytics and NLP/Text-Mining-NLP/HW/HW1")

# Load the following libraries ggplot2, ggthemes stringi, and tm
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)

# load the homework data in an object called `text`
text <- read.csv('Dec292020Tweets.csv')
  
#### 

# Examine the first 10 rows of data
head(text , 10)

# Print the column names to console. 
colnames(text)

# What is the first tweet text? 
# Answer: "RT @BlueTsunami5: @MollyJongFast He also said we'd have a vaccine 
#          before the election. That he'd limit casualties to 40-50k Americans.
#          That\342\200\246"
text$text[1]

# What are the dimension (number of rows and columns)? Use a function. Hint dim(),nrow(), ncol() could all help you
# Answer: rows = 10000 and columns = 16
dim(text)

#### 
# Find out what rows have "virus" in the $text column, ignoring the case, in an object called idx
idx = grep("virus" , text$text , ignore.case = TRUE)

# What is the length of idx?
# Answer: 576
length(idx)

# What is the tenth text mentioning "virus"
# Answer: "RT @the_female_lead: 91-year-old Margaret Keenan has been given her
#          second dose of the Pfizer coronavirus vaccine!\nhttps://t.co/HDKJ3zlXtP"
idx[10]
text$text[idx[10]]

#### 
# Use grepl to make idx  for 'virus', ignoring case
idx = grepl("virus" , text$text , ignore.case = TRUE)


# Now what is the length of idx?
# Answer: 10000
length(idx)

# As a percent, how many tweets mention "virus" among all tweets?
# Answer: 5.76%
(sum(idx)/nrow(text))*100

#### 
# Write a function accepting a text column
# use gsub subsituting 'http\\S+\\s*' for '' which removes URLS
# use gsub substituting '(RT|via)((?:\\b\\W*@\\w+)+)' for '' which removes "RT" exactly
# use tolower in the function on the text
# return the changed text
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- tolower(x)
  return(x)
}

# apply the function to JUST THE TEXT COLUMN to a new object txt
txt <- basicSubs(text$text)


#### 
# Use sum with stri_count on the newt txt object
# with "trump", "biden" and in the last one check for "virus" OR "vaccine"
trump  <- sum(stri_count(txt, fixed ='trump'))
biden  <- sum(stri_count(txt, fixed ='biden'))
vterms <- sum(stri_count(txt, regex ='\\bvirus\\b|\\bvaccine\\b'))


# Organize term objects into a data frame
termFreq <- data.frame(terms = c('trump','biden','vterms'),
                       freq  = c(trump,biden, vterms))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data, adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

#### 
# Create some stopwords using the 'SMART' lexicon and add 'rofl'
stops <- c(stopwords('SMART') , 'rofl')

# Create a Clean Corpus Function
# add into the function removePunctuation
# add into the function removeNumbers
# add into the function stripWhitespace
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Apply the VCorpus Function to a VectorSource of the original text object
# Hint: only pass in the vector NOT the entire dataframe using a $
cleanTxt <- VCorpus(VectorSource(text$text))

# Clean the Corpus with your cleanCorpus function, this will take a few seconds
cleanTxt <- cleanCorpus(cleanTxt, stops)

# Construct a DTM in an object called cleanMat
cleanMat <- DocumentTermMatrix(cleanTxt)

# Switch this to a simple matrix still called cleanMat
cleanMat <- as.matrix(cleanMat)

# What are the dimensions of this matrix
dim(cleanMat)

# What do rows represent in this matrix?
# Answer: 10000
nrow(cleanMat)

# How many unique words exist in the matrix?
# Answer: 12123
uwm <- unique(cleanMat , incomparables = FALSE , MARGIN = 1 , 
              fromLast = FALSE , drop = FALSE)
ncol(uwm)
# Internet reference for uwm code - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/unique

# End

