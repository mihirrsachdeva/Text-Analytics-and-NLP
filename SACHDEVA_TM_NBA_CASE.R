################################################################################
# Name: Mihir Sachdeva
# Assignment: 
# Subject: Text Analytics and NLP
# Email: msachdeva@student.hult.edu
# Date: March 2022
################################################################################
# Importing and loading all the required packages for our analysis

# install.packages("plyr")
# install.packages("qdap")
# install.packages("tm")
# install.packages("docstring")
# install.packages("ggthemes")
# install.packages("lubridate")
# install.packages('dendextend')
# install.packages('circlize')
# install.packages("magrittr")
# install.packages('wordcloud')
# install.packages('wordcloud2')
# install.packages('tidyverse')
# install.packages('tidytext')

library('dendextend')
library('circlize')
library("magrittr")
library('wordcloud')
library('wordcloud2')
library('ggplot2')
library("tidyverse")
library('tidytext')
library("plyr")
library("dplyr")
library("qdap")
library("tm")
library("docstring")
library("ggthemes")
library("lubridate")


# Options and Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
set.seed(123)

################################################################################
# Setting the working directory
setwd("~/Desktop/Text Analytics and NLP/Text-Mining-NLP/Case/Case I/Data_mod/")

# Checking if we have set the working directory properly
getwd()

################################################################################
# Merging the data sets of monthly intervals into a common dataset to work with
my_data         <- ("~/Desktop/Text Analytics and NLP/Text-Mining-NLP/Case/Case I/Data_mod/")

data_files      <- list.files(path   = my_data, 
                              pattern = ".csv")

NBA_data        <- ldply(data_files, read.csv)

# Basis of the analysis -- 
# The reason why I have chosen to create a new data directory for the CSV files is
# that I only wanted to use the months where the NBA league is live and actively 
# taken interest in. This helps me sort more relevant data. 

# Data from August, September, and October 2020 have been most significant 
# for my analysis as I am using the playoffs and finals at the foothill of my analysis. 
# The NBA is a globally enjoyed league of basketball with mentions and discussions
# throughout the year. As the analyst at NIKE, I need to put NBA into the 
# perspective of maximum reach and engagement.

# This will only happen when fans engage during the season and have more relevant
# and current discussions on the forum. We are comparing the top signed NIKE 
# athletes and how they are impacting fan engagement during the NBA season. 
# This will specifically be useful for the sports marketing field as we can 
# optimize player popularity for business opportunities and profit. 

# Season data sourced from - https://en.wikipedia.org/wiki/2019–20_NBA_season
# Player data sourced from - https://www.basketball-reference.com/playoffs/NBA_2020.html

################################################################################
# Taking a sample of 10% from our data
NBA_data    <- NBA_data[sample(nrow(NBA_data), size = (nrow(NBA_data)*(0.1))), ]

# exporting the tweet sample to NLP folder on local host
write.csv(NBA_data,'~/desktop/Text Analytics and NLP/twitter_data_sample.csv', 
          row.names = FALSE)

# Cleaning the data and getting it into a format that allows analysis
NBA_data <- read.csv('~/desktop/Text Analytics and NLP/twitter_data_sample.csv')

################################################################################
# Based on research from data available at - https://www.proballers.com
# Narrowing down our preview onto the teams of the top 3 NIKE athletes 
num_tweets <- count(NBA_data,team)
num_tweets[order(num_tweets$n), ]

# Plotting a bar chart to visually identify the quantity of tweets made in association
# with the teams of our top 3 signed athletes. It is important to notice that the 
# relevant athletes for our analysis come from the top 10 most tweeted about
# NBA teams according to the season stats.
ggplot(num_tweets, aes(x = reorder(team, n), y = n)) + 
  geom_bar(stat = "identity", width=0.8, fill='grey') + 
  coord_flip() + 
  theme_gdocs() + 
  theme(legend.position = "none") +
  scale_x_discrete(expand = c(0, 0.5)) +
  coord_flip(xlim = c(length(unique(num_tweets$team))-11,
                      length(unique(num_tweets$team))))
            
# From the above chart, choosing the teams with the top 3 athletes signed by NIKE 
# Using information to reduce the dataset to make relevant choices for 
# Giannis Antetokounmpo, Kevin Durant, and Lebron James.
nike_athlete_teams <- c('Milwaukee Bucks','LA Lakers','Brooklyn Nets')

# Subsetting and creating a new dataframe for the 6 chosen teams to analyse
NBA_data <- subset(NBA_data, team == nike_athlete_teams)
################################################################################
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words based of data collected from research and visualizing
# plots and word clouds that are created further in the project. Some unwanted 
# words containing profanity have also been added to stopwords.This is just to 
# keep the data and charts free from any offensive content that can create 
# negative impressions on the proceedings of the analysis.

NBA_SW <- c(stopwords('SMART'),stopwords('english'),'boston','celtics','new','jersey',
            'nets','new','york','knicks','philadelphia','76ers','toronto','raptors',
            'chicago','bulls','cleveland','cavaliers','detroit','pistons','indiana',
            'pacers','atlanta','hawks','charlotte','bobcats','miami',
            'heat','orlando','magic','washington','wizards','denver','nuggets',
            'minnesota','timberwolves','oklahoma','city','thunder','portland','trail',
            'blazers','utah','jazz','golden','state','warriors','los','angeles',
            'clippers','phoenix','suns','sacramento','kings',
            'dallas','mavericks','houston','rockets','memphis','grizzlies','new',
            'orleans','hornets','san','antonio','spurs', 
            '3PT','3PA','AST','BLK','BS','C','CBA','CONF','DIV','DNP','FG','FGP',
            'FT','G','GB','L','L10','MIN','MPG','NBA','NBDL','OFF','OT','PCT',
            'PF','PG','PTS','REB','RPG','SF','SG','STL','TO','W','allstar','national',
            'arena','rankings','rookie','wins','round','rebounds','told','ball','swingman',
            'nba','teams','fans','live','games','points','pacers', 'pts',
            'year','back','coach','tonight','city','time','today',
            'season','win','state','basketball','team','game','sports',
            'lol', 'smh', 'rofl', 'lmao', 'lmfao', 'wtf', 'btw','nbd','nvm', 'lmk', 
            'kk', 'obvi', 'obv', 'srsly','RT','steve','nash','shit','hell','no','race',
            'suck','idiot','stupid','fake','fuck','omg', 'omfg', 'idk', 'idc', 
            'jell', 'iirc', 'ffs', 'fml', 'idgaf', 'stfu', 'tf',
            'omw', 'rn', 'ttyl', 'tyt', 'bball','rt','ass','...','<','>')

  
# Team Data from - https://www.ducksters.com/sports/list_of_nba_teams.php
# Player Data from - https://www.sportingnews.com/us/nba/news/best-nba-players-entering-2021-22/vwqyadnexkbs1vh8etej6wm2f      
# ABV Data from - https://www.predictem.com/nba/nba-basketball-acronyms-abbreviations/

################################################################################
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- tolower(x)
  return(x)
}

NBA_sub <- basicSubs(NBA_data$text)

# Read in Data, clean & organize
txtCorpus <- VCorpus(VectorSource(NBA_data$text))
txtCorpus <- cleanCorpus(txtCorpus, NBA_SW)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

################################################################################
library(stringi)
nike   <- sum(stri_count(NBA_sub, fixed ='nike'))
gnns   <- sum(stri_count(NBA_sub, fixed ='giannis'))
kvnd   <- sum(stri_count(NBA_sub, fixed ='durant'))
lbrn   <- sum(stri_count(NBA_sub, fixed ='lebron'))

# Inspect word associations
nike_ass <- findAssocs(tweetTDM, 'nike', 0.30)
gnns_ass <- findAssocs(tweetTDM, 'giannis', 0.30)
kvnd_ass <- findAssocs(tweetTDM, 'durant', 0.30)
lbrn_ass <- findAssocs(tweetTDM, 'lebron', 0.30)

# Organize the word associations for Nike
nike_ass_df <- data.frame(terms=names(nike_ass[[1]]),
                     value=unlist(nike_ass))
nike_ass_df$terms <- factor(nike_ass_df$terms, levels=nike_ass_df$terms)
rownames(nike_ass_df) <- NULL

# Organize the word associations for Giannis Antetokounmpo
gnns_ass_df <- data.frame(terms=names(gnns_ass[[1]]),
                          value=unlist(gnns_ass))
gnns_ass_df$terms <- factor(gnns_ass_df$terms, levels=gnns_ass_df$terms)
rownames(gnns_ass_df) <- NULL

# Organize the word associations for Kevin Durant
kvnd_ass_df <- data.frame(terms=names(kvnd_ass[[1]]),
                          value=unlist(kvnd_ass))
kvnd_ass_df$terms <- factor(kvnd_ass_df$terms, levels=kvnd_ass_df$terms)
rownames(kvnd_ass_df) <- NULL

# Organize the word associations for LeBron James
lbrn_ass_df <- data.frame(terms=names(lbrn_ass[[1]]),
                          value=unlist(lbrn_ass))
lbrn_ass_df$terms <- factor(lbrn_ass_df$terms, levels=lbrn_ass_df$terms)
rownames(lbrn_ass_df) <- NULL

################################################################################ 
text_ass <- rm_url(NBA_data$text)

# Make a dot plot for Nike
ggplot(nike_ass_df, aes(y=terms)) +
  geom_point(aes(x=value), data=nike_ass_df, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="blue", hjust="inward", vjust ="inward" , size=3)

# NIKE
word_associate(text_ass, 
               match.string = 'nike', 
               stopwords = NBA_SW,
               network.plot = T,
               xlim = c(1, 50),
               cloud.colors = c('Blue','red'))

# MORE QDAP!
word_associate(text_ass, 
               match.string = 'nike', 
               stopwords = NBA_SW,
               wordcloud = T,
               cloud.colors = c('black','darkred'))

# Make a dot plot for Giannis Antetokounmpo
ggplot(kvnd_ass_df, aes(y=terms)) +
  geom_point(aes(x=value), data=kvnd_ass_df, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="blue", hjust="inward", vjust ="inward" , size=3) 

# Giannis Antetokounmpo
word_associate(text_ass, 
               match.string = 'giannis', 
               stopwords = NBA_SW,
               network.plot = T,
               xlim = c(1, 10),
               cloud.colors = c('Blue','red'))

# MORE QDAP!
word_associate(text_ass, 
               match.string = 'giannis', 
               stopwords = NBA_SW,
               wordcloud = T,
               cloud.colors = c('black','darkred'))

# Make a dot plot for Kevin Durant
ggplot(nike_ass_df, aes(y=terms)) +
  geom_point(aes(x=value), data=gnns_ass_df, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="blue", hjust="inward", vjust ="inward" , size=3) 

# Kevin Durant
word_associate(text_ass, 
               match.string = 'durant', 
               stopwords = NBA_SW,
               network.plot = T,
               cloud.colors = c('Blue','red'))

# MORE QDAP!
word_associate(text_ass, 
               match.string = 'durant', 
               stopwords = NBA_SW,
               wordcloud = T,
               cloud.colors = c('black','darkred'))

# Make a dot plot for LeBron James
ggplot(lbrn_ass_df, aes(y=terms)) +
  geom_point(aes(x=value), data=lbrn_ass_df, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="blue", hjust="inward", vjust ="inward" , size=3) 

# Lebron James
word_associate(text_ass, 
               match.string = 'lebron', 
               stopwords = NBA_SW,
               network.plot = T,
               cloud.colors = c('blue','darkred'))

# MORE QDAP!
word_associate(text_ass, 
               match.string = 'lebron', 
               stopwords = NBA_SW,
               wordcloud = T,
               cloud.colors = c('blue','darkred'))

# Collective wordcloud to see similarity trends between all players and the brand
word_associate(text_ass, 
               match.string = 'nike','giannis','durant','lebron',
               stopwords = NBA_SW,
               wordcloud = T,
               cloud.colors = c('blue','darkred','grey'))

################################################################################
# Reduce TDM
reducedTDM <- removeSparseTerms(tweetTDM, sparse=0.985) 
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

library(ggdendro)
ggdendrogram(hc, rotate=FALSE) 

################################################################################
# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(NBA_data))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, NBA_SW)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
NBA_TDM  <- TermDocumentMatrix(txtCorpus, 
                               control=list(tokenize=bigramTokens))
NBA_TDMm <- as.matrix(NBA_TDM)

################################################################################ 
# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

################################################################################ 
# See a bi-gram
nikeTweet <- grep('nike|giannis|durant|lebron', rownames(NBA_TDMm))
NBA_TDMm[(nikeTweet-2):(nikeTweet)]

################################################################################ 
# Get Row Sums & organize
NBA_TDMv <- sort(rowSums(NBA_TDMm), decreasing = TRUE)
NBA_DF   <- data.frame(word = names(NBA_TDMv), freq = NBA_TDMv)

################################################################################
# Make simple word cloud

# library
library(wordcloud2) 

# Gives a proposed palette
set.seed(123)

wordcloud2(NBA_DF, 
           size=1.6, 
           color='random-dark',
           minRotation = -pi/8, 
           maxRotation = -pi/8,
           rotateRatio = 1)

################################################################################ 

