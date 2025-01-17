# we are going to be importing and exporting data, so we need to know where to find that data;
# start by getting your working directory (the place where our data will be imported from or exported to):
getwd()

# likely your working directory will show up in one of these forms:
# c:/users/YourOhioID/Documents
# \\\\home.ohio.edu/home/YourOhioID/Documents

# once we know what our current working directory is, it will make it easier to set our working directory; first, 
#   create a folder on your desktop called "learningActivity" - this is where we will place our data to be imported, 
#   along with where we will find exported data that we will generate during the activity;
# to set your working directory to a folder called "learningActivity" on your "Desktop", it would look like one 
#   of these commands (you will determine which according to your own environment and adjust either as necessary):
#
# setwd("c:/users/YourOhioID/Desktop/learningActivity")
# setwd("\\\\home.ohio.edu/home/YourOhioID/Desktop/learningActivity")

setwd("C:/Users/138070/Dropbox/MBA/Mod7")

# you do not have to install all of the packages at once; however, if you know what packages are required, 
#   it is OK to install them at once: ---> remember warnings are OK, errors are NOT

install.packages("twitteR")
install.packages("plyr")
install.packages("stringr")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")


# since we installed the packages that we will need, we can go ahead and load them since that will be required 
#   before we can use them:

library(twitteR)
library(plyr)
library(stringr)
library(tm)
library(wordcloud)
library(SnowballC)


# we can use a "c" funciton to combine data - in this example we will combine a list of words

positiveWords = c("good", "best", "great")
negativeWords = c("bad", "worse", "worst")

# display the list of words

class(positiveWords)
negativeWords
positiveWords

# while we could build the lists that we want to use for the sentiment analysis manually, we will use some larger 
#   pre-defined lists; in this case, we will build read a positive and negative lexicon (terms that associate with 
#   positive and negative) into two variables to store each list of words; remember, these files need to be in 
#   your current working directory before the following commands are executed:

positiveWords = readLines("positiveWords.txt")
negativeWords = readLines("negativeWords.txt")

# remember, you can do the following to change the amount of words that you can see:
# getOption("max.print")
# options(max.print = 10000)

# ***
# a general comment, functions in R can be easily implemented; they can serve any purpose and are based on the commands 
#   contained within the function; for example, if you find yourself constantly re-writing the same code in order to help 
#   you perform some work on data, the commands that you are using can be listed out in a function, so whenever you need 
#   to perform those commands, you can just call the function and it will do the work for you
# ***

# build a generalCleaning function to implement what we want to consider "general cleaning;" to use it, we will 
#   pass it some data, it will "clean" the data according to our rules, then return the "cleaned" data:
generalCleaning = function(dataToClean)
{
  dataToReturn = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", dataToClean) # remove hyperlinks
  dataToReturn = gsub("<.*?>", "", dataToReturn) # remove HTML
  dataToReturn = gsub("[^[:graph:]]", " ", dataToReturn) # anything not alphabetic and punctuation (e.g. emojis, pictures, videos)
  dataToReturn = gsub("[[:punct:]]", "", dataToReturn) # remove punctuation
  dataToReturn = gsub("[[:cntrl:]]", " ", dataToReturn) # remove control characters
  dataToReturn = gsub("\\d+", "", dataToReturn) # remove numbers
  dataToReturn = gsub("[[:space:]]", " ", dataToReturn) #remove "white space" characters, noting " " vs ""
  # dataToReturn = gsub(""", "", dataToReturn) #remove specific characters
  # dataToReturn = gsub("'", "", dataToReturn) #remove specific characters
  # dataToReturn = gsub(""", "", dataToReturn) #remove specific characters
  # dataToReturn = gsub("  ", " ", dataToReturn) #remove specific characters
  # dataToReturn = gsub("���", "", dataToReturn) #remove specific characters
  
  dataToReturn = tolower(dataToReturn) # notice how we make it lowercase before returning the data
  return(dataToReturn)
}

# build a calculateSentiment function which will require 5 lists of data (dataToScore, dataToDisplay, 
#   positiveListOfWords, negativeListOfWords); this function will count all instances of positive matches 
#   and subtract all instances of negative matches to suggest a sentiment score for the data:
calculateSentiment = function(dataToScore, dataToDisplay, positiveListOfWords, negativeListOfWords, nameOfDataset)
{
  # for every row of data in dataToScore we will calculate the sentiment score, then build a list of scores 
  #   to list next to dataToDisplay:
  listOfScores = laply(dataToScore, function(singleRowOfData, positiveListOfWords, negativeListOfWords) {
    words = unlist(str_split(singleRowOfData, '\\s+')) #generates a list of all words in the row of data
    # next, generate a list indicating "true" for every word in the list of "words" that is also in 
    #   positiveListOfWords, otherwise indicates "false":
    positiveMatches = !is.na(match(words, positiveListOfWords)) 
    negativeMatches = !is.na(match(words, negativeListOfWords)) #same idea, but for negativeListOfWords
    # sum will count up all instances of "true" as 1, so in this case, we are counting the positiveMatches 
    #   then subtracting from negativeMatches:
    scoreForSingleRow = sum(positiveMatches) - sum(negativeMatches)
    return(scoreForSingleRow)
  }, positiveListOfWords, negativeListOfWords, .progress="text" )
  
  # remove emojis, pictures, videos, etc from our output (notice, dataToDisplay did not run through our 
  #   general cleaning)
  dataToDisplay = gsub("[^[:graph:]]", " ", dataToDisplay)
  
  # create a dataframe which will be the required data structure used to create a CSV (comma separated value) file;
  #   basically, return three columns, "which dataset, sentiment and text" with data listed under each:
  dataToReturn = data.frame("whichDataset"=nameOfDataset, sentiment=listOfScores, text=dataToDisplay)
  return(dataToReturn)
}

################################################################
# the setup_twitter_oauth and searchTwitter function will only work if you have previously setup a twitter 
#   account AND have successfully received twitter developer approval - see the document provided with 
#   the learning activity that describes these steps
################################################################

# https://www.rdocumentation.org/packages/twitteR/
# use your twitter account to be able to use the twitteR package to access the Twitter api to get direct twitter 
#   access; pay attention to the "Authentication with OAuth" section which will be in this format:
#setup_twitter_oauth("API key", "API secret", "Access token", "Access secret")

# searchTwitter provides access similar to using the search bar within Twitter:
#myTweets = twListToDF(searchTwitter("jeep for sale", num=5))

# userTimeline provides access to a stream of tweets based on the twitter username
#myTweets = twListToDF(userTimeline("ohiou", n=5))

# many more functions can be found here:
# https://www.rdocumentation.org/packages/twitteR/
# https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

#write.csv(myTweets, "jeepTweetsExample.csv", row.names=FALSE)

####### Here is where you continue - let's read the "jeepTweetsExample.csv" into "myTweets" 
myTweets = read.csv("jeepTweetsExample.csv")

# different ways to look at some of the data within the myTweets data
myTweets
# what is the data structure of tweets?
class(myTweets)
# display the internal data structure of tweets
str(myTweets)
# look at an individual column of data in tweets
myTweets$text


# note, the following datasets came from:
# https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/BarackObama.csv
# https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/realDonaldTrump.csv

# you could load them directly from those locations (assuming your computer can access the URLs), as a pre-caution, 
#   the 2 datasets have been provided so you can load them from your working directory:
myTweets1 = read.csv("BarackObama.csv", header=TRUE, sep=",")
myTweets2 = read.csv("realDonaldTrump.csv", header=TRUE, sep=",")

# take a look at the data

str(myTweets1)
summary(myTweets1)
#add a new column (called "whichDataset") to each dataframe and assign the entire column a value
myTweets1$whichDataset = "Obama"
myTweets2$whichDataset = "Trump"

# similar to how we used the "c" function, here we will use "rbind" to combine to dataframes
myTweets = rbind(myTweets1, myTweets2)

# take a look at the data

str(myTweets)
summary(myTweets)

# store the text column data in a variable - you will see how we use this shortly
myDataBeforeCleaning = myTweets$text
# store the text column data in a variable that we will use to score the sentiment of the data; this variable will
#   first pass through our generalCleaning function so that the state of data will be able to be compared to the 
#   imported positive/negative lexicons:
myDataAfterCleaning = generalCleaning(myTweets$text)

myDataBeforeCleaning[42]
myDataAfterCleaning[42]


# pass the text from the tweets to the sentiment function for scoring
scores = calculateSentiment(myDataAfterCleaning, myDataBeforeCleaning, positiveWords, negativeWords, myTweets$whichDataset)

# generate a sentiment.csv file, which should be in your working directory, that contains the sentiment score value 
#   (positive >0 or negative <0) along with the text that the score applies to; 

write.csv(scores, "sentiment.csv")

# let's not include "row.names"
write.csv(scores, "sentiment.csv", row.names = FALSE)

# build a buildTDM function to create a "term document matrix"
# to use it, we will pass it some data, it will format the data as necessary, remove stop words and stem the words, 
#   then return a term document matrix:

buildTDM = function(inputData)
{
  myCorpusData = VCorpus(VectorSource(inputData))
  myCorpusData = tm_map(myCorpusData, removeWords, stopwords("english"))
  myCorpusData = tm_map(myCorpusData, stemDocument)
  return(TermDocumentMatrix(myCorpusData))
  }

# generate myTDMData by passing our data to our buildTDM function:
myTDMData = buildTDM(myDataAfterCleaning)

# now that we have a TDM we can run functions directly on myTDMData like we have previously seen: 

findFreqTerms(myTDMData, 150, Inf)
findFreqTerms(myTDMData, 300, Inf)
# notice the "���" - we could modify our generalCleaning function to targer and remove this data
findAssocs(myTDMData, term="american", corlimit=0.1)

# additionally, we can use our TDM to build a wordcloud for quick visualization:
freqOfTerms = sort(rowSums(as.matrix(myTDMData)), decreasing=TRUE)

# get a list of the words from the heading
namesFromData = names(freqOfTerms)

# show the importance of words based on frequency with a word cloud
wordcloud(namesFromData, freqOfTerms, min.freq = 150)
