setwd("C:/Users/138070/Dropbox/MBA/HW7")

# Install Necessary Packages
install.packages("twitteR")
install.packages("plyr")
install.packages("stringr")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
library(twitteR)
library(plyr)
library(stringr)
library(tm)
library(wordcloud)
library(SnowballC)

# Declare and initialize variables
positiveWords = readLines("positiveWords.txt")
negativeWords = readLines("negativeWords.txt")
# Create general cleaning function
generalCleaning = function(dataToClean)
{
  dataToReturn = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", dataToClean) # remove hyperlinks
  dataToReturn = gsub("<.*?>", "", dataToReturn) # remove HTML
  dataToReturn = gsub("[^[:graph:]]", " ", dataToReturn) # anything not alphabetic and punctuation (e.g. emojis, pictures, videos)
  dataToReturn = gsub("[[:punct:]]", "", dataToReturn) # remove punctuation
  dataToReturn = gsub("[[:cntrl:]]", " ", dataToReturn) # remove control characters
  dataToReturn = gsub("\\d+", "", dataToReturn) # remove numbers
  dataToReturn = gsub("[[:space:]]", " ", dataToReturn) #remove "white space" characters, noting " " vs ""
  dataToReturn = tolower(dataToReturn) # notice how we make it lowercase before returning the data
  return(dataToReturn)
}
# create calculate sentiment function
calculateSentiment = function(dataToScore, dataToDisplay, positiveListOfWords, negativeListOfWords, nameOfDataset)
{
  listOfScores = laply(dataToScore, function(singleRowOfData, positiveListOfWords, negativeListOfWords) {
    words = unlist(str_split(singleRowOfData, '\\s+'))
    positiveMatches = !is.na(match(words, positiveListOfWords)) 
    negativeMatches = !is.na(match(words, negativeListOfWords)) #same idea, but for negativeListOfWords
    scoreForSingleRow = sum(positiveMatches) - sum(negativeMatches)
    return(scoreForSingleRow)
  }, positiveListOfWords, negativeListOfWords, .progress="text" )
  dataToDisplay = gsub("[^[:graph:]]", " ", dataToDisplay)
  dataToReturn = data.frame("whichDataset"=nameOfDataset, sentiment=listOfScores, text=dataToDisplay)
  return(dataToReturn)
}
# Declare and initialize csv file variables
drudgeReport = read.csv("drudge_report.csv", header = TRUE, sep = ",")
npr = read.csv("NPR.csv", header = TRUE, sep = ",")
nyTimes = read.csv("nytimes.csv", header = TRUE, sep = ",")
wsj = read.csv("WSJ.csv", header = TRUE, sep = ",")
# create dataset column for each csv file
drudgeReport$whichDataset = "Drudge Report"
npr$whichDataset = "NPR"
nyTimes$whichDataset = "NY Times"
wsj$whichDataset = "WSJ"
# bind csv data together
myData = rbind(drudgeReport, npr, nyTimes, wsj)
# establish "dirty" data variable
myDataBeforeCleaning = myData$text
# establish clean data variable
myDataAfterCleaning = generalCleaning(myData$text)

# myDataBeforeCleaning[42]
# myDataAfterCleaning[42]
# calculate scores
scores = calculateSentiment(myDataBeforeCleaning, myDataAfterCleaning, positiveWords, negativeWords, myData$whichDataset)
# create first sentiment file
write.csv(scores, "sentiment.csv", row.names = FALSE)
# create build tdm function
buildTDM = function(inputData)
{
  myCorpusData = VCorpus(VectorSource(inputData))
  myCorpusData = tm_map(myCorpusData, removeWords, stopwords("english"))
  myCorpusData = tm_map(myCorpusData, stemDocument)
  return(TermDocumentMatrix(myCorpusData))
}
# establish term document matrix
myTDMData = buildTDM(myDataAfterCleaning)
# Build wordcloud
freqOfTerms = sort(rowSums(as.matrix(myTDMData)), decreasing=TRUE)
namesFromData = names(freqOfTerms)
wordcloud(namesFromData, freqOfTerms, min.freq = 35)
# Tab 4 A:
length(findFreqTerms(myTDMData, 60, Inf))
findFreqTerms(myTDMData, 60, Inf)
# Tab 4 B:
length(findFreqTerms(myTDMData, 75, Inf))
findFreqTerms(myTDMData, 75, Inf)
# Tab 4 C:
length(findFreqTerms(myTDMData, 110, Inf))
findFreqTerms(myTDMData, 110, Inf)

# Generate new sentiment file with 2x new lexicons
happyWords = readLines("happyWords.txt")
angryWords = readLines("angryWords.txt")
happyWords = tolower(happyWords)
angryWords = tolower(angryWords)
newScores = calculateSentiment(myDataBeforeCleaning, myDataAfterCleaning, happyWords, angryWords, myData$whichDataset)
write.csv(newScores, "myAlternativeSentiment.csv", row.names = FALSE)
