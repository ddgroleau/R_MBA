
# ---- Set Working Directory, set variable to contents of text file,
# ---- Remove Punctuation and Unwanted Spaces
getwd()
setwd("C:/Users/138070/dropbox/MBA")
dataFromFile = readLines("dataForHW.txt")
# dataFromFile
cleanData = dataFromFile
cleanData = gsub("<.*?>","", cleanData)
cleanData = gsub("[[:punct:]]","",cleanData)
cleanData = gsub("[[:space:]]"," ",cleanData)

# ---- Create the corpus from the clean data
corpusData = VCorpus(VectorSource(cleanData))
# corpusData[[1]]$content
cleanCorpusData = corpusData

# ---- Remove stopwords from the clean corpus data
cleanCorpusData = tm_map(cleanCorpusData, removeWords, stopwords("english"))
# corpusData[[1]]$content
# cleanCorpusData[[1]]$content

# ---- Stem Document to find base terms
cleanCorpusData = tm_map(cleanCorpusData, stemDocument)

# ---- Create the term document matrix variable
# class(cleanData)
# class(cleanCorpusData)
myTDMData <- TermDocumentMatrix(cleanCorpusData)

# ---- Question 1 a) Find most frequent 3 terms
findFreqTerms(myTDMData, lowfreq = 2850, highfreq = Inf)

# ---- Question 1 b) Find most frequent 5 terms
findFreqTerms(myTDMData, lowfreq = 2700, highfreq = Inf)

# ---- Question 1 c) Find most frequent 7 terms
findFreqTerms(myTDMData, lowfreq = 1800, highfreq = Inf)

# ---- Question 2 -------- Reset data and remove digits (6 lines down)
dataFromFile = readLines("dataForHW.txt")
# dataFromFile
cleanData = dataFromFile
cleanData = gsub("[[:punct:]]","",cleanData)
cleanData = gsub("[[:space:]]"," ",cleanData)
cleanData = gsub("[[:digit:]]"," ",cleanData)

# ---- Question 2 -------- Re-create the corpus and term document matrix
corpusData = VCorpus(VectorSource(cleanData))
cleanCorpusData = corpusData
cleanCorpusData = tm_map(cleanCorpusData, removeWords, stopwords("english"))
# corpusData[[1]]$content
# cleanCorpusData[[1]]$content
cleanCorpusData = tm_map(cleanCorpusData, stemDocument)
myTDMData <- TermDocumentMatrix(cleanCorpusData)

# ---- Question 2 a) Find most frequent 3 terms
findFreqTerms(myTDMData, lowfreq = 2800, highfreq = Inf)

# ---- Question 2 b) Find most frequent 5 terms
findFreqTerms(myTDMData, lowfreq = 2000, highfreq = Inf)

# ---- Question 2 c) Find most frequent 7 terms
findFreqTerms(myTDMData, lowfreq = 1600, highfreq = Inf)

# --- Question 3 a) Find # of terms with frequency >= 500
length(findFreqTerms(myTDMData, lowfreq = 500, highfreq = Inf))

# --- Question 3 b) Find # of terms with frequency >= 250
length(findFreqTerms(myTDMData, lowfreq = 250, highfreq = Inf))

# --- Question 3 c) Find # of terms with frequency >= 2 and <=10
length(findFreqTerms(myTDMData, lowfreq = 2, highfreq = 10))

# --- Question 3 d) Find # of terms with frequency == 1
length(findFreqTerms(myTDMData, lowfreq = 1, highfreq = 1))

# --- Question 3 e) Find # of terms with frequency == 100
length(findFreqTerms(myTDMData, lowfreq = 100, highfreq = 100))

# --- Question 4 a) Show me the terms that show up 100 times 
findFreqTerms(myTDMData, lowfreq = 100, highfreq = 100)

# --- Question 4 b) Show me the terms that show up 75 times 
findFreqTerms(myTDMData, lowfreq = 75, highfreq = 75)

# --- Question 4 c) Show me the terms that show up 50 times 
findFreqTerms(myTDMData, lowfreq = 50, highfreq = 50)

# --- Question 5 a) Based on the term "bezukhov", find associated terms within 10% correlation
findAssocs(myTDMData, term="bezukhov",corlimit=0.1)

# --- Question 5 b) Based on the term "bezukhov", find associated terms within 7.5% correlation
findAssocs(myTDMData, term="bezukhov",corlimit=0.075)

# --- Question 5 c) Based on the term "bezukhov", find associated terms within 5% correlation
findAssocs(myTDMData, term="bezukhov",corlimit=0.05)

# --- Question 6 Find the top 2 terms in your data 
findFreqTerms(myTDMData, lowfreq = 3000, highfreq = Inf)
## The answer to this question is most likely the book "War & Peace"
