
#load libraries
require(stringi)
require("Rgraphviz")
library(wordcloud)
library(tau)
library(tm)
library(RWeka)
library(rJava)
library(plyr)


#load dataset
tfilename <- "./Coursera-SwiftKey/en_US/en_US.twitter_005.txt"

datasource<-as.matrix(readLines(tfilename,-1,skipNul = TRUE))
datasource <- iconv(datasource, "latin1", "ASCII", sub="")

sampledata <- Corpus(DataframeSource(datasource))
sampledata <- tm_map(sampledata, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
sampledata <- tm_map(sampledata, content_transformer(tolower)) #convert all to lower characters

# convert apostroph words to longer words
# credit: http://www.textfixer.com/resources/common-english-words.php
processApostrophe <- function(y){
        x<- gsub("\'ll", " will", y); x<- gsub("\'ve", " have", x);
        x<- gsub("ain't", " am not", x); x<- gsub("aren't", " are not", x);
        x<- gsub("can't", " cannot", x); x<- gsub("could've", " could have", x);
        x<- gsub("didn't", "did not", x); x<- gsub("doesn't", "does not", x);
        x<- gsub("don't", "do not", x); x<- gsub("hasn't", "has not", x);
        x<- gsub("he'd", "he would", x); x<- gsub("he'll", "he will", x);
        x<- gsub("he's", "he is", x); x<- gsub("how'd", "how would", x);
        x<- gsub("how'll", "how will", x); x<- gsub("how's", "how is", x);
        x<- gsub("i'd", "i would", x); x<- gsub("i'll", "i will", x);
        x<- gsub("i'm", "i am", x); x<- gsub("i've", "i have", x);
        x<- gsub("isn't", "is not", x); x<- gsub("it's", "it is", x);
        x<- gsub("might've", "might have", x); x<- gsub("mustn't", "must not", x);
        x<- gsub("shan't", "shall not", x); x<- gsub("she'd", "she would", x);
        x<- gsub("she'll", "she will", x); x<- gsub("she's", "she is", x);
        x<- gsub("should've", "should have", x); x<- gsub("shouldn't", "should not", x);
        x<- gsub("that'll", "that will", x); x<- gsub("that's", "that is", x);
        x<- gsub("there's", "there is", x); x<- gsub("they'd", "they would", x);
        x<- gsub("they'll", "they will", x); x<- gsub("they're", "they are", x);
        x<- gsub("they've", "they have", x); x<- gsub("why'd", "why would", x);
        x<- gsub("why'll", "why will", x); x<- gsub("why's", "why is", x);
        x<- gsub("won't", "will not", x); x<- gsub("would've", "would have", x);
        x<- gsub("wouldn't", "would not", x);x<- gsub("you'd", "you would", x);
        x<- gsub("you'll", "you will", x); x<- gsub("you've", "you have", x);
        x<- gsub("you're", "you are", x);x<- gsub("let's", "let us", x);
        x<- gsub("wasn't", "was not", x);
        #convert slang words commonly found in twitter
        x<- gsub(" u ", " you ", x);x<- gsub("lets", "let us", x);
        x<- gsub(" im ", " i am ", x);x<- gsub(" ur ", " you are ", x);
        x<- gsub(" gonna ", " going to ", x);
        x
}
sampledata <- tm_map(sampledata, content_transformer(processApostrophe)) # remove aprostrphe

sampledata <- tm_map(sampledata, content_transformer(removePunctuation)) #remove puctuations
sampledata <- tm_map(sampledata, content_transformer(removeNumbers)) #remove numbers
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) #remove urls
sampledata <- tm_map(sampledata, content_transformer(removeURL)) #remove url
removeURL2 <- function(x) gsub("www.[[:alnum:]]+.com", "", x) #remove urls
sampledata <- tm_map(sampledata, content_transformer(removeURL2)) #remove url

removeChar <- function(x) gsub("[^[:alpha:][:space:]]", "", x) #remove unwanted char
sampledata <- tm_map(sampledata, content_transformer(removeChar)) #remove other unwanted char
sampledata <- tm_map(sampledata, content_transformer(stripWhitespace)) #remove whitespace

# save it as a rds file.
saveRDS(sampledata, file = "./MilestoneReport/cleanedCorpusTwitter005.RDS")

