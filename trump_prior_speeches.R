library(tm)
library(wordcloud)

setwd("/Users/jenyeulo/Documents/Syracuse_University/IST687/csv_files/")#set my working directory
options(stringsAsFactors = FALSE)

trumpfile <- "trump_june15_nov16_speeches.txt"

trump <- readLines(trumpfile)
head(trump,3) #inspect the first three paragraphs

words.vec <- VectorSource(trump)
words.corpus <- Corpus(words.vec) #coerce the text file vector (sba) into a custom class called a corpus.
words.corpus

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, stripWhitespace)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english")) #remove english stopwords.

#create term-document matrix
tdm <- TermDocumentMatrix(words.corpus)
tdm
inspect(tdm)

#creating word clouds
m <- as.matrix(tdm) #coerce text data back into a plain data matrix so that we can sort it by frequency.
wordCounts <- rowSums(m) #sum the frequency of the words.
wordCounts <- sort(wordCounts, decreasing=TRUE) #sorted with the most frequent terms appearing first.
head(wordCounts)

cloudFrame <- data.frame(word=names(wordCounts), freq=wordCounts)
#wordcloud(cloudFrame$word, cloudFrame$freq) #creates a wordcloud with the most frequent term the largest.

wordcloud(names(wordCounts), wordCounts,
          min.freq = 2,
          max.words = 50,
          rot.per = 0.35,
          colors=brewer.pal(8,"Dark2")) #make it more colorful


#Ch15 Happy Words?
pos <- "positive-words.txt"
neg <- "negative-words.txt"

#seperate each word
p <- scan(pos,character(0), sep = "\n")
n <- scan(neg,character(0), sep = "\n")

#remove the headers
p <- p[-1:-34]
n <- n[-1:-34]

#count the total number of words in our text
totalWords <- sum(wordCounts)
#have a vector that just has all the words
words <- names(wordCounts)
words
#match the positve words, output are index numbers
matched <- match(words, p, nomatch = 0)
head(matched,10) #the first 7 words did not match, but the 8th word matched with the 1083rd word in "words".
matched[8] #shows the index number of the matched word
p[1083] #what is the 8th word in the positive word variable.
words[8] #what is the 8th word in "words"

#count all the words that did match
mCounts <- wordCounts[which(matched !=0)]
length(mCounts) #there are 510 unique positive words in the speech.

mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos #there are 9136 positive words total including duplicates.

#do the same for negative words.
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched !=0)]
nNeg <- sum(nCounts)
nWords <- names(nCounts)
nNeg #7324 total negative words scanned
length(nCounts) #864 unique negative words

#calculate percentage of positive and negative words for this speech:
ratioPos <- nPos/totalWords
ratioPos

ratioNeg <- nNeg/totalWords
ratioNeg
