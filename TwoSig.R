library(dplyr)
library(tm)
library(NLP)
library(openNLP)
two_sig = read.csv("C:/Users/marsh/Desktop/Kaggle/TS/two_sig.csv",stringsAsFactors = F)
two_sig = na.omit(two_sig)
two_sig$interest_level = as.factor(two_sig$interest_level)
str(two_sig)

text.source = VectorSource(two_sig$description)
text.corpus = VCorpus(text.source)

clean_corpus <- function(corpus){
  stops = c("starbucks","lot","people","come","can","drink","will","order","yeah","sure","able","however","also","yes", "coffee", "tea")
  all_stops <- c(stopwords("en"),stops)
  corpus = tm_map(corpus, content_transformer(tolower))
  #corpus = tm_map(corpus, content_transformer(replace_contraction))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removeWords,c(all_stops))
  corpus = tm_map(corpus, removePunctuation)
  return(corpus)
}

##One gram
cleaned.corpus = clean_corpus(text.corpus)
DTM = DocumentTermMatrix(cleaned.corpus)
DTM = removeSparseTerms(DTM,0.975)
DTM = as.matrix(DTM)
dim(DTM)

DTM_1_gram = data.frame(cbind(DTM,index = two_sig$X))

write.csv(DTM_1_gram,"C:/Users/marsh/Desktop/Kaggle/TS/DTM_1_gram.csv")

