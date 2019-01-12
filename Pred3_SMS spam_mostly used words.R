#Importing the file
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

#Converting the Type column into factor
sms_raw$type <- factor(sms_raw$type)

#Check whether Type column is decodd into factor
str(sms_raw$type)
table(sms_raw$type)

#Installing the TM package to use VCorpus function
install.packages("tm")
library(tm)

#sing Vcorpus function to make a collection of sms messages
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

#Using inspect() to recieve summary
inspect(sms_corpus[1:2])

#View single line of sms character message
as.character(sms_corpus[[1]])

#View multiple character sms messages
lapply(sms_corpus[1:2], as.character)

#Using tolower to retrn lower case version of string text
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

#Inspect the first message in corpus
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#Removing numbers from the test message
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#Using stopwords() to remove filler words
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

#Using removepunctation() to eleiminate punctuation from he text message
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
removePunctuation("hello...world")

#Installing snowballc() to use wodstem()
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

#Using stemdocument() to apply wordstem() to entire corpus
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#Remove whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#Creating DTM sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE, removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE, stemming = TRUE))
sms_dtm
sms_dtm2

#Creating training and test dataset
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

#saving a pair of vectors with labels
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

#Comparing the proportion of spam in training and test
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Installing wordcloud() to visualize the data
install.packages("wordcloud")
library(wordcloud)

#using wordcloud() to see what type of words are used in sms
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

#Taking subset of sms_raw data and ham data
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

#Adjusting maximum and minimum font size for words in the cloud
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Finding frequent words and saving them for later use
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

#Filter DTM accroding to the vector
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#Using convert_counts() to convert the count to yes or no
convert_counts <- function(x) {x <- ifelse(x > 0, "Yes", "No")}

#Using apply() to be used in each row and coloumn in a matrix
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)

#Installing e1071 package to apply naive bayes
install.packages("e1071")
library(e1071)

#Building model on sms_train matrix
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Using predict() and storing the values in sms_test_pred
sms_test_pred <- predict(sms_classifier, sms_test)

#Comparing prediction to the actual values
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))

#Building Naive Bayes model
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)

#Making predictions
sms_test_pred2 <- predict(sms_classifier2, sms_test)

#Comparing predicted classes to the actual classifications
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

