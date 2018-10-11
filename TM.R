library(XML)
library(tm)
library(readr) 
library(dplyr)
##### Constructing TF-IDF Matrices #####

# 
train<-read_csv("train.csv")
test<- read_csv(file="test.csv")
train1 = data.frame(train)

#Check if there's any missing values
miss = sapply(train1, function(x) length(which(is.na(x))))
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

missed<-train1[is.na(train1$text),]

#Since we will be grouping the data by userids and concatenate their blog post under each user id, 
#

#List the existing values of age, we can see that age ranges from 13-48
n_occur <- data.frame(table(train1$age))
n_occur

#Take a look at the dataset
train1[1:2,]

#Group this dataset by user
train2 <- aggregate(text~user.id,data = train1, paste, collapse = ",")
train2[1:2,]

#We will remove these rows, it is comparatively small set. So it won't have siginifcant effect on our analysis
train3 = na.omit(train2)

#Now check, good to go
miss = sapply(train3, function(x) length(which(is.na(x))))
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

# the tm package expects the columns to have specific names
names(train3) = c("doc_id", "text")

# convert the data frame to a volatile (in-memory) corpus object.
train4 = VCorpus(DataframeSource(train3))

# regular indexing returns a sub-corpus
inspect(train4[1:2])

# double-indexing accesses actual documents
train4[[1]]
train4[[1]]$content

# compute TF-IDF matrix and inspect sparsity
train.tfidf = DocumentTermMatrix(train4, control = list(weighting = weightTfIdf))
train.tfidf  # non-/sparse entries indicates how many of the DTM cells are non-zero and zero, respectively.
            # sparsity is number of non-zero cells divided by number of zero cells.

# inspect sub-matrix:  first 5 documents and first 5 terms
train.tfidf[1:5,1:5]
as.matrix(train.tfidf[1:5,1:5])

##### Reducing Term Sparsity #####

# there's a lot in the documents that we don't care about. clean up the corpus.
train4.clean = tm_map(train4, stripWhitespace)                          # remove extra whitespace
train4.clean = tm_map(train4.clean, removeNumbers)                      # remove numbers
train4.clean = tm_map(train4.clean, removePunctuation)                  # remove punctuation
train4.clean = tm_map(train4.clean, content_transformer(tolower))       # ignore case
train4.clean = tm_map(train4.clean, removeWords, stopwords("english"))  # remove stop words
train4.clean = tm_map(train4.clean, stemDocument)                       # stem all words

# compare original content of document 1 with cleaned content
train4[[1]]$content
train4.clean[[1]]$content  # do we care about misspellings resulting from stemming?

# recompute TF-IDF matrix using the cleaned corpus
train4.clean.tfidf = DocumentTermMatrix(train4.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
train4.clean.tfidf[1:5,1:5]
as.matrix(train4.clean.tfidf[1:5,1:5])

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(train4.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tfidf.99
as.matrix(tfidf.99[1:5,1:5])

# tfidf.70 = removeSparseTerms(news.clean.tfidf, 0.70)  # remove terms that are absent from at least 70% of documents
# tfidf.70
# as.matrix(tfidf.70[1:5, 1:5])
# news.clean[[1]]$content

# calculate inter-document similarity (euclidean)
dtm.tfidf.99 = as.matrix(tfidf.99)
dtm.dist.matrix = as.matrix(dist(dtm.tfidf.99))

# inspect documents most similar to (i.e., smallest distance from) the first document
most.similar.documents = order(dtm.dist.matrix[1,], decreasing = FALSE)
train4[[most.similar.documents[1]]]$content
train4[[most.similar.documents[2]]]$content
train4[[most.similar.documents[3]]]$content
train4[[most.similar.documents[4]]]$content
train4[[most.similar.documents[5]]]$content

# Select subset for cross-validation
sub1 <- sample(1:1460,size=730)
p1.train <- p1.train[sub1,]     
p1.test <- p1.train[-sub1,]
