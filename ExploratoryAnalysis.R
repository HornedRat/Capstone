library(tm)
library(wordcloud)
library(ngram)

#Choose the 'en_us' folder with data
data.path <- choose.dir()

collection <- VCorpus(DirSource(data.path, encoding = "UTF-8"))

summary(collection)
head(collection[[1]]$content)

# Counting lines, characters and words

textDesc <- function(txt) {
    lines <- length(txt)
    words <- wordcount(txt)
    avgChar <- mean(nchar(txt))
    avgWords <- words / lines
    c(lines = lines, words = words, avgChar = avgChar, avgWords = avgWords)
}

rbind(c(title = collection[[1]]$meta$id, textDesc(collection[[1]]$content)),
      c(title = collection[[2]]$meta$id, textDesc(collection[[2]]$content)),
      c(title = collection[[3]]$meta$id, textDesc(collection[[3]]$content)))

# Sampling to save memory and computing time
set.seed(2137)

blogs_sample <- sample(collection[[1]]$content, 1000)
news_sample <- sample(collection[[2]]$content, 1000)
twitter_sample <- sample(collection[[3]]$content, 1000)
collection <- VCorpus(VectorSource(list(blogs_sample,
                                        news_sample,
                                        twitter_sample)))


# Cleaning


collection <- tm_map(collection, removeWords, stopwords())
collection <- tm_map(collection, removePunctuation)
collection <- tm_map(collection, stripWhitespace)
