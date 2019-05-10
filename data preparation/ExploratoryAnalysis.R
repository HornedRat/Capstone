library(tm)
library(dplyr)
library(tidyr)
library(wordcloud)
library(ngram)
library(ggplot2)
library(lexicon)
library(RWeka)
library(gridExtra)

#Choose the 'en_us' folder with data
data.path <- choose.dir()
collection <- VCorpus(DirSource(data.path, encoding = "UTF-8"))
summary(collection)


# Counting lines, characters and words

textDesc <- function(txt) {
    #' returns basic statistics about a text - number of lines,
    #' words, mean characters in a line and mean words in line
    lines <- length(txt)
    words <- wordcount(txt)
    avgChar <- round(mean(nchar(txt)), 2)
    avgWords <- round(words / lines, 2)
    c(lines = lines, words = words, avgChar = avgChar, avgWords = avgWords)
}

rbind(c(title = collection[[1]]$meta$id, textDesc(collection[[1]]$content)),
      c(title = collection[[2]]$meta$id, textDesc(collection[[2]]$content)),
      c(title = collection[[3]]$meta$id, textDesc(collection[[3]]$content)))

# Sampling to save memory and computing time
set.seed(2137)

blogs_sample <- sample(collection[[1]]$content, 10000)
news_sample <- sample(collection[[2]]$content, 10000)
twitter_sample <- sample(collection[[3]]$content, 10000)
collection <- VCorpus(VectorSource(list(blogs_sample,
                                        news_sample,
                                        twitter_sample)))

collection[[1]]$meta$id <- "blogs_sample"
collection[[2]]$meta$id <- "news_sample"
collection[[3]]$meta$id <- "twitter_sample"


# Cleaning


#transforming to all lowercase
collection <- tm_map(collection, content_transformer(tolower))
#removing stopwords
collection <- tm_map(collection, removeWords, stopwords())
#removing profanities, using list of profanities from lexicon package
data("profanity_banned")
collection <- tm_map(collection, removeWords, profanity_banned)
#removing punctuation
collection <- tm_map(collection, removePunctuation, ucp = T)
#removing numbers, as they are less interesting in text mining
collection <- tm_map(collection, removeNumbers)
#removing dollarsigns, a very common character, but useless without numbers
subSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
collection <- tm_map(collection, subSpace, "\\$")
#stripping whitespaces
collection <- tm_map(collection, stripWhitespace)

# Exploration

# most common words - wordclouds
png("blogs.png")
wordcloud(collection[[1]]$content, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))
dev.off()
png("news.png")
wordcloud(collection[[2]]$content, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))
dev.off()
png("twitter.png")
wordcloud(collection[[3]]$content, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))
dev.off()

#Tokenization for n-grams


UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm1 <- TermDocumentMatrix(collection, control = list(tokenize = UnigramTokenizer))
tdm2 <- TermDocumentMatrix(collection, control = list(tokenize = BigramTokenizer))
tdm3 <- TermDocumentMatrix(collection, control = list(tokenize = TrigramTokenizer))

#Unigrams - most common words
blogs_unigrams <- data.frame(
                phrase = names(sort(as.matrix(tdm1)[,1], decreasing = T)[1:20]),
                freq = sort(as.matrix(tdm1)[,1], decreasing = T)[1:20])

news_unigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm1)[,2], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm1)[,2], decreasing = T)[1:20])

twitter_unigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm1)[,3], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm1)[,3], decreasing = T)[1:20])


g1_blogs <- ggplot(blogs_unigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Blogs") +
    coord_flip()

g1_news <- ggplot(news_unigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "News") +
    coord_flip()

g1_twitter <- ggplot(twitter_unigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Twitter") +
    coord_flip()


grid.arrange(g1_blogs, g1_news, g1_twitter, ncol=3)


#Bigrams - most common 2-word phrases
blogs_bigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm2)[,1], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm2)[,1], decreasing = T)[1:20])

news_bigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm2)[,2], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm2)[,2], decreasing = T)[1:20])

twitter_bigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm2)[,3], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm2)[,3], decreasing = T)[1:20])


g2_blogs <- ggplot(blogs_bigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Blogs") +
    coord_flip()

g2_news <- ggplot(news_bigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "News") +
    coord_flip()

g2_twitter <- ggplot(twitter_bigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Twitter") +
    coord_flip()


grid.arrange(g2_blogs, g2_news, g2_twitter, ncol=3)


#Trigrams - most common 3-word phrases
blogs_trigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm3)[,1], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm3)[,1], decreasing = T)[1:20])

news_trigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm3)[,2], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm3)[,2], decreasing = T)[1:20])

twitter_trigrams <- data.frame(
    phrase = names(sort(as.matrix(tdm3)[,3], decreasing = T)[1:20]),
    freq = sort(as.matrix(tdm3)[,3], decreasing = T)[1:20])


g3_blogs <- ggplot(blogs_trigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Blogs") +
    coord_flip()

g3_news <- ggplot(news_trigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "News") +
    coord_flip()

g3_twitter <- ggplot(twitter_trigrams, aes(x = reorder(phrase, freq),y=freq)) +
    geom_bar(stat = "identity") +
    ylab("Freqency") +
    xlab("Phrase") +
    labs(title = "Twitter") +
    coord_flip()


grid.arrange(g3_blogs, g3_news, g3_twitter, ncol=3)


#Attempt at foreign word detection

tdm.df <- data.frame(
            phrase = rownames(as.matrix(tdm1)),
            as.matrix(tdm1)
            )

eng.words <- tdm.df %>%
    mutate(is.english = phrase %in% grady_augmented) %>%
    group_by(is.english) %>%
    summarise(sum_blogs = sum(blogs_sample),
              sum_news = sum(news_sample),
              sum_twitter = sum(twitter_sample)) %>%
    transmute(is.english = is.english,
              Blogs = sum_blogs / sum(sum_blogs),
              News = sum_news / sum(sum_news),
              Twitter = sum_twitter / sum(sum_twitter)) %>%
    gather(key = "Source", value = "Percent", -is.english)

g_english <- ggplot(data = eng.words,
                    aes(x = Source,
                        y = Percent,
                        fill = is.english,
                        label = round(Percent * 100, 0))) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
