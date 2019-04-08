library(dplyr)
library(tidytext)
library(tidyr)

read_txt_tibble <- function(path = file.choose(), encoding = "UTF-8") {
    #interactively reads txt files into tibbles
    vector <- readLines(path, encoding = encoding)
    tibble <- tibble(line = 1:length(vector), text = vector)
    tibble
}

blogs <- read_txt_tibble("C:\\Users\\jakub.wiatrak\\Desktop\\final\\en_US\\en_US.blogs.txt")
news <- read_txt_tibble("C:\\Users\\jakub.wiatrak\\Desktop\\final\\en_US\\en_US.news.txt")
twitter <- read_txt_tibble("C:\\Users\\jakub.wiatrak\\Desktop\\final\\en_US\\en_US.twitter.txt")


#merging data
data <- bind_rows(mutate(blogs, source = "blogs"),
                  mutate(news, source = "news"),
                  mutate(twitter, source = "twitter"))
rm(blogs, news, twitter)

##### Sampling #####

set.seed(2137)
data <- sample_frac(data, size = 0.2)

#####

#Cleaning
#Removing non-ASCII characters, then stripping whitespace

data <- data %>%
    mutate(text =  gsub("[^\x20-\x7E]", "", text)) %>%
    mutate(text = gsub("[ ]+", " ", text))


#tokenizing into sentences
#we do that in order to avoid trigrams spanning more than 1 sentence

sentences <- data %>%
    unnest_tokens(sentence, text, token = "sentences")
rm(data)

#cleaning punctuation and numbers and NAs
sentences <- sentences %>%
    mutate(sentence = gsub("[^a-z ']","", sentence)) %>%
    mutate(sentence = gsub("[ ]+", " ", sentence)) %>%
    mutate(sentence = gsub(" +$", "", sentence))


#tokenizing into n-grams (up to 3) and converting it into TDMs
unigrams <- sentences %>%
    unnest_tokens(word_1, sentence, token = "words") %>%
    filter(!is.na(word_1)) %>%
    group_by(word_1) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n > 3)



bigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 2) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n > 3) %>%
    separate(phrase, into = c("word_1", "word_2"), sep = " ")


trigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 3) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n > 3) %>%
    separate(phrase, into = c("word_1", "word_2", "word_3"), sep = " ")


SBOdata <- list(unigrams, bigrams, trigrams)

save(SBOdata, file = "C:\\Users\\jakub.wiatrak\\Desktop\\final\\en_US\\SBOdata")



