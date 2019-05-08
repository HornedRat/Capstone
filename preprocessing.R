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
data <- sample_frac(data, size = 0.5)

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

#cleaning punctuation and numbers
sentences <- sentences %>%
    mutate(sentence = gsub("[^a-z ']","", sentence)) %>%
    mutate(sentence = gsub("[ ]+", " ", sentence)) %>%
    mutate(sentence = gsub(" +$", "", sentence))

gc()

SBOdata <- list()

#tokenizing into n-grams (up to 3) and converting it into TDMs
unigrams <- sentences %>%
    unnest_tokens(word_1, sentence, token = "words") %>%
    filter(!is.na(word_1)) %>%
    group_by(word_1) %>%
    summarise(n = n()) %>%   
    filter(n > 2) %>%
    arrange(desc(n))
    
SBOdata[[1]] <- unigrams
rm(unigrams)
gc()

bigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 2) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    filter(n > 2) %>%
    arrange(desc(n)) %>%
    separate(phrase, into = c("word_1", "word_2"), sep = " ")

SBOdata[[2]] <- bigrams
rm(bigrams)
gc()

trigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 3) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n > 2) %>%
    separate(phrase, into = c("word_1", "word_2", "word_3"), sep = " ")

SBOdata[[3]] <- trigrams
rm(trigrams)
gc()

quadrigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 4) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    filter(n > 2) %>%
    arrange(desc(n)) %>%
    separate(phrase, into = c("word_1", "word_2", "word_3", "word_4"), sep = " ")

SBOdata[[4]] <- quadrigrams
rm(quadrigrams)

quintigrams <- sentences %>%
    unnest_tokens(phrase, sentence, token = "ngrams", n = 5) %>%
    filter(!is.na(phrase)) %>%
    group_by(phrase) %>%
    summarise(n = n()) %>%
    filter(n > 3) %>%
    arrange(desc(n)) %>%
    separate(phrase, into = c("word_1", "word_2", "word_3", "word_4", "word_5"), sep = " ")

SBOdata[[5]] <- quintigrams
rm(quintigrams)

save(SBOdata, file = "SBOdata")


