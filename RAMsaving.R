#Ngrams - solution for to big ram usage
library(dplyr)
library(tidytext)

lines_at_a_time <- 20000

#NGRAMS

source <- file("sentences_all.txt", "r")
out <- file("unigrams_all.txt", "a")

linesread = lines_at_a_time
linesSoFar = 0

while (linesread >= lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(sentence = t)
    ngrams <- t %>%
        unnest_tokens(phrase, sentence, token = "words") %>%
        filter(!is.na(phrase))
    writeLines(as.character(ngrams$phrase), con = out)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)
close(out)


source <- file("sentences_all.txt", "r")
out <- file("bigrams_all.txt", "a")

linesread = lines_at_a_time
linesSoFar = 0

while (linesread > 0) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(sentence = t)
    ngrams <- t %>%
        unnest_tokens(phrase, sentence, token = "ngrams", n = 2) %>%
        filter(!is.na(phrase))
    writeLines(as.character(ngrams$phrase), con = out)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)
close(out)


source <- file("sentences_all.txt", "r")
out <- file("trigrams_all.txt", "a")

linesread = lines_at_a_time
linesSoFar = 0



while (linesread > 0) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(sentence = t)
    ngrams <- t %>%
        unnest_tokens(phrase, sentence, token = "ngrams", n = 3) %>%
        filter(!is.na(phrase))
    writeLines(as.character(ngrams$phrase), con = out)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)
close(out)


source <- file("sentences_all.txt", "r")
out <- file("quadrigrams_all.txt", "a")

linesread = lines_at_a_time
linesSoFar = 0



while (linesread > 0) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(sentence = t)
    ngrams <- t %>%
        unnest_tokens(phrase, sentence, token = "ngrams", n = 4) %>%
        filter(!is.na(phrase))
    writeLines(as.character(ngrams$phrase), con = out)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)
close(out)


source <- file("sentences_all.txt", "r")
out <- file("quintigrams_all.txt", "a")

linesread = lines_at_a_time
linesSoFar = 0

while (linesread >= lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(sentence = t)
    ngrams <- t %>%
        unnest_tokens(phrase, sentence, token = "ngrams", n = 5) %>%
        filter(!is.na(phrase))
    writeLines(as.character(ngrams$phrase), con = out)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)
close(out)

#TDMs - to nie dzia?a




#count lines
while(linesread == lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

#Unigrams

source <- file("preprocessed files/unigrams_all.txt", "r")
tdm1 <- tibble(phrase = character(), n = numeric())

lines_at_a_time <- 100000
linesread = lines_at_a_time
linesSoFar = 0

while(linesread == lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(phrase = t)
    tdm <- t %>%
        count(phrase)
    tdm1 <- bind_rows(tdm1, tdm)
    linesSoFar <- linesSoFar + linesread
        print(paste("Lines read:", linesSoFar))
}

close(source)

tdm1 <- tdm1 %>%
    count(phrase, wt = n) %>%
    arrange(desc(n))

save(tdm1, file = "preprocessed files/unigrams_all_tdm")

#Bigrams

source <- file("preprocessed files/bigrams_all.txt", "r")
tdm2 <- tibble(phrase = character(), n = numeric())

lines_at_a_time <- 100000
linesread = lines_at_a_time
linesSoFar = 0

while(linesread == lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(phrase = t)
    tdm <- t %>%
        count(phrase)
    tdm2 <- bind_rows(tdm2, tdm)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)

tdm2 <- tdm2 %>%
    count(phrase, wt = n) %>%
    arrange(desc(n))

save(tdm2, file = "preprocessed files/bigrams_all_tdm")

#Trigrams

source <- file("preprocessed files/trigrams_all.txt", "r")
tdm3 <- tibble(phrase = character(), n = numeric())

lines_at_a_time <- 500000
linesread = lines_at_a_time
linesSoFar = 0

while(linesread == lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(phrase = t)
    tdm <- t %>%
        count(phrase)
    tdm3 <- bind_rows(tdm3, tdm)
    linesSoFar <- linesSoFar + linesread
    print(paste("Lines read:", linesSoFar))
}

close(source)

tdm3 <- tdm3 %>%
    count(phrase, wt = n) %>%
    arrange(desc(n))

save(tdm3, file = "preprocessed files/trigrams_all_tdm")


#Temp folder version

source <- file("preprocessed files/trigrams_all.txt", "r")
lines_at_a_time <- 500000
linesread = lines_at_a_time
linesSoFar = 0
chunksCreated = 0

dir.create("temp")

#chunking
while(linesread == lines_at_a_time) {
    t <- readLines(source, lines_at_a_time)
    linesread <- length(t)
    t <- tibble(phrase = t)
    tdm <- t %>%
        count(phrase)
    write.csv(tdm,
              file = paste0("temp/chunk",chunksCreated, ".csv"),
              row.names = F)
    
    linesSoFar <- linesSoFar + linesread
    chunksCreated <- chunksCreated + 1
    print(paste("Lines read:", linesSoFar))
    print(paste("Chunks created:", chunksCreated))
}

close(source)


chunks <- list.files("/temp")


