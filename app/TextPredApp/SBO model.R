library(dplyr)
load(file = "SBOdata")

cleanText <- function(str) {
    #converts toLower, leaves only letters, strips whitespaces
    
    out <- tolower(str)
    out <- gsub("[^a-z ']","", out)
    out <- gsub("[ ]+", " ", out)
    out <- gsub(" +$", "", out)
    return(out)
}

getWords <- function(str, n = 3) {
    # gets the last n words from a text input
    v <- strsplit(str, split = " ")[[1]]
    if (length(v) < n) {
        out <- v
    } else {
        out <- v[(length(v) - n + 1):length(v)]
    }
    out
}

matchNgrams <- function(pre, data = SBOdata) {
    #pre - one or more words (as a char vector) (from getWords())
    words <- pre[pre != "" & !is.na(pre)]
    n <- length(words) + 1
    matched <- data[[n]]
    
    if(n>1) {
        for(i in 1:length(words)) {
            word <- words[i]
            matched <- filter(matched, matched[[i]] == word)
        }
    }
    out <- matched[,(ncol(matched)-1):ncol(matched)]
    names(out)[1] <- "word"
    out
}

getNmatches <- function(phrase, data = SBOdata) {
    #for length 0 and 1 it will return the same thing
    words <- phrase[phrase != "" & !is.na(phrase)]
    
    if(length(words)==0) {
        matched <- data[[1]]
    } else {
        matched <- data[[length(words)]]
        for(i in 1:length(words)) {
            word <- words[i]
            matched <- filter(matched, matched[[i]] == word)
        }
    }
    sum(matched$n, na.rm = T)
}

SBOpredict <- function(pre, lambda = 0.5, data = SBOdata) {
    #predicts the next word using a SBO - Stupid Back Off
    #returns 3 most probable words
    #pre - preceding words (from getWords function)
    #lambda - a back-off parameter
    
    len <- length(pre)
    
    #initializing an empty tibble
    predictions <- tibble(word = character(), score = numeric())
    
    # backs off, starting with using all words in pre
    # through using -1 word, finishing on unigrams
    for(i in 1:(len + 1)) {
        ngram <- pre[i:(len + 1)]
        occur <- getNmatches(ngram)
        
        if(occur > 0) {
            new_preds <- matchNgrams(ngram) %>%
                mutate(score = (n / occur) * (lambda^(i-1))) %>%
                select(-n) %>%
                top_n(3, score)
            predictions <- bind_rows(predictions, anti_join(new_preds, predictions, by="word"))
        }
    }
    
    top_n(predictions, 3, wt=score)
    
}


predict_from_text <- function(text) {
    input <- cleanText(text)
    input <- getWords(input)
    SBOpredict(input)
}



