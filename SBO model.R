library(dplyr)

load(file = "C:\\Users\\jakub.wiatrak\\Desktop\\final\\en_US\\SBOdata")

cleanText <- function(str) {
    #converts toLower, leaves only letters, strips whitespaces
    
    out <- tolower(str)
    out <- gsub("[^a-z ']","", out)
    out <- gsub("[ ]+", " ", out)
    out <- gsub(" +$", "", out)
    return(out)
}

getWords <- function(str, maxN = 3) {
    # gets the last N-1 words from a text input
    v <- strsplit(str, split = " ")[[1]]
    out <- v[(length(v) - maxN + 2):length(v)]
    out
}

matchNgrams <- function(pre, data = SBOdata) {
    #pre - one or more words (as a char vector) (from getWords())
    words <- pre[pre != ""]
    n <- length(words) + 1
    matched <- data[[n]]
    
    if(n>1) {
        for(i in 1:length(words)) {
            word <- words[i]
            matched <- filter(matched, matched[[i]] == word)
        }
    }
    matched[,(ncol(matched)-1):ncol(matched)]
}

getNmatches <- function(phrase, data = SBOdata) {
    #for length 0 and 1 it will return the same thing
    words <- phrase[phrase != ""]
    
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

SBOpredict <- function(pre, lambda = 0.4, data = SBOdata) {
    #predicts the next word using a SBO - Stupid Back Off
    #returns 3 most probable words
    #pre - preceding words (from getWords function)
    #lambda - a back-off parameter
    
    
    # backs off, starting with using all words in pre
    # through using -1 word, finishing on unigrams
    for(i in length(pre):0) {
        ngram <- pre[0:i]
        matchNgrams(ngram)
    }
    
    
    
}
