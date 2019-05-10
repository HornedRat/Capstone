load("SBOdata")
library(lexicon)

data("profanity_banned")


#profanity filtering
for(i in 1:length(SBOdata)) {
    forDelete <- SBOdata[[i]][,i] %in% profanity_banned
    print(sum(SBOdata[[i]][,i] %in% profanity_banned))
    SBOdata[[i]] <- SBOdata[[i]][!forDelete,]
}

save(SBOdata, file = "SBOdata")
