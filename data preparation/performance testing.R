data <- data %>%
    sample_n(size = 1000)


sample_first_words <- function(str) {
    words <- strsplit(str, split = " ")[[1]]
    l <- length(words)
    length_of_sample <- round(runif(1, min=1, max=l), 0)
    paste(words[1:length_of_sample], collapse = " ")
}

samp <- sapply(data$text, sample_first_words)


load("app/TextPredApp/SBOdata")

times <- numeric()

for(s in samp) {
    before <- Sys.time()
    predict_from_text(s)
    times <- c(times, Sys.time() - before)
}

library(ggplot2)

df <- data.frame(time = times * 1000)

# Histogram with density plot
ggplot(df, aes(x=time)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Time in miliseconds") +
    xlim(0,150) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size=20))
