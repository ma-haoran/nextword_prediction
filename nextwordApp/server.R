


library(tidyverse)
library(tidytext)
library(data.table)

all<-read_lines("train_data.csv")
all<-tibble(all)
colnames(all) <- "text"


## build ngram corpus
names <- function(x) {
    num <- seq_along(1:x)
    as.vector(paste("word", num, sep = ""))
}

break_text <- function(data, n) {
    corpus <- data %>%
        unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
        data.table() #data.table is faster for further process
    
    corpus[, names(n) :=
               tstrsplit(ngram, " ")][, ngram := NULL]
    
}

#stop<-data.table(stop_words)
all5 <- break_text(all, 5)



## process input
preprocess <- function(input) {
    input <- tolower(input) %>%
        tibble()
    colnames(input) <- "input"
    input <- input %>%
        unnest_tokens(input, input) %>%
        # remove stopwords
        # anti_join(stop_words,by=c("input"="word"))%>%
        data.table()
    input
}


## match grams and predict
nextword2 <- function(input, x) {
    predictor <- preprocess(input)
    
    matched <- all5[which(all5[, 4] == predictor[.N, input])]
    matched <- matched[which(matched[, 3] == predictor[.N - 1, input])]
    matched <- matched[which(matched[, 2] == predictor[.N - 2, input])]
    matched <- matched[which(matched[, 1] == predictor[.N - 3, input])]
    if (sum(is.na(matched)) == 0) {
        matched <- all5[which(all5[, 4] == predictor[.N, input])]
        matched <-
            matched[which(matched[, 3] == predictor[.N - 1, input])]
        matched <-
            matched[which(matched[, 2] == predictor[.N - 2, input])]
        if (sum(is.na(matched)) == 0) {
            matched <- all5[which(all5[, 4] == predictor[.N, input])]
            matched <-
                matched[which(matched[, 3] == predictor[.N - 1, input])]
            if (sum(is.na(matched)) == 0) {
                matched <- all5[which(all5[, 4] == predictor[.N, input])]
            }
        }
    }
    
    matched[, .(count = .N), by = word5][order(-count)][1:x, word5]
    
}







library(shiny)


shinyServer(function(input, output) {
    output$text <- renderText({
        nextword2(input$inputtext, input$x)
        
    })
    
})
