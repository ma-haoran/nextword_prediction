## process data


#Dowload file
setwd("C:/Users/Apple/Desktop/RStudio Tour/assignment/assignment10")

if(!file.exists("Coursera-SwiftKey.zip" )){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile ="Coursera-SwiftKey.zip" )
    unzip("Coursera-SwiftKey.zip" )
    
}

#Read file
library(tidyverse)
library(tidytext)
library(data.table)
twitter<-read_lines(file="final/en_US/en_US.twitter.txt",
                    skip_empty_rows = TRUE)

blog<-read_lines(file="final/en_US/en_US.blogs.txt",
                 skip_empty_rows = TRUE)

news<-read_lines(file="final/en_US/en_US.news.txt",
                 skip_empty_rows = TRUE)
set.seed(12345)
twitter<-tibble(twitter)
blog<-tibble(blog)
news<-tibble(news)

twitter<-slice_sample(twitter,prop=0.1)
blog<-slice_sample(blog,prop=0.1)
news<-slice_sample(news,prop=0.1)

colnames(twitter)<-"text"
colnames(blog)<-"text"
colnames(news)<-"text"

all<-rbind(twitter,blog,news)
## remove stopwords
all<-all%>%
    unnest_tokens(text,text)%>%
    anti_join(stop_words,by=c("text"="word"))%>%
    pull()%>%
    paste(collapse = " ")%>%
    tibble()
colnames(all)<-"text"



## build ngram corpus
names<-function(x){
    num<-seq_along(1:x)
    as.vector(paste("word",num,sep=""))
}

break_text<-function(data,n){
    corpus<-data%>%
        unnest_tokens(ngram,text,token = "ngrams",n=n)%>%
        data.table() #data.table is faster for further process
    
    corpus[,names(n):=
               tstrsplit(ngram," ")][,ngram:=NULL]
    
}

stop<-data.table(stop_words)
all5<-break_text(all,5)



## process input
preprocess<-function(input){
    input<-tolower(input)%>%
        tibble()
    colnames(input)<-"input"
    input<-input%>%
        unnest_tokens(input,input)%>%
        ## remove stopwords
        anti_join(stop_words,by=c("input"="word"))%>%
        data.table()
    input 
}


## match grams and predict
nextword2<-function(input,x){
    predictor<-preprocess(input)
    
    matched<-all5[which(all5[,4] == predictor[.N,input])]
    matched<-matched[which(matched[,3] == predictor[.N-1,input])]
    matched<-matched[which(matched[,2] == predictor[.N-2,input])]
    matched<-matched[which(matched[,1] == predictor[.N-3,input])]
    if(sum(is.na(matched))==0){
        matched<-all5[which(all5[,4] == predictor[.N,input])]
        matched<-matched[which(matched[,3] == predictor[.N-1,input])]
        matched<-matched[which(matched[,2] == predictor[.N-2,input])]
        if(sum(is.na(matched))==0){
            matched<-all5[which(all5[,4] == predictor[.N,input])]
            matched<-matched[which(matched[,3] == predictor[.N-1,input])]
            if(sum(is.na(matched))==0){
                matched<-all5[which(all5[,4] == predictor[.N,input])]
            }}}
    
    matched[!stop,on=c("word5"="word")][,.(count=.N),by=word5][order(-count)][1:x,word5]
    
}

