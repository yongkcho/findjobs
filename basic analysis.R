# import library
library(KoNLP)
library(dplyr)
library(tm)
library(wordcloud)
library(tidyverse)
library(qgraph)
library(corrplot) 
library(stringr)

# set directory
directory <- "C:/Users/ykun9/findjobs/1.data_acquisition/result"
setwd(directory)

# set dictionary
useSejongDic()


# load and combine dataset
all_df <- NULL
file_list <- list.files(directory)

for(file in file_list){
  temp_df <- readxl::read_xlsx(file)
  temp_df <- subset(temp_df, select = c("search_word",	"platform",	"title",	
                                        "url",	"company",	"end_date",	"location",
                                        "description", "type",	"qualification_1",
                                        "qualification_2",	"hashtags"))
  all_df <- rbind(all_df, temp_df)
  message(file, " is merged")
}

## make subset
# chk search_word
all_df$search_word %>% table()

sub_df <- filter(all_df, search_word == "데이터분석")
sub_df <- sub_df[!duplicated(sub_df),]

# make corpus

# define extract noun function
exNouns = function(x) { 
  paste(extractNoun(as.character(x)), collapse=" ")
}

txt_nouns <- sub_df$description
txt_nouns <- sapply(txt_nouns, exNouns)

myCorpus <- Corpus(VectorSource(txt_nouns))

myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)

tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(4,Inf)))
tdm <- as.data.frame(as.matrix(tdm))

# check top 100 words 
wordResult = sort(rowSums(tdm), decreasing=TRUE)  
wordResult[1:100]

ff.all<-tm_map(ff.all, content_transformer(function(x) gsub(x, pattern = "free", replacement = "freedom")))

# change some words
myCorpus <- tm_map(myCorpus, content_transformer(function(x) gsub(x, pattern = "분석을", replacement = "분석")))


myCorpus <- tm_map(myCorpus, gsub, 
               pattern = "data", replacement = "데이터", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "데이터를", replacement = "데이터", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "등비즈니스", replacement = "비즈니스", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "분석을", replacement = "분석", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "통해", replacement = "통한", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "전세계의", replacement = "전세계", fixed=TRUE)
myCorpus <- tm_map(myCorpus, gsub, 
                   pattern = "경험을", replacement = "경험", fixed=TRUE)

# set Stopwords
myStopwords <- c("있는", "있습니다", "또는", "있도록", "우리의", "있으신", "등의", "다양한", "기반",
                 "넣습니다","하신")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


# make tdm again
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(4,Inf)))
tdm <- as.data.frame(as.matrix(tdm))

# check top 100 words 
wordResult = sort(rowSums(tdm), decreasing=TRUE)  
#wordResult[1:100]

# Wordcloud
word_df <- data.frame(word = names(wordResult), freq = wordResult)
wordcloud(word_df$word, word_df$freq, 
          scale = c(5,1), min.freq = 3, random.order = F, use.r.layout = T,
          rot.per = 0)


# tf-idf 
tfIdf <- TermDocumentMatrix(myCorpus, 
                             control=list(wordLengths=c(4,30), weighting=weightTfIdf))
tfIdf <- as.data.frame(as.matrix(tfIdf))
tfIdfResult <- sort(rowSums(tfIdf), decreasing=TRUE)  

tfIdf_df <- data.frame(word = names(tfIdfResult), tfIdf = tfIdfResult)


# coocuerence network based on tf_idf
tf_idf.mat = as.matrix(tfIdf) #매트릭스로 변환
word.count <- rowSums(tf_idf.mat)
word.order <- order(word.count, decreasing=T)
rownames(tf_idf.mat)[word.order[1:50]]
freq.words <- tf_idf.mat[word.order[1:50], ]

co.matrix <- freq.words %*% t(freq.words)
co.matrix <- co.matrix[-30,-30]#湲곕え吏�문제해결

co_ocuerence <- co.matrix %>% as.data.frame() #save as dataframe for later
mean_list <- NULL
for(i in 1:nrow(co_ocuerence)){
  temp_for_calculte <- co_ocuerence[i,] %>% unname() %>% unlist()
  temp_for_calculte <- temp_for_calculte[temp_for_calculte != 0]
  mean_list[i] <- mean(temp_for_calculte) 
}
mean_value <- mean(mean_list)
mean_value

plot <- qgraph(co.matrix, labels=colnames(co.matrix),
               diag=FALSE, layout='spring',
               label.cex = 1, label.scale = F,
               threshold = mean_value,
               vsize=5)#log(diag(co.matrix))*5

# centrality
centrality <- centrality(plot, weighted = FALSE)
pbs <- centrality$Betweenness/centrality$OutDegree
pbs[is.nan(pbs)] <- 0
centrality <- data.frame(no =  names(pbs), pbs = round(pbs, digits = 2), 
                         betweeness = round(centrality$Betweenness, digits = 2))
View(centrality) #to add on ppt



