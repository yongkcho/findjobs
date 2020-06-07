# import library
library(KoNLP)
library(dplyr)
library(tm)
library(tidyverse)

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


# make corpus

# define extract noun function
exNouns = function(x) { 
  paste(extractNoun(as.character(x)), collapse=" ")
}

txt_nouns <- sub_df$description
txt_nouns <- sapply(myCorpus, exNouns)
myCorpus <- Corpus(VectorSource(txt_nouns))

myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)

tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(4,Inf)))
tdm <- as.data.frame(as.matrix(tdm))

# check top 100 words 
wordResult = sort(rowSums(tdm), decreasing=TRUE)  
wordResult[1:100]

# change some words
txt_nouns <- str_replace_all(txt_nouns, "데이터를", "데이터") 
txt_nouns <- str_replace_all(txt_nouns, "data", "데이터") 
txt_nouns <- str_replace_all(txt_nouns, "기반의", "기반") 
txt_nouns <- str_replace_all(txt_nouns, "기반으로", "기반") 
txt_nouns <- str_replace_all(txt_nouns, "능력의", "능력") 
txt_nouns <- str_replace_all(txt_nouns, "전세계의", "전세계") 
txt_nouns <- str_replace_all(txt_nouns, "경험이", "경험") 
txt_nouns <- str_replace_all(txt_nouns, "경험을", "경험") 
txt_nouns <- str_replace_all(txt_nouns, "통한", "통해")
txt_nouns <- str_replace_all(txt_nouns, "플랫폼을", "플랫폼")

# set Stopwords
myStopwords <- c("있는", "있습니다", "또는", "있도록", "우리의", "있으신")

# make tdm again


