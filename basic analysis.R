# import library
library(KoNLP)
library(dplyr)
library(tm)
library(tidyverse)
library(stringr)

library(ggplot2)
library(qgraph)
library(corrplot) 
library(wordcloud)

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

# pre-process data
all_df$qualification <- 




## make subset
# chk search_word
all_df$search_word %>% table()

sub_df <- filter(all_df, search_word == "데이터분석")
sub_df <- sub_df[!duplicated(sub_df),]

# basic ananlysis 
company_count <- sub_df$company %>% table() 
mean(company_count)

#filter(sub_df$type, grepl(paste("계약직", collapse = "|"), content))

sum(str_count(sub_df$type, "정규직"), na.rm = TRUE)
sum(str_count(sub_df$type, "계약직"), na.rm = TRUE)

salary <- grep("연봉", sub_df$type, value = T)#1097  value
salary <- str_remove_all(salary, paste(c("연봉", "원", "," , "\\-"),collapse = "|"))
salary <- strsplit(salary, "\\s{2,}") %>% unlist() %>% trimws()
salary_10000 <- salary / 10000

summary(salary_10000)
hist(salary_10000)
boxplot(salary_10000)

salary_10000 <- data.frame(salary = salary_10000)
ggplot(salary_10000, aes(x =, y = salary)) + 
  geom_boxplot(fill="gray")+
  labs(title="Salary Box  & Jitter Plot",x = "연봉" , y = "만원")+
  geom_jitter(width = 0.3, alpha = 0.3, size = 1.5, aes(x = 1.15) ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

# chk platform proportion
platform <- as.data.frame(table(sub_df$platform))
colnames(platform) <- c("platform", "count")
platform$platform <- factor(platform$platform, levels = rev(as.character(platform$platform)))

ggplot(platform, aes(x = "", y = count, fill = platform)) + 
  geom_bar(width = 1, size = 1 ,stat = "identity", color = "white") +
  coord_polar("y") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "platform share")  +
  geom_text(aes(label = paste0(count,"건")), 
            position = position_stack(vjust = ust = 0.5)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

# qualification

qualification <- c(sub_df$qualification_1[!is.na(sub_df$qualification_1)],
                   sub_df$qualification_2[!is.na(sub_df$qualification_2)])

qualification <- table(qualification) %>% as.data.frame()
qualification$qualification <- as.character(qualification$qualification)

summary(nchar(qualification$qualification))

skillset <- qualification[nchar(qualification$qualification) >=  20,]
qualification <- qualification[nchar(qualification$qualification) <  20,]

qualification$qualification <- str_replace_all(qualification$qualification, "↑", "이상")

# education_level
education_level <- data.frame(
  level = c("학력무관", "고졸", "대졸", "석사", "박사"),
  freq = c(sum(qualification$Freq[grep("학력무관", qualification$qualification)]),
           sum(qualification$Freq[grep("고졸", qualification$qualification)]),
           sum(qualification$Freq[grep("대졸", qualification$qualification)]),
           sum(qualification$Freq[grep("석사", qualification$qualification)]),
           sum(qualification$Freq[grep("박사", qualification$qualification)])))



# career level
career_level <- data.frame(
  level = c("경력무관", "초대졸", "1~3년", "4~7년", "7~10년", "10년 이상"),
  freq = c(sum(qualification$Freq[grep("학력무관", qualification$qualification)]),
           sum(qualification$Freq[grep("초대졸", qualification$qualification)]),
           sum(qualification$Freq[grep(paste0(seq(1,3), collapse = "|"), qualification$qualification)]),
           sum(qualification$Freq[grep(paste0(seq(4,7), collapse = "|"), qualification$qualification)]),
           sum(qualification$Freq[grep(paste0(seq(8,10), collapse = "|"), qualification$qualification)]),
           sum(qualification$Freq[grep(paste0(seq(11,20), collapse = "|"), qualification$qualification)])))

# make corpus
# define extract noun function
exNouns = function(x) { 
  paste(extractNoun(as.character(x)), collapse=" ")
}

# hashtags
hash_nouns <- sub_df$hashtags[!is.na(sub_df$hashtags)]
hash_nouns <- sapply(hash_nouns, exNouns)

hash_corpus <- Corpus(VectorSource(hash_nouns))
hash_corpus <- tm_map(hash_corpus, removePunctuation)
hash_corpus <- tm_map(hash_corpus, removeNumbers)
hash_corpus <- tm_map(hash_corpus, tolower)

hash_tdm <- TermDocumentMatrix(hash_corpus, control=list(wordLengths=c(4,Inf)))
hash_tdm <- as.data.frame(as.matrix(hash_tdm))

# make wordcloud with top 50 hashtags
hash_words <- sort(rowSums(hash_tdm), decreasing=TRUE)  
hash_words <- data.frame(word = names(hash_words), freq = hash_words)
rownames(hash_words) <- c()
to_remove <- c(1, 2, 3, 4, 5, 6, 7, 10, 11, 19, 25, 33, 55)
hash_words <- hash_words[-to_remove,]
wordcloud(hash_words$word[1:50], hash_words$freq[1:50], random.order = F, 
          scale = c(2, 0.5), rot.per = 0)

# description 
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

#ff.all<-tm_map(ff.all, content_transformer(function(x) gsub(x, pattern = "free", replacement = "freedom")))

# change some words
#myCorpus <- tm_map(myCorpus, content_transformer(function(x) gsub(x, pattern = "분석을", replacement = "분석")))


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
rownames(tf_idf.mat)[word.order[1:100]]
freq.words <- tf_idf.mat[word.order[1:100], ]

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



