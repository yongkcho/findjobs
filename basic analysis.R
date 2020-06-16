# import library
library(KoNLP)
library(dplyr)
library(tm)
library(tidytext)
library(tidyverse)
library(stringi)
library(stringr)
library(slam)
library(topicmodels)
library(lda)
library(Rmpfr)
library(LDAvis)

library(ggplot2)
library(igraph)
library(qgraph)
library(corrplot) 
library(wordcloud)

# set directory
directory <- "C:/Users/ykun9/findjobs/1.data_acquisition/result"
setwd(directory)

# set dictionary
useSejongDic()


#### merge dataset #####
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

#### pre-process data ####

# qualification
all_df$qualification_1[is.na(all_df$qualification_1)] <- ""
all_df$qualification_2[is.na(all_df$qualification_2)] <- ""

all_df$qualification <- paste(all_df$qualification_1, all_df$qualification_2, sep = " ")
all_df$qualification <- str_replace_all(all_df$qualification, "↑", "이상")

all_df$education_level <- ""
all_df$career_level <- ""

# education level
for(i in 1:nrow(all_df)){
  if(nchar(all_df$qualification[i]) == 1){all_df$education_level[i] <- ""}
  else if(grepl("학력무관", all_df$qualification[i])){all_df$education_level[i] <- "학력무관"}
  else if(grepl("고졸", all_df$qualification[i])){all_df$education_level[i] <- "고졸"}
  else if(grepl("대졸", all_df$qualification[i])){all_df$education_level[i] <- "대졸"}
  else if(grepl("석사", all_df$qualification[i])){all_df$education_level[i] <- "석사"}
  else if(grepl("박사", all_df$qualification[i])){all_df$education_level[i] <- "박사"}
}
all_df$education_level %>% table()

# career level
for(i in 1:nrow(all_df)){
  if(nchar(all_df$qualification[i]) == 1){all_df$education_level[i] <- ""}
  else if(grepl("경력무관", all_df$qualification[i])){all_df$career_level[i] <- "학력무관"}
  else if(grepl("고졸", all_df$qualification[i])){all_df$career_level[i] <- "고졸"}
  else if(grepl("초대졸", all_df$qualification[i])){all_df$career_level[i] <- "초대졸"}
  else if(grepl(paste0(seq(1,3), collapse = "|"), all_df$qualification[i])){all_df$career_level[i] <- "1 ~ 3년"}
  else if(grepl(paste0(seq(4,7), collapse = "|"), all_df$qualification[i])){all_df$career_level[i] <- "4 ~ 7년"}
  else if(grepl(paste0(seq(8,10), collapse = "|"), all_df$qualification[i])){all_df$career_level[i] <- "8 ~ 10년"}
  else if(grepl(paste0(seq(11,25), collapse = "|"), all_df$qualification[i])){all_df$career_level[i] <- "10년 이상"}
}
all_df$career_level %>% table()

all_df$avg_salary <- ""
all_df$min_salary <- ""
all_df$max_salary <- ""

# salary pre-processing
for(i in 1:nrow(all_df)){
  if(grepl("연봉",all_df$type[i])){
    temp_salary <- all_df$type[i]
    temp_salary <- str_remove_all(all_df$type[i], paste(c("연봉", "원", "," ),collapse = "|"))
    temp_salary <- str_split(temp_salary, "-")
    if(length(temp_salary[[1]]) == 2){
      all_df$min_salary[i] <- temp_salary[[1]][1] %>% trimws()
      all_df$max_salary[i] <- temp_salary[[1]][2] %>% trimws()
    } else {
      all_df$avg_salary[i] <- temp_salary[[1]][1]
      all_df$min_salary[i] <- all_df$avg_salary[i]
      all_df$max_salary[i] <- all_df$avg_salary[i]
    }
  }
}
all_df$max_salary <- as.integer(all_df$max_salary) / 10000
all_df$min_salary <- as.integer(all_df$min_salary) / 10000
all_df$max_salary[is.na(all_df$max_salary)] <- 0
all_df$min_salary[is.na(all_df$min_salary)] <- 0
all_df$avg_salary <- (all_df$min_salary + all_df$max_salary) / 2 

# job type pre-processing
# 정규직 or 계약직
all_df$job_type <- ""
for(i in 1:nrow(all_df)){
  if(grepl("계약직", all_df$type[i])){
    all_df$job_type[i] <- "계약직"
  } else if(grepl("정규직", all_df$type[i])) {
    all_df$job_type[i] <- "정규직"
  }
}
table(all_df$job_type)

#### make subset ####
# chk search_word
all_df$search_word %>% table()

sub_df <- filter(all_df, search_word == "데이터분석")
sub_df <- sub_df[!duplicated(sub_df),]

#### basic ananlysis #####
company_count <- sub_df$company %>% table() 
mean(company_count)
freq_co <- company_count[company_count > 10] %>% as.data.frame()
colnames(freq_co) <- c("company", "freq")
freq_co <- freq_co[order(freq_co$freq, rev(freq_co$company), decreasing = TRUE), ]
freq_co$company <- factor(freq_co$company, levels = rev(freq_co$company))

ggplot(freq_co, aes(x = company, y = freq)) + 
  geom_bar(stat = "identity", fill = "gray", colour="black") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Company Freq Bar Chart")  +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

#job_type
table(sub_df$job_type)

table(sub_df$education_level)

# career_level plot
career_level <- table(sub_df$career_level) %>% as.data.frame()
career_level <- career_level[career_level$Var1 != "",]
career_level$Var1 <- factor(career_level$Var1, levels = rev(c("8 ~ 10년",  "4 ~ 7년", "1 ~ 3년", "초대졸" , "고졸",  "학력무관")))

ggplot(career_level, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = Var1)) +
  #geom_bar(stat = "identity", fill = Var1, colour="black") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "career_level Line Chart")  +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

# education_level plot
education_level <- table(sub_df$education_level) %>% as.data.frame()
education_level <- education_level[education_level$Var1 != "",]
education_level$Var1 <- factor(education_level$Var1, levels = rev(c("박사",  "석사", "대졸", "고졸" , "학력무관")))

ggplot(education_level, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = Var1)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "education_level Line Chart")  +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))



# salary box plot
salary <- subset(sub_df, select = c("avg_salary", "career_level", "education_level", 
                                    "job_type", "max_salary", "min_salary","type"))
salary <- salary[salary$avg_salary != 0,]
summary(salary)

ggplot(salary, aes(x=avg_salary)) + 
  geom_histogram(color="black", fill="gray")+
  labs(title="Salary Histogram",x = "연봉" , y = "count")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

salary_re <- rbind(data.frame(type = "min", salary = salary$min_salary),
                   data.frame(type = "max", salary = salary$max_salary))

ggplot(salary_re, aes(x =  , y = salary)) + 
  geom_boxplot(fill="gray")+
  labs(title="Salary Box  & Jitter Plot",x = "연봉" , y = "만원")+
  #geom_jitter(width = 0.3, alpha = 0.3, size = 1.5, aes(x = 1.15) ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))  
  

# chk platform proportion
platform <- as.data.frame(table(sub_df$platform))
colnames(platform) <- c("platform", "count")
platform <- platform[order(platform$count, decreasing = TRUE),]
platform$platform <- factor(platform$platform, levels = rev(as.character(platform$platform)))

ggplot(platform, aes(x = "", y = count, fill = platform)) + 
  geom_bar(width = 1, size = 1 ,stat = "identity", color = "white") +
  coord_polar("y") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "platform share")  +
  geom_text(aes(label = paste0(count,"건")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

#### text analysis ####
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
txt_nouns <- sapply(txt_nouns, exNouns_sim)



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
tfIdf_df <- as.data.frame(as.matrix(tfIdf))
tfIdfResult <- sort(rowSums(tfIdf_df), decreasing=TRUE)  

tfIdf_df <- data.frame(word = names(tfIdfResult), tfIdf = tfIdfResult)

findAssocs(tfIdf, "데이터", 0.1) #단어 상관관계 분석

# coocuerence network based on tf_idf
tf_idf.mat = as.matrix(tfIdf_df) #매트릭스로 변환
word.count <- rowSums(tf_idf.mat)
word.order <- order(word.count, decreasing=T)
rownames(tf_idf.mat)[word.order[1:200]]
freq.words <- tf_idf.mat[word.order[1:200], ]

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
threshold <- quantile(mean_list)
threshold <- threshold[4]


plot <- qgraph(co.matrix, labels=colnames(co.matrix),
               diag=FALSE, layout='spring',
               label.cex = 1, label.scale = F,
               threshold = mean_value,
               vsize=5)#log(diag(co.matrix))*5

# centrality
centrality <- centrality(plot, weighted = FALSE)

# convert to igraph to calculate eigen vector 
plot_igraph <- as.igraph(plot)
eigen <-eigen_centrality(plot_igraph)
bonacich <- power_centrality(plot_igraph)

centrality <- data.frame(no =  names(pbs), 
                         betweeness = round(centrality$betweeness, digits = 2),
                         outdegree = round(centrality$outdegree, digits = 2),
                         indegree = round(centrality$indegree, digits = 2),
                         eigenvector = round(eigen$vector, digits = 2),
                         bonacich = round(bonacich, digits = 2))

# plot aigain with bonacich centrality
plot <- qgraph(co.matrix, labels=colnames(co.matrix),
               diag=FALSE, layout='spring',
               label.cex = 1, label.scale = F,
               threshold = threshold,
               vsize= centrality$bonacich * -5)#log(diag(co.matrix))*5

####3. LDA text classifier####
dtm <- DocumentTermMatrix(myCorpus, control=list(wordLengths=c(4,Inf),
                                                weighting = weightTf))
row_total <- apply(dtm, 1, sum)
dtm <- dtm[row_total > 0, ]
q_lda <- LDA(dtm, k = 2, seed = 1208)
q_topics <- tidy(q_lda, matrix="beta")

q_top_terms <- q_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

q_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  theme(axis.text.y=element_text(family="Apple SD Gothic Neo"))

beta_spread <- q_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio=log(topic2 / topic1))

bind_rows(beta_spread %>% top_n(-10, log_ratio), beta_spread %>% top_n(10, log_ratio)) %>%
  ggplot(aes(reorder(term, log_ratio), log_ratio)) +
  geom_col(show.legend=FALSE) +
  labs(x="term", y="Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip() +
  theme(axis.text.y=element_text(family="Apple SD Gothic Neo"))

q_documents <- tidy(q_lda, matrix="gamma")
q_documents

tidy(dtm) %>%
  filter(document == 8) %>%
  arrange(desc(count))


# k값 정하기
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec=precision) + llMed))))
}

seqk <- seq(2, 40, 1)
burnin <- 1000
iter <- 1000
keep <- 50

fitted_many <- lapply(seqk, function(k) LDA(dtm, k=k, method="Gibbs", control=list(burnin=burnin, iter=iter, keep=keep)))

logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) +
  geom_path(lwd=1.5) +
  theme(text=element_text(family=NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  ggplot2::annotate("text", x=9, y=-199000, label=paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  labs(title="Latent Dirichlet Allocation Analysis",
       subtitle="How many distinct topics?")

q_model <- LDA(dtm, k=34, method="Gibbs", control=list(iter=2000))

q_topics <- topics(q_model, 1)
q_terms <- as.data.frame(terms(q_model, 20), stringsAsFactors=FALSE)
q_terms[1:34] %>% View()

K <- 34
G <- 5000
alpha <- 0.02

fit <- LDA(dtm, k=K, method='Gibbs', control=list(iter=G, alpha=alpha))

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

options(encoding = 'utf-8')
serVis(topicmodels2LDAvis(fit), open.browser=TRUE, encoding = "utf-8")
#out.dir='2017-08-15-complaint-vis'