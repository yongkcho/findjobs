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


