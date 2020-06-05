####load_library####
library(rvest)
library(dplyr)
library(stringr)
library(RSelenium)
library(writexl)

#initial
remDr = remoteDriver(remoteServerAddr = "localhost",
                     port = 4445L,
                     browserName = "chrome")
remDr$open()

#saramin crawler
keyword <- c("전략기획자", "서비스기획자", "데이터분석", 
             "디지털 마케터", "마케팅 전략 기획자",
             "조직관리전문가", "교육전문가")

#initialize dataframe
all_df <- NULL

#crawl meta data
for(i in 1:length(keyword)){
  
  message(i, "th search word meta data of ", keyword[i], " begin at jobplanet")
  
  search_url <- paste0("https://www.jobplanet.co.kr/job_postings/search?utf8=%E2%9C%93&query=",
                       keyword[i],
                       "&posting_type=&jp_min_salary=&jp_min_recommend=&city_ids%5B%5D=1&order_by=score")
  remDr$navigate(search_url)
  Sys.sleep(1)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  try(result_count <- temp %>% html_node("#city_ids_ul > label:nth-child(2) > span.check_txt") %>% html_text() %>%
        str_remove_all("\n") %>% str_remove_all("서울") %>%
        str_remove_all("\\(") %>% str_remove_all("\\)") %>%
        trimws() %>% as.numeric())
  
  if(is.na(result_count) == TRUE){
    result_count <- temp %>% html_node("#city_ids_ul > label:nth-child(3) > span.check_txt") %>% html_text() %>%
      str_remove_all("\n") %>% str_remove_all("서울") %>%
      str_remove_all("\\(") %>% str_remove_all("\\)") %>%
      trimws() %>% as.numeric()
  }
  
  for(j in 1:ceiling(result_count / 20)){
    remDr$navigate(paste0(search_url, "&page=", j))
    
    temp <- remDr$getPageSource()[[1]] %>% read_html()
    title <- temp %>% html_nodes(".posting_name") %>% html_text()
    url <- temp %>% html_nodes(".unit_head a") %>% html_attr("href")
    company <- temp %>% html_
  }
  
  temp_df <- data.frame(search_word = keyword[i], title = title, url = url)
  all_df <- rbind(all_df, temp_df)
  
  message(i, "th search word meta data of ", keyword[i], " end at jobplanet")
}

#crawl main contents
all_df$url <- as.character(paste0("https://www.jobplanet.co.kr/", all_df$url)) 
contents_df <- NULL

for(j in 1:length(all_df$url)){
  remDr$navigate(all_df$url[j])
  Sys.sleep(0.8)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  company <- temp %>% html_node(".company_name") %>% html_text() %>%
    str_remove_all("\n") %>% trimws()
  company_type <- temp %>% html_node(".info span") %>% html_text()
  end_date <- temp %>% html_node(".duration") %>% html_text()
  hashtags <- temp %>% html_nodes(".tags.fix_height li") %>% html_text() %>%
    gsub(pattern = "\\s",   replacement = "") #remove space
  hashtags <- paste(hashtags, collapse = ", ")
  
  description <- temp %>% html_nodes(".paragraph") %>% html_text() %>% 
    str_remove_all("\n")
  description <- paste(description, collapse = " ")
  
  temp_df <- data.frame(comapany = company, company_type = company_type, end_date = end_date, 
                        hashtags = hashtags, description = description, url = all_df$url[j])
  contents_df <- rbind(contents_df, temp_df)
}

all_df <- cbind(all_df, contents_df)

write_xlsx(all_df, "jobplanet_crawling_result.xlsx")
