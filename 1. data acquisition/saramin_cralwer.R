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
  
  message(i, "th search word meta data of ", keyword[i], " begin at saramin")
  
  #move to search result 
  search_url <- paste0("http://www.saramin.co.kr/zf_user/search/recruit?search_area=main&search_done=y&search_optional_item=n&searchType=search&searchword=", 
                     keyword[i], "&loc_mcd=101000&recruitPage=",
                       1,
                       "&recruitSort=relation&recruitPageCount=100&inner_com_type=&company_cd=0,1,2,3,4,5,6,7,9&quick_apply=")
  remDr$navigate(search_url)  
  Sys.sleep(0.8)
  
  #check result count 
  temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
  result_count <- temp %>% html_node(".cnt_result") %>% html_text() %>%
    str_remove("총") %>%
    str_remove(",") %>%
    str_remove("건") %>%
    as.numeric()
  message(result_count, " result appear")
  
  for(j in 1:ceiling(result_count / 100)){
    search_url <- paste0("http://www.saramin.co.kr/zf_user/search/recruit?search_area=main&search_done=y&search_optional_item=n&searchType=search&searchword=", 
                         keyword[i], "&loc_mcd=101000&recruitPage=",
                         j,
                         "&recruitSort=relation&recruitPageCount=100&inner_com_type=&company_cd=0,1,2,3,4,5,6,7,9&quick_apply=")
    remDr$navigate(search_url)  
    Sys.sleep(0.8)
    
    temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
    
    title <- temp %>% html_nodes(".job_tit") %>% html_text() %>% 
      str_remove_all("\n") %>% trimws() #pre-process results. 
    url <- temp %>% html_nodes(".job_tit a") %>% html_attr("href")
    end_date <- temp %>% html_nodes(".job_date") %>% html_text() %>%
      gsub(pattern = "\\s",   replacement = "") #remove space
    
    qualification_1 <- temp %>% html_nodes(".job_condition span:nth-child(2)") %>% html_text()
    qualification_2 <- temp %>% html_nodes(".job_condition span:nth-child(3)") %>% html_text()
    type <- temp %>% html_nodes(".job_condition span:nth-child(4)") %>% html_text()
    
    location <- temp %>% html_nodes(".job_condition span:nth-child(1)") %>% html_text()
    company <- temp %>% html_nodes(".corp_name a span") %>% html_text()
    
    temp_df <- data.frame(search_word = keyword[i],
                          platform = "saramin",
                          title = title, url = url, company = company, 
                          end_date = end_date, type = type ,location = location,
                          qualification_1 = qualification_1, qualification_2 = qualification_2,
                          company = company)
    all_df <- rbind(all_df, temp_df)
  }
  
  message(i, "th search word meta data of ", keyword[i], " end at saramin")
}

#crawl main contents
all_df$url <- as.character(paste0("http://www.saramin.co.kr", all_df$url))
all_df$type <- as.character(all_df$type)
contents_df <- NULL

for(j in 1:length(all_df$url)){
  remDr$navigate(all_df$url[j]) 
  Sys.sleep(0.8)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
  
  type <- temp %>% html_node(".col > dl:nth-child(3) > dd") %>% html_text() %>% trimws()
  payment <- temp %>% html_node(".cont > div:nth-child(2) > dl:nth-child(1) > dd") %>% html_text() %>% trimws()
  position <- temp  %>% html_node(".cont > div:nth-child(2) > dl:nth-child(2) > dd") %>% html_text() %>% trimws()
  hours <- temp  %>% html_node(".cont > div:nth-child(2) > dl:nth-child(3) > dd") %>% html_text() %>% trimws() 
  location <- temp %>% html_node(".cont > div:nth-child(2) > dl:nth-child(4) > dd") %>% html_text() %>% 
  str_remove_all("지도") %>% trimws()
  
  # avg_year <- temp %>% html_node(".chart_line") %>% html_nodes(".value") %>% html_text() %>% as.numeric()
  # avg_year <- round(value[1] * 0 + value[2] * 1 + value[3] * 2.5 + value[4] * 4.5 + value[5] * 7 / sum(value), digits = 2)

  hashtags  <- temp %>% html_node(".tags") %>% html_text() %>% str_remove("더보기")
  view_count <- temp %>% html_node(".meta li strong") %>% html_text()
  applicant <- temp %>% html_node(".total dd span") %>% html_text()
  
  temp_df <- data.frame(url = all_df$url[i], type = type, payment = payment, position = position,
                        hours = hours, location = location, hashtags = hashtags,
                        view_count = view_count, applicant = applicant)
  contents_df <- rbind(contents_df, temp_df)
  
  if(j %% 100 == 0){message(round(j / length(all_df$url), 2)* 100, "% is done.")}
}

all_df <- cbind(all_df, contents_df)

write_xlsx(all_df, "saramin_crawling_result.xlsx")
