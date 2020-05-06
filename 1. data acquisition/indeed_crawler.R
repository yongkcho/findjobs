####load_library####
library(rvest)
library(dplyr)
library(stringr)
library(RSelenium)
library(stringr)
library(writexl)

#initial
remDr = remoteDriver(remoteServerAddr = "localhost",
                     port = 4445L,
                     browserName = "chrome")
remDr$open()

#Indeed data crawling 
keyword <- c("전략기획자", "서비스기획자", "데이터분석", 
             "디지털 마케터", "마케팅 전략 기획자",
             "조직관리전문가", "교육전문가")

#initialize dataframe
all_df <- NULL

#crawl meta data
for(i in 1:length(keyword)){
  
  message(i, "th search word meta data of ", keyword[i], " begin at indeed")
  
  #move to search result 
  search_url <- paste0("https://kr.indeed.com/jobs?q=", keyword[i], "&l=서울")
  remDr$navigate(search_url)
  # 
  # #put locaiton condition
  # location.input <- remDr$findElement("css", "#where")
  # location.input$sendKeysToElement(list("서울", key = "enter"))
  # Sys.sleep(0.5)
  # 
  #check result count 
  temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
  result_count <- temp %>% html_node(".searchCount-a11y-contrast-color div") %>% html_text() %>%
    str_remove("\n                    1페이지 결과 ") %>%
    str_remove(",") %>%
    str_remove("건") %>%
    as.numeric()
  message(result_count, " result appear")
  
  page_url <- temp %>% html_nodes(".title a") %>% html_attr("href")
  created_date <- temp %>% html_nodes(".date") %>% html_text()
  
  #scarp url 
  for(j in 1:ceiling(result_count / 12)){
    remDr$navigate(paste0(search_url, "&start=", j * 10))
    Sys.sleep(0.5)
    temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
    
    temp_url <- temp %>% html_nodes(".title a") %>% html_attr("href")
    page_url <- c(page_url, temp_url)
    
    temp_date <- temp %>% html_nodes(".date") %>% html_text()
    created_date <- c(created_date, temp_date)
  }
  temp_df <- data.frame(search_word = keyword[i], url = page_url, created_date = created_date)
  temp_df <- temp_df[!duplicated(temp_df$url),]
  
  all_df <- rbind(all_df, temp_df)
  temp_df <- NULL
  
  message(i, "th search word meta data of ", keyword[i], " end at indeed")
}

#fix_crawled_url
#bak <- all_df  #making backup file
all_df$url <- as.character(paste0("https://kr.indeed.com/", all_df$url))

contents_df <- NULL #initialize contents_df

#crawl detailed data
for(i in 1:length(all_df$url)){
  remDr$navigate(all_df$url[i])
  Sys.sleep(0.8)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  title <- temp %>% html_node(".jobsearch-JobInfoHeader-title-container") %>% html_text()
  company <- temp %>% html_node(".icl-u-lg-mr--sm.icl-u-xs-mr--xs") %>% html_text()
  type <- temp %>% html_node("div.jobsearch-JobMetadataHeader.icl-u-xs-mb--md > div:nth-child(2) > span") %>% html_text()
  if(is.na(type) == TRUE){type <- ""}
  
  description <- temp %>% html_node(".jobsearch-jobDescriptionText") %>% html_text()
  
  location <- temp %>% html_node(".jobsearch-JobMetadataHeader.icl-u-xs-mb--md") %>% html_text()
  
  temp_df <- data.frame(title = title, company = company, description = description, 
                        type = type, location = location,
                        url = all_df$url[i]) #url will use as a key to join two data frames
  contents_df <- rbind(contents_df, temp_df)
  
  if(i %% 100 == 0){message(round(i / length(all_df$url), 2)* 100, "% is done.")}
}


write_xlsx(all_df, "indeed_crawling_result.xlsx")
