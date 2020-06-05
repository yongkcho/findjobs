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
  
  message(i, "th search word meta data of ", keyword[i], " begin at jobkorea")
  
  search_url <- paste0("http://www.jobkorea.co.kr/Search/?stext=",
                       keyword[i],
                       "&local=I000&tabType=recruit")
  remDr$navigate(search_url)
  Sys.sleep(1)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  result_count <- temp %>% html_node(".dev_tot") %>% html_text() %>%
    str_remove_all(",") %>% as.numeric()
  
  for(j in 1:ceiling(result_count / 20)){
    search_url <- paste0("http://www.jobkorea.co.kr/Search/?stext=",
                         keyword[i],
                         "&local=I000&tabType=recruit&Page_No=",
                         j)
    remDr$navigate(search_url)
    Sys.sleep(1)
    
    temp <- remDr$getPageSource()[[1]] %>% read_html()
    
    #scarp data
    url <- temp %>% html_nodes(".list-default") %>% html_nodes(".title.dev_view")  %>% html_attr("href")
      
    company <- temp %>% html_nodes(".recruit-info") %>% html_nodes(".post-list-corp a") %>% 
      html_attr("title")
    company <- company[!is.na(company)]
    
    title <- temp %>% html_nodes(".recruit-info") %>% html_nodes(".title.dev_view") %>% 
      html_attr("title")
    title <- title[!is.na(title)]
    
    qualification_1 <- temp %>% html_nodes(".recruit-info") %>% html_nodes(".exp") %>% html_text()
    # qualification_2 <- temp %>% html_nodes(".recruit-info") %>% html_nodes(".edu") %>% html_text()
    # edu를 채우지 않은 회사들 때문에 수가 맞지 않아 제거, 향후 각 페이지 스크랩에서 보완
    type <- temp %>% html_nodes(".list-default") %>% html_nodes(".option > span:nth-child(3)") %>% html_text()
    location <- temp %>% html_nodes(".list-default") %>% html_nodes(".loc.long") %>% html_text()
    end_date <- temp %>% html_nodes(".list-default ul li p") %>% html_nodes(".date") %>% html_text()
    
    # hashtags <- temp %>%  html_nodes(".list-default") %>% html_nodes(".etc") %>% html_text() 
    # hashtags를 채우지 않은 회사를 때문에 수가 맞지 않아 제거, 상동
    
    temp_df <- data.frame(search_word = keyword[i], url = url, title = title,
                          company = company, type = type, qualification_1 = qualification_1,
                          end_date = end_date)
    
    all_df <- rbind(all_df, temp_df)
    
    if(j %% 10 == 0){message(round(j / ceiling(result_count / 20), 2)* 100, "% is done.")}
  }
  
  message(i, "th search word meta data of ", keyword[i], " end at jobkorea")
  
}

#crawl contents data
all_df$url <- as.character(paste0("http://www.jobkorea.co.kr", all_df$url))
contents_df <- NULL

for(i in 3201:length(all_df$url)){
  try(remDr$navigate(all_df$url[i]))
  try(remDr$acceptAlert())
  
  # chk_deleted <- remDr$getCurrentUrl()[[1]]
  # if(chk_deleted == "https://www.jobkorea.co.kr/"){
  #   temp_df <- data.frame(url = all_df$url[i], qualification_2 = "",
  #                         preferential = "", payment = "", company_type = "",
  #                         welfare = "", applicant = "", hashtags = "deleted")
  #   next
  # }
  # 
  Sys.sleep(2)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  is_block <- temp %>% html_nodes(".reasonExp") %>% html_text()
  if(length(is_block) != 0){
    message("blocked at ", i, "th page.")
    break
  }

  
  temp_table <- temp %>% html_nodes(".tbList > dd:nth-child(4)") %>% html_text() %>% 
    gsub(pattern = "\\s",   replacement = "") #remove space
  qualification_2 <- temp_table[1]
  preferential <- temp %>% html_nodes(".pref") %>% html_text()
  preferential <- paste(preferential, collapse =", ")
  payment <- temp_table[2]

  temp_table <- temp %>% html_nodes(".tbList > dd:nth-child(8)") %>% html_text() %>% 
    gsub(pattern = "\\s",   replacement = "") #remove space
  comapny_type <- temp_table[2]
  hours <- temp_table[1]
  
  welfare <- temp %>% html_nodes(".location") %>% html_nodes(".tbRow") %>% html_text() %>%
    gsub(pattern = "\\s", replacement = "")
  welfare <- welfare[2]
    
  applicant <- temp %>% html_nodes(".metrics.metricsCount") %>% html_nodes(".value") %>% html_text() %>%
    str_remove_all("명") %>% as.numeric()
  if(length(applicant) == 0){applicant <- 0}
  
  hashtags <- temp %>%  html_nodes(".list.extendBx li") %>% html_text() %>%
    str_remove_all("\n") %>% trimws()
  hashtags <- paste(hashtags, collapse = ", ")
  
  temp_df <- data.frame(url = all_df$url[i], qualification_2 = qualification_2,
                        preferential = preferential, payment = payment, company_type = company_type,
                        welfare = welfare, applicant = applicant, hashtags = hashtags)
  contents_df <- rbind(contents_df, temp_df)
  
  if(i %% 100 == 0){
    message(round(i / length(all_df$url), 2)* 100, "% is done.")
    Sys.sleep(300)
    }
}

all_df <- cbind(all_df, contents_df)

write_xlsx(all_df, "jobkorea_result.xlsx")
