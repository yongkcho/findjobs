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

#Wanted data crawling 
#Wanted의 경우, 검색어로 찾는 것 보다는 직군을 클릭하는게 효율적임
search_df <- data.frame( #검색어 대신 관심있는 영역의 url을 저장해 사용 
  keyword <- c("전략기획자", "서비스기획자", "데이터분석", 
               "디지털 마케터", "마케팅 전략 기획자",
               "조직관리전문가", "교육전문가"),
  searh_num <- c("507/563", "507/565", "507/656",
                 "523/1030", "523/719",
                 "517/649", "517/648"))

#set search_word
for(i in 1:length(search_df$keyword)){
  
  message(i, "th search word ", search_df$keyword[i], " begin at Wanted")
  
  #move to search result
  remDr$navigate(paste0("https://www.wanted.co.kr/wdlist/", search_df$searh_num[i],
                        "?country=kr&job_sort=job.latest_order&years=-1&locations=all"))
  
  #Wanted는 검색 개수가 나오지 않기 때문에, 중간 갯수를 체크해 비교
  #initialize compare_num and scorll_num
  temp <- remDr$getPageSource()[[1]] %>% read_html() #scrpe page
  compare_num <- length(temp %>% html_nodes("._3D4OeuZHyGXN7wwibRM5BJ a") %>% html_attr("href"))
  scroll_num <- 0 #after scroll
  
  #페이지를 스크롤하며 url을 크롤링
  #scarpe searh_url
  while(compare_num != scroll_num){
    compare_num <- scroll_num
    
    #scroll down
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(1)
    
    #scrap result
    temp <- remDr$getPageSource()[[1]] %>% read_html()
    scroll_num <- length(temp %>% html_nodes("._3D4OeuZHyGXN7wwibRM5BJ a") %>% html_attr("href"))
    
    #in case crawling stop_left chk point
    if(scroll_num %% 100 == 0){search_url <- temp %>% html_nodes("._3D4OeuZHyGXN7wwibRM5BJ a") %>% html_attr("href")}
  }
  
  search_url <- temp %>% html_nodes("._3D4OeuZHyGXN7wwibRM5BJ a") %>% html_attr("href")
  
  #scrap details at each url
  for(j in 1:length(search_url)){
    remDr$navigate(paste0("https://www.wanted.co.kr", search_url[j]))
    Sys.sleep(1)
    
    #if page removed skip to next one
    if(remDr$getCurrentUrl()[[1]][1] == "https://www.wanted.co.kr/wd/undefined"){next}
    temp <- remDr$getPageSource()[[1]] %>% read_html()
    test <- temp %>% html_nodes("._3XP3DBqOgzsz7P6KrVpbGO > div:nth-child(2)") %>% html_node(".body") %>% html_text()
    
    #wait until contents are fully load
    while(length(test) == 0){
      Sys.sleep(2) 
      temp <- remDr$getPageSource()[[1]] %>% read_html()
      test <- temp %>% html_nodes("._3XP3DBqOgzsz7P6KrVpbGO > div:nth-child(2)") %>% html_node(".body") %>% html_text()
    } #if content is NULL try scarp page again
    
    title <- temp %>% html_node(".Bfoa2bzuGpxK9ieE1GxhW h2") %>% html_text()
    company <- temp %>% html_node(".Bfoa2bzuGpxK9ieE1GxhW h6 a") %>% html_text()
    hash_tag <- temp %>% html_nodes(".ObubI7m2AFE5fxlR8Va9t ul li a") %>% html_text()
    hash_tag <- paste(hash_tag, collapse = ", ") #해시태그를 ,로 구분하기 위해 정제
    
    introduction <- temp %>% html_node("._1LnfhLPc7hiSZaaXxRv11H > p:nth-child(1)") %>% html_text()
    description <- temp %>% html_node("._1LnfhLPc7hiSZaaXxRv11H > p:nth-child(3)") %>% html_text()
    qualification <- temp %>% html_node("._1LnfhLPc7hiSZaaXxRv11H > p:nth-child(5)") %>% html_text()
    preferential <- temp %>% html_node("._1LnfhLPc7hiSZaaXxRv11H > p:nth-child(7)") %>% html_text()
    welfare <- temp %>% html_node("._1LnfhLPc7hiSZaaXxRv11H > p:nth-child(9)") %>% html_text()
    
    end_date <- temp %>% html_nodes("._3XP3DBqOgzsz7P6KrVpbGO") %>% html_node(".body") %>% html_text()
    location <- temp %>% html_nodes("._3XP3DBqOgzsz7P6KrVpbGO > div:nth-child(2)") %>% html_node(".body") %>% html_text()
    
    if(length(location) == 0){location <- ""}
    temp_df <- data.frame(search_word = search_df$keyword[i],
                          platfrom = "Wanted",
                          title = title, company = company, hash_tag = hash_tag,
                          end_date = end_date, location = location,
                          introduction = introduction, description = description,
                          qualification = qualification, preferential = preferential,
                          welfare = welfare) #url 끼는거 까먹었다... 추가 필요함 
    temp <- NULL #initialize temp
    all_df <- rbind(all_df, temp_df)
    
    #printout message to check progress
    if(j %% 50 == 0){message(j, "th article out of ", length(search_url) ," is done.")}
  }
  message(i, "th search word ", search_df$keyword[i], " end at Wanted")
}

write_xlsx(all_df, "wanted_crawling_result.xlsx")