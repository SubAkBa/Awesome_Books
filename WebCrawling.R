# Ready to Crawling
install.packages("rvest")
install.packages("dplyr")
install.packages("httr")
install.packages("RSelenium")
library(rvest)
library(dplyr)
library(httr)
library(RSelenium)

rm(list = ls()); gc(reset = T)

driver <- rsDriver()
chrome <- driver[["client"]]

book_info <- data.frame()
save_info <- read.csv("Books_Information.csv")
book_info <- rbind(save_info, book_info)

main_url <- "http://www.yes24.com"
pagenumber <- "?PageNumber="
ChangeNAFunc <- function(text, num){
  if(num == 1){
    result <- ifelse(is.na(text), 'X', text)
  } else if(num == 2){
    result <- ifelse(is.na(text), 0, text)
  } else if(num == 3){
    result <- ifelse(is.na(text), 'X', 'O')
  } else if(num == 4){
    result <- ifelse(length(text) == 1, 
                     (text %>% strsplit(" | "))[[1]][1], 
                     (text %>% strsplit(" | "))[[2]][1])
  }
  
  return (result)
}

genre_url <- "/24/Category/Display/001001017005"
novel_url <- "/24/Category/Display/001001017001"

cate_url <- c(genre_url, novel_url)

# Case 1: Genre / Case 2: Novel
for(t in 1 : 2){
  body_url <- read_html(paste0(main_url, cate_url[t])) %>% 
    html_nodes("#cateSubListWrap dt a") %>% html_attr("href")

  # 장르와 소설 분류를 세부적으로 진입
  for(i in 1 : length(body_url)){
    chrome$navigate(paste0(main_url, body_url[i]))
    list_html <- chrome$getPageSource()[[1]] %>% read_html()
    
    if(t == 1){
      Genre <- list_html %>% 
        html_nodes(xpath = '//*[@id="cateSubWrap"]/div[2]/div[1]/h3') %>% html_text()
      Novel <- "X"
    } else{
      Novel <- list_html %>% 
        html_nodes(xpath = '//*[@id="cateSubWrap"]/div[2]/div[1]/h3') %>% html_text()
      Genre <- "X"
    }
    
    last_page <- (list_html %>% html_node(".yesUI_pagenS a:last-child") %>% 
                    html_attr("href") %>% strsplit("="))[[1]][2]
    # 각 세부 카테고리의 모든 페이지
    for(j in 1 : last_page){
      chrome$navigate(paste0(main_url, body_url[i], pagenumber, j))
      
      page_html <- chrome$getPageSource()[[1]] %>% read_html()
      
      book_exist <- page_html %>% html_nodes(".goods_btn")
      
      page_url <- page_html %>% html_nodes(".goods_name a") %>% 
        html_attr("href") %>% unique()
  
      # 각각의 페이지 내에 있는 책 리스트
      for(k in 1 : length(page_url)){트
        
        if(!is.na(book_exist[k] %>% html_node(".txt_soldout"))){
          next
        }
        
        book_page <- paste0(main_url, page_url[k])
        chrome$navigate(book_page)
        book_html <- chrome$getPageSource()[[1]] %>% read_html()
        
        Title <- book_html %>% html_node(".gd_name") %>% html_text()
        Writer <- trimws(strsplit((book_html %>% 
                                     html_node(".gd_auth") %>% 
                                     html_text() %>% 
                                     strsplit("저"))[[1]][1], "엮음")[[1]][1], "both")
        
        Publisher <- book_html %>% html_node(".gd_pub") %>% html_text()
        Price <- book_html %>% html_node(".nor_price") %>% html_text()
        Pagenum <- ChangeNAFunc(book_html %>% html_nodes(".tb_detail01 .cell_2col") %>% 
                                  html_text(), 4)
        
        Series <- ChangeNAFunc((book_html %>% 
                                  html_node(xpath = '//*[@id="spanGdKeynote"]/a[2]') %>% 
                                  html_text() %>% strsplit("-"))[[1]][1], 1) 
        
        Reviewcount <- ChangeNAFunc(book_html %>% 
                                      html_node(".gd_reviewCount .txC_blue") %>% 
                                      html_text(), 2)
        Score <- ChangeNAFunc(book_html %>% html_node(".yes_b") %>% html_text(), 2)
        Sellnum <- ChangeNAFunc((book_html %>% html_node(".gd_sellNum") %>% 
                                   html_text() %>% strsplit(" "))[[1]][12], 2)
        Avail19 <- ChangeNAFunc(((book_html %>% html_nodes(".tb_detail01 .cell_2col") %>% 
                                   html_text())[5]), 3)
        
        book_temp <- cbind(Title, Writer, Publisher, Series, Price, Pagenum,
                           Reviewcount, Score, Sellnum, Avail19, Novel, Genre)
        book_info <- rbind(book_info, book_temp)
        print(paste0("i : ", i, " j / page : ", j, " / ", last_page, " k : ", k))
      }
    }
  }
}
write.csv(book_info, "Books_Information.csv", row.names = F)
