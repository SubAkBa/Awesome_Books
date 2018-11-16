install.packages("Hmisc")
install.packages("fastDummies")
install.packages("recommenderlab")
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("RODBC")
install.packages("RMySQL")
library(reshape2)
library(Hmisc)
library(dplyr)
library(tidyr)
library(data.table)
library(fastDummies)
library(arules)
library(arulesViz)
library(recommenderlab)
library(RODBC)
library(RMySQL)

rm(list = ls()); gc(reset = T)
books <- read.csv("Books_Information.csv")
options(scipen = 10)

# Price numeric
books$Price <- as.numeric(gsub(",", "", 
                               substr(as.character(books$Price),
                                      1, nchar(as.character(books$Price)) - 1)))
summary(books$Price)
boxplot(books$Price)
books %>% filter(Price > 100000) %>% select(Title, Price)
books %>% filter(Price == max(Price)) %>% select(Title, Price)

# Pagenum numeric / 이상치(outlier) 변경
books$Pagenum <- as.numeric(substr(as.character(books$Pagenum), 1,
                                      nchar(as.character(books$Pagenum)) - 1))
summary(books$Pagenum)
boxplot(books$Pagenum)
books[books$Title == "서태후 5", "Pagenum"] <- 448
books[books$Title == "판타지 베오울프 1", "Pagenum"] <- 279
books[books$Title == "긴네무 집", "Pagenum"] <- 264

# Title char
books$Title <- as.character(books$Title)

# Writer "" 값 채워넣기
books[books$Title == "내 무덤 내가 팠다", "Writer"] <- "아름다운앎"
books[books$Title == "반삼국지 상", "Writer"] <- "저우다황"
books[books$Title == "반삼국지 하", "Writer"] <- "저우다황"
books[books$Title == "사악한 최면술사", "Writer"] <- "저우하오후이"
books[books$Title == "인민의 이름으로", "Writer"] <- "저우메이썬"
books[books$Title == "정원사 챈스의 외출", "Writer"] <- "저지 코진스키"
books[books$Title == "조센징 천황 죽이기", "Writer"] <- "저스틴 박"
books[books$Title == "한결같이 흘러가는 시간", "Writer"] <- "저스틴 고"
books[books$Title == "홍경래의 난", "Writer"] <- "최항기"

# Novel과 Genre 더미변수 생성 후 병합
books <- cbind(books, dummy_cols(books[, 11 : 12]))
colnames(books)
books[, 11 : 14] <- NULL

for(i in 0 : 21){
  colnames(books)[i + 11] <- paste0("col", i)
}
overlab_bookinfo <- books %>% group_by(Title) %>% 
  summarise(Novel_X = mean(col0),
            Novel_Korea = mean(col1),
            Novel_United = mean(col2),
            Novel_Japan = mean(col3),
            Novel_China = mean(col4),
            Novel_SpainLatin = mean(col5),
            Novel_France = mean(col6),
            Novel_Germany = mean(col7),
            Novel_Russia = mean(col8),
            Novel_Italy = mean(col9),
            Novel_EEurope = mean(col10),
            Novel_NEurope = mean(col11),
            Novel_EtcCountry = mean(col12),
            Novel_WriterSelect = mean(col13),
            Genre_Mystery = mean(col14),
            Genre_Horror = mean(col15),
            Genre_Fantasy = mean(col16),
            Genre_Martialart = mean(col17),
            Genre_SF = mean(col18),
            Genre_Thriller = mean(col19),
            Genre_History = mean(col20),
            Genre_X = mean(col21),
            n = n())
books <- books[, 1 : 10]
books <- merge(books, overlab_bookinfo, by = "Title", all.x = T)
write.csv(books, "Books_Information_1105.csv", row.names = F)

# Data Divide
books <- read.csv("Books_Information_1105.csv")
one <- books %>% filter(n == 1)
two <- books %>% filter(n == 2)
etc <- books %>% filter(n > 2)
write.csv(one, "Books_Information_one.csv", row.names = F)
write.csv(two, "Books_Information_two.csv", row.names = F)
write.csv(etc, "Books_Information_etc.csv", row.names = F)

# 그룹화 (publisher, Avail19 제거)
book_data <- books %>% group_by(Title, Writer, Series) %>%
  summarise(Price = round(mean(Price), -2), Pagenum = round(median(Pagenum)),
            Reviewcount = round(median(Reviewcount)), Score = median(Score),
            Sellnum = round(mean(Sellnum)), 
            Novel_X = ifelse(sum(Novel_X) > 0, 1, 0), 
            Novel_Korea = ifelse(sum(Novel_Korea) > 0, 1, 0),
            Novel_United = ifelse(sum(Novel_United) > 0, 1, 0), 
            Novel_Japan = ifelse(sum(Novel_Japan) > 0, 1, 0),
            Novel_China = ifelse(sum(Novel_China) > 0, 1, 0), 
            Novel_SpainLatin = ifelse(sum(Novel_SpainLatin) > 0, 1, 0),
            Novel_France = ifelse(sum(Novel_France) > 0, 1, 0), 
            Novel_Germany = ifelse(sum(Novel_Germany) > 0, 1, 0),
            Novel_Russia = ifelse(sum(Novel_Russia) > 0, 1, 0), 
            Novel_Italy = ifelse(sum(Novel_Italy) > 0, 1, 0),
            Novel_EEurope = ifelse(sum(Novel_EEurope) > 0, 1, 0), 
            Novel_NEurope = ifelse(sum(Novel_NEurope) > 0, 1, 0),
            Novel_EtcCountry = ifelse(sum(Novel_EtcCountry) > 0, 1, 0), 
            Novel_WriterSelect = ifelse(sum(Novel_WriterSelect) > 0, 1, 0),
            Genre_Mystery = ifelse(sum(Genre_Mystery) > 0, 1, 0),
            Genre_Horror = ifelse(sum(Genre_Horror) > 0, 1, 0),
            Genre_Fantasy = ifelse(sum(Genre_Fantasy) > 0, 1, 0),
            Genre_Martialart = ifelse(sum(Genre_Martialart) > 0, 1, 0),
            Genre_SF = ifelse(sum(Genre_SF) > 0, 1, 0),
            Genre_Thriller = ifelse(sum(Genre_Thriller) > 0, 1, 0),
            Genre_History = ifelse(sum(Genre_History) > 0, 1, 0),
            Genre_X = ifelse(sum(Genre_X) > 0, 1, 0),
            n = mean(n))
book_data <- as.data.frame(book_data)

# 장르 / 소설이 있을 경우 X부분을 0으로 변경
for(i in 1 : nrow(book_data)){
  for(j in 10 : 22){
    if(book_data[i, j] == 1){
      book_data[i, 9] <- 0
      break
    }
  }
  for(k in 23 : 29){
    if(book_data[i, k] == 1){
      book_data[i, 30] <- 0
      break
    }
  }
  print(i)
}

# 필요없는 row 데이터 제거
# (1) 예약판매, 세트 제거
books <- books[-grep("(예약판매|세트|컬렉션|SET|전집|도쿠가와|바이링궐|오디오북)",
                     books$Title), ]
# (2) Pagenum 결측치행 제거
books <- books %>% filter(!is.na(Pagenum))

# num -> factor구간으로 변경
books <- read.csv("Books_Information_group.csv")
str(books)
head(books)
View(books)

# Writer 데이터 파일 생성 (나중에 조인을 이용하여 사용 해야 함.)
writerlist <- data.frame(Writer = unique(books$Writer))
writerlist <- writerlist %>% arrange(Writer)
write.csv(writerlist, "Writer.csv")
writerlist <- read.csv("Writer.csv")
colnames(writerlist)[1] <- "Number"
write.csv(writerlist, "Writer.csv", row.names = F)

books <- merge(books, writerlist, by = "Writer", all.x = T)
writertemp <- books$Number
books$Writer <- writertemp
books[, c(31, 32)] <- NULL # 그룹화 했을 때의 갯수 까지 제거
books <- books[, c(2, 1, 3 : dim(books)[2])]
books <- books %>% arrange(Title)

# 마찬가지로 Series 데이터 파일, Title 데이터 파일 생성
serieslist <- data.frame(Series = unique(books$Series))
serieslist <- serieslist %>% arrange(Series)
write.csv(serieslist, "Series.csv")
serieslist <- read.csv("Series.csv")
colnames(serieslist)[1] <- "Number"
write.csv(serieslist, "Series.csv", row.names = F)

books <- merge(books, serieslist, by = "Series", all.x = T)
seriestemp <- books$Number
books$Series <- seriestemp
books[, 31] <- NULL
books <- books[, c(2, 3, 1, 4 : dim(books)[2])]
books <- books %>% arrange(Title)
write.csv(books, "Books_Information_pruning.csv", row.names = F)

titlelist <- data.frame(Title = unique(books$Title))
titlelist <- titlelist %>% arrange(Title)
write.csv(titlelist, "Title.csv")
titlelist <- read.csv("Title.csv")
colnames(titlelist)[1] <- "Number"
titlelist$Number <- paste0('t', titlelist$Number)
write.csv(titlelist, "Title.csv", row.names = F)

books <- merge(books, titlelist, by = "Title", all.x = T)
titletemp <- books$Number
books$Title <- titletemp
books[, 39] <- NULL
write.csv(books, "Books_Information_soon_end.csv", row.names = F)

# Price factor로 변환준비
summary(books$Price)
boxplot(books$Price)
Pricerange <- cut(books$Price, 
                  breaks = c(0, 5000, 10000, 15000, 20000, max(books$Price) + 1),
                  labels = c("0_5000", "5000_10000", "10000_15000", 
                    "15000_20000", "20000_"),
                  include.lowest = T,
                  right = F)
books$Price <- Pricerange

# Pagenum factor로 변환준비
summary(books$Pagenum)
boxplot(books$Pagenum)
books %>% filter(is.na(Pagenum)) %>% nrow()
books %>% select(Pagenum) %>% filter(Pagenum >= 500) %>% nrow()
Pagenumrange <- cut(books$Pagenum, 
                  breaks = c(0, 100, 200, 300, 400, max(books$Pagenum)),
                  labels = c("0_100", "100_200", "200_300", 
                             "300_400", "400_"),
                  include.lowest = T,
                  right = T)
books$Pagenum <- Pagenumrange

# Price / Pagenum Make Dummy and Ceiling
books <- cbind(books, dummy_cols(books[, c(4, 5)]))
books[, c(4, 5, 31, 32)] <- NULL
books <- books[, c(1 : 6, 32 : 29, 33, 38, 35, 34, 36, 37, 7 : 28)]
books$Reviewcount <- ceiling(books$Reviewcount)
books$Sellnum <- ceiling(books$Sellnum)
write.csv(books, "Books_Information_Range.csv", row.names = F)

# Colnames change
books$Writer <- paste0('w', books$Writer)
books$Series <- paste0('s', books$Series)

writers <- read.csv("Writer.csv")
writers$Number <- paste0('w', writers$Number)
write.csv(writers, "Writer.csv", row.names = F)

series <- read.csv("Series.csv")
series$Number <- paste0('s', series$Number)
write.csv(series, "Series.csv", row.names = F)
write.csv(books, "Books_Information_maybefinal.csv", row.names = F)

# Delete Series, Sellnum, Price & Pagenum Variables
# Merge Title, Writer
books <- read.csv("Books_Information_maybefinal.csv")
books[, c(3, 5, 7 : 16)] <- NULL
title <- read.csv("Title.csv")
colnames(books)[1] <- "Number"
writer <- read.csv("Writer.csv")
books <- merge(title, books, by = "Number", all.y = T)
books[, 1] <- NULL
colnames(books)[2] <- "Number"
books <- merge(writer, books, by = "Number", all.y = T)
books <- books[, c(2, 1, 4, 3, 5 : dim(books)[2])]
write.csv(books, "Books_Information_semifinal.csv", row.names = F)


# Melt datas
books <- read.csv("Books_Information_semifinal.csv")
# first. Novel
melt_book <- melt(books, id.vars = c(1 : 4, 19 : 26))
melt_book <- melt_book %>% arrange(Title)
novellist <- c("없음", "한국", "영미", "일본", "중국", "스페인/중남미", "프랑스",
                "독일", "러시아", "이탈리아", "동유럽", "북유럽", "기타국가", "작가선집")
formulastr <- colnames(melt_book)[1]
for(i in 2 : 12){
  formulastr <- paste0(formulastr, "+", colnames(melt_book)[i])
}
index <- which(melt_book$value == 1)
test <- melt_book
test$variable <- "Novel"
head(dcast(test, as.formula(paste0(formulastr, "~variable")), sum))


# RMySQL
con <- dbConnect(MySQL(), username = "root",
                password = "1234", host = "localhost",
                port = 3306, dbname = "R")
dbListTables(con)
dbGetQuery(con, "select user_email, user_priority from usertbl")


# 고객 데이터 만들기 / 유사도 측정
books <- read.csv("Books_Information_maybefinal.csv")
Title <- read.csv("Title.csv")
writer <- read.csv("Writer.csv")
series <- read.csv("Series.csv")
customer <- data.frame(num = 1 : 15, 
                       custid = c("c1", "c1", "c2", "c2", "c3", 
                                  "c4", "c3", "c5", "c5", "c5",
                                  "c6", "c4", "c1", "c3", "c6"),
                       book = c(blist1[1 : 3, 1], blist1[2, 1], blist1[1, 1],
                                tlist1[2 : 4, 1], tlist1[5, 1], tlist1[2 : 3, 1],
                                tlist1[1, 1], blist1[2 : 3, 1], blist1[1, 1]))
customer_lists <- split(customer$book, customer$custid)
customer_tran <- as(customer_lists, "transactions")
summary(customer_tran)
itemFrequency(customer_tran)
itemFrequencyPlot(customer_tran, support = 0.1)
itemFrequencyPlot(customer_tran, topN = 5)
customer_rule <- apriori(customer_tran)
summary(customer_rule)
inspect(customer_rule)

# Make Model
test <- merge(customer, books, by = "Title", all.x = T)
colnames(test)
total <- test %>% group_by(id) %>% summarise(n = n())
for(i in c(3, 4, 8 : dim(test)[2])){
  t <- test %>% group_by(id, !!as.name(colnames(test)[i])) %>% 
    summarise(n = n()) %>% dcast(as.formula(paste0("id~", colnames(test)[i])), 
                                 value.var = "n")
  total <- cbind(total, t[, 2 : length(t )])
}