캡스톤 디자인(졸업 작품)
================

1. 도서 추천 서비스 앱
======================

------------------------------------------------------------------------

1.1 개요
--------

먼저, 이 프로젝트에서 다른 팀원들은 어플리케이션 설계 및 구현을 담당하였고 본인은 도서를 추천하는 시스템을 맡았다.
아이템 및 사용자 협업 필터링을 상황에 맞게 적용하여 개인 별로 알맞은 도서들을 추천해주는 서비스를 구현하였다.

1.2 순서
--------

-   yes24 웹 크롤링을 통한 도서 데이터 모으기
-   도서 데이터 전처리
-   협업필터링 적용
-   파이어베이스, 안드로이드 연동

2. yes24 웹 크롤링
==================

------------------------------------------------------------------------

yes24 도서 데이터를 사용한 이유는 url 규칙을 찾기 쉬웠던게 yes24 였다...
(1) 교보문고 같은 경우 url뒤 파라미터가 보이지 않는 데도 불구하고 페이지 화면은 바껴서 규칙을 찾을 수 없었고,
(2) 반디앤루니스는 robots.txt 파일을 들여다보면 검색엔진을 제한한다고 써 있어서 하지 못하였다.
~~(3) 가장 큰 이유는 아무래도 내 지식이 얕아 url을 읽지 못했기 때문이 아닐까라는 생각~~

2.1 필요한 라이브러리 장착
--------------------------

``` r
library(rvest)
library(dplyr)
library(httr)
library(RSelenium)
```

2.2 크롤링을 위한 코드 구현
---------------------------

``` r
driver <- rsDriver()
chrome <- driver[["client"]]

book_info <- data.frame()
save_info <- read.csv("Books_Information.csv")
book_info <- rbind(save_info, book_info)

main_url <- "http://www.yes24.com"
pagenumber <- "?PageNumber="

genre_url <- "/24/Category/Display/001001017005"
novel_url <- "/24/Category/Display/001001017001"

cate_url <- c(genre_url, novel_url)
```

book\_info &lt;- rbind(save\_info, book\_info)가 있는 이유?
*:크롤링할 데이터가 많아 중간중간 끊어 저장하고 다시 크롤링 했기 때문에 묶어주기 위해 삽입했다.*

``` r
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
```

이 함수는 크롤링을 하면서 결측치가 들어오기 때문에 num에 따라서 결측치를 채워주기 위해 만들었다.

**yes24의 도서 데이터를 크롤링 하는 메인 코드 부분이다.**

``` r
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
      for(k in 1 : length(page_url)){
        
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
```

2.3 데이터 구조
---------------

    ##                 Title          Writer           Publisher
    ## 1 돌이킬 수 없는 약속   야쿠마루 가쿠            북플라자
    ## 2 매스커레이드 나이트 히가시노 게이고            현대문학
    ## 3   가면산장 살인사건 히가시노 게이고                재인
    ## 4     11문자 살인사건 히가시노 게이고 알에이치코리아(RHK)
    ## 5       앨리스 죽이기 코바야시 야스미              검은숲
    ## 6     용의자 X의 헌신 히가시노 게이고                재인
    ##                Series Price Pagenum Reviewcount Score Sellnum Avail19
    ## 1                   X 13500     380         102     8  548595       X
    ## 2 매스커레이드 시리즈 13320     556          32     8   55707       X
    ## 3                   X 13320     336         134     8   88926       X
    ## 4                   X 13320     344          61     8   34083       X
    ## 5                   X 12150     364          47     6  149877       X
    ## 6                   X 15120     448          51     8   77232       X
    ##   Novel Genre
    ## 1     x  추리
    ## 2     x  추리
    ## 3     x  추리
    ## 4     x  추리
    ## 5     x  추리
    ## 6     x  추리

    ## 'data.frame':    30563 obs. of  12 variables:
    ##  $ Title      : Factor w/ 24779 levels "'에드거 앨런 포' 베스트 단편집 : The Best of Edgar Allan Poe (영어 원서)",..: 5624 7374 616 63 14612 16335 10551 10553 6115 7375 ...
    ##  $ Writer     : Factor w/ 8180 levels "","(송)이방 등 모음 / 김장환 외 공역",..: 4351 8165 8165 8165 7202 8165 2601 2601 8165 8165 ...
    ##  $ Publisher  : Factor w/ 1657 levels "19.0","1984(일구팔사)",..: 666 1588 1200 959 81 1200 498 498 1588 1588 ...
    ##  $ Series     : Factor w/ 107 levels "2013 올해의 책",..: 12 31 12 12 12 12 12 12 4 31 ...
    ##  $ Price      : int  13500 13320 13320 13320 12150 15120 14220 14220 13320 12600 ...
    ##  $ Pagenum    : int  380 556 336 344 364 448 516 508 524 344 ...
    ##  $ Reviewcount: int  102 32 134 61 47 51 0 0 118 43 ...
    ##  $ Score      : int  8 8 8 8 6 8 0 0 8 8 ...
    ##  $ Sellnum    : int  548595 55707 88926 34083 149877 77232 3420 3180 65490 15138 ...
    ##  $ Avail19    : Factor w/ 2 levels "O","X": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Novel      : Factor w/ 14 levels "x","기타 국가의 소설",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Genre      : Factor w/ 8 levels "SF","X","공포",..: 7 7 7 7 7 7 7 7 7 7 ...

| 변수        | 변수설명                                           |
|-------------|----------------------------------------------------|
| Title       | 책 제목                                            |
| Writer      | 작가                                               |
| Publisher   | 출판사                                             |
| Series      | 시리즈 ex) 아서 코난 도일 시리즈, 링컨 라임 시리즈 |
| Price       | 책 가격                                            |
| Pagenum     | 총 페이지 쪽수                                     |
| Reviewcount | 리뷰 갯수                                          |
| Score       | 평점                                               |
| Sellnum     | 판매지수                                           |
| Avail19     | 19세 미만 여부                                     |
| Novel       | 소설 분류 ex) 한국소설, 일본소설 ..(국가분류)      |
| Genre       | 장르                                               |

3. 데이터 전처리 (PreProcessing)
================================

------------------------------------------------------------------------

3.1 라이브러리
--------------

``` r
library(reshape2)
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:Hmisc':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:reshape2':
    ## 
    ##     smiths

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:reshape2':
    ## 
    ##     dcast, melt

``` r
library(fastDummies)
library(RODBC)
library(RMySQL)
```

    ## Loading required package: DBI

3.2 이상치, 결측치 확인
-----------------------

    ## book_info 
    ## 
    ##  12  Variables      30563  Observations
    ## ---------------------------------------------------------------------------
    ## Title 
    ##        n  missing distinct 
    ##    30563        0    24779 
    ## 
    ## lowest : '에드거 앨런 포' 베스트 단편집 : The Best of Edgar Allan Poe (영어 원서)   (그 느낌 그대로) 국어 교과서 단편소설 1집                                  (그 느낌 그대로) 국어 교과서 단편소설 2집                                  (그동안 숨겨진 희귀본) 그림형제 동화 1집 : Grimms' Fairy Tales (영어 원서) (그동안 숨겨진 희귀본) 그림형제 동화 2집 : Grimms' Fairy Tales (영어 원서)
    ## highest: 힉스Higgs                                                                  힐                                                                         힐 하우스의 수상한 여자들                                                  힐 하우스의 유령                                                           힘겨운 사랑                                                               
    ## ---------------------------------------------------------------------------
    ## Writer 
    ##        n  missing distinct 
    ##    30563        0     8180 
    ## 
    ## lowest :                                         (송)이방 등 모음 / 김장환 외 공역       2017 서울영서초 6학년, 신옥경 공        303행성                                 30cm_books 편                          
    ## highest: 히사오 주란                             히사오 주란, 노무라 고도, 오카모토 기도 힐러리 맨틀                             힐러리 멘텔                             힘센소설가 7인 외                      
    ## ---------------------------------------------------------------------------
    ## Publisher 
    ##        n  missing distinct 
    ##    30562        1     1657 
    ## 
    ## lowest : 19.0                    1984(일구팔사)          1984Books(일구팔사북스) 21세기북스              30cm                   
    ## highest: 희고희고                희담                    희망과하라시비츠        힐북                    힘써                   
    ## ---------------------------------------------------------------------------
    ## Series 
    ##        n  missing distinct 
    ##    30563        0      107 
    ## 
    ## lowest : 2013 올해의 책         2014 올해의 책         2015 올해의 책         2016 올해의 책         2017 올해의 책        
    ## highest: 행복한 탐정 시리즈     헛소리 시리즈          현대문학 핀 시리즈     현대지성 클래식        형사 해리 홀레 시리즈 
    ## ---------------------------------------------------------------------------
    ## Price 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##    30563        0      559    0.983    11849     6583     6750     7200 
    ##      .25      .50      .75      .90      .95 
    ##     7200    10450    12420    14250    20700 
    ## 
    ## lowest :    1800    2250    2400    2520    2700
    ## highest:  486000  558000  585000  990000 2375000
    ## ---------------------------------------------------------------------------
    ## Pagenum 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##    30281      282      998        1    376.4      186      176      220 
    ##      .25      .50      .75      .90      .95 
    ##      288      318      400      519      610 
    ## 
    ## lowest :     1    17    29    32    33, highest: 12840 19000 19600 20600 30000
    ## ---------------------------------------------------------------------------
    ## Reviewcount 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##    30563        0      253    0.853    8.015    13.55        0        0 
    ##      .25      .50      .75      .90      .95 
    ##        0        0        6       21       38 
    ## 
    ## lowest :    0    1    2    3    4, highest:  732  797  832  869 1043
    ## ---------------------------------------------------------------------------
    ## Score 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##    30563        0       10    0.826    3.714    4.135        0        0 
    ##      .25      .50      .75      .90      .95 
    ##        0        0        8        8        9 
    ##                                                                       
    ## Value          0     2     3     4     5     6     7     8     9    10
    ## Frequency  16091    20    11    59   118  1504  1647  9219   634  1260
    ## Proportion 0.526 0.001 0.000 0.002 0.004 0.049 0.054 0.302 0.021 0.041
    ## ---------------------------------------------------------------------------
    ## Sellnum 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##    30563        0     1723    0.973      898     1653        0        0 
    ##      .25      .50      .75      .90      .95 
    ##        0       36      222     1058     2652 
    ## 
    ## lowest :      0     12     18     24     30, highest: 232734 548595 563541 572619 817959
    ## ---------------------------------------------------------------------------
    ## Avail19 
    ##        n  missing distinct 
    ##    30563        0        2 
    ##                       
    ## Value          O     X
    ## Frequency     96 30467
    ## Proportion 0.003 0.997
    ## ---------------------------------------------------------------------------
    ## Novel 
    ##        n  missing distinct 
    ##    30563        0       14 
    ## 
    ## x (13978, 0.457), 기타 국가의 소설 (615, 0.020), 독일소설 (947, 0.031),
    ## 동유럽소설 (143, 0.005), 러시아소설 (621, 0.020), 북유럽소설 (233, 0.008),
    ## 스페인/중남미소설 (311, 0.010), 영미소설 (3501, 0.115), 이탈리아소설 (194,
    ## 0.006), 일본소설 (2335, 0.076), 작가 선집 (395, 0.013), 중국소설 (899,
    ## 0.029), 프랑스소설 (1646, 0.054), 한국소설 (4745, 0.155)
    ## ---------------------------------------------------------------------------
    ## Genre 
    ##        n  missing distinct 
    ##    30563        0        8 
    ##                                                                   
    ## Value          SF      X   공포   무협 스릴러   역사   추리 판타지
    ## Frequency     592  16585    684   3145    849   2117   1989   4602
    ## Proportion  0.019  0.543  0.022  0.103  0.028  0.069  0.065  0.151
    ## ---------------------------------------------------------------------------

    ##                   Title                   Writer     
    ##  데미안              :   49   애거사 크리스티:  388  
    ##  젊은 베르테르의 슬픔:   40   아서 코난 도일 :  349  
    ##  어린 왕자           :   37   레프 톨스토이  :  162  
    ##  위대한 개츠비       :   37   나관중         :  150  
    ##  노인과 바다         :   31   모리스 르블랑  :  147  
    ##  이방인              :   29   헤르만 헤세    :  132  
    ##  (Other)             :30340   (Other)        :29235  
    ##                   Publisher                             Series     
    ##  청어람                : 1565   X                          :30192  
    ##  문학동네              : 1259   창비 큰글자도서            :   28  
    ##  로크미디어            : 1059   한국 대표소설 선집         :   25  
    ##  파피루스(디앤씨미디어):  896   시공사 장르문학 시리즈     :   24  
    ##  어울림출판사          :  862   타우누스                   :   12  
    ##  (Other)               :24921   사립탐정 켄지&제나로 시리즈:   11  
    ##  NA's                  :    1   (Other)                    :  271  
    ##      Price            Pagenum         Reviewcount           Score       
    ##  Min.   :   1800   Min.   :    1.0   Min.   :   0.000   Min.   : 0.000  
    ##  1st Qu.:   7200   1st Qu.:  288.0   1st Qu.:   0.000   1st Qu.: 0.000  
    ##  Median :  10450   Median :  318.0   Median :   0.000   Median : 0.000  
    ##  Mean   :  11849   Mean   :  376.4   Mean   :   8.015   Mean   : 3.714  
    ##  3rd Qu.:  12420   3rd Qu.:  400.0   3rd Qu.:   6.000   3rd Qu.: 8.000  
    ##  Max.   :2375000   Max.   :30000.0   Max.   :1043.000   Max.   :10.000  
    ##                    NA's   :282                                          
    ##     Sellnum       Avail19          Novel           Genre      
    ##  Min.   :     0   O:   96   x         :13978   X      :16585  
    ##  1st Qu.:     0   X:30467   한국소설  : 4745   판타지 : 4602  
    ##  Median :    36             영미소설  : 3501   무협   : 3145  
    ##  Mean   :   898             일본소설  : 2335   역사   : 2117  
    ##  3rd Qu.:   222             프랑스소설: 1646   추리   : 1989  
    ##  Max.   :817959             독일소설  :  947   스릴러 :  849  
    ##                             (Other)   : 3411   (Other): 1276

결측치는 Pagenum에서 282개, Publisher에서 1개 확인됐다.
먼저 Publisher 결측치를 확인해보자.

    ##                                Title      Writer Publisher Series Price
    ## 26240 어린왕자 소설 & 컬러링 북 세트 생 텍쥐페리      <NA>      X 21600
    ##       Pagenum Reviewcount Score Sellnum Avail19      Novel Genre
    ## 26240     272          11    10       0       X 프랑스소설     X

*확인 결과 yes24 페이지에서 출판사 정보가 없어 결측치로 삽입됐다.*

    ##                                    Title            Writer
    ## 35        코너스톤 아르센 뤼팽 전집 세트     모리스 르블랑
    ## 221            명탐정 푸아로 베스트 세트   애거사 크리스티
    ## 287              죽지 않는 학생 살인사건       노자키 마도
    ## 304            만능감정사 Q의 사건수첩 7 마츠오카 케이스케
    ## 431  결정판 아르센 뤼팽 전집 1-10권 세트     모리스 르블랑
    ## 464                   베를린 누와르 세트           필립 커
    ## 472   셜록 홈즈 에센셜 에디션 01+02 세트    아서 코난 도일
    ## 546           셜록 홈스 전집 단편집 세트    아서 코난 도일
    ## 555                               파편 2            홍수연
    ## 556                            파편 세트            홍수연
    ## 576                방해자 상, 하 2권 SET     오쿠다 히데오
    ## 587             셜록 홈스 전집 장편 세트    아서 코난 도일
    ## 722             셜록홈즈의 마지막 인사 1    아서 코난 도일
    ## 723             셜록홈즈의 마지막 인사 2    아서 코난 도일
    ## 724             셜록홈즈의 마지막 인사 3    아서 코난 도일
    ## 1216                       목련이 피었다            서미애
    ## 1293                셜록홈즈 단편 베스트    아서 코난 도일
    ## 1618                    푸코의 진자 세트     움베르토 에코
    ## 1998             러브크래프트 전집 세트  H.P. 러브크래프트
    ## 2033             죽지 않는 학생 살인사건       노자키 마도
    ##              Publisher Series  Price Pagenum Reviewcount Score Sellnum
    ## 35      코너스톤(도서)      X  58500      NA           0     0    2628
    ## 221           황금가지      X  40000      NA           1    10    1974
    ## 287     영상출판미디어      X   8100      NA           2     6     294
    ## 304     영상출판미디어      X   9000      NA           7     8     258
    ## 431       arte(아르테)      X 285300      NA          19     8       0
    ## 464           북스피어      X  37260      NA           0     0     378
    ## 472     코너스톤(도서)      X  27000      NA           7     8       0
    ## 546           엘릭시르      X  68400      NA           2    10      96
    ## 555  파란 (파란미디어)      X  11700      NA           8     7     120
    ## 556  파란 (파란미디어)      X  23400      NA          36     8     738
    ## 576           북스토리      X  27000      NA          24     8       0
    ## 587           엘릭시르      X  46800      NA           4     8      72
    ## 722               큰글      X  23000      NA           0     0       0
    ## 723               큰글      X  23000      NA           0     0       0
    ## 724               큰글      X  23000      NA           0     0       0
    ## 1216            청어람      X   9900      NA           9     8     144
    ## 1293      시간과공간사      X   5400      NA           0     0       0
    ## 1618          열린책들      X  34560      NA          42     7       0
    ## 1998          황금가지      X  73350      NA           2     7    2895
    ## 2033    영상출판미디어      X   8100      NA           2     6     294
    ##      Avail19 Novel Genre
    ## 35         X     x  추리
    ## 221        X     x  추리
    ## 287        X     x  추리
    ## 304        X     x  추리
    ## 431        X     x  추리
    ## 464        X     x  추리
    ## 472        X     x  추리
    ## 546        X     x  추리
    ## 555        X     x  추리
    ## 556        X     x  추리
    ## 576        X     x  추리
    ## 587        X     x  추리
    ## 722        X     x  추리
    ## 723        X     x  추리
    ## 724        X     x  추리
    ## 1216       X     x  추리
    ## 1293       X     x  추리
    ## 1618       X     x  추리
    ## 1998       X     x  공포
    ## 2033       X     x  공포

*20개의 데이터만 확인 해본 결과 단일 책이 아닌 세트로 구성되어 있어서*
*쪽수 정보가 페이지에 없기 때문에 결측치로 삽입됐다.*

*세트 데이터는 단일품만으로도 충분히 추천시스템을 구현할 수 있고 무엇보다 데이터가 지저분하다고 판단되어 제거하기로 결정했다.*

``` r
book_info <- book_info %>% filter(!is.na(Pagenum) & !is.na(Publisher))
```
