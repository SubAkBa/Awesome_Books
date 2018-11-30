install.packages("recommenderlab")
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("plyr")
library(reshape2)
library(arules)
library(arulesViz)
library(recommenderlab)
library(dplyr)
library(plyr)

# Create example User Data & User PreferBooksData
write.csv(books, "Books_Information_final.csv")
books <- read.csv("Books_Information_final.csv")
colnames(books)[1] <- "Id"
write.csv(books, "Books_Information_final.csv", row.names = F)
books <- read.csv("Books_Information_final.csv")
user_data <- data.frame(user_email = c("1@gmail.com", "2@gmail.com", "3@gmail.com",
                                       "4@gmail.com", "5@gmail.com", "6@gmail.com",
                                       "7@gmail.com", "8@gmail.com", "9@gmail.com",
                                       "10@gmail.com"),
                        user_pwd = c("1234", "1234", "1234", "1234", "1234", "1234",
                                     "1234", "1234", "1234", "1234"),
                        user_nickname = c("a", "b", "c", "d", "e", "f", "g", "h",
                                          "i", "j"),
                        user_priority = c("장르", "소설", "작가", "장르", "소설", "작가",
                                          "장르", "소설", "작가", "장르"))
write.csv(user_data, "Users_Information.csv", row.names = F)
index1 <- sampleBy(~ Novel, frac = .0005, data = books) %>% select(Id)
index2 <- sampleBy(~ Genre1, frac = .0004, data = books) %>% select(Id)
index3 <- sampleBy(~ Genre2, frac = .0004, data = books) %>% select(Id)
random_index <- unlist(c(index1, index2, index3))
names(random_index) <- NULL
user_prefer_book <- data.frame(user_email = c("1@gmail.com", "2@gmail.com", "3@gmail.com",
                                              "4@gmail.com", "5@gmail.com", "6@gmail.com",
                                              "7@gmail.com", "8@gmail.com", "9@gmail.com",
                                              "10@gmail.com"),
                               book1 = c(random_index[seq(1, 30, 3)]),
                               book2 = c(random_index[seq(2, 30, 3)]),
                               book3 = c(random_index[seq(3, 30, 3)]))
write.csv(user_prefer_book, "Users_Prefer_Information.csv", row.names = F)

# Recommend books using Users_Prefer_Information data & Books_Information_final
books <- read.csv("Books_Information_final.csv")
users <- read.csv("Users_Information.csv", stringsAsFactors = F)
user_prefer <- read.csv("Users_Prefer_Information.csv", stringsAsFactors = F)
analysisbook <- data.frame(user_email = users$user_email,
                           book1 = NA, book2 = NA, book3 = NA, book4 = NA,
                           book5 = NA, stringsAsFactors = F)
# X -> NA
books[books == "X"] <- NA
books[books == "x"] <- NA
write.csv(books, "Books_Information_final.csv", row.names = F)

for(i in 1 : 10){
  analysisbook[i, c(2 : 6)] <- 
    ItemCollaborFiltering(users[i, "user_priority"], books, 
                          c(user_prefer[i, 2], user_prefer[i, 3], user_prefer[i, 4]))
  print(i)
}

# ItemCollaborativeFiltering Function
ItemCollaborFiltering <- function(users_priority, booksinfo, user_prefers){
  columnindex <- ifelse(users_priority == "장르", 1,
                        ifelse(users_priority == "소설", 2, 3))
  columnname <- c("Genre", "Novel", "Writer")
  if(columnindex == 1){
    categories <- data.frame(cate1 = NA, cate2 = NA, cate3 = NA, 
                             cate4 = NA, cate5 = NA, cate6 = NA,
                             cate7 = NA, cate8 = NA, cate9 = NA)
    
    for(j in 1 : dim(categories)[2]){
      categories[j] <- booksinfo[user_prefers[((j * 3) %/% 10) + 1], 
                                 paste0(columnname[columnindex], ifelse(j %% 3 == 0, 
                                                                        3, j %% 3))]
    }
  } else{
    categories <- data.frame(cate1 = NA, cate2 = NA, cate3 = NA)
    
    for(j in 1 : dim(categories)[2]){
      categories[j] <- booksinfo[user_prefers[j], 
                                 columnname[columnindex]]
    }
  }
  
  
  allcategory <- categories %>% melt(id.vars = NULL) %>% 
    group_by(value) %>% dplyr::summarise(n = n()) %>% # dplyr & plyr conflict
    arrange(desc(n), value) %>% as.data.frame()
  
  if(columnindex == 1){
    analysisdata <- data.frame()
    for(p in 1 : 3){
      analysisdata <- 
        rbind(analysisdata, 
              (ddply(booksinfo, paste0(columnname[columnindex], p), function(sub){
                sub %>% arrange(desc(Score), desc(Reviewcount)) %>% head(2) %>% 
                  filter(!!as.name(paste0(columnname[columnindex], p)) 
                         %in% allcategory$value)
              })) %>% select(Title))
      analysisdata <- unique(analysisdata)
    }
  } else{
    analysisdata <- 
      (ddply(booksinfo, columnname[columnindex], function(sub){
        sub %>% arrange(desc(Score), desc(Reviewcount)) %>% head(2)
      }) %>% filter(!!as.name(columnname[columnindex])
                    %in% allcategory$value) %>% select(Title))[[1]][1 : 5]
  }
  return(as.character(analysisdata))
}

# UserCollaborativeFiltering using library recommenderlab
books <- read.csv("Books_Information_final.csv")
index <- sample(nrow(books), 20000, replace = T)
bought <- books[index, ] %>% select(Id, Title)
bought$user_email <- paste0(rep(1 : 2000, each = 10), "@naver.com")
bought <- bought[, c(3, 1, 2)]
rownames(bought) <- NULL
write.csv(bought, "Book_Bought_Information.csv", row.names = F)

bought <- read.csv("Book_Bought_Information.csv")
realmat <- as(bought, "realRatingMatrix")
train_index <- sample(2000, 1400)
train_bought <- realmat[train_index]
train_bought <- train_bought[rowCounts(train_bought) >= 3]
# split, cross
# given = recommend item count, k = repeat simulation count
schema_bought <- evaluationScheme(train_bought, method = "split", train = .8,
                                  given = 5, goodRating = 4, k = 3)
UBCF_bought <- Recommender(train_bought, method = "UBCF", parameter = "Cosine")
test_bought <- realmat[-train_index, ]

UBCF_recommend_list <- predict(UBCF_bought, test_bought, n = 5)
View(as(UBCF_recommend_list, "list"))

# Pattern rule
split_bought <- split(bought$Id, bought$user_email)
transaction_bought <- as(split_bought, "transactions")
summary(transaction_bought)
itemFrequency(transaction_bought)
itemFrequencyPlot(transaction_bought, support = 0.001)
itemFrequencyPlot(transaction_bought, topN = 5)
rule_bought <- apriori(transaction_bought, 
                       parameter = list(support = 0.0005, 
                                        confidence = 0.002, minlen = 2))
View(inspect(rule_bought))