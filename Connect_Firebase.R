install.packages("devtools")
install_github("Kohze/fireData")
library(devtools)
library(fireData)

# Connect to Firebase
books <- read.csv("Books_Information_otosix.csv")
project_url <- "https://awesome-95cbd.firebaseio.com/"
project_id <- "awesome-95cbd"
email <- "circle5926@gmail.com"
web_api_key <- ""
token <- anonymous_login("")
createUser(projectAPI = "", 
           email = "circle5926@gmail.com", password = "")
authes <- auth(projectAPI = "", 
               email = "circle5926@gmail.com", password = "")

upload(x = books, projectURL = "https://awesome-95cbd.firebaseio.com/", 
       directory = "/")
