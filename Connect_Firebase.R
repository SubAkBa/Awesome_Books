install.packages("devtools")
install_github("Kohze/fireData")
library(devtools)
library(fireData)

# Connect to Firebase
books <- read.csv("Books_Information_otosix.csv")
project_url <- "https://awesome-95cbd.firebaseio.com/"
project_id <- "awesome-95cbd"
email <- "circle5926@gmail.com"
web_api_key <- "AIzaSyD_UJwNIFrHJHbyoXL1oTHEOSapVilgsCs"
token <- anonymous_login("AIzaSyD_UJwNIFrHJHbyoXL1oTHEOSapVilgsCs")
createUser(projectAPI = "AIzaSyD_UJwNIFrHJHbyoXL1oTHEOSapVilgsCs", 
           email = "circle5926@gmail.com", password = "cir#14cle15(26")
authes <- auth(projectAPI = "AIzaSyD_UJwNIFrHJHbyoXL1oTHEOSapVilgsCs", 
               email = "circle5926@gmail.com", password = "cir#14cle15(26")

upload(x = books, projectURL = "https://awesome-95cbd.firebaseio.com/", 
       directory = "/")