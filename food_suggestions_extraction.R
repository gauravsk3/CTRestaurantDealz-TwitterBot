generateTweet <- function() {
library(rvest)
library(RSelenium)
library(RCurl)
library(XML)
library(stringr)
library(xml2)
library(twitteR)
library(base64enc)

consumer_key = "7xbK2bfy2IUpEdpOsBtUyFeXJ"
consumer_secret = "9yN6Ocu4IFmpq5xqoN6DODHUBtZDQenoWLeFNYvZHJW4P1ET95"
access_token = "752182801764605954-yi3g7ULLqQaI1FWexZ7lPPsG2rgDjgX"
access_secret = "Nq2HYVUPtdHj12z15GCn4jgnfI8fvZCG1PvCZe8QbL4N7"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# GROUPON DATA

# The following code transforms the current data of the groupon's cape town food web page to a list of written
# recommendations along with the name of the associated restaurants.
# It stores the result in a (n listed restaurants) by 2 data frame

groupons <- read_html("https://www.groupon.co.za/coupons/cape-town/restaurants")

groupons.links <- groupons %>% html_nodes(css = "#global-container > div.ls-resp-main > div > div > div.row.collapse.wrap-page-content > div > section.row.collapse.wrap-gsm-deals-grid") %>% html_text()

groupons.string <- as.character(groupons.links)
groupons.string <- gsub(pattern = "\\\n", "", groupons.string)

groupons.vector <- str_split(groupons.string, pattern = "View Deal")
groupons.vector <- groupons.vector %>% sapply(function(x) str_trim(string = x))
groupons.vector <- as.vector(groupons.vector)

# isolate the deal suggestions
groupons.suggestions <- gsub(pattern = "[[:space:]]{2,}.*", "", groupons.vector)
groupons.suggestions <- as.vector(groupons.suggestions)
groupons.suggestions <- groupons.suggestions[-length(groupons.suggestions)]
# isolate the name of each restaurant
# tricky procedure: spot the suggestions part from each corresponding element of groupons.vectors, remove them,
# trim the string, then apply the same gsub method as the one used for the suggestions to isolate the names

groupons.names <- 1:length(groupons.suggestions)

for (i in 1:length(groupons.vector)) {
  groupons.names[i] <- gsub(pattern = groupons.suggestions[i], "", groupons.vector[i], fixed = T)
  groupons.names[i] <- str_trim(groupons.names[i])
  groupons.names[i] <- gsub(pattern = "[[:space:]]{2,}.*", "", groupons.names[i])
}

groupons.names <- groupons.names[-length(groupons.names)]

#------------------------------------------------------------------------

# Winter Specials DATA

winter <- read_html("https://www.mweb.co.za/Travel/view/tabid/4302/Article/19846/Cape-Town-Winter-Restaurant-Specials-2015.aspx")

winter.names <- winter %>% html_nodes("#dnn_ctr13083_ViewArticle_UpdatePanel1 > section > article > div.content > strong") %>% html_text()

winter.suggestions <- winter %>% html_nodes(xpath = "//*[@id='dnn_ctr13083_ViewArticle_UpdatePanel1']/section/article/div[2]/text()") %>% html_text()
winter.suggestions <- gsub("\\\r\\\n", "", winter.suggestions)
winter.suggestions <- winter.suggestions[-which(nchar(winter.suggestions) < 10)]
winter.suggestions <- winter.suggestions[-(1:4)]

#------------------------------------------------------------------------

rhino <- read_html("http://blog.rhinoafrica.com/en/2013/05/27/eat-out-for-less-this-winter-winter-restaurant-specials/")

rhino.list <- rhino %>% html_nodes("body > div.container.narrow > div.post-content") %>% html_children() %>% html_text()
rhino.list <- rhino.list[-c(1, which(rhino.list == ""))]
rhino.list <- rhino.list[-(1:3)]
rhino.list <- rhino.list[-length(rhino.list)]

split_char <- substr(str_split(rhino.list, pattern = " ")[[67]][3], 1, 1)

rhino.list <- str_split(rhino.list, pattern = split_char)

rhino.names <- c()
rhino.suggestions <- c()
for (i in 1:length(rhino.list)) {
  rhino.names <- c(rhino.names, rhino.list[[i]][1])
  rhino.suggestions <- c(rhino.suggestions, rhino.list[[i]][2])
}

rhino.names <- str_trim(rhino.names)
rhino.suggestions <- str_trim(rhino.suggestions)
rhino.suggestions <- substr(rhino.suggestions, 1, str_locate(rhino.suggestions, pattern = "Tel") - 2)

big.data <- data.frame("name" = c(groupons.names, winter.names, rhino.names),
                       "suggestion" = c(groupons.suggestions, winter.suggestions, rhino.suggestions)
)
big.data <- big.data[-c(31, 47, which(is.na(big.data$suggestion))),]  # small cleanup

#--------------------------------------------------------

# GPS Data

# Pattern:
# https://www.google.co.za/maps/place/<rest name with +>+Cape+Town

big.data$map.links <-
  paste("https://www.google.co.za/maps/place/", gsub(big.data$name, pattern = " ", replacement = "+"), "+Cape+Town/", sep = "")

big.data$suggestion <- as.character(big.data$suggestion)

(x <- sample(1:length(big.data$name), 1))
return (paste(big.data$name[x],":",big.data$suggestion[x]," ",big.data$map.links[x]))
}

old.tweets <- c("")
new.tweet <- generateTweet()
while (new.tweet %in% old.tweets) {
  new.tweet <- generateTweet()
}
tweet(new.tweet, bypassCharLimit = TRUE)
old.tweets <- c(old.tweets, new.tweet)
quit(save="no")
