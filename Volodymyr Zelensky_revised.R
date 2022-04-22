#in order to ignore warnings
options(warn = -1) 
#install.packages("twitteR")
library(twitteR)
#install.packages("ROAuth")
library(ROAuth)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rtweet")
library(rtweet)
#install.packages("tidyr")
library(tidyr)
#install.packages("tidytext")
library(tidytext)
#install.pckages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("tidyverse")
library(magrittr)
library(tidyverse)
#install.packages("janeaustenr")
library(janeaustenr)
#install.packages("widyr")
library(widyr)
#install.packages("dplyr")
library(plyr)
library(dplyr)

#Place your unique codes here
consumer_key <- "TvoZJOFx9BSmW0Oyf5NQZU5tl"
consumer_secret <- "M3IVdV8spOY2pd9b4TDquCcwbombvNyjrK6dYInDWqrcYjzWrC"
access_token <- "1515177592436899844-OWJVdzmrRTioW6P0iyPS7UxvlHakID"
access_token_secret <- "e5UC4eLk7WR8MPYRlPNZObgmtpNs6adQely2GmHKSDQrC"

#Create access token
create_token(
  app = "Datatextttt",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_token_secret,set_renv = TRUE)

get_token()

##Getting access to twitter
setup_twitter_oauth(consumer_key,consumer_secret,access_token=access_token,access_secret = access_token_secret)

#Creating a data frame name Zelenskyy
#Extracting the tweets of that user
Zelenskyy <- get_timeline("@ZelenskyyUa",n=209)
View(Zelenskyy)

#Selecting created_at,text columns from the data frame
tweets.Zelenskyy = Zelenskyy %>% select(created_at,text)
tweets.Zelenskyy

head(tweets.Zelenskyy$text)

#Selecting the tweets which we require from the specified dates 
tweets.Zelenskyy <- filter(tweets.Zelenskyy,created_at < "2022-04-13 14:01:25" & created_at > "2022-02-24 06:03:36")
head(tweets.Zelenskyy)
View(tweets.Zelenskyy)

#Remove http elements manually
tweets.Zelenskyy$stripped_text1 <- gsub("http\\S+","",tweets.Zelenskyy$text)

#Remove punctuation and add id for each tweet
tweets.Zelenskyy_stem <- tweets.Zelenskyy %>%
  select(stripped_text1) %>%
  unnest_tokens(word,stripped_text1)

head(tweets.Zelenskyy_stem)

#Remove stop words from list of words
cleaned_tweets.Zelenskyy <- tweets.Zelenskyy_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Zelenskyy)

###### Top 15 words (frequently used words) in Zelensky's tweets
cleaned_tweets.Zelenskyy %>%
  dplyr::count(word,sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word,y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x = 'Unique Words', y ='Count', title = "Frequently used words in Volodymyr Zelensky tweets")



### WORD CLOUD########
library(RColorBrewer)
library(wordcloud)

class(cleaned_tweets.Zelenskyy)

word_cloud1 <- unlist(cleaned_tweets.Zelenskyy)

wordcloud(word_cloud1)
wordcloud(word_cloud1, min.freq = 3)

wordcloud(word_cloud1, min.freq = 1, random.order = FALSE)

wordcloud(word_cloud1, min.freq = 3, random.order = FALSE, scale = c(4,1))

# Add colors to words

pal2 <- brewer.pal(8, "Dark2")
wordcloud(word_cloud1, min.freq = 4, random.order = FALSE , scale = c(4,1), colors = pal2)

######SENTIMENT ANALYSIS####

#install.packages("syuzhet")
library(syuzhet)

# Converting tweets to ASCII to trackle strange characters
cleaned_tweets.Zelenskyy <- iconv(cleaned_tweets.Zelenskyy, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
cleaned_tweets.Zelenskyy <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",cleaned_tweets.Zelenskyy)

# removing mentions, in case needed
cleaned_tweets.Zelenskyy <-gsub("@\\w+","",cleaned_tweets.Zelenskyy)

#Obtain sentimental scores
ew_sentiment<-get_nrc_sentiment((cleaned_tweets.Zelenskyy))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores

#Plotting the sentimental analysis
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores of Volodymyr Zelensky")+
  theme_minimal()
