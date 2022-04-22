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

consumer_key <- "TvoZJOFx9BSmW0Oyf5NQZU5tl"
consumer_secret <- "M3IVdV8spOY2pd9b4TDquCcwbombvNyjrK6dYInDWqrcYjzWrC"
access_token <- "1515177592436899844-OWJVdzmrRTioW6P0iyPS7UxvlHakID"
access_token_secret <- "e5UC4eLk7WR8MPYRlPNZObgmtpNs6adQely2GmHKSDQrC"

create_token(
  app = "Datatextttt",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_token_secret,set_renv = TRUE)

get_token()

setup_twitter_oauth(consumer_key,consumer_secret,access_token=access_token,access_secret = access_token_secret)


putin <- get_timeline("@KremlinRussia_E",n=60)
View(putin)


tweets.putin = putin %>% select(created_at,text)
tweets.putin

head(tweets.putin$text)

tweets.putin <- filter(tweets.putin,created_at < "2022-03-14 15:43:46" & created_at > "2022-02-24 05:07:32")
head(tweets.putin)
View(tweets.putin)

#Remove http elements manually
tweets.putin$stripped_text1 <- gsub("http\\S+","",tweets.putin$text)

#Remove punctuation and add id for each tweet
tweets.putin_stem <- tweets.putin %>%
  select(stripped_text1) %>%
  unnest_tokens(word,stripped_text1)

head(tweets.putin_stem)

#Remove stop words from list of words
cleaned_tweets.putin <- tweets.putin_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.putin)

###### Top 15 words in Vladimir Putin
cleaned_tweets.putin %>%
  dplyr::count(word,sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word,y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x = 'Unique Words', y ='Count', title = "Frequently used words in Vladimir Putin tweets")



### WORD CLOUD########
library(RColorBrewer)
library(wordcloud)

class(cleaned_tweets.putin)

word_cloud1 <- unlist(cleaned_tweets.putin)

wordcloud(word_cloud1)

wordcloud(word_cloud1, min.freq = 1, random.order = FALSE)

wordcloud(word_cloud1, min.freq = 3, random.order = FALSE, scale = c(4,1))

# Add colors to words
pal2 <- brewer.pal(8, "Dark2")
wordcloud(word_cloud1, min.freq = 4, random.order = FALSE , scale = c(4,1), colors = pal2)


#####SENTIMENT ANALYSIS

#install.packages("syuzhet")
library(syuzhet)

# Converting tweets to ASCII to trackle strange characters
cleaned_tweets.putin <- iconv(cleaned_tweets.putin, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
cleaned_tweets.putin <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",cleaned_tweets.putin)

# removing mentions, in case needed
cleaned_tweets.putin <-gsub("@\\w+","",cleaned_tweets.putin)

#Obtain sentimental scores
ew_sentiment<-get_nrc_sentiment((cleaned_tweets.putin))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

#Plotting the sentimental analysis
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores of Vladimir Putin")+
  theme_minimal()
