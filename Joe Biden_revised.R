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

#Getting access to twitter
setup_twitter_oauth(consumer_key,consumer_secret,access_token=access_token,access_secret = access_token_secret)

#Creating a data frame name biden
#Extracting the tweets of that user
biden <- get_timeline("@JoeBiden",n=180)
View(biden)

#Selecting created_at,text columns from the data frame
tweets.biden = biden %>% select(created_at,text)
tweets.biden
head(tweets.biden)

#Selecting the tweets which we require from the specified dates 
tweets.biden <- filter(tweets.biden,created_at < "2022-04-13 16:11:02" & created_at > "2022-02-24 03:59:36")
head(tweets.biden)
View(tweets.biden)

#Remove http elements manually
tweets.biden$stripped_text1 <- gsub("http\\S+","",tweets.biden$text)
View(tweets.biden)

#unnest_tokens() to convert to lowercase and then tokenize
tweets.biden_stem <- tweets.biden %>%
  select(stripped_text1) %>%
  unnest_tokens(word,stripped_text1)

head(tweets.biden_stem)
View(tweets.biden_stem)

#Remove stop words
cleaned_tweets.biden <- tweets.biden_stem %>%
  anti_join(stop_words)

head(cleaned_tweets.biden)
View(cleaned_tweets.biden)

#Top 15 words (most frequently used) in Joe Biden's tweets
cleaned_tweets.biden %>%
  dplyr::count(word,sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word,y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x = 'Unique Words', y ='Count', title = "Frequently used words in Joe Biden tweets")


### WORD CLOUD########


library(RColorBrewer)
library(wordcloud)

## class of data stored as words. 
#We want the class to be as characters.
class(cleaned_tweets.biden)

##One way is to unlist the list
word_cloud1 <- unlist(cleaned_tweets.biden)

wordcloud(word_cloud1)
wordcloud(word_cloud1, min.freq = 3)

wordcloud(word_cloud1, min.freq = 1, random.order = FALSE)


# Add colors to words
# We want frequent words to be 4 times larger in shape than infrequent ones
pal2 <- brewer.pal(8, "Dark2")
wordcloud(word_cloud1, min.freq = 4, random.order = FALSE , scale = c(4,1), colors = pal2)

######SENTIMENT ANALYSIS

#install.packages("syuzhet")
library(syuzhet)

# Converting tweets to ASCII to tackle strange characters
cleaned_tweets.biden <- iconv(cleaned_tweets.biden, from="UTF-8", to="ASCII", sub="")
head(cleaned_tweets.biden)

# removing re - tweets, in case needed 
cleaned_tweets.biden <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",cleaned_tweets.biden)
head(cleaned_tweets.biden)

# removing mentions, in case needed
cleaned_tweets.biden <-gsub("@\\w+","",cleaned_tweets.biden)

#Obtain sentimental scores
ew_sentiment<-get_nrc_sentiment((cleaned_tweets.biden))
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
  ggtitle("Total sentiment based on scores of Joe Biden")+
  theme_minimal()



