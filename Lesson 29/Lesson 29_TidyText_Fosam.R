#---TidyText
#-By: Bonvie Fosam
#-April 2020

#install.packages("tidytext")

library(tidyverse)
library(tidytext)

#set your working directory to where you have the yahoo and reuters data
setwd("/Users/christine.krueger/Documents/")

#load in the data
yahoo_df <- read_csv("yahoo_us_news_april_8_2020.csv")
reuters_df <- read_csv("reuters_df_april_8_2020.csv")

#tokenize the yahoo data and add source as a label
yahoo_token <- yahoo_df %>% 
  unnest_tokens(word, full_text) %>% 
  mutate(source = "yahoo")

#count words in yahoo, what do you notice?
yahoo_token %>% 
  count(word, sort = TRUE)


#get rid of common words
yahoo_freq <- yahoo_token %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50,n)

#plot the frequencies of yahoo words

ggplot(yahoo_freq, aes(reorder(word,n),n)) +
  geom_col() + coord_flip()



######What if you care about titles?
#count words in yahoo, what do you notice?

#get rid of common words
yahoo_freq2 <- yahoo_token %>% 
  anti_join(stop_words) %>% 
  count(word, title, sort = TRUE) %>% 
  group_by(word) %>% 
  mutate(word_count = sum(n)) %>% 
  ungroup() %>% 
  arrange(-word_count) %>% 
  mutate(rank = group_indices(.,factor(word, levels = unique(word)))) %>% 
  filter(rank <= 25)


#plot the frequencies of yahoo words
ggplot(yahoo_freq2, aes(reorder(word,rank),n, fill = title)) +
  geom_col() + theme(legend.text = element_text(size = 6), legend.position = 'bottom')+
  coord_flip()


######
#Ok, now lets start working on the reuters data

#tokenize the text and add a source label as previously done
reuters_token <- reuters_df %>% 
  unnest_tokens(word, text) %>% 
  mutate(source = 'reuters')

#get rid of common words, count them, and keep the top 50
reuters_freq <- reuters_token %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE) %>% 
  top_n(50, n)

#plot the frequencies of reuters words
ggplot(reuters_freq, aes(reorder(word,n),n)) + geom_col() + coord_flip()



#######Bonus Material!!!
#######Inverse Document Frequency
#Get freq counts for the top 250 words from each source
reuters_freq2 <- reuters_df %>% 
  unnest_tokens(word, text) %>% 
  mutate(source = 'reuters')
reuters_freq2 <- reuters_freq2 %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE) %>% 
  top_n(250, n)

yahoo_freq3 <- yahoo_df %>% 
  unnest_tokens(word, full_text) %>% 
  mutate(source = "yahoo")
yahoo_freq3 <- yahoo_freq3 %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  top_n(250,n)

#combine them into one dataframe

combined_df <- reuters_freq2 %>% 
  rbind(yahoo_freq3)

#The statistic tf-idf is intended to measure how 
#important a word is to a document in a collection (or corpus)
#of documents, for example, to one novel in a collection of 
#novels or to one website in a collection of websites.

#calacuate tf_idf

#reuters_tf_idf <- reuters_freq2 %>% 
 # bind_tf_idf(word,title,n)

#length(reuters_freq2)
#for(i in 1: number of rows)

#tf = number of words/number of times the word is there
#idf = log(number of articles/number of articles with that word)


#plot top 15 tf_idf for each source



