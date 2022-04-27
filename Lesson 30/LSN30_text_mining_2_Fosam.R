#---Text Mining II
#-By: Bonvie Fosam
#-April 13, 2020

#install.packages("textdata")

library(tidyverse)
library(tidytext)
library(textdata)

#---Sentiment Analysis

#---Load the yahoo data (if you need to do so)

yahoo_df <- read_csv("yahoo_us_news_april_8_2020.csv")

#---Tokenize the yahoo data and add source as a label
yahoo_token <- yahoo_df %>% 
  unnest_tokens(word, full_text) %>% 
  mutate(source = 'yahoo')


#---Get the afinn lexicon sentiment scores

get_sentiments('afinn')

#---Join afinn sentiment scores to your tokenized data
yahoo_sent <- yahoo_token %>% 
  inner_join(get_sentiments('afinn'))


#---Create a dply query that groups and summarises
yahoo_sent_sum <- yahoo_sent %>% 
  group_by(title, source) %>% 
  summarise(title_score= sum(value))


#---Plot it
ggplot(yahoo_sent_sum, aes(reorder(title, title_score), title_score, fill = source)) + 
  geom_col() + coord_flip() + theme_minimal()


#-----------------------------------------------------------------------------

#---Bigrams

reuters_df<- read.csv("reuters_df_april_8_2020.csv")

#---tokenize the reuters data and add source as a label
reuters_bigram <- reuters_df %>% 
  unnest_tokens(bigrams, text, token = 'ngrams', n = 2) %>% 
  mutate(source = 'reuters')


#---Count bigrams in reuters, what do you notice?
reuters_bigram %>% 
  count(bigrams, sort = TRUE)


#---Seperate for stop word removal
reuters_bigram_seperated <- reuters_bigram %>% 
  separate(bigrams, c('word1', 'word2'), sep = ' ')


#---Remove stop words
reuters_bigrams_filtered <- reuters_bigram_seperated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)


#---Reunite
reuters_bigram_united <- reuters_bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = ' ')


#---New bigram counts:
reuters_bigram_count <- reuters_bigram_united %>% 
  count(bigram, sort = TRUE)

#---Plot freqs
#---Plot the frequencies of reuters words
reuters_bigram_count %>% 
  top_n(25,n) %>% 
  ggplot(aes(reorder(bigram, n ),n)) + geom_col()+
  coord_flip()+theme_minimal()+ggtitle("Reuters Bigrams- Who is Le Miare")



#---What about sentiments? ---- have to do that by word
reuters_bigram_sentiment <- reuters_bigram_seperated %>% 
  inner_join(get_sentiments('afinn'), by = c(word1 = 'word')) %>% 
  inner_join(get_sentiments('afinn'), by = c(word2 = 'word')) %>% 
  mutate(bigram_score = value.x*value.y)


#---Combine and count
reuters_bigram_sentiment <- reuters_bigram_sentiment %>% 
  unite(bigrams, word1, word2, sep = ' ') %>% 
  count(bigrams, bigram_score)



#---Plot the bigrams by sentiment score
ggplot(reuters_bigram_sentiment, aes(reorder(bigrams, bigram_score), bigram_score, fill = 2))+
  geom_col() + coord_flip()


#-----------------------------------------------------------------------------

#---Now do sentiment analysis for reuters

reuters_df <- read_csv("reuters_df_april_8_2020.csv")


reuters_token <- reuters_df %>% 
  unnest_tokens(word, text) %>% 
  mutate(source = 'reuters')

get_sentiments('afinn')

reuters_sentiments <- reuters_token %>% 
  inner_join(get_sentiments('afinn'))

reuters_sentiments_sum <- reuters_sentiments %>% 
  group_by(title, source) %>% 
  summarize(title_score = sum(value))

ggplot(reuters_sentiments_sum, aes(reorder(title, title_score), title_score, fill = 2)) + 
  geom_col() + coord_flip() + theme_minimal()



# Find the bigrams for yahoo

yahoo_df <- read_csv("yahoo_us_news_april_8_2020.csv")

yahoo_bigram <- yahoo_df %>% 
  unnest_tokens(bigrams, full_text, token = 'ngrams', n = 2) %>% 
  mutate(source = 'yahoo')


yahoo_bigram_separate <- yahoo_bigram %>% 
  separate(bigrams, c('word1', 'word2'), sep = ' ')

yahoo_bigram_filter <- yahoo_bigram_separate %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

yahoo_bigram_unite <- yahoo_bigram_filter %>% 
  unite(bigram, word1, word2, sep = ' ')


yahoo_bigram_count <- yahoo_bigram_unite %>% 
  count(bigram, sort = TRUE) 

yahoo_bigram_count %>% 
  top_n(25,n) %>% 
  ggplot(aes(reorder(bigram, n), n)) + geom_col() +
  coord_flip() + theme_minimal()




#---Bind Reuters and yahoo together and plot it
total_sent_sum <- rbind(yahoo_sent_sum, reuters_sentiments_sum)

ggplot(total_sent_sum, aes(reorder(title, title_score), title_score, fill = source))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  ggtitle("Who Is The Most Positive / Negative?")


