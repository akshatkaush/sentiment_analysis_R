library(tidytext) #text analysis(consistes of negative and positive words)
library(tidyverse) #data manipulation,exploration and visualization
library(wordcloud) 
library(sentimentr) #get_sentiments()
library(reshape2) #acast
library(dplyr) #%>% data manipulation-->filter mutate
library(ggplot2)

#read the dataset
amazon.df <- read.csv("amazon.csv")
df = amazon.df
summary(df)

#replace breaks if any with single space
df = df %>% mutate(review_body = str_replace_all(df$review_body, "(<br />)+", " ")) 
head(df,5)

#PLOT 1-->To visualize the frequency of the 5 ratings no of products corresponding to each rating
ggplot(data = df, mapping = aes(x = star_rating, fill = as.factor(star_rating))) + geom_bar() + labs(x = "Star Rating", y = "Frequency", fill = "Star Rating")


#Plot 2-->To visualize the number of helpful votes in reviews
ggplot(data = df, mapping = aes(x = helpful_votes, y = star_rating, color = star_rating)) + 
  layer(geom = "point", stat="identity", position = position_dodge(width = 0.1)) +
  labs(x="Number of helpful votes", y= "Star rating", color = "Star rating") 


#Plot 3-->To visualize the number of unverified purchases per star rating
ggplot(data = df, aes(x = star_rating)) + 
  geom_bar(aes(fill = verified_purchase)) +
  labs(x = "Star Rating", y = "Frequency", fill = "Verified Purchase")



#DATA ANALYSIS USING NLP

#DATA CLEANING
#tokenize
tokens <- df %>% unnest_tokens(output = word, input = review_body)
rmarkdown::paged_table(tokens)
length(tokens$word)
length(unique(tokens$word))

#remove stopwords using anti_join
cleaned_tokens <- tokens %>% anti_join(get_stopwords())
length(unique(cleaned_tokens$word))

#remove numbers-->not significant
nums <- cleaned_tokens %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
cleaned_tokens <- cleaned_tokens %>% anti_join(nums, by = "word")
length(unique(cleaned_tokens$word))


#Plot 4-->create a wordcloud of top 100 most common words
pal <- brewer.pal(8,"PiYG")
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))


#mark sentiments for each word in word column
#get_sentiments("nrc")
#get_sentiments("bing")

#create 2 columns nrc and bing renamed from sentiment
sentiment_reviews = cleaned_tokens %>%
  left_join(get_sentiments("nrc")) %>%
  rename(nrc = sentiment) %>%
  left_join(get_sentiments("bing")) %>%
  rename(bing = sentiment)

head(sentiment_reviews)

#count the frequency of words if not na from bing column
polarity_word_counts <- sentiment_reviews %>% filter(!is.na(bing)) %>% count(word, bing, sort = TRUE)
head(polarity_word_counts)


#Plot 5--> To visualize how the words contibute to the sentiments
polarity_word_counts %>%
  filter(n >= 10) %>%
  mutate(n = ifelse(bing == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = bing)) +
  geom_col() +
  coord_flip() +
  scale_y_discrete(labels = abbreviate) +
  labs(y = "Contribution to sentiment")


#Plot 6-->wordcloud of positive and negative words in reviews
library(reshape2)
sentiment_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, bing, sort = TRUE) %>%
  acast(word ~ bing, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "magenta"),
                   max.words = 100)