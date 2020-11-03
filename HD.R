# Load Packages ----
libs = c("tidyverse", "tm", "wordcloud", "wordcloud2", "tidytext", "RColorBrewer", "igraph")
lapply(libs, require, character.only = TRUE)
rm(libs)

# Data Cleaning ----

df = data.frame(
  impact = ceiling(runif(100, min = 0, max  = 5)),
  familiarity = ceiling(runif(100, min = 0, max = 5)),
  norm = ceiling(runif(100, min = -1, max = 1))
) %>% mutate(assess = impact + familiarity)



df %>% ggplot() + geom_point(aes(x = familiarity, y = impact, color = assess, group = assess))





#Create a vector containing only the text
text <- data$text
# Create a corpus  
docs <- Corpus(VectorSource(text))

# If you’re working with tweets, use the following line of code to clean your text.
gsub("https\\S*", "", tweets$text) 
gsub("@\\S*", "", tweets$text) 
gsub("amp", "", tweets$text) 
gsub("[\r\n]", "", tweets$text)
gsub("[[:punct:]]", "", data$text)

# If you’re working with a corpus, there are several packages you can use to clean your text. 
# The following lines of code show you how to do this using the tm package.
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# tidytext
tweets_words <-  tweets %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- tweets_words %>% count(word, sort=TRUE)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))









