library(rtweet) ; library(tidyverse) ; library(tidytext) ;
library(wordcloud) ; library(reactable)

tweets_bear <- search_tweets(q = "#bear market", 
                        n = 500,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        lang = "en")
tweets_tsla <- search_tweets(q = "tesla", 
                        n = 500,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        lang = "en")

tweets_tsla %>% 
  sample_n(5) %>%
  select(created_at, screen_name, text, favorite_count, retweet_count)

write_as_csv(tweets_nvax, "tweets_nvax.csv")
write_as_csv(tweets_tsla, "tweets_tsla.csv")

tweets_nvax %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)

tweets_tsla %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)

tweets_nvax %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)

t <- tweets_tsla %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count, status_id)

tweets_nvax %>% 
  arrange(-favorite_count) %>%
  top_n(5, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)

tweets_nvax %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

tweets_nvax %>%
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#novavax") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)

words_nvax <- tweets_nvax %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

words_nvax %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))

words_tsla <- tweets_tsla %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word_tsla, text, token = "tweets") %>%
  filter(!word_tsla %in% stop_words$word_tsla,
         !word_tsla %in% str_remove_all(stop_words$word_tsla, "'"),
         str_detect(word_tsla, "[a-z]"),
         !str_detect(word_tsla, "^#"),         
         !str_detect(word_tsla, "@\\S+")) %>%
  count(word_tsla, sort = TRUE)

words_tsla %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))

reactable(tweets_tsla, filterable = TRUE, searchable = TRUE, bordered = TRUE, 
          striped = TRUE, highlight = TRUE,
          defaultPageSize = 25, showPageSizeOptions = TRUE, 
          showSortable = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), defaultSortOrder = "desc",
          columns = list(
            created_at = colDef(defaultSortOrder = "asc"),
            screen_name = colDef(defaultSortOrder = "asc"),
            text = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
            favorite_count = colDef(filterable = FALSE),
            retweet_count = colDef(filterable =  FALSE),
            urls_expanded_url = colDef(html = TRUE)
            )
          )