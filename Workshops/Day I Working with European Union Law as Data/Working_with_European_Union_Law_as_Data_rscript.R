#Exercise 1

library(eurlex)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidytext)
library(stringr)
library(textstem)
library(wordcloud2)

elx_make_query(resource_type = "any",
               directory = "12",
               include_force = TRUE) %>% 
  elx_run_query() -> result

result <- subset(result, force=="true")[1:15,]

legal.acts <- data.frame(CELEX=result$celex,
                           text=unlist(lapply(result$work, elx_fetch_data,
                                              language_1 = "en", type = "text")))

legal.acts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>% 
  filter(str_detect(word ,pattern = "\\d", negate = TRUE)) %>% 
  group_by(word)%>%
  summarise(n = n())%>%
  arrange(desc(n)) -> act.word

wordcloud2(act.word)

act.word %>% slice(1:10) -> act.word

act.word$n <- act.word$n/sum(act.word$n)
  
act.word %>%
  ggplot(aes(reorder(word,n), n, fill=word)) +
  geom_col(show.legend = FALSE, color = 'white') +
  labs(x = NULL, y = "frequency") +
  coord_flip()+theme_bw()

#Exercise 2

elx_make_query(resource_type = "regulation", include_date=TRUE, directory="09",
               include_force = TRUE) %>% 
  elx_run_query() -> regulations

regulations %>% 
  filter(as.Date(`callret-3`) >= as.Date("2020-01-01") & force=="true") -> regulations

regulations_new <- data.frame(CELEX=regulations$celex,
                         text=unlist(lapply(regulations$work, elx_fetch_data,
                                            language_1 = "en", type = "text")))

regulations_new %>%  
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>% 
  filter(str_detect(word ,pattern = "\\d", negate = TRUE)) %>% 
  group_by(CELEX, word) %>%
  summarise(n = n()) %>%
  bind_tf_idf(word, CELEX, n) %>% 
  arrange(desc(tf_idf)) %>%
  group_by(CELEX) %>% 
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = CELEX)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~CELEX, ncol = 3, scales = "free") +
  coord_flip()+
  theme_bw()

#Exercise 3
library(lubridate)

elx_make_query(resource_type = "caselaw", include_date=TRUE, directory="15") %>% 
  elx_run_query() -> caselaws

names(caselaws)[4] <- "date"

caselaws <- data.frame(year=year(caselaws$date),
                       CELEX=caselaws$celex,
                              text=unlist(lapply(caselaws$work, elx_fetch_data,
                                                 language_1 = "en",
                                                 type = "text")))

caselaws %>% 
  select(year,text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>% 
  filter(str_detect(word ,pattern = "\\d", negate = TRUE)) %>% 
  group_by(year, word) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(n==max(n)) -> caselaws_max_by_year

print(caselaws_max_by_year[1:12,1:2])

#Exercise 4
### Download 20 regulations. For each regulation title, show EuroVoc keywords. Plot the obtained results grouping them accordingly to the celex identifier. 

elx_make_query(resource_type = "recommendation", include_eurovoc = TRUE,
               limit=20) %>% 
  elx_run_query() -> recommendations

eurovoc_table <- elx_label_eurovoc(uri_eurovoc = recommendations$eurovoc)

recommendations_eurovoc <- cbind(recommendations,
                         unlist(lapply(recommendations$work, elx_fetch_data, 
                                       language_1 = "en", type = "title")))
recommendations_eurovoc %>%
  left_join(eurovoc_table) -> recommendations_eurovoc

recommendations_eurovoc %>%
  ggplot(aes(labels, fill=celex)) +
  geom_bar(show.legend = TRUE, color = 'white')+
  coord_flip()+theme_bw()

