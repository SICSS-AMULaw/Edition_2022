#Exercise 1.

library(readr)
library(stringr)
exercise1 <- read.csv("exercise1")$x
extract <- str_extract(string = exercise1, pattern = "Article \\d")
random_order <- extract[sample(1:length(extract), length(extract))]
indexes <- str_order(random_order)
random_order[indexes]

#Exercise 2.

library(readr)
library(stringr)
exercise1 <- read.csv("exercise1")$x
remove1 <- str_remove_all(string = exercise1, pattern = "\n")
remove2 <- str_remove_all(string = remove1, pattern = "(?<=(Article \\d)).{1,}")
random_order <- remove2[sample(1:length(remove2), length(remove2))]
str_sort(random_order)

#Exercise 3.

library(rvest)
library(stringr)
library(dplyr)
url <- "https://en.wikipedia.org/wiki/Member_states_of_the_United_Nations" 
url %>%
  read_html() %>%
  html_node(xpath='//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill=TRUE) -> table
table <- table[,-3]
table$`Member state` <- str_remove(string = table$`Member state`, pattern = "\\[note \\d\\]")
table

#Exercise 4.

library(eurlex)
library(textstem)
library(tidytext)
library(wordcloud2)
library(dplyr)

elx_make_query(resource_type = "regulation",
               directory = "16") %>% 
  elx_run_query() -> dataset

dataset <- dataset[match("32021R2282", dataset$celex),]

legal.act <- elx_fetch_data(dataset$work,
                            type = "text",
                            language_1 = "en")

legal.act <- data.frame(text = legal.act)

legal.act %>%
  unnest_tokens(word, text, token = "ngrams", n = 1) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) -> legal.act

legal.act$word <- ifelse(str_remove_all(legal.act$word, "\\d") == "",
                         NA,
                         str_remove_all(legal.act$word,"\\d"))

legal.act <- na.omit(legal.act)

legal.act %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:72) -> legal.act.frequency

wordcloud2(legal.act.frequency)

#Exercise 5.

library(readr)
library(eurlex)
library(tidytext)
library(stringr)
library(ggplot2)
library(textstem)
library(dplyr)

exercise5 <- read.csv("exercise5")$x

elx_make_query(resource_type = "any",
               directory = "1607") %>% 
  elx_run_query() -> dataset

dataset <- dataset[match(exercise5, dataset$celex),]

legal.acts <- data.frame(CELEX=dataset$celex,
                         text=unlist(lapply(dataset$work,elx_fetch_data,
                                            language_1 = "en",
                                            type = "text")))

legal.acts$text <- str_remove_all(legal.acts$text,"\\d")

legal.acts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>% 
  group_by(CELEX, word)%>%
  summarise(n = n())%>%
  arrange(desc(n)) -> act.word

act.word %>%
  bind_tf_idf(word, CELEX, n) -> act.word.2

act.word.2 %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(CELEX) %>% 
  slice(1:6) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = CELEX)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~CELEX, ncol = 2, scales = "free") +
  coord_flip()+
  theme_bw()

#Exercise 6.

#Hello, Buddy! If you thought that you should have use LDA, then sorry, you were wrong. I'm little mean. The answer is nothing more than pattern recognition. ;)

#Import data to R environment.

dataset <- read.csv("exercise6")$x
dataset <- str_remove(string = dataset, pattern = "CELEX number: ")
uniques <- !duplicated(str_sub(dataset, start = 6L, end = 6L))
coding <- str_sub(dataset, start = 6L, end = 6L)
unique_codes <- coding[uniques]

#Here you can find information about coding in CELEX numbers

library(rvest)
library(stringr)
library(dplyr)
url <- "https://eur-lex.europa.eu/content/tools/TableOfSectors/types_of_documents_in_eurlex.html"
url %>%
  read_html() %>%
  html_node(xpath='//*[@id="content"]/div/div/div/div[3]/table') %>%
  html_table(fill=TRUE) -> table
codes <- table[table$X2 %in% unique_codes,]
codes <- codes[,-1]
codes$X3 <- str_remove(string = codes$X3, pattern = " \\(with or without addressee\\)")
codes$X3 <- str_remove(string = codes$X3, pattern = " published in OJ C")
names(codes) <- c("code", "object")

codes

#The rest is quite easy.

decoding <- function(x)
{
  return(codes$object[match(x,codes$code)])
}

answer <- data.frame(CELEX = dataset, code = coding)

answer %>%
  mutate(object = decoding(code)) -> answer

answer <- answer[,-2]

answer

