setwd("~/Desktop/test")

library(pdftools)
library(tidytext)
library(dplyr)
library(plotly)
library(wordcloud)

pdf_file <- "Data_Resume_Marcelo_13_Apr_2019.pdf"

my_pdf_text <- pdf_text(pdf_file)

mydf <- data_frame(line=1, text=my_pdf_text)

seevee <- mydf %>%
  unnest_tokens(word, text) %>%
  count(word, sort=T) %>%
  ungroup()

word_counts <- seevee %>%
  anti_join(stop_words)

nrc_counts <- seevee %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nrc_table <- nrc_counts %>%
  count(sentiment,sort=T) %>%
  ungroup()

afinn_counts <- seevee %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort=T) %>%
  ungroup()

bing_counts <- seevee %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

## Plots

plot_ly(x = nrc_table$sentiment,
        y = nrc_table$n,
        type='bar') %>%
  layout(title = "Main sentiments by keywords")

set.seed(1234)
wordcloud(words = word_counts$word, freq = word_counts$n, min.freq = 1,scale=c(3,.3),
          max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))