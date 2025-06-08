#' ---
#' title: "Projekt zaliczeniowy: analiza opinii o filmie Mamma Mia!"
#' author: "Kujawska Iga, Skowronek Pola, Stodółka Dominika"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     theme: cerulean
#'     code_folding: show
#' ---

#' # Ustawienia i biblioteki----

knitr::opts_chunk$set(message = FALSE, warning = FALSE)

library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(SentimentAnalysis)

#' # Funkcje pomocnicze ----

data <- read.csv("Mamma_Mia_reviews.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "windows-1250")
data$Overall_Rating <- as.numeric(gsub(",", ".", data$Overall_Rating))
corpus <- VCorpus(VectorSource(data$Review_Text))

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


#usunięcie zbędnych nazw własnych 
corpus <- tm_map(corpus, removeWords, c("movie", "mamma", "film", "mia", "one", "musical", "just"))

corpus <- tm_map(corpus, stripWhitespace)


plot_lda_topics <- function(input_texts, k = 5, title_prefix = "") {
  corpus <- VCorpus(VectorSource(input_texts))
  DTM <- DocumentTermMatrix(corpus)
  lda <- LDA(DTM, k = k, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(
      title = paste("Tematy LDA:", title_prefix),
      x = "Terminy", y = "β (ważność słowa w temacie)"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
}

#' # Wczytywanie danych i przygotowanie korpusu ----


data_pos <- data %>% filter(Overall_Rating >= 7)
data_neg <- data %>% filter(Overall_Rating <= 4)


corpus_neg <- VCorpus(VectorSource(data_neg$Review_Text)) %>% clean_corpus()
corpus_pos <- VCorpus(VectorSource(data_pos$Review_Text)) %>% clean_corpus()

docs_neg <- sapply(corpus_neg, as.character)
docs_pos <- sapply(corpus_pos, as.character)

#' # Częstość słów i chmury słów ----
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

#zliczenie częstości słów w macierzach

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

#Chmura słów
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))

# Top 10
print(head(tdm_df, 10))

#Macierz częstośći TDM z TF-IDF
tdm_tfidf <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf

tdm_tfidf_m <- as.matrix(tdm_tfidf)

v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 10)

#Eksploracyjna analiza danych
wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))

print(head(tdm_tfidf_df, 10))

tdm_tfidf

inspect(tdm_tfidf)


tdm_tfidf_m[1:3, 1:3]

data$Recommended <- ifelse(data$Overall_Rating >= 6, "yes", "no")
data$Recommended <- factor(data$Recommended, levels = c("no", "yes"))

dtm_df <- as.data.frame(t(tdm_tfidf_m))

dtm_df$Recommended <- data$Recommended

dim(dtm_df)


tdm_neg <- TermDocumentMatrix(corpus_neg)
tdm_pos <- TermDocumentMatrix(corpus_pos)


# Lista własnych słów do usunięcia
custom_stopwords <- c("mamma", "mia", "film", "movie", "musical", "just", "really", "like")


corpus_pos <- tm_map(corpus_pos, removeWords, custom_stopwords)
corpus_neg <- tm_map(corpus_neg, removeWords, custom_stopwords)

corpus_pos <- tm_map(corpus_pos, removeWords, stopwords("english"))
corpus_neg <- tm_map(corpus_neg, removeWords, stopwords("english"))

tdm_pos <- TermDocumentMatrix(corpus_pos)
tdm_neg <- TermDocumentMatrix(corpus_neg)

freq_pos <- sort(rowSums(as.matrix(tdm_pos)), decreasing = TRUE)
freq_neg <- sort(rowSums(as.matrix(tdm_neg)), decreasing = TRUE)

df_pos <- data.frame(word = names(freq_pos), freq = freq_pos)
df_neg <- data.frame(word = names(freq_neg), freq = freq_neg)

# Wordcloud
wordcloud(df_pos$word, df_pos$freq, min.freq = 7, colors = brewer.pal(8, "Greens"))
title("Chmura słów - POZYTYWNE recenzje")

wordcloud(df_neg$word, df_neg$freq, min.freq = 7, colors = brewer.pal(8, "Reds"))
title("Chmura słów - NEGATYWNE recenzje")


#' # Analiza tematów LDA ----

plot_lda_topics(docs_neg, k = 4, title_prefix = "Negatywne")
plot_lda_topics(docs_pos, k = 4, title_prefix = "Pozytywne")

#' # Analiza sentymentu NRC ----

nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)
tokens_neg <- tibble(word = names(freq_neg), freq = freq_neg)
tokens_pos <- tibble(word = names(freq_pos), freq = freq_pos)

nrc_sentiment <- bind_rows(
  tokens_neg %>% inner_join(nrc) %>% mutate(type = "Negatywne"),
  tokens_pos %>% inner_join(nrc) %>% mutate(type = "Pozytywne")
)

ggplot(nrc_sentiment %>% count(sentiment, type), aes(x = sentiment, y = n, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Analiza sentymentu NRC", x = "Emocja", y = "Liczba słów") +
  theme_minimal()

#' # Analiza sentymentu Bing ----

bing <- read.csv("bing.csv", stringsAsFactors = FALSE)

bing_sentiment <- bind_rows(
  tokens_neg %>% inner_join(bing) %>% mutate(type = "Negatywne"),
  tokens_pos %>% inner_join(bing) %>% mutate(type = "Pozytywne")
)

ggplot(bing_sentiment %>% count(sentiment, type), aes(x = sentiment, y = n, fill = type)) +
  geom_col(position = "dodge") +
  labs(title = "Analiza sentymentu Bing", x = "Sentyment", y = "Liczba słów") +
  theme_minimal()

#' # Analiza sentymentu AFINN ----

afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)

afinn_sentiment <- bind_rows(
  tokens_neg %>% inner_join(afinn) %>% mutate(type = "Negatywne"),
  tokens_pos %>% inner_join(afinn) %>% mutate(type = "Pozytywne")
)

ggplot(afinn_sentiment, aes(x = value, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Rozkład AFINN", x = "Wartość sentymentu", y = "Liczba słów") +
  theme_minimal()

#' # SentimentAnalysis - słowniki GI, HE, LM, QDAP ----

text_all <- paste(unlist(c(docs_pos, docs_neg)), collapse = " ")
chunks <- unlist(strsplit(text_all, "(?<=[.!?])\\s+", perl = TRUE))
sentiment <- analyzeSentiment(chunks)

sent_gi <- convertToDirection(sentiment$SentimentGI)
sent_he <- convertToDirection(sentiment$SentimentHE)
sent_lm <- convertToDirection(sentiment$SentimentLM)
sent_qdap <- convertToDirection(sentiment$SentimentQDAP)

df_gi <- data.frame(Index = seq_along(sent_gi), Sentiment = sent_gi)
df_he <- data.frame(Index = seq_along(sent_he), Sentiment = sent_he)
df_lm <- data.frame(Index = seq_along(sent_lm), Sentiment = sent_lm)
df_qdap <- data.frame(Index = seq_along(sent_qdap), Sentiment = sent_qdap)

ggplot(df_gi, aes(x = Sentiment)) +
  geom_bar(fill = "darkgreen", alpha = 0.8) +
  labs(title = "Sentyment GI", x = "Kategoria", y = "Liczba") +
  theme_minimal()

ggplot(df_he, aes(x = Sentiment)) +
  geom_bar(fill = "purple", alpha = 0.8) +
  labs(title = "Sentyment HE", x = "Kategoria", y = "Liczba") +
  theme_minimal()

ggplot(df_lm, aes(x = Sentiment)) +
  geom_bar(fill = "orange", alpha = 0.8) +
  labs(title = "Sentyment LM", x = "Kategoria", y = "Liczba") +
  theme_minimal()

ggplot(df_qdap, aes(x = Sentiment)) +
  geom_bar(fill = "blue", alpha = 0.8) +
  labs(title = "Sentyment QDAP", x = "Kategoria", y = "Liczba") +
  theme_minimal()


#' # Eksport wyników ----

write.csv(df_pos, "wyniki_frekwencji_pozytywne.csv", row.names = FALSE)
write.csv(df_neg, "wyniki_frekwencji_negatywne.csv", row.names = FALSE)
write.csv(afinn_sentiment, "wyniki_afinn.csv", row.names = FALSE)

#' # Asocjacje słów w recenzjach ----

tdm <- TermDocumentMatrix(VCorpus(VectorSource(docs_neg)))
target_word <- "music"
cor_limit <- 0.3

associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)

assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)),
  score = assoc_sorted
)

ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#bcbddc", size = 1.1) +
  geom_point(color = "#54278f", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.2, size = 3.5) +
  labs(title = paste0("Asocjacje z terminem: '", target_word, "' w recenzjach NEGATYWNYCH"),
       subtitle = paste0("Próg korelacji ≥ ", cor_limit),
       x = "Siła skojarzenia", y = "Słowo") +
  theme_minimal()

tdm_pos_assoc <- TermDocumentMatrix(VCorpus(VectorSource(docs_pos)))
target_word_pos <- "music"
associations_pos <- findAssocs(tdm_pos_assoc, target_word_pos, corlimit = cor_limit)
assoc_vector_pos <- associations_pos[[target_word_pos]]
assoc_sorted_pos <- sort(assoc_vector_pos, decreasing = TRUE)

assoc_df_pos <- data.frame(
  word = factor(names(assoc_sorted_pos), levels = names(assoc_sorted_pos)),
  score = assoc_sorted_pos
)

ggplot(assoc_df_pos, aes(x = score, y = reorder(word, score))) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a1d99b", size = 1.1) +
  geom_point(color = "#238b45", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.2, size = 3.5) +
  labs(title = paste0("Asocjacje z terminem: '", target_word_pos, "' w recenzjach POZYTYWNYCH"),
       subtitle = paste0("Próg korelacji ≥ ", cor_limit),
       x = "Siła skojarzenia", y = "Słowo") +
  theme_minimal()

#' # Podsumowanie projektu ----
cat("W projekt przeprowadziłyśmy złożoną analizę recenzji kultowego filmu Mamma Mia, wykorzystując metody eksploracji tekstu, sentymentów, jak i modelowania tematów!")

