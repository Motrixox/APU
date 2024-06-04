# Instalacja i zaimportowanie niezbednych pakietow
# install.packages(pkgs=c("tm", "SnowballC", "wordcloud", "RColorBrewer", "syuzhet", "ggplot2"))
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Zaladowanie pliku - tekstu z wikipedii
# Strona: https://en.wikipedia.org/wiki/Poetry
setwd("D:/uczelnia/semestr8/apu_lab6")
text <- readLines("Poetry.txt")
TextDoc <- Corpus(VectorSource(text))

# Wyczyszczenie tekstu
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "ˆa“")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team"))
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, stripWhitespace)
TextDoc <- tm_map(TextDoc, stemDocument)
TextDoc <- tm_map(TextDoc, content_transformer(
  function(x) gsub(x, pattern = "mathemat", replacement = "math")))
TextDoc <- tm_map(TextDoc, content_transformer(
  function(x) gsub(x, pattern = " r ", replacement = " Rlanguage ")))

# Budowanie macierzy dokumentu
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Pokazanie 5 najczestszych slow
head(dtm_d, 5)

barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen",
        main ="20 najczestszych slow w artykule Poetry",
        ylab = "Czestotliwosc slow")

# Chmura slow
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, scale=c(5,0.5),
          min.freq = 1,
          max.words=100, random.order=FALSE,
          rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))

# Kojarzenie slow
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 30),
           corlimit = 0.5)

# Analiza sentymentu
# syuzhet
syuzhet_vector <- get_sentiment(text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

# affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# Analiza emocji
d<-get_nrc_sentiment(as.vector(dtm_d$word)) 
head (d,10)

td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[1:56]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment,
          ylab="count")+ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage"
)

# Grafy powiazan
# install.packages(pkgs=c("tidytext", "igraph", "ggraph"))
library("tidytext")
library("igraph")
library("ggraph")

fileName <- "Poetry.txt"
text <- readChar(fileName, file.info(fileName)$size)
library(dplyr)
text_df <- data_frame(line = 1, text = text)
text_df

library(tidytext)
tidy_text <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)

de <- data.frame("thy","OLD_WORDS")
names(de) <- c("word","lexicon")
stop_words <- rbind(stop_words,de)
de <- data.frame("1","OLD_WORDS")
names(de) <- c("word","lexicon")
de <- data.frame("hath","OLD_WORDS")
names(de) <- c("word","lexicon")
de <- data.frame("mar’d","OLD_WORDS")
names(de) <- c("word","lexicon")

stop_words <- rbind(stop_words,de)

tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE)

# Tworzenie bigramow

text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
text_bigrams
text_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# Konstruowanie grafów

bigram_graph <- bigram_counts %>%
  filter(word1 == "poet" | word2 == "lord") %>%
  graph_from_data_frame()

bigram_graph4 <- bigram_counts %>%
  filter(word1 == "poetry" | word2 == "poetry") %>%
  graph_from_data_frame()

bigram_graph5 <- bigram_counts %>%
  filter(word1 == "poet" | word2 == "poet") %>%
  graph_from_data_frame()

# Wyświetlenie grafów

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

dev.new()
ggraph(bigram_graph4, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), position = "identity") +
  theme_void()

dev.new(width = 550, height = 330, unit = "px")
ggraph(bigram_graph5, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), position = "identity") +
  theme_void()

# pierwszy poziom
bigram_graph1 <- bigram_counts %>%
  filter(word1 %in% c("poetry","uses") | word2 %in%
           c("poetry","uses"))

# drugi poziom
bigram_graph2 <- bigram_counts %>%
  filter(word1 %in% bigram_graph1$word1 | word1 %in%
           bigram_graph1$word2 | word2 %in% bigram_graph1$word1 |
           word2 %in% bigram_graph1$word2)

# trzeci poziom
bigram_graph3 <- bigram_counts %>%
  filter(word1 %in% bigram_graph2$word1 | word1 %in%
           bigram_graph2$word2 | word2 %in% bigram_graph2$word1 |
           word2 %in% bigram_graph2$word2)

bigram_graph
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph1%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.new()
ggraph(bigram_graph2%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.new()
ggraph(bigram_graph3%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()