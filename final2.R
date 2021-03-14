library(readtext)
library(readr)
library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(stringr)
library(quanteda)
library(SnowballC)
#library(syuzhet)
library(stm)

folder<-"G:\\AI_Impact_PaperData"
filelist <- list.files(path = folder, pattern = "*.txt")
filelist <- paste(folder, "\\", filelist, sep="")
ai <-lapply(filelist, FUN = readLines)
myfile <- lapply(ai, FUN = paste, collapse=" ")

#creating tokens

mydata <- VCorpus(VectorSource(myfile))
mydata <- tm_map(mydata, removeWords, stopwords("english"))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
mydata <- tm_map(mydata, toSpace, "-")
mydata <- tm_map(mydata, toSpace, ",")
mydata <- tm_map(mydata, toSpace, ".")
mydata <- tm_map(mydata, toSpace, ";")                 
toks<-corpus(mydata)
corp_toks <- tokens(toks)
print(corp_toks)                        

kw_pattern1 <- kwic(corp_toks, pattern = phrase('employment*'), window = 20)
head(kw_pattern1, 10)
kw_pattern2 <- kwic(corp_toks, pattern = phrase('artificial*'), window = 20)
kw_pattern3 <- kwic(corp_toks, pattern = phrase('replace*'), window = 20)
kw_pattern4 <- kwic(corp_toks, pattern = phrase('automation*'), window = 20)
kw_pattern5 <- kwic(corp_toks, pattern = phrase('*jobs*'), window=20)
kw_pattern6 <- kwic(corp_toks, pattern = phrase('skills*'), window = 20)
kw_pattern7 <- kwic(corp_toks, pattern = phrase('new*'), window = 20)
kw_pattern8 <- kwic(corp_toks, pattern = phrase('unemployment*'), window=20)
kw_pattern9 <- kwic(corp_toks, pattern = phrase('create*'), window = 20)
kw_pattern10 <- kwic(corp_toks, pattern = phrase('improve*'), window = 20)
kw_pattern11 <- kwic(corp_toks, pattern = phrase('robots*'), window = 20)

View(kw_pattern1)
View(kw_pattern2)
View(kw_pattern3)
View(kw_pattern4)
View(kw_pattern5)
View(kw_pattern6)
View(kw_pattern7)
View(kw_pattern8)
View(kw_pattern9)
View(kw_pattern10)
View(kw_pattern11)
x<-tokens_remove(corp_toks, removeWords, pattern=("will"))
mycorpus <- dfm(corp_toks,remove = stopwords("english"),  remove_punct = TRUE)
x<-topfeatures(mycorpus,20)


#build corpus
mydata <- VCorpus(VectorSource(myfile))


#clean corpus
mydata <- tm_map(mydata, content_transformer(tolower))
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#mydata <- tm_map(mydata, content_transformer(removeNumPunct))
mydata <- tm_map(mydata, removeWords, stopwords("english"))
mydata <- tm_map(mydata, stripWhitespace)
mydata <- tm_map(mydata, removeNumbers)
mydata <- tm_map(mydata, removePunctuation)
#mydata <- tm_map(mydata, stemDocument)
#mydata<- tm_map(mydata, removeWords, c('will','can','jobs'))

#toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#mydata <- tm_map(mydata, toSpace, "-")
#mydata <- tm_map(mydata, toSpace, ",")
#mydata <- tm_map(mydata, toSpace, ".")
#mydata <- tm_map(mydata, toSpace, ";")

#define and eliminate all custom stop words
myStopwords <- c("will","market","two","value","still","see","going","according","now","workplace","companies","economy","opportunities",
                 "productivity","industries","workforce","machine","example","require","education","increase","businesses","change","created",
                 "wave","s","ai","based","us","however","automated","technologies","humans","human","world","future","work","need","become","years","report","use","time","percent",
                 "research","million","even","already","digital","make","however","better","society","may","tech","use","ai","say","job","can",
                 "creating","ensure","new","jobs","also","roles","just","well","pwc","next","one","help","way","able","take","like","human","many","people","data","said","likely")
mydata <- tm_map(mydata, removeWords, myStopwords)

mydata_dtm <- TermDocumentMatrix(mydata)
data_mat <- as.matrix(mydata_dtm)
data_freq <- sort(rowSums(data_mat),decreasing=TRUE)
freq_df1 <- data.frame(word = names(data_freq),freq=data_freq)
  #x<-data.frame(text = sapply(mydata, as.character), stringsAsFactors = FALSE)
#freq_df2 <- freq_df1[c("automated","replace","employment","unemployment","manufacturing","repetitive","selfdriving","positive","eliminate","displaced","consulting","healthcare","marketing","analysts","drones","engineering","bookeeping","operations","programming","security","driverless","negative","medicine","receptionists","telemarketers","scientists","fear","transportation","writers","accounting","clerical")]
#wordcloud
set.seed(123)
wordcloud(freq_df1$word, freq_df1$freq, random.order = FALSE, max.words = 101, colors = brewer.pal(8, "Dark2"))

findAssocs(mydata_dtm, c("job"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("automation"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("employment"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("unemployment"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("robots"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("impact"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("services"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("workers"), 0.06)[[1]][1:50]
findAssocs(mydata_dtm, c("create"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("tasks"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("skills"), 0.05)[[1]][1:50]
#x<-findAssocs(mydata_dtm, c("artificial","automation","employment","unemployment","robots","impact","services","workers","create"), 0.05)[[1]][1:50]

#x<-findFreqTerms(mydata_dtm, lowfreq = 15, highfreq = 80)
#freq_df2<-freq_df[c("replac","displaced","destroy","create","increase","decrease","training","skills","new","employment")]
#head(corpus_df2, 30)

freq_tab<-freq_df1[order(-freq_df1$freq),][1:20,]
freq_plot <- ggplot(freq_tab, aes(x = reorder(word, -freq), freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Words", y = "Frequency", title = "Word Distribution with Frequency") +
  geom_text(aes(label = freq), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
freq_plot

#Category Distribution pie chart
library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))
ggplot(freq_tab, aes(x="", y=freq, fill=reorder(word, -freq))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Category Distribution") +
  theme(legend.title = element_blank()) +
  theme_void()+scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(50))

freq_words<-findFreqTerms(mydata_dtm, lowfreq = 50)
freq1<-dfm(freq_words)
findFreqTerms(dtm, lowfreq=50)

#correlation graph
freq_words<-findFreqTerms(mydata_dtm, lowfreq = 30)
plot(mydata_dtm, term = freq_words, corThreshold = 0.12, 
     weighting = F, attrs=list(node=list(width=20, fontsize=24, fontcolor="blue", color="red")))

#convert corpus to tidy format
mydata_corpus <- mydata %>% tidy()
mydata_corpus

d_corpus <- mydata_corpus %>% 
  select(id, text)
tidy_df <- mydata_corpus %>%
  unnest_tokens(word, text)

tidy_df_sent <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) #%>% comparison.cloud(colors = c("gray20", "gray80"),
                                                  # max.words = 100)

tidy_df_sent <- tidy_df_sent %>%
  mutate(sentiment = positive - negative)

tidy_df_sent

ggplot(tidy_df_sent, aes(id, sentiment, fill = id)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bing_word_counts <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#comparing three lexicons for sentiment analysis
afinn <- tidy_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(id ) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_df %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_df %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(id, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


bigrams <- tidy_df %>%
  unnest_tokens(bigram, word, token = "ngrams", n=2)

#bigrams
bigrams %>%
  count(bigram, sort = TRUE)
bigram_tf_idf <- bigrams %>%
  count(id, bigram) %>%
  bind_tf_idf(bigram, id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
igraph::graph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Structured Topic Modelling
library(stm)
toks<-corpus(mydata)
corp_toks <- tokens(toks)
mycorpus <- dfm(corp_toks,remove = stopwords("english"),  remove_punct = TRUE)
quant_dfm <- dfm_trim(mycorpus, min_termfreq = 10, max_docfreq = 20)
topic.count <- 20 # Assigns the number of topics
# Calculate the STM 
dfm2stm <- convert(quant_dfm, to = "stm")

model.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = topic.count,
  data = dfm2stm$meta,
  init.type = "Spectral"
)
#to get an insight into the model
as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
labelTopics(model.stm, c(3, 14, 13, 20))

#plot
plot(
  model.stm,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation"
)

#Using word cloud to visualize different words of a topic
stm::cloud(model.stm,
           topic = 11,  #highest share, same for topic 13
           scale = c(2.25, .5))
#to visualize two topics
plot(model.stm,
     type = "perspectives",
     topics = c(2, 10),
     main = "Putting two different topics in perspective")

library(huge)
cor_topic<-topicCorr(model.stm, method = c("huge"), cutoff = 0.01, verbose = TRUE)
plot(cor_topic)


AP_topics <- tidy(model.stm, matrix = "beta")

ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#For checking relation between coronavirus and artificial intelligence
folder<-"G:\\covid"
filelist <- list.files(path = folder, pattern = "*.txt")
filelist <- paste(folder, "\\", filelist, sep="")
ai <-lapply(filelist, FUN = readLines)
myfile <- lapply(ai, FUN = paste, collapse=" ")

#build corpus
mydata <- VCorpus(VectorSource(myfile))
mydata <- tm_map(mydata, removeWords, stopwords("english"))
                 
toks<-corpus(mydata)
corp_toks <- tokens(toks)
print(corp_toks)                        

kw_pattern1 <- kwic(corp_toks, pattern = phrase('coronavirus*'), window = 20)
head(kw_pattern1)
kw_pattern2 <- kwic(corp_toks, pattern = phrase('medical*'), window=20)
kw_pattern3 <- kwic(corp_toks, pattern = phrase('ai*'), window=20)
kw_pattern4 <- kwic(corp_toks, pattern = phrase('testing*'), window=20)
kw_pattern5 <- kwic(corp_toks, pattern = phrase('*analysis*'), window=20)
kw_pattern6 <- kwic(corp_toks, pattern = phrase('artificial*'), window=20)
kw_pattern7 <- kwic(corp_toks, pattern = phrase('covid*'), window = 20)
kw_pattern8 <- kwic(corp_toks, pattern = phrase('technology*'), window=20)
kw_pattern9 <- kwic(corp_toks, pattern = phrase('diagnosis*'), window=20)


View(kw_pattern1)
View(kw_pattern2)
View(kw_pattern3)
View(kw_pattern4)
View(kw_pattern5)
View(kw_pattern6)
View(kw_pattern7)
View(kw_pattern8)
View(kw_pattern9)

x<-tokens_remove(corp_toks, removeWords, pattern=("will"))
mycorpus <- dfm(corp_toks,remove = stopwords("english"),  remove_punct = TRUE)
x<-topfeatures(mycorpus,20)

#clean corpus
mydata <- tm_map(mydata, content_transformer(tolower))
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#mydata <- tm_map(mydata, content_transformer(removeNumPunct))
mydata <- tm_map(mydata, removeWords, stopwords("english"))
mydata <- tm_map(mydata, stripWhitespace)
mydata <- tm_map(mydata, removeNumbers)
mydata <- tm_map(mydata, removePunctuation)
#mydata <- tm_map(mydata, stemDocument)
mydata<- tm_map(mydata, removeWords, c("will","the","can","conference","ai","also","just","well","pwc","next","one","help","way","able","take","like","many","people","data","said","likely"))

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
mydata <- tm_map(mydata, toSpace, "-")
mydata <- tm_map(mydata, toSpace, ",")
mydata <- tm_map(mydata, toSpace, ".")
mydata <- tm_map(mydata, toSpace, ";")

#define and eliminate all custom stop words
myStopwords <- c("will","the","hai","many","key","used","now","use","likely","give","conference","using","can","also","just","well","next","one","way","able","take","like","data","just","still","may","'re","said","like","even","whether","see")
mydata <- tm_map(mydata, removeWords, myStopwords)

mydata_dtm <- TermDocumentMatrix(mydata)
data_mat <- as.matrix(mydata_dtm)
data_freq <- sort(rowSums(data_mat),decreasing=TRUE)
freq_df1 <- data.frame(word = names(data_freq),freq=data_freq)
freq_df2 <- freq_df1[c("automated","replace","employment","unemployment","manufacturing","repetitive","selfdriving","positive","eliminate","displaced","consulting","healthcare","marketing","analysts","drones","engineering","bookeeping","operations","programming","security","driverless","negative","medicine","receptionists","telemarketers","scientists","fear","transportation","writers","accounting","clerical")]
#wordcloud
set.seed(100)
wordcloud(freq_df1$word, freq_df1$freq, random.order = FALSE, max.words = 101, colors = brewer.pal(8, "Dark2"))

x<-findAssocs(mydata_dtm, c("coronavirus","artificial","covid-19","covid","virus","technology","diagnosis"), 0.05)
findAssocs(mydata_dtm, c("coronavirus"), 0.05)[[1]][1:50]
findAssocs(mydata_dtm, c("artificial"), 0.05)[[1]][1:100]
findAssocs(mydata_dtm, c("covid"), 0.05)[[1]][1:100]
findAssocs(mydata_dtm, c("technology"), 0.05)[[1]][1:50]

#correlation graph
freq_words<-findFreqTerms(mydata_dtm, lowfreq = 15)
plot(mydata_dtm, term = freq_words, corThreshold = 0.12, 
     weighting = F, attrs=list(node=list(width=20, fontsize=24, fontcolor="blue", color="red")))
#x<-findFreqTerms(mydata_dtm, lowfreq = 15, highfreq = 80)
#freq_df2<-freq_df[c("replac","displaced","destroy","create","increase","decrease","training","skills","new","employment")]
#head(corpus_df2, 30)

freq_tab<-freq_df1[order(-freq_df1$freq),][1:20,]
freq_plot <- ggplot(freq_tab, aes(x = reorder(word, -freq), freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Words", y = "Frequency", title = "Word Distribution with Frequency") +
  geom_text(aes(label = freq), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
freq_plot

#Category Distribution pie chart
library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))
ggplot(freq_tab, aes(x="", y=freq, fill=reorder(word, -freq))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Category Distribution") +
  theme(legend.title = element_blank()) +
  theme_void()+scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(50))


#Structured Topic Modelling
library(stm)
toks<-corpus(mydata)
corp_toks <- tokens(toks)
mycorpus <- dfm(corp_toks,remove = stopwords("english"),  remove_punct = TRUE)
quant_dfm <- dfm_trim(mycorpus, min_termfreq = 10, max_docfreq = 20)
topic.count <- 10 # Assigns the number of topics
# Calculate the STM 
dfm2stm <- convert(quant_dfm, to = "stm")

model.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = topic.count,
  data = dfm2stm$meta,
  init.type = "Spectral"
)
#to get an insight into the model
as.data.frame(t(labelTopics(model.stm, n = 10)$prob))
labelTopics(model.stm, c(2, 10, 5, 8))

#plot
plot(
  model.stm,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation"
)

#Using word cloud to visualize different words of a topic
stm::cloud(model.stm,
           topic = 2,  #highest share, same for topic 13
           scale = c(2.25, .5))
#to visualize two topics
plot(model.stm,
     type = "perspectives",
     topics = c(7, 9),
     main = "Putting two different topics in perspective")

library(huge)
cor_topic<-topicCorr(model.stm, method = c("simple"), cutoff = 0.01, verbose = TRUE)
plot(cor_topic)


AP_topics <- tidy(model.stm, matrix = "beta")

ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
