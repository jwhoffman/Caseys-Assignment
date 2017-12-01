library(syuzhet)
library(SnowballC)
library(RMySQL)
library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(stringr)
library(ROAuth)
library(tidytext)
library(readr)
library(stringdist)
library(ggplot2)
library(ggiraph)

data <- read.csv("data.csv")
stop <- read.csv("stopwords.csv")

# Create unique IDs

colnames(data)[1] <- "ID"

# Remove retweets

data <- data[!grepl("^rt", data$Contents), ]

# Remove tweets with web content (majority of them are irrelevant)

data <- data[!grepl("http", data$Contents), ]

# Use regular expressions to separate tweets by product

data$jager <-
  ifelse((
    grepl("jager", data$Contents, fixed = TRUE) == TRUE |
      grepl("yager", data$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

data$jack <-
  ifelse((
    grepl("jd", data$Contents, fixed = TRUE) == TRUE |
      grepl("jack", data$Contents, fixed = TRUE) == TRUE |
      grepl("daniel", data$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

data$fireball <-
  ifelse((
    grepl("fireball", data$Contents, fixed = TRUE) == TRUE |
      grepl("firebal", data$Contents, fixed = TRUE) == TRUE |
      grepl("fire ball", data$Contents, fixed = TRUE) == TRUE |
      grepl("fire bal", data$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

jager <- filter(data, jager == 1)

jack <- filter(data, jack == 1)

fireball <- filter(data, fireball == 1)

other <- filter(data, jager == 0 & jack == 0 & fireball == 0)

jack$jackfire <-
  ifelse((
    grepl("fire", jack$Contents, fixed = TRUE) == TRUE |
      grepl("fogo", jack$Contents, fixed = TRUE) == TRUE |
      grepl("cinnamon", jack$Contents, fixed = TRUE) == TRUE |
      grepl("canel", jack$Contents, fixed = TRUE) == TRUE |
      grepl("red", jack$Contents, fixed = TRUE) == TRUE |
      grepl("vermelho", jack$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

jack$jackhoney <-
  ifelse((
    grepl("honey", jack$Contents, fixed = TRUE) == TRUE |
      grepl("mel", jack$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

jackfire <- filter(jack, jackfire == 1)

jackhoney <- filter(jack, jackhoney == 1)

# The fireball df has a lot of irrelevant tweets. Some appear to be songs and headlines, as well as other random stuff.

fireball$song <- ifelse((grepl("nicki", fireball$Contents, fixed = TRUE) == TRUE |
                           grepl("minaj", fireball$Contents, fixed = TRUE) == TRUE |
                           grepl("willow", fireball$Contents, fixed = TRUE) == TRUE |
                           grepl("smith", fireball$Contents, fixed = TRUE) == TRUE |
                           (grepl("evening", fireball$Contents, fixed = TRUE) == TRUE &
                              grepl("throne", fireball$Contents, fixed = TRUE) == TRUE)
                         ), 1, 0)

sum(fireball$song)

fireball$headline <- ifelse(((grepl("sky", fireball$Contents, fixed = TRUE) == TRUE &
                               grepl("freaked", fireball$Contents, fixed = TRUE) == TRUE) |
                               (grepl("southern", fireball$Contents, fixed = TRUE) == TRUE &
                               grepl("california", fireball$Contents, fixed = TRUE) == TRUE) |
                               grepl("meteor", fireball$Contents, fixed = TRUE) == TRUE
), 1, 0)

sum(fireball$headline)

fireball$random <- ifelse((grepl("frozen", fireball$Contents) == TRUE &
                              grepl("surface", fireball$Contents) == TRUE),1,0)

sum(fireball$random)

fireball <- filter(fireball, song != 1 & headline != 1 & random != 1)

fireball <- unique(fireball)

# Some people misspell Mick Jagger :/

jager$mick <- ifelse(((grepl("mick", jager$Contents) == TRUE) &
                       grepl("jager", jager$Contents) == TRUE),1,0)

sum(jager$mick)

jager <- filter(jager, mick != 1)

# DFs for sentiment analysis = jager, fireball, jackfire, jackhoney -> compare sentiment across brands before/after launch.
# Compare sentiment of honey in 2014 to fire after launch.

# Consumer insights -> whiskey and beer, whiskey and coconut water?

other$coco <-
  ifelse((
    grepl("gelo de coco", other$Contents, fixed = TRUE) == TRUE |
      grepl("coco", other$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

gelodecoco <- filter(other, coco == 1)

other$beer <-
  ifelse(((
    grepl("whiskey", other$Contents, fixed = TRUE) == TRUE |
      grepl("uisque", other$Contents, fixed = TRUE) == TRUE |
      grepl("shot", other$Contents, fixed = TRUE) == TRUE |
      grepl("dose", other$Contents, fixed = TRUE) == TRUE |
      grepl("whisky", other$Contents, fixed = TRUE) == TRUE |
      grepl("whisk", other$Contents, fixed = TRUE) == TRUE
  ) &
    grepl("beer", other$Contents, fixed = TRUE) == TRUE |
    grepl("cerveja", other$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

cerveja <- filter(other, beer == 1)

uncategorized <- filter(other, coco == 0 & beer == 0)

# Write important dfs to csv

write_csv(jackfire, "jackfire.csv")
write_csv(jackhoney, "jackhoney.csv")
write_csv(fireball, "fireball.csv")
write_csv(jager, "jager.csv")
write_csv(cerveja, "cerveja.csv")
write_csv(gelodecoco, "gelodecoco.csv")

# Specific insights for Marissa = whiskey and coconut water & shot of whiskey and beer. Can we find any trends? What is the sentiment?


# Distance Based Clustering

# Clustering jack brands (honey and fire)

distancemodels <-
  stringdistmatrix(jack$Contents, jack$Contents, method = "cosine", q = 2)

rownames(distancemodels) <- jack$Contents

hc <- hclust(as.dist(distancemodels))

dfClust <- data.frame(jack$Contents, cutree(hc, k = 10))

names(dfClust) <- c("modelname", "cluster")

plot(table(dfClust$cluster))

jack$cluster <- dfClust$cluster

clust1 <- filter(jack, cluster == 1)

# Overall word cloud

docs <- Corpus(VectorSource(data$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

wordcloud(
  d$word,
  d$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)


# Jack Fire word cloud

docs <- Corpus(VectorSource(jackfire$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)
docs <-
  tm_map(
    docs,
    removeWords,
    c(
      "jackdaniels",
      "jackdanielsfire",
      "jackfire",
      "jackdanielswhiskey",
      "jackdanielstennessee",
      "nois",
      "whiskey",
      "whisky",
      "uisque",
      "pra",
      "jack",
      "red",
      "canela",
      "fire"
    )
  )

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

wordcloud(
  d$word,
  d$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

# Jack Honey word cloud

docs <- Corpus(VectorSource(jackhoney$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)
docs <-
  tm_map(
    docs,
    removeWords,
    c(
      "jack",
      "daniel",
      "daniels",
      "honey",
      "mel",
      "jackdaniels",
      "jackdanielshoney",
      "jackhoney",
      "jackdanielswhiskey",
      "jackdanielstennessee",
      "nois",
      "whiskey",
      "whisky",
      "uisque",
      "pra",
      "jackdemel"
    )
  )

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

wordcloud(
  d$word,
  d$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

# Fireball word cloud

docs <- Corpus(VectorSource(unique(fireball$Contents)))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stopwords)
docs <-
  tm_map(
    docs,
    removeWords,
    c(
      "fireball",
      "firebal",
      "fire",
      "ball",
      "cinnamon",
      "canela",
      "nois",
      "whiskey",
      "whisky",
      "uisque",
      "pra"
    )
  )

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

wordcloud(
  d$word,
  d$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

# Jager word cloud

docs <- Corpus(VectorSource(jager$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stopwords)
docs <-
  tm_map(
    docs,
    removeWords,
    c(
      "jager",
      "yager",
      "jagermeister",
      "yagermeister",
      "jagermister",
      "nois",
      "pra",
      "jagerbomb",
      "bombs"
    )
  )

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

wordcloud(
  d$word,
  d$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

unique(fireball$Contents)


# Word Associations and Topic Modeling

docs <- Corpus(VectorSource(data$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stopwords)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

findAssocs(dtm, "jackhoney", 0.2)
findAssocs(dtm, "jackfire", 0.2)
findAssocs(dtm, "jager", 0.2)

dtm <- as.DocumentTermMatrix(dtm)
install.packages("topicmodels")
library(topicmodels)

# Latent Dirichlet Allocation model for ALL TWEETS
# Will need to do this individually by brand (or at least the Jack brands) for consumer insights

lda <- LDA(dtm, k = 8)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)
topics <- data.frame(date = data$Date, topic = topics)

topics$date1 <- as.character(topics$date)
topics$date <- as.Date(topics$date1)

topics <- filter(topics, date >= "2017-01-01")

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

# Jack LDA

docs <- Corpus(VectorSource(jack$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stopwords)
docs <-
  tm_map(
    docs,
    removeWords,
    c(
      "jack",
      "daniel",
      "daniels"
      "jackdaniels",
      "jackdanielswhiskey",
      "jackdanielstennessee",
      "nois",
      "whiskey",
      "whisky",
      "uisque",
      "pra"
    )
  )

dtm <- TermDocumentMatrix(docs)
dtm <- as.DocumentTermMatrix(dtm)

lda <- LDA(dtm, k = 8)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)
topics <- data.frame(date = jack$Date, topic = topics)

topics$date1 <- as.character(topics$date)
topics$date <- as.Date(topics$date1)

topics <- filter(topics, date >= "2017-01-01")

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

