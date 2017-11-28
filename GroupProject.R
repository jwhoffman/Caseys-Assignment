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

data <- read.csv("data.csv")
stop <- read.csv("stopwords.csv")

# Remove retweets

data <- data[!grepl("^rt", data$Contents), ]

# Additional cleaning that was missed previously

data$Contents <- gsub("ú", "u", data$Contents)
data$Contents <- gsub("ä", "a", data$Contents)
data$Contents <- gsub("ó", "o", data$Contents)
data$Contents <- gsub("ô", "o", data$Contents)
data$Contents <- gsub("à", "a", data$Contents)

# Clean the stopwords

stop$stopwords <- gsub("ú", "u", stop$stopwords)
stop$stopwords <- gsub("ä", "a", stop$stopwords)
stop$stopwords <- gsub("ó", "o", stop$stopwords)
stop$stopwords <- gsub("ô", "o", stop$stopwords)
stop$stopwords <- gsub("á", "a", stop$stopwords)
stop$stopwords <- gsub("ã", "a", stop$stopwords)
stop$stopwords <- gsub("é", "e", stop$stopwords)
stop$stopwords <- gsub("í", "i", stop$stopwords)
stop$stopwords <- gsub("ê", "e", stop$stopwords)
stop$stopwords <- gsub("à", "a", stop$stopwords)

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
  ),
  1, 0)

jack$jackhoney <-
  ifelse((
    grepl("honey", jack$Contents, fixed = TRUE) == TRUE |
      grepl("mel", jack$Contents, fixed = TRUE) == TRUE
  ), 1, 0)

jackfire <- filter(jack, jackfire == 1)
jackhoney <- filter(jack, jackhoney == 1)


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







# Jack Fire word cloud

docs <- Corpus(VectorSource(jackfire$Contents))
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
      "daniels",
      "fire",
      "canela",
      "caneleira",
      "fogo",
      "red",
      "jackdaniels",
      "jackdanielsfire",
      "jackfire",
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
docs <- tm_map(docs, removeWords, stop$stopwords)
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
