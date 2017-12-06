
libs <- function() {

    # Data Manipulation
    library(tm)
    library(plyr)
    library(stringr)
    library(tidyverse)
    library(tidytext)
    library(readr)
    
    # Modeling
    library(stringdist)
    library(topicmodels)
    library(stopwords)
    
    # Visualization
    library(wordcloud)
    library(RColorBrewer)
    library(ggplot2)
    library(ggiraph)
    library(plotly)
    library(ggthemes)
    
    # Haven't used yet but maybe
    library(SnowballC)
    library(RMySQL)
}

# Load Libraries

libs()

# Read in data

data <- read.csv("data.csv")
stop <- read.csv("stopwords.csv")

# Create unique IDs

colnames(data)[1] <- "ID"

# Remove retweets
# Run both ways

rt <- data[grepl("^rt", data$Contents),]

data <- data[!grepl("^rt", data$Contents), ]

# Remove Instagram

insta <- data[grepl("instagram", data$URL),]

data <- data[!grepl("instagram", data$URL),]

# Remove tweets with web content (Brenden's image analysis)

web <- data[grepl("http", data$Contents), ]

data <- data[!grepl("http", data$Contents), ]

# Filter to 2017 (include 2014 if distance modeling)

data$Date1 <- as.factor(data$Date)
data$Date1 <- as.character(data$Date1)
data$Date1 <- as.Date(data$Date1, "%Y-%m-%d")

data$Date <- as.Date(data$Date1, "%Y-%m-%d")

data <- filter(data, Date >= "2017-01-01")

# Use regular expressions to separate tweets by product

data$jager <-
  ifelse(
    grepl("jager|jäger|yager|yäger", data$Contents) == TRUE, 1, 0)

data$jack <-
  ifelse(
    grepl("jd|jack|daniel|jack_honey", data$Contents) == TRUE, 1, 0)

data$fireball <-
  ifelse(
    grepl("fireball|firebal", data$Contents) == TRUE, 1, 0)

jager <- filter(data, jager == 1)

jack <- filter(data, jack == 1)

fireball <- filter(data, fireball == 1)

other <- filter(data, jager == 0 & jack == 0 & fireball == 0)

jack$jackfire <-
  ifelse(
    grepl("jack_fire", jack$Contents) == TRUE, 1, 0)

jack$jackhoney <-
  ifelse(
    grepl("jack_honey", jack$Contents, fixed = TRUE) == TRUE, 1, 0)

jackfire <- filter(jack, jackfire == 1)

jackhoney <- filter(jack, jackhoney == 1)

# The fireball df has a lot of irrelevant tweets. Some appear to be songs and headlines, as well as other random stuff.

fireball$song <- ifelse(grepl("nicki|minaj|willow|smith", fireball$Contents) == TRUE &
                           grepl("evening", fireball$Contents) == TRUE &
                          grepl("throne", fireball$Contents) == TRUE, 1, 0)

fireball$nicki <- ifelse(grepl("nicki|minaj|willow|smith", fireball$Contents) == TRUE, 1, 0)

sum(fireball$nicki)

fireball$headline <- ifelse(grepl("freaked", fireball$Contents) == TRUE &
                              grepl("sky", fireball$Contents) == TRUE|
                               grepl("southern", fireball$Contents) == TRUE &
                              grepl("california", fireball$Contents) == TRUE|
                               grepl("meteor", fireball$Contents) == TRUE, 1, 0)

sum(fireball$headline)

fireball$random <- ifelse(grepl("frozen", fireball$Contents) == TRUE &
                            grepl("surface", fireball$Contents) == TRUE,1,0)

sum(fireball$random)

fireball <- filter(fireball, song != 1 & headline != 1 & random != 1, nicki != 1)

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
    grepl("gelo_de_coco", other$Contents, fixed = TRUE) == TRUE |
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

write.csv(jackfire, "jackfire.csv")
write.csv(jackhoney, "jackhoney.csv")
write.csv(fireball, "fireball.csv")
write.csv(jager, "jager.csv")
write.csv(cerveja, "cerveja.csv")
write.csv(gelodecoco, "gelodecoco.csv")

jackfire <- read.csv("jackfire.csv")
jackhoney <- read.csv("jackhoney.csv")
fireball <- read.csv("fireball.csv")
jager <- read.csv("jager.csv")
cerveja <- read.csv("cerveja.csv")
gelodecoco <- read.csv("gelodecoco.csv")

# Specific insights for Marissa = whiskey and coconut water & shot of whiskey and beer. Can we find any trends? What is the sentiment?


                # # Distance Based Clustering
                # 
                # # Clustering jack brands (honey and fire)
                # 
                # distancemodels <-
                #   stringdistmatrix(jack$Contents, jack$Contents, method = "cosine", q = 2)
                # 
                # rownames(distancemodels) <- jack$Contents
                # 
                # hc <- hclust(as.dist(distancemodels))
                # 
                # dfClust <- data.frame(jack$Contents, cutree(hc, k = 10))
                # 
                # names(dfClust) <- c("modelname", "cluster")
                # 
                # plot(table(dfClust$cluster))
                # 
                # jack$cluster <- dfClust$cluster
                # 
                # clust1 <- filter(jack, cluster == 1)
                # 


# Comparing our performance to Crimson Hexagon

jf <- filter(jackfire, Category == "Irrelevant/Off Topic")
jh <- filter(jackhoney, Category == "Irrelevant/Off Topic")
jm <- filter(jager, Category == "Irrelevant/Off Topic")
fb <- filter(fireball, Category == "Irrelevant/Off Topic")

nrow(jf) + nrow(jh) + nrow(jm) + nrow(fb)

nrow(jackfire) + nrow(jackhoney) + nrow(jager) + nrow(fireball)

# Can we also compare performance of sentiment analysis results?


# Word Associations and Topic Modeling

docs <- Corpus(VectorSource(jackfire$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)
docs <- tm_map(docs, removeWords, stopwords$pt)
docs <- tm_map(docs, removeWords, c("pra",
                                    "jackfire",
                                    "jackdanielsfire",
                                    "jackdaniels",
                                    "jack",
                                    "jackdecanela",
                                    "jackcanela",
                                    "jackvermelho",
                                    "jackdanielsdecanela",
                                    "mim",
                                    "daniels",
                                    "whiskey",
                                    "whisky",
                                    "fire",
                                    "jackdanielshoney",
                                    "jackhoney",
                                    "jackdemel"))


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

dtm <- as.DocumentTermMatrix(dtm)

# Latent Dirichlet Allocation model for jackfire

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals != 0, ]  #remove all docs without words

lda <- LDA(dtm, k = 4)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)

jackfire2 <- jackfire[rowTotals!=0,]
topics <- data.frame(date = jackfire2$Date, topic = topics)

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")


# agua de coco appears to be a topic related to jackfire. let's take a closer look at some of these tweets by searching
# in the df viewer.

# After googling some repeated lines, it appears there are some popular songs in Brazil that mentions gelo de coco, Jack, and red.
# This might explain some of the results we are finding. Let's remove the tweets referencing those songs and rerun the
# LDA.

#https://www.google.com/search?rlz=1C1GGRV_enUS765US765&ei=p8gmWvH9BIbzmAHp_ZGwCA&q=mc+mm+social%2C+narga+e+piscina+release+date&oq=mc+mm+social%2C+narga+e+piscina+release+date&gs_l=psy-ab.3..33i160k1.8257.9770.0.9898.13.13.0.0.0.0.180.1181.11j2.13.0....0...1c.1.64.psy-ab..0.12.994...33i21k1.0.BH3psN-dYDY
#https://www.letras.mus.br/mc-pp-da-vs/hoje-e-sem-estresse/
#https://www.vagalume.com.br/mc-kevin/30-minutos.html
#https://www.google.com/search?q=Jack+daniels+red+bull%2C+gelo+de+agua+de+Coco+%C3%A9+o+calmante+ideal+pra+mim+que+ja+nasci+louco&rlz=1C1GGRV_enUS765US765&oq=Jack+daniels+red+bull%2C+gelo+de+agua+de+Coco+%C3%A9+o+calmante+ideal+pra+mim+que+ja+nasci+louco&aqs=chrome..69i57.661j0j7&sourceid=chrome&ie=UTF-8


# Also, Arteks appears to be a brazilian furniture/lamp manufacturer/seller, and they have many posts advertising their
# JD brand light fixtures. Although these are good advertising for the brand, let's remove them for the purpose of this analysis.

#https://www.instagram.com/p/BZ3Ng-RjG3b/

jackfire$song <- ifelse(grepl("gelo", jackfire$Contents) == TRUE &
                          grepl("coco", jackfire$Contents) == TRUE &
                          grepl("jack", jackfire$Contents) == TRUE &
                          grepl("gargalo|gagalo|gargalho|gagalho", jackfire$Contents) == TRUE, 1, 0)


sum(jackfire$song)

jackfire$song2 <- ifelse(grepl("copo", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("dela", jackfire$Contents) == TRUE &
                           grepl("red", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)
sum(jackfire$song2)

jackfire$song3 <- ifelse(grepl("coco", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("bul", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)

sum(jackfire$song3)

jackfire$song4 <- ifelse(grepl("agua", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("bul", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)



jackfire <- filter(jackfire, song != 1 & song2 != 1 & song3 != 1 & song4 != 1)

jackfire$lamp <- ifelse((grepl("arteks", jackfire$Contents) == TRUE),1,0)

jackfire <- filter(jackfire, lamp != 1)


docs <- Corpus(VectorSource(jackfire$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)
docs <- tm_map(docs, removeWords, stopwords$pt)
docs <- tm_map(docs, removeWords, c("pra",
                                    "jackfire",
                                    "jackdanielsfire",
                                    "jackdaniels",
                                    "jack",
                                    "jackdecanela",
                                    "jackcanela",
                                    "jackvermelho",
                                    "jackdanielsdecanela",
                                    "mim",
                                    "daniels",
                                    "whiskey",
                                    "whisky",
                                    "fire",
                                    "jackdanielshoney",
                                    "jackhoney",
                                    "jackdemel"))


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

dtm <- as.DocumentTermMatrix(dtm)

# Latent Dirichlet Allocation model for jackfire

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals != 0, ]  #remove all docs without words

lda <- LDA(dtm, k = 3)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)

jackfire2 <- jackfire[rowTotals!=0,]
topics <- data.frame(date = jackfire2$Date, topic = topics)

topics$date <- as.Date(topics$date)

ggplotly(ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack"))

library(tidytext)

# Top words and their probabilities
# the model computes the probability of that term being generated from that topic

tops <- tidy(lda, matrix = "beta")

top_terms <- tops %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Jack Fire word cloud

jackfiretop <- cbind(jackfire2,topics)


jftop1 <- filter(jackfiretop, topic == 1)
jftop2 <- filter(jackfiretop, topic == 2)
jftop3 <- filter(jackfiretop, topic == 3)
jftop4 <- filter(jackfiretop, topic == 4)

top <- Corpus(VectorSource(jftop1$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                    "jackfire",
                                    "jackdanielsfire",
                                    "jackdaniels",
                                    "jack",
                                    "jackdecanela",
                                    "jackcanela",
                                    "jackvermelho",
                                    "jackdanielsdecanela",
                                    "mim",
                                    "daniels",
                                    "whiskey",
                                    "whisky",
                                    "fire",
                                    "jackdanielshoney",
                                    "jackhoney",
                                    "jackdemel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jftop2$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdaniels",
                                  "jack",
                                  "jackdecanela",
                                  "jackcanela",
                                  "jackvermelho",
                                  "jackdanielsdecanela",
                                  "mim",
                                  "daniels",
                                  "whiskey",
                                  "whisky",
                                  "fire",
                                  "jackdanielshoney",
                                  "jackhoney",
                                  "jackdemel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jftop3$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdaniels",
                                  "jack",
                                  "jackdecanela",
                                  "jackcanela",
                                  "jackvermelho",
                                  "jackdanielsdecanela",
                                  "mim",
                                  "daniels",
                                  "whiskey",
                                  "whisky",
                                  "fire",
                                  "jackdanielshoney",
                                  "jackhoney",
                                  "jackdemel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jftop4$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdaniels",
                                  "jack",
                                  "jackdecanela",
                                  "jackcanela",
                                  "jackvermelho",
                                  "jackdanielsdecanela",
                                  "mim",
                                  "daniels",
                                  "whiskey",
                                  "whisky",
                                  "fire",
                                  "jackdanielshoney",
                                  "jackhoney",
                                  "jackdemel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)




######## Honey

docs2 <- Corpus(VectorSource(jackhoney$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                    "jackhoney",
                                    "jackdanielshoney",
                                    "jackdaniels",
                                    "jack",
                                    "jackdemel",
                                    "jackmel",
                                    "jackdanielsdemel",
                                    "mim",
                                    "jackfire",
                                    "jackdanielsfire",
                                    "jackdecanela",
                                    "honey",
                                    "whiskey",
                                    "whisky",
                                    "daniels"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 3)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

jackhoney2 <- jackhoney[rowTotals!=0,]
topics2 <- data.frame(date = jackhoney2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")

# Looks like honey also has the Arteks advertising. Will need to remove those. 
# Let's examine the tweets about milkshakes. Looks like there is a place called shakeshakemilkshakes posting instagram
# posts that have seemingly nothing to do with jack daniels, but they are using the hashtag.

jackhoney$shake <- ifelse(grepl("shake shake", jackhoney$Contents) == TRUE , 1, 0)


sum(jackhoney$shake)

jackhoney$lamp <- ifelse((grepl("arteks", jackhoney$Contents) == TRUE),1,0)

sum(jackhoney$lamp)

# There's also something weird going on with whatsapp

jackhoney$whatsapp <- ifelse((grepl("pubinho", jackhoney$Contents) == TRUE |
                                grepl("estrelaeshop", jackhoney$Contents) == TRUE),1,0)

sum(jackhoney$whatsapp)

# Actually everything containing whatsapp appears to be selling something. Remove all of these.

jackhoney$whatsapp <- ifelse(grepl("whatsapp", jackhoney$Contents) == TRUE,1,0)

jackhoney<- filter(jackhoney, shake != 1 & lamp != 1 & whatsapp != 1)

# baressp appears to be advertisements from bars. they mention multiple brands and do not lend much insight to jack

jackhoney$bar <- ifelse(grepl("baressp", jackhoney$Contents) == TRUE,1,0)

jackhoney <- filter(jackhoney, bar != 1)

docs2 <- Corpus(VectorSource(jackhoney$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                      "jackhoney",
                                      "jackdanielshoney",
                                      "jackdaniels",
                                      "jack",
                                      "jackdemel",
                                      "jackmel",
                                      "jackdanielsdemel",
                                      "mim",
                                      "jackfire",
                                      "jackdanielsfire",
                                      "jackdecanela",
                                      "honey",
                                      "whiskey",
                                      "whisky",
                                      "daniels"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 2)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

jackhoney2 <- jackhoney[rowTotals!=0,]
topics2 <- data.frame(date = jackhoney2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")

tops2 <- tidy(lda2, matrix = "beta")

top_terms2 <- tops2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Jack Honey word cloud

jackhoneytop <- cbind(jackhoney2, topics2)


jhtop1 <- filter(jackhoneytop, topic == 1)
jhtop2 <- filter(jackhoneytop, topic == 2)
jhtop3 <- filter(jackhoneytop, topic == 3)
jhtop4 <- filter(jackhoneytop, topic == 4)
jhtop5 <- filter(jackhoneytop, topic == 5)


top <- Corpus(VectorSource(jhtop1$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                      "jackhoney",
                                      "jackdanielshoney",
                                      "jackdaniels",
                                      "jack",
                                      "jackdemel",
                                      "jackmel",
                                      "jackdanielsdemel",
                                      "mim",
                                      "jackfire",
                                      "jackdanielsfire",
                                      "jackdecanela",
                                      "honey",
                                      "whiskey",
                                      "whisky",
                                      "daniels",
                                  "mel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jhtop2$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackhoney",
                                  "jackdanielshoney",
                                  "jackdaniels",
                                  "jack",
                                  "jackdemel",
                                  "jackmel",
                                  "jackdanielsdemel",
                                  "mim",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdecanela",
                                  "honey",
                                  "whiskey",
                                  "whisky",
                                  "daniels",
                                  "mel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jhtop3$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackhoney",
                                  "jackdanielshoney",
                                  "jackdaniels",
                                  "jack",
                                  "jackdemel",
                                  "jackmel",
                                  "jackdanielsdemel",
                                  "mim",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdecanela",
                                  "honey",
                                  "whiskey",
                                  "whisky",
                                  "daniels",
                                  "mel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

top <- Corpus(VectorSource(jhtop4$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackhoney",
                                  "jackdanielshoney",
                                  "jackdaniels",
                                  "jack",
                                  "jackdemel",
                                  "jackmel",
                                  "jackdanielsdemel",
                                  "mim",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdecanela",
                                  "honey",
                                  "whiskey",
                                  "whisky",
                                  "daniels",
                                  "mel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)


top <- Corpus(VectorSource(jhtop5$Contents))
top <- tm_map(top, removeNumbers)
top <- tm_map(top, removePunctuation)
top <- tm_map(top, stripWhitespace)
top <- tm_map(top, content_transformer(tolower))
top <- tm_map(top, removeWords, stop$stop)
top <- tm_map(top, removeWords, stopwords$pt)
top <- tm_map(top, removeWords, c("pra",
                                  "jackhoney",
                                  "jackdanielshoney",
                                  "jackdaniels",
                                  "jack",
                                  "jackdemel",
                                  "jackmel",
                                  "jackdanielsdemel",
                                  "mim",
                                  "jackfire",
                                  "jackdanielsfire",
                                  "jackdecanela",
                                  "honey",
                                  "whiskey",
                                  "whisky",
                                  "daniels",
                                  "mel"))

dtmtop <- TermDocumentMatrix(top)
mtop <- as.matrix(dtmtop)
vtop <- sort(rowSums(mtop), decreasing = TRUE)
dtop <- data.frame(word = names(vtop), freq = vtop)
head(dtop, 10)

wordcloud(
  dtop$word,
  dtop$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)


docs2 <- Corpus(VectorSource(fireball$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                      "jackhoney",
                                      "jackdanielshoney",
                                      "jackdaniels",
                                      "jack",
                                      "jackdemel",
                                      "jackmel",
                                      "jackdanielsdemel",
                                      "mim",
                                      "jackfire",
                                      "jackdanielsfire",
                                      "jackdecanela",
                                      "honey",
                                      "whiskey",
                                      "whisky",
                                      "daniels"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 2)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

jackhoney2 <- jackhoney[rowTotals!=0,]
topics2 <- data.frame(date = jackhoney2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")


## Lots of English in here........... strange

fireball$song <- ifelse(grepl("my", fireball$Contents) == TRUE &
                          grepl("sweet", fireball$Contents) == TRUE &
                          grepl("fireball", fireball$Contents) == TRUE,1,0)

fireball$song2 <- ifelse(grepl("its|it's|it|it s|it is", fireball$Contents) == TRUE &
                          grepl("me", fireball$Contents) == TRUE &
                          grepl("fireball", fireball$Contents) == TRUE,1,0)

fireball$song3 <- ifelse(grepl("fireball", fireball$Contents) == TRUE &
                           grepl("lemon", fireball$Contents) == TRUE &
                           grepl("drop", fireball$Contents) == TRUE,1,0)

fireball$random <- ifelse(grepl("game", fireball$Contents) == TRUE &
                            grepl("dj", fireball$Contents) == TRUE &
                            grepl("fireball", fireball$Contents) == TRUE,1,0)

fireball$game <- ifelse(grepl("attack", fireball$Contents) == TRUE &
                         grepl("fireball", fireball$Contents) == TRUE,1,0)

fireball$game2 <- ifelse(grepl("malenagames", fireball$Contents) == TRUE, 1, 0)



fireball <- filter(fireball, song != 1, song2 != 1, song3 != 1, random != 1, game != 1, game2 != 1)

docs2 <- Corpus(VectorSource(fireball$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, stopwords$en)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                      "mim",
                                      "whiskey",
                                      "whisky",
                                      "daniels",
                                      "fireball",
                                      "canela"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 2)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

fireball2 <- fireball[rowTotals!=0,]
topics2 <- data.frame(date = fireball2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")


topsfb <- tidy(lda2, matrix = "beta")

top_termsfb <- topsfb %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_termsfb %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


### Jager

docs2 <- Corpus(VectorSource(jager$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                      "mim",
                                      "whiskey",
                                      "whisky",
                                      "jager",
                                      "jagermeister"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 2)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

fireball2 <- fireball[rowTotals!=0,]
topics2 <- data.frame(date = fireball2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")

# Something weird with videomtv

jager$mtv <- ifelse(grepl("videomtv", jager$Contents) == TRUE, 1, 0)
jager$awards <- ifelse(grepl("iheartawards", jager$Contents) == TRUE, 1, 0)

jager <- filter(jager, mtv != 1, awards != 1)

# bandit and jager is something with a game

jager$game <- ifelse(grepl("jager", jager$Contents) == TRUE &
                       grepl("bandit", jager$Contents) == TRUE,1,0)


jager <- filter(jager, game != 1)

docs2 <- Corpus(VectorSource(jager$Contents))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stop$stop)
docs2 <- tm_map(docs2, removeWords, stopwords$pt)
docs2 <- tm_map(docs2, removeWords, stopwords$en)
docs2 <- tm_map(docs2, removeWords, c("pra",
                                      "mim",
                                      "whiskey",
                                      "whisky",
                                      "jager",
                                      "jagermeister",
                                      "con",
                                      "una",
                                      "del",
                                      "tava",
                                      "los",
                                      "las"))


dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)

dtm2 <- as.DocumentTermMatrix(dtm2)


# Latent Dirichlet Allocation model for jackhoney

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

lda2 <- LDA(dtm2, k = 3)
term2 <- terms(lda2, 7)
(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2)

jager2 <- jager[rowTotals!=0,]
topics2 <- data.frame(date = jager2$Date, topic = topics2)


ggplot(topics2, aes(date, fill = term2[topic])) +
  geom_density(position = "stack")

topsj <- tidy(lda2, matrix = "beta")

top_termsj <- topsj %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_termsj %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




# Reverse!!!!!!!!!!!!!!!!

library(translateR)

getGoogleLanguages()

# portuguese = pt
# english = en

KEY <- ""

translation <- translate(data,
                         Contents,
                         google.api.key = KEY,
                         source.lang = pt,
                         target.lang = en)



data$Contents <- as.character(data$Contents)
sum(nchar(data$Contents))



# Share of tweets


jhoney <- select(jackhoney, Date, Contents)
jfire <- select(jackfire, Date, Contents)
yager <- select(jager, Date, Contents)
fbbb <- select(fireball, Date, Contents)

jhoney$Brand <- "Jack Honey"
jfire$Brand <- "Jack Fire"
yager$Brand <- "Jager"
fbbb$Brand <- "Fireball"

share <- rbind(jhoney, jfire, yager, fbbb)

share$Date <- as.Date(share$Date)

ggplotly(ggplot(share, aes(Date, fill = factor(Brand))) +
           geom_density(position = "fill") +
           scale_fill_manual(values = c("darkorange3", "brown3", "goldenrod2", "darkolivegreen4")) +
           theme_minimal())


share2 <- rbind(jhoney, yager, fbbb)

ggplotly(ggplot(share2, aes(Date, fill = factor(Brand))) +
           geom_density(position = "fill") +
           scale_fill_manual(values = c("darkorange3", "goldenrod2", "darkolivegreen4")) +
           theme_minimal())

fire <- select(fireball, ID, Date, Contents) %>%
  mutate(Brand = "Fireball")

jf <- select(jackfire, ID, Date, Contents) %>%
  mutate(Brand = "JackFire")

fbjf <- rbind(jf, fire)

docs <- Corpus(VectorSource(fbjf$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stop$stop)
docs <- tm_map(docs, removeWords, stopwords$pt)
docs <- tm_map(docs, removeWords, stopwords$en)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v2), freq = v2)




write_excel_csv(jackfire, "jackfire.xls")
write_excel_csv(jackhoney, "jackhoney.xls")
write_excel_csv(fireball, "fireball.xls")
write_excel_csv(jager, "jager.xls")
write_excel_csv(data, "data.xls")


