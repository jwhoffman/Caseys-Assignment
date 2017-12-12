
libs <- function() {

    # Data Manipulation
    library(tm)
    library(plyr)
    library(stringr)
    library(tidyverse)
    library(tidytext)
    library(readr)
    library(tidyr)
    
    # Modeling
    library(stringdist)
    library(topicmodels)
    library(stopwords)
    library(lexiconPT)
    
    # Visualization
    library(wordcloud)
    library(RColorBrewer)
    library(ggplot2)
    library(ggiraph)
    library(plotly)
    library(ggthemes)
    library(scales)
    
}

# Load Libraries

libs()

# Read in data

data <- read.csv("data.csv")
stop <- read.csv("stopwords.csv")

# Create unique IDs

colnames(data[1]) <- "ID"

# Remove retweets (store in separate df in case you need them)

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

# kkkkkkk is portuguese for "hahaha"

data$Contents <- gsub("[k]{3,}", "hahaha", data$Contents)

data$Contents <- gsub("\\b[ha]{4,}.*", "hahaha", data$Contents)

# Use gsub expressions to separate tweets by product

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

# Clean the brand specific data frames

# The fireball df has a lot of irrelevant tweets. Some appear to be songs and headlines, as well as other random stuff.

fireball$song <- ifelse(grepl("nicki|minaj|willow|smith", fireball$Contents) == TRUE &
                           grepl("evening", fireball$Contents) == TRUE &
                          grepl("throne", fireball$Contents) == TRUE, 1, 0)

fireball$nicki <- ifelse(grepl("nicki|minaj|willow|smith", fireball$Contents) == TRUE, 1, 0)

fireball$headline <- ifelse(grepl("freaked", fireball$Contents) == TRUE &
                              grepl("sky", fireball$Contents) == TRUE|
                               grepl("southern", fireball$Contents) == TRUE &
                              grepl("california", fireball$Contents) == TRUE|
                               grepl("meteor", fireball$Contents) == TRUE, 1, 0)

fireball$random <- ifelse(grepl("frozen", fireball$Contents) == TRUE &
                            grepl("surface", fireball$Contents) == TRUE,1,0)

fireball <- filter(fireball, song != 1 & headline != 1 & random != 1, nicki != 1)

fireball$jutsu <- ifelse(grepl("jutsu", fireball$Contents) == TRUE,1,0)

fireball$purple <- ifelse(grepl("purple", fireball$Contents) == TRUE,1,0)

fireball$gene <- ifelse(grepl("gene", fireball$Contents) == TRUE,1,0)

fireball <- filter(fireball, jutsu != 1 & purple != 1 & gene != 1)

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

fireball$guile <- ifelse(grepl("guile", fireball$Contents) == TRUE, 1, 0)

fireball$one <- ifelse(grepl("one", fireball$Contents) == TRUE, 1, 0)

fireball$she <- ifelse(grepl("she\\b", fireball$Contents) == TRUE, 1, 0)

fireball <- filter(fireball, guile != 1, one != 1, she != 1)

# Some people misspell Mick Jagger :/

jager$mick <- ifelse(((grepl("mick", jager$Contents) == TRUE) &
                       grepl("jager", jager$Contents) == TRUE),1,0)

jager <- filter(jager, mick != 1)

# Something weird with mtv

jager$mtv <- ifelse(grepl("mtv", jager$Contents) == TRUE, 1, 0)

jager$awards <- ifelse(grepl("iheartawards", jager$Contents) == TRUE, 1, 0)

jager <- filter(jager, mtv != 1, awards != 1)

# bandit and jager is something with a game

jager$game <- ifelse(grepl("jager", jager$Contents) == TRUE &
                       grepl("bandit", jager$Contents) == TRUE,1,0)

jager$sind <- ifelse(grepl("sind", jager$Contents) == TRUE,1,0)

jager <- filter(jager, game != 1, sind != 1)

# agua de coco/ gelo de coco appears to be a topic related to jackfire, per BF marketing team. 
# let's take a closer look at some of these tweets by searching in the df viewer.

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

jackfire$song2 <- ifelse(grepl("copo", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("dela", jackfire$Contents) == TRUE &
                           grepl("red", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)

jackfire$song3 <- ifelse(grepl("coco", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("bul", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)

jackfire$song4 <- ifelse(grepl("agua", jackfire$Contents) == TRUE &
                           grepl("gelo", jackfire$Contents) == TRUE &
                           grepl("bul", jackfire$Contents) == TRUE &
                           grepl("jack", jackfire$Contents) == TRUE , 1, 0)

jackfire <- filter(jackfire, song != 1 & song2 != 1 & song3 != 1 & song4 != 1)

jackfire$lamp <- ifelse((grepl("arteks", jackfire$Contents) == TRUE),1,0)

jackfire <- filter(jackfire, lamp != 1)

# Looks like honey also has the Arteks advertising. Will need to remove those. 
# Let's examine the tweets about milkshakes. Looks like there is a place called shakeshakemilkshakes posting instagram
# posts that have seemingly nothing to do with jack daniels, but they are using the hashtag.

jackhoney$shake <- ifelse(grepl("shake shake", jackhoney$Contents) == TRUE , 1, 0)

jackhoney$lamp <- ifelse((grepl("arteks", jackhoney$Contents) == TRUE),1,0)

# There's also something weird going on with whatsapp

jackhoney$whatsapp <- ifelse((grepl("pubinho", jackhoney$Contents) == TRUE |
                                grepl("estrelaeshop", jackhoney$Contents) == TRUE),1,0)

# Actually everything containing whatsapp appears to be selling something. Remove all of these.

jackhoney$whatsapp <- ifelse(grepl("whatsapp", jackhoney$Contents) == TRUE,1,0)

jackhoney<- filter(jackhoney, shake != 1 & lamp != 1 & whatsapp != 1)

# baressp appears to be advertisements from bars. they mention multiple brands and do not lend much insight to jack

jackhoney$bar <- ifelse(grepl("baressp", jackhoney$Contents) == TRUE,1,0)

jackhoney <- filter(jackhoney, bar != 1)

jackhoney$fica <- ifelse(grepl("fica", jackhoney$Contents) == TRUE &
                           grepl("loko|luoco|loco|louco", jackhoney$Contents) == TRUE,1,0)

jackhoney$fourm <- ifelse(grepl("4m", jackhoney$Contents) == TRUE, 1, 0)

jackhoney$fumando <- ifelse(grepl("fuma", jackhoney$Contents) == TRUE &
                              grepl("degustando", jackhoney$Contents), 1, 0)

jackhoney$brisa <- ifelse(grepl("corte", jackhoney$Contents) == TRUE &
                            grepl("brisa", jackhoney$Contents), 1, 0)

jackhoney <- filter(jackhoney, fica != 1, fourm != 1, fumando != 1, brisa != 1)

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

# Write to xls '97 for Jeff

write_excel_csv(jackfire, "jackfire.xls")
write_excel_csv(jackhoney, "jackhoney.xls")
write_excel_csv(fireball, "fireball.xls")
write_excel_csv(jager, "jager.xls")
write_excel_csv(data, "data.xls")

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




# Word Associations and Topic Modeling

##### Jack Fire ######

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
                                    "jackdemel",
                                    "canela"))



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

dtm <- as.DocumentTermMatrix(dtm)

# Latent Dirichlet Allocation model for jackfire

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals != 0, ]  #remove all docs without words

set.seed(7)
lda <- LDA(dtm, k = 3)
term <- terms(lda, 6)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

# ## Translate to English for final visual
# term[[1]] <- "hahaha, shot, to drink, to buy"
# term[[2]] <- "to experience, mto, WTF, honey"
# term[[3]] <- "i want, to drink, i will, bottle"

topics <- topics(lda)

jackfire2 <- jackfire[rowTotals!=0,]
topics <- data.frame(date = jackfire2$Date, topic = topics)

topics$date <- as.Date(topics$date)

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack") +
  labs(title = "JackFire - Density of Topics, 2017 (LDA)")

jackfire2$topic <- topics$topic

# Top words and their probabilities
# the model computes the probability of that term being generated from that topic

tops <- tidy(lda, matrix = "beta")

top_terms <- tops %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# # Sub in English words
# 
# top_terms$term <- gsub("experimentar", "to experience", top_terms$term)
# top_terms$term <- gsub("mano", "bro", top_terms$term)
# top_terms$term <- gsub("melhor", "best", top_terms$term)
# top_terms$term <- gsub("beber", "to drink", top_terms$term)
# top_terms$term <- gsub("rio", "river", top_terms$term)
# top_terms$term <- gsub("queria", "i want", top_terms$term)
# top_terms$term <- gsub("canela", "cinnamon", top_terms$term)
# top_terms$term <- gsub("tomar", "to take (to drink)", top_terms$term)
# top_terms$term <- gsub("tomei", "drank", top_terms$term)
# top_terms$term <- gsub("pqp", "WTF", top_terms$term)
# top_terms$term <- gsub("rio", "river", top_terms$term)
# top_terms$term <- gsub("comprar", "to buy", top_terms$term)
# top_terms$term <- gsub("vontade", "will", top_terms$term)
# top_terms$term <- gsub("vou", "i will", top_terms$term)
# top_terms$term <- gsub("garrafa", "bottle", top_terms$term)
# top_terms$term <- gsub("dose", "shot", top_terms$term)
# top_terms$term <- gsub("tava", "was", top_terms$term)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs( title = "JackFire - Top 5 Words by Topic", xlab ="probability of term coming from topic")

######## Honey #########

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

######## Fireball #########

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


# Latent Dirichlet Allocation model for fireball

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

set.seed(7)
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

##### Jager ####

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


# Latent Dirichlet Allocation model for jager

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm2   <- dtm2[rowTotals != 0, ]  #remove all docs without words

set.seed(7)
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


## There does not appear to be enough data to get good separations between topics, especially for the two most
## important brands, Jack Fire and JAck Honey.


# Translate??????

# library(translateR)
# 
# getGoogleLanguages()
# 
# # portuguese = pt
# # english = en
# 
# KEY <- ""
# 
# translation <- translate(data,
#                          Contents,
#                          google.api.key = KEY,
#                          source.lang = pt,
#                          target.lang = en)
# 
# 
# 
# data$Contents <- as.character(data$Contents)
# sum(nchar(data$Contents))

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

ggplot(share, aes(Date, fill = Brand)) +
           geom_density(position = "fill") +
           scale_fill_manual(values = c("darkorange3", "brown3", "goldenrod2", "darkolivegreen4")) +
           theme_minimal() +
  labs(title = "Share of Tweets by Brand, 2017")


share2 <- rbind(jhoney, yager, fbbb)

ggplot(share2, aes(Date, fill = Brand)) +
           geom_density(position = "fill") +
           scale_fill_manual(values = c("darkorange3", "goldenrod2", "darkolivegreen4")) +
           theme_minimal() +
  labs(title = "Share of Tweets by Brand (Excluding JackFire), 2017")


# Word Exploration

fire <- select(fireball, ID, Date, Contents) %>%
  mutate(Brand = "Fireball")

jf <- select(jackfire, ID, Date, Contents) %>%
  mutate(Brand = "JackFire")

# JackFire and Fireball

fbjf <- rbind(jf, fire)

jg <- select(jager, ID, Date, Contents) %>%
  mutate(Brand = "Jager")

# JackFire and Jager

jfjg <- rbind(jf, jg)


### Fireball

# Create a dataframe of the tweets only, which has a column name of 'Contents'
contents <- fbjf %>%
  transmute(line = row_number(), text = Contents, brand = Brand)

contents$text <- as.character(contents$text)

contents_word <- contents %>%
  unnest_tokens(word, text)

# Read in the portuguese stopwords

stop$stop <- trimws(stop_words$stop, which = c("both"))

stop_pt <- data_frame(word = stopwords$pt)

stop_en <- data.frame(word = stopwords$en)

tidy_tweets <- contents_word %>%
  filter(!word %in% stop_pt$word) %>%
  filter(!word %in% stop$stop) %>%
  filter(!word %in% stop_en$word)

frequency <- tidy_tweets %>%
  dplyr::group_by(brand) %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::left_join(tidy_tweets %>%
                     dplyr::group_by(brand) %>%
                     dplyr::summarise(total = n())) %>%
  dplyr::mutate(freq = n/total)


frequency <- frequency %>%
  select(brand, word, freq) %>%
  spread(brand, freq) %>%
  arrange(Fireball, JackFire)

frequency$word <- removeNumbers(frequency$word)

ggplot(frequency, aes(Fireball, JackFire)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  labs(title = "Word Frequencies - JackFire and Fireball")

frequency <- filter(frequency, !is.na(Fireball) && !is.na(JackFire))

frequency$Fireball <- ifelse(is.na(frequency$Fireball), 0, frequency$Fireball)

frequencyx <- na.omit(frequency)

cor(frequencyx$Fireball, frequencyx$JackFire)


# Words near the line are used with equal frequency between JackFire and Fireball tweets.

tidy_tweets$word <- removeNumbers(tidy_tweets$word)

word_ratios <- tidy_tweets %>%
  dplyr::filter(!str_detect(word, "^_ | ^@")) %>%
  dplyr::count(word, brand) %>%
  dplyr::filter(sum(n) >= 10) %>%
  dplyr::ungroup() %>%
  tidyr::spread(brand, n, fill = 0) %>%
  dplyr::mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  dplyr::mutate(logratio = log(JackFire / Fireball)) %>%
  dplyr::arrange(desc(logratio))

# word_ratios$word <- gsub("experimentar", "to experience", word_ratios$word)
# word_ratios$word<- gsub("aniversário", "birthday", word_ratios$word)
# word_ratios$word <- gsub("pai", "dad", word_ratios$word)
# word_ratios$word <- gsub("mel", "honey", word_ratios$word)
# word_ratios$word <- gsub("horrível", "horrible", word_ratios$word)
# word_ratios$word <- gsub("gelo_de_coco", "coconut ice", word_ratios$word)
# word_ratios$word<- gsub("fds", "weekend", word_ratios$word)
# word_ratios$word <- gsub("experimentei", "i experienced", word_ratios$word)
# word_ratios$word <- gsub("arthud_", "arthud (translation unknown)", word_ratios$word)
# word_ratios$word <- gsub("soltar", "drop", word_ratios$word)
# word_ratios$word <- gsub("joga", "play", word_ratios$word)
# word_ratios$word <- gsub("bola", "ball", word_ratios$word)


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (JackFire / Fireball)") +
  scale_fill_discrete(name = "", labels = c("JackFire", "Fireball")) +
  labs(title = "JackFire vs Fireball - Word Ratios")


#### Jager

# Create a dataframe of the tweets only, which has a column name of 'Contents'
contents <- jfjg %>%
  transmute(line = row_number(), text = Contents, brand = Brand)

contents$text <- as.character(contents$text)

contents_word <- contents %>%
  unnest_tokens(word, text)

tidy_tweets <- contents_word %>%
  filter(!word %in% stop_pt$word) %>%
  filter(!word %in% stop_words$stop) %>%
  filter(!word %in% stop_en$word)


frequency <- tidy_tweets %>%
  dplyr::group_by(brand) %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::left_join(tidy_tweets %>%
                     dplyr::group_by(brand) %>%
                     dplyr::summarise(total = n())) %>%
  dplyr::mutate(freq = n/total)


frequency <- frequency %>%
  select(brand, word, freq) %>%
  spread(brand, freq) %>%
  arrange(Jager, JackFire)

frequency$word <- removeNumbers(frequency$word)

ggplot(frequency, aes(Jager, JackFire)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  labs(title = "Word Frequencies - JackFire and Jager")


frequencyx <- na.omit(frequency)

cor(frequencyx$Jager, frequencyx$JackFire)


# Words near the line are used with equal frequency between JackFire and Fireball tweets.

tidy_tweets$word <- removeNumbers(tidy_tweets$word)
tidy_tweets <- filter(tidy_tweets, word != "las") %>%
  filter(word != "el") %>%
  filter(word != "en") %>%
  filter(word != "yo") %>%
  filter(word != "le")

word_ratios <- tidy_tweets %>%
  dplyr::filter(!str_detect(word, "^_ | ^@")) %>%
  dplyr::count(word, brand) %>%
  dplyr::filter(sum(n) >= 10) %>%
  dplyr::ungroup() %>%
  tidyr::spread(brand, n, fill = 0) %>%
  dplyr::mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  dplyr::mutate(logratio = log(JackFire / Jager)) %>%
  dplyr::arrange(desc(logratio))


# word_ratios$word <- gsub("canela", "cinnamon", word_ratios$word)
# word_ratios$word<- gsub("ouvi", "i heard", word_ratios$word)
# word_ratios$word <- gsub("gostei", "liked it", word_ratios$word)
# word_ratios$word <- gsub("fogonoteto", "blazing fire", word_ratios$word)
# word_ratios$word <- gsub("papo", "horrible", word_ratios$word)
# word_ratios$word <- gsub("gelo_de_coco", "coconut ice", word_ratios$word)
# word_ratios$word<- gsub("slc", "slc (translation unknown)", word_ratios$word)
# word_ratios$word <- gsub("ya", "already", word_ratios$word)
# word_ratios$word <- gsub("arthud_", "arthud (translation unknown)", word_ratios$word)
# word_ratios$word <- gsub("hoy", "today", word_ratios$word)
# word_ratios$word <- gsub("voy", "i go", word_ratios$word)
# word_ratios$word <- gsub("mi", "me", word_ratios$word)
# word_ratios$word <- gsub("tengo", "i have", word_ratios$word)
# word_ratios$word <- gsub("doses", "shots", word_ratios$word)
# word_ratios$word <- gsub("ganas", "you win", word_ratios$word)


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (JackFire / Jager)") +
  scale_fill_discrete(name = "", labels = c("JackFire", "Jager")) +
  labs(title = "JackFire vs Jager - Word Ratios")

## Correlated topic model

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

# CTM model for jackfire

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals != 0, ]  #remove all docs without words

set.seed(7)
ctm <- CTM(dtm, k = 2)
term <- terms(ctm, 6)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

# ## Translate to English for final visual
# term[[1]] <- "cinnamon, drank, bottle, will, best, mto (translation unknown)"
# term[[2]] <- "hahaha, i want, to take (to drink), i will, shot, to drink"




topics <- topics(ctm)

jackfire2 <- jackfire[rowTotals!=0,]
topics <- data.frame(date = jackfire2$Date, topic = topics)

topics$date <- as.Date(topics$date)

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack") +
  labs(title = "JackFire - Density of Topics, 2017 (CTM)")



# Comparing our performance to Crimson Hexagon???

jf <- filter(jackfire, Category == "Irrelevant/Off Topic")
jh <- filter(jackhoney, Category == "Irrelevant/Off Topic")
jm <- filter(jager, Category == "Irrelevant/Off Topic")
fb <- filter(fireball, Category == "Irrelevant/Off Topic")

nrow(jf) + nrow(jh) + nrow(jm) + nrow(fb)

nrow(jackfire) + nrow(jackhoney) + nrow(jager) + nrow(fireball)

# Can we also compare performance of sentiment analysis results?



