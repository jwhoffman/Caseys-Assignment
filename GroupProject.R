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

data <- read_csv("data.csv")

# Filter to Brazil only

data <- filter(data, Country == "Brazil")

# Use regular expressions to separate tweets by product

data$cate <- 0

for (i in 1:nrow(data)) {
  if (grepl("jager", data$Contents[i], fixed = TRUE) == TRUE|grepl("yager", data$Contents[i], fixed = TRUE) == TRUE) {
    data$cate[i] <- "jager"
  } else if (grepl("jd", data$Contents[i], fixed = TRUE) == TRUE|grepl("jack", data$Contents[i], fixed = TRUE) == TRUE|
             grepl("daniel", data$Contents[i], fixed = TRUE) == TRUE) {
    data$cate[i] <- "JackDaniels"
  } else if (grepl("fireball", data$Contents[i], fixed = TRUE) == TRUE|grepl("firebal", data$Contents[i], fixed = TRUE) == TRUE) {
    data$cate[i] <- "fireball"
  } else {
    data$cate[i] <- "unknown"
  }
} 

jager <- filter(data, cate == "jager")
jack <- filter(data, cate == "JackDaniels")
fireball <- filter(data, cate == "fireball")

other <- filter(data, cate == "unknown")

jack$jackfire <- ifelse((grepl("fire", jack$Contents, fixed = TRUE) == TRUE|grepl("fogo", jack$Contents, fixed = TRUE) == TRUE|
                     grepl("cinnamon", jack$Contents, fixed = TRUE) == TRUE|grepl("canel", jack$Contents, fixed = TRUE) == TRUE|
                     grepl("red", jack$Contents, fixed = TRUE) == TRUE),1,0)

jack$jackhoney <- ifelse((grepl("honey", jack$Contents, fixed = TRUE) == TRUE|grepl("mel", jack$Contents, fixed = TRUE) == TRUE),1,0)

jackfire <- filter(jack, jackfire == 1)
jackhoney <- filter(jack, jackhoney == 1)


# DFs for sentiment analysis = jager, fireball, jackfire, jackhoney -> compare sentiment across brands before/after launch. 
# Compare sentiment of honey in 2014 to fire after launch.

# Consumer insights -> whiskey and beer, whiskey and coconut water?

other$coco <- ifelse(((grepl("whiskey", other$Contents, fixed = TRUE) == TRUE|grepl("uisque", other$Contents, fixed = TRUE) == TRUE)&
                         grepl("coco", other$Contents, fixed = TRUE) == TRUE),1,0)

gelodecoco <- filter(other, coco == 1)

other$beer <- ifelse(((grepl("whiskey", other$Contents, fixed = TRUE) == TRUE|grepl("uisque", other$Contents, fixed = TRUE) == TRUE|
                         grepl("shot", other$Contents, fixed = TRUE) == TRUE|grepl("dose", other$Contents, fixed = TRUE) == TRUE)&
                        grepl("beer", other$Contents, fixed = TRUE) == TRUE|grepl("cerveja", other$Contents, fixed = TRUE) == TRUE),1,0)

cerveja <- filter(other, beer == 1)

# Specific insights for Marissa = whiskey and coconut water & shot of whiskey and beer. Can we find any trends? What is the sentiment?






docs <- Corpus(VectorSource(data$Contents))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c('de','a','o','que','e','do','da','em',
                                    'um','para','é','com','não','uma','os','no',
                                    'se','na','por','mais','as','dos','como',
                                    'mas','foi','ao','ele','das','tem','�','seu',
                                    'sua','ou','ser','quando','muito','há','nos',
                                    'já','está','eu','também','só','pelo','pela',
                                    'até','isso','ela','entre','era','depois','sem',
                                    'mesmo','aos','ter','seus','quem','nas','me',
                                    'esse','eles','estão','você','tinha','foram',
                                    'essa','num','nem','suas','meu','�s','minha',
                                    'têm','numa','pelos','elas','havia','seja',
                                    'qual','será','nós','tenho','lhe','deles','essas',
                                    'esses','pelas','este','fosse','dele','tu','te',
                                    'vocês','vos','lhes','meus','minhas','teu','tua',
                                    'teus','tuas','nosso','nossa','nossos','nossas',
                                    'dela','delas','esta','estes','estas','aquele',
                                    'aquela','aqueles','aquelas','isto','aquilo',
                                    'estou','está','estamos','estão','estive','esteve',
                                    'estivemos','estiveram','estava','estávamos',
                                    'estavam','estivera','estivéramos','esteja',
                                    'estejamos','estejam','estivesse','estivéssemos',
                                    'estivessem','estiver','estivermos','estiverem',
                                    'hei','há','havemos','hão','houve','houvemos',
                                    'houveram','houvera','houvéramos','haja','hajamos',
                                    'hajam','houvesse','houvéssemos','houvessem',
                                    'houver','houvermos','houverem','houverei',
                                    'houverá','houveremos','houverão','houveria',
                                    'houver�amos','houveriam','sou','somos','são',
                                    'era','�ramos','eram','fui','foi','fomos','foram',
                                    'fora','fôramos','seja','sejamos','sejam','fosse',
                                    'fôssemos','fossem','for','formos','forem','serei',
                                    'será','seremos','serão','seria','seramos','seriam',
                                    'tenho','tem','temos','tém','tinha','t�nhamos',
                                    'tinham','tive','teve','tivemos','tiveram','tivera',
                                    'tivéramos','tenha','tenhamos','tenham','tivesse',
                                    'tirassemos','tivessem','tiver','tivermos','tiverem',
                                    'terei','terai','teremos','teraio','teria','teramos',
                                    'teriam'))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

wordcloud(d$word, d$freq)
