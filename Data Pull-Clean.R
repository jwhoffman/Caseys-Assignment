library(tidyverse)
library(tm)

getTweets <- function() {
  # Read individual data files
  
  first <- read.csv("Posts from 2017-01-25.csv")
  second <- read.csv("Posts from 2017-04-21.csv")
  third <- read.csv("Posts from 2017-07-01.csv")
  fourth <- read.csv("Posts from 2017-08-07.csv")
  fifth <- read.csv("Posts from 2017-09-21.csv")
  one <- read.csv("1.csv")
  two <- read.csv("2.csv")
  three <- read.csv("3.csv")
  four <- read.csv("4.csv")
  five <- read.csv("5.csv")
  six <- read.csv("6.csv")
  seven <- read.csv("7.csv")
  eight <- read.csv("8.csv")
  nine <- read.csv("9.csv")
  ten <- read.csv("10.csv")
  
  # Combine into one df
  
  tweets <-
    rbind(
      first,
      second,
      third,
      fourth,
      fifth,
      one,
      two,
      three,
      four,
      five,
      six,
      seven,
      eight,
      nine,
      ten
    )
  
  # Filter only the tweets that we know are from Brazil
  
  tweets <- filter(tweets, Country == "Brazil")
  
  return(tweets)
  
}


getStopwords <- function() {
  # Portuguese stopwords
  
  stop <- read.csv("stopwords_raw.csv")
  return(stop)
  
}

cleanData <- function(df1, df2) {
  tweets <- df1
  stop <- df2
  
  # Clean the tweets
  
  tweets$Contents <- removePunctuation(as.character(tweets$Contents))
  tweets$Contents <- tolower(tweets$Contents)
  
  tweets$Contents <- gsub("jack daniels honey", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s honey", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jack honey", "jack_honey", tweets$Contents)
  tweets$Contents <- gsub("jack daniels fire", "jack_daniels_fire", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s fire", "jack_daniels_fire", tweets$Contents)
  tweets$Contents <- gsub("jack fire", "jack_fire", tweets$Contents)
  tweets$Contents <- gsub("jack daniel honey", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jack daniels", "jack_daniels", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s", "jack_daniels", tweets$Contents)
  tweets$Contents <- gsub("jack daniels tennessee honey", "jack_daniels_tennessee_honey", tweets$Contents)
  tweets$Contents <- gsub("jack daniels tennessee fire", "jack_daniels_tennessee_fire", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee honey", "jack_daniels_tennessee_honey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee fire", "jack_daniels_tennessee_fire", tweets$Contents)
  
  
  tweets$Contents <- gsub("jager bomb", "jager_bomb", tweets$Contents)
  tweets$Contents <- gsub("jager bom", "jager_bom", tweets$Contents)
  tweets$Contents <- gsub("yager bomb", "yager_bomb", tweets$Contents)
  tweets$Contents <- gsub("yager bom", "yager_bom", tweets$Contents)
  
  tweets$Contents <- gsub("jager meister", "jagermeister", tweets$Contents)
  tweets$Contents <- gsub("yager meister", "yagermeister", tweets$Contents)
  tweets$Contents <- gsub("jager mister", "jagermeister", tweets$Contents)
  tweets$Contents <- gsub("yager mister", "yagermeister", tweets$Contents)
  
  tweets$Contents <- gsub("jäger bomb", "jager_bomb", tweets$Contents)
  tweets$Contents <- gsub("jäger bom", "jager_bom", tweets$Contents)
  tweets$Contents <- gsub("yäger bomb", "yager_bomb", tweets$Contents)
  tweets$Contents <- gsub("yäger bom", "yager_bom", tweets$Contents)
  
  tweets$Contents <- gsub("jäger meister", "jagermeister", tweets$Contents)
  tweets$Contents <- gsub("yäger meister", "yagermeister", tweets$Contents)
  tweets$Contents <- gsub("jäger mister", "jagermeister", tweets$Contents)
  tweets$Contents <- gsub("yäger mister", "yagermeister", tweets$Contents)
  
  tweets$Contents <- gsub("jackdanielshoney", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jackdanielhoney", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jackhoney", "jack_honey", tweets$Contents)
  tweets$Contents <- gsub("jackdanielsfire", "jack_daniels_fire", tweets$Contents)
  tweets$Contents <- gsub("jackdanielfire", "jack_daniels_fire", tweets$Contents)
  tweets$Contents <- gsub("jackfire", "jack_fire", tweets$Contents)
  tweets$Contents <- gsub("jackdanielshoney", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jackdanielhoney", "jack_daniels_honey", tweets$Contents)
  tweets$Contents <- gsub("jackdaniels", "jack_daniels", tweets$Contents)
  tweets$Contents <- gsub("jackdaniel", "jack_daniels", tweets$Contents)
  
  tweets$Contents <- gsub("jack daniels tennessee whiskey", "jack_daniels_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniels tennessee whisky", "jack_daniels_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee whiskey", "jack_daniels_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee whisky", "jack_daniels_tennessee_whiskey", tweets$Contents)
  
  tweets$Contents <- gsub("jack daniels tennessee uisque", "jack_daniels_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee uisque", "jack_daniels_tennessee_whiskey", tweets$Contents)
  
  tweets$Contents <- gsub("tennessee whiskey", "tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("tennessee whisky", "tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("tennessee whisk", "tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("tennessee uisque", "tennessee_whiskey", tweets$Contents)
  
  tweets$Contents <- gsub("jack tennessee uisque", "jack_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack tennessee whiskey", "jack_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack tennessee whisky", "jack_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniels tennessee whisk", "jack_daniels_tennessee_whiskey", tweets$Contents)
  tweets$Contents <- gsub("jack daniel s tennessee whisk", "jack_daniels_tennessee_whiskey", tweets$Contents)
  
  tweets$Contents <- gsub("jack de mel", "jack_de_mel", tweets$Contents)
  tweets$Contents <- gsub("jack daniels fogo", "jack_daniels_fogo", tweets$Contents)
  tweets$Contents <- gsub("jack daniels red", "jack_daniels_red", tweets$Contents)
  tweets$Contents <- gsub("jack daniels vermelho", "jack_daniels_vermelho", tweets$Contents)
  tweets$Contents <- gsub("jack daniels de mel", "jack_daniels_de_mel", tweets$Contents)
  tweets$Contents <- gsub("jack fogo", "jack_fogo", tweets$Contents)
  tweets$Contents <- gsub("jack red", "jack_red", tweets$Contents)
  tweets$Contents <- gsub("jack vermelho", "jack_vermelho", tweets$Contents)
  tweets$Contents <- gsub("jack mel", "jack_mel", tweets$Contents)
  tweets$Contents <- gsub("jack canela", "jack_canela", tweets$Contents)
  tweets$Contents <- gsub("jack de canela", "jack_de_canela", tweets$Contents)
  tweets$Contents <- gsub("jack daniels de canela", "jack_daniels_de_canela", tweets$Contents)
  tweets$Contents <- gsub("jack caneleira", "jack_caneleira", tweets$Contents)
  tweets$Contents <- gsub("jack de caneleira", "jack_de_caneleira", tweets$Contents)
  tweets$Contents <- gsub("jack daniels de caneleira", "jack_daniels_de_caneleira", tweets$Contents)
  tweets$Contents <- gsub("jack daniels mel", "jack_daniels_mel", tweets$Contents)
  tweets$Contents <- gsub("jack daniels canela", "jack_daniels_canela", tweets$Contents)
  tweets$Contents <- gsub("jack daniels caneleira", "jack_daniels_caneleira", tweets$Contents)
  
  tweets$Contents <- gsub("jack daniels brasil", "jack_daniels_brasil", tweets$Contents)
  tweets$Contents <- gsub("jack daniels brazil", "jack_daniels_brasil", tweets$Contents)
  tweets$Contents <- gsub("jack daniel brasil", "jack_daniels_brasil", tweets$Contents)
  tweets$Contents <- gsub("jack daniel brazil", "jack_daniels_brasil", tweets$Contents)
  
  
  tweets$Contents <- gsub("gelo de coco", "gelo_de_coco", tweets$Contents)
  tweets$Contents <- gsub("gelodecoco", "gelo_de_coco", tweets$Contents)
  
  tweets$Contents <- gsub("fire ball", "fireball", tweets$Contents)
  tweets$Contents <- gsub("fire bal", "fireball", tweets$Contents)
  
  
  stop <<- stop
  
  # Rename columns
  
  colnames(tweets)[c(2, 6, 7)] <- c("Date", "State", "City")
  
  # Fix Date column
  
  tweets$Date <- as.character(tweets$Date)
  
  tweets$Date <- substr(tweets$Date, 1, nchar(tweets$Date) - 5)
  
  tweets$Date <- stripWhitespace(tweets$Date)
  
  tweets$Date <- as.Date(tweets$Date, "%m/%d/%Y")
  
  tweets <<- tweets
  
}

tweets <- getTweets()
stop <- getStopwords()
cleanData(tweets, stop)








write.csv(tweets, "data.csv")
write.csv(stop, "stopwords.csv")




