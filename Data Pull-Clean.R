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
  
  stop <- read.csv("stopwords.csv")
  return(stop)
  
}

cleanData <- function(df1, df2) {
  tweets <- df1
  stop <- df2
  
  # Clean the tweets
  
  tweets$Contents <- gsub("á", "a", tweets$Contents)
  tweets$Contents <- gsub("ã", "a", tweets$Contents)
  tweets$Contents <- gsub("ç", "z", tweets$Contents)
  tweets$Contents <- gsub("Ã", "a", tweets$Contents)
  tweets$Contents <- gsub("é", "e", tweets$Contents)
  tweets$Contents <- gsub("ó", "o", tweets$Contents)
  tweets$Contents <- gsub("í", "i", tweets$Contents)
  tweets$Contents <- gsub("ê", "e", tweets$Contents)
  
  tweets$Contents <- removePunctuation(tweets$Contents)
  tweets$Contents <- tolower(tweets$Contents)
  
  tweets$Contents <- gsub("á", "a", tweets$Contents)
  tweets$Contents <- gsub("ã", "a", tweets$Contents)
  tweets$Contents <- gsub("ç", "z", tweets$Contents)
  tweets$Contents <- gsub("Ã", "a", tweets$Contents)
  tweets$Contents <- gsub("é", "e", tweets$Contents)
  tweets$Contents <- gsub("ó", "o", tweets$Contents)
  tweets$Contents <- gsub("í", "i", tweets$Contents)
  tweets$Contents <- gsub("ê", "e", tweets$Contents)
  tweets$Contents <- gsub("ú", "u", tweets$Contents)
  tweets$Contents <- gsub("ä", "a", tweets$Contents)
  tweets$Contents <- gsub("ô", "o", tweets$Contents)
  tweets$Contents <- gsub("à", "a", tweets$Contents)
  
  # Clean the states/cities
  
  tweets$State.Region <- gsub("á", "a", tweets$State.Region)
  tweets$State.Region <- gsub("ã", "a", tweets$State.Region)
  tweets$State.Region <- gsub("é", "e", tweets$State.Region)
  tweets$State.Region <- gsub("ó", "o", tweets$State.Region)
  tweets$State.Region <- gsub("í", "i", tweets$State.Region)
  tweets$State.Region <- gsub("ê", "e", tweets$State.Region)
  tweets$State.Region <- gsub("ú", "u", tweets$State.Region)
  tweets$State.Region <- gsub("ä", "a", tweets$State.Region)
  tweets$State.Region <- gsub("ô", "o", tweets$State.Region)
  
  tweets$City.Urban.Area <- gsub("á", "a", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ã", "a", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("é", "e", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ó", "o", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("í", "i", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ê", "e", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ú", "u", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ä", "a", tweets$City.Urban.Area)
  tweets$City.Urban.Area <- gsub("ô", "o", tweets$City.Urban.Area)
  
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

write_csv(tweets, "data.csv")
write_csv(stop, "stopwords.csv")
