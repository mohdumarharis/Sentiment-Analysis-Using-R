# Installing the required packages for the analysis
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidytext")
# Loading libraries
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
setwd("/Users/haris/Downloads/Supporting materials for the coursework assignment-20221215")
# setting the working directory to the folder containing the data
# please change this according to your folder location.
modern_words <- read.delim("modern_word_count.txt") # loading the txt file
nrow(modern_words)
str(modern_words) # checking the structure of the data
modern_words%>%arrange(word_count)%>%head(50) # checking first 50 values after arranging
modern_words%>%arrange(desc(word_count))%>%head(50) # checking last 50 values after arranging
# this data is provided to us for the sentiment analysis but this data has
# very limited use here as this contains modern english words taken from the web and
# the play is written in antient english.
King_Lear <- read.csv("King_Lear_words_and_players_only.csv") #loading the given csv file
str(King_Lear)
King_Lear%>%head(30)
data(stop_words) # loading the stop words data
stop_words
nrow(stop_words) # checking the number of rows of this data as to confirm weather this can be used for
# removing unncessory words to clean the king_lear data
King_Lear_tidy <- King_Lear %>%
 unnest_tokens(word, text)%>%
 anti_join(stop_words)
print(head(King_Lear_tidy)) #checking the head
King_Lear_tidy %>%count(word, sort = TRUE)%>%head(50) # count most common words
# we have to create a custom stop words data to be used to remove the
# common words used in antient english for cleaning the data
custom_stop_word <- bind_rows(tibble(word = c("thou","thy","thee","tis"),
 lexicon = c("custom")),
 stop_words)
head(custom_stop_word)
# removal of custom stop words in King_Lear
King_Lear_tidy <- King_Lear_tidy %>%
 anti_join(custom_stop_word)
King_Lear_tidy
# we forst use "nrc" library for the sentiment analysis
nrc_joy <- get_sentiments("nrc") # checking the data in "nrc"
nrc %>% count(sentiment)
king_lear_joy_words <- King_Lear_tidy %>%inner_join(nrc_joy)%>%
 count(word, sort = TRUE) # segregatig joy words

king_lear_joy_words
print(plot(king_lear_joy_words$n)) # plotting graph for the joy words
king_lear_joy_words%>%arrange(desc(n))%>%head(10)
data1 <- King_Lear_tidy %>% inner_join(nrc) # joining the data with "nrc"
count(data1)/20 # calculating the number of words in each part of 20 parts
data1
data1_mutated <- mutate(data1,sno=row_number()) %>% count( index = sno %/% 407, sentiment)
# we divided the data into 20 eaual parts as to observe and compare the sentiments
# in each part because if we use single values, this cannot be analysed.
data2_mutated <- data1_mutated %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
# converting the data into a wider format for analysis
data1_mutated # checking the data
data2_mutated
king_lear_emotion <- data1 %>%filter(player=="LEAR") %>% mutate(sno=row_number()) %>%
 count( index = sno %/% 100, sentiment) # we can filter the words by king lear and analyse this
# to plot a graph which can show the changing emotions of king lear throughout the play
# here we have divided king lear's words into multiple parts to analyse each part
king_lear_emotion
library(ggplot2)
# we can observe the data of each emotion here as a graph
ggplot(data2_mutated, aes(index, anger)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, anticipation)) +
 geom_col()
ggplot(data2_mutated, aes(index, disgust)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, fear)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, joy)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, sadness)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, surprise)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, trust)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, positive)) +
 geom_col(show.legend = FALSE)
ggplot(data2_mutated, aes(index, negative)) +
 geom_col(show.legend = FALSE)
ggplot(data = data1_mutated,aes(index,n,color = sentiment)) +
 geom_line() # combined line graph of each emotion throughout the play
ggplot(data1_mutated,aes(index,n,color = sentiment))+
 geom_line()+
 facet_wrap(facets = vars(sentiment)) # graphs of each emotions as line graph
ggplot(king_lear_emotion,aes(index,n,color = sentiment))+
 geom_col()+
 facet_wrap(facets = vars(sentiment)) # king lear's emotions throughout the play
# now we use afinn lexicon for the sentiment analysis
King_Lear_words <- King_Lear_tidy %>%count(word, sort = TRUE)%>%
 inner_join(get_sentiments("afinn"),"word")%>%
 mutate(weighted=n*value) # joining the data from "afinn"
King_Lear_words$value%>%table()%>%barplot() # plotting a bar graph for "afinn" analysis
King_Lear_words$weighted%>%sum() # calculating the total score of the play by adding the weighted scores

# using bing library
bing <- get_sentiments("bing")
head(bing)
table(bing$sentiment)
King_Lear_tidy %>%
 inner_join(get_sentiments("bing"),"word") %>%count(player,sentiment)
# we can check the number of positive and negative words said by each character
king_lear_bing <- King_Lear_tidy %>% inner_join(bing)# joining "bing" data for analysis
(king_lear_bing%>%count())/100 # calculating number of words in each part of total 100 parts
king_lear_bing <- mutate(king_lear_bing,sno=row_number()) %>% count( index = sno %/%21 , sentiment)
king_lear_bing%>%head(20)
king_tidy_bing_wide <- king_lear_bing %>%
 pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
 mutate(sentiment = positive - negative) # converting to the wide format for separating each sentoment
king_tidy_bing_wide%>%head(20)
ggplot(king_tidy_bing_wide, aes(index, sentiment)) +
 geom_col(show.legend = FALSE) # generating graph for each part 

