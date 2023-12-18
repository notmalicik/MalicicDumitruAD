
library(openintro)
library(tidyverse)
library(ggplot2)
head(email50)

filter(number == big)
glimpse(email50)

#(med<-median(email50$num_char))
?email50
?dplyr::glimpse()

email50_big <- email50 %>%
  filter(number == "big")
glimpse(email50_big)

?if_else

email50_updated <- email50 %>%
  mutate(num_char_cat=if_else(
    num_char < (median(email50$num_char)), "bellow", "above"
  ))
glimpse(email50_updated)

email50_updated <- email50 %>%
  mutate(number_cat=if_else(
    number == "none", "no", "yes"
  ))

?ggplot
ggplot(email50_updated, aes(x=number_cat)) + geom_bar()
ggplot(email50_updated, aes(x=num_char, y=exclaim_mess, color=spam)) + 
  geom_point()

glimpse(cls_students)

?read_csv

evals_up <- mutate(evals, cls_type = case_when(
  cls_students <= 18 ~ "small",
  cls_students >= 19 & cls_students <= 59 ~ "midsize",
  cls_students >= 60 ~ "large"
))

glimpse(evals_up)

ggplot(evals_up, mapping = aes(x = cls_type)) + geom_bar()

spotify <- read.csv("E:/Univer/Anul 3/Analiza Datelor/Malicic Dumitru/Laboratorul 1/DataSet/dataset.csv")
glimpse(spotify)
ggplot(spotify, aes(x = popularity)) +
  geom_bar()
head(spotify)
summary(spotify)
dim(spotify)
