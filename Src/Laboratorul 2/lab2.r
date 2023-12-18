library(openintro)
library(tidyverse)
comics <- read.csv("E:/Univer/Anul 3/Analiza Datelor/Malicic Dumitru/Laboratorul 2/comic_characters.csv")

glimpse(comics)



levels(factor(comics$align))
levels(factor(comics$sex))

tab <- table(comics$align, comics$sex)
glimpse(tab)



library(dplyr)

tab

comics_filter <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

comics_filter

#2.3. Side-by-side barcharts

library(ggplot2)

ggplot(comics, aes(x = align, fill = sex)) +
  geom_bar(position = "dodge")

ggplot(comics, aes(x = sex, fill = align)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

tab1 <- table(comics$align, comics$sex)
options(scipen = 999, digits = 3)
prop.table(tab1)   #joint prop.
prop.table(tab1, 2) #cond. prop.

#Plot of gender by align
ggplot(comics, aes(x = align, fill = sex)) +
  geom_bar()

# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = sex)) + 
  geom_bar(position = "fill") +
  ylab("proportion")

# 5
cars <- cars93

glimpse(cars93)


cars %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 3)

cars %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 30)

cars %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 60)

# 6
cars %>%
  ggplot(aes(x = 1, y = price)) +
  geom_boxplot()
  

cars_no_out <- cars %>%
  filter(price > 40)

cars_no_out %>%
  ggplot(aes(x = 1, y = price)) +
  geom_boxplot()

# 7
ggplot(cars, aes(x = mpg_city)) +
  geom_histogram() +
  facet_wrap(~ type)

# 8
cars %>%
  filter(weight > 2000) %>%
  ggplot(aes(mpg_city)) +
  geom_histogram() +
  facet_wrap(~ drive_train)

spotify <- read.csv("E:/Univer/Anul 3/Analiza Datelor/Malicic Dumitru/Laboratorul 1/DataSet/dataset.csv")

levels(factor(spotify$artists))
levels(factor(spotify$album_name))

tabspotify <- table(comics$track_name, comics$explicit)
glimpse(tabspotify)



library(dplyr)

tabspotify

spotify_filter <- spotify %>%
  filter(explicit != "False") %>%
  droplevels()

spotify_filter2 <- spotify %>%
  filter(explicit != "True") %>%
  droplevels()

spotify_filter

ggplot(spotify_filter, aes(x = popularity)) + 
  geom_bar()
ggplot(spotify_filter2, aes(x = popularity)) + 
  geom_bar()

#2.3. Side-by-side barcharts

library(ggplot2)

ggplot(spotify, aes(x = key, fill = tempo)) +
  geom_bar(position = "dodge")


spotify %>%
  ggplot(aes(popularity)) +
  geom_histogram(binwidth = 3)
spotify %>%
  ggplot(aes(popularity)) +
  geom_histogram(binwidth = 30)
spotify %>%
  ggplot(aes(popularity)) +
  geom_histogram(binwidth = 60)


# 6
spotify %>%
  ggplot(aes(x = 1, y = popularity)) +
  geom_boxplot()


populare <- spotify %>%
  filter(popularity > 80)
glimpse(populare)

populare %>%
  ggplot(aes(x = energy)) +
  geom_bar()

# 7
ggplot(spotify, aes(x = popularity)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ track_genre)

# 8
spotify %>%
  filter(popularity > 90) %>%
  ggplot(aes(tempo)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(~ track_genre)
