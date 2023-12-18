library(openintro)
library(tidyverse)
library(gapminder)
??gapminder
glimpse(gapminder)
gap2007 <- gapminder %>%
  filter(year == 2007)

meanmed_life <- gap2007 %>%
  group_by(continent) %>%
  summarise(mean_lifeExp = mean(lifeExp), med_lifeExp = median(lifeExp))

print(meanmed_life)
library(ggplot2)

gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()


sumgap <- gap2007 %>%
  group_by(continent) %>%
  summarize(sdev = sd(lifeExp), iqr = IQR(lifeExp), numar = n())

print(sumgap)

gap2007 %>%
  group_by(continent == "Africa") %>%
  ggplot(aes(x = lifeExp)) +
  geom_density()

america_data <- gap2007 %>%

  filter(continent == "Americas")
center_measure <- mean(america_data$lifeExp)
spread_measure <- sd(america_data$lifeExp)

center_measure
spread_measure

#4
ggplot(gap2007, aes(x = pop)) +
  geom_density() 

gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))

print(gap2007)

#5
asia <- gap2007 %>%
  filter(continent == "Asia") 

asia <- asia %>%
  mutate(is_outlier = if_else(lifeExp < 50, TRUE, FALSE))

ggplot(asia, aes(x = "", y = lifeExp, fill = is_outlier)) +
  geom_boxplot()

spotify %>%
  ggplot(aes(x = popularity)) +
  geom_density()

spotify_populare <- spotify %>%
  filter(popularity > 90)

glimpse(spotify_populare)  

spotify_populare %>%
  group_by(track_genre) %>%
  ggplot(aes(y = track_genre)) +
  geom_bar(fill = "#2ab2d4") +
  theme(text = element_text(size = 16))

medTempo <- spotify_populare %>%
  group_by(track_genre) %>%
  summarise(avg_tempo = mean(tempo))

medTempo

min(medTempo$avg_tempo)
max(medTempo$avg_tempo)

medTempo %>%
  ggplot(aes(x = track_genre, y = avg_tempo)) +
  geom_point() + 
  scale_y_continuous(
    breaks = seq(65, 145, by = 5),
    limits = c(65, 145)
  )
  # theme(text = element_text(size = 16))
spotify$duration_minute <- spotify$duration_ms / 60000
spotify$duration_second <- spotify$duration_ms / 1000
spotify$duration_minute <- gsub("\\.", ":", spotify$duration_minute)

ggplot(spotify, aes(x = duration_second, y = danceability)) +
  geom_point(color = "blue") +
  scale_x_continuous(
    breaks = seq(0, 6000, by = 1000),
    limits = c(0, 6000)
  ) +
  labs(title = "Scatter Plot cu : Durata vs. Dansibialitatea Melodiilor", x = "Durata (s)", y = "Dansibilittea")

ggplot(spotify, aes(x = popularity)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = paste("Histograma Popularității Melodiilor"), x = "Popularitate", y = "Număr de Melodii")
glimpse(spotify)
