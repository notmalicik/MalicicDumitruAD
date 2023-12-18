library(tidyverse)
library(openintro)
library(ggplot2)

ggplot(data = possum, aes(y = total_l, x = tail_l)) + 
  geom_point()

ggplot(data = possum, aes(y = total_l, x = tail_l)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data = possum, aes(y = total_l, x = tail_l)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#Excercise numero 1
##########################################################
data(bdims)

ggplot(bdims, aes(x = hgt, y = wgt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Height (inches)", y = "Weight (lbs)")

ggplot(spotify_populare, aes(y = danceability, x = energy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")


###########################################################
glimpse(textbooks)

textbooks |> 
  ggplot(aes(x = course, y = ucla_new)) + 
  geom_point()

ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) + 
  geom_point()  

ggplot(data = textbooks, aes(x = amaz_new, y = ucla_new)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

lm(ucla_new ~ amaz_new, data = textbooks)

textbooks <- textbooks |> 
  mutate(amaz_new_cents = amaz_new * 100) 
lm(ucla_new ~ amaz_new_cents, data = textbooks)


#Exercise numero 2
##############################

lm(wgt ~ hgt, data = bdims)
         

data(mlbbat10)
lm(slg ~ obp, data = mlbbat10)


data(mammals)
lm(log(body_wt) ~ log(brain_wt), data = mammals)


#################################

books_mod <- lm(ucla_new ~ amaz_new, data = textbooks)
class(books_mod)

books_mod

coef(books_mod)

summary(books_mod)

fitted.values(books_mod)

residuals(books_mod)

library(broom)
augment(books_mod)

#Exercise numero 3
##############################

hgt_wgt_mod <- lm(wgt ~ hgt, data = bdims)

coef(hgt_wgt_mod)

summary(hgt_wgt_mod)

hgt_wgt_tidy <- augment(hgt_wgt_mod)

glimpse(hgt_wgt_tidy)

###########################################