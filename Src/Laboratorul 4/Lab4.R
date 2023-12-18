library(tidyverse) 
library(openintro) 
library(dplyr)
ggplot(data = possum, aes(y = total_l, x = tail_l)) + 
  geom_point()

ggplot(data = possum, aes(y = total_l, x = tail_l)) + 
  geom_point() + 
  labs(x = "Length of Possum Tail (cm)", y = "Length of Possum Body (cm)")

summary(possum)
glimpse(possum)

possum <- possum |> 
  mutate(tail_cut = cut(tail_l, breaks = 5)) 
ggplot(data = possum, aes(y = total_l, x = tail_cut)) + 
  geom_point()

ggplot(data = possum, aes(y = total_l, x = tail_cut)) + 
  geom_boxplot()

ggplot(data = possum, aes(y = total_l, x = tail_cut)) + 
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(color = "sky blue", width = 0.2)

glimpse(ncbirths)
?ncbirths

ggplot(data = ncbirths, aes(x = weeks, y = weight)) + 
  geom_point()

ncbirths$weeks_cut <- cut(ncbirths$weeks, breaks = 4)

ggplot(data = ncbirths, aes(x = weeks_cut, y = weight)) +
  geom_boxplot() + 
  labs(y = "Greutatea (KG)", x = "Saptamanile")

data(mammals)

ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) +
  geom_point() +
  labs(x = "Greutatea Corpului", y = "Greutatea Creierului")

data(mlbbat10)
?mlbbat10
ggplot(data = mlbbat10, aes(x = obp, y = slg)) +
  geom_point() +
  labs(x = "On-Base Percentage", y = "Slugging Percentage(total_base / at_bat)")

data(bdims)
bdims$sex <- as.factor(bdims$sex)

ggplot(data = bdims, aes(x = wgt, y = hgt, color = sex)) +
  geom_point() +
  labs(y = "Inaltimea (Cm)", x = "Greutatea (Kg)")

data(smoking)
?smoking
ggplot(data = smoking, aes(x = age, y = amt_weekdays)) +
  geom_point() +
  labs(x = "Varsta", y = "Cate tigari fumate in Weekend")

ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) + 
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) + 
  geom_point() + scale_x_log10() + 
  scale_y_log10()

ggplot(data = mlbbat10, aes(x = stolen_base, y = home_run)) + 
  geom_point(alpha = 0.5)

ggplot(data = mlbbat10, aes(x = stolen_base, y = home_run)) + 
  geom_jitter(alpha = 0.5)

mlbbat10 |> 
  filter(stolen_base > 60 | home_run > 50) |> 
  select(name, team, position, stolen_base, home_run)

ab_gt_200 <- mlbbat10 %>% filter(at_bat >= 200)

ggplot(data = ab_gt_200, aes(x = obp, y = slg)) +
  geom_point() +
  labs(x = "On-Base Percentage", y = "Slugging Percentage(total_base / at_bat)")

player_below_200_obp <- ab_gt_200 %>% filter(obp < 0.200)

ggplot(data = ab_gt_200, aes(x = obp, y = slg)) +
  geom_point() +
  labs(x = "On-Base Percentage", y = "Slugging Percentage(total_base / at_bat)")
