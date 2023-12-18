spotifydataset <- spotify_populare

glimpse(spotifydataset)

set.seed(123)

# Împărțirea datelor în set de antrenare (80%) și set de testare (20%)
index <- createDataPartition(spotifydataset$popularity, p = 0.8, list = FALSE)
train_data <- spotifydataset[index, ]
test_data <- spotifydataset[-index, ]

# Verifică dimensiunile seturilor de date
dim(train_data)
dim(test_data)

shapiro.test(train_data$popularity)

linear_model <- lm(popularity ~ tempo, data = train_data)

# Sumarul modelului
summary(linear_model)

# Previziuni pe setul de testare
predictions <- predict(linear_model, newdata = test_data)

# Evaluarea modelului (de exemplu, MSE)
mse <- mean((predictions - test_data$popularity)^2)
print(paste("Mean Squared Error:", mse))


train_data$popularity_binary <- ifelse(train_data$popularity > mean(train_data$popularity), 1, 0)
test_data$popularity_binary <- ifelse(test_data$popularity > mean(train_data$popularity), 1, 0)

# Modelul de regresie logistică
logistic_model <- glm(popularity_binary ~ valence + danceability, data = train_data, family = "binomial")

# Sumarul modelului
summary(logistic_model)

# Previziuni pe setul de testare
probabilities <- predict(logistic_model, newdata = test_data, type = "response")

# Evaluarea modelului (de exemplu, AUC-ROC)
library(pROC)
roc_curve <- roc(test_data$popularity_binary, probabilities)
print(paste("AUC-ROC:", auc(roc_curve)))

scatter_plot <- ggplot(train_data, aes(x = danceability, y = popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Regresie Liniară a Popularității în funcție de Valence",
       x = "Valence",
       y = "Popularitate")

# Afișează diagrama
print(scatter_plot)

library(ggplot2)
# Creează obiectul ROC
roc_curve <- roc(test_data$popularity_binary, probabilities)

# Valorile necesare pentru diagramă
roc_data <- data.frame(
  specificity = 1 - roc_curve$specificity,
  sensitivity = roc_curve$sensitivity
)

# Creează diagrama ROC folosind ggplot2
roc_plot <- ggplot(data = roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Curba ROC pentru Regresia Logistică",
       x = "Rata de Fals Pozitive",
       y = "Rata de Adevărate Pozitive")

print(roc_plot)
