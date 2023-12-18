library(reshape2)
spotifynoid <- spotify
spotifynoid$track_id = NULL
spotifynoid_noduplicate <- spotifynoid %>%
  filter(!duplicated(spotifynoid[, c("track_name")]))

spotifynoid_noduplicate <- spotifynoid_noduplicate %>%
  arrange(desc(popularity))

m5 <- spotifynoid_noduplicate[c(1:5),]
m5<- m5[, c(4,8,9,13,14,16,17)] 
m5<- as.data.frame(m5)
m5.long <- melt(m5, id.vars="track_name")

mp1<- ggplot(data=m5.long, aes(x=variable, y=value))+
  geom_bar(aes(y=value, fill=track_name),stat="identity", alpha=0.8 , position="dodge")+ 
  ylab("Value")+ xlab("Variables of a song")+coord_flip()+ggtitle("Top 5 songs in Spotify")
mp1


a1 <- group_by(spotify, artists)
a2 <- dplyr::summarise(a1,  count=n())
a2 <- arrange(a2, desc(count))
a3 <- filter(a2, count>150)


ap1 <- ggplot(a3, aes(x=reorder(artists,count),y=count))+
  geom_bar(aes(y=count,fill=artists), stat="identity")+
  labs(x="Artists", y="Number of Songs",
       title="Popular Artists On Spotify")+ theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) 
ap1


spotify_populare70 <- spotify %>%
  filter(popularity > 75)
glimpse(spotify_populare70)

df <- spotify_populare70 %>%
  filter(!duplicated(spotify_populare70[, c("track_name")]))
  
df <- na.omit(df)
df$track_id <- NULL
# Model de regresie liniară




library(ggplot2)

hist(df$popularity)

popularity_log <- log(df$popularity)
hist(popularity_log)
# Creare scatter plot cu regresie liniară
y_limits <- c(80, 100)

# Creează scatter plot cu limite specifice pentru axa y
ggplot(train_data, aes(x = duration_ms, y = popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot cu Regresie Liniară",
       x = "Durată (ms)",
       y = "Popularitate")
  


library(caret)
library(MASS)


set.seed(123)
df$explicit <- as.integer(as.logical(df$explicit))
df$explicit <- as.factor(df$explicit)

glimpse(df)
# Crearea unui set de date de antrenare și unul de testare
index <- createDataPartition(df$explicit, p = 0.7, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Model de regresie logistică folosind setul de antrenare
logistic_model <- train(
  explicit ~ popularity,
  data = train_data,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10),
)
#warnings()
# Afișarea rezultatelor
print(logistic_model)
library(pROC)
library(caret)

datapr <- data.frame(popularity = 81)
predction2 <- predict(logistic_model, datapr)

print(predction2)

roc_obj <- roc(test_data$explicit, predction2)
auc_value <- auc(roc_obj)

# Desenează curba ROC
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))

# Calculează matricea de confuzie
conf_matrix <- confusionMatrix(data = as.factor(round(predction2)), reference = as.factor(datapr$popularity))
print(conf_matrix)

# Extrage metricile
precision <- confusion_matrix$byClass["Precision"]
sensitivity <- confusion_matrix$byClass["Sensitivity"]
specificity <- confusion_matrix$byClass["Specificity"]

set.seed(123)
indexl <- createDataPartition(df$tempo, p = 0.7, list = FALSE)
train_datal <- df[indexl, ]
test_datal <- df[-indexl, ]



# Model de regresie liniar folosind setul de antrenare
linear_model <- lm(popularity ~ duration_ms, data = train_datal)

# Sumar al modelului
summary(linear_model)

?predict

new_data = data.frame(duration_ms = 10000)
print(new_data)
predict(linear_model, new_data)

