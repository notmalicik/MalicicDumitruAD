###### PART 1 ##### 
### Linear Regression ###
### Prerequisites ###

library(dplyr)
library(ggplot2)

# modeling packages

library(caret)
library(rsample)

# model interpretability packages
library(vip)

#dataset
install.packages('AmesHousing')
library(AmesHousing)

#Access data
ames <- AmesHousing::make_ames()
dim(ames)

#Data spliting
set.seed(123)
split <- initial_split(ames, prop = 0.7,
                            strata = 'Sale_Price')
ames_train <- training(split)
ames_test <- training(split)

# simple linear regression
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
model1

# Error metrics
sigma(model1) #RMSE
sigma(model1) #MSE


# multiple linear regression
(model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train))

# alternatively we can use update() the model formula used for simple lienar regression
(model2 <- update(model1, . ~ . + Year_Built))

# include all possible main effects
model3 <- lm(Sale_Price ~ ., data = ames_train)
# print estimated coefficients in a tidy data frame
broom::tidy(model3)


### ASSESSING MODEL ACCURACY ###

#model 1
set.seed(123)
(cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area,
  data = ames_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))


#model 2
set.seed(123)
cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built,
  data = ames_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

#model 3 CV
set.seed(123)
cv_model3 <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

# extract out of sample performamce measures
summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3)))

# Condition 1: Linear relationship
p1 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = F) +
  scale_y_continuous('Sale price', labels = scales::dollar) +
  xlab('Year built') +
  ggtitle(paste('Non-transformed variables with a\n',
                'non-linear relationshop'))
p1


p2 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_log10('Sale price', labels = scales::dollar,
                breaks = seq(0, 400000, by = 100000)) +
  xlab('Year built') +
  ggtitle(paste('Transforming variables can privide a\n',
          'near-linear relationship'))
gridExtra::grid.arrange(p1, p2, nrow = 1)


# Condition 2: Constant variance among residuals
df1 <- broom::augment(cv_model1$finalModel, data = ames_train)
glimpse(df1)
p1 <- ggplot(df1, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Sale_Price ~ Gr_Liv_Area')
p1

df2 <- broom::augment(cv_model3$finalModel, data = ames_train)
glimpse(df2)
p2 <- ggplot(df2, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Sale_Price ~ ,')
p2

# Condition 3: No autocorrelation
df1 <- mutate(df1, id = row_number())
glimpse(df1)

df2 <- mutate(df2, id = row_number())
glimpse(df2)

p1 <- ggplot(df1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Correlated residuals.')
p1

p2 <- ggplot(df2, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Uncorrelated residuals.')
p2

# Condition 4: More obseravtions than predictors

### Feature interpretation
vip(cv_model3, num_features = 10)



###### PART 2 ##### 
### Logistic Regression ###

# Access data
install.packages("modeldata")
library(modeldata)
dim(attrition)
glimpse(attrition)
# Peek at response variable
head(attrition$Attrition)
df <- attrition %>% mutate_if(is.ordered, factor, ordered = F)

# Create training (70%) and test (30%) sets
churn_split <- initial_split(df, prop = .7, strata = 'Attrition')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

model1 <- glm(Attrition ~ MonthlyIncome, family = 'binomial', data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = 'binomial', data = churn_train)

tidy(model1)
tidy(model2)

exp(coef(model1))
exp(coef(model2))

# Multiple logistic regression
model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime, family = 'binomial',
  data = churn_train
)
tidy(model3)

### ASSESSING MODEL ACCURACY ###
?set.seed
set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model3 <- train(
  Attrition ~ .,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

summary( 
  resamples(
    list(
    model1 = cv_model1, model2 = cv_model2, model3 = cv_model3
  ))
)$statistics$Accuracy


# predict class
# predict class
pred_class <- predict(cv_model3, churn_train)
# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = 'Yes'),
  reference = relevel(churn_train$Attrition, ref = 'Yes')
)
install.packages("ROCR")
library(ROCR)
# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = 'prob')$Yes
m3_prob <- predict(cv_model3, churn_train, type = 'prob')$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Attrition) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_train$Attrition) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)

### Feature interpretation
vip(cv_model3, num_features = 20)
