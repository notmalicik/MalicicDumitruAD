library(dplyr)
library(ggplot2)
library(openintro)
library(tidyverse)

# modeling packages
library(caret)
library(rsample)

# model interpretability packages
library(vip)

#dataset
insurance_med <- read_csv('E:/Univer/Anul 3/Analiza Datelor/Malicic Dumitru/Laboratorul 7/insurance_med.csv')
dim(insurance_med)
glimpse(insurance_med)

#Data spliting
set.seed(123)
split <- initial_split(insurance_med, prop = 0.7, strata = 'expenses')

insurance_med_train <- training(split)
insurance_med_test <- training(split)

# simple linear regression
model1 <- lm(expenses ~ smoker, data = insurance_med_train)
model1

# Error metrics
sigma(model1) 


# multiple linear regression
(model2 <- lm(expenses ~ smoker + region, data = insurance_med_train))
sigma(model2)

(model2 <- update(model1, . ~ . + region))
# include all possible main effects
model3 <- lm(expenses ~ ., data = insurance_med_train)
model3
sigma(model3)

broom::tidy(model3)

### ASSESSING MODEL ACCURACY ###

#model 1
set.seed(123)
(cv_model1 <- train(
  form = expenses ~ bmi,
  data = insurance_med_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))


#model 2
set.seed(123)
cv_model2 <- train(
  expenses ~ bmi + age,
  data = insurance_med_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

cv_model2


#model 3 CV
set.seed(123)
cv_model3 <- train(
  expenses ~ .,
  data = insurance_med_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

cv_model3


summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3)))

p1 <- ggplot(insurance_med_train, aes(bmi, expenses)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = F) +
  scale_y_continuous('Exspenses', labels = scales::dollar) +
  xlab('BMI') +
  ggtitle(paste('Non-transformed variables with a\n',
                'non-linear relationshop'))
p1

p2 <- ggplot(insurance_med_train, aes(bmi, expenses)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_log10('Exspenses', labels = scales::dollar,
                breaks = seq(0, 400000, by = 100000)) +
  xlab('BMI') +
  ggtitle(paste('Transforming variables can privide a\n',
                'near-linear relationship'))
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Condition 2: Constant variance among residuals
df1 <- broom::augment(cv_model1$finalModel, data = insurance_med_train)
glimpse(df1)
p1 <- ggplot(df1, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Expenses ~ Smoker')
p1

df2 <- broom::augment(cv_model3$finalModel, data = insurance_med_train)
glimpse(df2)
p2 <- ggplot(df2, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Exspenses ~ ,')
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
insurance_churn <- read_csv('E:/Univer/Anul 3/Analiza Datelor/Malicic Dumitru/Laboratorul 7/insurance_churn.csv')

# Split the data into training (70%) and test (30%) sets
set.seed(123)
churn_split_insurance <- initial_split(insurance_churn, prop = 0.7, strata = 'churn')
churn_train_insurance <- training(churn_split_insurance)
churn_test_insurance <- testing(churn_split_insurance)

# Model 1: Using 'loyalty_years'
model1_insurance <- glm(churn ~ loyalty_years, family = 'binomial', data = churn_train_insurance)

# Model 2: Using 'vehicles_covered' and 'premium_plan_ind'
model2_insurance <- glm(churn ~ vehicles_covered + premium_plan_ind, family = 'binomial', data = churn_train_insurance)

# Model 3: Using all available features
model3_insurance <- glm(churn ~ ., family = 'binomial', data = churn_train_insurance)

# Assessing Model Accuracy
set.seed(123)
cv_model1_insurance <- train(
  churn ~ loyalty_years,
  data = churn_train_insurance,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123)
cv_model2_insurance <- train(
  churn ~ vehicles_covered + premium_plan_ind,
  data = churn_train_insurance,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123)
cv_model3_insurance <- train(
  churn ~ .,
  data = churn_train_insurance,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

# Model summaries
summary(resamples(list(model1 = cv_model1_insurance, model2 = cv_model2_insurance, model3 = cv_model3_insurance)))

pred_class <- predict(cv_model3_insurance, data = churn_train_insurance)


library(ROCR)

# Compute predicted probabilities
m1_prob <- predict(cv_model1_insurance, churn_train_insurance, type = 'raw')
m3_prob <- predict(cv_model3_insurance, churn_train_insurance, type = 'raw')

perf1 <- prediction(m1_prob, churn_train_insurance$churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_train_insurance$churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)

### Feature interpretation
vip(cv_model3, num_features = 20)

