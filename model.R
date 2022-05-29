library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# __________ Run at first time __________
raw <- read.csv("SBA.csv")

decision_rule <- function(ref, predict_logistic) {
  
  cutoff <- runif(50)
  missclassificationRate <- c()
  for (i in cutoff) {
    factor <- factor(ifelse(predict_logistic > i, "yes", "no"))
    df <- data.frame(factor = factor, actual = ref)
    n <- nrow(filter(df, as.character(factor)!=as.character(ref)))
    missclassificationRate <- append(missclassificationRate, n/nrow(df))
  }
  
  return(data.frame(
    cutoff = cutoff,
    missclassificationRate = missclassificationRate
  ))
}

# _______________________________________

# Reset data here.
data <- sample_frac(raw, 1)

data <- data %>%
  select(-X, -UrbanRural) %>%
  drop_na() %>%
  mutate(State = as.factor(State)) %>%
  mutate(NAICS = as.factor(NAICS)) %>%
  mutate(NewExist = as.factor(NewExist)) %>%
  mutate(RevLineCr = as.factor(RevLineCr)) %>%
  mutate(LowDoc = as.factor(LowDoc)) %>%
  mutate(Franchise = as.factor(Franchise)) %>%
  mutate(RealEstate = as.factor(RealEstate)) %>%
  mutate(Recession = as.factor(Recession)) %>%
  mutate(BankInState = as.factor(BankInState)) %>%
  mutate(Default = as.factor(Default))

# Hold out
set.seed(007)
n <- nrow(data)
train_id <- sample(1: n, size = 0.7*n)
train <- data[train_id,] 
test <- data[-train_id,]
table(train$Default)

# Regression model
model <- glm(Default ~ Term, data = train, family = binomial)
summary(model)
predict_logistic <- predict(model, test, type = "response")
decision <- decision_rule(test$Default, predict_logistic)
ggplot(decision, aes(cutoff, missclassificationRate)) + geom_point()
cutoff <- decision$cutoff[match(min(decision$missclassificationRate), decision$missclassificationRate)]
factor <- factor(ifelse(predict_logistic > cutoff, "yes", "no"))
confusionMatrix(factor, test$Default, positive = "no", mode = "prec_recall")

# Decision tree model
tree <- rpart(Default ~ ., data = train)
rpart.plot(tree)
tree$variable.importance
predict_tree <- predict(tree, test, type = "class")
confusionMatrix(predict_tree, test$Default, positive = "no", mode = "prec_recall")

# Lift
predict_prob <- predict(tree, test)[, "no"]
lift_result <- data.frame(prob = predict_prob, y = test$Default)
lift_obj <- lift(y ~ prob, data = lift_result, class = "no")
plot(lift_obj)

# Cross validation
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(Default ~ ., data = train, trControl = train_control, method = "rpart")
model_cv