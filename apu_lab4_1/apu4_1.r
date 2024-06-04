# Użycie potrzebnych bibliotek

library(datasets)
library(mlr)
library(readr)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(C50)

# Zaladowanie danych
cars = mtcars

# Konwersja kolumny klasyfikacyjnej na typ factor
cars$cyl <- as.factor(cars$cyl)
str(cars)

# Utworzenie klasyfikatora na podstawie liczby cylindrów
cars_task <- makeClassifTask(data = cars, target = "cyl")

# Podzial danych
data <- c("training", "test")
splitData <-  split(cars, data)

# Lista Learnerów
levels(factor(listLearners()$type))

# Utworzenie learnera i trening
learner <- makeLearner("classif.rpart")
model <- train(learner, cars_task)

# Ocena jakosci modelu
predictions <- predict(model, newdata = splitData$test)

performance <- performance(predictions, measures = list(acc))
print(performance)

summary(model)

# Drzewo decyzyjne
rpart.plot(getLearnerModel(model), roundint = FALSE)

# Reguły klasyfikacyjne
ruleModel <- C5.0(cyl ~ ., data=splitData$training)
summary(ruleModel)
plot(ruleModel)