# Użycie potrzebnych bibliotek
library("rFerns")
library("randomForest")
library("mlr")

# wczytanie i refaktoryzacja danych
setwd("C:/Users/Mateusz/Downloads/OneDrive_2_4.06.2024/lab44_apu")
aparaty <- read.csv("aparaty.csv")
aparaty <- aparaty[,-3]
aparaty <- aparaty[,-2]
aparaty <- aparaty[,-1]
aparaty$ocena <- as.factor(aparaty$ocena)

# utworzenie zadania klasyfikacji
aparaty_task <- makeClassifTask(data = aparaty, target = "ocena")

# tworzenie metod
lrns <- makeLearners(c("lda","rpart", "C50","rFerns",
                       "randomForest"), type = "classif")

# porownanie dzialania metod
comparasion <- benchmark(learners = lrns,
                        tasks = aparaty_task,
                        resampling = cv5)

comparasion

# manualnie przepisane wartości accuracy z benchmarku
learner <- c("lda", "rpart", "C50", "rFerns", "randomForest")
accuracy <- c(0.73, 0.87, 0.53, 0.60, 0.73)

# utworzenie wykresu slupkowego
data <- data.frame(learner, accuracy)
barplot(data$accuracy, names.arg = data$learner, ylim = c(0, 1), ylab = "Accuracy")

# utworzenie zestawów danych treningowych i testowych
aparaty_train <- aparaty[1:13, ]
aparaty_test <- aparaty[14:15, ]

# trenowanie modelu
aparaty_learner <- makeLearner("classif.rpart")
aparaty_model <- train(aparaty_learner, aparaty_task)

# predykcje
aparaty_predictions <- predict(aparaty_model, newdata = aparaty_test)

# określenie performencu po treningu
performance <- performance(aparaty_predictions, measures = list(acc))
print(performance)