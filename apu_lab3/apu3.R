# zad 1 
library("neuralnet")

x <- as.data.frame(runif(300, min = 1, max = 16))
result <- exp(1)^sqrt(x)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

scaled.x <- as.data.frame(lapply(x, normalize))

training_data <- cbind(scaled.x, result)
colnames(training_data) <- c("Argument", "Wynik")

net.calculations <- neuralnet(Wynik~Argument, training_data, hidden = c(3,2,1),
                              threshold = 0.01, stepmax = 1e7)

plot(net.calculations)





# zad 1 test
test_data <- as.data.frame(runif(10, min = 1, max = 16))
scaled.test_data <- as.data.frame(lapply(test_data, normalize))

net.results <- compute(net.calculations, scaled.test_data)

ls(net.results)

real_results <- exp(1)^sqrt(test_data)

prediction_results <- cbind(test_data, net.results$net.result, real_results,
                            real_results - net.results$net.result)
colnames(prediction_results) <- c("Argument X", "Przewidziany wynik",
                                  "Prawdziwy wynik", "Error")

print(prediction_results)







# zad 2
setwd("D:/R")
aparaty <- read.csv("aparatyaaa.csv")

aparaty <- aparaty[,-6]
aparaty <- aparaty[,-2]
aparaty <- aparaty[,-1]
aparaty

aparaty <- as.data.frame(lapply(aparaty, normalize))

net.price <- neuralnet(cena~zakres_czulosci+rozdzielczosc,
                        aparaty, hidden = c(5,3,3), threshold = 0.01)

plot(net.price)


