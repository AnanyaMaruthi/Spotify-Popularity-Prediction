library('scales')
library('hexbin')
library('randomForest')

calculate_accuracy <- function(ground_truth, predictions) {
  ground_truth <- cut(ground_truth, breaks = seq(from = 0, to = 100, by = 1), labels = FALSE, include.lowest = TRUE)
  predictions <- cut(predictions, breaks = seq(from = 0, to = 100, by = 1), labels = FALSE, include.lowest = TRUE)
  values <- ground_truth %in% predictions
  percent(sum(values) / length(values)) 
}

data <- read.csv("./data/spotify-filtered.csv", header=TRUE)
data <- data[sample(nrow(data)),]
# bound <- floor((nrow(data)/4)*1)  
bound <- floor(nrow(data) / 10)
train <- data[1 : bound, ]
test <- data[(bound + 1): nrow(data), ] 
summary(data)

model1 <- randomForest(popularity ~ acousticness + danceability + duration_ms + energy + speechiness + valence, data = train)
model1

"
  95%
"
y = predict(model1, test)
calculate_accuracy(test$popularity, y)
