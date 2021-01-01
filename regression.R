library('scales')
library('hexbin')

calculate_accuracy <- function(ground_truth, predictions) {
  ground_truth <- cut(ground_truth, breaks = seq(from = 0, to = 100, by = 1), labels = FALSE, include.lowest = TRUE)
  predictions <- cut(predictions, breaks = seq(from = 0, to = 100, by = 1), labels = FALSE, include.lowest = TRUE)
  values <- ground_truth %in% predictions
  percent(sum(values) / length(values)) 
}

data <- read.csv("./data/spotify-filtered.csv", header=TRUE)
data <- data[sample(nrow(data)),]
bound <- floor((nrow(data)/4)*3)  
train <- data[1 : bound, ]
test <- data[(bound + 1): nrow(data), ] 
summary(data)


"
  Regression Model - 1
"
model1 <- lm(popularity ~ acousticness + danceability + duration_ms + energy + 
               liveness + speechiness + tempo + valence, data=train)
summary(model1)

y = predict(model1, test)
calculate_accuracy(test$popularity, y)

res = resid(model3)
hexbinplot(res ~ train$popularity, xlab="Popularity", ylab="Residuals", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Residual Graph - 1", aspect=1)


"
  Regression Model - 2
  Since acousticness and Energy might have a relationship
"
model2 <- lm(popularity ~ danceability + duration_ms + liveness + speechiness + 
               tempo + valence + (acousticness * energy), data=train)
summary(model2)

y = predict(model2, test)
calculate_accuracy(test$popularity, y)

res = resid(model3)
hexbinplot(res ~ train$popularity, xlab="Popularity", ylab="Residuals", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Residual Graph - 2", aspect=1)


"
  Regression Model - 3
  Since liveness & speechiness might have a relation and danceability & valence
"
model3 <- lm(popularity ~ duration_ms + (acousticness * energy) + tempo +
               (liveness * speechiness) + (danceability * valence), data=train)
summary(model3)

y = predict(model3, test)
calculate_accuracy(test$popularity, y)

res = resid(model3)
hexbinplot(res ~ train$popularity, xlab="Popularity", ylab="Residuals", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Residual Graph - 3", aspect=1)


"
  Logistic Model
  Does not significantly increase accuracy. Within error ranges of 2 percent
"
model4 <- glm(popularity ~  duration_ms + (acousticness * energy) +
               (liveness * speechiness) + (danceability * valence), data=train)
summary(model4)

y = predict(model4, test)
calculate_accuracy(test$popularity, y)

res = resid(model4)
hexbinplot(res ~ train$popularity, xlab="Popularity", ylab="Residuals", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Residual Graph - 4", aspect=1)        
