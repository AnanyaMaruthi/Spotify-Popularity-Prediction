library(ggplot2)
require(gridExtra)

"
  DATA PREPARATION
"
data <- read.csv("./data/spotify.csv", header=TRUE)
names(data)[1] <- 'genre'
summary(data)
"
  Removing ID and Name Attributes
"
required_fields <- c('genre', 'artist_name', 'popularity', 'acousticness', 'danceability', 'duration_ms', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')
data <- subset(data, select=required_fields)
summary(data)
"
  Any null values present
"
any(is.na(data))


"
  Removing Irrelevant Columns
"
summary(data$instrumentalness)

instrumentalness_histogram <- ggplot(data, aes(x=data$instrumentalness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=100) +
  geom_density(alpha=0.2, fill="darkblue")

instrumentalness_log_histogram <- ggplot(data, aes(x=log10(data$instrumentalness))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="darkblue")

instrumentalness_histogram
instrumentalness_log_histogram

required_fields <- c('genre', 'artist_name', 'popularity', 'acousticness', 'danceability', 'duration_ms', 'energy', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')
data <- subset(data, select=required_fields)


"
  Removing Outliers
"
summary(data$duration_ms)
summary(log(data$duration_ms))

duration_ms_histogram <- ggplot(data, aes(x=data$duration_ms)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=100) +
  geom_density(alpha=0.2, fill="darkblue")

duration_ms_log_histogram <- ggplot(data, aes(x=log10(data$duration_ms))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="darkblue")

duration_ms_histogram
duration_ms_log_histogram

boxplot(data$duration_ms, names=c("Duration_ms"), col=c("darkblue"))
boxplot(log10(data$duration_ms), names=c("Duration_ms"), col=c("darkblue"))


duration_ms_iqr <- IQR(data$duration_ms)
duration_ms_iqr
duration_ms_median <- median(data$duration_ms)
duration_ms_median
duration_ms_min <- duration_ms_median - (duration_ms_iqr * 1.5)
duration_ms_max <- duration_ms_median + (duration_ms_iqr * 1.5)
duration_ms_min
duration_ms_max


data <- data[data$duration_ms <= duration_ms_max, ]
data <- data[data$duration_ms >= duration_ms_min, ]
summary(data$duration_ms)
summary(log(data$duration_ms))

duration_ms_histogram <- ggplot(data, aes(x=duration_ms / 1000)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=30) +
  geom_density(alpha=0.2, fill="darkblue")

duration_ms_log_histogram <- ggplot(data, aes(x=log10(duration_ms / 1000))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=20) +
  geom_density(alpha=0.2, fill="darkblue")

duration_ms_histogram
duration_ms_log_histogram

boxplot(data$duration_ms / 1000, names=c("Durations"), col=c("darkblue"))

write.csv(data, './data/spotify-cleaned.csv', row.names = FALSE)
