library(corrplot)
library(hexbin)

data <- read.csv("./data/spotify-cleaned.csv", header=TRUE)

# CORRELATION TESTS
correlation_fields <- c("acousticness", "danceability", "duration_ms", "energy", "liveness", "loudness", "popularity", "speechiness", "tempo", "valence")
data_correlation <- subset(data, select=correlation_fields)
correlation <- cor(data_correlation)

# Visualizing Correlation
corrplot(correlation, method="number", type="upper", tl.col="black", tl.srt=45)
corrplot(correlation, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Not great fit but we can still remove
hexbinplot(data$energy ~ data$loudness, xlab="Loudness", ylab="Enery", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Enery vs Loudness", aspect=1)


# Let's keep it and see what happens
hexbinplot(data$energy ~ data$acousticness, xlab="Acousticness", ylab="Enery", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Enery vs Acousticness", aspect=1)

correlation_fields <- c("acousticness", "danceability", "duration_ms", "energy", "liveness", "popularity", "speechiness", "tempo", "valence")
data_correlation <- subset(data, select=correlation_fields)
correlation <- cor(data_correlation)

# Visualizing Correlation
corrplot(correlation, method="number", type="upper", tl.col="black", tl.srt=45)
corrplot(correlation, type="upper", order="hclust", tl.col="black", tl.srt=45)


write.csv(data_correlation, './data/spotify-filtered.csv', row.names = FALSE)
