library(ggplot2)
require(gridExtra)
library(hexbin)

# DATA PREPARATION
data <- read.csv("./data/spotify-cleaned.csv", header=TRUE)

# HISTOGRAM DENSITY PLOTS
acousticness_histogram <- ggplot(data, aes(x=acousticness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="green")

# acousticness_histogram

danceability_histogram <- ggplot(data, aes(x=danceability)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="blue")

# danceability_histogram

duration_histogram <- ggplot(data, aes(x=duration_ms)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="chocolate")

# duration_histogram

duration_log_histogram <- ggplot(data, aes(x=log10(duration_ms))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="chocolate")

# duration_log_histogram

energy_histogram <- ggplot(data, aes(x=energy)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="cyan")

# energy_histogram

liveness_histogram <- ggplot(data, aes(x=liveness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="red")

# liveness_histogram

loudness_histogram <- ggplot(data, aes(x=loudness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="darkseagreen")

# loudness_histogram

popularity_histogram <- ggplot(data, aes(x=popularity)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="deeppink")

# popularity_histogram

speechiness_histogram <- ggplot(data, aes(x=speechiness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="navy")

# speechiness_histogram

speechiness_log_histogram <- ggplot(data, aes(x=log10(speechiness))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="navy")

# speechiness_log_histogram

tempo_histogram <- ggplot(data, aes(x=tempo)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="plum")

# tempo_histogram

valence_histogram <- ggplot(data, aes(x=valence)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", bins=40) +
  geom_density(alpha=0.2, fill="purple")

# valence_histogram

grid.arrange(acousticness_histogram, danceability_histogram, duration_histogram,
             duration_log_histogram, energy_histogram, liveness_histogram, 
             loudness_histogram, popularity_histogram, tempo_histogram, 
             speechiness_histogram, speechiness_log_histogram, valence_histogram, 
             ncol=3)


# BOX PLOTS
boxplot(data$acousticness, data$danceability, data$energy, data$liveness, data$speechiness, data$valence,
        at=c(1, 2, 3, 4, 5, 6), 
        names=c("Acousticness", "Danceability", "Energy", "Liveness",  "Speechiness", "Valence"),
        col=c("purple", "darkseagreen", "chocolate", "cyan", "navy", "deeppink"),
        las = 2)

boxplot(data$popularity, col="darkseagreen", ylab="Popularity", horizontal=TRUE)
boxplot(data$tempo, col="navy", ylab="Tempo", horizontal=TRUE)
boxplot(data$loudness, col="lightblue", ylab="Loudness", horizontal=TRUE)
boxplot(data$duration, col="lightblue", ylab="Duration in seconds", horizontal=TRUE)

# Due to high density of data, we shall use hexbin
# NOTE - g adds grid, r adds regression line

# Popularity
hexbinplot(data$popularity ~ data$danceability, xlab="Danceability", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Danceability", aspect=1)

hexbinplot(data$popularity ~ data$acousticness, xlab="Acousticness", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Acousticness", aspect=1)

hexbinplot(data$popularity ~ data$valence, xlab="Valence", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Valence", aspect=1)

hexbinplot(data$popularity ~ data$duration / 1000, xlab="Duration", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Duration", aspect=3/4 )

hexbinplot(data$popularity ~ data$energy, xlab="Energy", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Energy", aspect=1)

hexbinplot(data$popularity ~ data$instrumentalness, xlab="Instrumentalness", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Instrumentalness", aspect=1)

hexbinplot(data$popularity ~ data$liveness, xlab="Liveness", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Liveness", aspect=1)

hexbinplot(data$popularity ~ data$loudness, xlab="Loudness", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Loudness", aspect=1)

hexbinplot(data$popularity ~ data$tempo, xlab="Tempo", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Tempo", aspect=1)

hexbinplot(data$popularity ~ data$speechiness, xlab="Speechiness", ylab="Popularity", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Popularity vs Speechiness", aspect=1)

# Acousticness
hexbinplot(data$acousticness ~ data$danceability, xlab="Danceability", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Danceability", aspect=1)

hexbinplot(data$acousticness ~ data$valence, xlab="Valence", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Valence", aspect=1)

hexbinplot(data$acousticness ~ data$duration / 1000, xlab="Duration", ylab="Acousticness", 
           style="nested.centroids", type=c("g"),
           main="Acousticness vs Duration", aspect=1 )

hexbinplot(data$acousticness ~ data$energy, xlab="Energy", ylab="Acousticness", 
           style="nested.centroids", type=c("g"), 
           main="Acousticness vs Energy", aspect=1)

hexbinplot(data$acousticness ~ data$instrumentalness, xlab="Instrumentalness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Instrumentalness", aspect=1)

hexbinplot(data$acousticness ~ data$liveness, xlab="Liveness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Liveness", aspect=1)

hexbinplot(data$acousticness ~ data$loudness, xlab="Loudness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Loudness", aspect=1)

hexbinplot(data$acousticness ~ data$tempo, xlab="Tempo", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Tempo", aspect=1)

hexbinplot(data$acousticness ~ data$speechiness, xlab="Speechiness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Speechiness", aspect=1)

# Danceability
hexbinplot(data$danceability ~ data$valence, xlab="Valence", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Valence", aspect=1)

hexbinplot(data$danceability ~ data$duration / 1000, xlab="Duration", ylab="Danceability", 
           style="nested.centroids", type=c("g"),
           main="Danceability vs Duration", aspect=1 )

hexbinplot(data$danceability ~ data$energy, xlab="Energy", ylab="Danceability", 
           style="nested.centroids", type=c("g"), 
           main="Danceability vs Energy", aspect=1)

hexbinplot(data$danceability ~ data$instrumentalness, xlab="Instrumentalness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Instrumentalness", aspect=1)

hexbinplot(data$danceability ~ data$liveness, xlab="Liveness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Liveness", aspect=1)

hexbinplot(data$danceability ~ data$loudness, xlab="Loudness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Loudness", aspect=1)

hexbinplot(data$danceability ~ data$tempo, xlab="Tempo", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Tempo", aspect=1)

hexbinplot(data$danceability ~ data$speechiness, xlab="Speechiness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Speechiness", aspect=1)

# Valence
hexbinplot(data$valence ~ data$duration / 1000, xlab="Duration", ylab="Valence", 
           style="nested.centroids", type=c("g"),
           main="Valence vs Duration", aspect=1 )

hexbinplot(data$valence ~ data$energy, xlab="Energy", ylab="Valence", 
           style="nested.centroids", type=c("g"), 
           main="Valence vs Energy", aspect=1)

hexbinplot(data$valence ~ data$instrumentalness, xlab="Instrumentalness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Instrumentalness", aspect=1)

hexbinplot(data$valence ~ data$liveness, xlab="Liveness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Liveness", aspect=1)

hexbinplot(data$valence ~ data$loudness, xlab="Loudness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Loudness", aspect=1)

hexbinplot(data$valence ~ data$tempo, xlab="Tempo", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Tempo", aspect=1)

hexbinplot(data$valence ~ data$speechiness, xlab="Speechiness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Speechiness", aspect=1)

# Duration
hexbinplot(data$duration / 1000 ~ data$energy, xlab="Energy", ylab="Duration", 
           style="nested.centroids", type=c("g"), 
           main="Duration in ms vs Energy", aspect=1)

hexbinplot(data$duration / 1000 ~ data$instrumentalness, xlab="Instrumentalness", ylab="Durations", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Instrumentalness", aspect=1)

hexbinplot(data$duration / 1000 ~ data$liveness, xlab="Liveness", ylab="Duration", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Liveness", aspect=1)

hexbinplot(data$duration / 1000 ~ data$loudness, xlab="Loudness", ylab="Duration", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Loudness", aspect=1)

hexbinplot(data$duration / 1000 ~ data$tempo, xlab="Tempo", ylab="Duration", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Tempo", aspect=1)

hexbinplot(data$duration / 1000 ~ data$speechiness, xlab="Speechiness", ylab="Duration", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Speechiness", aspect=1)

# Energy
hexbinplot(data$energy ~ data$instrumentalness, xlab="Instrumentalness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Instrumentalness", aspect=1)

hexbinplot(data$energy ~ data$liveness, xlab="Liveness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Liveness", aspect=1)

hexbinplot(data$energy ~ data$loudness, xlab="Loudness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Loudness", aspect=1)

hexbinplot(data$energy ~ data$tempo, xlab="Tempo", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Tempo", aspect=1)

hexbinplot(data$energy ~ data$speechiness, xlab="Speechiness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Speechiness", aspect=1)

# Instrumentalness
hexbinplot(data$instrumentalness ~ data$liveness, xlab="Liveness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Liveness", aspect=1)

hexbinplot(data$instrumentalness ~ data$loudness, xlab="Loudness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Loudness", aspect=1)

hexbinplot(data$instrumentalness ~ data$tempo, xlab="Tempo", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Tempo", aspect=1)

hexbinplot(data$instrumentalness ~ data$speechiness, xlab="Speechiness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Speechiness", aspect=1)

# Liveness
hexbinplot(data$liveness ~ data$loudness, xlab="Loudness", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Loudness", aspect=1)

hexbinplot(data$liveness ~ data$tempo, xlab="Tempo", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Tempo", aspect=1)

hexbinplot(data$liveness ~ data$speechiness, xlab="Speechiness", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Speechiness", aspect=1)

# Loudness
hexbinplot(data$loudness ~ data$tempo, xlab="Tempo", ylab="Loudness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Loudness vs Tempo", aspect=1)

hexbinplot(data$loudness ~ data$speechiness, xlab="Speechiness", ylab="Loudness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Loudness vs Speechiness", aspect=1)

# Tempo
hexbinplot(data$tempo ~ data$speechiness, xlab="Speechiness", ylab="Tempo", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Tempo vs Speechiness", aspect=1)
