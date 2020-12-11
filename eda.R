library(ggplot2)
require(gridExtra)

# make sure to setwd(/youe/directory)
df <- read.delim(file="./data_from_2000/spotify.csv", header=TRUE, sep=",")
summary(df)

requiredFields = c("valence", "year", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "loudness", "popularity", "name", "speechiness", "tempo" )
data <- subset(df, select=requiredFields)
summary(data)

# check for null values
any(is.na(data))
# O/P: FALSE => No null values

# HISTOGRAM DENSITY PLOTS
valence_histogram <- ggplot(data, aes(x=valence)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="purple")

acousticness_histogram <- ggplot(data, aes(x=acousticness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="green")

danceability_histogram <- ggplot(data, aes(x=danceability)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="blue")

duration_histogram <- ggplot(data, aes(x=duration_ms)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="chocolate")

duration_log_histogram <- ggplot(data, aes(x=log10(duration_ms))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="chocolate")

energy_histogram <- ggplot(data, aes(x=energy)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="cyan")

instrumentalness_histogram <- ggplot(data, aes(x=instrumentalness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="darkblue")

instrumentalness_log_histogram <- ggplot(data, aes(x=log10(instrumentalness))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="darkblue")

liveness_histogram <- ggplot(data, aes(x=liveness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="red")

loudness_histogram <- ggplot(data, aes(x=loudness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="darkseagreen")

popularity_histogram <- ggplot(data, aes(x=popularity)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="deeppink")

speechiness_histogram <- ggplot(data, aes(x=speechiness)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="navy")

speechiness_log_histogram <- ggplot(data, aes(x=log10(speechiness))) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="navy")

tempo_histogram <- ggplot(data, aes(x=tempo)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=0.2, fill="plum")

grid.arrange(valence_histogram, acousticness_histogram, danceability_histogram, 
             duration_histogram, duration_log_histogram, energy_histogram, 
             instrumentalness_histogram, instrumentalness_log_histogram, liveness_histogram, 
             loudness_histogram, popularity_histogram, tempo_histogram,
             speechiness_histogram, speechiness_log_histogram,
             ncol=3)

# We see only danceability, popularity and tempo are approximately normally distributed

# BOX PLOTS
acousticness <- data$acousticness
danceability <- data$danceability
energy <- data$energy
instrumentalness <- data$instrumentalness
liveness <- data$liveness
speechiness <- data$speechiness
valence <- data$valence

boxplot(acousticness, danceability, energy, instrumentalness, liveness, speechiness, valence,
        at=c(1,2,3,4,5,6,7), 
        names=c("Acousticness", "Danceability", "Energy", "Instrumentalness", "Liveness", "Speechiness", "Valence"),
        col=c("purple", "darkseagreen", "blue", "chocolate", "cyan", "navy", "deeppink"),
        las = 2)
# Lot of outliers, do we remove ?

popularity <- data$popularity
boxplot(popularity, col="darkseagreen", ylab="Popularity", horizontal=TRUE)

tempo <- data$tempo
boxplot(tempo, col="navy", ylab="Tempo", horizontal=TRUE)

loudness <- data$loudness
boxplot(loudness, col="lightblue", ylab="Loudness", horizontal=TRUE)

duration <- data$duration_ms / 1000 
boxplot(duration, col="lightblue", ylab="Duration in seconds", horizontal=TRUE)

# PAIR PLOTS
# NOTE -> this doesnt give any inferable plot
# And needs a loooot of RAM => DONT RUN
# numeric_fields <-  c("valence", "year", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "loudness", "popularity", "speechiness", "tempo" )
# data_numeric <- subset(data, select=numeric_fields)
# summary(data_numeric)
# pairs(data_numeric)

# CORRELATION TESTS
correlation_fields <-  c("valence", "year", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo" )
data_correlation <- subset(data, select=correlation_fields)
summary(data_correlation)

correlation <- cor(data_correlation)
correlation

# Visualizing correlation
# install.packages("corrplot")
library(corrplot)
corrplot(correlation, type="upper", order="hclust", tl.col="black", tl.srt=45)
corrplot(correlation, method="color", type="upper", tl.col="black", tl.srt=45)
corrplot(correlation, method="number", type="upper", tl.col="black", tl.srt=45)
# should i change colors?


# SCATTER PLOT BETWEEN FEATURE AND POPULARITY
plot(danceability, popularity, xlab="Danceability", ylab="Popularity", pch=20)

# due to high density of data, we shall use hexbin
# install.packages("hexbin")
library(hexbin)
# NOTE - g adds grid, r adds regression line

# Popularity
hexbinplot(popularity ~ danceability, xlab="Danceability", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Danceability", aspect=1)

hexbinplot(popularity ~ acousticness, xlab="Acousticness", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Acousticness", aspect=1)

hexbinplot(popularity ~ valence, xlab="Valence", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Valence", aspect=1)

hexbinplot(popularity ~ duration, xlab="Duration in ms", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Duration", aspect=3/4 )

hexbinplot(popularity ~ energy, xlab="Energy", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Energy", aspect=1)

hexbinplot(popularity ~ instrumentalness, xlab="Instrumentalness", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Instrumentalness", aspect=1)

hexbinplot(popularity ~ liveness, xlab="Liveness", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Liveness", aspect=1)

hexbinplot(popularity ~ loudness, xlab="Loudness", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Loudness", aspect=1)

hexbinplot(popularity ~ tempo, xlab="Tempo", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Tempo", aspect=1)

hexbinplot(popularity ~ speechiness, xlab="Speechiness", ylab="Popularity", 
             style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
             main="Popularity vs Speechiness", aspect=1)

# Acousticness
hexbinplot(acousticness ~ danceability, xlab="Danceability", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Danceability", aspect=1)

hexbinplot(acousticness ~ valence, xlab="Valence", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Valence", aspect=1)

hexbinplot(acousticness ~ duration, xlab="Duration in ms", ylab="Acousticness", 
           style="nested.centroids", type=c("g"),
           main="Acousticness vs Duration", aspect=1 )

hexbinplot(acousticness ~ energy, xlab="Energy", ylab="Acousticness", 
           style="nested.centroids", type=c("g"), 
           main="Acousticness vs Energy", aspect=1)

hexbinplot(acousticness ~ instrumentalness, xlab="Instrumentalness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Instrumentalness", aspect=1)

hexbinplot(acousticness ~ liveness, xlab="Liveness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Liveness", aspect=1)

hexbinplot(acousticness ~ loudness, xlab="Loudness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Loudness", aspect=1)

hexbinplot(acousticness ~ tempo, xlab="Tempo", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Tempo", aspect=1)

hexbinplot(acousticness ~ speechiness, xlab="Speechiness", ylab="Acousticness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Acousticness vs Speechiness", aspect=1)

# Danceability
hexbinplot(danceability ~ valence, xlab="Valence", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Valence", aspect=1)

hexbinplot(danceability ~ duration, xlab="Duration in ms", ylab="Danceability", 
           style="nested.centroids", type=c("g"),
           main="Danceability vs Duration", aspect=1 )

hexbinplot(danceability ~ energy, xlab="Energy", ylab="Danceability", 
           style="nested.centroids", type=c("g"), 
           main="Danceability vs Energy", aspect=1)

hexbinplot(danceability ~ instrumentalness, xlab="Instrumentalness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Instrumentalness", aspect=1)

hexbinplot(danceability ~ liveness, xlab="Liveness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Liveness", aspect=1)

hexbinplot(danceability ~ loudness, xlab="Loudness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Loudness", aspect=1)

hexbinplot(danceability ~ tempo, xlab="Tempo", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Tempo", aspect=1)

hexbinplot(danceability ~ speechiness, xlab="Speechiness", ylab="Danceability", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Danceability vs Speechiness", aspect=1)

# Valence
hexbinplot(valence ~ duration, xlab="Duration in ms", ylab="Valence", 
           style="nested.centroids", type=c("g"),
           main="Valence vs Duration", aspect=1 )

hexbinplot(valence ~ energy, xlab="Energy", ylab="Valence", 
           style="nested.centroids", type=c("g"), 
           main="Valence vs Energy", aspect=1)

hexbinplot(valence ~ instrumentalness, xlab="Instrumentalness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Instrumentalness", aspect=1)

hexbinplot(valence ~ liveness, xlab="Liveness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Liveness", aspect=1)

hexbinplot(valence ~ loudness, xlab="Loudness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Loudness", aspect=1)

hexbinplot(valence ~ tempo, xlab="Tempo", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Tempo", aspect=1)

hexbinplot(valence ~ speechiness, xlab="Speechiness", ylab="Valence", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Valence vs Speechiness", aspect=1)

# Duration
hexbinplot(duration ~ energy, xlab="Energy", ylab="Duration in ms", 
           style="nested.centroids", type=c("g"), 
           main="Duration in ms vs Energy", aspect=1)

hexbinplot(duration ~ instrumentalness, xlab="Instrumentalness", ylab="Duration in ms", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Instrumentalness", aspect=1)

hexbinplot(duration ~ liveness, xlab="Liveness", ylab="Duration in ms", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Liveness", aspect=1)

hexbinplot(duration ~ loudness, xlab="Loudness", ylab="Duration in ms", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Loudness", aspect=1)

hexbinplot(duration ~ tempo, xlab="Tempo", ylab="Duration in ms", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Tempo", aspect=1)

hexbinplot(duration ~ speechiness, xlab="Speechiness", ylab="Duration in ms", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Duration in ms vs Speechiness", aspect=1)

# Energy
hexbinplot(energy ~ instrumentalness, xlab="Instrumentalness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Instrumentalness", aspect=1)

hexbinplot(energy ~ liveness, xlab="Liveness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Liveness", aspect=1)

hexbinplot(energy ~ loudness, xlab="Loudness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Loudness", aspect=1)

hexbinplot(energy ~ tempo, xlab="Tempo", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Tempo", aspect=1)

hexbinplot(energy ~ speechiness, xlab="Speechiness", ylab="Energy", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Energy vs Speechiness", aspect=1)

# Instrumentalness
hexbinplot(instrumentalness ~ liveness, xlab="Liveness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Liveness", aspect=1)

hexbinplot(instrumentalness ~ loudness, xlab="Loudness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Loudness", aspect=1)

hexbinplot(instrumentalness ~ tempo, xlab="Tempo", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Tempo", aspect=1)

hexbinplot(instrumentalness ~ speechiness, xlab="Speechiness", ylab="Instrumentalness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Instrumentalness vs Speechiness", aspect=1)

# Liveness
hexbinplot(liveness ~ loudness, xlab="Loudness", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Loudness", aspect=1)

hexbinplot(liveness ~ tempo, xlab="Tempo", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Tempo", aspect=1)

hexbinplot(liveness ~ speechiness, xlab="Speechiness", ylab="Liveness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Liveness vs Speechiness", aspect=1)

# Loudness
hexbinplot(loudness ~ tempo, xlab="Tempo", ylab="Loudness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Loudness vs Tempo", aspect=1)

hexbinplot(loudness ~ speechiness, xlab="Speechiness", ylab="Loudness", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Loudness vs Speechiness", aspect=1)

# Tempo
hexbinplot(tempo ~ speechiness, xlab="Speechiness", ylab="Tempo", 
           style="nested.centroids", type=c("g","r"), col.line="yellow", lwd="5",
           main="Tempo vs Speechiness", aspect=1)




