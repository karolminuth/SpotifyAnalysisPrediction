rm(list=ls())

library(caTools)
library(caret)
library(data.table)

df <- read.csv("C:/Users/Karol/R_Projects/WIZ/PROJECT/data.csv", header=TRUE, sep=",")
df$target_factor <- factor(df$target)
df$duration_in_s <- (df$duration_ms)/1000

# Data analysis, presentation and visualization


# Target presentation
plot(df$target_factor, main="Likes or not likes music")

# One of the advantage of this dataset is almost equal amount of negative and positive values of target
not_likes <- df[df$target_factor == "0",]
likes <- df[df$target_factor == "1",]
not_likes_amonut <- as.numeric(length(not_likes$target_factor))
likes_amount <- as.numeric(length(likes$target_factor))
data_pie <- c(not_likes_amonut, likes_amount)
lbls <- paste(names(data_pie))
pct <- round(data_pie/sum(data_pie)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c("red", "green")
pie(data_pie,labels = lbls, col=colors, main="Likes or not likes music")
label_no_chunk <- paste('Not Likes -> ', as.character(data_pie[1]))
label_chunk <- paste('Likes -> ', as.character(data_pie[2]))
lbls <- c(label_no_chunk, label_chunk)
legend(.9, .1, lbls, cex = 0.7, fill = colors)

# The most favourite music are songs with tempo in range 125+-5
hist(df$tempo, breaks = 15, probability = T, xlab = "Tempo", main = "Distribution of tempo", col = "blue")
lines(density(df$tempo), col="red", lwd=3, lty=5)

# Here is shown that smaller range of tempo will be from 125 to 130
hist(df$tempo, breaks = 25, probability = T, xlab = "Tempo",  main = "Distribution of tempo", col = "blue")
lines(density(df$tempo), col="red", lwd=3, lty=5)

# In below chart is shown the advantage of duration in rnage from 200 to 250 seconds
hist(df$duration_in_s, breaks=30, probability = T, xlab = "Duration in seconds",  main = "Distribution of duration time of songs", col = 'yellow')
lines(density(df$duration_in_s), col="red", lwd=3, lty=5)

# Here is shown that the most of songs have 220 +- 20 seconds
hist(df$duration_in_s, breaks=40, probability = T, xlab = "Duration in seconds",  main = "Distribution of duration time of songs", col = 'yellow')
lines(density(df$duration_in_s), col="red", lwd=3, lty=5)


# In below diagrams are shown the influences of the most relevant variables for target with easily distinct differences 

# In below chart it is possible to notice little bigger mean values and median value for duration in seconds for target equal one
plot(df$duration_in_s~df$target_factor, xlab = 'Target', ylab = 'Duration in seconds')

# In below chart it is possible to notice little bigger mean values and median value for acousticness for target equal zero. 
# According of that it is likely to notice that user not enjoy with large values of acousticness
plot(df$acousticness~df$target_factor, xlab = 'Target', ylab = 'Acousticness')

# In below chart it is possible to notice little bigger mean values and median value for energy for target equal zero. 
# According of that it is likely to notice that user not enjoy with large values of energy
plot(df$energy~df$target_factor, xlab = 'Target', ylab = 'Energy')

# In below chart it is possible to notice little bigger mean values and median value for energy for target equal zero. 
# According of that it is likely to notice that user not enjoy with large values of energy
plot(df$tempo~df$target_factor, xlab = 'Target', ylab = 'Tempo')

# In below chart it is possible to notice much bigger mean values for instrumentalness for target equal one. 
# According of that it is likely to notice that user really enjoy of instrumentalness
plot(df$instrumentalness~df$target_factor, xlab = 'Target', ylab = 'Instrumentalness')

# In below chart it is possible to notice much bigger mean values and median value for danceability for target equal one. 
# According of that it is likely to notice that user really enjoy with danceability songs
plot(df$danceability~df$target_factor, xlab = 'Target', ylab = 'Danceability')


# In below diagram is shown the influence between 2 variables
# In below chart it is possible to notice that the best songs to dance are with tempo equal more or less 130
plot(df$danceability~df$tempo, pch=2, xlab ='Tempo', ylab = 'Danceability')



