rm(list=ls())

library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

d <- read.csv("C:/Users/Karol/R_Projects/WIZ/PROJECT/data.csv", header=TRUE, sep=",")
summary(d)
TRAIN_DATA_SIZE = 0.8
CONSTANT_SEED <- 11

# Preproccesing and splitting data
d$duration_in_s <- (d$duration_ms)/1000
d$target_factor <- factor(d$target)

plot(d$target_factor)
summary(d$duration_ms)

n <- nrow(d)
i <- floor(TRAIN_DATA_SIZE * n)
set.seed(CONSTANT_SEED)
s <- sample.int(n, i, replace = F)

t <- 0.5

d.train <- d[s,]
d.test <- d[-s,]

split <- sample.split(d$target, SplitRatio=TRAIN_DATA_SIZE)
df_train <- subset(d, split == TRUE)
df_test <- subset(d, split == FALSE)

df_train[, !names(df_train) %in% c("target", "mode", "artist", "song_title", "target_factor")] <- 
  scale(df_train[, !names(df_train) %in% c("target", "mode", "artist", "song_title", "target_factor")])

df_test[, !names(df_test) %in% c("target", "mode", "artist", "song_title", "target_factor")] <- 
  scale(df_test[, !names(df_test) %in% c("target", "mode", "artist", "song_title", "target_factor")])

# Trees with normalization
model_trees_nor <- rpart(data=df_train, target ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo)

prp(model_trees_nor)

pred_proba_trees_nor <- predict(model_trees_nor, newdata = df_test, type = 'vector')

hist(pred_proba_trees_nor, xlab = "Values of probabilites",  main = "Distribution of probabilites for Trees with normalization", col = "orange")

df_test$predict_trees_nor <- 0
df_test[pred_proba_trees_nor>t, ]$predict_trees_nor <- 1

confusionMatrix(factor(df_test$predict_trees_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)))

precision <- posPredValue(factor(df_test$predict_trees_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)), 
                          positive="1")

recall <- sensitivity(factor(df_test$predict_trees_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)), 
                      positive="1")

F1_trees_nor <- (2 * precision * recall) / (precision + recall)
F1_trees_nor

# Trees without normalization
model_trees <- rpart(data=d.train,  target ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo)

prp(model_trees)

pred_proba_trees <- predict(model_trees, newdata = d.test, type = 'vector')

hist(pred_proba_trees, xlab = "Values of probabilites",  main = "Distribution of probabilites for Trees without normalization", col = "orange")

d.test$predict_trees <- 0
d.test[pred_proba_trees>t, ]$predict_trees <- 1

confusionMatrix(factor(d.test$predict_trees, levels=c(1,0)), factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(factor(d.test$predict_trees, levels=c(1,0)), factor(d.test$target, levels=c(1,0)), positive="1")

recall <- sensitivity(factor(d.test$predict_trees, levels=c(1,0)), factor(d.test$target, levels=c(1,0)), positive="1")

F1_trees <- (2 * precision * recall) / (precision + recall)
F1_trees

roc.model_trees_nor <- roc(df_test$target, pred_proba_trees_nor)
roc.model_trees <- roc(d.test$target, pred_proba_trees)

colors <- c("purple", "pink")
plot(roc.model_trees_nor, col=colors[1], main="ROC for Trees")
lines(roc.model_trees, col=colors[2])

label_trees_nor <- paste('With normalization')
label_trees <- paste('Without normalization')
lbls <- c(label_trees_nor, label_trees)
legend(0.2, 0.3, lbls, cex = 0.7, fill = colors)



