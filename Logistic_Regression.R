rm(list=ls())

library(caret)
library(pROC)

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

# Logistic Regression with normalization
model_lr_nor <- glm(data = df_train, target ~ acousticness + danceability + duration_ms + energy + instrumentalness + 
                      liveness + loudness + mode + speechiness + tempo, family = 'binomial')

pred_proba_lr_nor <- predict(model_lr_nor, newdata = df_test, type = 'response')

hist(pred_proba_lr_nor, xlab = "Values of probabilites",  main = "Distribution of probabilites for LR with normalization", col = "brown")

df_test$predict_lr_nor <- 0
df_test[pred_proba_lr_nor>t, ]$predict_lr_nor <- 1

confusionMatrix(factor(df_test$predict_lr_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)))

precision <- posPredValue(factor(df_test$predict_lr_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)),
                          positive="1")
recall <- sensitivity(factor(df_test$predict_lr_nor, levels=c(1,0)), factor(df_test$target, levels=c(1,0)), positive="1")

F1_lr_nor <- (2 * precision * recall) / (precision + recall)
F1_lr_nor


# Logistic Regression without normalization

model_lr <- glm(data = d.train, target ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo, family = 'binomial')

pred_proba_lr <- predict(model_lr, newdata = d.test, type = 'response')

hist(pred_proba_lr, xlab = "Values of probabilites",  main = "Distribution of probabilites for LR without normalization", col = "brown")

d.test$predict_lr <- 0
d.test[pred_proba_lr>t, ]$predict_lr <- 1

confusionMatrix(factor(d.test$predict_lr, levels=c(1,0)), factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(factor(d.test$predict_lr, levels=c(1,0)), factor(d.test$target, levels=c(1,0)), positive="1")
recall <- sensitivity(factor(d.test$predict_lr, levels=c(1,0)), factor(d.test$target, levels=c(1,0)), positive="1")

F1_lr <- (2 * precision * recall) / (precision + recall)
F1_lr


roc.model_lr_nor <- roc(df_test$target, pred_proba_lr_nor)
roc.model_lr <- roc(d.test$target, pred_proba_lr)

colors <- c("red", "blue")
plot(roc.model_lr_nor, col=colors[1], main="ROC for Logistic Regression")
lines(roc.model_lr, col=colors[2])

label_lr_nor <- paste('With normalization')
label_lr <- paste('Without normalization')
lbls <- c(label_lr_nor, label_lr)
legend(0.2, 0.3, lbls, cex = 0.7, fill = colors)



