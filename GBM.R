rm(list=ls())

# Gradient Boosting Machines (GBM)

library(caret)
library(pROC)
library(tictoc)
library(varhandle)
library(ggplot2)

d <- read.csv("C:/Users/Karol/R_Projects/WIZ/PROJECT/data.csv", header=TRUE, sep=",")
summary(d)
TRAIN_DATA_SIZE = 0.8
CONSTANT_SEED <- 11

# Preproccesing and splitting data
d$duration_in_s <- (d$duration_ms)/1000

n <- nrow(d)
i <- floor(TRAIN_DATA_SIZE * n)
set.seed(CONSTANT_SEED)
s <- sample.int(n, i, replace = F)

d.train <- d[s,]
d.test <- d[-s,]

man_grid <- expand.grid(n.trees = seq(from = 10, to = 300, by = 20),
                        interaction.depth = seq(from = 1, to = 10, length.out = 6), 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "grid")
tic()
model_gbm <- train(form = factor(target) ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo, 
                   data = d.train,
                   method = "gbm", 
                   trControl = fitControl,
                   verbose = FALSE,
                   tuneGrid = man_grid)
toc()

plot(model_gbm)
plot(model_gbm, metric = "Kappa", plotType = "level")
plot(model_gbm, metric = "Accuracy", plotType = "level")
ggplot(model_gbm)
ggplot(model_gbm, metric = "Kappa", plotType = "level")
ggplot(model_gbm, metric = "Accuracy", plotType = "level")

pred_target_gbm <- predict(model_gbm, newdata = d.test)

confusionMatrix(pred_target_gbm, factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(pred_target_gbm,factor(d.test$target, levels=c(1,0)), positive="1")
recall <- sensitivity(pred_target_gbm, factor(d.test$target, levels=c(1,0)), positive="1")

F1_gbm <- (2 * precision * recall) / (precision + recall)
F1_gbm

roc.model_gbm <- roc(d.test$target, unfactor(pred_target_gbm))
plot(roc.model_gbm)
roc.model_gbm



