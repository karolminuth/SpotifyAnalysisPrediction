rm(list=ls())

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

man_grid <- expand.grid(min.node.size = seq(from = 3, to = 12, by = 2),
                        mtry = seq(from = 1, to = 10, by = 2),
                        splitrule = c('gini', 'extratrees', 'hellinger'))

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "grid")
tic()
model_random_forest <- train(form = factor(target) ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo,
                             data = d.train,
                             method = "ranger",
                             trControl = fitControl,
                             verbose = TRUE,
                             tuneGrid = man_grid)
toc()

plot(model_random_forest)
plot(model_random_forest, metric = "Kappa", plotType = "level")
plot(model_random_forest, metric = "Accuracy", plotType = "level")
ggplot(model_random_forest)
ggplot(model_random_forest, metric = "Kappa", plotType = "level")
ggplot(model_random_forest, metric = "Accuracy", plotType = "level")

pred_target_random_forest <- predict(model_random_forest, newdata = d.test)

confusionMatrix(pred_target_random_forest, factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(pred_target_random_forest, factor(d.test$target, levels=c(1,0)), positive="1")

recall <- sensitivity(pred_target_random_forest, factor(d.test$target, levels=c(1,0)), positive="1")

F1_random_forest <- (2 * precision * recall) / (precision + recall)
F1_random_forest

roc.model_random_forest <- roc(d.test$target, unfactor(pred_target_random_forest))
plot(roc.model_random_forest)
roc.model_random_forest



