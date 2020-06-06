rm(list=ls())

# Support Vector Machine - Support Vector Classifier (SVM - SVC)

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

man_grid <- expand.grid(degree = seq(from = 1, to = 8, by = 1),
                        C = seq(from = 0.1, to = 22, by = 2),
                        scale = seq(from = 0.01, to = 0.1, by = 0.03))

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "grid")

model_svc <- train(form = factor(target) ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo, 
                   data = d.train,
                   method = "svmPoly", 
                   trControl = fitControl,
                   verbose = FALSE,
                   tuneGrid = man_grid)
toc()

plot(model_svc)
plot(model_svc, metric = "Kappa", plotType = "level")
plot(model_svc, metric = "Accuracy", plotType = "level")
ggplot(model_svc)
ggplot(model_svc, metric = "Kappa", plotType = "level")
ggplot(model_svc, metric = "Accuracy", plotType = "level")

pred_target_svc <- predict(model_svc, newdata = d.test)

confusionMatrix(pred_target_svc, factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(pred_target_svc, factor(d.test$target, levels=c(1,0)), positive="1")
recall <- sensitivity(pred_target_svc, factor(d.test$target, levels=c(1,0)),  positive="1")

F1_svc <- (2 * precision * recall) / (precision + recall)
F1_svc

roc.model_svc <- roc(d.test$target, unfactor(pred_target_svc))
plot(roc.model_svc)
roc.model_svc



