rm(list=ls())

# eXtreme Gradient Boosting (XGB, xgboost)

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
HYPERMATERS_TUNNING_ITERATION <- 200
n <- nrow(d)
i <- floor(TRAIN_DATA_SIZE * n)
set.seed(CONSTANT_SEED)
s <- sample.int(n, i, replace = F)

d.train <- d[s,]
d.test <- d[-s,]

# # good parameters after long random search
# man_grid <- expand.grid(eta = 0.03,
#                         max_depth = c(6, 7, 8, 9),
#                         gamma = c(2, 3.5, 10, 15),
#                         colsample_bytree = 0.53,
#                         min_child_weight = 0,
#                         subsample = 0.54,
#                         nrounds = 650)
# 
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 3,
#                            repeats = 5,
#                            search = "grid")
# tic()
# model_xgb <- train(form = factor(target) ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo,
#                    data = d.train,
#                    method = "xgbTree",
#                    trControl = fitControl,
#                    verbose = TRUE,
#                    tuneGrid = man_grid)
# toc()

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "random")

tic()
model_xgb <- train(form = factor(target) ~ acousticness + danceability + duration_in_s + energy + instrumentalness + liveness + loudness + mode + speechiness + tempo,
                   data = d.train,
                   method = "xgbTree",
                   trControl = fitControl,
                   verbose = FALSE,
                   tuneLength = HYPERMATERS_TUNNING_ITERATION)
toc()

plot(model_xgb)
plot(model_xgb, metric = "Kappa", plotType = "level")
plot(model_xgb, metric = "Accuracy", plotType = "level")
ggplot(model_xgb)
ggplot(model_xgb, metric = "Kappa", plotType = "level")
ggplot(model_xgb, metric = "Accuracy", plotType = "level")

pred_target_xgb <- predict(model_xgb, newdata = d.test)

confusionMatrix(pred_target_xgb, factor(d.test$target, levels=c(1,0)))

precision <- posPredValue(pred_target_xgb, factor(d.test$target, levels=c(1,0)), positive="1")
recall <- sensitivity(pred_target_xgb, factor(d.test$target, levels=c(1,0)), positive="1")

F1_xgb <- (2 * precision * recall) / (precision + recall)
F1_xgb

roc.model_xgb <- roc(d.test$target, unfactor(pred_target_xgb))
plot(roc.model_xgb)
roc.model_xgb



