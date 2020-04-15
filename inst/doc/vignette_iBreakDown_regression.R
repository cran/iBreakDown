## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  warning = FALSE,
  fig.width = 6, fig.height = 6,
  message = FALSE
)

## -----------------------------------------------------------------------------
# devtools::install_github("ModelOriented/DALEX")
library(DALEX)
library(iBreakDown)

head(dragons)
new_observation <- dragons_test[1,]
new_observation

## -----------------------------------------------------------------------------
m_lm <- lm(life_length ~ . , data = dragons)

## -----------------------------------------------------------------------------
bd_lm <- local_attributions(m_lm,
                            data = dragons_test,
                            new_observation =  new_observation,
                            keep_distributions = TRUE)

## -----------------------------------------------------------------------------
bd_lm

## -----------------------------------------------------------------------------
plot(bd_lm)

## -----------------------------------------------------------------------------
plot(bd_lm, baseline = 0)

## -----------------------------------------------------------------------------
plot(bd_lm, plot_distributions = TRUE)

## -----------------------------------------------------------------------------
library(randomForest)

m_rf <- randomForest(life_length ~ . , data = dragons)

bd_rf <- local_attributions(m_rf,
                            data = dragons_test,
                            new_observation =  new_observation)

head(bd_rf)
plot(bd_rf)

## -----------------------------------------------------------------------------
library(e1071)

m_svm <- svm(life_length ~ . , data = dragons)

bd_svm <- local_attributions(m_svm,
                            data = dragons_test,
                            new_observation =  new_observation)

plot(bd_svm)

## -----------------------------------------------------------------------------
library("xgboost")

model_matrix <- model.matrix(life_length ~ . -1, dragons)
data <- xgb.DMatrix(model_matrix, label = dragons$life_length)

params <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, eval_metric = "rmse")

m_xgboost <- xgb.train(params, data, nrounds = 50)

test_matrix <- model.matrix(life_length ~ . -1, dragons_test)
observation_matrix <- test_matrix[1,,drop=FALSE]

bd_xgboost <- local_attributions(m_xgboost,
                                 data = test_matrix,
                                 new_observation =  observation_matrix)

plot(bd_xgboost)
plotD3(bd_xgboost)

## -----------------------------------------------------------------------------
library(nnet)

x <- max(abs(dragons$life_length))
digits <- floor(log10(x))
normalizing_factor <- round(x, -digits)
m_nnet <- nnet(life_length/normalizing_factor ~ . , data = dragons, size = 10, linout = TRUE)

p_fun <- function(model, new_observation){
  predict(model, newdata = new_observation)*normalizing_factor
}

bd_nnet <- local_attributions(m_nnet,
                            data = dragons_test,
                            new_observation =  new_observation,
                            predict_function = p_fun)

plot(bd_nnet)

