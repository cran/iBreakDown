## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 6, fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
# devtools::install_github("ModelOriented/DALEX")
library("DALEX")
library("iBreakDown")

head(HR)
new_observation <- HR_test[1,]
new_observation

## -----------------------------------------------------------------------------
library("nnet")
m_glm <- multinom(status ~ . , data = HR, probabilities = TRUE, model = TRUE)

## -----------------------------------------------------------------------------
p_fun <- function(object, newdata) {
   if (nrow(newdata) == 1) {
      as.matrix(t(predict(object, newdata, type = "prob")))
   } else {
     as.matrix(predict(object, newdata=newdata, type = "prob"))
   }
 }

## -----------------------------------------------------------------------------
bd_glm <- local_attributions(m_glm,
                            data = HR_test,
                            new_observation =  new_observation,
                            keep_distributions = TRUE,
                            predict_function = p_fun)

## -----------------------------------------------------------------------------
bd_glm

## -----------------------------------------------------------------------------
plot(bd_glm)

## -----------------------------------------------------------------------------
plot(bd_glm, baseline = 0)

## -----------------------------------------------------------------------------
plot(bd_glm, plot_distributions = TRUE)

## -----------------------------------------------------------------------------
library(randomForest)

m_rf <- randomForest(status ~ . , data = HR)

p_fun <- function(object, newdata){predict(object, newdata = newdata, type = "prob")}

bd_rf <- local_attributions(m_rf,
                            data = HR_test,
                            new_observation =  new_observation,
                            predict_function = p_fun)

bd_rf
plot(bd_rf)
plotD3(bd_rf)

## -----------------------------------------------------------------------------
library(e1071)

m_svm <- svm(status ~ . , data = HR, type = "C-classification", probability = TRUE)

p_fun <- function(object, newdata){
  p <- predict(object, newdata = newdata, probability = TRUE)
  attr(p, "probabilities")
  }

bd_svm <- local_attributions(m_svm,
                            data = HR_test,
                            new_observation =  new_observation,
                            predict_function = p_fun)

plot(bd_svm)
plotD3(bd_svm)

## -----------------------------------------------------------------------------
library("xgboost")

model_matrix <- model.matrix(status == "fired" ~ . -1, HR)
data <- xgb.DMatrix(model_matrix, label = HR$status == "fired")

params <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2,
                objective = "binary:logistic", eval_metric = "auc")

m_xgboost <- xgb.train(params, data, nrounds = 50)

test_matrix <- model.matrix(status == "fired" ~ . -1, HR_test)
observation_matrix <- test_matrix[1,,drop=FALSE]

bd_xgboost <- local_attributions(m_xgboost,
                                 data = test_matrix,
                                 new_observation =  observation_matrix)

plot(bd_xgboost)
plotD3(bd_xgboost)

