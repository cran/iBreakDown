## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 6, fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ------------------------------------------------------------------------
# devtools::install_github("ModelOriented/DALEX")
library("DALEX")
library("iBreakDown")

head(HR)
new_observation <- HR_test[1,]
new_observation

## ------------------------------------------------------------------------
library("nnet")
m_glm <- multinom(status ~ . , data = HR, probabilities = TRUE, model = TRUE)

## ------------------------------------------------------------------------
p_fun <- function(object, newdata) {
   if (nrow(newdata) == 1) {
      as.matrix(t(predict(object, newdata, type = "prob")))
   } else {
     as.matrix(predict(object, newdata=newdata, type = "prob"))
   }
 }

## ------------------------------------------------------------------------
bd_glm <- local_attributions(m_glm,
                            data = HR_test,
                            new_observation =  new_observation,
                            keep_distributions = TRUE,
                            predict_function = p_fun)

## ------------------------------------------------------------------------
bd_glm

## ------------------------------------------------------------------------
plot(bd_glm)

## ------------------------------------------------------------------------
plot(bd_glm, baseline = 0)

## ------------------------------------------------------------------------
plot(bd_glm, plot_distributions = TRUE)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
library("caret")
m_knn <- knn3(status ~ . , data = HR, k = 5)
bd_knn <- local_attributions(m_knn,
                            data = HR_test,
                            new_observation =  new_observation)

plot(bd_knn)
plotD3(bd_knn)

