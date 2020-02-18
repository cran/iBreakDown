## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  warning = FALSE,
  fig.width = 6, fig.height = 6,
  message = FALSE
)

## -----------------------------------------------------------------------------
# devtools::install_github("pbiecek/DALEX")
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
library(caret)

m_knn <- knnreg(life_length ~ . , data = dragons)

bd_knn <- local_attributions(m_knn,
                            data = dragons_test,
                            new_observation =  new_observation)

plot(bd_knn)

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

