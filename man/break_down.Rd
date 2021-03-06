% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/break_down.R
\name{break_down}
\alias{break_down}
\alias{break_down.explainer}
\alias{break_down.default}
\title{Model Agnostic Sequential Variable Attributions}
\usage{
break_down(x, ..., interactions = FALSE)

\method{break_down}{explainer}(x, new_observation, ..., interactions = FALSE)

\method{break_down}{default}(
  x,
  data,
  predict_function = predict,
  new_observation,
  keep_distributions = FALSE,
  order = NULL,
  label = class(x)[1],
  ...,
  interactions = FALSE
)
}
\arguments{
\item{x}{an explainer created with function \code{\link[DALEX]{explain}} or a model.}

\item{...}{parameters passed to \code{local_*} functions.}

\item{interactions}{shall interactions be included?}

\item{new_observation}{a new observation with columns that correspond to variables used in the model.}

\item{data}{validation dataset, will be extracted from \code{x} if it is an explainer.}

\item{predict_function}{predict function, will be extracted from \code{x} if it's an explainer.}

\item{keep_distributions}{if \code{TRUE}, then distribution of partial predictions is stored and can be plotted with the generic \code{plot()}.}

\item{order}{if not \code{NULL}, then it will be a fixed order of variables. It can be a numeric vector or vector with names of variables.}

\item{label}{name of the model. By default it is extracted from the 'class' attribute of the model.}
}
\value{
an object of the \code{break_down} class.
}
\description{
This function finds Variable Attributions via Sequential Variable Conditioning.
It calls either \code{\link{local_attributions}} for additive attributions
or \code{\link{local_interactions}} for attributions with interactions.
}
\examples{
library("DALEX")
library("iBreakDown")
set.seed(1313)
model_titanic_glm <- glm(survived ~ gender + age + fare,
                       data = titanic_imputed, family = "binomial")
explain_titanic_glm <- explain(model_titanic_glm,
                           data = titanic_imputed,
                           y = titanic_imputed$survived,
                           label = "glm")

bd_glm <- break_down(explain_titanic_glm, titanic_imputed[1, ])
bd_glm
plot(bd_glm, max_features = 3)

\dontrun{
## Not run:
library("randomForest")
set.seed(1313)
# example with interaction
# classification for HR data
model <- randomForest(status ~ . , data = HR)
new_observation <- HR_test[1,]

explainer_rf <- explain(model,
                        data = HR[1:1000,1:5])

bd_rf <- break_down(explainer_rf,
                           new_observation)
head(bd_rf)
plot(bd_rf)
}
}
\references{
Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
}
\seealso{
\code{\link{local_attributions}}, \code{\link{local_interactions}}
}
