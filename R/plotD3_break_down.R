#' @title Plot Break Down Objects in D3 with r2d3 package.
#'
#' @description
#' Plots waterfall break down for objects of the \code{break_down} class.
#'
#' @param x an explanation created with \code{\link{break_down}}
#' @param ... other parameters.
#' @param baseline if numeric then veritical line will start in \code{baseline}.
#' @param max_features maximal number of features to be included in the plot. By default it's \code{10}.
#' @param digits number of decimal places (\code{\link{round}}) or significant digits (\code{\link{signif}}) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function a function to be used for rounding numbers.
#' This should be \code{\link{signif}} which keeps a specified number of significant digits or \code{\link{round}} (which is default) to have the same precision for all components.
#' @param bar_width width of bars in px. By default it's 12px
#' @param margin extend x axis domain range to adjust the plot. Usually value between 0.1 and 0.3, by default it's \code{0.2}
#' @param scale_height if \code{TRUE}, the height of the plot scales with window size
#' @param min_max a range of OX axis. By deafult \code{NA} therefore will be extracted from the contributions of \code{x}.
#' But can be set to some constants, usefull if these plots are used for comparisons.
#' @param vcolors If \code{NA} (default), DrWhy colors are used.
#' @param chart_title a character. Set custom title
#' @param time in ms. Set the animation length
#' @param max_vars alias for the \code{max_features} parameter.
#'
#' @return a \code{r2d3} object.
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#' set.seed(1313)
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                        data = titanic_imputed, family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic_imputed,
#'                            y = titanic_imputed$survived,
#'                            label = "glm")
#'
#' bd_glm <- local_attributions(explain_titanic_glm, titanic_imputed[1, ])
#' bd_glm
#' plotD3(bd_glm)
#'
#' \dontrun{
#' ## Not run:
#' library("randomForest")
#'
#' m_rf <- randomForest(status ~ . , data = HR[2:2000,])
#' new_observation <- HR_test[1,]
#' new_observation
#'
#' p_fun <- function(object, newdata){predict(object, newdata=newdata, type = "prob")}
#'
#' bd_rf <- local_attributions(m_rf,
#'                            data = HR_test,
#'                            new_observation =  new_observation,
#'                            predict_function = p_fun)
#'
#' bd_rf
#' plotD3(bd_rf)
#' }
#'
#' @export
#' @rdname plotD3_break_down
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3_break_down
plotD3.break_down <- function(x, ...,
                        baseline = NA,
                        max_features = 10,
                        digits = 3, rounding_function = round,
                        bar_width = 12,
                        margin = 0.2,
                        scale_height = FALSE,
                        min_max = NA,
                        vcolors = NA,
                        chart_title = NA,
                        time = 0,
                        max_vars = NULL) {
  # fix for https://github.com/ModelOriented/iBreakDown/issues/77
  colnames(x) <- gsub(colnames(x), pattern = "cummulative", replacement = "cumulative")

  # aliases
  if (!is.null(max_vars)) {
    max_features <- max_vars
  }


  n <- length(list(...)) + 1
  m <- c()

  bdl <- list(x, ...)
  deleted_indexes <- c()

  dl <- list()
  model_names <- c()

  for (i in 1:n) {
    x <- bdl[[i]]

    if (!("break_down" %in% class(x))) stop("The function requires an object created with local_attributions().")

    # because apparently one explainer can make multiple plots
    if (length(levels(x[,'label'])) > 1) {
      # update plot count
      n <- n + length(levels(x[,'label'])) - 1

      # add new data frames to list
      bdl <- c(bdl, split(x, f=x[,'label']))

      # remember indexes to delete
      deleted_indexes <- c(deleted_indexes, i)
    }
  }

  # delete doubled data frames
  bdl[deleted_indexes] <- NULL

  # iterate through updated data frame list
  for (i in 1:n) {
    x <- bdl[[i]]

    # remember number of features to compare
    m <- c(m, ifelse(nrow(x) - 2 <= max_features, nrow(x), max_features + 3))

    new_x <- prepare_data_for_break_down_plotD3(x, baseline, max_features, rounding_function, digits)

    dl[[i]] <- new_x

    # remember plot names
    model_names <- c(model_names,as.character(x[,'label'][1]))
  }

  if (length(unique(m)) > 1) stop("Models have different numbers of features.")

  m <- unique(m)
  names(dl) <- model_names

  df <- do.call(rbind, dl)

  # later count longest label width in d3
  label_list <- as.character(df[,'variable'])

  if (any(is.na(min_max))) {
    if (is.na(baseline)) {
      min_max <- range(df[,'cumulative'])
    } else {
      min_max <- range(df[,'cumulative'], baseline)
    }
  }

  # count margins

  min_max_margin <- abs(min_max[2]-min_max[1])*margin
  xmin <- min_max[1] - min_max_margin
  xmax <- min_max[2] + min_max_margin

  options <- list(xmin = xmin, xmax = xmax,
                  n = n, m = m, barWidth = bar_width,
                  scaleHeight = scale_height, time = time,
                  vcolors = ifelse(is.na(vcolors), "default", vcolors),
                  chartTitle = ifelse(is.na(chart_title), "Local attributions", chart_title))

  temp <- jsonlite::toJSON(list(dl, label_list))

  # plot D3 object
  r2d3::r2d3(
    data = temp,
    script = system.file("d3js/breakDownD3.js", package = "iBreakDown"),
    dependencies = list(
      system.file("d3js/colorsDrWhy.js", package = "iBreakDown"),
      system.file("d3js/d3-tip.js", package = "iBreakDown"),
      system.file("d3js/hackHead.js", package = "iBreakDown")
    ),
    css = system.file("d3js/themeDrWhy.css", package = "iBreakDown"),
    options = options,
    d3_version = "4"
  )
}

prepare_data_for_break_down_plotD3 <- function(x, baseline, max_features = 10, rounding_function, digits) {

  # fix df
  x[,'variable'] <- as.character(x[,'variable'])
  x[,'variable_name'] <- as.character(x[,'variable_name'])

  x[x[,'variable_name']=="",'variable_name'] <- "prediction"

  temp <- data.frame(x[c(1,nrow(x)),])
  x <- data.frame(x[-c(1,nrow(x)),])

  if (nrow(x) > max_features) {
    last_row <- max_features + 1
    new_x <- x[1:last_row,]
    new_x[last_row,'variable'] <- "+ all other factors"
    new_x[last_row,'contribution'] <- sum(x[last_row:nrow(x),'contribution'])
    new_x[last_row,'cumulative'] <- x[nrow(x),'cumulative']
    new_x[last_row,'sign'] <- ifelse(new_x[last_row,'contribution'] > 0,1,-1)

    x <- new_x
  }

  x <- rbind(temp[1,], x, temp[2,])

  if (is.na(baseline)) {
    baseline <- x[1,"cumulative"]
  }

  # fix contribution and sign
  x[c(1,nrow(x)),"contribution"] <- x[c(1,nrow(x)),"contribution"] - baseline

  x[c(1,nrow(x)),"sign"] <- ifelse(x[c(1,nrow(x)),"contribution"] > 0,1,ifelse(x[c(1,nrow(x)),"contribution"] < 0,-1,0))

  # use for bars
  x[,'barStart'] <- ifelse(x[,'sign'] == "1", x[,'cumulative'] - x[,'contribution'], x[,'cumulative'])
  x[,'barSupport'] <- ifelse(x[,'sign'] == "1", x[,'cumulative'], x[,'cumulative'] - x[,'contribution'])

  # use for text label and tooltip
  x[,'contribution'] <- rounding_function(x['contribution'], digits)
  x[,'cumulative'] <- rounding_function(x['cumulative'], digits)

  # use for color
  x[c(1,nrow(x)),"sign"] <- "X"

  x[,'tooltipText'] <- ifelse(x[,'sign'] == "X", paste0("Average response: ",x[1,'cumulative'],
                                                        "<br>", "Prediction: ",
                                                        x[nrow(x),'cumulative']),
                              paste0(substr(x[,'variable'], 1, 25),
                                     "<br>", ifelse(x[,'contribution'] > 0, "increases", "decreases"),
                                     " average response <br>by ", abs(x[,'contribution'])))

  x
}
