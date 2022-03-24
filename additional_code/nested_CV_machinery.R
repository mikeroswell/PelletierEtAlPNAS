library(tidyverse)
library(doParallel)
library(caret)
library(pROC)
library(ROCR)
library(tictoc)


folder <- function(dat, resp, k = 10, times = 5){
  createMultiFolds(as_tibble(dat)[,resp][[1]], k = k, times = times)
}

# function to tune with repeated CV

fit_rf <- function(data, formu, sampling = NULL, tuneMethod = "none", repeats = NULL, max_mtry = 150){ # set up to take formula as a string

  # Define tuning parameter grid
  # mtry = Number of variables randomly sampled as candidate variables per tree
  fitGrid <- expand.grid(mtry=seq(2, max_mtry, 1))

  # Define tuning and training method


  fC = caret::trainControl(method = tuneMethod
                           , number = ifelse(tuneMethod =="repeatedcv", 10, NA)
                           , repeats = ifelse(is.null(repeats), ifelse(tuneMethod != "none", 10, NA), repeats)
                           , sampling =  sampling
                           , classProbs = TRUE
                           , summaryFunction = twoClassSummary)


  # Train RF model
  rf_fit <- caret::train(formu
                         , data = data
                         , method = "rf" # this implements randomForest::randomForest but with controls
                         , distribution = "bernoulli"
                         , metric = "ROC" # for more robust results with class imbalance
                         , tuneGrid =if(tuneMethod !="none"){fitGrid} else{NULL} # try trees with different numbers of variables
                         , trControl = fC # cross validation and samplling
                         , na.action = na.pass # shouldn't be an issues here
                         # , verbose = T
                         , importance = T
                         # might tell me something interesting.
  )

  # Plot RF tuning results
  # rf_fit$bestTune
  # rf_tune <- rf_fit$results
  #


  # Return fit
  return(rf_fit)

}

# subset a data.frame for categorical variables
#' Indices for categorical data
#'
#' @param x `data.frame`
#'
#' @return Logical vector of `length(x)`, `TRUE` for categorical, `FALSE`
#'   otherwise
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#' y = sample(1:2, 5, replace =T)
#' , x1 = 1:5
#' , x2 = letters[1:5]
#' , x3 = LETTERS[1:5]
#'   )
#'
#' is_categorical(dat)
is_categorical <- function(x) {
  sapply(x, function(y) {is.factor(y) | is.character(y)})
}


# fix the model with blind attempt
#' Add unseen levels to fit object
#'
#' @param mod model object
#' @param test.dat `data.frame`, "new.data" on which to predict value from `mod`
#'
#' @return revised model object without missing factor levels
#' @export
#'
#' @examples
#'
#' # make data
#' set.seed(123)
#' dat <- data.frame(
#' y = sample(0:1, 5, replace =T)
#' , x1 = 1:5
#' , x2 = letters[1:5]
#' , x3 = LETTERS[1:5]
#'   )
#'
#' # split into test and train
#' test <- dat[1, ]
#' train <- dat[2:5, ]
#'
#' # fit model on test data
#' my_mod <- glm(y ~ ., data = train, family = "binomial")
#'
#' # predict throws an error
#' # predict(my_mod, test)
#'
#' # after fix, predict does not throw error
#' predict(fix.mod(my_mod, test), test)
#'
fix.mod <- function(mod, test.dat, resp){
  v.names = is_categorical(test.dat)
  mod$xlevels = Map(union, mod$xlevels,  lapply(test.dat[v.names], unique))
  return(mod)
}


# drop problematic variables
# compare factor levels from two dataframes with identical variables
nomatch<-function(x, y){
  colnames(x)[!sapply(1:length(colnames(x)), function(x.name){
    all(y[ ,x.name] %in% x[ ,x.name])
  })]
}
