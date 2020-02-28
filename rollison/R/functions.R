#Miles Rollison Last Edited 12/19/2019 TODO add current = T to consecutive

#event; make consecutive count SINCE event

#consecutive_event = function(x, current = F, since = F){
#' Count consecutive periods that an event has occured
#'
#' @param x vector of events
#' @param current if TRUE, the current observation will be included in the count
#'
#' @return returns a vector of same length as x with number of consecutive periods event has happened
#' @export
#'
#' @examples

# n = 0
# y = c()
#
# if(since == T){
#   for (i in x) {
#     y = c(y,n)
#     if(i == 0) n = n + 1
#     else if(i > 0) n = 0
#   }
#
#   return(y)
# }
#
# for (i in x) {
#   y = c(y, n)
#   if(i == 0) n = 0
#   else if (i > 0) n = n + 1
#
# }
#
# return(y)
# }

lm_tbl = function(model){
#' Present lm() summary output as tibble
#'
#' This function formats lm() regression output as a tibble
#'
#' @param model A model resulting from fitting a linear model with lm()
#'
#' @return A tibble with the regression output coefficients, Std. Error, t-value, and p-value.
#'
#' @export
#'
#' @examples
  s = summary(model)
  tbl = bind_cols(feature = row.names(s$coefficients), as_tibble(s$coefficients))
  colnames(tbl)[5] = "pvalue"
  return(tbl)
}

install_essentials = function(exclude = ''){
#' Install essential packages for new R install
#'
#'  @param exclude a list of packages to exclude from installation

  pkgs = c('e1071',
           'cli',
           'gh',
           'devtools',
           'tidyverse',
           'magrittr',
           'caret',
           'ranger',
	         'xgboost',
           'tm',
           'tokenizers'
           'tidytext',
           'quantreg')
  pkgs = setdiff(pkgs, exclude)

  install.packages(pkgs)
}

make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  as_tibble(d)
}

make_lags = function(df, v, l= 1){
#' Generate lagged variables
#'
#' Makes \code{l} sets of variables v, lagged 1 - \code{l} times
#'
#' @param v variables to lag \code{l} times.
#' A list of columns can be passed in using vars()
#'
#' @param l number of lag iterations
#'
#' @return Returns a copy of \code{df} with variables
#' passed in through \code{v} lagged \code{l} iterations
#'
#' @example
#'
#' make_lags(tbl, price, l = 2)
#'  # returns tbl with price_lag1 and price_lag2 appended
  require(dplyr)
  require(magrittr)
  for(i in seq(1:l)){
    df %<>%
      mutate_at(v, function(x){lag(x, i)}) %>%
      rename_at(v, function(x){ paste0(x, "_lag", i) }) %>%
      full_join(df)
  }
  return(df)
}

make_names = function(names, unique = F, allow_ = T){
#' Make Syntactically Valid Names
#'
#' Just like \code{make.names} but with _ instead of .
#'
#' @param names character vector to be coerced to syntactically valid names.
#'
#' @return A character vector of same length as names with each changed to a syntactically valid name,
#' in the current locale's encoding.
#' @export
#'
#' @examples
  chartr(".", "_", make.names(names, unique, allow_) )
}

Mode = function(x){
#' Function for the mode of a sample
#'
#' @param x vector of numbers
#'
#' @return returns the mode(s) of the vector

  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

na_magic <- function (x, replace = 0) {
#' Make NA magically turn into another value
#'
#' @param x vector with NAs
#' @param replace value for transmogrifying NAs into
#'
#' @return returns a vector of same length as x with transmogrified values
#' @export
#'
#' @examples
  x[is.na(x)] <- replace
  return(x)
}

prop_compare = function(df, y, train = training, test = testing){
#' Prints proportion tables for a dataset and
#'training and testing subsets
#'
#' @param df full dataframe
#' @param y  response variable to make proportion table for
#' @param train training dataset
#' @param test testing dataset
#'
#' @return prints three proportion tables
#'
  y = deparse(substitute(y))
  print("Full Dataset")
  print(prop.table(table(df[[y]])))
  print("Training Dataset")
  print(prop.table(table(train[[y]])))
  print("Testing Dataset")
  print(prop.table(table(test[[y]])))
}

rescale = function(x, a = 0, b = 1){
#' Function to rescale a vector of numbers
#'
#' @param x vector of values to be normalized
#' @param a minimum value for rescaling
#' @param b maximum value for rescaling
#'
#' @return returns a vector of values rescaled between a and b

  y = a + ( ( (x - min(x, na.rm = T))*(b - a)  ) /
          ( max(x, na.rm = T) - min(x, na.rm = T) ) )
}

#summary_tibble = function(x){
#  require(magrittr)
#  x %>% map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")
#}


trim.leading <- function (x){
#' Function to trim leading whitespace from a string
#'
#' @param x string to trim whitespace from

#' @return returns a string with leading whitespace trimmed
	gsub("^\\s+", "", x)
}


trim.trailing <- function (x){
#' Function to trim trailing whitespace from a string
#'
#' @param x string to trim whitespace from

#' @return returns a string with trailing whitespace trimmed
  gsub("\\s+$", "", x)
}


trim <- function (x){
#' Function to trim both leading and trailing whitespace from a string
#'
#' @param x string to trim whitespace from

#' @return returns a string with both leading and trailing whitespace trimmed

	gsub("^\\s+|\\s+$", "", x)
}


