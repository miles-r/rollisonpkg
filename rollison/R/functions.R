#Miles Rollison
#Last Edited 12/19/2019
#TODO add current = T to consecutive event; make consecutive count SINCE event

consecutive_event = function(x, current = F, since = F){
#' Count consecutive periods that an event has occured
#'
#' @param x vector of events
#' @param current if TRUE, the current observation will be included in the count
#'
#' @return returns a vector of same length as x with number of consecutive periods event has happened
#' @export
#'
#' @examples

n = 0
y = c()

if(since == T){
  for (i in x) {
    y = c(y,n)
    if(i == 0) n = n + 1
    else if(i > 0) n = 0
  }

  return(y)
}


for (i in x) {
  y = c(y, n)
  if(i == 0) n = 0
  else if (i > 0) n = n + 1

}

return(y)
}


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

make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  as_tibble(d)
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

summary_tibble = function(x){
  x %>% map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")
}

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
