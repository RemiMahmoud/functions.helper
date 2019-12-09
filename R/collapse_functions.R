
# collapse a vector with a sum or a string depending on vector type
#' collapse_sum
#'
#' @param x a numeric or a character vector
#'
#' @return a double equals to the sum of x or a character made by collapsing  unique characters of x
#' @export
#' @examples
#' collapse_sum(c(3,2,2))
#' collapse_sum(c("a", "a", "b"))
#'
collapse_sum <- function(x)
{ifelse(is.numeric(x), sum(x), paste(unique(x), collapse="_"))}


#' collapse_unique
#'
#' @param x a numeric or a character vector
#'
#' @return a double equals to the sum of unique values of x or a character made by collapsing unique characters of x
#' @export
#' @examples
#' collapse_unique(c(3,2,2))
#' collapse_unique(c("a", "a", "b"))
#'
collapse_unique <- function(x)
  {ifelse(is.numeric(x), sum(unique(x)), paste(unique(x), collapse="_"))}
