#' Which customized
#'
#' @param x a logical vector or array. NAs are allowed and omitted (treated as if FALSE).
#' @param arr.ind logical; should array indices be returned when x is an array?
#' @param useNames logical indicating if the value of arrayInd() should have (non-null) dimnames at all.
#'
#' @description Same function as base::which, except that it returns NA instead of integer(0) when there is not TRUE in x
#' @return If TRUE present in x: same thing as base::which, else returns NA
#' @export
#'
#' @examples
#' which(letters == "a character that doesn't exist")
#' which.customized(letters == "a character that doesn't exist")
which.customized <- function(x, arr.ind = FALSE, useNames = TRUE){
  result = which(x, arr.ind = FALSE, useNames = TRUE)
  ifelse(identical(result, integer(0)), return(NA), return(result))
}
