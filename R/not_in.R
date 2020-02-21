#' Not in
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
#' @return A logical vector, indicating if a match was located for each element of x: thus the values are TRUE or FALSE and never NA.
#' @export
#'
#' @examples
#' "foo" %ni% letters
'%ni%' <- function(x, table){
  match(x, table, nomatch = 0) <= 0
}
