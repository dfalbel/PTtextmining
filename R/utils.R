#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Tranforma os números mairoes que zero em 1
#'
maior_que_zero_em <- function(x, v = 1){
  ifelse(x > 0, v, x)
}