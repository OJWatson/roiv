#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Bind list of data frames together
#'
#' @param x List of data.frames to bind together
rbind_list_base <- function(x) {
  x2 <- do.call(
    rbind.data.frame,
    c(x, stringsAsFactors = FALSE, make.row.names = FALSE)
  )
  rownames(x2) <- seq_len(dim(x2)[1])
  x2
}
