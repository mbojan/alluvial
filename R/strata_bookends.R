#' Bookends strata style for alluvial diagrams
#'
#' Returns a function accepted by the \code{strata} argument of
#' \code{\link{alluvial}} that sets the outermost axes to \code{"box"} and
#' all intermediate axes to \code{"stripes"}.
#'
#' @return A function of one argument (\code{np}, number of axes), suitable
#'   for passing to the \code{strata} argument of \code{\link{alluvial}}.
#'
#' @export
strata_bookends <- function() {
  function(np) c("box", rep("stripes", np - 2), "box")
}
