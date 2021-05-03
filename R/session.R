#' Create and Delete a Browsing Session
#'
#' @description
#' `sesh_start()` instantiates a browsing session using a headless PhantomJS
#' web driver
#'
#' `sesh_end()` deletes a browsing session and finalizes the `Session` object
#'
#' @param phantom A list containing the PhantomJS `process` and `integer` port
#'   number
#'
#' @return A webdriver \code{\link[webdriver:Session]{Session}}
#'
#' @name phtm-session
#'
#' @keywords internal
NULL

#' @rdname phtm-session
sesh_start <- function(phantom) {
  webdriver::Session$new(host = phtm_host(phantom), port = phtm_port(phantom))
}

#' @rdname phtm-session
sesh_end <- function(session) session$delete()
