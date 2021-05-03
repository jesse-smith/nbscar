#' Start or End an NBS Browsing Session
#'
#' `nbs_start()` starts a browsing session and logs into NBS
#'
#' `nbs_end()` logs out of NBS and ends the browsing session
#'
#' @param phantom A list containing a PhantomJS `process` and `integer` port
#'   number
#'
#' @param session A webdriver \code{\link[webdriver:Session]{Session}}
#'
#' @param creds A `secret` containing NBS login credentials
#'
#' @param show_screen `logical`. Should the browser screen be displayed on
#'   return?
#'
#' @return A webdriver \code{\link[webdriver:Session]{Session}}
#'
#' @name nbs-session
NULL

#' @rdname nbs-session
#'
#' @export
nbs_start <- function(
  phantom,
  creds = nbs_creds(),
  show_screen = rlang::is_interactive()
) {
  nbs_login(
    sesh_start(phantom),
    creds = creds,
    show_screen = show_screen
  )
}

#' @rdname nbs-session
#'
#' @export
nbs_end <- function(session, show_screen = rlang::is_interactive()) {
  sesh_end(nbs_logout(session, show_screen = show_screen))
}
