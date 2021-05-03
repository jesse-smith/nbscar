#' Log In and Out of NBS
#'
#' `nbs_login()` logs a user into NBS in a PhantomJS browsing session
#'
#' `nbs_logout()` logs a user out of NBS in a PhantomJS browsing session
#'
#' @inheritParams nbs-session
#'
#' @return A webdriver \code{\link[webdriver:Session]{Session}}
#'
#' @name nbs-loginout
#'
#' @aliases nbs_login nbs_logout
#'
#' @keywords internal
NULL

#' @rdname nbs-loginout
nbs_login <- function(
  session,
  creds = nbs_creds(),
  show_screen = rlang::is_interactive()
) {

  # Go to NBS Login
  session$go('https://hssi.tn.gov/auth/Login')

  # Enter username
  session$
    findElement(xpath = '//input[@name="usr_name"]')$
    clear()$
    sendKeys(scrt_x(creds[['Username']]))
  Sys.sleep(0.1)

  # Enter password
  session$
    findElement(xpath = '//input[@name="usr_password"]')$
    clear()$
    sendKeys(scrt_x(creds[['Password']]))
  Sys.sleep(0.1)

  # Login
  session$
    findElement(xpath = '//button[@type="submit"]')$
    click()
  Sys.sleep(0.5)

  # Click `OK` on login info page
  try(
    {session$
      findElement(xpath = '//button')$
      click()
    Sys.sleep(0.5)},
    silent = TRUE
  )

  # Click 'NBS Production' link on landing page
  session$
    findElement(xpath = "//a[contains(text(),'NBS Production')]")$
    click()
  Sys.sleep(0.5)

  if (show_screen) session$takeScreenshot()

  invisible(session)
}

#' @rdname nbs-loginout
nbs_logout <- function(session, show_screen = rlang::is_interactive()) {

  for (i in 1:5) {
    logout <- try(
      session$
        findElement(xpath = '//a[contains(text(),"Logout")]')$
        click(),
      silent = TRUE
    )

    if (rlang::inherits_any(logout, "try-error")) {
      rlang::abort("Logout button not found")
    }

    session_is_on_login_page <- stringr::str_detect(
      session$getUrl(),
      "hssi[.]tn[.]gov[/]auth[/]Login"
    )

    if (session_is_on_login_page) break

    Sys.sleep(0.5)
  }

  if (show_screen) session$takeScreenshot()

  invisible(session)
}
