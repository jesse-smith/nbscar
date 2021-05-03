#' NBS Credentials
#'
#' @description
#' `nbs_creds()` gets the username/password for NBS, if they exist. By default,
#' it asks the user to set the credentials if they do not exist.
#'
#' `nbs_set_creds()` sets or resets the NBS username and password.
#'
#' @param set_empty Boolean. Should `nbs_creds()` ask the user for their
#'   credentials if none are found?
#'
#' @param user String. NBS username; if empty, `set_nbs_creds()` will prompt the
#'   user for their username.
#'
#' @return A `secret` containing the NBS username and password
#'
#' @export
nbs_creds <- function(set_empty = TRUE) {
  assert_bool(set_empty)

  user <- NULL
  pass <- NULL
  key <- keyring::key_list('nbscar')

  if (vec_is_empty(key) && set_empty) {
    nbs_set_creds()
  } else if (rlang::is_empty(key)) {
    rlang::abort(
      paste(
        'No NBS credentials found.',
        'Please set your credentials using `nbs_set_creds()`.'
      )
    )
  } else {
    service <- key[['service']]
    user <- key[['username']]
    pass <- keyring::key_get(service, username = user)
    secret(c(Username = user, Password = pass), secrets = c(FALSE, TRUE))
  }
}

#' @rdname nbs_creds
#'
#' @export
nbs_set_creds <- function(user = NULL) {
  set_creds(service = 'nbscar', user = user)
}
