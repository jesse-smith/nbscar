get_user <- function(user = NULL, prompt = 'NBS Username') {
  assert_string(prompt, allow_empty = TRUE)

  # Get username
  if (rlang::is_empty(user) && verify_rstudio()) {
    user <- rstudioapi::askForPassword(prompt)
  } else if (is.null(user)) {
    user <- readline(paste0(prompt, ': '))
  }

  secret(
    c(Username = assert_string(user, allow_empty = FALSE)),
    secrets = FALSE
  )
}


get_pass <- function(prompt = 'NBS Password') {
  # Check args
  assert_string(prompt, allow_empty = TRUE)

  # Get password
  pass <- getPass::getPass(prompt, forcemask = TRUE)

  # Check input and return
  secret(vec_set_names(assert_string(pass, allow_empty = FALSE), 'Password'))
}

set_creds <- function(service, user = NULL) {
  user <- get_user()
  pass <- get_pass()

  keyring::key_set_with_value(
    'nbscar',
    username = scrt_x(user),
    password = scrt_x(pass)
  )

  vec_c(user, pass)
}
