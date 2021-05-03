# `secret` Exports -------------------------------------------------------------

#' Create a `secret`
#'
#' `secret`s are character vectors that print with the specified `secrets`
#' hidden by asterisks (`***`).
#'
#' `secret()` and `as_secret()` creates a `secret`, casting to `character` first
#'   if needed and possible
#'
#' `is_secret()` tests whether an object is a well-formed `secret`
#'
#' `scrt_lock()` and `scrt_unlock()` lock and unlock the `secrets`
#'
#' `scrt_set_secrets()` sets secret values in a `secret`
#'
#' `scrt_locked()`  tests whether a `secret` is locked or unlocked
#'
#' @inherit new_secret params return
#'
#' @keywords internal
#'
#' @export
secret <- function(x = character(), secrets = NULL, locked = TRUE) {
  if (is_secret(x)) {
    assert_bool(locked)
    if (vec_is_empty(secrets)) {
      secrets <- scrt_secrets(x)
    } else {
      secrets <- expand_secrets(secrets, x)
    }
    x <- scrt_set_secrets(x, secrets)
    if (locked) scrt_lock(x) else scrt_unlock(x)
  } else {
    validate_secret(
      new_secret(vec_cast(x, character()), secrets = secrets, locked = locked)
    )
  }
}

#' @rdname secret
#'
#' @export
as_secret <- function(x, secrets = NULL) {
  secret(x, secrets = secrets)
}

#' @rdname secret
#'
#' @export
is_secret <- function(x) {
  rlang::is_false(
    rlang::inherits_any(try(validate_secret(x), silent = TRUE), 'try-error')
  )
}

#' @rdname secret
#'
#' @export
scrt_lock <- function(x) validate_secret(structure(x, locked = TRUE))

#' @rdname secret
#'
#' @export
scrt_unlock <- function(x) validate_secret(structure(x, locked = FALSE))

#' @rdname secret
#'
#' @export
scrt_set_secrets <- function(x, secrets) {
  validate_secret(
    new_secret(
      vec_set_names(scrt_x(x), scrt_names(x)),
      secrets = expand_secrets(secrets, x),
      locked = scrt_locked(x)
    )
  )
}

#' @rdname secret
#'
#' @export
scrt_locked <- function(x) attr(x, 'locked', exact = TRUE)

# `secret` Internal Helpers ----------------------------------------------------

#' Internal `secret` Helpers
#'
#' @param x A `character` vector
#'
#' @param secrets `logical` of the same size as `x`. Elements to hide when
#'   printing (defaults to all)
#'
#' @param class string. An optional subclass
#'
#' @param ... Additional attributes to give to the subclass
#'
#' @return A `secret`
#'
#' @name secret-helpers-internal
#'
#' @keywords internal
NULL

#' @rdname secret-helpers-internal
scrt_x <- function(x) field(x, 'x')

#' @rdname secret-helpers-internal
scrt_secrets <- function(x) field(x, 'secrets')

#' @rdname secret-helpers-internal
scrt_names <- function(x) {
  nms <- field(x, 'names')
  if (all(is.na(nms))) NULL else nms
}

#' @rdname secret-helpers-internal
#'
#' @export
scrt_set_names <- function(x, names) {
  validate_secret(
    new_secret(
      vec_set_names(scrt_x(x), names),
      secrets = scrt_secrets(x),
      locked  = scrt_locked(x)
    )
  )
}

#' @rdname secret-helpers-internal
new_secret <- function(x, secrets = NULL, locked = TRUE, class = NULL, ...) {
  nms <- vec_names(x)
  if (vec_is_empty(nms)) nms <- vec_rep(NA_character_, vec_size(x))

  new_rcrd(
    data.frame(
      x = vec_assert(x, character()),
      secrets = expand_secrets(secrets, x),
      names = nms
    ),
    locked = assert_bool(locked),
    ...,
    class = append(std_class(class), 'nbscar_secret')
  )
}

#' Validate a Well-Formed `secret` Object
#'
#' @param x Object to validate
#'
#' @return `x` (or throw an error if invalid)
#'
#' @keywords internal
validate_secret <- function(x) {
  # Get arg name
  arg <- rlang::expr_label(rlang::enexpr(x))

  # Check class
  if (!rlang::inherits_all(x, c('nbscar_secret', 'vctrs_rcrd'))) {
    rlang::abort(paste(arg, 'must inherit from `nbscar_secret`'))
  }

  # Check type of `x`
  vec_assert(scrt_x(x), character())

  # Check type of `secrets`
  vec_assert(scrt_secrets(x), logical(), size = vec_size(scrt_x(x)))

  # Check type of `names`
  nms <- vec_assert(field(x, 'names'), character(), size = vec_size(scrt_x(x)))

  # Check type of `locked`
  assert_bool(scrt_locked(x))

  # Check `secrets` values
  if (any(is.na(scrt_secrets(x)))) {
    rlang::abort(paste(
      'The `secrets` field must be a `logical` with all boolean values',
      'and length equal to `x`'
    ))
  }

  x
}

#' @rdname secret-helpers-internal
expand_secrets <- function(secrets, x) {
  if (vec_is_empty(secrets)) return(vec_rep(TRUE, vec_size(x)))

  s <- vec_assert(
    vec_assign(
      vec_rep(FALSE, vec_size(x)),
      vec_as_location(secrets, n = vec_size(x), names = vec_names(x)),
      value = TRUE
    ),
    logical(),
    size = vec_size(x),
    arg = 'expanded secrets'
  )

  s
}

# `secret` Methods -------------------------------------------------------------

#' S3 Methods for `secret` Objects
#'
#' @keywords internal
#'
#' @name secret-methods
NULL

#' @rdname secret-methods
#'
#' @export
format.nbscar_secret <- function(x, ...) {
  values <- scrt_x(x)
  scrts <- scrt_secrets(x)
  nms <- scrt_names(x)
  vec_set_names(
    if (scrt_locked(x)) vec_assign(values, scrts, value = '***') else values,
    if (all(is.na(nms))) NULL else nms
  )
}

#' @rdname secret-methods
#'
#' @export
obj_str_footer.nbscar_secret <- function(x, ...) cat(' locked:', scrt_locked(x))

#' @rdname secret-methods
#'
#' @export
names.nbscar_secret <- function(x) scrt_names(x)

#' @rdname secret-methods
#'
#' @export
`names<-.nbscar_secret` <- function(x, value) scrt_set_names(x, value)

#' @rdname secret-methods
#'
#' @export
vec_ptype_abbr.nbscar_secret <- function(x, ...) 'scrt'

#' @rdname secret-methods
#'
#' @export
vec_ptype_full.nbscar_secret <- function(x, ...) 'secret'

#' @rdname secret-methods
#'
#' @export
vec_ptype2.nbscar_secret.nbscar_secret <- function(x, y, ...) secret()

#' @rdname secret-methods
#'
#' @export
vec_ptype2.nbscar_secret.character <- function(x, y, ...) secret()

#' @rdname secret-methods
#'
#' @export
vec_ptype2.character.nbscar_secret <- function(x, y, ...) secret()

#' @rdname secret-methods
#'
#' @export
vec_cast.nbscar_secret.nbscar_secret <- function(x, to, ...) x

#' @rdname secret-methods
#'
#' @export
vec_cast.nbscar_secret.character <- function(x, to, ...) secret(x)

#' @rdname secret-methods
#'
#' @export
vec_cast.character.nbscar_secret <- function(x, to, ...) {
  arg <- rlang::expr_label(rlang::enexpr(x))
  if (scrt_locked(x)) {
    rlang::warn(paste(
      'The `secrets` in', arg, 'have been replaced with asterisks (`"***"`)'
    ))
  }
  format(x)
}

#' @rdname secret-methods
#'
#' @export
`$.nbscar_secret` <- function(x, i) x[i]

#' @rdname secret-methods
#'
#' @export
`[[.nbscar_secret` <- function(x, i) x[i]
