#' Start a PhantomJS Process
#'
#' `phtm_start()` starts a PhantomJS process that can be used to instantiate
#' headless web browsing sessions.
#'
#' @param timeout How long to wait (in seconds) for the connection to the
#'   PhantomJS process to be established. Can be a decimal.
#'
#' @param debug PhantomJS debug level
#'
#' @return A list containing the PhantomJS `process` and `integer` port number
#'
#' @export
phtm_start <- function(
  timeout = 5,
  debug = c('info', 'error', 'warn', 'debug')
) {
  timeout <- as.double(timeout)
  debug <- toupper(rlang::arg_match(debug)[[1L]])

  webdriver::run_phantomjs(
    debugLevel = debug,
    timeout = as.integer(timeout * 1e3)
  )
}

#' Kill PhantomJS Processes
#'
#' `phtm_kill()` kills PhantomJS processes that are children (or descendants)
#' of a given parent process. This is useful for shutting down orphaned
#' PhantomJS processes.
#'
#' @param parent The parent process of the PhantomJS processes to kill; the
#'   default is the current R process
#'
#' @param excl PhantomJS processes to keep alive. Either a list containing a
#'   PhantomJS `process` object, a PhantomJS `process` object, a PhantomJS `ps`
#'   object, or a list containing multiple of these objects, all of the same
#'   type.
#'
#' @param recursive Boolean. Should `phtm_kill()` kill all PhantomJS
#'   descendants of `parent` or only children?
#'
#' @return An `integer` number of PhantomJS processes killed
#'
#' @export
phtm_kill <- function(
  parent = ps::ps_handle(),
  excl = NULL,
  recursive = FALSE
) {
  phantoms <- phtm_find()

  if (rlang::is_empty(phantoms)) return(0L)

  phantom_pids <- vapply(phantoms, FUN = ps::ps_pid, FUN.VALUE = integer(1L))

  if (is_phantom(excl)) {
    excl_ps <- list(excl[[1L]]$as_ps_handle())
  } else if (is_process(excl)) {
    excl_ps <- list(excl$as_ps_handle())
  } else if (is_ps_handle(excl)) {
    excl_ps <- list(excl)
  } else if (!rlang::is_empty(excl)) {
    excl_ps <- lapply(excl, FUN = phtm_get_ps)
  } else {
    excl_ps <- NULL
  }

  if (!rlang::is_empty(excl_ps)) {
    excl_pids <- vapply(excl_ps, FUN = ps::ps_pid, FUN.VALUE = integer(1L))
  } else {
    excl_pids <- integer(0L)
  }

  to_kill <- !phantom_pids %in% excl_pids

  lapply(phantoms[to_kill], ps::ps_kill)

  sum(to_kill)
}

#' Find PhantomJS Processes Running in Background
#'
#' @param parent The parent process to search
#'
#' @param recursive Boolean. Should all descendants be returned, or only
#'   children?
#'
#' @return A `list` of `ps_handle`s
#'
#' @keywords internal
phtm_find <- function(parent = ps::ps_handle(), recursive = FALSE) {
  children <- ps::ps_children(parent, recursive = recursive)

  is_phantom <- vapply(
    children,
    FUN = ps::ps_name,
    FUN.VALUE = character(1L)
  ) == 'phantomjs.exe'

  children[is_phantom]
}

#' Get Info From an Object Returned by \code{\link[webdriver:run_phantomjs]{run_phantomjs()}}
#'
#' @param phantom A length 2 list returned by
#'   \code{\link[webdriver:run_phantomjs]{run_phantomjs()}}
#'
#' @return The `process`, `host`, or `port`
#'
#' @name phtm-connection
#'
#' @keywords internal
NULL

#' @rdname phtm-connection
phtm_process <- function(x) {
  if (rlang::is_list(x) && is_process(x[['process']])) {
    x[['process']]
  } else if (is_process(x)) {
    x
  } else {
    arg <- rlang::expr_label(rlang::enexpr(x))
    rlang::abort(paste(
      arg,
      'must be a `process` or a `list` with a `process` named "process"'
    ))
  }
}

#' @rdname phtm-connection
phtm_host <- function(x) {
  p <- phtm_process(x)
  stringr::str_extract(
    p$get_cmdline()[[3L]],
    '([0-9]{1,3}[.]){3}[0-9]{1,3}'
  )
}

#' @rdname phtm-connection
phtm_port <- function(x) {
  p <- phtm_process(x)
  as.integer(stringr::str_extract(p$get_cmdline()[[3L]], '(?<=:)[0-9]+$'))
}

#' Extract the `ps_handle` of a PhantomJS list or `process`
#'
#' @param x A list with a PhantomJS `process` element named `process`, or a
#'   `process` object
#'
#' @return A `ps_handle`
#'
#' @keywords internal
phtm_ps <- function(x) phtm_process(x)$as_ps_handle()

#' Boolean Tests
#'
#' @description
#' `is_phantom()` tests whether an object is a PhantomJS list
#'
#' `is_process()` tests whether an object is a `process`
#'
#' `is_ps_handle()` tests whether an object is a `ps_handle`
#'
#' @param x An object to test
#'
#' @return `TRUE` or `FALSE`
#'
#' @name booleans
#'
#' @keywords internal
NULL

#' @rdname booleans
#'
#' @keywords internal
is_phantom <- function(x) {
  (
    rlang::is_true(rlang::is_list(x))
    && rlang::is_true(rlang::inherits_any(x, 'list'))
    && rlang::is_true(!rlang::is_empty(names(x)))
    && rlang::is_true(all.equal(names(x), c('process', 'port')))
    && is_process(x[['process']])
    && rlang::is_true(rlang::is_integer(x[['port']]))
    && rlang::is_true(x[['process']]$get_name() == 'phantomjs.exe')
  )
}

#' @rdname booleans
#'
#' @keywords internal
is_process <- function(x) {
  (
    rlang::is_true(rlang::is_environment(x))
    && rlang::is_true(rlang::inherits_all(x, c('process', 'R6')))
  )
}

#' @rdname booleans
#'
#' @keywords internal
is_ps_handle <- function(x) {
  (
    rlang::is_true(typeof(x) == 'externalptr')
    && rlang::is_true(rlang::inherits_all(x, 'ps_handle'))
  )
}
