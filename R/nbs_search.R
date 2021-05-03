nbs_export_new <- function(
  session,
  type = c('inv', 'lab'),
  event_status = c('new', 'update')
) {
  nbs_search_inv(session)
  nbs_export_queue(session)
}

#' Go to Advanced Search Tab
#'
#' @param session A \code{\link[webdriver:Session]{Session}} object
#'
#' @param type The search entity (patient, investigation, or lab)
#'
#' @param show_screen Should the browser screen be displayed on return?
#'
#' @return The `session` (invisibly)
#'
#' @keywords internal
nbs_go_to_search <- function(
  session,
  type = c('inv', 'pat', 'lab'),
  show_screen = rlang::is_interactive()
) {

  type <- rlang::arg_match(type)[[1L]]

  # Go to homepage
  session$
    findElement(xpath = '//a[contains(text(), "Home")]')$
    click()

  # Go to advanced search
  session$
    findElement(xpath = '//a[contains(text(), "Advanced Search")]')$
    click()

  # Select specified tab
  tab_id <- if (type == 'pat') '"tabs0head0"' else '"tabs0head1"'
  session$
    findElement(xpath = paste0('//td[@id = ', tab_id, ']'))$
    click()

  # Select event type, if needed
  if (type %in% c('inv', 'lab')) {
    etype_keys <- if (type == 'inv') 'Inv' else 'Lab'
    session$
      findElement(xpath = '//input[@name = "ETYPE_textbox"]')$
      clear()$
      sendKeys(etype_keys, webdriver::key$tab)
  }

  if (show_screen) session$takeScreenshot()

  invisible(session)
}

nbs_search_inv <- function(
  session,
  condition = 'COVID-19',
  jurisdiction = 'Memphis',
  date_type = 'Date of Report',
  date = Sys.Date(),
  event_status = 'New',
  inv_status = 'Open',
  case_status = c('C', 'P'),
  export = TRUE,
  show_screen = rlang::is_interactive()
) {

  # Go to search page
  nbs_go_to_search(session, type = 'inv', show_screen = FALSE)

  # Enter condition
  session$
    findElement(xpath = '//input[@name = "COND_textbox"]')$
    clear()$
    sendKeys(condition, webdriver::key$tab)

  # Enter jurisdiction
  session$
    findElement(xpath = '//input[@name = "JURISD_textbox"]')$
    clear()$
    sendKeys(jurisdiction, webdriver::key$tab)

  # Enter event date type
  session$
    findElement(xpath = '//input[@name = "ESR200_textbox"]')$
    clear()$
    sendKeys(date_type, webdriver::key$tab)

  # Enter event date
  session$
    findElement(xpath = '//input[@name = "personSearch.dateFrom"]')$
    clear()$
    sendKeys(
      format(as.Date(date, origin = '1970-01-01'), '%m/%d/%Y'),
      webdriver::key$tab
    )

  # Enter event status
  if (!'New' %in% event_status) {
    session$
      findElement(xpath = '//input[@name = "personSearch.newInitialSelected"]')$
      click()
  }
  if (!'Update' %in% event_status) {
    session$
      findElement(xpath = '//input[@name = "personSearch.updateSelected"]')$
      click()
  }

  # Enter investigation status
  session$
    findElement(xpath = '//input[@name = "InvestigationStatus_textbox"]')$
    clear()$
    sendKeys(inv_status, webdriver::key$tab)

  # Select case status
  case_status_xpath <- paste0(
    '//select[@id = "caseStatusList"]/option[@value = "C" or @value = "P"]'
  )
  case_status <- session$findElements(xpath = case_status_xpath)
  for (status in case_status) status$click()

  # Submit search
  session$
    findElement(xpath = '//input[@type = "button" and @name = "Submit"]')$
    click()

  if (show_screen) session$takeScreenshot()
}

nbs_export_queue <- function(session, show_screen = rlang::is_interactive()) {
  pg_info_init <- nbs_page_info(nbs_pages(session))

  pg_current <- nbs_page_current(pg_info_init)
  pg_max <- nbs_page_max(pg_info_init)

  queue <- nbs_search_results(session)

  while(pg_current < pg_max) {
    nbs_go_to_next_page(session, show_screen = show_screen)
    pg_info <- nbs_page_info(nbs_pages(session))
    pg_current <- nbs_page_current(pg_info)
    pg_max <- nbs_page_max(pg_info)
    queue <- rbind(queue, nbs_search_results(session))
  }

  queue
}

nbs_go_to_next_page <- function(
  session,
  show_screen = rlang::is_interactive()
) {
  pages <- nbs_pages(session)
  pg_info <- nbs_page_info(pages)
  pg_current <- nbs_page_current(pg_info)
  pg_next <- nbs_page_next(pg_info, pg_current = pg_current)

  pages[[nbs_page_index(pg_info, pg_next)]]$click()

  if (show_screen) session$takeScreenshot()

  invisible(session)
}

nbs_pages <- function(session) {
  session$
    findElement(xpath = '//span[@class = "pagelinks"]')$
    findElements(xpath = '*')
}

nbs_page_index <- function(pg_info, pg_number) {
  vec_slice(
    pg_info[['index']],
    pg_info[['text']] == as.character(pg_number)
  )
}

nbs_page_info <- function(pages) {
  df <- tibble::tibble(
    name = character(vec_size(pages)),
    text = character(vec_size(pages)),
    index = vec_seq_along(pages)
  )
  for (i in vec_seq_along(pages)) {
    df[i, 'name'] <- pages[[i]]$getName()
    df[i, 'text']    <- pages[[i]]$getText()
  }
  df
}

nbs_page_next <- function(pg_info, pg_current = NULL) {
  if (vec_is_empty(pg_current)) pg_current <- nbs_page_current(pg_info)
  pg_max <- nbs_page_max(pg_info)

  if (pg_current >= pg_max) pg_current else pg_current + 1L
}

nbs_page_current <- function(pg_info) {
  as.integer(vec_slice(pg_info[['text']], pg_info[['name']] == 'strong'))
}

nbs_page_max <- function(pg_info) {
  max(
    as.integer(stringr::str_subset(pg_info[['text']], '[0-9]+')),
    na.rm = TRUE
  )
}

nbs_search_results <- function(session) {
  table <- rvest::html_node(
    xml2::read_html(session$getSource()),
    xpath = '//table[@id = "searchResultsTable"]'
  )

  df <- rvest::html_table(table)
  colnames(df) <- stringr::str_remove(colnames(df), '\\s*OK/.*$')
  df
}
