#' Are you running MacOS?
#'
#' \code{is_macos} checks your \code{\link{.Platform}} and \code{\link{Sys.info}} variables to determine whether or not you are running MacOS.
#'
#' @return boolean
#' @export
#'
is_mac <- function() {
  .Platform$OS.type == 'unix' & Sys.info()['sysname'] == 'Darwin'
}

#' Are you running Windows?
#'
#' \code{is_windows} checks your \code{\link{.Platform}} variable to determine whether or not you are running Windows.
#'
#' @return boolean
#' @export
#'
is_windows <- function() {
  .Platform$OS.type == 'windows'
}

#' Write data to the clipboard.
#'
#' \code{write_to_clipboard} writes the input data to the clipboard.
#'
#' @param data anything
#'
#' @return data (invisibly)
#' @export
#'
write_to_clipboard <- function(data) {
  if (is_mac()) cat(data, file = pipe('pbcopy'))
  if (is_windows()) writeClipboard(as.character(data))
  data
}

#' Get data from URL and use cookie
#'
#' @param url character
#' @param session_cookie character
#'
#' @return character
#' @export
#'
get_data <- function(url, session_cookie = NULL) {
  if (is.null(session_cookie)) session_cookie <- Sys.getenv('AOC_SC')
  res <- httr::GET(url, httr::set_cookies(session = session_cookie))
  httr::content(res, encoding = 'UTF-8')
}
