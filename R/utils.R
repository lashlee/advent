write_to_clipboard <- function(data) {
  if (.Platform$OS.type == 'unix') cat(data, file = pipe('pbcopy'))
  if (.Platform$OS.type == 'windows') writeClipboard(as.character(data))
  invisible(TRUE)
}
