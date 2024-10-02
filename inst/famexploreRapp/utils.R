capitalize_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}
