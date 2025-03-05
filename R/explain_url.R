#' Parse and Explain URL Query Parameters
#'
#' Extracts and parses the query parameters from a given URL. It splits the
#' query string into key-value pairs and returns a structured dataframe.
#'
#' @param url A character string representing the URL to be parsed.
#'
#' @return A dataframe with two columns: \code{Key} and \code{Value},
#'    containing the parsed query parameters.
#' @export
#'
#' @examples
#' explain_url("https://example.com/page?name=John&age=30&city=New%20York")
explain_url <- function(url) {
  query_string <- stringr::str_split(url, "\\?", simplify = TRUE)[, 2]
  key_value_pairs <- stringr::str_split(query_string, "&")[[1]]

  parsed_pairs <- lapply(key_value_pairs, function(pair) {
    split_pair <- stringr::str_split(pair, "=", simplify = TRUE)
    key <- split_pair[1]
    value <- ifelse(length(split_pair) > 1, utils::URLdecode(split_pair[2]), NA)
    data.frame(Key = key, Value = value, stringsAsFactors = FALSE)
  })

  do.call(rbind, parsed_pairs)
}
