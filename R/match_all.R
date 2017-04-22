#' Generate a Regular Expression for Finding Multiple Patterns in a Text
#'
#' 
#' @param pattern1,pattern2 Two character strings each containing a word or a \link[regular expression]{stringi-search-regex} to be found in the same text.  Alternately, pattern1 can be a character vector.
#' @param max_distance The maximum number of words intervening between the two specified patterns
#' 
#' @details
#' \code{match_all} is a convenience function that creates a regular expression pattern for finding if two words (or other regular expression patterns) occur within a text
#' 
#' 
#' @return A regular expression pattern
#'
#' @examples
#' colors <- "red|blue|yellow"
#' sentences <- c("The car parked by their house was red",
#'                "Her yellow cat enjoys the sun on the windowsill",
#'                "The blue city bus passed right by the stopped truck")
#' stringr::str_detect(sentences, match_all("yellow", "car"))
#' stringr::str_detect(sentences, match_all(colors, "car"))
#' stringr::str_detect(sentences, match_all(colors, "car|bus"))
#' stringr::str_detect(sentences, match_all(c("bus", "truck", "city")))
#'
#' @export

match_all <- function(pattern1, pattern2 = "") {
    if (!pattern=="") {
        pattern <- paste0("^",
                          c(pattern1, pattern2) %>% 
                              paste0("(?=.*\\b(", ., ")\\b)", collapse = ""),
                          ".*$")
    } else {
        pattern <- paste0("^",
                          pattern1 %>% 
                              paste0("(?=.*\\b(", ., ")\\b)", collapse = ""),
                          ".*$")
    }
    return(pattern)
}
