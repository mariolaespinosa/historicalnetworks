#' Generate a Regular Expression for Finding Two Patterns Occurring Near Each Other
#'
#' 
#' @param pattern1,pattern2 Two character strings each containing a word or a \link[regular expression]{stringi-search-regex} to be found near each other.
#' @param max_distance The maximum number of words intervening between the two specified patterns
#' 
#' @details
#' \code{match_near} is a convenience function that creates a regular expression pattern for finding if two words (or other regular expression patterns) occur within a specified number of words of each other
#' 
#' 
#' @return A regular expression pattern
#'
#' @examples
#' colors <- "red|blue|yellow"
#' sentences <- c("The car parked by their house was red",
#'                "The yellow cat enjoys the sun atop the car's roof",
#'                "The blue city bus passed right by the stopped car")
#' stringr::str_detect(sentences, match_near(colors, "car", 5))
#' stringr::str_detect(sentences, match_near(colors, "car|bus", 5))
#' stringr::str_detect(sentences, match_near(colors, "car", 10))
#' 
#'
#' @export

match_near <- function(pattern1, pattern2, max_distance = 250) {
    pattern <- paste0("\\b(\\w*", pattern1, "\\w*)\\W+(?:\\w+\\W+){1,", max_distance, "}?(\\w*", pattern2,
                      "\\w*)\\b|(\\w*", pattern2,"\\w*)\\W+(?:\\w+\\W+){1,", max_distance,"}?(\\w*", pattern1, "\\w*)\\b")
    return(pattern)
}
