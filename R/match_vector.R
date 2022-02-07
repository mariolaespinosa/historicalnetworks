#' Generate a Regular Expression for Finding Any of a Vector of Patterns
#'
#' 
#' @param pattern A character vector containing a series of words or \link[regular expression]{stringi-search-regex}s, any one of which is sufficient for a match
#' 
#' @details
#' \code{match_vector} is a convenience function that creates a regular expression pattern for finding any of a vector of words (or other regular expression patterns)
#' 
#' 
#' @return A regular expression pattern
#'
#' @examples
#' colors <- c("red", "blue", "yellow")
#' sentences <- c("The car parked by their house was red",
#'                "The yellow cat enjoys the sun atop the car's roof",
#'                "The blue city bus passed right by the stopped car")
#' stringr::str_detect(sentences, match_vector(colors))
#' 
#'
#' @export

match_vector <- function(pattern) {
    new_pattern <- str_replace_all(paste0("(", paste0(pattern, collapse = ")|("), ")"), " ", "\\\\s+")
    return(new_pattern)
}
