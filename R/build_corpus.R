#' Build a Corpus of Works from the Internet Archive
#'
#' \code{build_corpus} downloads the OCR text versions of works found by searching the Internet Archive's metadata for the specified `keywords` over a given `date_range` (provided in the format "yyyy TO yyyy"), and it returns a dataframe that includes the Internet Archive's metadata about the retrieved works along with the location of the corresponding text files.
#' 
#' @param keywords A vector of keywords to search in the metadata of the Internet Archive's text collection
#' @param date_range The desired data range to search, specified in the format "yyyy TO yyyy"
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the Internet Archive will be downloaded.
#' @param max_results The maximum number of text results
#' @param chime Should the function chime on completion?
#' 
#' @details
#' Details needed
#' 
#' @return A dataframe representing the corpus of downloaded texts
#'
#' @examples
#' \dontrun{
#'  yf_corpus <- build_corpus(keywords = "yellow fever")
#' }
#'
#' @import internetarchive
#' @importFrom dplyr "%>%" select mutate filter group_by
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom tools file_ext
#' @importFrom stringr str_detect str_extract str_replace str_replace_all
#' @importFrom beepr beep
#'
#' @export

build_corpus <- function(keywords, 
                         date_range = "1700 TO 1899",
                         download_dir = "data-raw/corpus",
                         max_results = 10000,
                         chime = TRUE) {
    found_items <- purrr::map(keywords, function(keyword) {
        ia_search(c(text = keyword, date = date_range), 
                             num_results = max_results)
        }) %>% 
        unlist() %>% 
        unique() %>% 
        sort() %>% 
        ia_get_items()
    
    metadata <- found_items %>% 
        ia_metadata2() %>% 
        tidyr::spread(key = field, value = value) %>% 
        select(id, title, date, creator, creator1, creator2, volume, publisher) %>% 
        mutate(author = ifelse(is.na(creator), creator1, creator),
               city = ifelse(str_detect(publisher, ":"), 
                             str_extract(publisher, "^.*?(?=:)") %>% 
                                 str_replace("(^[^,]*).*", "\\1"),
                             str_replace(publisher, "(^[^,]*).*", "\\1")) %>% 
                   str_replace("^A\\s+", "") %>%
                   str_replace("^Printed in\\s+", "") %>% 
                   str_replace_all("[\\[\\]]", "") %>%
                   str_replace("-", " ") %>% 
                   str_replace("\\?\\s*$", "") %>% 
                   ifelse(test = str_detect(., "&|and"), NA_character_, .) %>% 
                   str_trim()) %>% 
        select(id, author, date, title, city, publisher, creator2, volume)
    
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE) 

    corpus <- metadata %>% 
        left_join(found_items %>% 
                      ia_files2() %>% 
                      filter(str_detect(file, "djvu\\.txt$")) %>% 
                      group_by(id) %>% 
                      ia_download(dir = download_dir, overwrite = FALSE)) %>% 
        filter(!is.na(local_file)) %>% 
        select(-file, -type)
    
    if (chime) {
        beepr::beep()
    }
    
    return(corpus)
}

ia_metadata2 <- function(items) {
    metadata_to_data_frame <- function(i) {
        m <- unlist(i$metadata)
        if (is.character(unlist(i$metadata$identifier))) {
            data_frame(id = unlist(i$metadata$identifier), field = names(m), 
                       value = unname(m)) 
        } else {
            data_frame(id = NA_character_, field = NA_character_, 
                       value = NA_character_)
        }
    }
    dfs <- lapply(items, metadata_to_data_frame)
    bind_rows(dfs) %>% filter(!is.na(id))
}


ia_files2 <- function (items) {
    files_to_data_frame <- function(i) {
        f <- unlist(names(i$files))
        if (is.character(unlist(i$metadata$identifier))) {
            data_frame(id = unlist(i$metadata$identifier), file = f, 
                       type = tools::file_ext(file)) 
        } else {
            data_frame(id = NA_character_, file = NA_character_, 
                       type = NA_character_)
        }
    }
    dfs <- lapply(items, files_to_data_frame)
    bind_rows(dfs)
}
