library(tidyverse)
library(historicalnetworks)

# yf_corpus <- build_corpus(keywords = "yellow fever",
#                           date_range = "1700 TO 1899")
# 
# # saving this output is a good idea, given the runtime 
# save(list = "yf_corpus", file = "data/yf_corpus.rda")

corpus_keywords1 <- c("yellow fever", "fever", "disease", "medicine", "health", "military hygiene")
corpus_keywords2 <- c("vomito", "fiebre", "enfermidad", "medicina", "salud", "higiene",
                     "fièvre", "maladie", "médecine", "santé", "hygiène",
                     "west indies", "caribbean", "mexico", "new spain")
corpus_keywords3 <- c("caribe", "méxico",
                     "caraïbes", "mexique", "saint domingue")

all_corpus1 <- purrr::map_df(corpus_keywords1, function(keyword) {
    corpus_part <- tryCatch({build_corpus(keywords = keyword,
                                    date_range = "1650 TO 1900",
                                    max_results = 50000)}, 
                            error = function(e) {
                                tibble(id = "error",
                                       author = keyword)
                            })
    return(corpus_part)
})

all_corpus2 <- purrr::map_df(corpus_keywords2, function(keyword) {
    corpus_part <- tryCatch({build_corpus(keywords = keyword,
                                          date_range = "1650 TO 1900",
                                          max_results = 50000)}, 
                            error = function(e) {
                                tibble(id = "error",
                                       author = keyword)
                            })
    return(corpus_part)
})

all_corpus3 <- purrr::map_df(corpus_keywords3, function(keyword) {
    corpus_part <- tryCatch({build_corpus(keywords = keyword,
                                          date_range = "1650 TO 1900",
                                          max_results = 50000)}, 
                            error = function(e) {
                                tibble(id = "error",
                                       author = keyword)
                            })
    return(corpus_part)
})

keywords_to_redo <- bind_rows(all_corpus1, all_corpus2, all_corpus3) %>% 
    filter(id == "error") %>% 
    pull(author)

found_items <- purrr::map(keywords_to_redo, function(keyword) {
    internetarchive::ia_search(c(text = keyword, date = date_range), 
              num_results = max_results)
}) %>% 
    unlist() %>% 
    unique() %>% 
    sort() %>% 
    internetarchive::ia_get_items()

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

all_corpus4 <- metadata %>% 
    left_join(found_items %>% 
                  ia_files2() %>% 
                  filter(str_detect(file, "djvu\\.txt$")) %>% 
                  group_by(id) %>% 
                  internetarchive::ia_download(dir = download_dir, overwrite = FALSE)) %>% 
    filter(!is.na(local_file)) %>% 
    select(-file, -type)

all_corpus_raw <- bind_rows(all_corpus1, all_corpus2, all_corpus3, all_corpus4) %>% 
    filter(!id=="error")

save(list = "all_corpus_raw", file = "data/all_corpus_raw.rda")
load("data/all_corpus_raw.rda")

all_files <- list.files("data-raw/corpus")
all_corpus_raw <- all_corpus_raw %>% filter(local_file %in% paste0("data-raw/corpus/", all_files))

yf_keywords <- c("yellow fever", "fiebre amarillo", "fièvre jaune", "febre amarella",
                 "black vomit", "vomito negro", "vomito prieto", 
                 "maladie de siam")

yf_corpus_all <- all_corpus_raw %>% 
    omit_duplicates() %>% 
    subset_corpus(yf_keywords) %>%
    mutate(local_file = str_replace(local_file, "corpus", "yf_corpus"))

dir.create("data-raw/yf_corpus")
file.copy(str_replace(yf_corpus_all$local_file, "yf_corpus", "corpus"), yf_corpus_all$local_file)

save(list = "yf_corpus_all", file = "data/yf_corpus_all.rda")
load("data/yf_corpus_all.rda")

yf_corpus <- yf_corpus %>% 
    omit_duplicates() %>% 
    filter(!(str_detect(title, "Magazine|Journal|Gazette|(Scientific American)")))

race_pattern <- "blacks|negroes|slave|africa|négres|esclave|africain|senegal|negros|esclavo"

yf_corpus_race <- yf_corpus %>%
    subset_corpus(race_pattern)

lining_race_1g <- yf_corpus_race %>% 
    find_citing(data_frame(cited_author = "Lining", cited_year = 1756), near = race_pattern)

classified_lining_race_1g <- classify_citing(lining_race_1g) %>% 
    filter(classification > 0) %>% 
    mutate(author = recode(author, 
                           "Royal College of Physicians of London. n 80046799" = "De Maria, Alfonso"))

lining_race_2g <- yf_corpus_race %>% 
    find_citing(classified_lining_race_1g, near = race_pattern)

classified_lining_race_2g <- classify_citing(lining_race_2g) %>% 
    filter(classification > 0) %>% 
    mutate(author = recode(author, 
                           "Royal College of Physicians of London. n 80046799" = "De Maria, Alfonso"))

cross_cites <- bind_rows(classified_lining_race_1g,
                         classified_lining_race_2g) %>% 
    find_citing(classified_lining_race_2g, near = race_pattern) 

classified_cross_cites <- classify_citing(cross_cites) %>% 
    filter(classification > 0)

all_cites <- bind_rows(classified_lining_race_1g,
                       classified_lining_race_2g,
                       classified_cross_cites) %>% 
    distinct() %>% 
    mutate(citing_author = if_else(str_detect(author, ","), 
                                   str_replace(author, "(^[^,]*).*", "\\1"),
                                   author),
           citing_author = recode(citing_author,
                                  "Great Britain. General Board of Health" = "GB GBH",
                                  "Jones" = "Jones and Allen"),
           citing = paste(citing_author, date),
           cited = recode(cited,
                          "Jones 1794" = "Jones and Allen 1794")) %>% 
    select(cited, citing, date, classification)

lining_plot <- citation_network_plot(all_cites)



####

elevation_pattern <- "elevat|altitude|oak|hill|mountain|roble|montaña|colina|((high|low)\\s+(lands?|grounds?))|(lieux\\s+(éléves|bas))|(tierras?\\s+(altas?|bajas?))"

yf_corpus_elevation <- yf_corpus_all %>% 
    subset_corpus(elevation_pattern)

yf_corpus_elevation <- yf_corpus_elevation %>% 
    mutate(year = str_extract(date, "\\d{4}"))

early_elevation <- bind_rows(tibble(cited_author = "Duhamel", cited_year = 1759),
                             tibble(cited_author = "Lind", cited_year = 1762), # b21365246 (1788 ed)
                             tibble(cited_author = "Monro", cited_year = 1764), # accdisease00monr (cites Duhamel)
                             tibble(cited_author = "Rollo", cited_year = 1783), # b21947430 (cites Monro and Lind)
                             tibble(cited_author = "Hunter", cited_year = 1788), # observationsondi00hunt
                             tibble(cited_author = "Jackson", cited_year = 1791), # atreatiseonfeve00jackgoog
                             tibble(cited_author = "Humboldt", cited_year = 1811), # politicalessayo00blacgoog
                             tibble(cited_author = "Chisholm", cited_year = 1799), # 2546006R.nlm.nih.gov; b21299018_0001
                             tibble(cited_author = "Bally", cited_year = 1814), # b28740476
                             tibble(cited_author = "Tullock", cited_year = 1838),
                             tibble(cited_author = "Dickinson", cited_year = 1819)) # b29341991

cites_early_elevation <- yf_corpus_elevation %>% 
    find_citing(early_elevation, near = elevation_pattern)

classified_early_elevation_1g <- classify_citing(cites_early_elevation) %>% 
    filter(classification > 0)

cites_more_elevation <- yf_corpus_elevation %>% 
    find_citing(more_elevation, near = elevation_pattern)

classified_more_elevation_1g <- classify_citing(cites_more_elevation) %>% 
    filter(classification > 0)

classified_elevation_1g <- bind_rows(classified_early_elevation_1g,
                                     classified_more_elevation_1g) %>% 
    mutate(author = recode(author,
                           "Royal College of Surgeons of England" = "Williams"))

elevation_2g <- yf_corpus_race %>% 
    find_citing(classified_elevation_1g, near = elevation_pattern)

classified_elevation_2g <- classify_citing(elevation_2g) %>% 
    filter(classification > 0) 

cross_cites <- bind_rows(classified_elevation_1g,
                         classified_elevation_2g) %>% 
    find_citing(classified_lining_race_2g, near = race_pattern) 

classified_cross_cites <- classify_citing(cross_cites) %>% 
    filter(classification > 0)

all_cites <- bind_rows(classified_lining_race_1g,
                       classified_lining_race_2g,
                       classified_cross_cites) %>% 
    distinct() %>% 
    mutate(citing_author = if_else(str_detect(author, ","), 
                                   str_replace(author, "(^[^,]*).*", "\\1"),
                                   author),
           citing_author = recode(citing_author,
                                  "Great Britain. General Board of Health" = "GB GBH",
                                  "Jones" = "Jones and Allen"),
           citing = paste(citing_author, date),
           cited = recode(cited,
                          "Jones 1794" = "Jones and Allen 1794")) %>% 
    select(cited, citing, date, classification)

lining_plot <- citation_network_plot(all_cites)