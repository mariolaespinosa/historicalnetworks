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
    mutate(local_file = str_replace(local_file, "/corpus/", "/yf_corpus/")) %>% 
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

elevation_pattern <- "(elevat(ion|ed))|altitude|oaks?|hills?|mountains?|robles?|montañas?|colina|((high|low)\\s+(lands?|grounds?))|(lieux\\s+(éléves|bas))|(tierras?\\s+(altas?|bajas?))"

yf_corpus_elevation <- yf_corpus_all %>% 
    subset_corpus(elevation_pattern)

yf_corpus_elevation <- yf_corpus_elevation %>% 
    mutate(year = str_extract(date, "\\d{4}"))

yf_corpus_elevation2 <- yf_corpus_elevation %>% 
    subset_corpus(match_near(match_vector(yf_keywords), elevation_pattern))

early_elevation0 <- bind_rows(tibble(cited_author = "Duhamel", cited_year = 1759),
                             tibble(cited_author = "Lind", cited_year = 1768), # b21365246 (1788 ed); 1768 ed has quote at 198-199
                             tibble(cited_author = "Monro", cited_year = 1764), # accdisease00monr (cites Duhamel)
                             tibble(cited_author = "Rollo", cited_year = 1783), # b21947430 (cites Monro and Lind (2d edition))
                             tibble(cited_author = "Hunter", cited_year = 1788), # * observationsondi00hunt
                             tibble(cited_author = "Jackson", cited_year = 1791), # * atreatiseonfeve00jackgoog (disagrees, at 87)
                             tibble(cited_author = "Chisholm", cited_year = 1799), # * 2546006R.nlm.nih.gov; b21299018_0001
                             tibble(cited_author = "Humboldt", cited_year = 1811), # politicalessayo00blacgoog
                             tibble(cited_author = "Bally", cited_year = 1814), # b28740476 (pp324-326, 334-336; cites Humboldt and Chisholm as additional examples)
                             tibble(cited_author = "Dickinson", cited_year = 1819), # * b28741365 at 59 (cites Humboldt)
                             tibble(cited_author = "Tulloch", cited_year = 1835)) 
# * In all_corpus_raw, but not yf_corpus_elevation2

early_elevation <- all_corpus_raw %>% 
    distinct() %>% 
    filter(id == "b21365246" | # Lind 1788 [1768]
               id == "accdisease00monr" | # Monro 1764
               id == "b21947430" | # Rollo 1783
               id == "observationsondi00hunt" | # Hunter 1788
               id == "atreatiseonfeve00jackgoog" | # Jackson 1791
               id == "b21299018_0001" |  # Chisholm 1801; not in earlier versions of Chisholm
               id == "politicalessayo09humbgoog" | # Humboldt 1811
               id == "b28740476" | # Bally 1814
               id == "b28268428") %>% # Dickinson 1819
    bind_rows(tibble(id = NA, author = "Duhamel", date = "1759"),
              tibble(id = NA, author = "Du Hamel", date = "1759"),
              tibble(id = NA, author = "Lind", date = "1768")) %>% 
    mutate(local_file = str_replace(local_file, "corpus", "yf_corpus"))

early_elevation2 <- all_corpus_raw %>% 
    distinct() %>% 
    filter(id == "b21304415" | # Bancroft 1811
               id == "observationsondi00blan" | # Blane 1785
               id == "2548030R.nlm.nih.gov" | # Currie 1794 at 71
               id == "2541029R.nlm.nih.gov" | # Alibert 1807 at 175
               id == "presentstateofsp02walt" | # Walton 1810 at 101 "Elevated and airy situations are the most healthy . . . [vomito] is observable in Vera Cruz, from example, and arises from the situation and moist vicinity of that city ; hence it ceases when we get into the interior."
               id == "histoiredesmalad01poup" | # Poupée-Desportes 1770 at 66
               id == "2551017R.nlm.nih.gov" | # Davidge 1814 at 72
               id == "b21297940" | # Pym 1815 at 201
               id == "b22372167" | # Doughty 1816 at 73
               id == "in.ernet.dli.2015.31007" | # Fergusson 1817 at 130
               id == "2574034R.nlm.nih.gov" | # Hosack 1820 at 949
               # id == "b21353815_0" | # Dalmas 1820 (2d ed., totally rewritten) at 65 "Enfin, lorsqu'en 1802 l'armée du général Leclerc s'empara de Saint-Domingue, j'appris du général Thouvenot que les troupes stationées sur le plateau de Plaisance conservérent leur viguer et leur santé, quoique la fièvre jaune y fut plusieurs fois importée" du Cap 
               id == "2577008R.nlm.nih.gov" | # Whitney 1833, _The Family Physician_(!), at 22: "These precautions should be strictly attended to by soldiers while marching, and their marches should be short, and their quarters on high airy grounds, or if on low marshy grounds it should be drained by ditching."
               id == "cihm_43284" | # Martin 1834 (History of the British Colonies, Vol. 2) at 179-180 (re Jamaica)
               id == "atwelvemonthsre00maddgoog" | # Madden 1835 at 337-338, quotes British Army surgeon Dr. M'Grath (long at Jamaica): "cases of yellow fever are always to be found in Jamaica except in the mountainous districts where at a certain elevation it is wholly unknown."  Never at over 3000 ft
               # id == "cihm_38226" | # Martin 1834b at 6
               id == "63540640RX1.nlm.nih.gov" | # Tweedie 1840 at 337
               id == "b29339819" | # Curtis 1839 at 104-105: "Residents in the East or West Indies should be very careful in the choice of the situation of their dwellings: the more elevated, and the further removed from all kinds of water, the better."
               id == "b21454309" | # Alison 1843 at 420 (textbook?) : "the worst fevers of the tropical climates are nearly confined to a moderate elevation above the level of the sea"
               id == "narrativeatouri02henrgoog" # Tudor 1834 at 161-162
               ) %>% bind_rows(tibble(id = NA, author = "Tulloch", date = "1835"),
                tibble(id = NA, author = "Dalmas", date = "1805")) # [Dalmas 1805 at 41 not in archive.org except reprint in mmoiresurlanon00lefo] "J'ai appris du général Thouvenot , chef de l'état-major, que les troupes qui occupoieat le plateau élevé de Plaisance ont toujours joui d'une bonne santé, malgré que la maladie y ait élé plusieurs fois apportée par des personnes qui l'avoient prise au Cap, ou ailleurs"
               
cross_ee <- early_elevation %>%
    filter(!is.na(local_file)) %>% 
    find_citing(early_elevation, near = elevation_pattern)

classified_cross_ee <- classify_citing(cross_ee) %>% 
    filter(classification > 0)

cites_early_elevation <- yf_corpus_elevation2 %>% 
    filter(!is.na(local_file)) %>% 
    find_citing(early_elevation, near = elevation_pattern) %>% 
    arrange(year, author, cited) %>% 
    filter(!(str_detect(title, "Magazine|[jJ]ournal|Gazette|(Scientific American)")))

# done <- read_csv("classify_cites_early_elevation.csv") %>% 
#     left_join(yf_corpus_elevation2, by = "id")
# 
# cites_early_elevation <- cites_early_elevation %>% 
#     anti_join(done, by = "author")

cites_early_elevation_pre1846 <- cites_early_elevation %>% filter(year < 1846)

classified_early_elevation_pre1846_1g <- classify_citing(cites_early_elevation_pre1846)

classified_early_elevation_1g2 <- read_csv("classify_cites_early_elevation_pre1846.csv", col_types = "ccicc") %>% 
    filter(classification > 0) %>% 
    left_join(yf_corpus_elevation2, by = c("id"))

cites_ee2 <- yf_corpus_elevation2 %>%
    filter(year < 1846) %>% 
    filter(!is.na(local_file)) %>% 
    find_citing(early_elevation2, near = match_near(elevation_pattern, match_vector(yf_keywords))) %>% 
    arrange(year, author, cited) %>% 
    filter(!(str_detect(title, "Magazine|[jJ]ournal|Gazette|(Scientific American)")))

classified_cites_ee2 <- classify_citing(cites_ee2) %>% 
    filter(classification > 0)



classified_cross_ee <- read_csv("classify_cross_ee.csv", col_types = "ccicc") %>% 
    filter(classification > 0) %>% 
    left_join(yf_corpus_elevation, by = c("id"))

classified_early_elevation_1g2 <- read_csv("classify_cites_early_elevation_pre1846.csv", col_types = "ccicc") %>% 
    filter(classification > 0) %>% 
    left_join(yf_corpus_elevation2, by = c("id"))

classified_cites_ee2 <- read_csv("classify_cites_ee2.csv", col_types = "ccicc") %>% 
    filter(classification > 0) %>% 
    left_join(yf_corpus_elevation2, by = c("id"))

classified_all <- bind_rows(classified_cross_ee,
                            classified_early_elevation_1g2,
                            classified_cites_ee2) %>% 
    filter(classification == 1)

elevation_cites <- classified_all %>% 
    mutate(author = recode(author,
                           "Royal College of Physicians of London" = "Dickinson, Nodes",
                           "Royal College of Physicians of Edinburgh" = if_else(id == "b21914461", "Johnson", "Arnold"),
                           "University of Glasgow. Library" = "Hutchison",
                           "Not Available" = "Duncan",
                           "Académie des sciences (France) n 50055092" = "Portal"),
           citing_author = if_else(str_detect(author, ","), 
                                   str_replace(author, "(^[^,]*).*", "\\1"),
                                   word(author, -1)),
           citing = paste(citing_author, date),
           city = recode(city,
                         "C. S. Van Winkle" = "New York",
                         "Longman" = "London"),
           city = if_else(is.na(city), "London", city)) %>% 
    filter(!str_detect(author, "Rush")) %>% 
    select(cited, citing, date, classification) %>% 
    distinct()

# elevation_2g <- yf_corpus_race %>% 
#     find_citing(classified_elevation_1g, near = elevation_pattern)
# 
# classified_elevation_2g <- classify_citing(elevation_2g) %>% 
#     filter(classification > 0) 
# 
# cross_cites <- bind_rows(classified_elevation_1g,
#                          classified_elevation_2g) %>% 
#     find_citing(classified_lining_race_2g, near = race_pattern) 
# 
# classified_cross_cites <- classify_citing(cross_cites) %>% 
#     filter(classification > 0)


elevation_plot <- citation_network_plot(elevation_cites)