library(tidyverse)
library(historicalnetworks)

# yf_corpus <- build_corpus(keywords = "yellow fever",
#                           date_range = "1700 TO 1899")
# 
# # saving this output is a good idea, given the runtime 
# save(list = "yf_corpus", file = "data/yf_corpus.rda")

corpus_keywords <- c("yellow fever", "fever", "disease", "medicine", "health", "military hygiene",
                     "vomito", "fiebre", "enfermidad", "medicina", "salud", "higiene",
                     "fièvre", "maladie", "médecine", "santé", "hygiène",
                     "west indies", "caribbean", "mexico", "new spain",
                     "caribe", "méxico",
                     "caraïbes", "mexique", "saint domingue")

all_corpus <- build_corpus(keywords = corpus_keywords,
                           date_range = "1650 TO 1900",
                           max_results = 50000)

yf_keywords <- c("yellow fever", "fiebre amarillo", "fièvre jaune",
                 "black vomit", "vomito negro", "vomito prieto", 
                 "maladie de siam")


load("data/yf_corpus.rda")

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

elevation_pattern <- "elevat|altitude|oak|hill|mountain|roble|montaña|colina|(high|low)\\s+land|lieux\\s+(éléves|bas)"

yf_corpus_elevation <- yf_corpus %>% 
    subset_corpus(elevation_pattern)

early_elevation <- bind_rows(tibble(cited_author = "Duhamel", cited_year = 1759),
                             tibble(cited_author = "Lind", cited_year = 1762), # b21365246 (1788 ed)
                             tibble(cited_author = "Monro", cited_year = 1764), # accdisease00monr (cites Duhamel)
                             tibble(cited_author = "Rollo", cited_year = 1783), # b21947430 (cites Monro and Lind)
                             tibble(cited_author = "Hunter", cited_year = 1788), # observationsondi00hunt
                             tibble(cited_author = "Jackson", cited_year = 1791), 
                             tibble(cited_author = "Humboldt", cited_year = 1811))
# Tullock
# Bally
# Dickinson 1819
# Chisholm

more_elevation <- bind_rows(tibble(cited_author = "Chisholm", cited_year = 1799), # 2546006R.nlm.nih.gov; b21299018_0001
                            tibble(cited_author = "Bally", cited_year = 1814), # b28740476
                            tibble(cited_author = "Tullock", cited_year = 1838),
                            tibble(cited_author = "Dickinson", cited_year = 1819))

cites_early_elevation <- yf_corpus_elevation %>% 
    find_citing(early_elevation, near = elevation_pattern)

classified_early_elevation_1g <- classify_citing(cites_early_elevation) %>% 
    filter(classification > 0)

cites_more_elevation <- yf_corpus_elevation %>% 
    find_citing(more_elevation, near = elevation_pattern)

classified_more_elevation_1g <- classify_citing(cites_more_elevation) %>% 
    filter(classification > 0)
