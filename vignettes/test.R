library(tidyverse)
library(stringr)
library(historicalnetworks)

# yf_corpus <- build_corpus(keywords = "yellow fever",
#                           date_range = "1700 TO 1899")
# 
# # saving this output is a good idea, given the runtime 
# save(list = "yf_corpus", file = "data/yf_corpus.rda")

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

race_cites <- bind_rows(classified_lining_race_1g,
                        classified_lining_race_2g,
                        classified_cross_cites) %>% 
    select(-.row) %>% 
    distinct() %>% 
    mutate(citing_author = if_else(str_detect(author, ","), 
                                   str_replace(author, "(^[^,]*).*", "\\1"),
                                   author),
           citing_author = recode(citing_author,
                                  "Great Britain. General Board of Health" = "GB GBH",
                                  "Jones" = "Jones and Allen"),
           citing = paste(citing_author, date)) %>% 
    select(cited, citing, date, classification) %>% 
    bind_rows(data_frame(citing = "Lining 1756", date = 1756, classification = 1))
