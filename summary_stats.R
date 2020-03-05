#load package
library(tidyverse)

#read in raw data
# top_aa <- read_csv("dta_aa.csv")
# top_nhpi <- read_csv("dta_nhpi.csv")

#racial group specific metro list
# asian_metrolist <- c("1","2","4","5","6","8","10","11","12",
#                      "13","14","15","17","18","20","21","22","23","25","26")
# nhpi_metrolist <- c("3","4","7","8","9","10","11","13","15",
#                     "16","17","18","19","20","21","22",
#                     "23","24","25","27")
#metro list reference
# counties_nhpi <- read_csv("nhpi_countylists.csv") %>% 
#   rename(metro_label = NAMELSAD_2)
# counties <- read_csv("top_metro_ct.csv")
# metro_list <- counties %>% 
#   dplyr::select(NAMELSAD) %>%
#   rename(metro_label = NAMELSAD) %>% 
#   unique() %>% 
#   mutate(metro = as.numeric(row_number()))

# getting reference labels ------------------------------------------------
label <- load_variables(year = 2018, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_023", "B02015_024", "B02015_025"))
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"

label2 <- load_variables(year = 2018, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012"))
label2[1,2] <- "NHPI"


#summary
state_list <- c("CA", "CO", "PA", "NJ", "DE", "MD", "DC", "VA", 
                "WV", "FL", "MO", "IL", "IN", "WI", "MA", "NH", 
                "MI", "MN", "NY", "NC", "SC", "OR", "WA", "TX", 
                "AZ", "HI", "AK", "AR", "UT", "GA", "NV")

metro_GEOID <- c("12060", "14460", "22220", "19100", "19820", "16980", "11260", 
                 "26420", "27980", "29820", "31080", "33460", "35620", "37980", "38060", 
                 "39340", "40140", "40900", "41620", "41740", "41860", "41940", 
                 "42660", "44700", "46520", "47900")
data <- get_acs(table = "B01003", geography = "metropolitan statistical area/micropolitan statistical area",
                year = 2018, cache_table = T)
data2 <- get_acs(table = "B02015", geography = "metropolitan statistical area/micropolitan statistical area",
                year = 2018, cache_table = T, summary_var = "B02015_001")
data3 <- get_acs(table = "B02016", geography = "metropolitan statistical area/micropolitan statistical area",
                 year = 2018, cache_table = T, summary_var = "B02016_001")

metro_filter <- data %>% 
  filter(GEOID %in% metro_GEOID) %>% 
  dplyr::select(GEOID, NAME) 
  
tot <- data %>% 
  filter(GEOID %in% metro_GEOID) %>% 
  dplyr::select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)

asn <- data2 %>% 
  filter(GEOID %in% metro_GEOID) %>% 
  dplyr::select(GEOID, NAME, variable, estimate, summary_est) %>% 
  rename(asn_pop = summary_est,
         aa_pop = estimate) %>% 
  left_join(label) %>% 
  left_join(tot) %>% 
  filter(! variable == "B02015_001",
         is.na(label) == F) %>% 
  group_by(GEOID) %>% 
  arrange(desc(aa_pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% filter(rank == 1) %>% 
  mutate(pct_asn = scales::percent((asn_pop / tot_pop), accuracy = 0.1),
         pct_aa = scales::percent((aa_pop / asn_pop), accuracy = 0.1),
         tot_pop = scales::comma(tot_pop),
         asn_pop = scales::comma(asn_pop)) %>% 
  dplyr::select(NAME, tot_pop, asn_pop, pct_asn, label, pct_aa)

pi <- data3 %>% 
  filter(GEOID %in% metro_GEOID) %>% 
  dplyr::select(GEOID, NAME, variable, estimate, summary_est) %>% 
  rename(nhpi_pop = summary_est,
         pi_pop = estimate) %>% 
  left_join(label2) %>% 
  left_join(tot) %>% 
  filter(! variable == "B02016_001",
         is.na(label) == F) %>% 
  group_by(GEOID) %>% 
  arrange(desc(pi_pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% filter(rank == 1) %>% 
  mutate(pct_nhpi = scales::percent((nhpi_pop / tot_pop), accuracy = 0.1),
         pct_pi = scales::percent((pi_pop / nhpi_pop), accuracy = 0.1),
         tot_pop = scales::comma(tot_pop),
         nhpi_pop = scales::comma(nhpi_pop),
         label2 = label) %>% 
  dplyr::select(NAME, tot_pop, nhpi_pop, pct_nhpi, label2, pct_pi)


summary_dta <- asn %>% left_join(pi)
write_csv(summary_dta, "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/summary_2018.csv")

