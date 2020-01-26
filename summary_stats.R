#load package
library(tidyverse)

#read in raw data
top_aa <- read_csv("dta_aa.csv")
top_nhpi <- read_csv("dta_nhpi.csv")

#racial group specific metro list
# asian_metrolist <- c("1","2","4","5","6","8","10","11","12",
#                      "13","14","15","17","18","20","21","22","23","25","26")
# nhpi_metrolist <- c("3","4","7","8","9","10","11","13","15",
#                     "16","17","18","19","20","21","22",
#                     "23","24","25","27")
#metro list reference
counties_nhpi <- read_csv("nhpi_countylists.csv") %>% 
  rename(metro_label = NAMELSAD_2)
counties <- read_csv("top_metro_ct.csv")
metro_list <- counties %>% 
  dplyr::select(NAMELSAD) %>%
  rename(metro_label = NAMELSAD) %>% 
  unique() %>% 
  mutate(metro = as.numeric(row_number()))

# getting reference labels ------------------------------------------------
label <- load_variables(year = 2017, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A"))
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"

#summary
summary_aa <- top_aa %>% 
  select(metro, variable, pop) %>% 
  group_by(metro, variable) %>% 
  mutate(pop = sum(pop)) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(metro) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank == 1) %>% 
  left_join(metro_list) %>% 
  mutate(poplb = scales::comma(pop)) %>% 
  left_join(label) %>% 
  dplyr::select(metro_label, metro, label, poplb) %>% 
  write_csv("summary_aa.csv", na = "")




# getting reference labels ------------------------------------------------
label <- load_variables(year = 2017, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A"))
label[1,2] <- "NHPI"


summary_nhpi <- top_nhpi %>% 
  select(metro, variable, pop) %>% 
  group_by(metro, variable) %>% 
  mutate(pop = sum(pop)) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(metro) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank == 1) %>% 
  left_join(metro_list) %>% 
  mutate(poplb = scales::comma(pop)) %>% 
  left_join(label) %>% 
  dplyr::select(metro_label, metro, label, poplb) %>% 
  mutate(metro_label = case_when(
    metro == 27 ~"Portland-Vancouver-Hillsboro, OR-WA Metro Area",
    TRUE ~metro_label)) %>% 
  write_csv("summary_nhpi.csv", na = "")



