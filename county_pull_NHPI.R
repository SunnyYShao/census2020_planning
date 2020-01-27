# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)

# getting reference labels ------------------------------------------------
label <- load_variables(year = 2017, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A"))
label[1,2] <- "NHPI"

# total pop at tract level ------------------------------------------------
tot_county <- get_acs(table = "B01003", year = 2017, geography = "county", cache_table = T)
tot_county <- tot_county %>% 
  dplyr::select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)
# pulling detailed AA tract-level data ------------------------------------
nhpi_ct <- get_acs(table = "B02016", year = 2017, geography = "county", summary_var = "B02016_001", cache_table = T)

dta <- nhpi_ct %>% 
  dplyr::select(GEOID, NAME, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02016_001", "B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A")) %>% 
  left_join(tot_county) %>% 
  rename(pop = estimate,
         nhpi_pop = summary_est) %>% 
  mutate(pct_nhpi = round(pop / nhpi_pop, digits = 2),
         pct_tot = round(pop / tot_pop, digits = 2)) %>% 
  mutate(pct_nhpi = case_when(
    nhpi_pop == 0 ~0,
    pop == 0 ~0,
    TRUE ~pct_nhpi)) %>% 
  mutate(pct_tot = case_when(
    tot_pop == 0 ~0,
    pop ==0 ~0,
    TRUE ~pct_tot))

top_nhpi <- dta %>% 
  group_by(GEOID) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1) %>% 
  ungroup() %>% 
  mutate(top1 = scales::comma(pop),
         top2 = case_when(
           is.na(pct_nhpi) == F ~paste(as.character(pct_nhpi*100), "%", sep = ""),
           TRUE ~as.character(pct_nhpi))) %>% 
  mutate(top3 = case_when(
    is.na(pct_tot) == F ~paste(as.character(pct_tot*100), "%", sep = ""),
    TRUE ~as.character(pct_tot))) %>% 
  # dplyr::select(GEOID, NAME, variable, top1, top2, top3, pop, pct_asn, pct_tot, metro) %>% 
  left_join(label) %>% dplyr::select(-variable)

library(tigris)
options(tigris_use_cache = T)

library(rappdirs)
user_cache_dir()

library(GISTools)
library(rgdal)
# install.packages("rmapshaper")  # install if necessary
library(sf)
library(rmapshaper)
install.packages("rgeos")
library(rgeos)
devtools::install_github('walkerke/tigris')

combined <- counties(cb = TRUE, refresh = TRUE)
combined_generalized = ms_simplify(combined, keep = 0.05)

# top NHPI in each metro area ------------------------------------
merge <- geo_join(combined_generalized, top_nhpi, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/CT_NHPI", layer="CT_NHPI", driver="ESRI Shapefile") # this is in geographical projection
##############

