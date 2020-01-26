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

# list of state and counties where top MSAs locate ------------------------
state_list <- c("CA", "CO", "PA", "NJ", "DE", "MD", "DC", "VA", 
                "WV", "FL", "MO", "IL", "IN", "WI", "MA", "NH", 
                "MI", "MN", "NY", "NC", "SC", "OR", "WA", "TX", 
                "AZ", "HI", "AK", "AR", "UT", "GA", "NV")

counties_nhpi <- read_csv("nhpi_countylists.csv") %>% 
  rename(metro_label = NAMELSAD_2)

counties <- read_csv("top_metro_ct.csv")

metro_list <- counties %>% 
  dplyr::select(NAMELSAD) %>%
  rename(metro_label = NAMELSAD) %>% 
  unique() %>% 
  mutate(metro = as.numeric(row_number()))

counties_nhpi <- counties_nhpi %>% 
  left_join(metro_list) %>% 
  mutate(metro = case_when(
    metro_label == "Portland-Vancouver-Hillsboro, OR-WA Metro Area" ~27,
    TRUE ~metro))


counties_slim <- counties_nhpi %>% 
  dplyr::select(-metro_label) %>% 
  rename(ct_geoid = GEOID)

top_ct <- as.vector(counties_slim$ct_geoid)

# total pop at tract level ------------------------------------------------
tot_tract <- get_acs(table = "B01003", year = 2017, geography = "tract", state = state_list, cache_table = T)
tot_tract <- tot_tract %>% 
  dplyr::select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)

# pulling detailed AA tract-level data ------------------------------------
nhpi_tract <- get_acs(table = "B02016", year = 2017, geography = "tract", state = state_list, summary_var = "B02016_001", cache_table = T)

dta <- nhpi_tract %>% 
  dplyr::select(GEOID, NAME, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02016_001", "B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A")) %>% 
  left_join(tot_tract) %>% 
  mutate(ct_geoid = substr(GEOID, 1, 5)) %>% 
  filter(ct_geoid %in% top_ct) %>% 
  left_join(counties_slim) %>% 
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
    TRUE ~pct_tot)) %>% 
  dplyr::select(-ct_geoid)

write_csv(dta, "dta_nhpi.csv", na = "")
#####Top NHPI groups in each census tract
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

top_nhpi_map <- top_nhpi %>% 
  dplyr::select(GEOID, NAME, label, top1, top2, top3, pop, pct_nhpi, pct_tot, metro) %>% 
  mutate(metro = as.integer(metro)) %>% 
  mutate(pop = as.integer(pop),
         pct_nhpi = round(pct_nhpi, digits = 2),
         pct_tot = round(pct_tot, digits = 2)) %>% 
  mutate(pop = case_when(
    top1 == "0" ~NA_integer_,
    TRUE ~pop)) %>% 
  mutate(pct_nhpi = case_when(
    top2 == "0%" ~NA_real_,
    TRUE ~pct_nhpi)) %>% 
  mutate(pct_tot = case_when(
    top3 == "0%" ~NA_real_,
    TRUE ~pct_tot)) %>% 
  write_csv("Top_NHPIs.csv", na = "")
  

# generating group specific df --------------------------------------------
dta <- dta %>% 
  mutate(poplb = scales::comma(pop)) %>% 
  mutate(pctlb1 = case_when(
    is.na(pct_nhpi)==F ~paste(as.character(pct_nhpi*100), "%", sep = ""),
    TRUE ~as.character(pct_nhpi))) %>% 
  mutate(pctlb2 = case_when(
    is.na(pct_tot)==F ~paste(as.character(pct_tot*100), "%", sep = ""),
    TRUE ~as.character(pct_tot))) %>% 
  dplyr::select(-nhpi_pop, -tot_pop, -NAMELSAD) %>% 
  mutate(pop = case_when(
    pop == 0 ~NA_real_,
    TRUE ~pop)) %>% 
  mutate(pct_nhpi = case_when(
    pct_nhpi == 0 ~NA_real_,
    TRUE ~pct_nhpi)) %>% 
  mutate(pct_tot = case_when(
    pct_tot == 0 ~NA_real_,
    TRUE ~pct_tot)) %>% 
  mutate(metro = as.integer(metro),
         pop = as.integer(pop),
         pct_nhpi = round(pct_nhpi, digits = 2),
         pct_tot = round(pct_tot, digits = 2))


# group specific layers ---------------------------------------------------

#group 2
dta_nhpi2 <- dta %>% 
  filter(variable == "A002") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

#group 3
dta_nhpi3 <- dta %>% 
  filter(variable == "A003") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

#group 4
dta_nhpi4 <- dta %>% 
  filter(variable == "A004") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

#group 6
dta_nhpi6 <- dta %>% 
  filter(variable == "A006") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

#group 7
dta_nhpi7 <- dta %>% 
  filter(variable == "A007") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

#group 9
dta_nhpi9 <- dta %>% 
  filter(variable == "A009") %>%
  rename(pop2 = pop,
         Ap2 = pct_nhpi,
         Tp2 = pct_tot) %>% 
  left_join(top_nhpi_map) %>% 
  dplyr::select(-variable)

# getting tract-level shapefile -------------------------------------------
library(tigris)
options(tigris_use_cache = T)

library(rappdirs)
user_cache_dir()

library(GISTools)
library(rgdal)
# install.packages("rmapshaper")  # install if necessary
library(sf)
library(rmapshaper)
# install.packages("rgeos")
library(rgeos)
# devtools::install_github('walkerke/tigris')
# rm(combined, combined_generalized)


combined <- rbind_tigris(lapply(state_list, function(x){
  tracts(x, cb = TRUE, refresh = TRUE)}))

#getting city broundaries shapefile
cities <- rbind_tigris(lapply(state_list, function(x){
  places(x, cb = TRUE, refresh = TRUE)}))
city_generalized = ms_simplify(cities, keep = 0.05)
writeOGR(obj=city_generalized, dsn = "/Users/sunnyshao/Documents/census2020_planning/cit_nhpi", layer="cit_nhpi", driver="ESRI Shapefile") # this is in geographical projection

# combined_generalized <- gSimplify(combined, 0.4, topologyPreserve=TRUE)
combined_generalized = ms_simplify(combined, keep = 0.05)

# top NHPI in each metro area ------------------------------------
merge <- geo_join(combined_generalized, top_nhpi_map, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/topNHPI", layer="toppi", driver="ESRI Shapefile") # this is in geographical projection
##############

# NHPI in each metro area ------------------------------------

#group 2
merge <- geo_join(combined_generalized, dta_nhpi2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi02", layer="pi02", driver="ESRI Shapefile") # this is in geographical projection

#group 3
merge <- geo_join(combined_generalized, dta_nhpi3, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi03", layer="pi03", driver="ESRI Shapefile") # this is in geographical projection

#group 4
merge <- geo_join(combined_generalized, dta_nhpi4, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi04", layer="pi04", driver="ESRI Shapefile") # this is in geographical projection

#group 6
merge <- geo_join(combined_generalized, dta_nhpi6, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi06", layer="pi06", driver="ESRI Shapefile") # this is in geographical projection

#group 7
merge <- geo_join(combined_generalized, dta_nhpi7, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi07", layer="pi07", driver="ESRI Shapefile") # this is in geographical projection

#group 9
merge <- geo_join(combined_generalized, dta_nhpi9, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/pi09", layer="pi09", driver="ESRI Shapefile") # this is in geographical projection









