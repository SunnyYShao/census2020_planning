# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)

label <- load_variables(year = 2018, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A"))
label[1,2] <- "NHPI"

# list of state and counties where top MSAs locate ------------------------
# state_list <- c("CA", "CO", "PA", "NJ", "DE", "MD", "DC", "VA", 
#                 "WV", "FL", "MO", "IL", "IN", "WI", "MA", "NH", 
#                 "MI", "MN", "NY", "NC", "SC", "OR", "WA", "TX", 
#                 "AZ", "HI", "AK", "AR", "UT", "GA", "NV")

#getting city-level list within sacramento metro ready ---------
CA_cities <- places(state = "CA", cb = TRUE, refresh = TRUE)
city_generalized = ms_simplify(CA_cities, keep = 0.05)
# writeOGR(obj=city_generalized, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/city_CA", layer="citCA", driver="ESRI Shapefile") # this is in geographical projection
Sacramento <- core_based_statistical_areas(resolution = "500k", cb = TRUE, refresh = TRUE)
Sacramento_generalized <- ms_simplify(Sacramento, keep = 0.05)
# writeOGR(obj=Sacramento_generalized, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/CA_metro", layer="metroCA", driver="ESRI Shapefile") # this is in geographical projection
library(sf)
library(rgdal)
city_shape <- readOGR(dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/sacramento city list", layer = "cit_sa")
city_dta <- read_sf(dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/sacramento city list", layer = "cit_sa")
city_dta <- city_dta %>% 
  as.data.frame() %>% 
  dplyr::select(GEOID, NAME, NAME_2, -geometry)

# total pop at city level ------------------------------------------------
tot_city <- get_acs(table = "B01003", year = 2018, geography = "place", state = "CA", cache_table = T)
tot_city <- tot_city %>% 
  dplyr::select(GEOID, estimate) %>% 
  rename(tot_pop = estimate)
#asian pop at city level ------------------------------------
dta <- get_acs(table = "B02016", year = 2018, geography = "place", state = "CA", summary_var = "B02016_001", cache_table = T)
dta <- dta %>% 
  dplyr::select(GEOID, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02016_001", "B02016_005", "B02016_008", "B02016_010", "B02016_011", "B02016_012")) %>% 
  mutate(variable = str_replace_all(variable, "B02016_", "A")) %>% 
  left_join(tot_city) %>% 
  left_join(city_dta) %>% 
  filter(is.na(NAME)==F) %>% 
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

merge <- geo_join(city_shape, top_nhpi, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/topNHPI", layer="toppi", driver="ESRI Shapefile") # this is in geographical projection
##############

#group 2
merge <- geo_join(city_shape, dta_nhpi2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi02", layer="pi02", driver="ESRI Shapefile") # this is in geographical projection

#group 3
merge <- geo_join(city_shape, dta_nhpi3, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi03", layer="pi03", driver="ESRI Shapefile") # this is in geographical projection

#group 4
merge <- geo_join(city_shape, dta_nhpi4, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi04", layer="pi04", driver="ESRI Shapefile") # this is in geographical projection

#group 6
merge <- geo_join(city_shape, dta_nhpi6, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi06", layer="pi06", driver="ESRI Shapefile") # this is in geographical projection

#group 7
merge <- geo_join(city_shape, dta_nhpi7, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi07", layer="pi07", driver="ESRI Shapefile") # this is in geographical projection

#group 9
merge <- geo_join(city_shape, dta_nhpi9, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/pi09", layer="pi09", driver="ESRI Shapefile") # this is in geographical projection

