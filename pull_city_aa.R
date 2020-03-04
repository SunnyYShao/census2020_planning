# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)

label <- load_variables(year = 2018, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_004", "B02015_005", "B02015_015", 
                          "B02015_016", "B02015_017", "B02015_023", 
                          "B02015_024", "B02015_025")) %>% 
  mutate(order = row_number(),
         var = paste("G", order, sep = "")) %>% 
  select(-order)
label[1,2] <- "Asian Am"
label[5,2] <- "Chinese"

label2 <- load_variables(year = 2010, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02006")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  dplyr::select(name, label) %>% 
  filter(!name %in% c("B02006_018", "B02006_019")) %>% 
  rename(variable = name) %>% 
  mutate(order = row_number(),
         var = paste("G", order, sep = "")) %>% 
  select(-order)
  
label2[1,2] <- "Asian Am"
label2[5,2] <- "Chinese"

label <- label %>% left_join(label2)
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

tot_city_2010 <- get_acs(table = "B01003", year = 2010, geography = "place", state = "CA", cache_table = T)
tot_city_2010 <- tot_city_2010 %>% 
  dplyr::select(GEOID, estimate) %>% 
  rename(tot_pop = estimate)


#asian pop at city level ------------------------------------
#2010 data
data <- get_acs(table = "B02006", year = 2010, geography = "place", state = "CA", summary_var = "B02006_001", cache_table = T)
dta_2010 <- data %>% 
  dplyr::select(GEOID, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02006_018", "B02006_019")) %>% 
  left_join(label2) %>% 
  left_join(tot_city) %>% 
  left_join(city_dta) %>% 
  filter(is.na(NAME)==F) %>% 
  rename(pop = estimate,
         asn_pop = summary_est) %>% 
  rename(est = pop,
         asn = asn_pop,
         tot = tot_pop) %>% 
  dplyr::select(GEOID, est, asn, tot, var, NAME) %>% 
  gather(key, value, -var, -NAME, -GEOID) %>% 
  mutate(label = paste(var, key, "10", sep = "")) %>% 
  select(GEOID, NAME, label, value) %>% 
  spread(label, value)

top_asn_2010 <- data %>% 
  dplyr::select(GEOID, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02006_018", "B02006_019")) %>% 
  left_join(label2) %>% 
  left_join(tot_city) %>% 
  left_join(city_dta) %>% 
  filter(is.na(NAME)==F,
         label != "Asian Am") %>% 
  rename(pop10 = estimate,
         asn_pop10 = summary_est,
         tot_pop10 = tot_pop) %>% 
  dplyr::select(GEOID, NAME, label, pop10, asn_pop10, tot_pop10) %>% 
  group_by(GEOID) %>% 
  arrange(desc(pop10)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank == 1) %>% 
  mutate(pct_asn10 = scales::percent(round((pop10 / asn_pop10), digits = 2)),
         pct_tot10 = scales::percent(round((pop10 / tot_pop10), digits = 2))) %>% 
  dplyr::select(-rank)
  
  
#2018 data
data2 <- get_acs(table = "B02015", year = 2018, geography = "place", state = "CA", summary_var = "B02015_001", cache_table = T)
dta_2018 <- data2 %>% 
  dplyr::select(GEOID, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02015_004", "B02015_005", "B02015_015", 
                          "B02015_016", "B02015_017", "B02015_023", 
                          "B02015_024", "B02015_025")) %>% 
left_join(label) %>% 
  left_join(tot_city) %>% 
  left_join(city_dta) %>% 
  filter(is.na(NAME)==F) %>% 
  rename(pop = estimate,
         asn_pop = summary_est) %>% 
  rename(est = pop,
         asn = asn_pop,
         tot = tot_pop) %>% 
  dplyr::select(GEOID, est, asn, tot, var, NAME) %>% 
  gather(key, value, -var, -NAME, -GEOID) %>% 
  mutate(label = paste(var, key, "18", sep = "")) %>% 
  select(GEOID, NAME, label, value) %>% 
  spread(label, value)

top_asn_2018 <- data2 %>% 
  dplyr::select(GEOID, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02015_004", "B02015_005", "B02015_015", 
                          "B02015_016", "B02015_017", "B02015_023", 
                          "B02015_024", "B02015_025")) %>% 
  left_join(label) %>% 
  left_join(tot_city) %>% 
  left_join(city_dta) %>% 
  filter(is.na(NAME)==F,
         label != "Asian Am") %>% 
  rename(pop18 = estimate,
         asn_pop18 = summary_est,
         tot_pop18 = tot_pop,
         label18 = label) %>% 
  dplyr::select(GEOID, NAME, label18, pop18, asn_pop18, tot_pop18) %>% 
  group_by(GEOID) %>% 
  arrange(desc(pop18)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank == 1) %>% 
  mutate(pct_asn18 = scales::percent(round((pop18 / asn_pop18), digits = 2)),
         pct_tot18 = scales::percent(round((pop18 / tot_pop18), digits = 2))) %>% 
  dplyr::select(-rank)

dta_final <- dta_2018 %>% left_join(dta_2010)
top_asn <- top_asn_2018 %>% left_join(top_asn_2010)

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
merge <- geo_join(city_shape, top_asn, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/topAA_1018", layer="topaa", driver="ESRI Shapefile") # this is in geographical projection







#archived -------------

#group 2
dta_asn2 <- dta %>% 
  filter(variable == "A002") %>%
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 3
dta_asn3 <- dta %>% 
  filter(variable == "A003") %>% 
  rename(pop3 = pop,
         Ap3 = pct_asn,
         Tp3 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 4
dta_asn4 <- dta %>% 
  filter(variable == "A004") %>% 
  rename(pop4 = pop,
         Ap4 = pct_asn,
         Tp4 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 5
dta_asn5 <- dta %>% 
  filter(variable == "A005") %>% 
  rename(pop5 = pop,
         Ap5 = pct_asn,
         Tp5 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 6
dta_asn6 <- dta %>% 
  filter(variable == "A006") %>% 
  rename(pop6 = pop,
         Ap6 = pct_asn,
         Tp6 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 7
dta_asn7 <- dta %>% 
  filter(variable == "A007") %>% 
  rename(pop7 = pop,
         Ap7 = pct_asn,
         Tp7 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 8
dta_asn8 <- dta %>% 
  filter(variable == "A008") %>% 
  rename(pop8 = pop,
         Ap8 = pct_asn,
         Tp8 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 9
dta_asn9 <- dta %>% 
  filter(variable == "A009") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 10
dta_asn10 <- dta %>% 
  filter(variable == "A010") %>% 
  rename(pop10 = pop,
         Ap10 = pct_asn,
         Tp10 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 11
dta_asn11 <- dta %>% 
  filter(variable == "A011") %>% 
  rename(pop11 = pop,
         Ap11 = pct_asn,
         Tp11 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 12
dta_asn12 <- dta %>% 
  filter(variable == "A012") %>% 
  left_join(top_asn) %>% 
  dplyr::select(-variable) %>% 
  rename(pop12 = pop,
         Ap12 = pct_asn,
         Tp12 = pct_tot)
#group 13
dta_asn13 <- dta %>% 
  filter(variable == "A013") %>% 
  rename(pop13 = pop,
         Ap13 = pct_asn,
         Tp13 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 14
dta_asn14 <- dta %>% 
  filter(variable == "A014") %>% 
  rename(pop14 = pop,
         Ap14 = pct_asn,
         Tp14 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 15
dta_asn15 <- dta %>% 
  filter(variable == "A015") %>% 
  rename(pop15 = pop,
         Ap15 = pct_asn,
         Tp15 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 16
dta_asn16 <- dta %>% 
  filter(variable == "A016") %>% 
  rename(pop16 = pop,
         Ap16 = pct_asn,
         Tp16 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)


#group 17
dta_asn17 <- dta %>% 
  filter(variable == "A017") %>% 
  rename(pop17 = pop,
         Ap17 = pct_asn,
         Tp17 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 18
dta_asn18 <- dta %>% 
  filter(variable == "A018") %>% 
  rename(pop18 = pop,
         Ap18 = pct_asn,
         Tp18 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 19
dta_asn19 <- dta %>% 
  filter(variable == "A019") %>% 
  rename(pop19 = pop,
         Ap19 = pct_asn,
         Tp19 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 20
dta_asn20 <- dta %>% 
  filter(variable == "A020") %>% 
  rename(pop20 = pop,
         Ap20 = pct_asn,
         Tp20 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 21
dta_asn21 <- dta %>% 
  filter(variable == "A021") %>% 
  left_join(top_asn) %>% 
  dplyr::select(-variable) %>% 
  rename(pop21 = pop,
         Ap21 = pct_asn,
         Tp21 = pct_tot)
#group 22
dta_asn22 <- dta %>% 
  filter(variable == "A022") %>% 
  rename(pop22 = pop,
         Ap22 = pct_asn,
         Tp22 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

merge <- geo_join(city_shape, top_asn, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/topAA", layer="topaa", driver="ESRI Shapefile") # this is in geographical projection
##############

#Asian Group 2 - Asian Indian
merge <- geo_join(city_shape, dta_asn2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa2", layer="aa2", driver="ESRI Shapefile") # this is in geographical projection

merge <- geo_join(city_shape, dta_asn3, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa3", layer = "aa3", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn4, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa4", layer = "aa4", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn5, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa5", layer = "aa5", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn6, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa6", layer = "aa6", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn7, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa7", layer = "aa7", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn8, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa8", layer = "aa8", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn9, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa9", layer = "aa9", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn10, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa10", layer = "aa10", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn11, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa11", layer = "aa11", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn12, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa12", layer = "aa12", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn13, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa13", layer = "aa13", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn14, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa14", layer = "aa14", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn15, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa15", layer = "aa15", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn16, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa16", layer = "aa16", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn17, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa17", layer = "aa17", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn18, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa18", layer = "aa18", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn19, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa19", layer = "aa19", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn20, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa20", layer = "aa20", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn21, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa21", layer = "aa21", driver = "ESRI Shapefile")

merge <- geo_join(city_shape, dta_asn22, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/sacramento_city/aa22", layer = "aa22", driver = "ESRI Shapefile")
