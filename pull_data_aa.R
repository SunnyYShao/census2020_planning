library(stringr)
library(tigris)
library(leaflet)
library(GISTools)
library(rgdal)
library(maptools)
library(tidycensus)
# install.packages("geojsonio")
library(geojsonio)
# install.packages("geojsonlint")
library(geojsonlint)
# install.packages("rmapshaper")
library(rmapshaper)
library(mapdeck)
library(tidyverse)
# devtools::install_github("rstudio/leaflet.mapboxgl")
library(leaflet.mapboxgl)
options(mapbox.accessToken = key)


# devtools::install_github("walkerke/tidycensus", force = T)
# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)


# getting reference labels ------------------------------------------------
label <- load_variables(year = 2018, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A"))
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"

# list of state and counties where top MSAs locate ------------------------
state_list <- c("CA", "CO", "PA", "NJ", "DE", "MD", "DC", "VA", 
                "WV", "FL", "MO", "IL", "IN", "WI", "MA", "NH", 
                "MI", "MN", "NY", "NC", "SC", "OR", "WA", "TX", 
                "AZ", "HI", "AK", "AR", "UT", "GA", "NV")

counties <- read_csv("top_metro_ct.csv")

metro_list <- counties %>% 
  dplyr::select(NAMELSAD) %>% 
  unique() %>% 
  mutate(metro = row_number())

# metro_geo <- read_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/topmetro_geoid.csv") %>% 
#   left_join(metro_list)
# 
# write_csv(metro_geo, "metro_geo.csv", na = "")

counties_slim <- counties %>% 
  dplyr::select(-NAMELSAD_2) %>% 
  rename(ct_geoid = GEOID_2,
         asnP = top_metro_asnP,
         metroP = top_metro_metroP,
         nhpiP = top_metro_nhpiP) %>% 
  left_join(metro_list) %>% 
  dplyr::select(-NAMELSAD)

top_ct <- as.vector(counties_slim$ct_geoid)


# total pop at tract level ------------------------------------------------
tot_tract <- get_acs(table = "B01003", year = 2018, geography = "tract", state = state_list, cache_table = T)
tot_tract <- tot_tract %>% 
  dplyr::select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)

# pulling detailed AA tract-level data ------------------------------------
asn_tract <- get_acs(table = "B02015", year = 2018, geography = "tract", state = state_list, summary_var = "B02015_001", cache_table = T)
dta <- asn_tract %>% 
  dplyr::select(GEOID, NAME, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02015_001", "B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A")) %>% 
  left_join(tot_tract) %>% 
  mutate(ct_geoid = substr(GEOID, 1, 5)) %>% 
  filter(ct_geoid %in% top_ct) %>% 
  left_join(counties_slim) %>% 
  # dplyr::select(-ct_geoid) %>% 
  rename(pop = estimate,
         asn_pop = summary_est) %>% 
  mutate(pct_asn = round(pop / asn_pop, digits = 2),
         pct_tot = round(pop / tot_pop, digits = 2)) %>% 
  mutate(pct_asn = case_when(
    asn_pop == 0 ~NA_real_,
    TRUE ~pct_asn)) %>% 
  mutate(pct_tot = case_when(
    tot_pop == 0 ~NA_real_,
    TRUE ~pct_tot)) %>% 
  dplyr::select(-ct_geoid)

write_csv(dta, "dta_aa.csv", na = "")

#######
top_asn <- dta %>% 
  group_by(GEOID) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1) %>% 
  mutate(top1 = scales::comma(pop),
         top2 = case_when(
           is.na(pct_asn) == F ~paste(as.character(pct_asn*100), "%", sep = ""),
           TRUE ~as.character(pct_asn))) %>% 
  mutate(top3 = case_when(
           is.na(pct_tot) == F ~paste(as.character(pct_tot*100), "%", sep = ""),
           TRUE ~as.character(pct_tot))) %>% 
  # dplyr::select(GEOID, NAME, variable, top1, top2, top3, pop, pct_asn, pct_tot, metro) %>% 
  left_join(label) %>% dplyr::select(-variable)

top_asn_map <- top_asn %>% 
  dplyr::select(GEOID, NAME, label, top1, top2, top3, pop, pct_asn, pct_tot, metro) %>% 
  write_csv("Top_Asians.csv", na = "")

# generating group specific df --------------------------------------------
dta <- dta %>% 
  mutate(poplb = scales::comma(pop)) %>% 
  mutate(pctlb1 = case_when(
    is.na(pct_asn)==F ~paste(as.character(pct_asn*100), "%", sep = ""),
    TRUE ~as.character(pct_asn))) %>% 
  mutate(pctlb2 = case_when(
    is.na(pct_tot)==F ~paste(as.character(pct_tot*100), "%", sep = ""),
    TRUE ~as.character(pct_tot))) %>% 
  dplyr::select(-asn_pop, -tot_pop, -metroP, -asnP, -nhpiP)

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
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 4
dta_asn4 <- dta %>% 
  filter(variable == "A004") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 5
dta_asn5 <- dta %>% 
  filter(variable == "A005") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 6
dta_asn6 <- dta %>% 
  filter(variable == "A006") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)
#group 7
dta_asn7 <- dta %>% 
  filter(variable == "A007") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 8
dta_asn8 <- dta %>% 
  filter(variable == "A008") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
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
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 11
dta_asn11 <- dta %>% 
  filter(variable == "A011") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 12
dta_asn12 <- dta %>% 
  filter(variable == "A012") %>% 
  left_join(top_asn) %>% 
  dplyr::select(-variable) %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot)
#group 13
dta_asn13 <- dta %>% 
  filter(variable == "A013") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 14
dta_asn14 <- dta %>% 
  filter(variable == "A014") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 15
dta_asn15 <- dta %>% 
  filter(variable == "A015") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 16
dta_asn16 <- dta %>% 
  filter(variable == "A016") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)


#group 17
dta_asn17 <- dta %>% 
  filter(variable == "A017") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 18
dta_asn18 <- dta %>% 
  filter(variable == "A018") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 19
dta_asn19 <- dta %>% 
  filter(variable == "A019") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 20
dta_asn20 <- dta %>% 
  filter(variable == "A020") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
  dplyr::select(-variable)

#group 21
dta_asn21 <- dta %>% 
  filter(variable == "A021") %>% 
  left_join(top_asn) %>% 
  dplyr::select(-variable) %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot)
#group 22
dta_asn22 <- dta %>% 
  filter(variable == "A022") %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot) %>% 
  left_join(top_asn_map) %>% 
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

# combined_generalized <- gSimplify(combined, 0.4, topologyPreserve=TRUE)
combined_generalized = ms_simplify(combined, keep = 0.05)

#writeOGR(obj=combined_generalized, dsn = "/Users/sunnyshao/Documents/census2020_planning", layer="test", driver="ESRI Shapefile") # this is in geographical projection


#getting city broundaries shapefile
cities <- rbind_tigris(lapply(state_list, function(x){
  places(x, cb = TRUE, refresh = TRUE)}))

# combined_generalized <- gSimplify(combined, 0.4, topologyPreserve=TRUE)
city_generalized = ms_simplify(cities, keep = 0.05)
writeOGR(obj=city_generalized, dsn = "/Users/sunnyshao/Documents/census2020_planning/cit_nhpi", layer="cit", driver="ESRI Shapefile") # this is in geographical projection

##############

#read in shapefile
final_pop <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("P", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, pop) %>% 
  spread(group, pop)

final_lb1 <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("lb1", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, poplb) %>% 
  spread(group, poplb)

final_pctaa <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("AP", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, pct_asn) %>% 
  spread(group, pct_asn)

final_lb2 <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("lb2", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, pctlb1) %>% 
  spread(group, pctlb1)

final_pct <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("TP", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, pct_tot) %>% 
  spread(group, pct_tot)

final_lb3 <- dta %>% 
  filter(metro == 10) %>% 
  mutate(group = paste("lb3", variable, sep = "")) %>% 
  dplyr::select(GEOID, NAME, group, pctlb2) %>% 
  spread(group, pctlb2)

final <- final_pop %>% 
  left_join(final_pctaa) %>% 
  left_join(final_pct) %>% 
  left_join(final_lb1) %>% 
  left_join(final_lb2) %>% 
  left_join(final_lb3) %>% 
  left_join(top_asn_map)

merge <- geo_join(tract_shape, final, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/nv", layer="nv", driver="ESRI Shapefile") # this is in geographical projection


#read in selected metro map at census tract level
tract_shape <- readOGR(dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa_metro", layer = "aa_metro")


#top Asian Am in each metro area
merge <- geo_join(tract_shape, top_asn, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/topAA", layer="topaa", driver="ESRI Shapefile") # this is in geographical projection
##############

#Asian Group 2 - Asian Indian
merge <- geo_join(tract_shape, dta_asn2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa2", layer="aa2", driver="ESRI Shapefile") # this is in geographical projection


merge <- geo_join(tract_shape, top_asn, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/topasn", layer="topaa", driver="ESRI Shapefile") # this is in geographical projection


merge <- geo_join(tract_shape, dta_asn3, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa3", layer = "aa3", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn4, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa4", layer = "aa4", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn5, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa5", layer = "aa5", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn6, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa6", layer = "aa6", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn7, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa7", layer = "aa7", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn8, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa8", layer = "aa8", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn9, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa9", layer = "aa9", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn10, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa10", layer = "aa10", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn11, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa11", layer = "aa11", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn12, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa12", layer = "aa12", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn13, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa13", layer = "aa13", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn14, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa14", layer = "aa14", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn15, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa15", layer = "aa15", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn16, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa16", layer = "aa16", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn17, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa17", layer = "aa17", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn18, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa18", layer = "aa18", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn19, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa19", layer = "aa19", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn20, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa20", layer = "aa20", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn21, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa21", layer = "aa21", driver = "ESRI Shapefile")

merge <- geo_join(tract_shape, dta_asn22, "GEOID", "GEOID")
writeOGR(obj = merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/2018 acs data/aa22", layer = "aa22", driver = "ESRI Shapefile")

rm(dta_asn2, dta_asn3, dta_asn4, dta_asn5, dta_asn6,
   dta_asn7, dta_asn8, dta_asn9, dta_asn10, dta_asn11,
   dta_asn12, dta_asn13, dta_asn14, dta_asn15, dta_asn16,
   dta_asn17, dta_asn18, dta_asn19, dta_asn20, dta_asn21,
   dta_asn22, combined_generalized, combined, merge)

#
sum <- dta %>% 
  filter(metro == 19) %>% 
  dplyr::select(GEOID, NAME, variable, pop, asn_pop, tot_pop, metro) %>% 
  group_by(variable) %>% 
  mutate(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(variable) %>% 
  mutate(tot_pop = sum(tot_pop),
         asn_pop = sum(asn_pop)) %>% 
  ungroup() %>% 
  dplyr::select(metro, variable, pop, tot_pop, asn_pop) %>% 
  unique() %>% 
  mutate(scales::percent(pop / asn_pop))
  
#save large files
write_csv(tot_tract, "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/tot_tract.csv", na= "")
write_csv(asn_tract, "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/asn_tract.csv", na = "")

stats <- top_asn %>% 
  filter(metro == "17")
