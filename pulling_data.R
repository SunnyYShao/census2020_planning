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


devtools::install_github("walkerke/tidycensus", force = T)
# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)


# getting reference labels ------------------------------------------------
label <- load_variables(year = 2017, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% select(variable, label) %>% 
  filter(!variable %in% c("B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A"))
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"

# list of state and counties where top MSAs locate ------------------------
state_list <- c("CA", "CO", "PA", "NJ", "DE", "MD", "DC", "VA", 
                "WV", "FL", "MO", "IL", "IN", "WI", "MA", "NH", 
                "MI", "MN", "NY", "NC", "SC", "OR", "WA", "TX")
counties <- read_csv("ct_topmsa.csv")
top_ct <- as.vector(counties$GEOID)


# total pop at tract level ------------------------------------------------
tot_tract <- get_acs(table = "B01003", year = 2017, geography = "tract", state = state_list)
tot_tract <- tot_tract %>% 
  select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)

# pulling detailed AA tract-level data ------------------------------------
asn_tract <- get_acs(table = "B02015", year = 2017, geography = "tract", state = state_list, summary_var = "B02015_001")

dta <- asn_tract %>% 
  select(GEOID, NAME, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02015_001", "B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A")) %>% 
  left_join(tot_tract) %>% 
  mutate(ct_geoid = substr(GEOID, 1, 5)) %>% 
  filter(ct_geoid %in% top_ct) %>% 
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
  select(-ct_geoid, -asn_pop, -tot_pop)

top_asn_final <- dta %>% 
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
  select(GEOID, NAME, variable, top1, top2, top3, pop, pct_asn, pct_tot) %>% 
  left_join(label) %>% select(-variable)

top_asn <- top_asn_final %>% select(-pop, -pct_asn, -pct_tot)

# generating group specific df --------------------------------------------
dta <- dta %>% 
  mutate(poplb = scales::comma(pop)) %>% 
  mutate(pctlb1 = case_when(
    is.na(pct_asn)==F ~paste(as.character(pct_asn*100), "%", sep = ""),
    TRUE ~as.character(pct_asn))) %>% 
  mutate(pctlb2 = case_when(
    is.na(pct_tot)==F ~paste(as.character(pct_tot*100), "%", sep = ""),
    TRUE ~as.character(pct_tot)))

#group 2
dta_asn2 <- dta %>% 
  filter(variable == "A002") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot)
#group 3
dta_asn3 <- dta %>% 
  filter(variable == "A003") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop3 = pop,
         Ap3 = pct_asn,
         Tp3 = pct_tot)
#group 4
dta_asn4 <- dta %>% 
  filter(variable == "A004") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop4 = pop,
         Ap4 = pct_asn,
         Tp4 = pct_tot)
#group 5
dta_asn5 <- dta %>% 
  filter(variable == "A005") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop5 = pop,
         Ap5 = pct_asn,
         Tp5 = pct_tot)
#group 6
dta_asn6 <- dta %>% 
  filter(variable == "A006") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop6 = pop,
         Ap6 = pct_asn,
         Tp6 = pct_tot)
#group 7
dta_asn7 <- dta %>% 
  filter(variable == "A007") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop7 = pop,
         Ap7 = pct_asn,
         Tp7 = pct_tot)
#group 8
dta_asn2 <- dta %>% 
  filter(variable == "A008") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop8 = pop,
         Ap8 = pct_asn,
         Tp8 = pct_tot)
#group 9
dta_asn9 <- dta %>% 
  filter(variable == "A009") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop9 = pop,
         Ap9 = pct_asn,
         Tp9 = pct_tot)
#group 10
dta_asn10 <- dta %>% 
  filter(variable == "A010") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop10 = pop,
         Ap10 = pct_asn,
         Tp10 = pct_tot)
#group 11
dta_asn11 <- dta %>% 
  filter(variable == "A011") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop11 = pop,
         Ap11 = pct_asn,
         Tp11 = pct_tot)
#group 12
dta_asn12 <- dta %>% 
  filter(variable == "A012") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop12 = pop,
         Ap12 = pct_asn,
         Tp12 = pct_tot)
#group 13
dta_asn13 <- dta %>% 
  filter(variable == "A013") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop13 = pop,
         Ap13 = pct_asn,
         Tp13 = pct_tot)
#group 14
dta_asn14 <- dta %>% 
  filter(variable == "A014") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop14 = pop,
         Ap14 = pct_asn,
         Tp14 = pct_tot)
#group 15
dta_asn15 <- dta %>% 
  filter(variable == "A015") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop15 = pop,
         Ap15 = pct_asn,
         Tp15 = pct_tot)
#group 16
dta_asn16 <- dta %>% 
  filter(variable == "A016") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop16 = pop,
         Ap16 = pct_asn,
         Tp16 = pct_tot)

dta_asn2 <- dta %>% 
  filter(variable == "A002") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop2 = pop,
         Ap2 = pct_asn,
         Tp2 = pct_tot)
#group 17
dta_asn17 <- dta %>% 
  filter(variable == "A017") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop17 = pop,
         Ap17 = pct_asn,
         Tp17 = pct_tot)
#group 18
dta_asn18 <- dta %>% 
  filter(variable == "A018") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop18 = pop,
         Ap18 = pct_asn,
         Tp18 = pct_tot)
#group 19
dta_asn19 <- dta %>% 
  filter(variable == "A019") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop19 = pop,
         Ap19 = pct_asn,
         Tp19 = pct_tot)
#group 20
dta_asn20 <- dta %>% 
  filter(variable == "A020") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop20 = pop,
         Ap20 = pct_asn,
         Tp20 = pct_tot)
#group 21
dta_asn21 <- dta %>% 
  filter(variable == "A021") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop21 = pop,
         Ap21 = pct_asn,
         Tp21 = pct_tot)
#group 22
dta_asn22 <- dta %>% 
  filter(variable == "A022") %>% 
  left_join(top_asn) %>% 
  select(-variable) %>% 
  rename(pop22 = pop,
         Ap22 = pct_asn,
         Tp22 = pct_tot)


# getting tract-level shapefile -------------------------------------------
library(tigris)
library(GISTools)
library(rgdal)
# install.packages("rmapshaper")  # install if necessary
library("sf")
library("rmapshaper")
# install.packages("rgeos")
library(rgeos)

# rm(combined, combined_generalized)

combined <- rbind_tigris(lapply(state_list, function(x){
  tracts(x, cb = TRUE)}))
# combined_generalized <- gSimplify(combined, 0.4, topologyPreserve=TRUE)
combined_generalized = ms_simplify(combined, keep = 0.09)


#read in shapefile

merge <- geo_join(combined_generalized, dta_asn2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn="aa2", layer="aa2", driver="ESRI Shapefile") # this is in geographical projection



