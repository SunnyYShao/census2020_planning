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
state_list <- c("GA","WI","WA","VA","TX","PA","NY","NJ","NH","MN","MI","MD", "MA","IN","IL","FL","DE","DC","CO","CA", "AZ")
top_tract <- c("06001", "08001", "11001", "10003", "24003", "27003", "34003", "08005", 
               "24005", "34005", "36005", "34007", "24009", "25009", "12011", "04013", 
               "06013", "13013", "24013", "34013", "51013", "08014", "13015", "24015", 
               "33015", "34015", "48015", "24017", "25017", "33017", "34017", "42017", 
               "08019", "27019", "34019", "04021", "24021", "25021", "25023", "34023", 
               "24025", "25025", "27025", "34025", "24027", "34027", "34029", "42029", 
               "08031", "17031", "24031", "34031", "24033", "34033", "53033", "08035", 
               "13035", "24035", "34035", "06037", "17037", "27037", "34037", "54037", 
               "08039", "34039", "48039", "06041", "17043", "51043", "13045", "42045", 
               "08047", "36047", "51047", "12053", "27053", "53053", "12057", "13057", 
               "06059", "08059", "27059", "36059", "51059", "55059", "36061", "51061", 
               "53061", "13063", "17063", "06065", "13067", "06071", "48071", "06073", 
               "18073", "06075", "13077", "27079", "36079", "06081", "36081", "13085", 
               "36085", "48085", "12086", "26087", "36087", "13089", "17089", "18089", 
               "42091", "08093", "17093", "26093", "55093", "27095", "13097", "17097", 
               "12099", "26099", "12101", "42101", "12103", "36103", "42103", "51107", 
               "55109", "17111", "18111", "13113", "48113", "51113", "13117", "36119", 
               "13121", "48121", "27123", "26125", "18127", "13135", "27139", "48139", 
               "27141", "13143", "26147", "13149", "13151", "51153", "48157", "51157", 
               "13159", "26163", "27163", "48167", "13171", "27171", "51177", "51179", 
               "51187", "17197", "13199", "48201", "13211", "13217", "13223", "13227", 
               "13231", "48231", "13247", "48251", "13255", "48257", "48291", "13297", 
               "48339", "48367", "48397", "48439", "48473", "48497", "24510", "51510", 
               "51600", "51610", "51630", "51683", "51685")


# total pop at tract level ------------------------------------------------
tot_tract <- get_acs(table = "B01003", year = 2017, geography = "tract", state = state_list)
tot_tract <- tot_tract %>% 
  select(GEOID, NAME, estimate) %>% 
  rename(tot_pop = estimate)

# pulling detailed AA tract-level data ------------------------------------
asn_tract <- get_acs(table = "B02015", year = 2017, geography = "tract", state = state_list, summary_var = "B02015_001")
asn_tract <- asn_tract %>% 
  select(GEOID, NAME, variable, estimate, summary_est) %>% 
  filter(!variable %in% c("B02015_001", "B02015_023", "B02015_024", "B02015_025")) %>% 
  mutate(variable = str_replace_all(variable, "B02015_", "A")) %>% 
  left_join(tot_tract) %>% 
  mutate(ct_geoid = substr(GEOID, 1, 5)) %>% 
  filter(ct_geoid %in% top_tract) %>% 
  rename(pop = estimate,
         asn_pop = summary_est) %>% 
  mutate(pct_asn = round(pop / asn_pop, digits = 2),
         pct_tot = round(pop / tot_pop, digits = 2)) %>% 
  mutate(pct_asn = case_when(
    asn_pop == 0 ~NA_real_,
    TRUE ~pct_asn)) %>% 
  mutate(pct_tot = case_when(
    tot_pop == 0 ~NA_real_,
    TRUE ~pct_tot))

top_asn <- asn_tract %>% 
  group_by(GEOID) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1) %>% 
  mutate(population = scales::comma(pop))

dta_asn2 <- asn_tract %>% 
  select(-ct_geoid) %>% 
  filter(variable = A002)

dta_asn2 <- asn_tract %>% 
  select(-ct_geoid) %>% 
  filter(variable = A002)

dta_asn2 <- asn_tract %>% 
  select(-ct_geoid) %>% 
  filter(variable = A002)

            # data2 <- get_acs(table = "B01003", year = 2017, geography = "county", cache_table = T)

