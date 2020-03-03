# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)

state_list <- c(" AL", 
                " AK", 
                " AZ", 
                " AR", 
                " CA", 
                " CO", 
                " CT", 
                " DE", 
                " FL", 
                " GA", 
                " HI", 
                " ID", 
                " IL", 
                " IN", 
                " IA", 
                " KS", 
                " KY", 
                " LA", 
                " ME", 
                " MD", 
                " MA", 
                " MI", 
                " MN", 
                " MS", 
                " MO", 
                " MT", 
                " NE", 
                " NV", 
                " NH", 
                " NJ", 
                " NM", 
                " NY", 
                " NC", 
                " ND", 
                " OH", 
                " OK", 
                " OR", 
                " PA", 
                " RI", 
                " SC", 
                " SD", 
                " TN", 
                " TX", 
                " UT", 
                " VT", 
                " VA", 
                " WA", 
                " WV", 
                " WI", 
                " WY")

asn_tract <- get_acs(variables = "B02015_001", year = 2017, geography = "tract", 
                     state = state_list, cache_table = T)

asn_tract <- get_acs(variables = "B02015_001", year = 2017, geography = "tract", 
                     state = state_list, cache_table = T)
dta <- asn_tract %>% 
  dplyr::select(GEOID, NAME, variable, estimate)

dta2 <- read_csv("DEC_00_SF1_PCT005_with_ann.csv") %>% 
  dplyr::select("GEO.id2", "GEO.display-label", VD01) %>% 
  filter(VD01 != "Total:") %>% 
  mutate(VD01 = as.numeric(VD01)) %>% 
  rename(pop_2000 = VD01,
         GEOID = GEO.id2)

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


combined <- rbind_tigris(lapply(state_list, function(x){
  tracts(x, cb = TRUE, refresh = TRUE)}))

# combined_generalized <- gSimplify(combined, 0.4, topologyPreserve=TRUE)
combined_generalized = ms_simplify(combined, keep = 0.05)

merge <- geo_join(combined_generalized, dta, "GEOID", "GEOID")
writeOGR(obj=merge, dsn = "/Users/sunnyshao/Documents/census2020_planning/asn_tract", layer="asn_tract", driver="ESRI Shapefile") # this is in geographical projection

merge <- geo_join(combined_generalized, dta2, "GEOID", "GEOID")
writeOGR(obj=merge, dsn = "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/asn_tract00", layer="asn_tract00", driver="ESRI Shapefile") # this is in geographical projection
