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



library(tidycensus)
library(tidyverse)
library(stringr)


# getting reference labels ------------------------------------------------
label <- load_variables(year = 2017, dataset = "acs5", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% select(variable, label)
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"
state_list <- c("NY", "CA", "IL", "TX", "DC", "FL", "PA", "GA", "MA", "AZ", "MI", "WA", "MN", "CO")
asn_tract <- get_acs(table = "B02015", year = 2017, geography = "tract", state = state_list)
data2 <- get_acs(table = "B01003", year = 2017, geography = "county", cache_table = T)

