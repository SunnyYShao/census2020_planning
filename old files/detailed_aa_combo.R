# devtools::install_github("walkerke/tidycensus", force = T)
# load variables ----------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(stringr)

# getting reference labels ------------------------------------------------
label <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02018")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label)
  # filter(!variable %in% c("B02018_023", "B02018_024", "B02018_025"))
label[1,2] <- "Asian Am"
label[7,2] <- "Chinese"

data <- get_acs(table = "B02018", year = 2018, survey = "acs1", geography = "state", summary_var = "B02018_001", cache_table = T)

detailed_aa_combo <- data %>% 
  left_join(label) %>% 
  select(NAME, label, estimate) %>% 
  filter(is.na(estimate)==F) %>% 
  spread(NAME, estimate) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/detailed18_AA_combo1YR_state.csv")
  
data2 <- read_csv("/Users/sunnyshao/Downloads/CVAP_2014-2018_ACS_csv_files/County.csv")

ct_tot <- data2 %>% 
  separate(geoname, into = c("County", "State"), sep = ", ") %>% 
  filter(lntitle %in% c("Total")) %>% 
  select(State, County, lntitle, tot_est, cvap_est) %>% 
  select(-lntitle)


ct_cvap <- data2 %>% 
  separate(geoname, into = c("County", "State"), sep = ", ") %>% 
  filter(lntitle %in% c("Asian Alone", "Native Hawaiian or Other Pacific Islander Alone")) %>% 
  select(State, County, lntitle, tot_est, cvap_est) %>% 
  group_by(State, County) %>% 
  mutate(tot_est = sum(tot_est),
         cvap_est = sum(cvap_est)) %>% 
  select(-lntitle) %>% 
  rename(aapi_pop = tot_est, 
         aapi_cvap = cvap_est) %>% 
  unique() %>% 
  left_join(ct_tot) %>% 
  mutate(pct_aapi_pop = aapi_pop / tot_est,
         aapi_pop = aapi_pop,
         pct_aapi_cvap = aapi_cvap / cvap_est,
         aapi_cvap = aapi_cvap) %>% 
  select(State, County, pct_aapi_pop, aapi_pop, pct_aapi_cvap, aapi_cvap) %>% 
  group_by(State) %>% 
  arrange(desc(aapi_pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank <= 3) %>% 
  select(State, County, rank, pct_aapi_pop,
         aapi_pop, pct_aapi_cvap, aapi_cvap) %>% 
  rename(Rank = rank,
         `County Name` = County,
         `Percent AAPI Population in County` = pct_aapi_pop,
         `AAPI County Population` = aapi_pop,
         `Percent AAPI County CVAP` = pct_aapi_cvap,
         `AAPI County CVAP Population` = aapi_cvap) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/county_cvap_aapi.csv")


data <- get_acs(table = "B16005D", geography = "state", year = 2018, survey = "acs5")

lep <- data %>% 
  mutate(label = case_when(
    variable %in% c("B16005D_004","B16005D_009") ~"non_eng",
    variable %in% c("B16005D_006","B16005D_011") ~"lep",
    variable %in% c("B16005D_001") ~"total",
  TRUE ~NA_character_)) %>% 
  filter(is.na(label)==F) %>%
  group_by(GEOID, label) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% 
  select(-moe, -variable, -GEOID) %>% 
  unique() %>% 
  spread(label, estimate) %>% 
  mutate(pct_lep = lep / non_eng,
         pct_neng = non_eng / total) %>% 
  select(NAME, pct_lep, pct_neng) %>% 
  rename(`Percent of non-english at home that are LEP` = pct_lep,
         `Percent of Asians that Speak Language Other than English at home` = pct_neng) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/lep_state_18.csv")


data <- get_acs(variables = c("C27001D_004", "C27001D_007", "C27001D_010"), year = 2018, geography = "state", summary_var = "C27001D_001")

ins <- data %>%
  select(GEOID, NAME, estimate, summary_est) %>% 
  group_by(GEOID) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(pct_no_ins = estimate / summary_est,
         pop_no_ins = estimate) %>% 
  select(NAME, pct_no_ins, pop_no_ins) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/no_ins_AA18.csv")


pov_aa <- get_acs(variables = c("B17001D_002"), year = 2018, geography = "state", summary_var = "B17001D_001") %>% 
  select(GEOID, NAME, estimate, summary_est) %>% 
  mutate(pct_pov = estimate / summary_est,
         pop_pov = estimate) %>% 
  select(NAME, pct_pov, pop_pov) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/POV_AA18.csv")


pov_pi <- get_acs(variables = c("B17001E_002"), year = 2018, geography = "state", summary_var = "B17001D_001") %>% 
  select(GEOID, NAME, estimate, summary_est) %>% 
  mutate(pct_pov = estimate / summary_est,
         pop_pov = estimate) %>% 
  select(NAME, pct_pov, pop_pov) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/POV_NHPI18.csv")



label <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02019")) %>% filter(keep == "TRUE")%>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label)
label[1,2] <- "NHPI alone or in combo"
label[11, 2] <- "Other Pacific Islanders, not specified"

data <- get_acs(table = "B02019", year = 2018, survey = "acs5", geography = "state") %>% 
  filter(NAME == "Hawaii") %>% 
  left_join(label) %>% 
  select(label, estimate) %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/hawaii_nhpi_pop18.csv")



data2 <- read_csv("/Users/sunnyshao/Downloads/CVAP_2014-2018_ACS_csv_files/County.csv")

ct_tot <- data2 %>% 
  separate(geoname, into = c("County", "State"), sep = ", ") %>% 
  filter(lntitle %in% c("Total")) %>% 
  select(State, County, lntitle, tot_est, cvap_est) %>% 
  select(-lntitle)


ct_cvap <- data2 %>% 
  separate(geoname, into = c("County", "State"), sep = ", ") %>% 
  filter(lntitle %in% c("Native Hawaiian or Other Pacific Islander Alone")) %>% 
  select(State, County, lntitle, tot_est, cvap_est) %>% 
  group_by(State, County) %>% 
  mutate(tot_est = sum(tot_est),
         cvap_est = sum(cvap_est)) %>% 
  select(-lntitle) %>% 
  rename(nhpi_pop = tot_est, 
         nhpi_cvap = cvap_est) %>% 
  unique() %>% 
  left_join(ct_tot) %>% 
  mutate(pct_nhpi_pop = nhpi_pop / tot_est,
         nhpi_pop = nhpi_pop,
         pct_nhpi_cvap = nhpi_cvap / cvap_est,
         nhpi_cvap = nhpi_cvap) %>% 
  select(State, County, pct_nhpi_pop, nhpi_pop, pct_nhpi_cvap, nhpi_cvap, tot_est, cvap_est) %>% 
  group_by(State) %>% 
  arrange(desc(nhpi_pop)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  filter(rank <= 5) %>% 
  select(State, County, rank, nhpi_pop, tot_est,
         pct_nhpi_pop, nhpi_cvap, cvap_est, pct_nhpi_cvap) %>% 
  rename(Rank = rank,
         `County Name` = County,
         `Size of NHPI Population` = nhpi_pop,
         `Total pop` = tot_est,
         `Percent NHPI Population in County` = pct_nhpi_pop,
         `NHPI County CVAP Population` = nhpi_cvap,
         `Total CVAP` = cvap_est,
         `Percent NHPI County CVAP` = pct_nhpi_cvap) %>% 
  filter(State == "Hawaii") %>% 
  write_csv("/Users/sunnyshao/Dropbox/AAPI DATA Desktop/census planning/county_cvap_nhpi2018.csv")





