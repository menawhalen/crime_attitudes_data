## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

## read in data
dat25 <- read_csv("cities/nyc/old_data/crime_pull25.csv") %>% clean_names()

dat23 <- read_csv("cities/nyc/old_data/newYork_crimeOLD.csv") %>% clean_names()
### really has 8 mil rows so probs it
dat24 <- read_csv("cities/nyc/old_data/newYork_crime_historic.csv") %>% clean_names()
###############
summary(mdy(dat23$cmplnt_fr_dt))## only part of 2023 to june
summary(mdy(dat24$cmplnt_fr_dt))## ends at the end of 2022
summary(mdy(dat25$cmplnt_fr_dt))### has mostly 2025 till march but not a lot of 2023-2024
#### combining dataset and remove duplicates

# names(dat23) %in% names(dat24)

dat_list <- list(dat23, dat24, dat25) %>% map( ~ .x %>% 
  select(cmplnt_num, cmplnt_fr_dt, ofns_desc, rpt_dt, latitude, longitude))
dat_list[[3]]$cmplnt_num <- as.character(dat_list[[3]]$cmplnt_num)
bind_rows(dat_list) %>% 
  pull(cmplnt_num) %>% unique() %>% length()
8768018
## only a thousand-ish duplicated list
## total 8769183
dat <- bind_rows(dat_list)
## but distinct only say 1 is repeated
dat %>% 
  distinct()

dups_cmplnt <- dat %>% 
  group_by(cmplnt_num) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(cmplnt_num)
## all of these are older from early 2000's
doubles <- dat %>% 
  filter(cmplnt_num %in% dups_cmplnt) %>% 
  arrange(cmplnt_num)
map_df(1:1104*2, ~ doubles[.x,])
### join the two together
dat <- dat %>% 
  filter(!(cmplnt_num %in% dups_cmplnt)) %>% ## all those that aren't duplicated
  bind_rows(map_df(1:1104*2, ~ doubles[.x,])) ## first row of the duplicates

## from 8769684 to 8768580
###########################
clean_dat <- dat %>%
  select(cmplnt_fr_dt, ofns_desc, latitude, longitude) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = mdy(cmplnt_fr_dt), # change to date
         ofns_desc = str_to_lower(ofns_desc)) %>%  # make to lower case
  select(-cmplnt_fr_dt) %>%
  filter(date > ymd("2015-12-31")) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "sex|rape") ~ "sex crimes",
    str_detect(ofns_desc, "robbery") ~ "robbery",
    str_detect(ofns_desc, "assault") ~ "assault",
    str_detect(ofns_desc, "larceny|theft") ~ "theft",
    str_detect(ofns_desc, "burglary") ~ "burglary",
    str_detect(ofns_desc, "arson") ~ "arson",
    str_detect(ofns_desc, "harrassment") ~ "harrassment",
    str_detect(ofns_desc, "motor vehicle") ~ "harrassment",
    str_detect(ofns_desc, "weapon") ~ "weapon",
    str_detect(ofns_desc, "drug|narcotic") ~ "drugs",
    .default = "other"
  ))
### down to 3.6 million post 2016

## do not use again only for the first time to get geoms
# library(tidycensus)
# census_api_key("aa5ff0c4c7a7ea56db3bafe255b0ee1c6733786e")
# 
# acs_zip_totals16_19 <- 2016:2019 %>%
#   map(\(x) get_acs(geography = "zcta",
#                    year = x,
#                    survey = "acs5",
#                    variables = "B01003_001", geometry = TRUE))
# names(acs_zip_totals16_19) <- 2016:2019
# acs_zip_totals20_21 <- 2020:2021 %>%
#   map(\(x) get_acs(geography = "zcta",
#                    year = x,
#                    survey = "acs5",
#                    variables = "B01003_001", geometry = TRUE))
# names(acs_zip_totals20_21) <- 2020:2021
# 
# acs_zip_totals22_23 <- 2022:2023 %>%
#   map(\(x) get_acs(geography = "zcta",
#                    year = x,
#                    survey = "acs5",
#                    variables = "B01003_001"))
# names(acs_zip_totals22_23) <- 2022:2023
# 
# 
# acs_zip_totals16_19 %>%
#   bind_rows(.id = "year") %>%
#   bind_rows(acs_zip_totals20_21 %>%
#               bind_rows(.id = "year")) %>% 
#   ### include 22 and 23 with zip geoms from 2021
#   bind_rows(acs_zip_totals22_23 %>% 
#               bind_rows(.id = "year") %>% 
#               left_join(select(acs_zip_totals20_21$`2021`,GEOID, NAME, variable, geometry), 
#                         by = join_by(GEOID, NAME, variable))) %>% 
#   clean_names() %>% 
#   write_rds("us_zctas_2016_2023.rds")

### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

joined_zip <- st_join(st_as_sf(clean_dat, coords = c("longitude", "latitude"), crs = st_crs(zips)),
                 select(zips, year, geoid, estimate))

zip_crime <- joined_zip %>% 
  tibble() %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% ## assumes pop after 2023 is same as 2023 and actually comes from same 2021 shapefiles in larger dataset
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, geoid, estimate)


final <- zip_crime %>% 
  rename(zcta = geoid, population = estimate) %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date)) %>% 
  group_by(month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

tibble(final) %>% 
  mutate(zcta = as.numeric(zcta)) %>% 
  group_by(year, zcta) %>% 
  reframe(total = sum(count)) %>% 
  pivot_wider(id_cols = zcta, names_from = year, values_from = total) %>% view()

write_csv(final, "cities/nyc/current.csv")
### the year of 2024 is missing and too low for numbers across all zip codes
write_csv(dat, "cities/nyc/full_uncleaned_current.csv")

clean_dat %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(factor(year))) +
  geom_bar(stat = "count")
## number are way to small for 2023 onward
dat %>% 
  mutate(year = year(mdy(cmplnt_fr_dt))) %>%
  filter(year > 2016) %>% 
  ggplot(aes(factor(year))) +
  geom_bar(stat = "count")
## looks similar in all data

## need more from 2023