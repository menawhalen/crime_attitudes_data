## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data
dat <- read_csv("cities/charlotte/old_data/CMPD_Incidents.csv") %>% 
  clean_names()
######################################################
############# cleaning data
clean_dat <- dat %>%
  select(date_incident_began, highest_nibrs_description, zip, latitude_public, longitude_public) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = ymd_hms(date_incident_began), # change to date
         ofns_desc = str_to_lower(highest_nibrs_description)) %>%  # make to lower case
  select(-c(date_incident_began, highest_nibrs_description)) %>%
  filter(date > ymd_hms("2015-12-31 23:59:59")) %>% 
  filter(!is.na(latitude_public) & !is.na(longitude_public)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "motor vehicle theft") ~ "mvt",
    str_detect(ofns_desc, "sex|rape") ~ "sex crimes",
    str_detect(ofns_desc, "robbery") ~ "robbery",
    str_detect(ofns_desc, "assault") ~ "assault",
    str_detect(ofns_desc, "larceny|theft") ~ "theft",
    str_detect(ofns_desc, "burglary") ~ "burglary",
    str_detect(ofns_desc, "arson") ~ "arson",
    #str_detect(ofns_desc, "harrassment") ~ "harrassment",
    str_detect(ofns_desc, "weapon") ~ "weapon",
    str_detect(ofns_desc, "drug|narcotic") ~ "drugs",
    .default = "other"
  ))
## loosing about 2,000


############# zip codes
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

missing_zip <- clean_dat %>% 
  filter(is.na(zip)) %>% 
  st_as_sf(coords = c("longitude_public", "latitude_public"), crs = st_crs(zips)) %>% 
  st_join(select(zips, year, geoid, estimate)) %>% 
  tibble() %>% 
  select(-c(zip, geometry)) %>% 
  rename(zip = geoid)
  
matched_zip <- clean_dat %>% 
  filter(!is.na(zip)) %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(zips, by = c("zip" = "geoid")) %>% 
  select(date, ofns_desc, desc_crime_type, year, zip, estimate)


zip_crime <- bind_rows(missing_zip, matched_zip) %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% ## assumes pop after 2023 is same as 2023 and actually comes from same 2021 shapefiles in larger dataset
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, zip, estimate)


final <- zip_crime %>% 
  rename(zcta = zip, population = estimate) %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date),
         city = "charlotte") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

### 2016 is too small and no consistently reported here

final <- final %>% 
  filter(year != 2016)

write_csv(final, "cities/charlotte/current.csv")
write_csv(dat, "cities/charlotte/full_uncleaned_current.csv")

clean_dat %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(factor(year))) +
  geom_bar(stat = "count")

tibble(final) %>% 
  mutate(zcta = as.numeric(zcta)) %>% 
  group_by(year, zcta) %>% 
  reframe(total = sum(count)) %>% 
  pivot_wider(id_cols = zcta, names_from = year, values_from = total) %>% view()

summary(clean_dat$date)

final %>% 
  group_by(year, desc_crime_type) %>% 
  summarise(total = sum(count)) %>% 
  ggplot(aes(factor(year), total, group = desc_crime_type)) +
  geom_line() +
  facet_wrap(~desc_crime_type, scales = "free_y")
