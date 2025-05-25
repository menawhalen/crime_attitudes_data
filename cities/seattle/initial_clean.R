## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data
dat <- read_csv("cities/seattle/old_data/SPD_Crime_Data__2008-Present_20250523.csv") %>% 
  clean_names()

####################
#### duplicates
## only one file

############# cleaning data
clean_dat <- dat %>%
  select(offense_date, nibrs_offense_code_description, latitude, longitude) %>% ##
  mutate(date = mdy_hms(offense_date), # change to date
         ofns_desc = str_to_lower(nibrs_offense_code_description)) %>%  # make to lower case
  select(-c(offense_date, nibrs_offense_code_description)) %>%
  filter(date > ymd_hms("2015-12-31 23:59:59")) %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "vehicle theft") ~ "mvt",
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
## now 800K

summary(mdy_hms(dat$offense_date)) 
## but dates go back to 2003 
############### zips
zips <- read_rds("us_zctas_2016_2023.rds")

clean_dat <- clean_dat %>% 
  filter(!(latitude %in% c("REDACTED", "-1.0")))
## 140,445 thousand

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
         year = year(date),
         city = "seattle") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/seattle/current.csv")
write_csv(dat, "cities/seattle/full_uncleaned_current.csv")

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










