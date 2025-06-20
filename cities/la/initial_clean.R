## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

file_names <- list.files(path = "cities/la/old_data", pattern = ".csv")

dat_files <- map(file_names, ~ read_csv(str_c("cities/la/old_data/", .x), col_types = cols(DR_NO = col_double())) 
                 %>% clean_names)
### combine all files
dat <- bind_rows(dat_files)

######################################################
############# cleaning data
clean_dat <- dat %>%
  select(date_occ, crm_cd_desc, lat, lon) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = mdy_hms(date_occ), # change to date
         ofns_desc = str_to_lower(crm_cd_desc)) %>%  # make to lower case
  select(-c(date_occ, crm_cd_desc)) %>%
  filter(date > ymd_hms("2015-12-31 23:59:59")) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "vehicle - stolen") ~ "mvt",
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
## almost 2 million but goes back from 2010


## issues with post 2024 so remove and replace with 
repeat_year <- clean_dat %>% 
  filter(date <= ymd("2024-02-29") & date >= ymd("2023-03-01")) %>% 
  mutate(date = date %m+% years(1)) 


clean_dat <- clean_dat %>% 
  filter(date < ymd("2024-02-29")) %>% ## remove after march 2024
  bind_rows(repeat_year) ## repeat march 23-24

############# zip codes
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

joined_zip <- st_join(st_as_sf(clean_dat, coords = c("lon", "lat"), crs = st_crs(zips)),
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
         city = "la") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/la/current.csv")
write_csv(dat, "cities/la/full_uncleaned_current.csv")

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
