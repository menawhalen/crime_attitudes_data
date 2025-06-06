## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

### load data
file_names <- list.files(path = "cities/portland/old_data", pattern = ".csv")

dat_files <- map(file_names, ~ read_csv(str_c("cities/portland/old_data/", .x)) 
                 %>% clean_names)
### size = 550781
dups <- bind_rows(dat_files)  %>% 
  group_by(case_number) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(case_number)
## 24713 cases doubles
#### 550781
bind_rows(dat_files)  %>% 
  filter(case_number %in% dups) %>% 
  arrange(case_number) %>% view()
### case number isn't a good use since the offense are different
## same as binding them together 550781
dat <- bind_rows(dat_files) %>% 
  distinct()

######################################################
############# cleaning data
clean_dat <- dat %>%
  select(occur_date, offense_category, open_data_lat, open_data_lon) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = mdy(occur_date), # change to date
         ofns_desc = str_to_lower(offense_category)) %>%  # make to lower case
  select(-c(occur_date, offense_category)) %>%
  filter(date > ymd("2015-12-31")) %>% 
  filter(!is.na(open_data_lat) & !is.na(open_data_lon)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "motor vehicle") ~ "mvt",
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

############# zip codes
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

joined_zip <- st_join(st_as_sf(clean_dat, coords = c("open_data_lon", "open_data_lat"), crs = st_crs(zips)),
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
         city = "portland") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

######## explore and check

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

write_csv(final, "cities/portland/current.csv")
write_csv(dat, "cities/portland/full_uncleaned_current.csv")


final %>% 
  group_by(year, desc_crime_type) %>% 
  summarise(total = sum(count)) %>% 
  ggplot(aes(factor(year), total, group = desc_crime_type)) +
  geom_line() +
  facet_wrap(~desc_crime_type, scales = "free_y")
### missing sex crimes since no location data is available for those 
