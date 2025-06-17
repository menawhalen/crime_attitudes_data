## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data

file_names <- list.files(path = "cities/san_francisco/old_data", pattern = ".csv")

dat_files <- map(file_names[1:2], ~ read_csv(str_c("cities/san_francisco/old_data/", .x)) 
                 %>% clean_names)
####################
#### duplicates

names(dat_files[[2]]) ## before 2018
names(dat_files[[1]])  ## 

### before goes to may 2018 after starts at 2018 jan

old_2018<- dat_files[[2]] %>% 
  mutate(date = mdy(date)) %>% 
  filter(date > ymd("2017-12-31"))

new_2018 <- dat_files[[1]] %>% 
  filter(incident_date < ymd("2018-05-16") & incident_date > ymd("2017-12-31"))

new_2018 %>% 
  filter(incident_number %in% unique(old_2018$incidnt_num))



dat <- dat_files[[2]] %>% 
  select(incidnt_num, date, category, descript, data_loaded_at, x, y) %>% 
  rename(incident_number = incidnt_num, incident_date = date, incident_category = category, incident_description = descript, latitude = y, longitude = x) %>% 
  mutate(incident_date = mdy(incident_date)) %>%
  filter(incident_date < ymd("2017-12-31")) %>% ## remove older data from 2018. (more is in new folder)
  bind_rows(bind_rows(dat_files[[1]]) %>% 
  select(incident_number, incident_date, incident_category, incident_description, data_loaded_at, latitude, longitude) %>% 
  mutate(incident_number = as.character(incident_number)))


dups <- dat %>% 
  group_by(incident_number) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(incident_number)

doubles <- dat %>% 
  filter(incident_number %in% dups) %>% 
  arrange(incident_number)
## incident can have multiple crime descriptions
### original 3,847,165
dat <- dat %>% 
  distinct()
## distinct rows 3,243,516
### so removes 600K+ rows


############# cleaning data
clean_dat <- dat %>%
  select(incident_date, incident_category, latitude, longitude) %>% ##
  mutate(date = ymd(incident_date), # change to date
         ofns_desc = str_to_lower(incident_category)) %>%  # make to lower case
  select(-c(incident_date, incident_category)) %>%
  filter(date > ymd("2015-12-31")) %>% 
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
## now 1.4 mil

summary(ymd(dat$incident_date)) 
## but dates go back to 2003 
############### zips
zips <- read_rds("us_zctas_2016_2023.rds")

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
         city = "san_francisco") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/san_francisco/current.csv")
write_csv(dat, "cities/san_francisco/full_uncleaned_current.csv")

clean_dat %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(factor(year))) +
  geom_bar(stat = "count")


final %>% 
  group_by(year) %>% 
  summarise(total = sum(count))



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



dat %>% 
  mutate(date = ymd(incident_date)) %>% 
  filter(year(date) == 2018) %>% 
  group_by(month = month(date)) %>% 
  tally()
