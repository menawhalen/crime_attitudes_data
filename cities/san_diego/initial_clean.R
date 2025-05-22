## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data

file_names <- list.files(path = "cities/san_diego/old_data", pattern = ".csv")

dat_files <- map(file_names, ~ read_csv(str_c("cities/san_diego/old_data/", .x)) 
                 %>% clean_names)

names(dat_files[[3]])
sum(dat_files[[2]]$incident_uid != dat_files[[1]]$incident_uid) ## files 1 and two are the same
## I will just use one

## has around 510K (510589)
dat_files[[1]] %>% 
  select(cibrs_unique_offense_id, incident_date, cibrs_offense_description, zip_code)
## has 429305
dat_files[[3]] %>% 
  select(nibrs_uniq, occured_on, ibr_offense_description, zip, longitude, latitude)


cibrs_uniq <- dat_files[[1]]$cibrs_unique_offense_id 
nibrs_uniq <- dat_files[[3]]$nibrs_uniq

sum(cibrs_uniq %in% nibrs_uniq)
sum(nibrs_uniq %in% cibrs_uniq)
## nothing in common. 

### these come from two different places
## Cibrs comes from opendata.sandag.org and covers more area than just the city of san diego
### nibrs comes from data.sangiego.gov and is from the san diego police department

### only looking at from the pd of san diego

dat_new <- read_csv("cities/san_diego/old_data/pd_nibrs_datasd.csv")
dat_old <- read_csv("cities/san_diego/old_data/pd_nibrs_datasdOLD.csv")

######################## duplicates
dups <- dat_old$nibrs_uniq[dat_old$nibrs_uniq %in% dat_new$nibrs_uniq]

dat_old %>% 
  filter(!(nibrs_uniq %in% dups)) %>% 
  bind_rows(dat_new) %>% 
  group_by(nibrs_uniq) %>% 
  count() %>% 
  filter(n > 1)


dat <- dat_old %>% 
  filter(!(nibrs_uniq %in% dups)) %>% 
  bind_rows(dat_new)


############# cleaning data
clean_dat <- dat %>%
  select(occured_on, pd_offense_category, zip, latitude, longitude) %>% ##
  mutate(date = ymd_hms(occured_on), # change to date
         ofns_desc = str_to_lower(pd_offense_category)) %>%  # make to lower case
  select(-c(occured_on, pd_offense_category)) %>%
  filter(date > ymd_hms("2016-01-01 00:00:00")) %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% ## cant use if missing
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


############### zips
zips <- read_rds("us_zctas_2016_2023.rds")
## check missing zips

sum(is.na(clean_dat$zip))
## 1712

zip_crime <- clean_dat %>% 
  filter(is.na(zip)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(zips)) %>% 
  st_join(select(zips, year, geoid, estimate)) %>% 
  tibble() %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% ## assumes pop after 2023 is same as 2023 and actually comes from same 2021 shapefiles in larger dataset
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, geoid, estimate) %>% 
  rename(zip = geoid) %>% 
  bind_rows( clean_dat %>% 
  filter(!is.na(zip)) %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(tibble(zips), by = c("zip" = "geoid")) %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% 
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, zip, estimate) )


final <- zip_crime %>% 
  rename(zcta = zip, population = estimate) %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date),
         city = "san_diego") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/san_diego/current.csv")
write_csv(dat, "cities/san_diego/full_uncleaned_current.csv")

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

