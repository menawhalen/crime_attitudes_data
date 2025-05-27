## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

dat <- map(str_c("cities/boston/old_data/", list.files("cities/boston/old_data", pattern = "*.csv")), ~ read.csv(.) %>% 
             clean_names() %>% 
             mutate(shooting = as.character(shooting),
                    reporting_area = as.character(reporting_area)))

### duplicates
## two records from 2023 onward


dups <- tibble(bind_rows(dat)) %>% 
  group_by(incident_number) %>% 
  count() %>% 
  filter(n>1) %>% 
  pull(incident_number)

doubles <- tibble(bind_rows(dat)) %>% 
  filter(incident_number %in% dups) %>% 
  arrange(incident_number)
## distinct works just fine

dat <- bind_rows(dat) %>% 
  tibble() %>% ### 1 million
  distinct() ## now 845K

######################################################
############# cleaning data
clean_dat <- dat %>%
  select(occurred_on_date, offense_description, lat, long) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = ymd_hms(occurred_on_date), # change to date
         ofns_desc = str_to_lower(offense_description)) %>%  # make to lower case
  select(-c(occurred_on_date, offense_description)) %>%
  filter(date > ymd_hms("2015-12-31 23:59:59")) %>% 
  filter(!is.na(lat) & !is.na(long)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "motor vehicle") ~ "mvt",
    #str_detect(ofns_desc, "sex|rape") ~ "sex crimes",
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
## lost about 100K now at 747K

############# zip codes
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

joined_zip <- st_join(st_as_sf(clean_dat, coords = c("long", "lat"), crs = st_crs(zips)),
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
         city = "boston") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)


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


write_csv(final, "cities/boston/current.csv")
write_csv(dat, "cities/boston/full_uncleaned_current.csv")


