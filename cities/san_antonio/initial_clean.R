## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data

dat_old <- read_csv("cities/san_antonio/old_data/offenses_2023_2024OLD.csv") %>% clean_names()
dat_new <- read_csv("cities/san_antonio/old_data/offense23_25.csv") %>% clean_names()


########## duplicates
dups <- dat_new %>% 
  select(-id) %>% 
  bind_rows(dat_old) %>% 
  group_by(report_id) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(report_id)

doubles <- dat_new %>% 
  select(-id) %>% 
  bind_rows(dat_old) %>% 
  filter(report_id %in% dups) %>% 
  arrange(report_id)
## report date can have multipl

dat_new %>% 
  select(-id) %>% 
  bind_rows(dat_old) %>% ## 648847
  distinct() ## 648817
## only removes 30 duplicates

dat <- dat_new %>% 
  select(-id) %>% 
  bind_rows(dat_old) %>% 
  select(-date_time) %>% 
  distinct()
### 335316

############# cleaning data
clean_dat <- dat %>%
  select(report_date, nibrs_code_name, zip_code) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = ymd(report_date), # change to date
         ofns_desc = str_to_lower(nibrs_code_name)) %>%  # make to lower case
  select(-c(report_date, nibrs_code_name)) %>%
  filter(date > ymd("2015-12-31")) %>% 
  filter(!is.na(zip_code)) %>% ## cant use if missing
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
## no missing zips but only till 2023

############# zip codes
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")
## this has all the same goemetrys from 2021 onward to 2023

zip_crime <- clean_dat %>% 
  left_join(tibble(zips), by = c("zip_code" = "geoid")) %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% ## assumes pop after 2023 is same as 2023 and actually comes from same 2021 shapefiles in larger dataset
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, zip_code, estimate)


final <- zip_crime %>% 
  rename(zcta = zip_code, population = estimate) %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date),
         city = "san_antonio") %>% 
  group_by(city, month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/san_antonio/current.csv")
write_csv(dat, "cities/san_antonio/full_uncleaned_current.csv")

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
