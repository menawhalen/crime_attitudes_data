## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)


### load data

dat_old <- read_csv("cities/phoenix/old_data/phoenix_crimeOLD.csv") %>% clean_names()

dat_new <- read_csv("cities/phoenix/old_data/phoenix_crime25.csv") %>% clean_names()

names(dat_old)
names(dat_new)
summary(mdy_hm(dat_old$occurred_on))
summary(mdy_hm(dat_new$occurred_on)) ## also goes back so might be duplicated
######################################################
### duplicates
#######################
dups <- bind_rows(dat_old, dat_new) %>% 
  group_by(inc_number) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(inc_number)
## 503268 duplicates
## about 79581 difference
bind_rows(dat_old, dat_new) %>% 
  filter(inc_number %in% dups) %>% 
  arrange(inc_number) %>% view()
## 1006564/2 = 503282
## old 503456-503282 = 174
bind_rows(dat_old, dat_new) %>% 
  distinct()
## 583677
## 583677-582849 add 828 incidents
dat <- bind_rows(dat_old, dat_new) %>% 
  distinct()
#######################################################
########### cleaning data

clean_dat <- dat %>%
  select(occurred_on, ucr_crime_category, zip) %>% ## usinged cmplt_fr_dt as date since is the earliest crime date and the most accurate time of the incident
  mutate(date = mdy_hm(occurred_on), # change to date
         ofns_desc = str_to_lower(ucr_crime_category),
         zip = factor(zip)) %>%  # make to lower case
  select(-c(occurred_on, ucr_crime_category)) %>%
  filter(date > ymd("2015-12-31")) %>%
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

################################ zips
### no zip data needed for matching only population
zips <- read_rds("us_zctas_2016_2023.rds")

zip_crime <- clean_dat %>% 
  left_join(tibble(zips), by = c("zip" = "geoid")) %>% 
  mutate(year_crime = ifelse(year(date) > 2023, 2023, year(date))) %>% ## assumes pop after 2023 is same as 2023 and actually comes from same 2021 shapefiles in larger dataset
  filter(year == year_crime) %>% 
  select(date, desc_crime_type, zip, estimate)


final <- zip_crime %>% 
  rename(zcta = zip, population = estimate) %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date)) %>% 
  group_by(month, year, zcta, desc_crime_type, population) %>% 
  tally(name = "count") %>% 
  mutate(rate = count/population*100000)

write_csv(final, "cities/phoenix/current.csv")
write_csv(dat, "cities/phoenix/full_uncleaned_current.csv")

clean_dat %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(factor(year))) +
  geom_bar(stat = "count")

final %>% 
  group_by(year, desc_crime_type) %>% 
  summarise(total = sum(count)) %>% 
  ggplot(aes(factor(year), total, group = desc_crime_type)) +
  geom_line() +
  facet_wrap(~desc_crime_type, scales = "free_y")
## no other option or weapons
