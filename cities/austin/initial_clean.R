## load packages
library(tidyverse)
library(sf)
library(janitor)
library(viridis)

############### load data
dat <- read_csv("cities/austin/old_data/Crime_Reports_20250525.csv") %>% 
  clean_names()

############# cleaning data
clean_dat <- dat %>%
  select(occurred_date, highest_offense_description, census_block_group) %>% ##
  mutate(date = mdy(occurred_date), # change to date
         ofns_desc = str_to_lower(highest_offense_description)) %>%  # make to lower case
  select(-c(occurred_date, highest_offense_description)) %>%
  filter(date > ymd("2015-12-31")) %>% 
  filter(!is.na(census_block_group)) %>% ## cant use if missing
  mutate(desc_crime_type = case_when(
    str_detect(ofns_desc, "murder|manslaughter|homicide") ~ "homicide",
    str_detect(ofns_desc, "vehicle theft") ~ "mvt",
    str_detect(ofns_desc, "sex|rape") ~ "sex crimes",
    str_detect(ofns_desc, "robbery") ~ "robbery",
    str_detect(ofns_desc, "assault|deadly conduct") ~ "assault",
    str_detect(ofns_desc, "larceny|theft") ~ "theft",
    str_detect(ofns_desc, "burglary") ~ "burglary",
    str_detect(ofns_desc, "arson") ~ "arson",
    #str_detect(ofns_desc, "harrassment") ~ "harrassment",
    str_detect(ofns_desc, "weapon") ~ "weapon",
    str_detect(ofns_desc, "drug|narcotic") ~ "drugs",
    .default = "other"
  ))
## now 886K

##### spatial zips info
### zip code joins
zips <- read_rds("us_zctas_2016_2023.rds")


block_groups <- unique(clean_dat$census_block_group)

blocks <- st_read("cities/austin/old_data/Census_Block_Groups_20250525.csv") %>% 
  clean_names() %>% 
  st_as_sf(wkt = "polygon", crs = st_crs(zips))

ggplot(blocks) +
  geom_sf()
## going to need to spatially interpolate crimes 
### from blocks to zips
dat_agg_crime_block <- clean_dat %>% 
  mutate(month = month(date, label = T, abbr = T),
         year = year(date),
         census_block_group = as.character(census_block_group)) %>% 
  group_by(month, year, desc_crime_type, census_block_group) %>% 
  tally(name = "count")

dat_crime_sp <- dat_agg_crime_block %>% 
  left_join(blocks) %>% 
  select(month, year, desc_crime_type, census_block_group, count, polygon) %>% 
  st_as_sf(crs = st_crs(zips)) 
  

austin_zips <- blocks %>% 
  st_join(zips)

## correct spatial and only 2020 year
austin_zips <- austin_zips %>% 
  tibble() %>% 
  select(year,census_block_group, geoid, estimate) %>% 
  left_join(zips) %>% 
  filter(year == 2020) %>% 
  distinct() %>% 
  st_as_sf() 

ggplot(austin_zips) +
  geom_sf()

subzip <- zips %>% 
  filter(geoid %in% unique(austin_zips$geoid) & year == 2020) 

#zip_crimes <- st_interpolate_aw(x = dat_crime_sp["count"], to = subzip, extensive = TRUE)

groups_dat <- dat_crime_sp %>% 
  group_by(desc_crime_type, year) %>% 
  group_split()

names(groups_dat) <- dat_crime_sp %>% 
  group_by(desc_crime_type, year) %>% 
  group_keys() %>% 
  mutate(label = str_c(str_replace(desc_crime_type, " ", "_"), "_", year)) %>%
  pull(label)

groups_int <- map(groups_dat, ~ st_interpolate_aw(x = .x["count"], to = austin_zips, extensive = TRUE))

system.time(st_interpolate_aw(x = groups_dat[[1]]["count"], to = austin_zips, extensive = TRUE))

system.time(st_interpolate_aw(x = groups_dat[['other_2019']]["count"], to = austin_zips, extensive = TRUE))

#test <- st_interpolate_aw(x = groups_dat[[1]]["count"], to = austin_zips, extensive = TRUE)


## taking too much time just make a cross walk

intersection <- st_intersection(
  blocks %>% select(census_block_group),
  subzip %>% select(geoid)
) %>% 
  mutate(
    intersect_area = st_area(.),
    block_area = st_area(st_geometry(blocks)[match(census_block_group, blocks$census_block_group)]),
    weight = as.numeric(intersect_area / block_area)
  ) %>%
  st_drop_geometry()

final <- dat_agg_crime_block %>%
  left_join(intersection, by = "census_block_group") %>%
  mutate(weighted_count = count * weight) %>%
  group_by(geoid, year, month, desc_crime_type) %>%
  summarise(zcta_crime = sum(weighted_count, na.rm = TRUE)) %>% 
  left_join(zips %>% select(geoid, estimate, year) %>% mutate(year = as.numeric(year))) %>% 
  st_drop_geometry() %>% 
  select(-geometry) %>% 
  rename(zcta = geoid, population = estimate, count = zcta_crime) %>% 
  mutate(city = "austin") %>% 
  mutate(rate = count/population*100000)


tibble(final) %>% 
  mutate(zcta = as.numeric(zcta)) %>% 
  group_by(year, zcta) %>% 
  reframe(total = sum(count)) %>% 
  pivot_wider(id_cols = zcta, names_from = year, values_from = total) %>% view()


final %>% 
  group_by(year, desc_crime_type) %>% 
  summarise(total = sum(count)) %>% 
  ggplot(aes(factor(year), total, group = desc_crime_type)) +
  geom_line() +
  facet_wrap(~desc_crime_type, scales = "free_y")


write_csv(final, "cities/austin/current.csv")
write_csv(dat, "cities/austin/full_uncleaned_current.csv")
