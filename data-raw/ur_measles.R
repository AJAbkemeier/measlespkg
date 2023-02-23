library(dplyr)
library(tidyr)
library(tibble)
load("R/sysdata.rda")
urr = urban_rural_raw

measles = cbind(urr$measles_rural, urr$measles_urban) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = as.numeric(date) + 1900) %>%
  pivot_longer(-date, names_to = "unit", values_to = "cases") %>%
  mutate(date =  as.Date(lubridate::date_decimal(date))) %>%
  select(unit, everything()) %>%
  arrange(unit)

demog_pop = cbind(urr$pop_rural, urr$pop_urban) %>%
  rownames_to_column(var = "year") %>%
  mutate(year = as.integer(year) + 1900) %>%
  pivot_longer(-year, names_to = "unit", values_to = "pop")
demog_births = cbind(urr$births_rural, urr$births_urban) %>%
  rownames_to_column(var = "year") %>%
  mutate(year = as.integer(year) + 1900) %>%
  pivot_longer(-year, names_to = "unit", values_to = "births")
demog = full_join(demog_pop, demog_births, by = c("unit", "year")) %>%
  select(unit, everything())

coord = rbind(urr$coord_rural, urr$coord_urban) %>%
  rename(unit = "X", long = "Long", lat = "Lat") %>%
  as_tibble()

ur_measles = list(
  measles = measles,
  demog = demog,
  coord = coord
)
usethis::use_data(ur_measles, overwrite = TRUE)
