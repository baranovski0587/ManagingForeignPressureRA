setwd('C:/Users/yegor/Desktop/BaranovskiRA_work/DataRaw')

library(dplyr)
library(readr)
library(stringr)

cepii.countries <- read_csv('Countries_V202211.csv')

countries.selected <- cepii.countries %>%
  select(country_id, country) %>%
  arrange(country_id)

countries.selected <- countries.selected %>%
  filter(!str_ends(country_id, "\\.1"))

countries.selected <- countries.selected %>%
  mutate(country_id = str_remove(country_id, "\\.2"))

write.csv(countries.selected, file = 'C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/cleaned.cepii.codes.csv' , row.names = FALSE)