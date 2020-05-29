# Load Data ---------------------------------------------------------------
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

# Load Libraries ----------------------------------------------------------
library(here)
library(tidyverse)

library(rgdal)
library(sp)
library(maptools)
library(mapproj)
library(geojsonio)
library(ggplot2)
library(ggmap)
library(viridis)
library(broom)
library(glue)

# Explore Data ------------------------------------------------------------
dim(tickets)
colnames(tickets)

unique(tickets$issuing_agency)
unique(tickets$fine)
unique(tickets$violation_desc)
table(tickets$fine)

tickets <- tickets %>%
  arrange(issuing_agency, issue_datetime) %>%
  distinct()

# Get geoJSON -------------------------------------------------------------
# https://www.opendataphilly.org/dataset/zip-codes/resource/825cc9f5-92c2-4b7c-8b4e-6affa41396ee
data_json <- geojson_read(x = "3_12_2019/Zipcodes_Poly.geojson", what = "sp",stringsAsFactors = F)

plot(data_json)

tidy_json <- tidy(data_json, region = "CODE")
length(unique(tidy_json$id))
length(unique(tickets$zip_code))



# Prepare for Plot --------------------------------------------------------
count_violation <- tickets %>%
  group_by(zip_code) %>%
  summarise_at(vars(fine), list(~ length(.))) %>%
  mutate_at(vars(zip_code), list(~ as.character(.)))

p <- tidy_json %>%
  rename(zip_code=id) %>%
  select(c(long,lat,zip_code,group)) %>%
  full_join(count_violation, by="zip_code") %>%
  ggplot() +
  geom_polygon(aes( x = long, y = lat, group = group, fill= fine), color="white") +
  theme_void() +
  scale_fill_viridis(name="Total amount of infringements") +
  coord_map(); p

ggsave(glue("TidyTuesday_3_12_2019.png"), width = 10, height = 6)

# rayshader::plot_gg(p, scale = 50,width = 10, height = 10)

