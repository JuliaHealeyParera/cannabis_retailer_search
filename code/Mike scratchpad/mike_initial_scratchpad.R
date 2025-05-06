library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(tidycensus)
library(ggrepel)

# TODO Should probably pull down NC county map from tidycensus here
# Get NC map from US census ####
# load_variables("acs5", year = 2020)
options(tigris_use_cache = T)
nc_county_sf = tidycensus::get_acs(geography = "county", state = "NC", year = 2020, variables = c("total_pop" = "B01001A_001"), geometry = T) 
nc_county_sf = nc_county_sf |> st_transform("wgs84")
nc_county_sf |> st_geometry() |> plot()

# Test search ####
api_key = read_csv("code/mike_place_api_key.csv") |> filter(name == "places") |> pull(key)
# TODO demo 1 search

# Read tobacco table ####
tobacco_tbl = read_csv("ignore/data/tobacco retailers/list3_tobacco_20240910_pregeocode_geocodio_6e3531302d300449332adfded219f5770d2cbd95.csv")
tobacco_tbl = tobacco_tbl |> clean_names()
tobacco_tbl = tobacco_tbl |> 
  mutate(county = county |> str_to_lower()) |> 
  mutate(retailer_name = retailer_name |> str_to_title())

tobacco_tbl |> names()
tobacco_sf = st_as_sf(tobacco_tbl, coords = c("longitude", "latitude")) |> 
  st_set_crs("wgs84")

## Quick plot ####
tobacco_sf |> st_geometry() |> plot(pch = ".")
tobacco_sf |> filter(county %in% c("orange", "durham")) |> 
                       st_geometry() |> plot(pch = ".")


# Alcohol ####
alcohol_sf = st_read("ignore/data/alcohol retailers/study_outlets.shp")
alcohol_sf = alcohol_sf |> st_transform(st_crs(tobacco_sf))
alcohol_sf = alcohol_sf |> mutate(trad_nm = trad_nm |> str_to_title())
alcohol_sf  

alcohol_sf |> filter(corp_nm |> str_detect("ABC")) # Quick look

# Flatten into single dataset
alcohol_sf |> str(); tobacco_sf |> str()
retail_sf = bind_rows(alcohol_sf |> 
                        select(store_name = trad_nm, store_address = address, store_type = prmt_ty) |> 
                        mutate(source = "Alcohol"),
                      tobacco_sf |> 
                        select(store_name = retailer_name, store_address = street_address, store_type = store_type) |> 
                        mutate(source = "Tobacco"))
retail_sf

# Skim for difficulty ####
## Big map ####
ggplot()+
  geom_sf(data = nc_county_sf)+
  geom_sf(data = tobacco_sf, color = "red", size = .2)+
  geom_sf(data = alcohol_sf |> st_jitter(), color = "blue", size = .2)+
  labs(title = "Quick map: tobacco (red)_and alcohol (blue)")+
  theme_void()
## Big map 2 ####
ggplot()+
  geom_sf(data = nc_county_sf)+
  geom_sf(data = retail_sf, aes(color = source), size = .2, alpha = 0.4)+
  facet_wrap(~source)+
  labs(title = "Quick map: tobacco and alcohol")+
  theme_void()



## Zoom map 1: Orange and Durham ####
focus_area_sf = nc_county_sf |> filter(NAME |> str_detect("Orange|Durham"))
ggplot()+
  geom_sf(data = focus_area_sf)+
  geom_sf(data = tobacco_sf[focus_area_sf,], color = "red", size = .2)+
  geom_text_repel(data = tobacco_sf[focus_area_sf,], 
                  aes(label = retailer_name, geometry = geometry), stat = "sf_coordinates",
                  size = 2, color = "red")+
  geom_sf(data = alcohol_sf[focus_area_sf,] |> st_jitter(), color = "blue", size = .2)+
  geom_text_repel(data = alcohol_sf[focus_area_sf,], 
                  aes(label = trad_nm, geometry = geometry), stat = "sf_coordinates",
                  size = 2, color = "blue")+
    labs(title = "Quick map: tobacco (red)_and alcohol (blue)")+
  theme_void()
ggsave("process/MDF scratchpad process/Orange & Durham - tobacco and alcohol retailers, select labels.png", width = 17, height = 8)

## Zoom map 2: Duke nearby ####
duke_sf = st_as_sf(tibble(place = "Duke University", lat = 36.0014, long = -78.9382), coords = c("long", "lat")) |> 
  st_set_crs("wgs84")
duke_buffer_sf = duke_sf |> st_buffer(3000) # technically degrees, roughly 10mi
plot(nc_county_sf |> st_geometry()); plot(duke_buffer_sf |> st_geometry(), add = T); 
focus_area_sf = duke_buffer_sf

ggplot()+
  # geom_sf(data = nc_county_sf[focus_area_sf, ])+
  geom_sf(data = duke_sf, size = 6, color = "dark blue", alpha = 0.8)+
  geom_sf(data = retail_sf[focus_area_sf,], aes(color = source), size = .3)+
  geom_text_repel(data = retail_sf[focus_area_sf,], 
                  aes(label = store_name, geometry = geometry, color = source), stat = "sf_coordinates",
                  size = 2, max.overlaps = Inf)+
  labs(title = "Quick map: tobacco and alcohol near Duke", subtitle = "Flattened alcohol and tobacco retailer locations for testing")+
  theme_void()
ggsave("process/MDF scratchpad process/Duke zoom - tobacco and alcohol retailers, select labels.png", width = 17, height = 8)

## Zoom map 3: UNC ####
unc_sf = st_as_sf(tibble(place = "UNC Chapel Hill", lat = 35.9132, long = -79.0558), coords = c("long", "lat")) |> 
  st_set_crs("wgs84")
unc_buffer_sf = unc_sf |> st_buffer(3000) # technically degrees, roughly 10mi
plot(nc_county_sf |> st_geometry()); plot(unc_buffer_sf |> st_geometry(), add = T); 
focus_area_sf = unc_buffer_sf

ggplot()+
  # geom_sf(data = nc_county_sf[focus_area_sf, ])+
  geom_sf(data = retail_sf[focus_area_sf,], aes(color = source), size = .3)+
  geom_sf(data = unc_sf, size = 6, color = "light blue")+
  geom_text_repel(data = retail_sf[focus_area_sf,], 
                  aes(label = store_name, geometry = geometry, color = source), stat = "sf_coordinates",
                  size = 2, max.overlaps = Inf)+
  labs(title = "Quick map: tobacco and alcohol", subtitle = "Retailers near UNC-CH")+
  theme_void()
ggsave("process/MDF scratchpad process/UNC zoom - tobacco and alcohol retailers, select labels.png", width = 17, height = 8)

# TODO Flatten into a single dataset, easier mapping.


# Spatial buffer ####
hold = alcohol_sf |> st_intersection(tobacco_sf)
hold

hold


