# Function that accepts an sf file, returns an sf file ... with a new unique ID field?
# spatial dedup in same CRS. Name dedup also? Or just do one?
# input: spatial distance matrix + threshold, text_dist (%?) and threshold? 

library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(tidycensus)
library(ggrepel)
library(tictoc)

# TODO Should probably pull down NC county map from tidycensus here
# Get NC map from US census ####
# load_variables("acs5", year = 2020)
options(tigris_use_cache = T)
nc_county_sf = tidycensus::get_acs(geography = "county", state = "NC", year = 2020, variables = c("total_pop" = "B01001A_001"), geometry = T) 
nc_county_sf = nc_county_sf |> st_transform("wgs84")
nc_county_sf |> st_geometry() |> plot()

# Test search ####
## (TODO) Read cannabis table ####
# TODO find, save, then read in cannabis web scrape here

## Read tobacco table ####
tobacco_tbl = read_csv("ignore/data/tobacco retailers/list3_tobacco_20240910_pregeocode_geocodio_6e3531302d300449332adfded219f5770d2cbd95.csv")
tobacco_tbl = tobacco_tbl |> clean_names()
tobacco_tbl = tobacco_tbl |> 
  mutate(county = county |> str_to_lower()) |> 
  mutate(retailer_name = retailer_name |> str_to_title())
tobacco_sf = st_as_sf(tobacco_tbl, coords = c("longitude", "latitude")) |> 
  st_set_crs("wgs84")
tobacco_sf |> st_geometry() |> plot(pch = ".")
tobacco_sf |> filter(county %in% c("orange", "durham")) |> 
  st_geometry() |> plot(pch = ".")

## Read Alcohol ####
alcohol_sf = st_read("ignore/data/alcohol retailers/study_outlets.shp")
alcohol_sf = alcohol_sf |> st_transform(st_crs(tobacco_sf))
alcohol_sf = alcohol_sf |> mutate(trad_nm = trad_nm |> str_to_title())
alcohol_sf |> filter(corp_nm |> str_detect("ABC")) # Quick look

# Flatten into single dataset ####
alcohol_sf |> str(); tobacco_sf |> str()
retail_sf = bind_rows(alcohol_sf |> 
                        select(store_name = trad_nm, store_address = address, store_type = prmt_ty) |> 
                        mutate(source = "Alcohol"),
                      tobacco_sf |> 
                        select(store_name = retailer_name, store_address = street_address, store_type = store_type) |> 
                        mutate(source = "Tobacco"))
retail_sf = retail_sf |> mutate(common_id = 1:n())

# Transform to NC state plane #### 
retail_sf = retail_sf |> st_transform(2264)
# I can't rememeber the nice way to transform units to miles
# could hand edit the wkt / proj4string
# REF: https://spatialreference.org/ref/?search=north+carolina

get_neighbors = function(this_sf, all_sf, buffer_radius = 0.25*5280){
  # TODO remove self from neighbor list. Perhaps by ID. Or by distance = 0
  # TODO be smart about NULL assignment. return an empty tbl/sf, for instance?
  neighbors_to_return = all_sf[this_sf |> st_buffer(dist = buffer_radius),]
  neighbors_to_return$distance = neighbors_to_return |>
    st_distance(neighbors_to_return |> slice_head(n=1)) 
  #^  bad habit, overloading function
  return(neighbors_to_return)
}
# TODO Intersting opportunity - what about KEEPING the first element of the list as 
# its self-reference and deduping - or not - what's inside? BUT AHHH now we hit the chain
# issue
# retail_sf[retail_sf$geometry[1] |> st_buffer(0.5*5280),] # pre-function demo
get_neighbors(retail_sf$geometry[1], retail_sf, 5280*0.5) # post function test

neighbor_nested_sf = retail_sf |> as_tibble() |> # Tibble promotion for nesting
  head(10)  |> # Comment for Testing
  mutate(neighbor_sf = map(geometry, get_neighbors, all_sf = retail_sf)) 
#ooh, don't like this (repeating retail_sf object name). |> _ placeholder is weird - Julia?
neighbor_nested_sf
neighbor_nested_sf$neighbor_sf[1]

## TEST: rename and unnest ####
neighbor_nested_sf |> 
  mutate(neighbor_tbl = map(neighbor_tbl, rename_with, \(x) paste0("nb_", x))) |> 
  unnest(neighbor_tbl)
# Unnest DOES work. I think this could be a feasible data structure / approach.

# Issue Parking Lot
# 1) NB placeholder issues w/ native pipe 
# https://stackoverflow.com/questions/67633022/what-are-the-differences-between-rs-native-pipe-and-the-magrittr-pipe
# 2) Best way to do spatial subset a[b, ] using tidyverse and st_ functions? filter --> st_whatever?
# 3) we're quickly going to get into chain link problems. May need to establish merged buffer clusters
#  and dedup within that
# 4) Easy way to transform units of crs into miles for easier spatial math interpretation? 
# Will still have to send back when mapping with common base layers in ft/m units
# retransform CRS 