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
library(units)
library(eulerr)


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
retail_sf = bind_rows(
  alcohol_sf |> 
    select(store_name = trad_nm, store_address = address, store_type = prmt_ty) |> 
    mutate(source = "Alcohol"),
  tobacco_sf |> 
    select(store_name = retailer_name, store_address = street_address, store_type = store_type) |> 
    mutate(source = "Tobacco")) |> 
  arrange(source, store_name) |> 
  mutate(common_id = 1:n()) |> 
  mutate(across(c(store_name, store_address), str_to_title))

# Transform to NC state plane #### 
retail_sf = retail_sf |> st_transform(2264)
retail_sf |> st_crs()
# I can't rememeber the nice way to transform units to miles
# could hand edit the wkt / proj4string
# REF: https://spatialreference.org/ref/?search=north+carolina


get_root_name = function(a){
  a |> str_remove_all("[0-9 ]+") 
} 
# may want to remove po box, etc. This could be more robust for str comparisons

get_store_name_distance = function(a, b){
  adist(get_root_store_name(a), get_root_store_name(b))
}

get_neighbor_info = function(this_sf, all_sf, buffer_radius = 0.25){
  # TODO remove self from neighbor list? Perhaps by ID. Or by distance = 0
  # TODO be smart about NULL assignment. return an empty tbl/sf, for instance?
  # TODO expects to set buffer radius in a certain units. Not ideal. Better to have explicit units.
  
  # Calc spatial distance
  neighbors_to_return = all_sf[st_buffer(this_sf, dist = buffer_radius * 5280),] # unsafe, need units()
  # neighbors_to_return = all_sf[this_sf |> st_buffer(dist = set_units(buffer_radius, "miles")),]
  neighbors_to_return$dist_geo = st_distance(neighbors_to_return, 
                                             neighbors_to_return[1,]) |> 
    units::set_units("miles") # ensure we're returning miles for interpretability
  
  
  # Calc store name distance
  neighbors_to_return$dist_name = 
    adist(get_root_name(neighbors_to_return$store_name),
          get_root_name(neighbors_to_return$store_name[1]))
  # ^ could also check in one is subset to another, and zero out if so.
  
  # Calc store address distance
  neighbors_to_return$dist_address = 
    adist(neighbors_to_return$store_address,
          neighbors_to_return$store_address[1])

  # Tag as same or not (somewhat arbitrary)
  neighbors_to_return = neighbors_to_return |> 
    mutate(is_same_outlet = case_when(dist_geo < set_units(0.05, "miles") & dist_address < 5 & dist_name < 7 ~ T,
                                      dist_geo < set_units(0.1, "miles") & dist_address < 2 & dist_name < 2 ~ T,
                                      T ~ F))
  
    
  return(neighbors_to_return)
}
#^  bad habit, overloading function. 
# But efficient-ish. root_store_name, address name easier to do once
# TODO Intersting opportunity - what about KEEPING the first element of the list as 
# its self-reference and deduping - or not - what's inside? BUT AHHH now we hit the chain issue
# could rewrite the buffer approach to merge, then compare inside each cluster and collapse.

# retail_sf[retail_sf$geometry[1] |> st_buffer(0.5*5280),] # pre-function demo
get_neighbor_info(retail_sf$geometry[3], retail_sf, .25) # post function test
retail_sf$geometry[3] |> st_crs() # the map() is stripping the crs from geometries. array_branch?
retail_sf$geometry[[3]] |> st_crs()

n_slice = 20 # n_slice = Inf
tic(paste0("tagging ", n_slice, " (x2) retailers for neighbors and dups"))
neighbor_nested_sf = retail_sf |> as_tibble() |> # Tibble promotion for nesting
  group_by(source) |> slice_head(n = n_slice) |> ungroup() |> # Comment for Testing. 100 in 12s -> 1000 in 2m
  mutate(neighbor_sf = map(geometry, get_neighbor_info, all_sf = retail_sf)) 
#^ ooh, don't like this (repeating retail_sf object name). |> _ placeholder is weird - Julia?
toc(log = T)
tic.log()
# TODO write tic log tic.log()
tic.clearlog()
neighbor_nested_sf
neighbor_nested_sf$neighbor_sf[1]

# Shrink to dups only
keep_duplicate_places = function(this_tbl){
  this_tbl |> filter(is_same_outlet)
}
get_source_list = function(this_tbl){
  this_tbl |> count(source) |> pull(source) |> paste(collapse = ", ")
}
no_earlier_match = function(this_id, this_tbl){
  !any(this_tbl$common_id < this_id)
}

neighbor_nested_sf = neighbor_nested_sf |> 
  mutate(same_outlet_sf = map(neighbor_sf, keep_duplicate_places)) |> 
  mutate(n_dups = map_int(same_outlet_sf, nrow)) |> 
  mutate(source_list = map_chr(same_outlet_sf, get_source_list)) |> 
  # TODO add id list
  mutate(has_no_earlier_match = map2_lgl(common_id, same_outlet_sf, no_earlier_match))
neighbor_nested_sf
neighbor_nested_sf |> count(source_list, n_dups)
neighbor_nested_sf |> filter(source_list == "Alcohol", n_dups == 2) |> pull(same_outlet_sf) # common_id 17, 15760
neighbor_nested_sf |> count(has_no_earlier_match)
saveRDS(neighbor_nested_sf, "process/neighbor_nested_sf.rds")

unique_nested_sf = neighbor_nested_sf |> filter(has_no_earlier_match)

neighbor_nested_sf
# neighbor_nested_sf |> print(n=Inf)
neighbor_nested_sf[502,]
neighbor_nested_sf$same_outlet_sf[502]
neighbor_nested_sf |> count(source_list)
neighbor_nested_sf |> count(n_dups)
neighbor_nested_sf |> count(has_no_earlier_match, n_dups)
#TODO NOTE still haven't really deduped yet. Need to only count dups once. Currently probably SORTA working

# Venn / Euler diagrams ####
set_list = list(tobacco = unique_nested_sf |> filter(source_list |> str_detect("Tobacco")) |> pull(common_id),
                alcohol = unique_nested_sf |> filter(source_list |> str_detect("Alcohol")) |> pull(common_id))
set_list |> euler() |> plot(quantities = T)
set_list |> venn() |> plot()



## TEST: rename and unnest ####
neighbor_nested_sf |> 
  mutate(neighbor_sf = map(neighbor_sf, rename_with, \(x) paste0("nb_", x))) |> 
  unnest(neighbor_sf)
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