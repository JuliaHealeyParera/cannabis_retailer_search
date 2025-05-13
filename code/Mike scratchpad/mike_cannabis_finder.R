library(tidyverse)
library(googleway)
library(sf)
library(ggrepel)
library(tidycensus)
library(janitor)

# Get NC map from US census ####
load_variables("acs5", year = 2020)
nc_county_sf = tidycensus::get_acs(geography = "county", state = "NC", year = 2020, variables = c("total_pop" = "B01001A_001"), geometry = T) 
nc_county_sf = nc_county_sf |> st_transform("wgs84")
nc_county_sf |> st_geometry() |> plot()

# Test search ####
api_key = read_csv("ignore/credentials/mike_place_api_key.csv") |> filter(name == "places") |> pull(key)

query_tbl = tibble(search_string_base = "cannabis cbd thc",
                   location = c("Durham County, NC", "Orange County, NC")) |> 
  mutate(search_string = paste(search_string_base, location))

# hold = googleway::google_places(search_string = paste(search_string_base, "durham county nc"), simplify = T, key = api_key) # single test
# place_tbl = hold$results |> as_tibble() |> clean_names() |> rename_with(~.x |> str_replace_all("\\$", "_"))

search_for_cannabis_places = function(this_search_string, this_key){
  googleway::google_places(search_string = this_search_string, simplify = T, key = this_key)
}

# ^ Just do this once, don't burn API calls. Need to price out the per-month cap on "free" usage. I think it's $200 of free calls a month...
query_tbl = query_tbl |>
  mutate(query_response = map(search_string, search_for_cannabis_places, this_key = api_key))
# TODO Save this object then comment out / separate script
query_tbl

search_result_tbl = query_tbl |> 
  mutate(search_results = map(query_response, pluck("results"))) |> 
  unnest(search_results) |>
  select(matches("business|address|lat$|lon$|closed|name")) |> 
  clean_names() |> rename_with(\(x) x |> str_replace_all("\\$", "_"))
search_result_tbl

place_tbl = search_result_tbl |> select(business_status, formatted_address, name, place_id, permanently_closed, lat, lng)

place_sf = place_tbl |> 
  bind_cols(place_tbl |> pull(geometry) |> pull(location)) |> 
  select(business_status, formatted_address, name, place_id, permanently_closed, lat, lng) |> 
  st_as_sf(coords = c("lng", "lat"), crs = "wgs84")  |> st_transform("wgs84")
ggplot(place_sf)+geom_sf()
# ^ I'm not 100% sure this is the right crs for google's data. Mixed online documentation, maybe "Web Mercator"
# https://gis.stackexchange.com/questions/60432/which-crs-to-use-for-google-maps

# Map ####
ggplot()+
  geom_sf(data = nc_county_sf |> filter(NAME |> str_detect("Orange|Durham|Alamance|Wake")))+
  geom_sf(data = place_sf, color = "red")+
  # geom_sf_text(data = place_sf, aes(label = str_wrap(name, 15)), size = 2, color = "red")+ # Drop for the repel versionA
  # geom_text_repel(data = place_sf, aes(label = str_wrap(name, 15), geometry = geometry), stat = "sf_coordinates", size = 2, color = "red")+
  geom_text_repel(data = place_sf, aes(label = str_wrap(name, 15), geometry = geometry), stat = "sf_coordinates", size = 3, color = "white", bg.color="grey10", bg.r=0.05, force = 30)+
  labs(title = "Scraped cannabis locations", subtitle = "Scraped locations from page 1 of results, 1 API call, of 'cannabis orange county nc'")+
  theme_minimal()

