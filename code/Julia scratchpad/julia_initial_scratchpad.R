library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(tidycensus)
library(ggrepel)

tobacco_tbl = read_csv("ignore/data/tobacco retailers/list3_tobacco_20240910_pregeocode_geocodio_6e3531302d300449332adfded219f5770d2cbd95.csv")
alcohol_shp = st_read("ignore/data/alcohol retailers/study_outlets.shp")

tobacco_tbl = tobacco_tbl |> clean_names() |> 
  mutate(source = "Tobacco", 
         corp_nm = NA,
         n_prms_ = NA, 
         n_lcnss = NA,
         prmt_ty = NA,
         hours = NA,
         phone = NA, 
         city = str_to_lower(city),
         retailer_name = str_to_lower(retailer_name)) |>
  rename(address = street_address)
alcohol_shp = alcohol_shp |> 
  mutate(source = "Alcohol", 
         store_type = NA, 
         unit_type = NA, 
         unit_number = NA, 
         accuracy_score = NA, 
         accuracy_type = NA, 
         city = str_to_lower(city),
         trad_nm = str_to_lower(trad_nm)) |>
  rename(retailer_name = trad_nm,
         longitude = longitd, 
         latitude = latitud)

tobacco_sf = st_as_sf(tobacco_tbl, coords = c("longitude", "latitude"), remove = FALSE) |> 
  st_set_crs("wgs84") |> st_transform('NAD83')
alcohol_sf = st_as_sf(alcohol_shp, coords = c("longitude", "latitude"), remove = FALSE) |> 
  st_transform(st_crs(tobacco_sf))

col_names <- c('retailer_name', 'corp_nm', 'address', 'city', 'n_prms_', 
               'n_lcnss', 'prmt_ty', 'zip', 'hours', 'phone', 'geometry', 
               'source', 'store_type', 'unit_type', 'unit_number', 'accuracy_score', 
               'accuracy_type', 'geometry', 'longitude', 'latitude')

combined_sf <- rbind(alcohol_sf |> select(all_of(col_names)), 
                     tobacco_sf |> select(all_of(col_names))) 
  
ggplot(combined_sf |> #Durham zoom over clustering of points for case-by-case analysis
         filter(city == "durham" & longitude > -79 & longitude < -78.8 & latitude > 35.7 & latitude < 36.2), 
       aes(color = source)) +
  geom_sf() 

#Diving in 
ggplot(combined_sf |>
         filter(city == "durham", 
                longitude > -79.5, longitude < -78.5, 
                latitude < 36.2, latitude > 35.8),
       aes(color = source, geometry = geometry)) +
  geom_sf()

comparison <- combined_sf |>
  filter(city == "durham", 
         latitude < 36.2, latitude > 35.8,
         longitude < -78.5, longitude > -79.5)

matches <- c("mi barrio su tienda hispana")

check_matches <- function(city_name, dist_lim) {
  durham_alc <- alcohol_sf |> filter(city == "durham", 
                                     latitude < 36.05, latitude > 35.95,
                                     longitude < -78.8, longitude > -79)
    #filter(city == city_name)
  durham_tob <- tobacco_sf |> filter(city == "durham", 
                                     latitude < 36.05, latitude > 35.95,
                                     longitude < -78.8, longitude > -79)
    #filter(city == city_name)
  
  for (a in 1:nrow(durham_alc)) {
    print(a)
    for (t in 1:nrow(durham_tob)) {
      dist = unclass(st_distance(durham_alc[a, ]['geometry'], durham_tob[t, ]['geometry']))
      if (dist < dist_lim) {
        t_name_alc = st_drop_geometry(durham_alc[a, ]['retailer_name'])[[1]]
        t_name_tob = st_drop_geometry(durham_tob[t, ]['retailer_name'])[[1]]
        t_idx_alc = a
        t_idx_tob = t
        t_adr_alc = st_drop_geometry(durham_alc[a, ]['address'])[[1]]
        t_adr_tob = st_drop_geometry(durham_tob[t, ]['address'])[[1]]
        t_geom_alc = durham_alc[a, ]['geometry'][[1]]
        t_geom_tob = durham_tob[t, ]['geometry'][[1]]
        match = c(name_alc = t_name_alc, name_tob = t_name_tob, 
                  idx_alc = t_idx_alc, idx_tob = t_idx_tob, 
                  street_address_alc = t_adr_alc, 
                  street_address_tob = t_adr_tob, 
                  geom_alc = t_geom_alc, geom_tob = t_geom_tob, distance = dist)
        if (is.null(comb_df)) {
          comb_df <- match
        } else {
          comb_df <- rbind(comb_df, match)
        }
      }
    }
  }
  
  return(comb_df)
}

subsetted_durham_matches <- write.csv(comb_df, 'process/JHP_scratchpad_process/sub_durham_matches.csv')

durham_matches <- check_matches("durham", 1000)

#Second Iteration####
durham_buff_alc <- alcohol_sf |> 
  filter(city == "durham") |>
  mutate(buffers = st_buffer(geometry, dist = 500), 
         row = row_number())

durham_pt_tob <- tobacco_sf |>
  filter(city == "durham") |>
  mutate(row = row_number()) 

#Is one object within the buffer of another?
is_match <- function(buffer, point, id) {
  obj <- st_within(point, buffer)[[1]]
  if (length(obj) == 0) {
    return(NA_character_)
  } else if (obj == 1) {
    return(id)
  } else {
    return(NA_character_)
  }
}

#List of all matches found in the tobacco dataset for a given buffer
#Function later mapped across larger dataset 
matches_list <- function(buffer) {
  buf_tob <- durham_pt_tob |>
    mutate(is_match = pmap(list(geometry, row), ~ is_match(buffer, ..1, ..2)))
  
  matches <- buf_tob |> filter(is_match != "NA") |> pull(row)
  matches_vec <- unlist(str_split(as.character(matches), ' '))

  return(matches_vec)
}

durham_duplicates_alc <- durham_buff_alc |> 
  mutate(matches = map(buffers, matches_list)) |> 
  unnest_longer(matches)

#To be further cleaned: dataset with one row being one possible duplicate pair
#Multiple rows for certain alcohol outlets (if multiple possible duplicates)
#Possible multiple rows for certain toabcco outlets (if in the buffers of multiple alcohol outlets)
duplicates_joined <- durham_duplicates_alc |>
  mutate(matches = as.integer(matches)) |>
  left_join(durham_pt_tob, by = join_by(matches == row))

duplicates_joined <- duplicates_joined |>
  mutate(dist = pmap(list(geometry.x, geometry.y), ~ as.numeric(st_distance(..1, ..2)))) |>
  unnest(dist)

r_samp <- sample_n(duplicates_joined, 100)
r_samp <- r_samp |> select(dist, geometry.x, geometry.y, retailer_name.y, retailer_name.x, address.x, address.y)
write.csv(r_samp, 'process/JHP_scratchpad_process/manual_samples.csv')
annotated <- read_csv('process/JHP_scratchpad_process/manual_samples_annotated.csv')
#Return to dist cutoff later, for now 

annotated <- annotated |>
  mutate(name_dist = pmap(list(address.x, address.y), ~ adist(..1, ..2))) |>
  unnest(name_dist)

dist_sum <- annotated |>
  mutate(name_dist = as.integer(name_dist)) |>
  pivot_longer(cols = c(dist, name_dist), 
               names_to = "dist_type", 
               values_to = "distance") |>
  group_by(duplicate, dist_type) |>
  summarize(mean = mean(distance), 
            median = median(distance),
            min = min(distance), 
            max = max(distance))
  
