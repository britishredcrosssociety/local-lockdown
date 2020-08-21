##
## Find car parks that are in or neighbour vulnerable MSOAs
##
library(tidyverse)
library(xml2)
library(sf)

# ---- Load Vulnerability Index ----
vi_sp <- read_sf("data/vulnerability-MSOA-UK.geojson")


# ---- Download/load Transport Direct car park data ----
# Note this hasn't been updated since Jan 2015
cp_files = list.files("data/car parks - Transport Direct", pattern = "Car*", full.names = TRUE)

carparks = tibble()  # where to store the carpark data

# Loop over all .xml files, extracting info about each carpark
for (cp_file in cp_files) {
  
  cp_data = read_xml(cp_file)
  
  carparks = bind_rows(
    carparks,
    
    tibble(
      Name = cp_data %>% xml_find_all("//d1:CarParkName") %>% xml_text(),
      Address = cp_data %>% xml_find_all("//d1:Address") %>% xml_text(),
      Postcode = cp_data %>% xml_find_all("//d1:Postcode") %>% xml_text(),
      StayType = cp_data %>% xml_find_all("//d1:StayType") %>% xml_text(),
      URL = cp_data %>% xml_find_all("//d1:URL") %>% xml_text()
    )
  )
  
  print(paste0("Finished ", cp_file))
}

rm(cp_data, cp_file, cp_files)

# Old code from a previous approach to use regex for data extraction
# Extract information from the carpark XML data
# extract_carpark = function(d, t) {
#   d %>% 
#     str_extract_all(paste0("<", t, ">.*"), simplify = TRUE) %>% 
#     str_remove(paste0("<", t, ">")) %>% 
#     str_remove(paste0("</", t, ">"))
# }
# 
# for (cp_file in cp_files) {
#   
#   cp_data = read_file(cp_file)
#   
#   # cp_names = extract_carpark(carparks, "CarParkName")
#   # cp_address = extract_carpark(carparks, "Address")
#   # cp_type = extract_carpark(carparks, "StayType")
#   # cp_url = extract_carpark(carparks, "URL")
#   # 
#   # postcodes = str_extract_all(carparks, brclib::postcode_regex(), simplify = TRUE)
#   
#   carparks = bind_rows(
#     carparks,
#     
#     tibble(
#       Name = extract_carpark(cp_data, "CarParkName"),
#       Address = extract_carpark(cp_data, "Address"),
#       Postcode = str_extract_all(cp_data, brclib::postcode_regex(), simplify = TRUE),
#       StayType = extract_carpark(cp_data, "StayType")
#       # URL = extract_carpark(cp_data, "URL")
#     )
#   )
#   
#   print(paste0("Finished ", cp_file))
# }


# ---- Geocode car parks ----
# download and unzip the ONS Postcode Directory (February 2020): https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-february-2020
# GET("https://www.arcgis.com/sharing/rest/content/items/82889274464b48ae8bf3e9458588a64b/data",
#     write_disk(tf <- tempfile(fileext = ".zip")))
# 
# unzip(tf, exdir = "data/postcodes")
# unlink(tf); rm(tf)

# load postcode directory and keep only relevant fields - we just need MSOA
postcodes_raw = read_csv("data/postcodes/Data/ONSPD_FEB_2020_UK.csv")
postcodes = postcodes_raw %>% dplyr::select(Postcode = pcd, MSOA11CD = msoa11, LAD19CD = oslaua, Longitude = long, Latitude = lat)

# the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
# Mutate to upper case
postcodes <- postcodes %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", "")) %>% 
  select(-Postcode)

carparks <- carparks %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", ""))

# Look up MSOAs and VI deciles for each hospital
carparks_vi = carparks %>%
  left_join(postcodes, by = "Postcode2") %>%
  left_join(vi_sp %>% select(Code, Socioeconomic.Vulnerability.decile), by = c("MSOA11CD" = "Code")) %>% 
  select(-Postcode2)


# ---- Find carparks whose neighbouring MSOAs are highly vulnerable ----
# Track car parks with id
carparks_vi <- carparks_vi %>% 
  mutate(id = row_number()) %>% 
  relocate(id)

# Create list of MSOAs that contain a car park
park_list <- carparks_vi %>% 
  as_tibble() %>% 
  select(id, MSOA11CD)

# Get list of vulnerable MSOAs that don't contain car parks
vuln_msoa_no_carpark <- vi_sp %>% 
  select(Code, Socioeconomic.Vulnerability.decile) %>% 
  left_join(park_list, by = c("Code" = "MSOA11CD")) %>% 
  filter(is.na(id) & Socioeconomic.Vulnerability.decile >= 9) %>% 
  st_set_geometry(NULL)

# Get list of MSOAs that contain carparks


# ---- For each vulnerable MSOA *without* a car park, find its neighbouring MSOAs that do contain car parks ----

neighbouring_carparks = tibble(MSOA11CD = character())

for (vuln_msoa in vuln_msoa_no_carpark$Code) {
  # Make a tibble containing the current vulnerable MSOA without a car park and all MSOAs with a car park, assigning row numbers to each
  curr_msoas <- vi_sp %>% 
    filter(Code == vuln_msoa | Code %in% carparks_vi$MSOA11CD) %>% 
    mutate(row_num = row_number())
  
  # Find the neighbours of each MSOA in `curr_msoas`
  # --> returns a tibble / adjacency matrix containing row IDs and column IDs
  vuln_msoa_neighbours <- curr_msoas %>%
    st_touches() %>% 
    as_tibble()  %>% 
    unique()
  
  # Get row number of the current vulnerable MSOA without a car park
  vuln_msoa_row_id <- curr_msoas %>% 
    st_set_geometry(NULL) %>% 
    filter(Code == vuln_msoa) %>% 
    select(Code, row_num)
  
  # Get IDs of MSOAs with car parks that neighbour the current vulnerable MSOA without one
  neighbouring_ids <- vuln_msoa_neighbours %>% 
    filter(row.id == vuln_msoa_row_id$row_num) %>% 
    select(col.id)
  
  # Extract MSOA codes that contain car parks and neighbour this current MSOA
  neighbouring_msoas <- curr_msoas %>% 
    st_set_geometry(NULL) %>% 
    filter(row_num %in% neighbouring_ids$col.id) %>% 
    select(MSOA11CD = Code)
  
  neighbouring_carparks = bind_rows(neighbouring_carparks,
                                    neighbouring_msoas)
  
}

neighbouring_carparks %>% write_csv("data/list of neighbouring MSOAs with car parks.csv")

# Get list of car parks that are either in a vulnerable MSOA or neighbour one that doesn't contain a car park
carparks_final = carparks_vi %>% 
  filter(Socioeconomic.Vulnerability.decile >= 9 | MSOA11CD %in% neighbouring_carparks$MSOA11CD)

# Save
carparks_final %>% 
  write_csv("data/carparks-vulnerability.csv")

carparks_final %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  write_sf("data/carparks-vulnerability.shp")
