##
## Find potential hospital sites for mobile testing based on whether they are in or surrounded by highly vulnerable neighbourhoods
##
library(tidyverse)
library(readxl)
library(httr)
library(sf)

# ---- Load Vulnerability Index ----
vi = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-UK.geojson")

vi = vi %>%
  dplyr::select(Code, Socioeconomic.Vulnerability.decile)


# ---- Load and geocode hospitals ----
hospitals_eng = read_csv("data/hospitals/hospitals-england.csv")
hospitals_wal = read_excel("data/hospitals/hospitals-wales.xls")
hospitals_sco = read_csv("data/hospitals/hospitals-scotland.csv")
hospitals_ni  = read_csv("data/hospitals/hospitals-ni.csv")

# Filter types of hospital
hospitals_eng = hospitals_eng %>%
  filter(SubType != "Mental Health Hospital")

hospitals_wal = hospitals_wal %>% 
  filter(Type %in% c("Major A&E Units",
                     "Community",
                     "Acute",
                     "Major acute",
                     "Minor A&E Units"))

# Make all hospitals data the same
hospitals_eng = hospitals_eng %>% 
  unite("Address", Address1:County, sep = ", ", na.rm = TRUE) %>% 
  select(Name = OrganisationName, Address, Postcode, )

hospitals_wal = hospitals_wal %>% 
  select(Name = Hospital, Address = Town, Postcode)

hospitals_sco = hospitals_sco %>% 
  select(Name = `Location Name`, Address, Postcode)

hospitals_ni = hospitals_ni %>% 
  select(Name = Hospital, Address = HospitalName, Postcode)

# Merge hospitals into a single dataframe
hospitals = bind_rows(hospitals_eng, hospitals_wal, hospitals_sco, hospitals_ni)

# Download and unzip the ONS Postcode Directory (February 2020): https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-february-2020
GET("https://www.arcgis.com/sharing/rest/content/items/82889274464b48ae8bf3e9458588a64b/data",
    write_disk(tf <- tempfile(fileext = ".zip")))

unzip(tf, exdir = "data/postcodes")
unlink(tf); rm(tf)

# load postcode directory and keep only relevant fields - we just need MSOA
postcodes_raw = read_csv("data/postcodes/Data/ONSPD_FEB_2020_UK.csv")
postcodes = postcodes_raw %>% dplyr::select(Postcode = pcd, lsoa11, msoa11, oslaua, long, lat)

# the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
# Mutate to upper case
postcodes <- postcodes %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", ""))

hospitals <- hospitals %>%
  mutate(Postcode2 = str_to_upper(Postcode),
         Postcode2 = str_replace_all(Postcode, " ", ""))

# Look up MSOAs and VI deciles for each hospital
hospitals_vi = hospitals %>%
  left_join(postcodes, by = "Postcode2") %>%
  
  # NI doesn't have MSOAs so copy its LSOAs over
  mutate(msoa11 = if_else(str_sub(lsoa11, 1, 1) == "9", lsoa11, msoa11)) %>% 
  
  left_join(vi, by = c("msoa11" = "Code"))

# convert to spatial dataframe and save
hospitals_sp = hospitals_vi %>%
  dplyr::select(Name, Address, Postcode = Postcode.x, LAD19CD = oslaua, MSOA11CD = msoa11, Longitude = long, Latitude = lat, Socioeconomic.Vulnerability.decile) %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 
  # write_sf("data/hospitals.shp")

# save as .csv too
# hospitals_eng_vi %>%
#   dplyr::select(OrganisationName, Sector, ParentName, Address1, Address2, Address3, City, Postcode = Postcode.x, Website, LAD19CD = oslaua, MSOA11CD = msoa11, Longitude, Latitude) %>%
#   write_csv("data/hospitals.csv")

# ---- Find hospitals whose neighbouring MSOAs are highly vulnerable ----
# Track hospitals with id
hospitals_sp <- hospitals_sp %>% 
  mutate(id = row_number()) %>% 
  relocate(id)

# Create list of MSOAs that contain a hospital
hosp_list <- hospitals_sp %>% 
  as_tibble() %>% 
  select(id, MSOA11CD)

# Get list of vulnerable MSOAs that don't contain hospitals
vuln_msoa_no_hospital <- vi %>% 
  select(Code, Socioeconomic.Vulnerability.decile) %>% 
  left_join(hosp_list, by = c("Code" = "MSOA11CD")) %>% 
  filter(is.na(id) & Socioeconomic.Vulnerability.decile >= 9) %>% 
  st_set_geometry(NULL)


# ---- For each vulnerable MSOA *without* a car park, find its neighbouring MSOAs that do contain car parks ----

neighbouring_hospitals = tibble(MSOA11CD = character())

for (vuln_msoa in vuln_msoa_no_hospital$Code) {
  # Make a tibble containing the current vulnerable MSOA without a hospital and all MSOAs with hospitals, assigning row numbers to each
  curr_msoas <- vi %>% 
    filter(Code == vuln_msoa | Code %in% hospitals_vi$msoa11) %>% 
    mutate(row_num = row_number())
  
  # Find the neighbours of each MSOA in `curr_msoas`
  # --> returns a tibble / adjacency matrix containing row IDs and column IDs
  vuln_msoa_neighbours <- curr_msoas %>%
    st_touches() %>% 
    as.data.frame() %>% 
    as_tibble()  %>% 
    unique()
  
  # Get row number of the current vulnerable MSOA without a hospital
  vuln_msoa_row_id <- curr_msoas %>% 
    st_set_geometry(NULL) %>% 
    filter(Code == vuln_msoa) %>% 
    select(Code, row_num)
  
  # Get IDs of MSOAs with hospitals that neighbour the current vulnerable MSOA without one
  neighbouring_ids <- vuln_msoa_neighbours %>% 
    filter(row.id == vuln_msoa_row_id$row_num) %>% 
    select(col.id)
  
  # Extract MSOA codes that contain hospitals and neighbour this current MSOA
  neighbouring_msoas <- curr_msoas %>% 
    st_set_geometry(NULL) %>% 
    filter(row_num %in% neighbouring_ids$col.id) %>% 
    select(MSOA11CD = Code)
  
  neighbouring_hospitals = bind_rows(neighbouring_hospitals,
                                     neighbouring_msoas)
  
}

neighbouring_hospitals %>% write_csv("data/list of neighbouring MSOAs with hospitals.csv")

# Get list of hospitals that are either in a vulnerable MSOA or neighbour one that doesn't contain a hospital
hospitals_final = hospitals_sp %>% 
  filter(Socioeconomic.Vulnerability.decile >= 9 | MSOA11CD %in% neighbouring_hospitals$MSOA11CD)

# Save
hospitals_final %>% 
  write_csv("data/hospitals-vulnerability.csv")

hospitals_final %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  write_sf("data/hospitals-vulnerability.shp")
