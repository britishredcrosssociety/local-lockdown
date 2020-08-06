##
## Find potential sites for mobile testing based on whether they are in or surrounded by highly vulnerable neighbourhoods
## - hospitals
## - car parks (to do)
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
hospitals_eng = read_delim("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/hospital-data/Hospital.csv", delim = "¬")
hospitals_eng = read_csv("data/hospitals-england.csv")
# hospitals_wal = read_excel("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/hospital-data/Wales%20hospital%20list.xls")

# Filter out 'Mental Health Hospital'
hospitals_eng %>% 
  filter(SubType != "Mental Health Hospital")

# download and unzip the ONS Postcode Directory (February 2020): https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-february-2020
GET("https://www.arcgis.com/sharing/rest/content/items/82889274464b48ae8bf3e9458588a64b/data",
    write_disk(tf <- tempfile(fileext = ".zip")))

unzip(tf, exdir = "data/postcodes")
unlink(tf); rm(tf)

# load postcode directory and keep only relevant fields - we just need MSOA
postcodes_raw = read_csv("data/postcodes/Data/ONSPD_FEB_2020_UK.csv")
postcodes = postcodes_raw %>% dplyr::select(Postcode = pcd, msoa11, oslaua)

# the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
postcodes$Postcode2 = gsub(" ", "", postcodes$Postcode)
hospitals_eng$Postcode2 = gsub(" ", "", hospitals_eng$Postcode)

# keep only hospitals in high vulnerability area
hospitals_eng_vi = hospitals_eng %>% 
  left_join(postcodes, by = "Postcode2") %>%
  left_join(vi, by = c("msoa11" = "Code")) %>% 
  
  filter(Socioeconomic.Vulnerability.decile >= 9)

# convert to spatial dataframe and save
hospitals_eng_vi %>% 
  dplyr::select(OrganisationName, Sector, ParentName, Address1, Address2, Address3, City, Postcode = Postcode.x, Website, LAD19CD = oslaua, MSOA11CD = msoa11, Longitude, Latitude) %>% 
  filter(!is.na(Longitude) & !is.na(Latitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  write_sf("data/hospitals.shp")

# save as .csv too
hospitals_eng_vi %>% 
  dplyr::select(OrganisationName, Sector, ParentName, Address1, Address2, Address3, City, Postcode = Postcode.x, Website, LAD19CD = oslaua, MSOA11CD = msoa11, Longitude, Latitude) %>% 
  write_csv("data/hospitals.csv")


# ---- Find hospitals whose neighbouring MSOAs are highly vulnerable ----
# potential method:
# make spatial dataframe containing only MSOA polygons where the hospitals are
# use st_touches / st_intersects to find the neighbours

hospitals_sp = hospitals_eng %>% 
  filter(!is.na(Longitude) & !is.na(Latitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

plot(st_make_grid(vi, cellsize = .1))

hospitals_sp[1,]

st_relate(vi, hospitals_sp[1,], "F***T****")

st_relate(hospitals_sp[1,], vi)







