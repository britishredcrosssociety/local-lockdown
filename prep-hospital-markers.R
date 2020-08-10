# ---- Load libraries ----
library(tidyverse)
library(sf)

# ---- Load data ----
hospitals <- read_sf("data/hospital-vulnerability.shp")

# ---- Clean data ----
# Keep only hospitals that are in vuln MSOA or border vuln neighbors
vuln_hospitals <- hospitals %>%
  filter(vl_nghb == 1 |
           vul_are == 1)

# Save
vuln_hospitals %>% 
  write_sf("data/hospital-markers.shp")
