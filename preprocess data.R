##
## Preprocess data for Shiny dashboard
##
library(tidyverse)
library(lubridate)
library(readxl)
library(nomisr)
library(httr)
library(sf)
library(jsonlite)

# Local Authority Districts Names and Codes in the United Kingdom
lads = read_csv("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv")

# ---- Vulnerability Index ----
vi = read_sf("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-UK.geojson")

# lookup local authorities that each MSOA is in
msoa_lad = read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20msoa%20to%20lad.csv")

# save a local copy
vi %>% 
  left_join(msoa_lad, by = c("Code" = "MSOA11CD")) %>% 
  write_sf("data/vulnerability.geojson")

# ---- Weekly infection rates ----
# Fetch National COVID-19 surveillance data report from https://www.gov.uk/government/publications/national-covid-19-surveillance-reports
# This URL corresponds to up to the 2 October 2020 (week 40) - so data up til the end of week 39:
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/923669/Weekly_COVID19_report_data_w40.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

covid = read_excel(tf, sheet = "Figure 12. All weeks rates UTLA", skip = 7)

# Clean the data
covid = covid %>% 
  select(LAD19CD = `UTLA code`, starts_with("week")) %>% 
  filter(str_sub(LAD19CD, 1, 1) == "E") %>% 
  mutate(across(starts_with("week"), as.numeric))


# Hackney and City of London boroughs are counted as one. Split into separate rows, using the same data for each
covid = bind_rows(covid %>% filter(LAD19CD != "E09000012/E09000001"),
                  covid %>% filter(LAD19CD == "E09000012/E09000001") %>% mutate(LAD19CD = "E09000012"),
                  covid %>% filter(LAD19CD == "E09000012/E09000001") %>% mutate(LAD19CD = "E09000001"))


# Calculate average rate for past three weeks, plus get latest rate
covid_sum = covid %>% 
  rowwise() %>% 
  mutate(`Mean infection rate over last 3 weeks` = mean(c_across((ncol(covid) - 2):ncol(covid)), na.rm = TRUE)) %>% 
  mutate(`SD infection rate over last 3 weeks` = sd(c_across((ncol(covid) - 2):ncol(covid)), na.rm = TRUE)) %>% 
  mutate(`upper`=`Mean infection rate over last 3 weeks` + `SD infection rate over last 3 weeks`) %>%
  mutate(`lower`=`Mean infection rate over last 3 weeks` - `SD infection rate over last 3 weeks`) %>%
  select(LAD19CD, `Latest infection rate` = ncol(covid), `Mean infection rate over last 3 weeks`, `upper`, `lower`)

#print(covid_sum)

covid_raw = lads %>% 
  select(LAD19CD, Name = LAD19NM) %>% 
  left_join(covid, by = "LAD19CD")

#renaming so will match with england avg.
covid_raw <- rename_with(covid_raw, ~ gsub("week 0", "", .x, fixed = TRUE))
covid_raw <- rename_with(covid_raw, ~ gsub("week ", "", .x, fixed = TRUE))


unlink(tf); rm(tf)

# ----- England cases per 100,000 per week ------ #should this be UK now?
# https://coronavirus.data.gov.uk/
# API guide
# https://coronavirus.data.gov.uk/developers-guide 
AREA_TYPE = "nation"
AREA_NAME = "england"

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create filters:
filters <- c(
  sprintf("areaType=%s", AREA_TYPE),
  sprintf("areaName=%s", AREA_NAME)
)

# Create the structure as a list or a list of lists:
structure <- list(
  date  = "date", 
  name  = "areaName", 
  code  = "areaCode", 
  cases = list(
    daily = "newCasesByPublishDate")
)


# The "httr::GET" method automatically encodes 
# the URL and its parameters:
httr::GET(
  # Concatenate the filters vector using a semicolon.
  url = endpoint,
  
  # Convert the structure to JSON (ensure 
  # that "auto_unbox" is set to TRUE).
  query = list(
    filters   = paste(filters, collapse = ";"),
    structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
  ),
  
  # The API server will automatically reject any
  # requests that take longer than 10 seconds to 
  # process.
  timeout(10)
) -> response

# Handle errors:
if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- content(response, "text")
daily_cases_eng <- jsonlite::fromJSON(json_text)
daily_cases_eng <- daily_cases_eng$data


#changed to using strftime with %V option as it calculates the week in the year based on date so you don't have to have a complete week like in lubridate (i think)
daily_cases_eng$week <- strftime(daily_cases_eng$date, format="%V")

# sum by week - divide by population * 100,000 to get cases per 100000 for each week
#ONS population of the UK: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates#:~:text=The%20UK%20population%20was%20estimated,the%20year%20to%20mid%2D2018.
#ONS population mid year 2019 estimates - UK 66,696,807, England 56,286,961
eng_cases_per_100000 <- daily_cases_eng %>% group_by(week) %>% summarise(week_cases_per_100000=round((sum(cases)/56286961)*100000,2))

# Match weeks to covid_raw
# so that calculation is always correct using column heading from covid raw 
headings <- colnames(covid_raw)
# - infection rate data starts at week five
starts_at <- as.numeric(headings[3])
# last column heading is current last week of lad infection rate data
ends_at <-as.numeric(tail(headings, n=1))

eng_cases_per_100000$week <- as.numeric(as.character(eng_cases_per_100000$week))
eng_cases_per_100000 <-  filter(eng_cases_per_100000, week >= starts_at & week <= ends_at)


#eng_cases_per_100000 <- eng_cases_per_100000[-c(1:4),]
#eng_cases_per_100000 <- eng_cases_per_100000[-c(37:38),]


#to merge with covid raw 
eng_cases_per_100000_wide <- eng_cases_per_100000 %>% pivot_wider(c(week), names_from=week, values_from=week_cases_per_100000)
eng_cases_per_100000_wide <- eng_cases_per_100000_wide %>% mutate(LAD19CD='England',.before=`5`) %>% mutate(Name='England',.before=`5`)

#print(eng_cases_per_100000_wide)
#add row to covid raw
covid_raw = covid_raw %>% add_row(eng_cases_per_100000_wide)

#write to file
write_csv(covid_raw, 'data/all_covid_infection_rate_data.csv')

# ---- Shielding ----
# Coronavirus Shielded Patient List, England - Local Authority: https://digital.nhs.uk/data-and-information/publications/statistical/mi-english-coronavirus-covid-19-shielded-patient-list-summary-totals/latest
# this is the data for up to 08/10/2020
shielded = read_csv("https://files.digital.nhs.uk/6E/B9AB85/Coronavirus%20Shielded%20Patient%20List%2C%20England%20-%20Open%20Data%20with%20CMO%20DG%20-%20LA%20-%202020-10-08.csv")

shielded = shielded %>% 
  # keep only latest values (if more than one extraction happens to be in this file)
  mutate(`Extract Date` = dmy(`Extract Date`)) %>% 
  filter(`Extract Date` == max(`Extract Date`)) %>% 
  
  filter(`LA Code` != "ENG") %>%  # don't need England-wide figures
  filter(`Breakdown Field` == "ALL") %>%  #don't need age/gender splits
  
  select(LAD19CD = `LA Code`, `Clinically extremely vulnerable` = `Patient Count`)


# ---- Ethnicity ----
##
## Load LA-level data from the Annual Population Survey:
## - Percentage of population who are {white or ethnic minority} and {UK born or not}
## - Percentage of working-age population who are {white or ethnic minority} and {UK born or not}
## - Unemployment rate: {white or ethnic minority} and {UK born or not}
## - Economic inactivity rate: {white or ethnic minority} and {UK born or not}
##
## Based on this URL: https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.jsonstat.json?geography=1811939329...1811939332,1811939334...1811939336,1811939338...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,943718401...943718419,943718421...943718463,943718465...943718497,943718499...943718512,943718420,943718464,943718498&date=latest&variable=861...868,873...880&measures=20599,21001,21002,21003
#### - testing - ###
## new url test: https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.jsonstat.json?geography=1820327937...1820328307&date=latest&variable=861...864&measures=20599,21001,21002,21003
#x <- nomis_data_info(id='NM_17_5')
#a <- nomis_search(name = '*Population*', keywords = c('population','annual'))
#test <- filter(a, name.value == 'annual population survey (variables (percentages))')
####

aps_raw = nomis_get_data(
  id = "NM_17_5",
  date = "latest",
  geography = "TYPE432",  # 2019 LAD
  variable = "861...864",  # "861...868,873...880",
  measures = "20599,21001,21002,21003",
  # variables to keep
  select = c(
    "GEOGRAPHY_CODE",
    "VARIABLE_NAME",
    "MEASURES_NAME",
    "OBS_VALUE"
  )
)

aps = aps_raw %>% 
  filter(MEASURES_NAME == "Variable") %>% 
  select(LAD19CD = GEOGRAPHY_CODE, Variable = VARIABLE_NAME, Value = OBS_VALUE) %>% 
  pivot_wider(names_from = Variable, values_from = Value)


# ---- Asylum ----
# download the latest stats on Section 95 support by local authority - https://www.gov.uk/government/statistical-data-sets/asylum-and-resettlement-datasets
# https://www.gov.uk/government/statistical-data-sets/asylum-and-resettlement-datasets
# This URL corresponds to data from June 2020:
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/910573/section-95-support-local-authority-datasets-jun-2020.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

asylum_raw = read_excel(tf, sheet = "Data - Asy_D11")  # check the sheet name is still valid if you're updating the URL above

# convert date column
asylum = asylum_raw %>% 
  mutate(Date = as.Date(`Date (as at…)`, format = "%d %b %Y"))

# get rid of rows with totals and keep only LADs with refugees
asylum = asylum %>% 
  filter(Date == max(asylum$Date)) %>% 
  select(LAD19CD = `LAD Code`, Support = `Support sub-type`, People) %>% 
  pivot_wider(names_from = Support, values_from = People, values_fn = list(People = sum), values_fill = list(People = 0)) %>% 
  
  mutate(`People receiving Section 95 support` = `Dispersed Accommodation` + `Subsistence Only`)

unlink(tf); rm(tf)

# ---- Deprivation ----
## Load LA-level deprivation scores
## - from: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
## - source: File 10: local authority district summaries
##
## We'll use two summary measures of deprivation in LAs: extent and proportion of Lower-layer Super Output Areas in most deprived 10% nationally
## - extent =  the proportion of the local population that live in areas classified as among the most deprived in the country
## 
## (see Table 3.2 in the IMD technical report for more details: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833951/IoD2019_Technical_Report.pdf)
##
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
    write_disk(tf_imd <- tempfile(fileext = ".xlsx")))

imd = read_excel(tf_imd, sheet = "IMD") 

imd = imd %>% 
  select(LAD19CD = `Local Authority District code (2019)`, Extent = `IMD 2019 - Extent`)

unlink(tf_imd); rm(tf_imd)

# Welsh IMD
wimd = read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/Welsh%20IMD%20-%20Local%20Authorities.csv") %>% 
  select(LAD19CD, Extent)

# Scottish IMD
simd = read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/Scottish%20IMD%20-%20Local%20Authorities.csv") %>% 
  select(LAD19CD, Extent)

# Welsh IMD
ni_imd = read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/NI%20IMD%20-%20Local%20Authorities.csv") %>% 
  select(LAD19CD, Extent)

imd = bind_rows(imd, wimd, simd, ni_imd)


# ---- Furlough ----
# https://www.gov.uk/government/statistics/coronavirus-job-retention-scheme-statistics-september-2020
# This URL corresponds to data from Sept 2020: next update will be on october 22nd
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/910962/CJRS_Statistics_August_2020_tables.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

furlough_raw <- read_excel(tf,
                          sheet = "5. Local Authority",
                          skip = 5)

# Keep only data
furlough <-
  furlough_raw %>% 
  filter(rowMeans(is.na(.)) < 1) %>% 
  slice(-422:-432)

# Rename cols
furlough <-
  furlough %>% 
  select(LAD19CD = `County and district / unitary authority Codes`,
         LAD19NM = `County and district / unitary authority`,
         "Furlough count" = `Employments furloughed`,
         "Furlough rate" = `Take-up rate`)

# Keep only LA codes and separate rows with duplicate LA codes and write to disk
furlough <-
  furlough %>% 
  filter(!str_detect(LAD19CD, "^E9") &
           !str_detect(LAD19CD, "^E1") &
           !str_detect(LAD19CD, "^W9") &
           !str_detect(LAD19CD, "^S9") &
           !str_detect(LAD19CD, "^N9")) %>% 
  separate_rows(LAD19CD, sep = ", ")

# ---- Homeless ----
# Load VI with homelessness rate
vi_msoa <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/output/vulnerability-MSOA-England.csv")

# Load lookup table
lookup <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv") %>% 
  select(MSOA11CD, LAD19CD)

# Extract variables from vi
homeless_msoa <-
  vi_msoa %>% 
  select(MSOA11CD = Code,
         Homelessness)

# Join lookup
homeless_msoa <-
  homeless_msoa %>% 
  left_join(lookup, by = "MSOA11CD")

# Aggregate to LA
homeless <- 
  homeless_msoa %>% 
  group_by(LAD19CD) %>% 
  summarise(Homelessness = mean(Homelessness))

# ---- Merge and save ----
la_data <-
  lads %>% 
  select(LAD19CD, Name = LAD19NM) %>% 
  left_join(covid_sum, by = "LAD19CD") %>% 
  left_join(shielded, by = "LAD19CD") %>% 
  left_join(imd, by = "LAD19CD") %>% 
  left_join(aps, by = "LAD19CD") %>% 
  left_join(asylum, by = "LAD19CD") %>% 
  left_join(furlough, by = "LAD19CD") %>% 
  left_join(homeless, by = "LAD19CD")

write_csv(la_data, "data/local authority stats.csv")
