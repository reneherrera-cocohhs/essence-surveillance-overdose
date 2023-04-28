# Introduction #### 
# query essence api for cdc all drug and cdc opioid overdose
# save to pin 

# Setup #### 
# package libraries 
library(here)
library(tidyverse)
library(Rnssp)
library(janitor)
library(lubridate)
library(pins)

# setup pin board 
pins_folder <- board_folder(
  path = "data-raw"
)

# set Rnnsp credentials
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)

# function to query the essence api for counts
query_essence_api_cts <- function(x){
  # url
  x
  
  # replace start date to be 91 days before today
  y <- str_replace(
    string = x,
    pattern = "(?<=startDate=)\\d{2}\\w{3}\\d{4}",
    replacement = str_c(
      format(today()-91, format = "%d%b%Y")
    )
  )
  
  # replace end date to be 7 days before today
  z <- str_replace(
    string = y,
    pattern = "(?<=endDate=)\\d{2}\\w{3}\\d{4}",
    replacement = str_c(
      format(today()-7, format = "%d%b%Y")
    )
  )
  
  # use updated url to query ESSENCE 
  api_data <- get_api_data(z)
  
  # return create a tidy data table 
  api_data$timeSeriesData %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(
      date = ymd(date)
    )
}

# cdc all drug v2
all_drug <- query_essence_api_cts(x = "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?startMonth=1&ddInformative=1&graphOnly=true&geography=15919&geography=33622&geography=33177&datasource=va_hosp&startDate=15Jan2023&medicalGroupingSystem=essencesyndromes&userId=4887&stratVal=hospitalGrouping&patientLoc=az_coconino&endDate=22Apr2023&percentParam=noPercent&admissionTypeCategory=e&graphOptions=single&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20all%20drug%20v2&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=weekly&hasBeenE=1")


# save to pin board 
pin_write(
  board = pins_folder,
  x = all_drug,
  name = "cdc-all-drug-v2",
  type = "rds",
  title = "CDC All Drug v2",
  description = "CDC All Drug v2",
  metadata = list(
    user = "rherrera",
    owner = "Coconino HHS",
    department = "Epidemiology"
  )
)

# cdc opioid overdose v3
opioid_od <- query_essence_api_cts(x = "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?startMonth=1&ddInformative=1&graphOnly=true&geography=15919&geography=33622&geography=33177&datasource=va_hosp&startDate=15Jan2023&medicalGroupingSystem=essencesyndromes&userId=4887&stratVal=hospitalGrouping&patientLoc=az_coconino&endDate=22Apr2023&percentParam=noPercent&admissionTypeCategory=e&graphOptions=single&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20opioid%20overdose%20v3&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=weekly&hasBeenE=1")

# save to pin board 
pin_write(
  board = pins_folder,
  x = opioid_od,
  name = "cdc-opioid-overdose-v3",
  type = "rds",
  title = "CDC Opioid Overdose v3",
  description = "CDC Opioid Overdose v3",
  metadata = list(
    user = "rherrera",
    owner = "Coconino HHS",
    department = "Epidemiology"
  )
)
