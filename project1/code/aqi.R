library(RAQSAPI)
library(tidyverse)

# set credentials
# TO RUN YOURSELF YOU WILL NEED TO SET YOUR OWN CREDENTIALS 
# aqs_credentials("alice_paul@brown.edu", "")

# cbsa is core based statistical areas
cbsa_df <- data.frame(
  marathon = c("Boston", "NYC", "Chicago", "Twin Cities", "Grandmas"),
  cbsa = c("14460", "35620", "16980", "33460", "20260")
)

# read in marathon dates and merge
marathon_dates <- read.csv("marathon_dates.csv")
marathon_dates$date <- as.Date(marathon_dates$date)
full_df <- left_join(marathon_dates, cbsa_df, by = "marathon")

# parameters for collection
aqi_parameters <- c(88101, 88502, 44201)

# empty results data frame
results_df <- data.frame(
  cbsa_code = character(0), 
  state_code = character(0), 
  county_code = character(0), 
  site_number = character(0), 
  date_local = character(0), 
  parameter_code = character(0),
  units_of_measure = character(0), 
  sample_duration = character(0), 
  aqi = integer(0),
  arithmetic_mean = numeric(0))

# loop through dates/marathons
for(i in 1:nrow(full_df)){
  next_df <- aqs_dailysummary_by_cbsa(parameter = aqi_parameters,
                           bdate = full_df$date[i],
                           edate = full_df$date[i],
                           cbsa_code = full_df$cbsa[i]) %>%
    dplyr::select(c("cbsa_code", "state_code", "county_code", 
                    "site_number", "date_local", "parameter_code",
                    "units_of_measure", "sample_duration", "aqi",
                    "arithmetic_mean"))
  results_df <- rbind(results_df, next_df)
}

results_df <- left_join(results_df, cbsa_df, by = c("cbsa_code" = "cbsa"))
write.csv(results_df, "aqi_values.csv", row.names = FALSE)

