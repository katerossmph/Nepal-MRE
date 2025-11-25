library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
# library(epitools)
library(ggthemes)
library(viridis)
library(skimr)
library(dplyr)

raw_data <- read.csv("NEP-2017-23.csv")
saveRDS(raw_data, "data/raw/raw_data.rds")

## FUNCTIONS
clean_igm <- function(x) {
  x_clean <- tolower(trimws(x)) 
  
  case_when(
    x_clean %in% c("1-Positive") ~ "positive",
    x_clean %in% c("2-Negative") ~ "negative",
    x_clean %in% c("3-Equivocal") ~ "equivocal",
    x_clean %in% c("4-Pending")  ~ "pending",
    x_clean %in% c("5-Not Tested") ~ "not tested",
    TRUE ~ NA_character_
  )
}


#### CLEAN DATA ####
clean_data <- raw_data 

clean_data <- clean_data%>%
  janitor::clean_names() %>%
  mutate(
    # Standardizing Variables
    
    # Date Variables
    dob = as_date(dob),
    dnot = as_date(dnot),
    doi = as_date(doi),
    date_last_mcv = as_date(date_last_mcv),
    date_last_rcv = as_date(date_last_rcv),
    donset_f = as_date(donset_f),
    donset_r = as_date(donset_r),
    date_spec_sero = as_date(date_spec_sero),
    date_sero_sent = as_date(date_sero_sent),
    date_sero_rec = as_date(date_sero_rec),
    date_mea_ig_m_result = as_date(date_mea_ig_m_result),
    date_rub_ig_m_result = as_date(date_rub_ig_m_result),
    date_viro_spec_coll = as_date(date_viro_spec_coll),
    date_viro_sent = as_date(date_viro_sent),
    date_viro_rec = as_date(date_viro_rec),
    date_mea_geno_result = as_date(date_mea_geno_result),
    date_rub_geno_result = as_date(date_rub_geno_result),
    donset = as_date(donset)
  ) %>%
  
  # mutate(doi = if_else(year(doi) < 2017 | year(doi) > 2023, as.Date(NA), doi))
  
  mutate(reporting_delay = as.numeric(difftime(dnot, donset, units = "days")), district = str_to_lower(district)) %>%
  
  mutate(age_at_onset = as.numeric(doi-dob)/365.25) %>%
  
  mutate(
    measles_ig_m_clean = case_when(
      measles_ig_m == "1-Positive" ~ "positive",
      measles_ig_m == "2-Negative" ~ "negative",
      measles_ig_m == "3-Equivocal" ~ "equivocal",
      measles_ig_m == "4-Pending" ~ "pending",
      measles_ig_m == "5-Not Tested" ~ "not tested",
      TRUE ~ NA_character_
    ),
    rubella_ig_m_clean = case_when(
      rubella_ig_m == "1-Positive" ~ "positive",
      rubella_ig_m == "2-Negative" ~ "negative",
      rubella_ig_m == "3-Equivocal" ~ "equivocal",
      rubella_ig_m == "4-Pending" ~ "pending",
      rubella_ig_m == "5-Not Tested" ~ "not tested",
      TRUE ~ NA_character_
    )
  ) %>%
  
  mutate(case_status = case_when(
    class %in% c("1-Clinically Confirmed Measles", "2-Laboratory Confirmed Measles",
                 "3-Epidemiologically Confirmed Measles") ~ "measles_positive",
    class %in% c("4-Laboratory Confirmed Rubella", 
                 "5-Epidemiologically Confirmed Rubella") ~ "rubella-positive",
    class == "6-Discarded" ~ "negative",
    class == "7-Pending" ~ "pending",
    TRUE ~ NA_character_
  )) 

