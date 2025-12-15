library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
# library(epitools)
library(ggthemes)
library(viridis)
library(skimr)
library(dplyr)

raw_data <- read.csv("data/raw/NEP-2017-23.csv")
saveRDS(raw_data, "data/raw/raw_data.rds")

district_lookup <- read.csv("data/raw/nepal_province_centroids.csv")

# Distributing Variables into their "types"

# Date Type
date_cols <- c("dob", "dnot", "doi", "date_last_mcv", "date_last_rcv", "donset_f", 
               "donset_r", "date_spec_sero", "date_sero_sent", "date_sero_rec", 
               "date_mea_ig_m_result", "date_rub_ig_m_result", "date_viro_spec_coll", 
               "date_viro_sent", "date_viro_rec", "date_mea_geno_result", 
               "date_rub_geno_result", "donset")

age_var <- c("ageyear", "agemon", "mon", "agegrp", "agegrp_o", "agegrp_rvc", "mcvdosegrp", "rcvdosegrp", "rcvdosegrp_rcv")

cat_var <- c("province", "district", "block", "sex", "ccc", "spec_id_sero", 
             "measles_ig_m", "rubella_ig_m", "spec_id_viro", "genotype_mea", 
             "genotype_rub", "class", "agegrp", "agegrp_o", 
             "agegrp_rvc", "mcvdosegrp", "rcvdosegrp", "rcvdosegrp_rcv")


factor_list <- c("sex", "ccc", "measles_ig_m", "rubella_ig_m", "genotype_mea", 
                 "genotype_rub", "class", "agegrp", "agegrp_o", 
                 "agegrp_rvc", "mcvdosegrp", "rcvdosegrp", "rcvdosegrp_rvc")



raw_data <- raw_data %>%
  janitor::clean_names()


# Inspecting variable types
str(raw_data)
summary(raw_data)

unique(clean_data$sex)


#### CLEAN DATA ####

# Imported dataset with clean list of districts and provinces
district_ref <- district_lookup %>%
  # Turning all "" into NAs
  mutate(across(where(is.character), ~na_if(str_trim(.x), ""))) %>%

  # Removing the bonus three lines at the bottom so that its a list of JUST 77 districts
  filter(!if_all(everything(), is.na)) %>%
  
  # standardizing district strings
  mutate(district_std = district %>%
           str_to_lower() %>%
           str_trim() %>%
           str_replace_all("\\s+", " "))

# Table List to fix the mistakees between district_ref and clean_data
district_fix <- c(
  "terhathum" = "tehrathum",
  "tanahu" = "tanahun",
  "kapilvastu" = "kapilavastu",
  "dolkha" = "dolakha",
  "sindhupalchowk" = "sindhupalchok",
  "kavre" = "kavrepalanchok",
  
  "nawalparasi_e" = "parasi",
  "nawalparasi_w" = "nawalpur",
  "nawalparasi" = "nawalpur",
  
  "rukum_e" = "eastern rukum",
  "rukum_w" = "western rukum",
  "rukum" = "eastern rukum"
)

clean_data <- raw_data %>%

    # Type Cleaning
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>% # Empty Strings --> NA
  
  # Date Variables 
  mutate(across(all_of(date_cols), ~as.Date(.x, format = "%d-%b-%y"))) %>%
  
  # Other Variables
  mutate(
    # ccc
    ccc = recode(ccc,
                 "1-Yes" = "1-Yes",
                 "1-YES" = "1-Yes",
                 "2-No" = "2-No", 
                 "2-NO" = "2-No", 
                 "9-Unknown" = "9-Unknown"),
    # AgeGrp
    agegrp = case_when(
      agegrp %in% c("A. <9 M", "A. 0 - 11", "A.<01", "B. 9-12 M") ~ "A. <1 y",
      agegrp %in% c("B. 12 - 59", "B.01-04", "C. 1-4 yrs") ~ "B. 1-4 y",
      agegrp %in% c("C. 60 - 119", "C.05-09", "D. 5-9 yrs") ~ "C. 5-9 y", 
      agegrp %in% c("D. 120 - 179", "D.10-14", "E. 10-14 yrs") ~ "D. 10-14 y",
      agegrp %in% c("E. 180 - 299", "E.15-24", "F. 15-24 yrs") ~ "E. 15-24 y",
      agegrp %in% c("F. 25-39", "G. 25-39 yrs") ~ "F. 25-39 y",
      agegrp %in% c("G. >=40", "H. 40+ yrs") ~ "G. 40+ y",
      TRUE ~ "H. Unknown"
    ),
    
    # AGEGRP_O
    agegrp_o = gsub("\\s+", "", agegrp_o),
    
    # AGEGRP_RVC
    agegrp_rvc = gsub("\\s", "", agegrp_rvc),
    
    # MCVDOSEGRP
    mcvdosegrp = recode(mcvdosegrp, 
                        "A. 0 dose" = "A. 0 dose",
                        "A.0 dose" = "A. 0 dose",
                        "B. 1 dose" = "B. 1 dose", 
                        "B.01 dose" = "B. 1 dose", 
                        "C. 2+ doses" = "C. 2+ doses", 
                        "C.>=2 doses" = "C. 2+ doses", 
                        "D. Unknown" = "D. Unknown", 
                        "D.Unknown" = "D. Unknown"),
    # RCVDOSEGRP
    rcvdosegrp = recode(rcvdosegrp, 
                        "A. 0 dose" = "A. 0 dose",
                        "A.0 dose" = "A. 0 dose",
                        "B. 1 dose" = "B. 1 dose", 
                        "B.01 dose" = "B. 1 dose", 
                        "C. 2+ doses" = "C. 2+ doses", 
                        "C.>=2 doses" = "C. 2+ doses", 
                        "D. Unknown" = "D. Unknown", 
                        "D.Unknown" = "D. Unknown"),
    
    # RCVDOSEGRP
    rcvdosegrp_rvc = recode(rcvdosegrp_rvc,
                            "A.0 dose" = "A. 0 dose", 
                            "B.1+ dose" = "B. 1+ dose", 
                            "C.Unknown" = "C. Unknown")
    ) %>%
  
  # Value Cleaning & Standardization
  mutate(across(all_of(factor_list), factor)) %>%
  
  # Standardizing district strings
  mutate(district_std = district %>%
           str_to_lower() %>%
           str_trim() %>%
           str_replace_all("\\s+", " ")) %>%
  
  # Applying district_fix to clean_data
  mutate(district_std = recode(district_std, !!!district_fix)) %>%
  
  # Joining province information to refix current provinces
  left_join(district_ref %>%
              select(district_std, province),
            by = "district_std") %>%
  
  # Standardize province as an ordered factor
#   mutate(province.y = factor(province.y, levels = c("1. Khoshi", "2. Madhesh", "3. Bagmati", "4. Gandaki", "5. Lumbini", "6. Karnali", "7. Sudurpaschim"), ordered = TRUE)) %>%
  
  # Droping and Renaming district/province variables
  select(-province.x) %>% # Drop the original province
  select(-district) %>%
  rename(province = province.y) %>%
  rename(district = district_std) %>%
  
  # Filtering out the dates outside of the time frame
  filter(is.na(doi) | doi >= as.Date("2017-01-01")) %>% # Now: 11372 obs x 54 variables
  filter(is.na(donset_f) | donset_f >= as.Date("2017-01-01")) %>% # Now: 11371 obs x 54 variables
  filter(is.na(date_sero_sent) | date_sero_sent >= as.Date("2017-01-01")) %>% # Now: 11371 obs x 54 variables
  filter(is.na(date_mea_ig_m_result) | date_mea_ig_m_result >= as.Date("2017-01-01")) %>% # Now: 11368 x 54 variables
  filter(is.na(date_rub_ig_m_result) | date_rub_ig_m_result >= as.Date("2017-01-01"))  %>% # Still 11368 x 54 variables
  filter(is.na(date_viro_rec) | date_viro_rec >= as.Date("2017-01-01")) %>% # Now 11366 x 54 variables
  
  # Any additionally calculated variables
  # Reporting delay
  mutate(reporting_delay = as.numeric(difftime(dnot, donset, units = "days")), district = str_to_lower(district)) %>%
  
  # Age at Onset
  mutate(age_at_onset = as.numeric(doi-dob)/365.25)

for(v in factor_list) {
  cat("----", v, "----\n")
  print(levels(clean_data[[v]]))
  cat("\n")
}

# Diagnosing mismatches between district values BEFORE JOINING (critical)
anti_join(clean_data, district_ref, by = "district_std") %>%
  distinct(district)

# Validate the join of district_ref onto clean_data
clean_data %>%
  filter(is.na(province.y)) %>%
  count(district)

clean_data %>%
  distinct(province.y) %>%
  arrange(province.y)

# Counting to determine where any dates are OUTSIDE of the 2017-01-01 start date
sum(clean_data$doi < as.Date("2017-01-01"), na.rm = TRUE)
sum(clean_data$donset_f < as.Date("2017-01-01"), na.rm = TRUE)# -> 1
sum(clean_data$date_sero_sent < as.Date("2017-01-01"), na.rm = TRUE) # -> 1
sum(clean_data$date_mea_ig_m_result < as.Date("2017-01-01"), na.rm = TRUE) # -> 2
sum(clean_data$date_rub_ig_m_result < as.Date("2017-01-01"), na.rm = TRUE) # -> 2
sum(clean_data$date_viro_rec < as.Date("2017-01-01"), na.rm = TRUE) # -> 2
