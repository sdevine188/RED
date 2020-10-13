library(tidyverse)
library(lubridate)
library(janitor)
library(xlsx)
library(testthat)
library(UpSetR)
library(skimr)

# setwd
setwd("X:/Asylum_EAD_study")

options(scipen = 999)


# load validate_anumbers function
source("code/helper_scripts/validate_anumbers.R")

# load get_invalid_anumbers function
source("code/helper_scripts/get_invalid_anumbers.R")

# load as_percent function
source("code/helper_scripts/as_percent.R")

# load add_group_index()
source("code/helper_scripts/add_group_index.R")

# load get_variation_functions()
source("code/helper_scripts/get_variation_functions.R")

# load fy()
source("code/helper_scripts/fy.R")

# load add_dummies()
source("code/helper_scripts/add_dummies.R")


##############################


# load i589 data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
i589_part1 <- read_csv("data/I-589/OP&S Data Extract FY2008 to FY2013.csv", col_types = cols(`Reopen Date` = col_character()))
i589_part2 <- read_csv("data/I-589/OP&S Data Extract FY2014 to FY2018.csv", col_types = cols(`Reopen Date` = col_character()))
i589_original <- bind_rows(i589_part1, i589_part2)
i589_original %>% glimpse()
i589_original %>% nrow() # 1128838
i589_original %>% distinct(`Principal Applicant A Number`) %>% nrow() # 673800

# rename variables
i589 <- i589_original %>% rename(anumber = `Principal Applicant A Number`, case_id = `Affirmative Case ID`,
                                 filing_date = `Filing Date`, status_at_entry = `Principal Applicant Status at Entry`,
                                 first_name = `Principal Applicant First Name`, middle_name = `Principal Applicant Middle Name`,
                                 last_name = `Principal Applicant Last Name`, street_address = `Street Address`,
                                 apt_number = `Apartment Number`, city = `City`, state = `State`,
                                 zip_code = `Zip Code`, gender = `Principal Applicant Gender`,
                                 marital_status = `Principal Marital Status`, date_of_birth = `Principal State of Birth`,
                                 country_of_birth = `Principal Applicant Country of Birth`, citizenship = `Principal Applicant Citizenship`,
                                 ethnicity = `Principal Applicant Ethnicity`, port_of_entry = `Principal Applicant Port of Entry`,
                                 date_of_entry = `Principal Applicant Date of Entry`, interview_outcome = `Interview Outcome`,
                                 interview_date = `Interview Date`, interview_location = `Interview Location`, 
                                 basis_of_claim = `Basis of Claim`,
                                 case_control_office_code = `Case Control Office`, decision_outcome = `Decision Outcome`, 
                                 decision_date = `Decision Date`, denial_referral_decision_reason = `Denial/Referral Decision Reason`,
                                 service_date = `Service Date`, service_outcome = `Service Outcome`, 
                                 close_reason = `Close Reason`, close_date = `Close Date`, 
                                 reopen_date = `Reopen Date`, attorney_flag = `Applicant has Attorney`,
                                 dep_anumber = `Dependent A Number`,
                                 dep_date_of_birth = `Dependent Date of Birth`, dep_first_name = `Dependent First Name`,
                                 dep_middle_name = `Dependent Middle Name`, dep_last_name = `Dependent Last Name`,
                                 dep_relationship_desc = `Dependent Relationship to Principal`, 
                                 dep_country = `Dependent Country of Birth`,
                                 dep_citizenship = `Dependent Citizenship`, dep_gender = `Dependent Gender`,
                                 dep_status_at_entry = `Dependent Status at Entry`, 
                                 dep_date_of_entry = `Dependent Date of Entry`, 
                                 dep_port_of_entry = `Dependent Port of Entry`)


###########################


# clean variables
i589 <- i589 %>% mutate(anumber = str_c("A", anumber), dep_anumber = str_c("A", dep_anumber),
                        date_of_birth = as.Date(date_of_birth), date_of_entry = as.Date(date_of_entry),
                        interview_date = as.Date(interview_date), close_date = as.Date(close_date),
                        decision_date = as.Date(decision_date), reopen_date = ymd(reopen_date),
                        filing_date_fy = fy(filing_date), decision_date_fy = fy(decision_date), 
                        dep_date_of_entry = as.Date(dep_date_of_entry),
                        dep_date_of_birth = as.Date(dep_date_of_birth),
                        case_control_office = case_when(case_control_office_code == "ZAR" ~ "Arlington, VA",
                                                        case_control_office_code == "ZCH" ~ "Chicago, IL",
                                                        case_control_office_code == "ZBO" ~ "Boston, MA",
                                                        case_control_office_code == "ZHN" ~ "Houston, TX",
                                                        case_control_office_code == "ZLA" ~ "Los Angeles, CA",
                                                        case_control_office_code == "ZMI" ~ "Miami, FL",
                                                        case_control_office_code == "ZNK" ~ "Newark, NJ",
                                                        case_control_office_code == "ZNY" ~ "New York, NY",
                                                        case_control_office_code == "ZOL" ~ "New Orleans, LA",
                                                        case_control_office_code == "ZSF" ~ "San Francisco, CA"))
i589 %>% glimpse()
i589 %>% select(contains("date")) %>% glimpse()
i589 %>% validate_anumbers(duplicates_allowed = TRUE)
i589 %>% filter(!is.na(dep_anumber)) %>% validate_anumbers(anumber_var = "dep_anumber", duplicates_allowed = TRUE)
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800


##########################


# clean citizenship and country
# note that raio primarily reports based on citizenship, not cob

# load crosswalk_country_to_region file
crosswalk_country_to_region <- read_csv("code/helper_scripts/crosswalk_country_to_region.csv")

# inspect
crosswalk_country_to_region
crosswalk_country_to_region %>% arrange(country_code) %>% distinct(country_code, country_name, output_country_name) %>% print(n = nrow(.))

# show the country names that don't match with crosswalk_country_to_region
i589 %>% distinct(country_of_birth, citizenship)
i589 %>% distinct(country_of_birth) %>% anti_join(., crosswalk_country_to_region, by = c("country_of_birth" = "country_name"))
i589 %>% mutate(citizenship = str_to_title(citizenship)) %>% distinct(citizenship) %>% 
        anti_join(., crosswalk_country_to_region, by = c("citizenship" = "country_name")) %>% print(n = nrow(.))


# standardize country names in citizenship and country_of_birth variables, 
# then join crosswalk_country_to_region for both citizenship and country_of_birth to get region_name and output_country_name vars
i589 <- i589 %>% mutate(citizenship = str_to_title(citizenship),
                        citizenship = case_when(citizenship == "Ussr" ~ "Soviet Union (former)",
                                                citizenship == "Soviet Union (Former)" ~ "Soviet Union (former)",
                                                citizenship == "China, People's Republic Of" ~ "China, People's Republic",
                                                citizenship == "The Gambia" ~ "Gambia",
                                                citizenship == "Trinidad And Tobago" ~ "Trinidad and Tobago",
                                                citizenship == "Congo" ~ "Congo, Republic",
                                                citizenship == "South Korea" ~ "Korea, South",
                                                citizenship == "North Korea" ~ "Korea, North",
                                                citizenship == "Korea" ~ "Korea, South",
                                                citizenship == "Dem Rep Of The Congo" ~ "Congo, Democratic Republic",
                                                citizenship == "Serbia And Montenegro" ~ "Serbia and Montenegro (former)",
                                                citizenship == "Serbia And Montenegro (Former)" ~ "Serbia and Montenegro (former)",
                                                citizenship == "Yugoslavia" ~ "Serbia and Montenegro (former)",
                                                citizenship == "Côte D'ivoire" ~ "Cote d'Ivoire",
                                                citizenship == "Cote D'ivoire" ~ "Cote d'Ivoire",
                                                citizenship == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
                                                citizenship == "Bosnia And Herzegovina" ~ "Bosnia and Herzegovina",
                                                citizenship == "Cape Verde" ~ "Cabo Verde",
                                                citizenship == "St. Lucia" ~ "Saint Lucia",
                                                citizenship == "St. Vincent-Grenadines" ~ "Saint Vincent and the Grenadines",
                                                citizenship == "Saint Vincent And The Grenadines" ~ "Saint Vincent and the Grenadines",
                                                citizenship == "Antigua-Barbuda" ~ "Antigua and Barbuda",
                                                citizenship == "Antigua And Barbuda" ~ "Antigua and Barbuda",
                                                citizenship == "St. Kitts-Nevis" ~ "Saint Kitts and Nevis",
                                                citizenship == "Saint Kitts And Nevis" ~ "Saint Kitts and Nevis",
                                                citizenship == "Sao Tome And Principe" ~ "Sao Tome and Principe",
                                                citizenship == "Czechoslovakia" ~ "Czechoslovakia (former)",
                                                citizenship == "Czechoslovakia (Former)" ~ "Czechoslovakia (former)",
                                                citizenship == "Czech Republic" ~ "Czechia", TRUE ~ citizenship),
                        country_of_birth = case_when(country_of_birth == "Soviet Union" ~ "Soviet Union (former)",
                                                     country_of_birth == "China" ~ "China, People's Republic",
                                                     country_of_birth == "THE GAMBIA" ~ "Gambia",
                                                     country_of_birth == "Congo (Brazzaville)" ~ "Congo, Republic",
                                                     country_of_birth == "Congo (Kinshasa)" ~ "Congo, Democratic Republic",
                                                     country_of_birth == "Serbia and Montenegro" ~ "Serbia and Montenegro (former)",
                                                     country_of_birth == "Yugoslavia" ~ "Serbia and Montenegro (former)",
                                                     country_of_birth == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                                     country_of_birth == "Czechoslovakia" ~ "Czechoslovakia (former)",
                                                     country_of_birth == "Bahamas, The" ~ "Bahamas",
                                                     country_of_birth == "Netherlands Antilles" ~ "Netherlands Antilles (former)",
                                                     country_of_birth == "Democratic Republic of the Congo" ~ "Congo, Democratic Republic",
                                                     country_of_birth == "Palestine" ~ "Unknown", TRUE ~ country_of_birth)) %>%
        left_join(., crosswalk_country_to_region %>% select(-country_code) %>% distinct(), by = c("citizenship" = "country_name")) %>%
        rename(citizenship_us_protectorate_flag = us_protectorate_flag, citizenship_region_name = region_name, 
               citizenship_output_country_name = output_country_name) %>%
        left_join(., crosswalk_country_to_region %>% select(-country_code) %>% distinct(), by = c("country_of_birth" = "country_name")) %>%
        rename(country_of_birth_us_protectorate_flag = us_protectorate_flag, country_of_birth_region_name = region_name, 
               country_of_birth_output_country_name = output_country_name)

# inspect
i589 %>% anti_join(., crosswalk_country_to_region, by = c("country_of_birth" = "country_name"))
i589 %>% anti_join(., crosswalk_country_to_region, by = c("citizenship" = "country_name"))
i589 %>% filter(is.na(country_of_birth))
i589 %>% distinct(anumber, country_of_birth) %>% count(country_of_birth) %>% arrange(desc(n)) %>% print(n = nrow(.))
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800


#########################################################################################################################################


# clean dep_citizenship and dep_country

# show the country names that don't match with crosswalk_country_to_region
i589 %>% distinct(dep_country, dep_citizenship)
i589 %>% distinct(dep_country) %>% anti_join(., crosswalk_country_to_region, by = c("dep_country" = "country_name"))
i589 %>% mutate(dep_citizenship = str_to_title(dep_citizenship)) %>% distinct(dep_citizenship) %>% 
        anti_join(., crosswalk_country_to_region, by = c("dep_citizenship" = "country_name")) %>% print(n = nrow(.))

# standardize country names in citizenship and country_of_birth variables, 
# then join crosswalk_country_to_region for both dep_citizenship and dep_country to get region_name and output_country_name vars
i589 <- i589 %>% mutate(dep_citizenship = str_to_title(dep_citizenship),
                        dep_citizenship = case_when(dep_citizenship == "Ussr" ~ "Soviet Union (former)",
                                                dep_citizenship == "Soviet Union (Former)" ~ "Soviet Union (former)",
                                                dep_citizenship == "China, People's Republic Of" ~ "China, People's Republic",
                                                dep_citizenship == "The Gambia" ~ "Gambia",
                                                dep_citizenship == "Trinidad And Tobago" ~ "Trinidad and Tobago",
                                                dep_citizenship == "Congo" ~ "Congo, Republic",
                                                dep_citizenship == "South Korea" ~ "Korea, South",
                                                dep_citizenship == "North Korea" ~ "Korea, North",
                                                dep_citizenship == "Korea" ~ "Korea, South",
                                                dep_citizenship == "Dem Rep Of The Congo" ~ "Congo, Democratic Republic",
                                                dep_citizenship == "Serbia And Montenegro" ~ "Serbia and Montenegro (former)",
                                                dep_citizenship == "Serbia And Montenegro (Former)" ~ "Serbia and Montenegro (former)",
                                                dep_citizenship == "Yugoslavia" ~ "Serbia and Montenegro (former)",
                                                dep_citizenship == "Côte D'ivoire" ~ "Cote d'Ivoire",
                                                dep_citizenship == "Cote D' Ivore" ~ "Cote d'Ivoire",
                                                dep_citizenship == "Cote D'ivoire" ~ "Cote d'Ivoire",
                                                dep_citizenship == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
                                                dep_citizenship == "Bosnia And Herzegovina" ~ "Bosnia and Herzegovina",
                                                dep_citizenship == "Cape Verde" ~ "Cabo Verde",
                                                dep_citizenship == "St. Lucia" ~ "Saint Lucia",
                                                dep_citizenship == "St. Vincent-Grenadines" ~ "Saint Vincent and the Grenadines",
                                                dep_citizenship == "Saint Vincent And The Grenadines" ~ "Saint Vincent and the Grenadines",
                                                dep_citizenship == "Antigua-Barbuda" ~ "Antigua and Barbuda",
                                                dep_citizenship == "Antigua And Barbuda" ~ "Antigua and Barbuda",
                                                dep_citizenship == "St. Kitts-Nevis" ~ "Saint Kitts and Nevis",
                                                dep_citizenship == "Saint Kitts And Nevis" ~ "Saint Kitts and Nevis",
                                                dep_citizenship == "Sao Tome And Principe" ~ "Sao Tome and Principe",
                                                dep_citizenship == "Czechoslovakia" ~ "Czechoslovakia (former)",
                                                dep_citizenship == "Czechoslovakia (Former)" ~ "Serbia and Montenegro (former)",
                                                dep_citizenship == "Czech Republic" ~ "Czechia", TRUE ~ dep_citizenship),
                        dep_country = case_when(dep_country == "Soviet Union" ~ "Soviet Union (former)",
                                                     dep_country == "China" ~ "China, People's Republic",
                                                     dep_country == "THE GAMBIA" ~ "Gambia",
                                                     dep_country == "Congo (Brazzaville)" ~ "Congo, Republic",
                                                     dep_country == "Congo (Kinshasa)" ~ "Congo, Democratic Republic",
                                                     dep_country == "Serbia and Montenegro" ~ "Serbia and Montenegro (former)",
                                                     dep_country == "Yugoslavia" ~ "Serbia and Montenegro (former)",
                                                     dep_country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                                     dep_country == "Czechoslovakia" ~ "Czechoslovakia (former)",
                                                     dep_country == "Bahamas, The" ~ "Bahamas",
                                                     dep_country == "Netherlands Antilles" ~ "Netherlands Antilles (former)",
                                                     dep_country == "Democratic Republic of the Congo" ~ "Congo, Democratic Republic",
                                                     dep_country == "Palestine" ~ "Unknown", TRUE ~ dep_country)) %>%
        left_join(., crosswalk_country_to_region %>% select(-country_code) %>% distinct(), by = c("dep_citizenship" = "country_name")) %>%
        rename(dep_citizenship_us_protectorate_flag = us_protectorate_flag, dep_citizenship_region_name = region_name, 
               dep_citizenship_output_country_name = output_country_name) %>%
        left_join(., crosswalk_country_to_region %>% select(-country_code) %>% distinct(), by = c("dep_country" = "country_name")) %>%
        rename(dep_country_us_protectorate_flag = us_protectorate_flag, dep_country_region_name = region_name, 
               dep_country_output_country_name = output_country_name)

# inspect
i589 %>% anti_join(., crosswalk_country_to_region, by = c("dep_country" = "country_name"))
i589 %>% anti_join(., crosswalk_country_to_region, by = c("dep_citizenship" = "country_name"))
i589 %>% filter(is.na(dep_country))
i589 %>% distinct(anumber, dep_country) %>% count(dep_country) %>% arrange(desc(n)) %>% print(n = nrow(.))
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% summarize_at(.vars = everything(.), .funs = ~ sum(is.na(.))) %>% 
        pivot_longer(cols = everything(), names_to = "var", values_to = "final_na_count") %>% arrange(desc(final_na_count)) %>% print(n = nrow(.))


#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################


# consolidate i589 data to one row per record
i589_placeholder <- i589
# i589 <- i589_placeholder


###############################################################################################################################################


# apply raio business rules to classify admin closed and pending decision_outcomes

# admin closed
# Any record with either 1) a Closed Date but no Service Date, 
# or 2) the most recent Closed Date being subsequent to the most recent Service Date, and most_recent reopen_date is either NA or preceding most_recent close_date 
i589 <- i589 %>% 
        group_by(anumber) %>% 
        mutate(max_close_date = max(close_date, na.rm = TRUE), 
                max_service_date = max(service_date, na.rm = TRUE),
                max_decision_date = max(decision_date, na.rm = TRUE),
                max_reopen_date = max(reopen_date, na.rm = TRUE)) %>% 
        mutate(max_close_date = case_when(max_close_date == -Inf ~ as.Date(NA_character_), TRUE ~ max_close_date),
               max_service_date = case_when(max_service_date == -Inf ~ as.Date(NA_character_), TRUE ~ max_service_date),
               max_decision_date = case_when(max_decision_date == -Inf ~ as.Date(NA_character_), TRUE ~ max_decision_date),
               max_reopen_date = case_when(max_reopen_date == -Inf ~ as.Date(NA_character_), TRUE ~ max_reopen_date)) %>%
        ungroup() %>%
        mutate(admin_closed = case_when(is.na(max_service_date) & !is.na(max_close_date) ~ 1,
                                        !is.na(max_service_date) & !is.na(max_close_date) & max_close_date > max_service_date &
                                                (is.na(max_reopen_date) | max_reopen_date < max_close_date) ~ 1, TRUE ~ 0)) 


############################


# inspect admin_closed
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, admin_closed) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(admin_closed == 1) %>% distinct(anumber) %>% nrow() # 67059
i589 %>% distinct(anumber, admin_closed, decision_outcome) %>% count(decision_outcome, admin_closed)
i589 %>% filter(admin_closed == 1) %>% distinct(anumber, decision_outcome) %>% count(decision_outcome)
i589 %>% filter(admin_closed == 1, !is.na(decision_outcome)) %>% distinct(anumber) %>%
        left_join(., i589, by = "anumber") %>%
        select(anumber, decision_outcome, decision_date, max_decision_date, service_date, max_service_date, close_date, max_close_date, admin_closed)


# note that the "no non-missing arguments to max; returning -Inf" warning you get when running the admin_closed code above
# does not actually return "-Inf" value for dates, like it does for numeric values (see example below)
# and the below check confirms that although you can filter on date == -Inf, the values it shows are all NA, so it seems to treat is as -Inf under the hood
# to fix this and make it treat like NA again, we convert -Inf to NA dates, which then is treated properly

# inspect i589 output (note this will show NA values, but not -Inf, because the code chunk above converts -Inf back to NA)
i589 %>% filter(max_close_date == -Inf | max_decision_date == -Inf | max_service_date == -Inf | max_reopen_date == -Inf) %>%
        select(anumber, service_date, max_service_date, decision_date, max_decision_date, close_date, max_close_date, reopen_date, max_reopen_date)
i589 %>% filter(is.na(max_close_date) | is.na(max_decision_date) | is.na(max_service_date) | is.na(max_reopen_date)) %>%
        select(anumber, service_date, max_service_date, decision_date, max_decision_date, close_date, max_close_date, reopen_date, max_reopen_date)

# example of how max works with grouped dates, and how -Inf and NA are handled
tibble(var = c(1, NA, 2, NA, NA, NA), group = c(1, 1, 1, 2, 2, 2)) %>% group_by(group) %>% mutate(max = max(var, na.rm = TRUE)) %>% 
        filter(max == -Inf)
tibble(var = ymd("2000-01-01", NA, "2003-01-01", NA, NA, NA), group = c(1, 1, 1, 2, 2, 2)) %>% group_by(group) %>% mutate(max = max(var, na.rm = TRUE)) %>% 
        filter(max == -Inf)
tibble(var = ymd("2000-01-01", NA, "2003-01-01", NA, NA, NA), group = c(1, 1, 1, 2, 2, 2)) %>% group_by(group) %>% mutate(max = max(var, na.rm = TRUE)) %>%
        filter(is.na(max))
tibble(var = ymd("2000-01-01", NA, "2003-01-01", NA, NA, NA), group = c(1, 1, 1, 2, 2, 2)) %>% group_by(group) %>% mutate(max = max(var, na.rm = TRUE)) %>%
        mutate(max = case_when(max == -Inf ~ as.Date(NA_character_), TRUE ~ max)) %>% filter(is.na(max))
tibble(var = ymd("2000-01-01", NA, "2003-01-01", NA, NA, NA), group = c(1, 1, 1, 2, 2, 2)) %>% group_by(group) %>% mutate(max = max(var, na.rm = TRUE)) %>%
        mutate(max = case_when(max == -Inf ~ as.Date(NA_character_), TRUE ~ max)) %>% filter(max == -Inf)
        

#####################


# save to file as temporary placeholder
# setwd("C:/Users/sjdevine/Work Folders/Desktop")
# write_csv(i589, path = "asylum_ead/data/I-589/i589_after_admin_closed_20200226.csv")
# i589_placeholder <- read_csv("asylum_ead/data/I-589/i589_after_admin_closed_20200226.csv",
#                              col_types = cols(reopen_date = col_date(), max_reopen_date = col_date()))
# i589 <- i589_placeholder
# i589 %>% glimpse()


####################################################################################################################################


# pending

# business rule for pending from raio email
# 1) no Closed Date and no decision_outcome (no reference to service_date)
# or 2) a most recent Decision Outcome that is non-terminal (NOID or Recommended Approval), 
# note that technically there are 15 anumbers with terminal outcomes, but non-terminal outcomes dates slightly afterward (eg A208371338, A206876365)
# these will be excluded from pending since they are considered to have a terminal outcome
# a better restatement of statement 2 avoiding this issue is 2b) has no terminal decision_outcome

# note the updated rule has no reference to most recent terminal decisions that lack a service date being considered pending
i589 <- i589 %>% 
        mutate(decision_outcome_flag = ifelse(!is.na(decision_outcome), 1, 0),
               terminal_decision_outcome_flag = ifelse(decision_outcome %in% c("Grant", "Referral", "Deny"), 1, 0),
               service_date_flag = ifelse(!is.na(service_date), 1, 0),
               close_date_flag = ifelse(!is.na(close_date), 1, 0),
               reopen_date_flag = ifelse(!is.na(reopen_date), 1, 0)) %>%
        group_by(anumber) %>% 
        mutate(has_decision_outcome = ifelse(sum(decision_outcome_flag) > 0, 1, 0),
               has_terminal_decision_outcome = ifelse(sum(terminal_decision_outcome_flag) > 0, 1, 0),
               has_service_date = ifelse(sum(service_date_flag) > 0, 1, 0),
               has_close_date = ifelse(sum(close_date_flag) > 0, 1, 0),
               has_reopen_date = ifelse(sum(reopen_date_flag) > 0, 1, 0)) %>%
        ungroup() %>%
        mutate(pending = case_when(has_decision_outcome == 0 & is.na(max_close_date) ~ 1,
                                   has_terminal_decision_outcome == 0 & admin_closed == 0 ~ 1,
                                   TRUE ~ 0)) %>%
        select(-c(decision_outcome_flag, terminal_decision_outcome_flag, service_date_flag, close_date_flag, reopen_date_flag))


##########################


# inspect pending
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
# all anumbers have only one value for pending
i589 %>% distinct(anumber, pending) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(pending == 1) %>% distinct(anumber) %>% nrow() # 267547
i589 %>% filter(pending == 1) %>% distinct(anumber, decision_outcome) %>% count(decision_outcome)
i589 %>% distinct(anumber, pending, admin_closed) %>% count(pending, admin_closed)
i589 %>% filter(pending == 1) %>% 
        select(anumber, decision_date, max_decision_date, decision_outcome,  
               service_date, max_service_date, close_date, max_close_date, admin_closed, pending)

# note there are still some minor data quality issues after applying raio's classification rules
# but these seem relatively minor, and the best choice is to stick with raio's explicit rules, and not try tinkering with assumptions for edge cases
# eg the 162 anumbers below bucket as pending, but have a close_date, no reopen_date, and no decision_date 
i589 %>% filter(has_decision_outcome == 0, has_close_date == 1, admin_closed == 0, (has_reopen_date == 0 | max_close_date > max_reopen_date)) %>%
        distinct(anumber) %>% nrow() # 162
i589 %>% filter(has_decision_outcome == 0, has_close_date == 1, admin_closed == 0, (has_reopen_date == 0 | max_close_date > max_reopen_date)) %>%
        distinct(anumber, filing_date, decision_date, decision_outcome, service_date, close_date, close_reason, reopen_date, admin_closed, pending) # 168

# still, all records bucket into terminal_decision, pending, or admin_closed, which means no records are unclassified
# note some with terminal_decision were subsequently admin_closed
i589 %>% distinct(anumber, has_terminal_decision_outcome, admin_closed, pending) %>% count(has_terminal_decision_outcome, admin_closed, pending)


######################################################################################################################################


# add interviewed_flag
i589 <- i589 %>% 
        mutate(interview_completed_flag = case_when(is.na(interview_outcome) ~ 0, 
                                                    interview_outcome == "Interview Completed" ~ 1, TRUE ~ 0)) %>%  
        group_by(anumber) %>% mutate(interviewed_flag = case_when(sum(interview_completed_flag, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% select(-interview_completed_flag)


#############################


# inspect interviewed_flag
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
# only one value for interviewed_flag per anumber
i589 %>% distinct(anumber, interviewed_flag) %>% count(anumber) %>% arrange(desc(n))
# get count of anumbers that were interviewed
i589 %>% distinct(anumber, interviewed_flag) %>% count(interviewed_flag) # 356695
i589 %>% filter(interview_outcome == "Interview Completed") %>% distinct(anumber) %>% nrow() # 356695
# check how many interview dates each interviewed anumber has
# note that 105 anumbers have two interview_dates associated with interview_completed interview_outcomes
# this is fine because we prune for most recent interview_date below, which makes sense for calculating processing times
i589 %>% filter(interview_outcome == "Interview Completed") %>% distinct(anumber, interview_date) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(interview_outcome == "Interview Completed") %>% distinct(anumber, interview_date) %>% count(anumber) %>%
        filter(n > 1) %>% select(anumber) %>% left_join(., i589, by = "anumber") %>%
        select(anumber, interview_outcome, interview_date, interviewed_flag) # 105


############################################################################################################################################


# add one_year_limit_flag
i589 <- i589 %>% 
        mutate(one_year_limit = case_when(is.na(denial_referral_decision_reason) ~ 0, 
                                          denial_referral_decision_reason == "1-Year Limit" ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>% mutate(one_year_limit_flag = ifelse(sum(one_year_limit, na.rm = TRUE) > 0, 1, 0)) %>%
        ungroup() %>% select(-one_year_limit)


################


# inspect
# note that 36872 anumbers have one_year_limit_flag = 1 but interview_outcome is not Interview Completed
# this seems like a slight data quality issue, since adjudicators should interview applicants before referring to see if they have an authorized exception
# will still bucket these as one-year filing referrals
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, one_year_limit_flag) %>% count(one_year_limit_flag) # 93073
i589 %>% filter(denial_referral_decision_reason == "1-Year Limit") %>% distinct(anumber) %>% nrow() # 93073
i589 %>% filter(one_year_limit_flag == 1) %>% distinct(anumber, one_year_limit_flag, interview_outcome) %>% count(interview_outcome)
i589 %>% filter(one_year_limit_flag == 1, interview_outcome != "Interview Completed") %>% 
        select(anumber, one_year_limit_flag, interview_outcome, denial_referral_decision_reason) # 36872


###############################################################################################################################################


# inspect has_recommended_approval flag
i589 %>% filter(decision_outcome == "Recommended Approval") %>% distinct(anumber) %>% nrow() # 24131


################


# add has_recommended_approval flag
i589 <- i589 %>% mutate(recommended_approval_flag = case_when(decision_outcome == "Recommended Approval" ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>% mutate(has_recommended_approval = case_when(sum(recommended_approval_flag) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% select(-recommended_approval_flag)


################


# inspect
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(has_recommended_approval == 1) %>% distinct(anumber) %>% nrow() # 24131
i589 %>% distinct(anumber, has_recommended_approval) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# add recommended_approval_decision_date
i589 <- i589 %>% filter(decision_outcome == "Recommended Approval") %>% 
        group_by(anumber) %>% arrange(desc(decision_date)) %>% add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>% rename(recommended_approval_decision_date = decision_date) %>%
        distinct(anumber, recommended_approval_decision_date) %>% left_join(i589, ., by = "anumber") 
        

####################


# inspect
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800
# note that all recommended approvals have a decision_date
i589 %>% filter(decision_outcome == "Recommended Approval", is.na(decision_date)) %>% distinct(anumber) %>% nrow() # 0
# and all has_recommended_approval anumbers have a recommended_approval_decision_date
i589 %>% filter(has_recommended_approval == 1, is.na(recommended_approval_decision_date)) %>% distinct(anumber) %>% nrow() # 0
# all anumbers have only one value for recommended_approval_decision_date
i589 %>% distinct(anumber, recommended_approval_decision_date) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


# inspect variable/record variation
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

record_variation <- i589 %>% get_record_variation(id_vars = anumber)
record_variation


###############################################################################################################################################


# inspect filing_date
variable_variation %>% filter(variable == "filing_date")
# note that only 47 anumbers have multiple filing_date, with just one anumber having the max of 3 distinct filing_dates 
record_variation %>% filter(filing_date_n_distinct > 1) %>% select(anumber, filing_date_n_distinct) %>% arrange(desc(filing_date_n_distinct))
i589 %>% distinct(anumber, filing_date) %>% count(anumber) %>% filter(n > 1) # 47
# these 47 anumbers have 221 records
record_variation %>% filter(filing_date_n_distinct > 1) %>% select(anumber, filing_date_n_distinct) %>% arrange(desc(filing_date_n_distinct)) %>%
        left_join(., i589, by = "anumber") %>% select(anumber, case_id, filing_date_n_distinct, filing_date, decision_outcome) # 221
# these 221 records have 146 with filing_date_index > 1, which would be dropped if filtering to just most recent filing_date 
i589 %>% group_by(anumber) %>% arrange(desc(filing_date)) %>% 
        add_group_index(group_vars = filing_date, group_name = "filing_date_index") %>%
        filter(filing_date_index > 1) %>% ungroup() %>% nrow() # 
# every case_id is associated with only one anumber
i589 %>% group_by(case_id) %>% distinct(anumber) %>% count(case_id) %>% arrange(desc(n))
# 88 anumbers have multiple case_ids
i589 %>% group_by(anumber) %>% distinct(case_id) %>% count(anumber) %>% arrange(desc(n)) %>% filter(n > 1)


# get filing_date_pruning_test to check what variables lose variation when simply filtering down to most recent filing_date
# you do lose variation when filtering to most recent filings, but only for those 47 anumbers with multiple filings, which is not a big deal overall
# result: best option is to just filter for most recent filing dates
filing_date_pruning_test <- i589 %>% group_by(anumber) %>% arrange(desc(filing_date)) %>% 
        add_group_index(group_vars = filing_date, group_name = "filing_date_index") %>%
        filter(filing_date_index == 1) %>% ungroup()
filing_date_pruning_test
filing_date_pruning_test %>% nrow() # 1128692
1128692 + 146 == 1128838
filing_date_pruning_test %>% distinct(anumber) %>% nrow() # 673800
filing_date_pruning_test %>% distinct(anumber, filing_date) %>% count(anumber) %>% arrange(desc(n))

# inspect filing_date_pruning_change_in_variable_variation
# no issue: changes were limited to the 47 anumbers with multiple filing dates, which is expected and ok
tictoc::tic()
filing_date_pruning_change_in_variable_variation <- filing_date_pruning_test %>% 
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
tictoc::toc() # 5.5 minutes
filing_date_pruning_change_in_variable_variation %>% print(n = nrow(.))


#################################


# filter down to most recent filing_date
i589 <- i589 %>% group_by(anumber) %>% arrange(desc(filing_date)) %>% 
        add_group_index(group_vars = filing_date, group_name = "filing_date_index") %>%
        filter(filing_date_index == 1) %>% ungroup() %>% select(-filing_date_index)


################################


# inspect
i589 %>% nrow() # 1128692
1128838 - 1128692 == 146
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, filing_date) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

record_variation <- i589 %>% get_record_variation(id_vars = anumber)
record_variation
tictoc::toc() # 5 min


###############################################################################################################################################


# inspect decision_outcomes/dates

# there are no remaining anumbers having multiple different decision_outcomes for a given decision_date
# so each anumber/decision_date combo refers to only a single decision_outcome, so this is no obstacle to taking most recent decision_date
i589 %>% distinct(anumber, decision_date, decision_outcome) %>% group_by(anumber, decision_date) %>% count(decision_outcome) %>% 
        ungroup() %>% arrange(desc(n), anumber)


#############################


# one potential drawback to just taking most recent decision_date are those anum with terminal decision_outcomes that don't have most recent date
terminal_decision_anum_decision_date_index <- i589 %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% 
        distinct(anumber) %>%
        left_join(., i589, by = "anumber") %>% select(anumber, decision_outcome, decision_date) %>%
        group_by(anumber) %>% arrange(desc(decision_date)) %>% 
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>% ungroup()


#############################


# inspect anum with multiple decision_dates
terminal_decision_anum_decision_date_index # 676313
terminal_decision_anum_decision_date_index %>% distinct(anumber) %>% nrow() # 339308
# note that 153 records have terminal decisions for not-most-recent decision, and most-recent decision is non-terminal
terminal_decision_anum_decision_date_index %>% 
        # exclude those with terminal decision for most recent decision_date, since we want to see only those with terminal for not-most-recent decision_date
        # and it's possible that an anumber could have a terminal decision for the most recent decision date, as well as subsequent decision dates
        mutate(most_recent_decision_is_terminal_flag = ifelse(decision_outcome %in% c("Deny", "Grant", "Referral") & decision_date_index == 1, 1, 0)) %>%
        filter(most_recent_decision_is_terminal_flag == 0, decision_outcome %in% c("Deny", "Grant", "Referral"), decision_date_index > 1) %>% 
        distinct(anumber, decision_date_index, most_recent_decision_is_terminal_flag) %>%
        left_join(., i589, by = "anumber") %>% 
        select(anumber, case_id, filing_date, decision_outcome, decision_date, decision_date_index, most_recent_decision_is_terminal_flag) # 153
# 32 anumbers make up those 153 records with terminal decisions for not-most-recent decision, and most-recent decision is non-terminal
terminal_decision_anum_decision_date_index %>% 
        mutate(most_recent_decision_is_terminal_flag = ifelse(decision_outcome %in% c("Deny", "Grant", "Referral") & decision_date_index == 1, 1, 0)) %>%
        filter(most_recent_decision_is_terminal_flag == 0, decision_outcome %in% c("Deny", "Grant", "Referral"), decision_date_index > 1) %>% 
        distinct(anumber) %>% nrow() # 32
# all 32 anumbers with not-most-recent terminal decisions have exactly 2 different decision dates
terminal_decision_anum_decision_date_index %>% 
        mutate(most_recent_decision_is_terminal_flag = ifelse(decision_outcome %in% c("Deny", "Grant", "Referral") & decision_date_index == 1, 1, 0)) %>%
        filter(most_recent_decision_is_terminal_flag == 0, decision_outcome %in% c("Deny", "Grant", "Referral"), decision_date_index > 1) %>% 
        distinct(anumber, decision_date_index) %>%
        left_join(., i589, by = "anumber") %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% 
        distinct(anumber, case_id, filing_date, decision_date, decision_outcome, decision_date_index) %>% 
        add_count(anumber, name = "anumber_count") %>% arrange(desc(anumber_count)) %>% print(n = nrow(.))

# only 5 anumbers have multiple terminal decision_outcome values (regardless of decision date)
i589 %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% distinct(anumber, decision_outcome) %>% count(anumber) %>% 
        rename(decision_outcome_count = n) %>% filter(decision_outcome_count > 1) %>%
        distinct(anumber) %>% nrow() # 5 
i589 %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% distinct(anumber, decision_outcome) %>% count(anumber) %>% 
        rename(decision_outcome_count = n) %>% filter(decision_outcome_count > 1) %>%
        distinct(anumber) %>% left_join(., i589, by = "anumber") %>% 
        select(anumber, case_id, filing_date, decision_date, decision_outcome) %>% print(n = nrow(.)) # 34

# there are 17 anum with multiple terminal decision_outcome_date combos
i589 %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% distinct(anumber, decision_outcome, decision_date) %>% 
        count(anumber) %>%
        rename(decision_outcome_date_count = n) %>% filter(decision_outcome_date_count > 1) %>% 
        distinct(anumber) %>% nrow() # 17
i589 %>% filter(decision_outcome %in% c("Deny", "Grant", "Referral")) %>% distinct(anumber, decision_outcome, decision_date) %>% 
        count(anumber) %>%
        rename(decision_outcome_date_count = n) %>% filter(decision_outcome_date_count > 1) %>% 
        distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        select(anumber, case_id, filing_date, decision_date, decision_outcome) %>%
        arrange(anumber, filing_date, case_id, decision_date, decision_outcome) %>% distinct() %>% print(n = nrow(.)) # 37

# confirm that 60159 not-most-recent-decision_date records will be dropped from most_recent_decision_date
i589 %>% filter(!(is.na(decision_date))) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>% arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index > 1) %>% nrow() # 60159


#############################


# get most recent decision_date for those with non-NA decision_date, 
# note this ignores the 32 anumbers with terminal decisions that are not-most-recent,
# but its defensible and simple to just take the most recent decision_date, and in any case 32 anumbers is relatively insignificant
# no need to take on complexity to code and explain a jury-rigged solution to a "data quality" issue that might not even be a data quality issue

# note that anumbers with only NA decision_dates will be added with most_recent_decision_date to form an updated i589 below
most_recent_decision_date <- i589 %>% filter(!(is.na(decision_date))) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>% arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>% ungroup() %>% 
        filter(decision_date_index == 1) %>% select(-decision_date_index)


#############################


# inspect most_recent_decision_date
most_recent_decision_date %>% nrow() # 618156
i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% nrow() # 450377
i589 %>% nrow() # 1128692
450377 + 618156 + 60159 == 1128692

most_recent_decision_date %>% distinct(anumber) %>% nrow() # 340209
i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% distinct(anumber) %>% nrow() # 333591
i589 %>% distinct(anumber) %>% nrow() # 673800
340209 + 333591 == 673800

# all anumbers in most_recent_decision_date have only one decision_date
most_recent_decision_date %>% distinct(anumber, decision_date) %>% count(anumber) %>% arrange(desc(n))

# but note there are 910 anumbers with 3856 records and 1820 distinct anum-decision_outcome-decision_date combos  
# who have multiple decision_outcome values for their single most-recent_decision_date
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% arrange(desc(n))
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) # 1820
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>% nrow() # 3586
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% nrow() # 910


# all 910 anumbers with multiple decision_outcome values for their single most-recent_decision_date, have exactly 2 decision_outcome values
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% rename(anumber_count = n) %>% 
        group_by(anumber) %>% mutate(decision_outcome_n_distinct = n_distinct(decision_outcome)) %>% ungroup() %>% 
        arrange(desc(decision_outcome_n_distinct))
# most of these 910 anumbers have recommended_approval -> grant sequences, some have NOID -> Referral
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% count(decision_outcome)
# all 910 anumbers have a terminal decision_outcome
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1, decision_outcome %in% c("Grant", "Referral", "Deny")) %>% distinct(anumber) %>% nrow() # 910
# none of the 910 anumbers have more than one terminal decision_outcome
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        filter(decision_outcome %in% c("Grant", "Referral", "Deny")) %>%
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% nrow() # 0 


# confirm that add_group_index method identifies the same 910 anumbers as having multiple decision_outcome values 
most_recent_decision_date %>% group_by(anumber) %>%
        arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_outcome, group_name = "decision_outcome_index") %>% 
        ungroup() %>% filter(decision_outcome_index > 1) %>% distinct(anumber) %>% nrow() # 910
# confirm that the 910 anumbers have 1793 records of non-terminal decisions that will be dropped 
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>%
        arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_outcome, group_name = "decision_outcome_index") %>% 
        ungroup() %>% filter(decision_outcome_index > 1) %>% nrow() # 1793
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>%
        arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_outcome, group_name = "decision_outcome_index") %>% 
        ungroup() %>% filter(decision_outcome_index > 1) %>% count(decision_outcome)

        
#############################


# remove non-terminal decision_outcome for those 910 anumbers who have multiple decision_outcome values for their single most-recent_decision_date
most_recent_decision_date <- most_recent_decision_date %>% group_by(anumber) %>% 
        arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_outcome, group_name = "decision_outcome_index") %>%
        ungroup() %>% filter(decision_outcome_index == 1) %>% select(-decision_outcome_index)


#############################


# inspect most_recent_decision_date
most_recent_decision_date %>% glimpse()
most_recent_decision_date %>% nrow() # 616363
most_recent_decision_date %>% distinct(anumber) %>% nrow() # 340209
618156 - 616363 == 1793
# all anumbers in most_recent_decision_date have only one decision_date/decision_outcome combo
most_recent_decision_date %>% distinct(anumber, decision_date, decision_outcome) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect before combining anumbers with only NA decision_date with most_recent_decision_date to get updated i589
i589 %>% nrow() # 1128692
i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% nrow() # 450377
most_recent_decision_date %>% nrow() # 616363
# confirm that 60159 not-most-recent-decision_date records will be dropped from most_recent_decision_date
i589 %>% filter(!(is.na(decision_date))) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>% arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index > 1) %>% nrow() # 60159
# confirm that the 910 anumbers have 1793 records of non-terminal decisions that will be dropped 
most_recent_decision_date %>% distinct(anumber, decision_outcome, decision_date) %>% 
        add_count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        group_by(anumber) %>%
        arrange(desc(decision_date), match(x = decision_outcome, table = c("Grant", "Referral", "Deny"))) %>%
        add_group_index(group_vars = decision_outcome, group_name = "decision_outcome_index") %>% 
        ungroup() %>% filter(decision_outcome_index > 1) %>% nrow() # 1793
450377 + 616363 + 60159 + 1793 == 1128692

i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% distinct(anumber) %>% nrow() # 333591
most_recent_decision_date %>% distinct(anumber) %>% nrow() # 340209
333591 + 340209 == 673800 

# create decision_date_pruning_test and get change in variable/record variation
# there is some loss of variation, but nothing unexpected or requiring a fix
# eg 131 anumbers lose some variation on basis_of_claim, but that seems like a legitamte loss from multiple boc entries associated with multiple decision_outcomes
# since we keep the latest decision_outcome, we can defensibly expect to keep the latest boc values
# also some single digit anumber counts losing variation on zip_code, close_date/reason, but these also seem expected and ok
decision_date_pruning_test <- i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% bind_rows(., most_recent_decision_date)
decision_date_pruning_test %>% nrow() # 1066740
i589 %>% nrow() # 1128692
1066740 + 60159 + 1793 == 1128692

# no significant issues in reductions; reductions mainly for decision-related variables, which is expected and ok
decision_date_pruning_test_change_in_variable_variation <- decision_date_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
decision_date_pruning_test_change_in_variable_variation %>% print(n = nrow(.))

decision_date_pruning_test_change_in_record_variation <- decision_date_pruning_test %>%
        get_change_in_record_variation(before_data = i589, after_data = ., id_vars = anumber)

# inspect variation reduction in basis_of_claim
# some valid basis_of_claim values got dropped in favor of some NAs (see A073640372)
# but overall, taking the most recent decision_date is defensible and simple, and the number of potential issues is relatively insignificant
decision_date_pruning_test_change_in_record_variation %>% filter(basis_of_claim_n_distinct < 0) %>%
        select(anumber, basis_of_claim_n_distinct) %>% distinct(anumber) %>% left_join(., decision_date_pruning_test, by = "anumber") %>%
        select(anumber, decision_date, basis_of_claim) %>% arrange(anumber)
decision_date_pruning_test_change_in_record_variation %>% filter(basis_of_claim_n_distinct < 0) %>%
        select(anumber, basis_of_claim_n_distinct) %>% distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        select(anumber, decision_date, basis_of_claim) %>% arrange(anumber)


############################


# combine anumbers with only NA decision_date with most_recent_decision_date to get updated i589
i589 <- i589 %>% filter(!(anumber %in% most_recent_decision_date$anumber)) %>% bind_rows(., most_recent_decision_date)


############################


# inspect
i589 %>% nrow() # 1066740
i589 %>% distinct(anumber) %>% nrow() # 673800
450377 + 616363 == 1066740
i589 %>% distinct(anumber, decision_date) %>% count(anumber) %>% arrange(desc(n))
i589 %>% distinct(anumber, decision_outcome) %>% count(anumber) %>% arrange(desc(n))


########################################################################################################################################################


# prune based on service_date

# inspect
i589 %>% group_by(anumber) %>% arrange(desc(service_date)) %>% 
        add_group_index(group_vars = service_date, group_name = "service_date_index") %>%
        ungroup() %>% filter(service_date_index > 1) %>% 
        distinct(anumber) %>% left_join(., i589, by = "anumber") %>%
        select(anumber, filing_date, decision_date, decision_outcome, interview_date, interview_outcome, 
               service_date, service_outcome, close_date, close_reason, reopen_date)

# inspect not_most_recent service_date records to be dropped
i589 %>% group_by(anumber) %>% arrange(desc(service_date)) %>% 
        add_group_index(group_vars = service_date, group_name = "service_date_index") %>%
        ungroup() %>% filter(service_date_index > 1) %>% nrow() # 9442

# get service_date_pruning_test
service_date_pruning_test <- i589 %>% group_by(anumber) %>% arrange(desc(service_date)) %>% 
        add_group_index(group_vars = service_date, group_name = "service_date_index") %>%
        ungroup() %>% filter(service_date_index == 1)
service_date_pruning_test %>% nrow() # 1057298
service_date_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 1066740
1057298 + 9442 == 1066740

# inspect service_date_pruning_test_change_in_variable_variation
# result: basically only service_date/outcome variation dropped
service_date_pruning_test_change_in_variable_variation <- service_date_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
service_date_pruning_test_change_in_variable_variation %>% print(n = nrow(.))


#####################################


# prune to most recent service_date
i589 <- i589 %>% group_by(anumber) %>% arrange(desc(service_date)) %>% 
        add_group_index(group_vars = service_date, group_name = "service_date_index") %>%
        ungroup() %>% filter(service_date_index == 1) %>% select(-service_date_index)


#####################################


# inspect
i589 %>% nrow() # 1057298
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, service_date) %>% count(anumber) %>% arrange(desc(n))


#####################################################################################################################################################


# inspect service_outcome
# result: only 1124 anumbers have multiple service_outcome, and all of them have only two
# since service_outcome won't be used in the analysis, it can be dropped
i589 %>% distinct(anumber, service_outcome) %>% count(anumber) %>% arrange(desc(n))
i589 %>% distinct(anumber, service_outcome) %>% count(anumber) %>% filter(n > 1) %>% nrow() # 1124
i589 %>% count(service_outcome) 


######################################


# drop service_outcome
i589 <- i589 %>% select(-service_outcome) %>% distinct()


######################################


# inspect
i589 %>% nrow() # 1052357
i589 %>% distinct(anumber) %>% nrow() # 673800


###############################################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

record_variation <- i589 %>% get_record_variation(id_vars = anumber)
record_variation
tictoc::toc() # 5 min


###############################################################################################################################################


# inspect interview_date to see if Interview Completed values are always the most recent interview date
# this ensures that grabbing the records with most recent interview date will not drop any Interview Completed values 
# result: there are only 110 records with interview_completed but not most_recent interview_date
# this is acceptable since taking the most recent interview_date is defensible and simple
i589 %>% group_by(anumber) %>% arrange(desc(interview_date), match(x = interview_outcome, table = c("Interview Completed"))) %>%
        add_group_index(group_vars = interview_date, group_name = "interview_date_index") %>%
        ungroup() %>% filter(interview_outcome == "Interview Completed") %>% 
        distinct(anumber, interview_date_index) %>% count(interview_date_index)

# inspect not_most_recent_interview_date records to be dropped
i589 %>% group_by(anumber) %>% arrange(desc(interview_date), match(x = interview_outcome, table = c("Interview Completed"))) %>%
        add_group_index(group_vars = interview_date, group_name = "interview_date_index") %>%
        ungroup() %>% filter(interview_date_index != 1) %>% nrow() # 212884

# get interview_date_pruning_test
interview_date_pruning_test <- i589 %>% group_by(anumber) %>% arrange(desc(interview_date), match(x = interview_outcome, table = c("Interview Completed"))) %>%
        add_group_index(group_vars = interview_date, group_name = "interview_date_index") %>%
        ungroup() %>% filter(interview_date_index == 1) 
interview_date_pruning_test %>% nrow() # 839473
interview_date_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 1052357
839473 + 212884 == 1052357

# inspect interview_date_pruning_test_change_in_variable_variation
# result: no issues - other than expected drop in interview_date/outcome variation, only single digit variation in a few variables
interview_date_pruning_test_change_in_variable_variation <- interview_date_pruning_test %>% 
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
interview_date_pruning_test_change_in_variable_variation %>% print(n = nrow(.))


############################


# prune down to records with most recent interview_date, or NA records if there is no interview_date 
i589 <- i589 %>% group_by(anumber) %>% arrange(desc(interview_date), match(x = interview_outcome, table = c("Interview Completed"))) %>%
        add_group_index(group_vars = interview_date, group_name = "interview_date_index") %>%
        ungroup() %>% filter(interview_date_index == 1) %>% select(-interview_date_index)


#############################


# inspect
i589 %>% nrow() # 839473
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, interview_date) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect interview_outcome

# only 1997 anumbers have multiple interview_outcomes, with 3 interview_outcome values the max
i589 %>% distinct(anumber, interview_outcome) %>% count(anumber) %>% arrange(desc(n)) %>% filter(n > 1) # 1997
# the two most frequent values are Interview_completed and applicant_no_show
# since interviewed_flag variable is already added, and interview_outcome won't be used in analysis, 
# best choice is to arrange by interview_completed, no-show, etc and just take the first value
i589 %>% distinct(anumber, interview_outcome) %>% count(anumber) %>% filter(n > 1) %>%
        distinct(anumber) %>% left_join(., i589, by = "anumber") %>% distinct(anumber, interview_outcome) %>% count(interview_outcome)

# inspect records to be dropped
i589 %>% group_by(anumber) %>% arrange(match(x = interview_outcome, table = c("Interview Completed", "Applicant No Show"))) %>%
        add_group_index(group_vars = interview_outcome, group_name = "interview_outcome_index") %>% 
        ungroup() %>% filter(interview_outcome_index > 1) %>% nrow() # 2490

# get interview_outcome_pruning_test
interview_outcome_pruning_test <- i589 %>% group_by(anumber) %>% arrange(match(x = interview_outcome, table = c("Interview Completed", "Applicant No Show"))) %>%
        add_group_index(group_vars = interview_outcome, group_name = "interview_outcome_index") %>% 
        ungroup() %>% filter(interview_outcome_index == 1)
interview_outcome_pruning_test %>% nrow() # 836983
interview_outcome_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 839473
836983 + 2490 == 839473

# get interview_outcome_pruning_test_change_in_variable_variation
# no issue: only interview_outcome variation was reduced
interview_outcome_pruning_test_change_in_variable_variation <- interview_outcome_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
interview_outcome_pruning_test_change_in_variable_variation %>% print(n = nrow(.))


##############################


# prune down interview_outcome
i589 <- i589 %>% group_by(anumber) %>% arrange(match(x = interview_outcome, table = c("Interview Completed", "Applicant No Show"))) %>%
        add_group_index(group_vars = interview_outcome, group_name = "interview_outcome_index") %>% 
        ungroup() %>% filter(interview_outcome_index == 1) %>% select(-interview_outcome_index)


#############################


# inspect
i589 %>% nrow() # 836983
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, interview_outcome) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

# record_variation <- i589 %>% get_record_variation(id_vars = anumber)
# record_variation
tictoc::toc() # 5 min


###############################################################################################################################################


# inspect reopen_date and close_date to determine which should be pruned next
# result: best choice is to prune for most_recent close_date before pruning for most_recent reopen_date

# 395 anumbers have multiple reopen_date values, with 3 values the max
# best choice is to take most recent reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% arrange(desc(n)) %>% filter(n > 1) # 395
# inspect full records for those with multiple reopen_dates
# confirmed that record structure records a reopen_date value in the same record as the preceding close_date
# in this structure, a post-reopen_date close_date appears on a new record where the reopen_date is NA
# so taking record with most_recent reopen_date would drop record with most_recent close_date
# and conversely, taking record with most_recent close_date would drop record with most_recent reopen_date

# the best decision though is to keep record with most_recent close_date, 
# since if an anumber has multiple close_dates, and also has a reopen_date (and hence would be affected by this choice) 
# the record with the most recent close date would always be the most recent record, 
# and even if it has an NA for reopen_date on that record, max_reopen_date retains info
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% 
        select(anumber, filing_date, decision_date, decision_outcome, close_date, close_reason, reopen_date, admin_closed, pending)
# note that 378 of the 395 anumbers with multiple reopen_dates are admin_closed
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% distinct(anumber, admin_closed) %>% count(admin_closed)
# and the other 7 of the 395 anumbers with multiple reopen_dates are pending
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% distinct(anumber, pending) %>% count(pending)
# note 184 anumbers have max_close_date > max_reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% filter(max_close_date > max_reopen_date) %>% distinct(anumber) %>% nrow() # 184
# inspect 184 anumbers have max_close_date > max_reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% filter(max_close_date > max_reopen_date) %>% distinct(anumber) %>% 
        left_join(., i589, by = "anumber") %>% distinct(anumber, filing_date, close_date, max_close_date, reopen_date, max_reopen_date, admin_closed, pending)
# 22 anumbers have max_close_date < max_reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% filter(max_close_date < max_reopen_date) %>% distinct(anumber) %>% nrow()
# inspect 22 anumbers have max_close_date < max_reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% filter(max_close_date < max_reopen_date) %>% distinct(anumber) %>% 
        left_join(., i589, by = "anumber") %>% distinct(anumber, filing_date, close_date, max_close_date, reopen_date, max_reopen_date, admin_closed, pending)
# note that 373 of the 395 anumbers have an NA reopen_date and a non-NA reopen_date
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% filter(n > 1) %>%
        left_join(., i589, by = "anumber") %>% 
        select(anumber, filing_date, decision_date, decision_outcome, close_date, close_reason, reopen_date, admin_closed, pending) %>%
        distinct(anumber, reopen_date) %>% mutate(reopen_date_is_na = ifelse(is.na(reopen_date), 1, 0)) %>%
        group_by(anumber) %>% count(reopen_date_is_na) %>% ungroup() %>% count(anumber) %>% arrange(desc(n)) %>% filter(n > 1)


###############################################################################################################################################


# inspect close_date

# 405 anumbers have multiple close_date values, with 3 values the max
# best choice is to take most recent close_date
i589 %>% distinct(anumber, close_date) %>% count(anumber) %>% arrange(desc(n)) %>% filter(n > 1) # 405

# inspect records to be dropped
i589 %>% group_by(anumber) %>% arrange(desc(close_date)) %>% add_group_index(group_vars = close_date, group_name = "close_date_index") %>%
        ungroup() %>% filter(close_date_index > 1) %>% nrow() # 451

# get close_date_pruning_test
close_date_pruning_test <- i589 %>% group_by(anumber) %>% arrange(desc(close_date)) %>%
        add_group_index(group_vars = close_date, group_name = "close_date_index") %>% 
        ungroup() %>% filter(close_date_index == 1)
close_date_pruning_test %>% nrow() # 836532
close_date_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 836983
836532 + 451 == 836983

# get close_date_pruning_test_change_in_variable_variation
# result: as expected, 400ish anumbers w reduced variation on close_date/reason and reopen_date
# there are also low single digit anumber count losing variation on some other variables, but overall insignificant
# best choice is to prune to most recent close_date
close_date_pruning_test_change_in_variable_variation <- close_date_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
close_date_pruning_test_change_in_variable_variation %>% print(n = nrow(.))


##############################


# prune down close_date
i589 <- i589 %>% group_by(anumber) %>% arrange(desc(close_date)) %>%
        add_group_index(group_vars = close_date, group_name = "close_date_index") %>% 
        ungroup() %>% filter(close_date_index == 1) %>% select(-close_date_index)


#############################


# inspect
i589 %>% nrow() # 836532
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, close_date) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect reopen_date
i589 %>% group_by(anumber) %>% arrange(desc(reopen_date)) %>% add_group_index(group_vars = reopen_date, group_name = "reopen_date_index") %>%
        ungroup() %>% filter(reopen_date_index > 1) %>% distinct(anumber) %>% nrow() # 39

# inspect records to be dropped
i589 %>% group_by(anumber) %>% arrange(desc(reopen_date)) %>% add_group_index(group_vars = reopen_date, group_name = "reopen_date_index") %>%
        ungroup() %>% filter(reopen_date_index > 1) %>% nrow() # 43

# get reopen_date_pruning_test
reopen_date_pruning_test <- i589 %>% group_by(anumber) %>% arrange(desc(reopen_date)) %>%
        add_group_index(group_vars = reopen_date, group_name = "reopen_date_index") %>% 
        ungroup() %>% filter(reopen_date_index == 1)
reopen_date_pruning_test %>% nrow() # 836489
reopen_date_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 836532
836489 + 43 == 836532

# get reopen_date_pruning_test_change_in_variable_variation
# no issues: almost only reopen_date/flag variation reduction; single digit reduction for close_reason and case_id 
# best choice is to prune to most recent reopen_date
tictoc::tic()
reopen_date_pruning_test_change_in_variable_variation <- reopen_date_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
reopen_date_pruning_test_change_in_variable_variation %>% print(n = nrow(.))
tictoc::toc()


##############################


# prune to most recent reopen_date
i589 <- i589 %>% group_by(anumber) %>% arrange(desc(reopen_date)) %>%
        add_group_index(group_vars = reopen_date, group_name = "reopen_date_index") %>% 
        ungroup() %>% filter(reopen_date_index == 1) %>% select(-reopen_date_index)


#############################


# inspect
i589 %>% nrow() # 836489
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, reopen_date) %>% count(anumber) %>% arrange(desc(n))


###############################################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

# record_variation <- i589 %>% get_record_variation(id_vars = anumber)
# record_variation
tictoc::toc() # 5 min


###############################################################################################################################################


# inspect citizenship
# only 160 anumbers with multiple citizenship
# best choice is just to take alphabetically first citizenship
i589 %>% group_by(anumber) %>% arrange(citizenship) %>% add_group_index(group_vars = citizenship, group_name = "citizenship_index") %>%
        ungroup() %>% filter(citizenship_index > 1) %>% distinct(anumber) %>% nrow() # 160

# inspect records of anumbers with multiple citizenship values
i589 %>% group_by(anumber) %>% arrange(citizenship) %>% add_group_index(group_vars = citizenship, group_name = "citizenship_index") %>%
        ungroup() %>% filter(citizenship_index > 1) %>% distinct(anumber) %>% 
        left_join(., i589, by = "anumber") %>% select(anumber, filing_date, decision_date, close_date, reopen_date, citizenship)

# inspect records to be dropped
i589 %>% group_by(anumber) %>% arrange(citizenship) %>% add_group_index(group_vars = citizenship, group_name = "citizenship_index") %>%
        ungroup() %>% filter(citizenship_index > 1) %>% nrow() # 321

# get citizenship_pruning_test
citizenship_pruning_test <- i589 %>% group_by(anumber) %>% arrange(citizenship) %>%
        add_group_index(group_vars = citizenship, group_name = "citizenship_index") %>% 
        ungroup() %>% filter(citizenship_index == 1)
citizenship_pruning_test %>% nrow() # 836168
citizenship_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 836489
836168 + 321 == 836489

# get citizenship_pruning_test_change_in_variable_variation
# no issues: almost only citizenship/flag variation reduction; single digit reduction for close_reason and case_id 
# best choice is to prune to most recent citizenship
tictoc::tic()
citizenship_pruning_test_change_in_variable_variation <- citizenship_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
citizenship_pruning_test_change_in_variable_variation %>% print(n = nrow(.))
tictoc::toc()


######################


# prune down citizenship alphabetically
i589 <- i589 %>% group_by(anumber) %>% arrange(citizenship) %>%
        add_group_index(group_vars = citizenship, group_name = "citizenship_index") %>% 
        ungroup() %>% filter(citizenship_index == 1) %>% select(-citizenship_index)



######################


# inspect
i589 %>% nrow() # 836168
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, citizenship) %>% count(anumber) %>% arrange(desc(n))


#####################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

# record_variation <- i589 %>% get_record_variation(id_vars = anumber)
# record_variation
tictoc::toc() # 5 min


###############################################################################################################################################


# inspect basis_of_claim
i589 %>% distinct(anumber, basis_of_claim) %>% count(basis_of_claim) %>% arrange(desc(n))
# 34879 anumbers with multiple basis_of_claim
i589 %>% distinct(anumber, basis_of_claim) %>% count(anumber) %>% filter(n > 1) %>% nrow() # 34879
# the maximum count of basis_of_claim values for a single anumber is 6
i589 %>% distinct(anumber, basis_of_claim) %>% count(anumber) %>% arrange(desc(n))
# 286790 anumbers have at least one basis_of_claim, meaning the remaining 673800 - 286790 = 387010 anumbers have no basis_of_claim
i589 %>% filter(!is.na(basis_of_claim)) %>% distinct(anumber) %>% nrow()
# 286441 of the 286790 anumbers (99.9%) with a basis_of_claim value had interview completed
i589 %>% filter(!is.na(basis_of_claim), interviewed_flag ==1 ) %>% distinct(anumber) %>% nrow()
# the 286441 anum w multiple basis_of_claim constitute 80% of the 356695 anumbers interviewed
i589 %>% filter(interviewed_flag == 1) %>% distinct(anumber) %>% nrow() # 356695

# best choice is to make dummies for each basis_of_claim value, then just drop basis_of_claim and get distinct()
basis_of_claim_pruning_test <- i589 %>% 
        mutate(boc_political_opinion_flag = case_when(basis_of_claim == "Political Opinion" ~ 1, TRUE ~ 0),
               boc_social_group_flag = case_when(basis_of_claim == "Particular Social Group" ~ 1, TRUE ~ 0),
               boc_religion_flag = case_when(basis_of_claim == "Religion" ~ 1, TRUE ~ 0),
               boc_family_planning_flag = case_when(basis_of_claim == "Coercive Family Planning" ~ 1, TRUE ~ 0),
               boc_race_flag = case_when(basis_of_claim == "Race" ~ 1, TRUE ~ 0),
               boc_no_nexus_flag = case_when(basis_of_claim == "No Nexus" ~ 1, TRUE ~ 0),
               boc_nationality_flag = case_when(basis_of_claim == "Nationality" ~ 1, TRUE ~ 0),
               boc_na_flag = case_when(is.na(basis_of_claim) ~ 1, TRUE ~ 0),
               basis_of_claim_flag = case_when(!is.na(boc_political_opinion_flag) | !is.na(boc_social_group_flag) |
                                                       !is.na(boc_religion_flag) | !is.na(boc_family_planning_flag) |
                                                       !is.na(boc_race_flag) | !is.na(boc_nationality_flag) ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>%
        mutate(boc_political_opinion_flag = case_when(sum(boc_political_opinion_flag) > 0 ~ 1, TRUE ~ 0),
               boc_social_group_flag = case_when(sum(boc_social_group_flag) > 0 ~ 1, TRUE ~ 0),
               boc_religion_flag = case_when(sum(boc_religion_flag) > 0 ~ 1, TRUE ~ 0),
               boc_family_planning_flag = case_when(sum(boc_family_planning_flag) > 0 ~ 1, TRUE ~ 0),
               boc_race_flag = case_when(sum(boc_race_flag) > 0 ~ 1, TRUE ~ 0),
               boc_no_nexus_flag = case_when(sum(boc_no_nexus_flag) > 0 ~ 1, TRUE ~ 0),
               boc_nationality_flag = case_when(sum(boc_nationality_flag) > 0 ~ 1, TRUE ~ 0),
               boc_na_flag = case_when(sum(boc_na_flag) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% select(-basis_of_claim) %>% distinct()

# inspect 
basis_of_claim_pruning_test %>% glimpse()
basis_of_claim_pruning_test %>% nrow() # 790814
basis_of_claim_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 836168
# confirm same anumber counts per basis_of_claim
i589 %>% distinct(anumber, basis_of_claim) %>% count(basis_of_claim) %>% arrange(desc(n))
basis_of_claim_pruning_test %>% filter(boc_na_flag == 1) %>% distinct(anumber) %>% nrow() # 387010
basis_of_claim_pruning_test %>% filter(boc_political_opinion_flag == 1) %>% distinct(anumber) %>% nrow() # 112673
basis_of_claim_pruning_test %>% filter(boc_social_group_flag == 1) %>% distinct(anumber) %>% nrow() # 90887
basis_of_claim_pruning_test %>% filter(boc_religion_flag == 1) %>% distinct(anumber) %>% nrow() # 71253
basis_of_claim_pruning_test %>% filter(boc_family_planning_flag == 1) %>% distinct(anumber) %>% nrow() # 26189
basis_of_claim_pruning_test %>% filter(boc_race_flag == 1) %>% distinct(anumber) %>% nrow() # 10083
basis_of_claim_pruning_test %>% filter(boc_no_nexus_flag == 1) %>% distinct(anumber) %>% nrow() # 9262
basis_of_claim_pruning_test %>% filter(boc_nationality_flag == 1) %>% distinct(anumber) %>% nrow() # 6572

# get change in variable variation
# no change to any remaning variables, only basis_of_claim was dropped and boc flags added
tictoc::tic()
basis_of_claim_pruning_test_change_in_variable_variation <- basis_of_claim_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
basis_of_claim_pruning_test_change_in_variable_variation %>% print(n = nrow(.))
tictoc::toc()


##############################


# create basis_of_claim flags
i589 <- i589 %>% 
        mutate(boc_political_opinion_flag = case_when(basis_of_claim == "Political Opinion" ~ 1, TRUE ~ 0),
               boc_social_group_flag = case_when(basis_of_claim == "Particular Social Group" ~ 1, TRUE ~ 0),
               boc_religion_flag = case_when(basis_of_claim == "Religion" ~ 1, TRUE ~ 0),
               boc_family_planning_flag = case_when(basis_of_claim == "Coercive Family Planning" ~ 1, TRUE ~ 0),
               boc_race_flag = case_when(basis_of_claim == "Race" ~ 1, TRUE ~ 0),
               boc_no_nexus_flag = case_when(basis_of_claim == "No Nexus" ~ 1, TRUE ~ 0),
               boc_nationality_flag = case_when(basis_of_claim == "Nationality" ~ 1, TRUE ~ 0),
               boc_na_flag = case_when(is.na(basis_of_claim) ~ 1, TRUE ~ 0),
               basis_of_claim_flag = case_when(!is.na(boc_political_opinion_flag) | !is.na(boc_social_group_flag) |
                                                       !is.na(boc_religion_flag) | !is.na(boc_family_planning_flag) |
                                                       !is.na(boc_race_flag) | !is.na(boc_nationality_flag) ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>%
        mutate(boc_political_opinion_flag = case_when(sum(boc_political_opinion_flag) > 0 ~ 1, TRUE ~ 0),
               boc_social_group_flag = case_when(sum(boc_social_group_flag) > 0 ~ 1, TRUE ~ 0),
               boc_religion_flag = case_when(sum(boc_religion_flag) > 0 ~ 1, TRUE ~ 0),
               boc_family_planning_flag = case_when(sum(boc_family_planning_flag) > 0 ~ 1, TRUE ~ 0),
               boc_race_flag = case_when(sum(boc_race_flag) > 0 ~ 1, TRUE ~ 0),
               boc_no_nexus_flag = case_when(sum(boc_no_nexus_flag) > 0 ~ 1, TRUE ~ 0),
               boc_nationality_flag = case_when(sum(boc_nationality_flag) > 0 ~ 1, TRUE ~ 0),
               boc_na_flag = case_when(sum(boc_na_flag) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% select(-basis_of_claim) %>% distinct()


######################


# inspect
i589 %>% nrow() # 790814
i589 %>% distinct(anumber) %>% nrow() # 673800


#############################################################################################################################################


# inspect variable/record variation
tictoc::tic()
variable_variation <- i589 %>% get_variable_variation(id_vars = anumber)
variable_variation %>% print(n = nrow(.))

# record_variation <- i589 %>% get_record_variation(id_vars = anumber)
# record_variation
tictoc::toc() # 5 min


#############################################################################################################################################


# inspect case_id
# note 3 anumbers have two case_ids each
# since it's only 3 anumbers, which is insignificant, i will just take the first case_id sequentially
i589 %>% distinct(anumber, case_id) %>% count(anumber) %>% filter(n > 1) 
i589 %>% distinct(anumber, case_id) %>% count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>%
        left_join(., i589, by = "anumber") %>% select(anumber, case_id)
i589 %>% distinct(anumber, case_id) %>% count(anumber) %>% filter(n > 1) %>% distinct(anumber) %>%
        left_join(., i589, by = "anumber") %>% select(anumber, case_id)

# get case_id_pruning_test
case_id_pruning_test <- i589 %>% group_by(anumber) %>% arrange(case_id) %>% 
        add_group_index(group_vars = case_id, group_name = "case_id_index") %>% 
        ungroup() %>% filter(case_id_index == 1)
case_id_pruning_test %>% nrow() # 790811
case_id_pruning_test %>% distinct(anumber) %>% nrow() # 673800
i589 %>% nrow() # 790814

# get change_in_variable_variation
# no issues - just case_id reduced variation
tictoc::tic()
case_id_pruning_test_change_in_variable_variation <- case_id_pruning_test %>%
        get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = anumber)
case_id_pruning_test_change_in_variable_variation %>% print(n = nrow(.))
tictoc::toc()


##########################


# prune case_id sequentially
i589 <- i589 %>% group_by(anumber) %>% arrange(case_id) %>% 
        add_group_index(group_vars = case_id, group_name = "case_id_index") %>% 
        ungroup() %>% filter(case_id_index == 1) %>% select(-case_id_index)


##########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800


#############################################################################################################################


# save temporary placeholder
# setwd("C:/Users/sjdevine/Work Folders/Desktop")
# write_csv(i589, path = "asylum_ead/data/I-589/i589_after_case_id_pruning_20200408.csv")
# i589 <- read_csv("asylum_ead/data/I-589/i589_after_case_id_pruning_20200408.csv",
#                  col_types = cols(reopen_date = col_date(), max_reopen_date = col_date()))
# i589 %>% glimpse()
# i589 %>% nrow() # 790811
# i589 %>% distinct(anumber) %>% nrow() # 673800


#############################################################################################################################


# inspect interviewed_flag to make sure how closely it matches the pruned values of interview_outcome
# note that interviewed_flag will be dropped, and is just being used as a diagnostic to confirm that pruning process didn't significantly alter records

# note that all remaining records with interview_completed have interviewed_flag = 1, as expected
i589 %>% filter(interview_outcome == "Interview Completed", interviewed_flag == 0) %>% nrow() # 0
i589 %>% distinct(anumber, interview_outcome, interviewed_flag) %>% count(interview_outcome, interviewed_flag)
# however, there are 108 anumbers with interviewd_flag = 1, but remaining record is not interview completed
i589 %>% filter(interview_outcome != "Interview Completed", interviewed_flag == 1) %>% distinct(anumber) %>% nrow() # 108
# inspect records for those 108 anumbers with interviewd_flag = 1, but remaining record is not interview completed
i589 %>% filter(interview_outcome != "Interview Completed", interviewed_flag == 1) %>% distinct(anumber, interviewed_flag) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        select(anumber, filing_date, interview_date, interview_outcome, interviewed_flag, decision_date, decision_outcome, denial_referral_decision_reason) %>%
        print(n = 25)
# all the interview_outcomes for these 108 anumbers are a variant of reschedule request
i589 %>% filter(interview_outcome != "Interview Completed", interviewed_flag == 1) %>% 
        distinct(anumber, interview_outcome) %>% count(interview_outcome)
# 107 of the 108 anumbers has a terminal decision_outcome, so it bolsters the case that they were in fact interviewed, 
# despite the reschedule interview_outcome on the most recent record
i589 %>% filter(interview_outcome != "Interview Completed", interviewed_flag == 1) %>% 
        distinct(anumber, interview_outcome, interviewed_flag, decision_outcome, denial_referral_decision_reason, has_terminal_decision_outcome) %>% 
        count(decision_outcome, denial_referral_decision_reason, has_terminal_decision_outcome, interview_outcome, interviewed_flag)

# result: no issue - 108 anumbers with an interview_completed record end up with a non-interview_completed value, due to most_recent pruning process


###############################


# drop interviewed_flag
i589 <- i589 %>% select(-interviewed_flag)


###############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect one_year_limit_flag to make sure how closely it matches the pruned values of denial_referral_decision_reason
# note that one_year_limit_flag will be dropped, and is just being used as a diagnostic to confirm that pruning process didn't significantly alter records

# note there are 4 anumbers with one_year_limit_flag = 1, but pruned denial_referral_decision_reason != 1-Year Limit
# one of the 4 anumbers has Not Eligible deneial_referral_decision_reason, and the other 3 have NA, 
i589 %>% distinct(anumber, decision_outcome, denial_referral_decision_reason, one_year_limit_flag) %>%
        count(one_year_limit_flag, decision_outcome, denial_referral_decision_reason) %>% print(n = nrow(.))

# inspect records of anumber with Not Eligible
# result: the most recent decision_date record has a denial_referral value of Not Eligible, so it looks like they intentially overwrote the referral reason
i589 %>% filter(denial_referral_decision_reason != "1-Year Limit", one_year_limit_flag == 1) %>% distinct(anumber, one_year_limit_flag) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        select(anumber, decision_outcome, filing_date, decision_outcome, decision_date, denial_referral_decision_reason, one_year_limit_flag)

# result: no issue - 4 anumbers with a 1-Year Limit record end up with a non-1-Year-Limit value, due to most_recent pruning process


#############################


# drop one_year_limit_flag
i589 <- i589 %>% select(-one_year_limit_flag)


############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect has_decision_outcome

# 27 anumbers have NA decision_outcome after pruning, but has_decision_outcome = 1
i589 %>% distinct(anumber, decision_outcome, has_decision_outcome) %>% count(has_decision_outcome, decision_outcome)

# inspect 27 anumber records
i589 %>% filter(has_decision_outcome == 1, is.na(decision_outcome)) %>% distinct(anumber, has_decision_outcome) %>%
        left_join(., i589_placeholder, by = "anumber") %>%
        select(anumber, filing_date, decision_outcome, decision_date, has_decision_outcome)

# note all 27 anumbers have NA decision_outcomes on most_recent_filing_date records
# since we pruned down to most recent filing_date, these 27 anumbers had their NA decision_outcome records retained
i589 %>% filter(has_decision_outcome == 1, is.na(decision_outcome)) %>% distinct(anumber, has_decision_outcome) %>%
        left_join(., i589_placeholder, by = "anumber") %>%
        group_by(anumber) %>% filter(filing_date == max(filing_date)) %>%
        ungroup() %>% distinct(anumber, decision_outcome) %>% count(decision_outcome)

# result: no issue - 27 anumbers have NA decision_outcome after pruning, but has_decision_outcome = 1; 
# but all have NA decision_outcomes on most_recent_filing_date records


#############################


# drop has_decision_outcome
i589 <- i589 %>% select(-has_decision_outcome)


############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect has_terminal_decision_outcome

# 43 anumbers have non-terminal decision_outcome, but has_terminal_decision_outcome = 1
i589 %>% distinct(anumber, decision_outcome, has_terminal_decision_outcome) %>% count(has_terminal_decision_outcome, decision_outcome)
i589 %>% filter(has_terminal_decision_outcome == 1, is.na(decision_outcome) | !(decision_outcome %in% c("Grant", "Deny", "Referral"))) %>% 
        distinct(anumber) %>% nrow() # 43

# inspect 43 anumber records
i589 %>% filter(has_terminal_decision_outcome == 1, is.na(decision_outcome) | !(decision_outcome %in% c("Grant", "Deny", "Referral"))) %>% 
        distinct(anumber, has_terminal_decision_outcome) %>%
        left_join(., i589_placeholder, by = "anumber") %>%
        select(anumber, filing_date, decision_outcome, decision_date, has_terminal_decision_outcome)

# all 43 anumbers have a non-terminal decision_outcome as the latest decision_date
i589 %>% filter(has_terminal_decision_outcome == 1, is.na(decision_outcome) | !(decision_outcome %in% c("Grant", "Deny", "Referral"))) %>% 
        distinct(anumber, has_terminal_decision_outcome) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        group_by(anumber) %>% filter(decision_date == max(decision_date) | is.na(decision_date)) %>%
        ungroup() %>% distinct(anumber, decision_outcome) %>% count(decision_outcome)

# result: no issue - 43 anumbers have non-terminal decision_outcome, but has_terminal_decision_outcome = 1
# but this is because all 43 anumbers have a non-terminal decision_outcome as the latest decision_date


######################


# drop has_terminal_decision_outcome
i589 <- i589 %>% select(-has_terminal_decision_outcome)


############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect has_service_date

# there are no anumbers with has_service_date = 0, but non-NA service_date values
i589 %>% filter(has_service_date == 0, !is.na(service_date)) %>% distinct(anumber) %>% nrow() # 0
# 27 anumbers have has_service_date = 1, but NA service_date
i589 %>% filter(has_service_date == 1, is.na(service_date)) %>% distinct(anumber) %>% nrow() # 27

# inspect records for 27 anumbers
i589 %>% filter(has_service_date == 1, is.na(service_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>% select(anumber, filing_date, service_date, has_service_date)

# all 27 anumbers have NA service_date as the most_recent filing_date, which explains why service_date = NA but has_service_date = 1
i589 %>% filter(has_service_date == 1, is.na(service_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>%
        group_by(anumber) %>% filter(filing_date == max(filing_date)) %>% 
        ungroup() %>% distinct(anumber, service_date) %>% count(service_date)

# result: no issue - there are 27 anumbers have has_service_date = 1, but NA service_date
# but all 27 anumbers have NA service_date as the most_recent filing_date, which explains why service_date = NA but has_service_date = 1


##############################


# drop has_service_date
i589 <- i589 %>% select(-has_service_date)


##############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect has_reopen_date

# there are no anumbers with has_reopen_date = 0, but non-NA reopen_date values
i589 %>% filter(has_reopen_date == 0, !is.na(reopen_date)) %>% distinct(anumber) %>% nrow() # 0
# 335 anumbers have has_reopen_date = 1, but NA reopen_date
i589 %>% filter(has_reopen_date == 1, is.na(reopen_date)) %>% distinct(anumber) %>% nrow() # 335

# inspect records for 335 anumbers
i589 %>% filter(has_reopen_date == 1, is.na(reopen_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>% select(anumber, filing_date, decision_date, close_date, reopen_date, has_reopen_date)

# all 335 anumbers have NA reopen_date as the most_recent close_date, which explains why reopen_date = NA but has_reopen_date = 1
i589 %>% filter(has_reopen_date == 1, is.na(reopen_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        group_by(anumber) %>% filter(close_date == max(close_date, na.rm = TRUE)) %>% 
        ungroup() %>% distinct(anumber, reopen_date) %>% count(reopen_date)

# result: no issue - 35 anumbers have has_reopen_date = 1, but NA reopen_date,
# but all 335 anumbers have NA reopen_date as the most_recent close_date, which explains why reopen_date = NA but has_reopen_date = 1


########################


# drop has_reopen_date
i589 <- i589 %>% select(-has_reopen_date)


##############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect has_close_date

# there are no anumbers with has_close_date = 0, but non-NA close_date values
i589 %>% filter(has_close_date == 0, !is.na(close_date)) %>% distinct(anumber) %>% nrow() # 0
# 14 anumbers have has_close_date = 1, but NA close_date
i589 %>% filter(has_close_date == 1, is.na(close_date)) %>% distinct(anumber) %>% nrow() # 14

# inspect records for 14 anumbers
i589 %>% filter(has_close_date == 1, is.na(close_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        select(anumber, filing_date, interview_date, interview_outcome, decision_outcome, decision_date, service_date, reopen_date, 
               denial_referral_decision_reason, close_date, close_reason, has_close_date) %>%
        print(n = nrow(.))

# all 14 anumbers have NA close_date as the most_recent close_date, which explains why close_date = NA but has_close_date = 1
# note that additional pruning filters for most_recent filing/decision/interview dates first needed to be sequentially applied to recreate why the
# record selected had NA close_date
i589 %>% filter(has_close_date == 1, is.na(close_date)) %>% distinct(anumber) %>%
        left_join(., i589_placeholder, by = "anumber") %>% 
        group_by(anumber) %>% arrange(desc(filing_date), desc(decision_date), desc(interview_date)) %>% 
        add_group_index(group_vars = filing_date, group_name = "filing_date_index") %>% 
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>% 
        add_group_index(group_vars = interview_date, group_name = "interview_date_index") %>%
        ungroup() %>% filter(filing_date_index == 1, decision_date_index == 1, interview_date_index == 1) %>% 
        distinct(anumber, close_date) %>% count(close_date)

# result: no issue - 14 anumbers have has_close_date = 1, but NA close_date,
# but all 14 anumbers have NA close_date as the most_recent close_date, which explains why close_date = NA but has_close_date = 1


############################


# drop has_close_date
i589 <- i589 %>% select(-has_close_date)


##############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% glimpse()


##################################################################################################################################################


# inspect admin_closed

# 12 anumbers have admin_closed = 1 but service/close_date values no longer meet criteria
i589 %>% filter(admin_closed == 1, ( !(is.na(service_date) & !is.na(close_date))), 
                !(!is.na(service_date) & !is.na(close_date) & (close_date > service_date) & (is.na(reopen_date) | reopen_date < close_date)) ) %>%
        distinct(anumber) %>% nrow() # 12
# inspect records for 12 anumbers w admin_closed = 1 but service/close_date values no longer meet criteria
# not a big issue - it seems like the records with close_date values were dropped because they were not on the decision_date was NA or not most recent
i589 %>% filter(admin_closed == 1, ( !(is.na(service_date) & !is.na(close_date))), 
                                    !(!is.na(service_date) & !is.na(close_date) & (close_date > service_date) & (is.na(reopen_date) | reopen_date < close_date)) ) %>%
        distinct(anumber) %>% left_join(., i589_placeholder, by = "anumber") %>%
        select(anumber, filing_date, decision_date, decision_outcome, service_date, close_date, close_reason, reopen_date, admin_closed)

# only 1 anumber had admin_closed = 0, but met service/close_date criteria
i589 %>% filter(admin_closed == 0, (is.na(service_date) & !is.na(close_date)) | 
                (!is.na(service_date) & !is.na(close_date) & (close_date > service_date) & (is.na(reopen_date) | reopen_date < close_date)) ) %>%
        distinct(anumber) %>% nrow() # 1
# inspect records for 1 anumber had admin_closed = 0, but met service/close_date criteria
# no issue - the record kept with most recent close_date had an NA reopen_date, but the reopen_date equalled the close_date
i589 %>% filter(admin_closed == 0, (is.na(service_date) & !is.na(close_date)) | 
                        (!is.na(service_date) & !is.na(close_date) & (close_date > service_date) & (is.na(reopen_date) | reopen_date < close_date)) ) %>%
        distinct(anumber) %>% left_join(., i589_placeholder, by = "anumber") %>%
        select(anumber, filing_date, decision_date, decision_outcome, service_date, close_date, close_reason, reopen_date, admin_closed)

# check before overwriting admin_closed using the same criteria based on current one-value-per-anumber structure (for non-dep variables)
i589 %>% filter(admin_closed == 1) %>% distinct(anumber) %>% nrow() # 67059
i589 %>% filter((is.na(service_date) & !is.na(close_date)) | 
                        (!is.na(service_date) & !is.na(close_date) & (close_date > service_date) & (is.na(reopen_date) | reopen_date < close_date))) %>%
        distinct(anumber) %>% nrow() # 67048
67059 - 12 + 1 == 67048


########################


# overwrite admin_closed using the same criteria based on current one-value-per-anumber structure (for non-dep variables)
i589 <- i589 %>% mutate(admin_closed = case_when(is.na(service_date) & !is.na(close_date) ~ 1,
                                                 !is.na(service_date) & !is.na(close_date) & close_date > service_date & 
                                                                  (is.na(reopen_date) | reopen_date < close_date) ~ 1, TRUE ~ 0))

        
##########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, admin_closed) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(admin_closed == 1) %>% distinct(anumber) %>% nrow() # 67048


##################################################################################################################################################


# inspect pending

# 52 anumbers have pending = 0, but meet criteria for pending
i589 %>% filter(pending == 0, (is.na(decision_outcome) & is.na(close_date)) | 
                        (!(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0)) %>%
        distinct(anumber) %>% nrow() # 52
# inspect records for 52 anumbers w pending = 0, but meet criteria for pending
# it seems like the record with most recent decision_date has a non-terminal decision_outcome, despite an earlier decision_date record have a terminal decision_outcome 
i589 %>% filter(pending == 0, (is.na(decision_outcome) & is.na(close_date)) | 
                        (!(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0)) %>% 
        distinct(anumber) %>% left_join(., i589_placeholder, by = "anumber") %>%
        select(anumber, filing_date, decision_date, decision_outcome, service_date, close_date, close_reason, reopen_date, admin_closed, pending)
# confirm that the most recent decision_outcome for all 52 anumbers is non-terminal, which explains why they get flagged as pending
i589 %>% filter(pending == 0, (is.na(decision_outcome) & is.na(close_date)) | 
                        (!(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0)) %>% 
        distinct(anumber) %>% left_join(., i589_placeholder, by = "anumber") %>%
        group_by(anumber) %>% arrange(desc(filing_date), desc(decision_date)) %>% 
        add_group_index(group_vars = decision_date, group_name = "decision_date_index") %>% 
        ungroup() %>% filter(decision_date_index == 1) %>% distinct(anumber, decision_outcome) %>% count(decision_outcome) %>% arrange(desc(n))


# there are no anumbers with pending = 1, but who don't meet criteria
i589 %>% filter(pending == 1, !(is.na(decision_outcome) & is.na(close_date)),
                        !(!(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0)) %>%
        distinct(anumber) %>% nrow() # 0

# inspect pending before overwriting
i589 %>% filter(pending == 1) %>% distinct(anumber) %>% nrow() # 267547
i589 %>% filter((is.na(decision_outcome) & is.na(close_date)) | 
                        (!(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0)) %>%
        distinct(anumber) %>% nrow() # 267599
267547 + 52 == 267599


#########################


# overwrite pending based on current one-value-per-anumber structure (excluding dep_ variables)
i589 <- i589 %>% mutate(pending = case_when(is.na(decision_outcome) & is.na(close_date) ~ 1, 
                                    !(decision_outcome %in% c("Grant", "Deny", "Referral")) & admin_closed == 0 ~ 1, TRUE ~ 0))


##########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, pending) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(pending == 1) %>% distinct(anumber) %>% nrow() # 267599


##################################################################################################################################################


# to help with inspect of interview/referral flag
# add interviewed_flag and one_year_limit_flag to i589_placeholder
i589_placeholder <- i589_placeholder %>% 
        # code used to create interviewed_flag (it was added after i589_placeholder was saved, so have to add it manually here)
        mutate(interview_completed_flag = case_when(is.na(interview_outcome) ~ 0, 
                                                    interview_outcome == "Interview Completed" ~ 1, TRUE ~ 0)) %>%  
        group_by(anumber) %>% mutate(interviewed_flag = case_when(sum(interview_completed_flag, na.rm = TRUE) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% select(-interview_completed_flag) %>%
        # code used to create one_year_limit_flag (it was added after i589_placeholder was saved, so have to add it manually here)
        mutate(one_year_limit = case_when(is.na(denial_referral_decision_reason) ~ 0, 
                                                       denial_referral_decision_reason == "1-Year Limit" ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>% mutate(one_year_limit_flag = ifelse(sum(one_year_limit, na.rm = TRUE) > 0, 1, 0)) %>%
        ungroup() %>% select(-one_year_limit) 


################################


# inspect interviewed_flag and one_year_limit_flag
# note that 330 anumbers have one_year_limit = 1 but interviewed_flag = 0
# this is a data quality issue, since all one_year_limit referrals should have been interviewed first
# but this issue was also present in raw data before pruning
i589_placeholder %>% distinct(anumber, one_year_limit_flag, interviewed_flag) %>% 
        count(one_year_limit_flag, interviewed_flag)
# note that are 191 anumbers with 1-year limit, but decision_outcome != Referral
# these will not be bucketed as one-year-limit
# this retains the interpretation that all in one_year_limit bucket have both referral and one_year_limit (though maybe not interview completed)
i589 %>% distinct(anumber, decision_outcome, denial_referral_decision_reason) %>% 
        filter(denial_referral_decision_reason == "1-Year Limit") %>%
        count(decision_outcome, denial_referral_decision_reason) %>% arrange(desc(n))

# result: best choice is to make buckets for ref w int, ref w one-year, and ref wo int; where ref_wo_int excludes ref_w_one_year that have not interview
# so the three referral buckets are mutually exclusive
# see raio definition of mutually exclusive referral buckets here: https://www.uscis.gov/sites/default/files/Affirmative_Asylum_Decisions_FY09-FY18_Q2.pdf


# get referral_flag_test
referral_flag_test <- i589 %>% mutate(referral_w_interview = case_when(decision_outcome == "Referral" & interview_outcome == "Interview Completed" & 
                                                                 denial_referral_decision_reason != "1-Year Limit" ~ 1, TRUE ~ 0),
                referral_w_one_year_limit = case_when(decision_outcome == "Referral" & denial_referral_decision_reason == "1-Year Limit" ~ 1, TRUE ~ 0),
                referral_wo_interview = case_when(decision_outcome == "Referral" & interview_outcome != "Interview Completed" &
                                                          denial_referral_decision_reason != "1-Year Limit" ~ 1, TRUE ~ 0),
                adjudicated_case_flag = case_when(decision_outcome %in% c("Grant", "Deny") |
                        (decision_outcome == "Referral" & (referral_w_interview == 1 | referral_w_one_year_limit == 1)) ~ 1, TRUE ~ 0))

# inspect referrals
referral_flag_test %>% distinct(anumber, referral_w_interview) %>% count(referral_w_interview) # 121477
referral_flag_test %>% distinct(anumber, referral_wo_interview) %>% count(referral_wo_interview) # 69
referral_flag_test %>% distinct(anumber, referral_w_one_year_limit) %>% count(referral_w_one_year_limit) # 92878
referral_flag_test %>% distinct(anumber, referral_w_interview, referral_wo_interview, referral_w_one_year_limit) %>%
        count(referral_w_interview, referral_wo_interview, referral_w_one_year_limit)

# inspect adjudicated_cases
referral_flag_test %>% distinct(anumber, adjudicated_case_flag) %>% count(adjudicated_case_flag) # 339196
referral_flag_test %>% distinct(anumber, adjudicated_case_flag, pending, admin_closed) %>% count(adjudicated_case_flag, pending, admin_closed)
referral_flag_test %>% filter(adjudicated_case_flag == 1) %>% distinct(anumber, decision_outcome) %>% count(decision_outcome)


#############################


# add referral flags
i589 <- i589 %>% mutate(referral_w_interview = case_when(decision_outcome == "Referral" & interview_outcome == "Interview Completed" & 
                                                         denial_referral_decision_reason != "1-Year Limit" ~ 1, TRUE ~ 0),
                referral_w_one_year_limit = case_when(decision_outcome == "Referral" & denial_referral_decision_reason == "1-Year Limit" ~ 1, TRUE ~ 0),
                referral_wo_interview = case_when(decision_outcome == "Referral" & (interview_outcome != "Interview Completed" | is.na(interview_outcome)) &
                                                          denial_referral_decision_reason != "1-Year Limit" ~ 1, TRUE ~ 0))


##############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber) %>% count(adjudicated_case_flag) # 339196
i589 %>% distinct(anumber, pending, admin_closed) %>% count(pending, admin_closed)
i589 %>% filter(decision_outcome == "Referral") %>% distinct(anumber, referral_w_interview, referral_w_one_year_limit, referral_wo_interview) %>%
        count(referral_w_interview, referral_w_one_year_limit, referral_wo_interview)


#############################################################################################################################


# add outcome_bucket
i589 <- i589 %>% mutate(outcome_bucket = case_when(admin_closed == 1 ~ "admin_closed",
                                                    decision_outcome == "Grant" ~ "grant",
                                                    decision_outcome == "Deny" ~ "deny",
                                                    referral_w_one_year_limit == 1 ~ "referral_w_one_year_limit",
                                                    referral_w_interview == 1 ~ "referral_w_interview",
                                                    referral_wo_interview == 1 ~ "referral_wo_interview",
                                                    pending == 1 ~ "pending",
                                                    TRUE ~ NA_character_)) 


##########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, outcome_bucket) %>% count(outcome_bucket) %>% arrange(desc(n))
i589 %>% distinct(anumber, outcome_bucket) %>% count(anumber) %>% arrange(desc(n))
i589 %>% filter(admin_closed == 1) %>% distinct(anumber) %>% nrow() # 67048
i589 %>% filter(pending == 1) %>% distinct(anumber) %>% nrow() # 267599


##########################


# drop admin_closed/referral/pending flags, since outcome_bucket will contain all final info
i589 <- i589 %>% select(-c(admin_closed, referral_w_one_year_limit, referral_w_interview, referral_wo_interview, pending))
        

#############################################################################################################################


# per discussion with nat and jose from raio (see email i sent to them 3/17/2020 at 2:27pm, saved in I-589 data folder)
# the referred_wo_interview category is essentially empty because most cases that would technically be bucketed there are coded in 
# GLOBAL case management system as admin_closed.  Jose explained that eg an interview no-show would be coded in GLOBAL as admin_closed,
# because they lack an adjudicative decision, but they could still be issued a Notice to Appear for Immig. Court for removal proceedings
# according to them, this is what was happening in the raio public summary report and monthly reports (see email), where admin_closed cases
# were bucketed into referral_wo_interview since it aided the purpose of the report, etc

# the takeaway is that, because there's only 74 anumbers coded in data extract as referral_wo_interview,
# we will just roll these few cases into admin_closed bucket

i589 <- i589 %>% mutate(outcome_bucket = case_when(outcome_bucket == "referral_wo_interview" ~ "admin_closed", TRUE ~ outcome_bucket))


########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% distinct(anumber, outcome_bucket) %>% count(outcome_bucket) %>% arrange(desc(n))
67048 + 74 == 67122


#############################################################################################################################


# add adjudicated_case_flag
i589 <- i589 %>% mutate(adjudicated_case_flag = case_when(outcome_bucket %in% c("grant", "deny", "referral_w_one_year_limit",
                                                        "referral_w_interview") ~ 1, TRUE ~ 0))


###################


# inspect
i589 %>% count(adjudicated_case_flag, outcome_bucket)


#############################################################################################################################


# inspect outcome_date and outcome_date_fy

# note all admin_closed have a close_date - this will be use for outcome_date
i589 %>% filter(outcome_bucket == "admin_closed", is.na(close_date)) %>% nrow() # 0
# note that 772 pending anumbers have non-NA decision_dates - these and all pending will get NA values for outcome_date
i589 %>% filter(outcome_bucket == "pending", !is.na(decision_date)) %>% distinct(anumber) %>% nrow() # 772


#########################


# add outcome_date and outcome_date_fy, which is decision_date, plus the close_date for admin_closed

# note that ~84 records that had originally been referral_wo_interview, but were then rolled into admin_closed, do not have a close_date
# have NA close_date, but non-NA decision_date, so will use decision_date for them rather than have NA outcome_date
i589 <- i589 %>% mutate(outcome_date = case_when(outcome_bucket == "admin_closed" & !is.na(close_date) ~ close_date,
                                                 outcome_bucket == "admin_closed" & is.na(close_date) ~ decision_date,
                                         outcome_bucket == "pending" ~ as.Date(NA_character_),
                                         TRUE ~ decision_date),
                        outcome_date_fy = fy(outcome_date))


##########################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(is.na(outcome_date)) %>% count(outcome_bucket)
i589 %>% glimpse()


#############################################################################################################################


# add flags for dependent spouse and kids, before splitting off dependent data
# dependent info can then be re-joined as needed for summaries

# inspect dep_relationship_desc
i589 %>% distinct(anumber, dep_relationship_desc) %>% count(dep_relationship_desc) # 93584 principal w/ child; 139246 principal w/ spouse
i589 %>% distinct(dep_relationship_desc, dep_anumber) %>% count(dep_relationship_desc) # 148530 dependent children; 139249 dependent spouses
# all dependent records appear to have valid dep_anumber
i589 %>% filter(is.na(dep_anumber), !(is.na(dep_relationship_desc) | !(is.na(dep_gender)) | !is.na(dep_last_name))) # 0
i589 %>% filter(!is.na(dep_anumber)) %>% validate_anumbers(anumber_var = "dep_anumber", duplicates_allowed = TRUE)
# note there are 4 dep_anumbers that also appear as principal anumbers - this seems like a data quality issue, 
# since raio said the global system is supposed to ensure this doesn't happen
# it seems like in reality it would happen often though, and raio could not explain what happens with former dep_anumbers submit as principal
# in any case, because count is so insignficant, best choice is to just ignore it, since summary stats will still describe the princ/dep population
i589 %>% filter(dep_anumber %in% i589$anumber) %>% nrow() # 4
i589 %>% filter(dep_anumber %in% i589$anumber | anumber %in% i589$dep_anumber) %>% 
        select(anumber, filing_date, decision_date, decision_outcome, close_date, denial_referral_decision_reason, dep_anumber, dep_relationship_desc)


#############################


# add dep_spouse_flag and dep_child_flag
i589 <- i589 %>% group_by(anumber) %>% mutate(has_dep_spouse = ifelse("Spouse" %in% dep_relationship_desc, 1, 0),
                                              has_dep_child = ifelse("Child" %in% dep_relationship_desc, 1, 0)) %>% ungroup()


#############################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(has_dep_spouse == 1) %>% distinct(anumber) %>% nrow() # 139246
i589 %>% filter(has_dep_child == 1) %>% distinct(anumber) %>% nrow() # 93584
i589 %>% distinct(anumber, has_dep_spouse, has_dep_child) %>% count(has_dep_spouse, has_dep_child)


###############################################################################################################################


# add dep_child_count
i589 <- i589 %>% group_by(anumber) %>% 
        mutate(dep_child_count = n_distinct(dep_anumber[dep_relationship_desc == "Child" & !is.na(dep_relationship_desc)])) %>% ungroup()


####################


# inspect
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(dep_relationship_desc == "Child") %>% distinct(anumber) %>% nrow() # 93584
i589 %>% distinct(anumber, dep_child_count) %>% count(dep_child_count)
i589 %>% distinct(anumber, dep_child_count) %>% count(dep_child_count) %>% filter(dep_child_count > 0) %>% summarize(sum = sum(n)) # 93584


###############################################################################################################################


# add dep_spouse_count
i589 <- i589 %>% group_by(anumber) %>% 
        mutate(dep_spouse_count = n_distinct(dep_anumber[dep_relationship_desc == "Spouse" & !is.na(dep_relationship_desc)])) %>% ungroup()


####################


# inspect
i589 %>% nrow() # 790811
i589 %>% distinct(anumber) %>% nrow() # 673800
i589 %>% filter(dep_relationship_desc == "Spouse") %>% distinct(anumber) %>% nrow() # 139246
i589 %>% distinct(anumber, dep_spouse_count) %>% count(dep_spouse_count)
i589 %>% distinct(anumber, dep_spouse_count) %>% count(dep_spouse_count) %>% filter(dep_spouse_count > 0) %>% summarize(sum = sum(n)) # 93584


###############################################################################################################################


# inspect decision_date and possible data quality issues/fixes

# note that all filing_dates fall within fy 2009-2018 cohort, as expected
i589 %>% count(filing_date_fy)
# and all anumbers have a valid filing_date
i589 %>% filter(is.na(filing_date)) %>% nrow() # 0

# # but that there are 943 anumbers w decision_date pre-fy 2009, which seems to be a data quality issue
# # these are probably nunc pro temp cases, the known data quality issue raio mentioned where the system overwrote the record of an earlier application
# # in order to make an earlier dependent appear as a principal, but also for other reasons
# # they basically hand-waived and said to expect some of these irregularities and to drop them summary calculations
# i589 %>% count(decision_date_fy) %>% print(n = nrow(.))
# i589 %>% filter(decision_date_fy < 2009) %>% nrow() # 944
# i589 %>% filter(decision_date_fy < 2009) %>% distinct(anumber) %>% nrow() # 943
# i589 %>% filter(decision_date < "2008-10-01") %>% select(anumber, filing_date, decision_date, outcome_bucket) 
# 
# # for reference, there are 1394 anumbers (1397 records) with filing_date > decision_date
# # but 451 of those anumbers have decision_dates post-2019, as expected
# i589 %>% filter(filing_date > decision_date) %>% select(anumber, filing_date, decision_date, outcome_bucket) # 1397
# i589 %>% filter(filing_date > decision_date) %>% distinct(anumber) %>% nrow() # 1394
# i589 %>% filter(filing_date > decision_date, decision_date_fy >= 2009) %>% select(anumber, filing_date, decision_date, outcome_bucket) # 453
# i589 %>% filter(filing_date > decision_date, decision_date_fy >= 2009) %>% distinct(anumber) %>% nrow() # 451
# 943 + 451 == 1394

# note that there are 1401 records / 1397 anumbers (0.2% of all anumbers) with filing_date after outcome_date; 
# 1376 anum if filing_date/outcome_fy are used instead
i589 %>% filter(filing_date > outcome_date) %>% select(anumber, filing_date, decision_date, outcome_bucket, outcome_date) %>% nrow() # 1401
i589 %>% filter(filing_date > outcome_date) %>% select(anumber, filing_date, decision_date, outcome_bucket, outcome_date) %>%
        distinct(anumber) %>% nrow() # 1397
i589 %>% filter(filing_date_fy > outcome_date_fy) %>% select(anumber, filing_date, decision_date, outcome_bucket, outcome_date) %>% 
        distinct(anumber) %>% nrow() # 1376
# dropping 1401 would leave 788617
i589 %>% filter(filing_date <= outcome_date | is.na(outcome_date)) %>% nrow() # 789410
# dropping the 1401 would leave all outcome_date_fy >= 2009
i589 %>% filter(filing_date <= outcome_date | is.na(outcome_date)) %>% 
        filter(outcome_date_fy < 2009) %>% nrow() # 0
i589 %>% nrow() # 790811
790811 - 789410 == 1401

# 1401 are spread even across filing_years
i589 %>% filter(filing_date > outcome_date) %>% count(filing_date_fy)
# 1401 spread evenly across outcome_date
i589 %>% filter(filing_date > outcome_date) %>% count(outcome_date_fy) %>% print(n = nrow(.))
# 942 of 1401 have pre-2009 outcome_date
i589 %>% filter(filing_date > outcome_date) %>% count(outcome_date_fy) %>% filter(outcome_date_fy < 2009) %>%
        summarize(sum = sum(n)) # 942
# note that the 1401 are disproportionately grants, which is not great, 
# but such a low number spread over years still seems defensible and unlikely to bias summary stats
i589 %>% filter(filing_date > outcome_date) %>% count(outcome_bucket)
# distribution of days is bimodal, with a peak at ~7 years and a peak at ~3 years
i589 %>% filter(filing_date > outcome_date) %>% mutate(filing_to_outcome_days = as.numeric(outcome_date - filing_date)) %>%
        ggplot(data = ., aes(x = filing_to_outcome_days)) + geom_histogram()

# note that even after filtering down to filing_date < outcome_date, there are still 2 anumbers with filing_date > decision_date
# both are admin_closed though, with close_date (and thus outcome_date) being correctly subsequent to filing_date, so this is valid
i589 %>% filter(filing_date < outcome_date) %>% filter(filing_date > decision_date) %>% nrow() # 2
i589 %>% filter(filing_date < outcome_date) %>% filter(filing_date > decision_date) %>% 
        select(anumber, filing_date, decision_date, outcome_bucket, outcome_date, close_date)
        


# best choice is to drop 1397 anum / 1401 records


#########################


# drop 1397 anumbers with decision_date prior to filing_date, likely nunc pro tunc cases
i589 <- i589 %>% filter(filing_date <= outcome_date | is.na(outcome_date)) 


########################


# inspect
i589 %>% nrow() # 789410
i589 %>% distinct(anumber) %>% nrow() # 672403
# check record count
790811 - 789410 == 1401
# check anumber count
673800 - 1397 == 672403


###############################################################################################################################


# add entry_to_filing_days

# note there are 502 anumbers with filing_date preceding date_of_entry
# based on ron wilson's feedback, these are likely due to admin reasons like nunc pro temp, where record is administratively changed; can be ignored
i589 %>% filter(filing_date < date_of_entry) %>% distinct(anumber) %>% nrow() # 502

# inspect entry_to_filing for outlier cutoff
# note that the iqr*1.5 method is inappropriate here, because it gives 884 as the upper outlier cutoff, which would erase the bimodal hump for cancellation applicants
# because it's bimodal, but we want to preserve the values and not NA them for crosstab summaries, the analysis will summarize with median instead of mean
inspect_entry_to_filing <- i589 %>% mutate(entry_to_filing_days = as.numeric(filing_date - date_of_entry),
                                           entry_to_filing_days = case_when(entry_to_filing_days < 0 ~ NA_real_, 
                                                                            TRUE ~ entry_to_filing_days))
inspect_entry_to_filing %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
entry_to_filing_days <- inspect_entry_to_filing %>% pull(entry_to_filing_days)
quantile(x = entry_to_filing_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))

# so will inspect manually for a natural inflection point
inspect_entry_to_filing %>% filter(entry_to_filing_days < 10000) %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
inspect_entry_to_filing %>% filter(entry_to_filing_days > 10000) %>% nrow() # 861
i589 %>% nrow() # 789410
861 / 789410 # 0.8%

inspect_entry_to_filing %>% filter(entry_to_filing_days < 7500) %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
inspect_entry_to_filing %>% filter(entry_to_filing_days > 7500) %>% nrow() # 6089
6089 / 789410 # 0.8%


###############


# note converting those 504 with non-sensical negative day values to NA for ease of calculation, 
i589 <- i589 %>% mutate(entry_to_filing_days = as.numeric(filing_date - date_of_entry),
                entry_to_filing_days = case_when(entry_to_filing_days < 0 ~ NA_real_, 
                                                 TRUE ~ entry_to_filing_days))


################


# inspect
i589 %>% select(anumber, filing_date, date_of_entry, entry_to_filing_days)
i589 %>% filter(filing_date < date_of_entry) %>% count(entry_to_filing_days)
i589 %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()


# note that 46272 anumbers are in referral_w_1_year_limit bucket, but have NA entry_to_filing_days
# 46187 of these 46272 (99.8%) are because date_of_entry is NA, which is a known data limitation requiring no cleaning/action
# the remaining 85 of these 46272 have NA entry_to_filing_days because date_of_entry is subsequent to filing_date, and negative entry_to_filing_days were NA'd
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", is.na(date_of_entry)) %>% distinct(anumber) %>% nrow() # 46187
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", is.na(filing_date)) %>% distinct(anumber) %>% nrow() # 0
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", is.na(entry_to_filing_days)) %>% distinct(anumber) %>% nrow() # 46272
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", !is.na(date_of_entry), is.na(entry_to_filing_days)) %>% distinct(anumber) %>% nrow() # 85
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", !is.na(date_of_entry), is.na(entry_to_filing_days)) %>%
        filter(date_of_entry > filing_date) %>% distinct(anumber) %>% nrow() # 85

# note there are 917 anumbers in referral_w_one_year_limit bucket, but entry_to_filing_days < 365, which seems like data quality issue
# will convert entry_to_filing_days to NA to avoid biasing the duration stats 
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", entry_to_filing_days < 365) %>% distinct(anumber) %>% nrow() # 917
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", entry_to_filing_days < 365) %>%
        ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", entry_to_filing_days < 365) %>%
        ggplot(data = ., aes(x = entry_to_filing_days)) + stat_ecdf()


##################


# set entry_to_filing_days values to NA for the 312 anumbers w one_year_limit = 1, but entry_to_filing_days < 365
i589 <- i589 %>% mutate(entry_to_filing_days = case_when(outcome_bucket == "referral_w_one_year_limit" &
                                                                 entry_to_filing_days < 365 ~ NA_real_, TRUE ~ entry_to_filing_days))


###################


# inspect
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", entry_to_filing_days < 365) %>% distinct(anumber) %>% nrow() # 0
i589 %>% filter(filing_date < date_of_entry) %>% select(anumber, filing_date, date_of_entry, entry_to_filing_days)
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit") %>%
        distinct(anumber, entry_to_filing_days) %>%
        count(entry_to_filing_days) %>% arrange(entry_to_filing_days)
i589 %>% filter(outcome_bucket == "referral_w_one_year_limit", is.na(entry_to_filing_days)) %>% distinct(anumber) %>% nrow() # 47189

# there are some outlier entry_to_filing_days, but instead of converting them to NA, will just use median for summaries
i589 %>% distinct(anumber, entry_to_filing_days) %>% count(entry_to_filing_days) %>% arrange(desc(entry_to_filing_days))
i589 %>% distinct(anumber, entry_to_filing_days) %>% filter(entry_to_filing_days > 10000) %>% nrow() # 856
i589 %>% distinct(anumber, entry_to_filing_days) %>% ggplot(., aes(x = entry_to_filing_days)) + geom_histogram()
i589 %>% distinct(anumber, entry_to_filing_days) %>% filter(entry_to_filing_days < 10000) %>% ggplot(., aes(x = entry_to_filing_days)) + geom_histogram()
i589 %>% distinct(anumber, entry_to_filing_days) %>% filter(entry_to_filing_days > 10000) %>% ggplot(., aes(x = entry_to_filing_days)) + geom_histogram()
i589 %>% distinct(anumber, entry_to_filing_days) %>% filter(entry_to_filing_days < 2500) %>% ggplot(., aes(x = entry_to_filing_days)) + geom_histogram()
i589 %>% distinct(anumber, entry_to_filing_days) %>% filter(entry_to_filing_days > 2500) %>% ggplot(., aes(x = entry_to_filing_days)) + geom_histogram()


#############################################################################################################################


# add referral_w_one_year_limit_days_excess
i589 <- i589 %>% mutate(referral_w_one_year_limit_days_excess = case_when(outcome_bucket == "referral_w_one_year_limit" ~ entry_to_filing_days,
                                                                  TRUE ~ NA_real_))


################


# inspect
i589 %>% distinct(anumber, referral_w_one_year_limit_days_excess) %>%
        ggplot(data = ., aes(x = referral_w_one_year_limit_days_excess)) + geom_histogram()
i589 %>% distinct(anumber, referral_w_one_year_limit_days_excess) %>%
        ggplot(data = ., aes(x = referral_w_one_year_limit_days_excess)) + stat_ecdf()


#############################################################################################################################


# add filing_to_terminal_decision_days

# note there are 3 anumbers with decision_date preceding filing_date
# based on ron wilson's feedback, these are likely due to admin reasons like nunc pro temp, where record is administratively changed; can be ignored
# also will input NA values for anumbers with pending decision, since they don't have decision yet and would muddy interpretation of processing times
i589 %>% filter(decision_date < filing_date) %>% distinct(anumber) %>% nrow() # 3
i589 %>% filter(is.na(outcome_date)) %>% count(outcome_bucket)
i589 %>% mutate(filing_to_terminal_decision_days = as.numeric(outcome_date - filing_date)) %>%
        filter(filing_to_terminal_decision_days < 0) %>% nrow() # 0

# inspect filing_to_terminal_decision
# note that final decision is to summarize with median in report
# note that 1640 is the upper outlier threshold, which looks appropriate
inspect_filing_to_terminal_decision <- i589 %>% mutate(filing_to_terminal_decision_days = as.numeric(outcome_date - filing_date),
                filing_to_terminal_decision_days = case_when(filing_to_terminal_decision_days < 0 ~ NA_real_,
                                                             TRUE ~ filing_to_terminal_decision_days)) 
inspect_filing_to_terminal_decision %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()
filing_to_terminal_decision_days <- inspect_filing_to_terminal_decision %>% pull(filing_to_terminal_decision_days)
quantile(x = filing_to_terminal_decision_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))

inspect_filing_to_terminal_decision %>% filter(filing_to_terminal_decision_days > 1640) %>% nrow() # 6932
i589 %>% nrow() # 789410
6932 / 789410 # 0.9%
inspect_filing_to_terminal_decision %>% 
        mutate(filing_to_terminal_decision_days = case_when(filing_to_terminal_decision_days > 1640 ~ NA_real_, TRUE ~ filing_to_terminal_decision_days)) %>%
        ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()


# also note that there is bimodal pattern for filing_to_terminal_decision dates shows up starting in 2015 when they began processing old cases under FIFO
# even starting with i589_raw data, the same bimodal pattern appears
i589 %>% filter(decision_outcome %in% c("Grant", "Deny", "Referral")) %>% nrow() # 376299
i589 %>% filter(decision_outcome %in% c("Grant", "Deny", "Referral")) %>% distinct(anumber) %>% nrow() # 337902
i589 %>% filter(decision_outcome %in% c("Grant", "Deny", "Referral")) %>% 
        distinct(anumber, filing_date, decision_outcome, decision_date, decision_date_fy) %>% nrow() # 337902
i589 %>% filter(decision_outcome %in% c("Grant", "Deny", "Referral")) %>% 
        distinct(anumber, filing_date, decision_outcome, decision_date, decision_date_fy) %>%
        mutate(filing_to_terminal_decision_days = as.numeric(decision_date - filing_date)) %>%
        filter(decision_date_fy == 2018) %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()


########################


# add filing_to_terminal_decision_days
i589 <- i589 %>% mutate(filing_to_terminal_decision_days = as.numeric(outcome_date - filing_date))


######################


# inspect

# inspect records with filing_to_terminal_decision_days
# note the mean gets pulled out quite a bit from the median, but still looks reasonable to use in analysis considering the fat tail
i589 %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days, fill = outcome_bucket)) + geom_histogram()
i589 %>% summarize(filing_to_terminal_decision_days_mean = mean(filing_to_terminal_decision_days, na.rm = TRUE),
                   filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE))
i589 %>% select(filing_to_terminal_decision_days) %>% skim()


#############################################################################################################################


# inspect recommended_approval_decision_date_fy
i589 %>% filter(has_recommended_approval == 1) %>% distinct(anumber) %>% nrow() # 24128
i589 %>% filter(!is.na(recommended_approval_decision_date)) %>% distinct(anumber) %>% nrow() # 24128


########################


# add recommended_approval_decision_date_fy
i589 <- i589 %>% mutate(recommended_approval_decision_date_fy = fy(recommended_approval_decision_date))


#########################


# inspect
i589 %>% nrow() # 789410
i589 %>% distinct(anumber) %>% nrow() # 672403
i589 %>% filter(!is.na(recommended_approval_decision_date_fy)) %>% distinct(anumber) %>% nrow() # 24128
i589 %>% filter(!is.na(recommended_approval_decision_date)) %>% distinct(anumber) %>% nrow() # 24128
i589 %>% distinct(anumber, recommended_approval_decision_date_fy) %>% count(recommended_approval_decision_date_fy)


#############################################################################################################################


# add ewi_flag
i589 <- i589 %>% mutate(ewi_flag = case_when(status_at_entry == "EWI" ~ 1, TRUE ~ 0))


#######################


# inspect
i589 %>% filter(status_at_entry == "EWI") %>% nrow() # 207789
i589 %>% count(ewi_flag)


#############################################################################################################################


# inspect ages
# note there are 5 anumbers at age 120 > at entry
i589 %>% mutate(age_at_entry = as.numeric(date_of_entry - date_of_birth) / 365) %>% 
        ggplot(data = ., aes(x = age_at_entry)) + geom_histogram()
i589 %>% mutate(age_at_entry = as.numeric(date_of_entry - date_of_birth) / 365) %>% 
        filter(age_at_entry > 100) %>% count(age_at_entry) %>% arrange(desc(age_at_entry))

# note that final decision to summarize with median
# get outlier cutoff for age_at_entry
# outlier cutoff is 64.8
age_at_entry <- i589 %>% mutate(age_at_entry = as.numeric(date_of_entry - date_of_birth) / 365) %>% 
        pull(age_at_entry)
quantile(x = age_at_entry, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))

# note that final decision to summarize with median
# get outlier cutoff for age_at_filing
# outlier cutoff is 63.9
i589 %>% mutate(age_at_filing = as.numeric(filing_date - date_of_birth) / 365) %>% ggplot(data = ., aes(x = age_at_filing)) + geom_histogram()
age_at_filing <- i589 %>% mutate(age_at_filing = as.numeric(filing_date - date_of_birth) / 365) %>% 
        pull(age_at_filing)
quantile(x = age_at_filing, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))


##################

# add age_at_filing and age_at_entry for princ 
# also convert dep_date_of_birth to Date class
i589 <- i589 %>% mutate(dep_date_of_birth = as.Date(dep_date_of_birth),
                age_at_filing = as.numeric(filing_date - date_of_birth) / 365,
                age_at_entry = as.numeric(date_of_entry - date_of_birth) / 365)


#################


# inspect
i589 %>% filter(age_at_entry < 0) %>% nrow() # 0
i589 %>% filter(age_at_filing < 0) %>% nrow() # 0

i589 %>% ggplot(data = ., aes(x = age_at_entry)) + geom_histogram()
i589 %>% ggplot(data = ., aes(x = age_at_filing)) + geom_histogram()


#############################################################################################################################


# add dep_age_at_filing/entry
# note all dependents have a date of birth
i589 %>% filter(!is.na(dep_anumber)) %>% filter(is.na(dep_date_of_birth)) # 0

i589 %>% mutate(dep_age_at_princ_filing = as.numeric(filing_date - dep_date_of_birth) / 365) %>%
        ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
i589 %>% mutate(dep_age_at_princ_entry = as.numeric(date_of_entry - dep_date_of_birth) / 365) %>%
        ggplot(data = ., aes(x = dep_age_at_princ_entry)) + geom_histogram()

# note that final decision to summarize with median
# get outlier cutoff for dep_age_at_princ_filing
# outlier cutoff is 75.9
i589 %>% mutate(dep_age_at_princ_filing = as.numeric(filing_date - dep_date_of_birth) / 365) %>% 
        ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
dep_age_at_princ_filing <- i589 %>% mutate(dep_age_at_princ_filing = as.numeric(filing_date - dep_date_of_birth) / 365) %>% 
        pull(dep_age_at_princ_filing)
quantile(x = dep_age_at_princ_filing, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))

# note that final decision to summarize with median
# get outlier cutoff for dep_age_at_princ_entry
# outlier cutoff is 69.4
i589 %>% mutate(dep_age_at_princ_entry = as.numeric(filing_date - dep_date_of_birth) / 365) %>% 
        ggplot(data = ., aes(x = dep_age_at_princ_entry)) + geom_histogram()
dep_age_at_princ_entry <- i589 %>% mutate(dep_age_at_princ_entry = as.numeric(date_of_entry - dep_date_of_birth) / 365) %>% 
        pull(dep_age_at_princ_entry)
quantile(x = dep_age_at_princ_entry, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))


########################


# add dep_age_at_princ_filing/entry
i589 <- i589 %>% mutate(dep_age_at_princ_filing = as.numeric(filing_date - dep_date_of_birth) / 365,
                dep_age_at_princ_entry = as.numeric(date_of_entry - dep_date_of_birth) / 365)


#########################


# inspect
i589 %>% nrow() # 789410
i589 %>% distinct(anumber) %>% nrow() # 672403
i589 %>% filter(dep_relationship_desc == "Spouse") %>% ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
i589 %>% filter(dep_relationship_desc == "Child") %>% ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
# note there are 422 anumebrs with negative age at princ_entry, which is reasonable
# there are only 12 with negative age at princ_filing, which is a bit odd, but could have added dep_child later (max negative age is 5.5 years); can ignore
i589 %>% filter(dep_age_at_princ_entry < 0) %>% nrow() # 422
i589 %>% filter(dep_age_at_princ_filing < 0) %>% nrow() # 12
i589 %>% filter(dep_age_at_princ_entry < 0) %>% arrange(desc(dep_age_at_princ_entry)) %>% count(dep_age_at_princ_entry)
i589 %>% filter(dep_age_at_princ_filing < 0) %>% arrange(desc(dep_age_at_princ_filing)) %>% count(dep_age_at_princ_filing)


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


# create i589_princ, dropping dependent info, leaving one record per anumber
i589 %>% glimpse()
i589_princ <- i589 %>% select(-c(dep_anumber, dep_date_of_birth, dep_age_at_princ_entry, dep_age_at_princ_filing, 
                                 dep_first_name, dep_middle_name, dep_last_name, dep_relationship_desc,
                                 dep_country, dep_citizenship, dep_gender, dep_status_at_entry, dep_port_of_entry, dep_date_of_entry,
                                 dep_citizenship_output_country_name, dep_citizenship_region_name, dep_citizenship_us_protectorate_flag,
                                 dep_country_output_country_name, dep_country_region_name, dep_country_us_protectorate_flag,
                                 max_close_date, max_decision_date, max_service_date, max_reopen_date)) %>% distinct()

# create i589_dep
i589_dep <- i589 %>% select(-c(max_close_date, max_decision_date, max_service_date, max_reopen_date, 
                               dep_child_count, dep_spouse_count, has_dep_spouse, has_dep_child)) %>%
        filter(!is.na(dep_anumber)) %>% distinct()


##############################


# inspect i589_princ
i589_princ %>% glimpse()
i589 %>% distinct(anumber) %>% nrow() # 672403
i589_princ %>% nrow() # 672403
i589_princ %>% distinct(anumber) %>% nrow() # 672403

# inspect i589_dep
i589_dep %>% glimpse()
i589 %>% distinct(dep_anumber) %>% filter(!is.na(dep_anumber)) %>% nrow() # 287759
i589_dep %>% distinct(dep_anumber) %>% nrow() # 287759

i589 %>% filter(!is.na(dep_anumber)) %>% distinct(anumber) %>% nrow() # 170873
i589_dep %>% distinct(anumber) %>% nrow() # 170873
# note there are still some dep duplicates to prune
i589_dep %>% nrow() # 287880

# note that 4 anumber/dep_anumbers are listed on both the princ and dep datasets
# raio could not answer repeated questions about possibility/frequency of dependents later filing as principals
# most coherent explanation seeme to be that the system would overwrite the anumber in some way, but it was very hand-wavy,
# in the dataset provided, since there are so few instances, we will ignore them
i589_dep %>% filter(dep_anumber %in% data$anumber) %>% nrow() # 4
i589 %>% filter(anumber %in% i589_dep$dep_anumber) %>% nrow() # 4

# note all dep records have a relationship_desc value
i589_dep %>% filter(is.na(dep_relationship_desc)) %>% nrow() # 0
i589_dep %>% distinct(dep_relationship_desc, dep_anumber) %>% count(dep_relationship_desc) # 148514 dependent children; 139245 dependent spouses


###############################


# save i589_princ
# setwd("C:/Users/sjdevine/Work Folders/Desktop")
# write_csv(i589_princ, path = "asylum_ead/data/I-589/i589_princ_20200408.csv")
# i589_princ <- read_csv("asylum_ead/data/I-589/i589_princ_20200408.csv",
#                         col_types = cols(reopen_date = col_date()))

# save i589 in current shape (principal records are one-row-per-anumber, but dep variables are not yet)
# setwd("C:/Users/sjdevine/Work Folders/Desktop")
# write_csv(i589, path = "asylum_ead/data/I-589/i589_after_princ_dep_split_20200408.csv")
# i589 <- read_csv("asylum_ead/data/I-589/i589_after_princ_dep_split_20200408.csv",
#                  col_types = cols(reopen_date = col_date()))


#############################################################################################################################
#############################################################################################################################


# get_variable_variation for i589_dep
# only variables w variation are citizenship-related, about 121 dep_anumbers
i589_dep_variable_variation <- get_variable_variation(i589_dep, id_vars = dep_anumber)
i589_dep_variable_variation %>% print(n = nrow(.))


#############################################################################################################################


# inspect dep_citizenship values

# since only 121 dep_anumbers have multiple citizenship values, simplest to just retain record with alphabetically first citizenship
i589_dep %>% group_by(dep_anumber) %>% mutate(dep_citizenship_n_distinct = n_distinct(dep_citizenship)) %>% ungroup() %>%
        filter(dep_citizenship_n_distinct > 1) %>% distinct(dep_anumber) %>% nrow() # 121
i589_dep %>% group_by(dep_anumber) %>% mutate(dep_citizenship_n_distinct = n_distinct(dep_citizenship)) %>% ungroup() %>%
        filter(dep_citizenship_n_distinct > 1) %>% 
        select(dep_anumber, dep_citizenship_n_distinct, dep_citizenship, dep_citizenship_output_country_name, dep_citizenship_region_name)

# 121 records to be dropped from the 283 total records of those 121 anumbers with multiple citizenship values
i589_dep %>% group_by(dep_anumber) %>% mutate(dep_citizenship_n_distinct = n_distinct(dep_citizenship)) %>% ungroup() %>%
        filter(dep_citizenship_n_distinct > 1) %>% distinct(anumber) %>% left_join(., i589_dep, by = "anumber") %>%
        nrow() # 283
i589_dep %>% group_by(dep_anumber) %>% arrange(dep_citizenship) %>% 
        add_group_index(group_vars = dep_citizenship, group_name = "dep_citizenship_index") %>%
        ungroup() %>% filter(dep_citizenship_index > 1) %>% nrow() # 121

# insepct dep_citizenship_pruning_test
dep_citizenship_pruning_test <- i589_dep %>% group_by(dep_anumber) %>% arrange(dep_citizenship) %>% 
        add_group_index(group_vars = dep_citizenship, group_name = "dep_citizenship_index") %>%
        ungroup() %>% filter(dep_citizenship_index == 1)
dep_citizenship_pruning_test %>% nrow() # 287779
dep_citizenship_pruning_test %>% distinct(dep_anumber) %>% nrow() # 287779
287880 - 287759 == 121

# get dep_citizenship_pruning_test_change_in_variable_variation
# dep_citizenship_pruning_test_change_in_variable_variation <- dep_citizenship_pruning_test %>% 
#         get_change_in_variable_variation(before_data = i589, after_data = ., id_vars = dep_anumber)


##########################


# prune based on dep_citizenship values
i589_dep <- i589_dep %>% group_by(dep_anumber) %>% arrange(dep_citizenship) %>% 
        add_group_index(group_vars = dep_citizenship, group_name = "dep_citizenship_index") %>%
        ungroup() %>% filter(dep_citizenship_index == 1)


#########################


# inspect
i589_dep %>% nrow() # 287759
i589_dep %>% distinct(dep_anumber) %>% nrow() # 287759
287880 - 287759 == 121


#############################################################################################################################


# save i589_dep
# setwd("C:/Users/sjdevine/Work Folders/Desktop")
# write_csv(i589_dep, path = "asylum_ead/data/I-589/i589_dep_20200408.csv")
# i589_dep <- read_csv("asylum_ead/data/I-589/i589_dep_20200408.csv",
#                  col_types = cols(reopen_date = col_date()))


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# create extract with decision classifications to validate with raio
i589_extract <- i589_princ %>% select(anumber, filing_date, interview_date, interview_outcome, decision_date, decision_outcome, outcome_bucket,
                      service_date, close_date, close_reason, denial_referral_decision_reason)

# inspect
i589_extract
i589_extract %>% nrow() # 672403
i589_extract %>% distinct(anumber) %>% nrow() # 672403
i589_original %>% distinct(`Principal Applicant A Number`) %>% nrow() # 673800
# dropped 1397 anumbers with decision_date prior to filing_date, likely nunc pro tunc cases
673800 - 672403 == 1397

# save
i589_extract %>% write_csv("data/I-589/i589_extract_20200428.csv")


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


# validate data
# result data validates reasonably well against table 8 from asylum EAD final rule 
# (except ref_wo_int and admin_closed - explained by jose and Nat from RAIO, see above) 
# (NPRM table is based on RAIO monthly reports - see link below)
# also validates against OPQ counts of 
# https://www.uscis.gov/tools/reports-studies/immigration-forms-data
# monthly raio reports: search for " Affirmative Asylum Statistics"
# opq annual reports: search for "All USCIS Application and Petition Form Types"

# create a modified version of table 8 from asylum ead final rule
# with receipts/outcomes by fy
# key difference is that our data will show only the fy 2009-2018 filing cohort, whereas table 8 shows all outcomes; receipts should track closely though

# get receipts_by_fy
receipts_by_fy <- i589_princ %>% group_by(filing_date_fy) %>% summarize(receipts = n())
receipts_by_fy

# get outcomes_by_fy
i589_princ %>% count(outcome_bucket)
outcomes_by_fy <- i589_princ %>% add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(grants = sum(outcome_bucket.grant),
                  denials = sum(outcome_bucket.deny),
                  admin_closed = sum(outcome_bucket.admin_closed),
                  referral_w_interview = sum(outcome_bucket.referral_w_interview),
                  referral_wo_interview = sum(outcome_bucket.referral_wo_interview),
                  referral_w_one_year_limit = sum(outcome_bucket.referral_w_one_year_limit),
                  total_referrals = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit)) %>% 
        filter(!is.na(outcome_date_fy))
outcomes_by_fy

# add receipts_by_fy to outcomes_by_fy
outcomes_by_fy %>% rename(fy = outcome_date_fy) %>% left_join(., receipts_by_fy %>% rename(fy = filing_date_fy), by = "fy") %>%
        select(fy, receipts, grants, denials, referral_w_interview, referral_w_one_year_limit, referral_wo_interview, total_referrals, admin_closed)

# save
# write_csv(outcomes_by_fy, path = "asylum_ead/data/I-589/outcomes_by_fy_table.csv")


#########################################################################################################
#########################################################################################################
#########################################################################################################


# validate final outcomes with raio, and note the 1397 anumbers dropped
# code below confirms that 1397 anumbers all had only decision_dates < filing_dates

# start with initially reading in i589 raw data
i589 %>% distinct(`Principal Applicant A Number`) %>% nrow() # 673800
data %>% distinct(anumber) %>% nrow() # 672403
# then run the initial code cleaning variable names and anumber/dates at the top of this script (omitted here for brevity) 
# the i589 dataframe below is the result after these initial cleaning steps on the raw data
i589 %>% anti_join(., data, by ="anumber") %>% distinct(anumber) %>% nrow() # 1397
i589 %>% anti_join(., data, by ="anumber") %>% filter(decision_date > filing_date) %>% nrow() # 0
i589 %>% anti_join(., data, by ="anumber") %>% 
        select(anumber, filing_date, interview_outcome, interview_date, decision_outcome, decision_date,
               close_date, denial_referral_decision_reason)




