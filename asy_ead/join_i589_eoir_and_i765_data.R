library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(naniar)

# setwd
setwd("X:/Asylum_EAD_study")

options(scipen=999)


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


#################################################################################################################################


setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")

# load i589_princ 
i589_princ <- read_csv("data/I-589/i589_princ_20200408.csv",
                       col_types = cols(reopen_date = col_date()))

# inspect
i589_princ %>% glimpse()
i589_princ %>% nrow() # 672403
i589_princ %>% distinct(anumber) %>% nrow() # 672403


##############################


# load eoir 
eoir <- read_csv("data/EOIR/doj_eoir_20200408.csv")

# inspect
eoir %>% glimpse()
eoir %>% nrow() # 207900
eoir %>% distinct(anumber) %>% nrow() # 207900


##############################


# read in ead data
ead <- read_csv("data/I-765/i765_pa_raw.csv")

# inspect 
ead %>% glimpse()
ead %>% nrow() # 1222322
ead %>% distinct(anumber) %>% nrow()


###############################


# join data
data <- i589_princ %>% left_join(., eoir, by = "anumber")

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


#######################################################################################################################################################


# inspect referral_to_case_received_days for outliers
inspect_referral_to_case_received_days <- data %>% 
        mutate(eoir_referral_to_case_received_days = as.numeric(eoir_case_received_date - outcome_date),
               eoir_referral_to_case_received_days = case_when(eoir_referral_to_case_received_days < 0 ~ NA_real_,
                                                               TRUE ~ eoir_referral_to_case_received_days),
               eoir_referral_to_case_received_days = case_when(!(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit")) ~ NA_real_,
                                                               TRUE ~ eoir_referral_to_case_received_days))
inspect_referral_to_case_received_days %>% ggplot(data = ., aes(x = eoir_referral_to_case_received_days)) + geom_histogram()
inspect_referral_to_case_received_days %>% 
        filter(eoir_referral_to_case_received_days < 100) %>%
        ggplot(data = ., aes(x = eoir_referral_to_case_received_days)) + geom_histogram()

# note the final decision is to summarize with median
# get outlier threshold
# 65.5 days is the upper outlier threshold
eoir_referral_to_case_received_days <- inspect_referral_to_case_received_days %>% pull(eoir_referral_to_case_received_days)
quantile(x = eoir_referral_to_case_received_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_referral_to_case_received_days %>% filter(eoir_referral_to_case_received_days > 65.5) %>% nrow() # 20193
inspect_referral_to_case_received_days %>% filter(eoir_received_flag == 1) %>% nrow() # 207900
20193 / 207900 # 9.7%

# inspect for negative eoir_referral_to_case_received_days
# note there are 28325 with case_received prior to outcome_date; these seem to be UAC kids, 
# these UAC cases get forwarded directly to EOIR, so RAIO can't always adjudicate first
data %>% filter(eoir_case_received_date < outcome_date) %>% nrow() # 28325
data %>% filter(eoir_case_received_date < outcome_date) %>% count(outcome_bucket)
data %>% filter(eoir_case_received_date < outcome_date) %>% 
        ggplot(data = ., aes(x = age_at_entry)) + geom_histogram()


#######################


# add eoir_referral_to_case_received_days
data <- data %>% mutate(eoir_referral_to_case_received_days = as.numeric(eoir_case_received_date - outcome_date),
                        eoir_referral_to_case_received_days = case_when(eoir_referral_to_case_received_days < 0 ~ NA_real_,
                                                                            TRUE ~ eoir_referral_to_case_received_days),
                        eoir_referral_to_case_received_days = case_when(!(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit")) ~ NA_real_,
                                                                          TRUE ~ eoir_referral_to_case_received_days))


#######################


# inspect
data %>% filter(!is.na(eoir_referral_to_case_received_days)) %>% count(outcome_bucket)
data %>% ggplot(data = ., aes(x = eoir_referral_to_case_received_days)) + geom_histogram() 
# note that 28287 referrals have eoir_case_received_date preceding outcome_date
# this is not unexpected, and is consistent with UAC story, where they get filed with EOIR immediately
data %>% filter(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit"),
                outcome_date > eoir_case_received_date) %>% nrow() # 28287
data %>% filter(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit"),
                outcome_date > eoir_case_received_date) %>% 
        ggplot(data = ., aes(x = eoir_case_received_date_fy)) + geom_bar()
data %>% filter(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit"),
                outcome_date > eoir_case_received_date) %>% 
        ggplot(data = ., aes(x = age_at_filing)) + geom_histogram()


################################################################################################################################################


# inspect filing_to_eoir_terminal_decision_days
data %>% count(eoir_received_flag, outcome_bucket)  
data %>% count(eoir_received_flag, eoir_outcome) 
# note that recrods w/ bia_decisions always have eoir_outcome
data %>% mutate(bia_decision_flag = ifelse(is.na(eoir_bia_decision), 0, 1)) %>%
        count(bia_decision_flag, eoir_outcome)
data %>% filter(!is.na(eoir_bia_decision)) %>% nrow() # 6114
# and bia_decision_dates are always subsequent to eoir_outcome_date
data %>% filter(!is.na(eoir_bia_decision), eoir_outcome_date < eoir_bia_decision_date) %>% nrow() # 6114
# note that all eoir_outcomes have an eoir_outcome_date except for pending 
data %>% mutate(eoir_outcome_date_flag = ifelse(is.na(eoir_outcome_date), 0, 1)) %>% count(eoir_outcome_date_flag, eoir_outcome)

# note final decision is to summarize with median
# inspect_filing_to_eoir_terminal_decision_days
# 3120 is the upper outlier threshold
inspect_filing_to_eoir_terminal_decision <- data %>% 
        mutate(filing_to_eoir_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - filing_date), 
                          !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - filing_date),
                          TRUE ~ NA_real_),
               filing_to_eoir_terminal_decision_days = case_when(filing_to_eoir_terminal_decision_days < 0 ~ NA_real_,
                                                                 TRUE ~ filing_to_eoir_terminal_decision_days))

inspect_filing_to_eoir_terminal_decision %>% ggplot(data = ., aes(x = filing_to_eoir_terminal_decision_days)) + geom_histogram()
filing_to_eoir_terminal_decision_days <- inspect_filing_to_eoir_terminal_decision %>% pull(filing_to_eoir_terminal_decision_days)
quantile(x = filing_to_eoir_terminal_decision_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_filing_to_eoir_terminal_decision %>% filter(filing_to_eoir_terminal_decision_days > 3120) %>% nrow() # 1169
data %>% filter(!is.na(eoir_outcome_date)) %>% nrow() # 111287
1169 / 111287 # 1%


########################


# add filing_to_eoir_terminal_decision_days
data <- data %>% 
        mutate(filing_to_eoir_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - filing_date), 
                                                                 !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - filing_date),
                                                                 TRUE ~ NA_real_),
               filing_to_eoir_terminal_decision_days = case_when(filing_to_eoir_terminal_decision_days < 0 ~ NA_real_,
                                                                 TRUE ~ filing_to_eoir_terminal_decision_days))

########################


# inspect
data %>% ggplot(data = ., aes(x = filing_to_eoir_terminal_decision_days)) + geom_histogram()


################################################################################################################################################


# inspect days_currently_pending_w_raio/eoir
inspect_days_currently_pending <- data %>% mutate(days_currently_pending_w_raio = case_when(outcome_bucket == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                TRUE ~ NA_real_),
                                        days_currently_pending_w_eoir = case_when(eoir_outcome == "pending" ~ as.numeric(ymd("2020-03-01") - eoir_case_received_date),
                                                                                  TRUE ~ NA_real_),
                                        days_currently_pending_overall = case_when(outcome_bucket == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                                                   eoir_outcome == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                                                   TRUE ~ NA_real_),
                                        currently_pending_w_raio = case_when(outcome_bucket == "pending" ~ 1, TRUE ~ 0),
                                        currently_pending_w_eoir = case_when(eoir_outcome == "pending" ~ 1, TRUE ~ 0))

# inspect distribution
inspect_days_currently_pending %>% summarize(days_currently_pending_w_raio_median = median(days_currently_pending_w_raio, na.rm = TRUE),
                                             days_currently_pending_w_raio_mean = mean(days_currently_pending_w_raio, na.rm = TRUE),
                                             days_currently_pending_w_eoir_median = median(days_currently_pending_w_eoir, na.rm = TRUE),
                                             days_currently_pending_w_eoir_mean = mean(days_currently_pending_w_eoir, na.rm = TRUE),
                                             days_currently_pending_overall_median = median(days_currently_pending_overall, na.rm = TRUE),
                                             days_currently_pending_overall_mean = mean(days_currently_pending_overall, na.rm = TRUE))
inspect_days_currently_pending %>% ggplot(data = ., aes(x = days_currently_pending_w_raio)) + geom_histogram()
inspect_days_currently_pending %>% ggplot(data = ., aes(x = days_currently_pending_w_eoir)) + geom_histogram()
inspect_days_currently_pending %>% ggplot(data = ., aes(x = days_currently_pending_overall, fill = factor(currently_pending_w_raio))) + geom_histogram()


#########################


# add days_currently_pending_w_raio/eoir
data <- data %>% mutate(days_currently_pending_w_raio = case_when(outcome_bucket == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                          TRUE ~ NA_real_),
                days_currently_pending_w_eoir = case_when(eoir_outcome == "pending" ~ as.numeric(ymd("2020-03-01") - eoir_case_received_date),
                                                          TRUE ~ NA_real_),
                days_currently_pending_overall = case_when(outcome_bucket == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                           eoir_outcome == "pending" ~ as.numeric(ymd("2020-03-01") - filing_date),
                                                           TRUE ~ NA_real_),
                currently_pending_w_raio = case_when(outcome_bucket == "pending" ~ 1, TRUE ~ 0),
                currently_pending_w_eoir = case_when(eoir_outcome == "pending" ~ 1, TRUE ~ 0))


#########################


# inspect
data %>% summarize(days_currently_pending_w_raio_median = median(days_currently_pending_w_raio, na.rm = TRUE),
                                             days_currently_pending_w_raio_mean = mean(days_currently_pending_w_raio, na.rm = TRUE),
                                             days_currently_pending_w_eoir_median = median(days_currently_pending_w_eoir, na.rm = TRUE),
                                             days_currently_pending_w_eoir_mean = mean(days_currently_pending_w_eoir, na.rm = TRUE),
                                             days_currently_pending_overall_median = median(days_currently_pending_overall, na.rm = TRUE),
                                             days_currently_pending_overall_mean = mean(days_currently_pending_overall, na.rm = TRUE))
data %>% ggplot(data = ., aes(x = days_currently_pending_w_raio)) + geom_histogram()
data %>% ggplot(data = ., aes(x = days_currently_pending_w_eoir)) + geom_histogram()
data %>% ggplot(data = ., aes(x = days_currently_pending_overall, fill = factor(currently_pending_w_raio))) + geom_histogram()


################################################################################################################################################


# inspect outcome_bucket for those received by eoir
# note that 134 anumbers are coded as admin_closed/pending with raio, but received_by eoir
# for simplicity, will remove the eoir records for these 134, since only referrals should have eoir records
data %>% count(eoir_received_flag, outcome_bucket)


#############################


# remove 129 eoir records that raio data shows as admin_closed or pending
eoir_to_be_removed <- data %>% filter(eoir_received_flag == 1, outcome_bucket %in% c("admin_closed", "pending"))


#####################


eoir_to_be_removed %>% nrow() # 129
# note that 86 don't even have eoir_ij_decisions
eoir_to_be_removed %>% miss_var_summary() %>% print(n = nrow(.))


####################


data_minus_eoir_to_be_removed <- data %>% filter(!(anumber %in% eoir_to_be_removed$anumber))


#####################

data_minus_eoir_to_be_removed %>% nrow() # 672274
data %>% nrow() # 672403
672403 - 672274 == 129


#####################


# convert all eoir vars to NA for eoir_to_be_removed
eoir_to_be_removed <- eoir_to_be_removed %>% map_at(.at = vars(starts_with("eoir_")), .f = ~ rep(NA, times = nrow(eoir_to_be_removed))) %>% bind_cols()


#####################

eoir_to_be_removed %>% glimpse()
eoir_to_be_removed %>% miss_var_summary() %>% print(n = nrow(.))


#####################


# bind eoir_to_be_removed back to data_minus
data <- data_minus_eoir_to_be_removed %>% bind_rows(., eoir_to_be_removed)


#############################


# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403
data %>% filter(anumber %in% eoir_to_be_removed$anumber) %>% miss_var_summary() %>% print(n = nrow(.))
data %>% count(eoir_received_flag, outcome_bucket)


################################################################################################################################################


# clean eoir flags so they are all 1/0, getting rid of NA values for non-BIA records after linkup

# inspect
data %>% select(tidyselect::matches("eoir.*flag"))
data %>% count(eoir_ij_attorney_flag)
data %>% count(eoir_bia_attorney_flag)
data %>% count(eoir_adjudicated_flag)
data %>% count(eoir_appeal_filed_flag)
data %>% count(eoir_received_flag)
data %>% count(eoir_cancellation_applied)
data %>% count(eoir_cancellation_granted)
data %>% count(eoir_absentia)


##################################


# clean eoir flags
data <- data %>% mutate(eoir_ij_attorney_flag = case_when(is.na(eoir_ij_attorney_flag) ~ 0, TRUE ~ eoir_ij_attorney_flag),
                        eoir_bia_attorney_flag = case_when(is.na(eoir_bia_attorney_flag) ~ 0, TRUE ~ eoir_bia_attorney_flag),
                        eoir_adjudicated_flag = case_when(is.na(eoir_adjudicated_flag) ~ 0, TRUE ~ eoir_adjudicated_flag),
                        eoir_appeal_filed_flag = case_when(is.na(eoir_appeal_filed_flag) ~ 0, TRUE ~ eoir_appeal_filed_flag),
                        eoir_received_flag = case_when(is.na(eoir_received_flag) ~ 0, TRUE ~ eoir_received_flag),
                        eoir_cancellation_applied = case_when(is.na(eoir_cancellation_applied) ~ 0, TRUE ~ eoir_cancellation_applied),
                        eoir_cancellation_granted = case_when(is.na(eoir_cancellation_granted) ~ 0, TRUE ~ eoir_cancellation_granted),
                        eoir_absentia = case_when(is.na(eoir_absentia) ~ 0, TRUE ~ eoir_absentia))


###################################


# inspect
data %>% select(tidyselect::matches("eoir.*flag"))
data %>% count(eoir_ij_attorney_flag)
data %>% count(eoir_bia_attorney_flag)
data %>% count(eoir_adjudicated_flag)
data %>% count(eoir_appeal_filed_flag)
data %>% count(eoir_received_flag)
data %>% count(eoir_cancellation_applied)
data %>% count(eoir_cancellation_granted)
data %>% count(eoir_absentia)


################################################################################################################################################


# inspect case_completed_flag
# note there are 3513 + 2955 = 6468 referrals that did not have eoir records
# i could make these 6468 NA, but then counts of completed manually calculated as a pct of total applications would not give same answer as mean(case_completed)
# since 6468 is 0.96% of applications, simplifying by using 0 instead of NA won't meaningfully affect any rates
data %>% count(outcome_bucket)
data %>% count(outcome_bucket, eoir_outcome) %>% print(n = nrow(.))
data %>% filter(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit"), is.na(eoir_outcome)) %>% 
        count(filing_date_fy)


##########################


# add case_completed_flag
data <- data %>% mutate(case_completed_flag = case_when(outcome_bucket == "pending" ~ 0, eoir_outcome == "pending" ~ 0, TRUE ~ 1))


##########################


# inspect
data %>% count(case_completed_flag, outcome_bucket, eoir_outcome) %>% print(n = nrow(.))


################################################################################################################################################


# add overall_relief_granted_flag
data <- data %>% mutate(overall_relief_granted_flag = case_when(outcome_bucket == "grant" | 
                                                                        eoir_outcome %in% c("relief_granted", "cancellation_of_removal") ~ 1, TRUE ~ 0))


###################


# inspect
data %>% count(overall_relief_granted_flag, outcome_bucket, eoir_outcome) %>% print(n = nrow(.))
data %>% summarize(overall_relief_granted_sum = sum(overall_relief_granted_flag), case_completed_sum = sum(case_completed_flag),
                   overall_relief_granted_rate = overall_relief_granted_sum / case_completed_sum)


################################################################################################################################################


# add terminal_adjudicated_flag
data <- data %>% mutate(terminal_adjudicated_flag = case_when(outcome_bucket %in% c("grant", "deny") | 
                                eoir_outcome %in% c("cancellation_of_removal", "relief_granted", "removal", "terminated", "voluntary_departure") ~ 1, 
                                TRUE ~ 0))


##############


# inspect
data %>% count(terminal_adjudicated_flag, outcome_bucket, eoir_outcome) %>% print(n = nrow(.))
data %>% count(terminal_adjudicated_flag, overall_relief_granted_flag)


################################################################################################################################################


# load income data
income <- read_csv("data/country_conditions/income_20200531.csv")


######################


# inspect
income
income %>% nrow() # 6976
data %>% nrow() # 672403
data %>% glimpse()


####################


# join income data 
# note this code was inspected in country_conditions_clean_data.R
data <- data %>% mutate(citizenship_output_country_name_for_income_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        left_join(., income %>% distinct(country_clean, calendar_year, income_group, income_group_bucket, gni_per_capita), 
                  by = c("citizenship_output_country_name_for_income_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>%
        select(-citizenship_output_country_name_for_income_join)


##################


# inspect
data %>% nrow() # 672403
data %>% glimpse()
data %>% filter(is.na(income_group_bucket)) %>% distinct(citizenship_output_country_name)


################################################################################################################################################


# load homicide data
homicide <- read_csv("data/country_conditions/homicide_20200531.csv")


######################


# inspect
homicide
homicide %>% nrow() # 3910
data %>% nrow() # 672403
data %>% glimpse()


####################


# join homicide data 
# note this code was inspected in country_conditions_clean_data.R
data <- data %>% mutate(citizenship_output_country_name_for_homicide_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                                    citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                                    citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                                    TRUE ~ citizenship_output_country_name),
                        filing_date_cy = year(filing_date)) %>%
        left_join(., homicide %>% distinct(country_clean, calendar_year, homicide_count, homicide_rate_per_100k, homicide_bucket), 
                  by = c("citizenship_output_country_name_for_homicide_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>%
        select(-citizenship_output_country_name_for_homicide_join)


##################


# inspect
# result: 107 countries/calendar_year combos in i589 data dont have match in homicide
# 23 of the 107 have no homicide data for cy 2008-2018; the rest have some data, but not for all years
data %>% nrow() # 672403
data %>% glimpse()
data %>% filter(is.na(homicide_bucket)) %>% distinct(citizenship_output_country_name)


################################################################################################################################################


# load freedom data
freedom <- read_csv("data/country_conditions/freedom_20200531.csv")


######################


# inspect
freedom
freedom %>% nrow() # 3129
data %>% nrow() # 672403
data %>% glimpse()


####################


# join freedom data 
# note this code was inspected in country_conditions_clean_data.R
data <- data %>% mutate(citizenship_output_country_name_for_freedom_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                                    citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                                    citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                                    TRUE ~ citizenship_output_country_name),
                        filing_date_cy = year(filing_date)) %>%
        left_join(., freedom %>% distinct(country_clean, survey_calendar_year, total_prcl_score, prcl_bucket), 
                  by = c("citizenship_output_country_name_for_freedom_join" = "country_clean", "filing_date_cy" = "survey_calendar_year")) %>%
        select(-citizenship_output_country_name_for_freedom_join)


##################


# inspect
data %>% nrow() # 672403
data %>% glimpse()
data %>% filter(is.na(prcl_bucket)) %>% distinct(citizenship_output_country_name)


################################################################################################################################################


# save data
data %>% glimpse()
# data %>% write_csv("data/joined_data_20200408.csv")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################


# inspect joined data

# inspect cancellation of removal
data %>% count(outcome_bucket) %>% arrange(desc(n)) # 92837
data %>% filter(eoir_outcome == "cancellation_of_removal") %>% nrow() # 12849
data %>% count(eoir_cancellation_applied, outcome_bucket) %>% arrange(eoir_cancellation_applied, desc(n))
data %>% count(eoir_cancellation_granted, outcome_bucket) %>% arrange(eoir_cancellation_granted, desc(n))
data %>% count(eoir_cancellation_granted, eoir_outcome) %>% arrange(eoir_cancellation_granted, desc(n))
data %>% filter(outcome_bucket == "referral_w_one_year_limit") %>% count(eoir_outcome) %>% arrange(desc(n))


########################


# inspect eoir_received_flag
# note there are ~6000 records with RAIO referred but EOIR didn't have a matching record
data %>% count(eoir_received_flag, outcome_bucket)



########################


# inspect adjudicated_flag
data %>% count(adjudicated_case_flag, outcome_bucket)


########################


# inspect eoir_case_received_date
eoir %>% filter(is.na(eoir_case_received_date)) %>% nrow() # 0


########################


# inspect eoir_outcome_date
eoir %>% filter(is.na(eoir_outcome_date)) %>% count(eoir_outcome)


#######################


# inspect filing_date
i589_princ %>% filter(is.na(filing_date)) %>% nrow() # 0


#######################


# inspect inconsitent sequencing of dates
data %>% mutate(na_outcome_date_fy = case_when(is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        count(na_outcome_date_fy, outcome_bucket) # 71
data %>% filter(filing_date > outcome_date) %>% nrow() # 456
data %>% filter(filing_date > outcome_date) %>% count(filing_date_fy)
data %>% filter(filing_date > outcome_date) %>% count(outcome_date_fy)
data %>% filter(eoir_case_received_date > eoir_outcome_date) %>% nrow() # 6
data %>% filter(eoir_outcome_date > eoir_bia_decision_date) %>% nrow() # 0
data %>% filter(filing_date > eoir_case_received_date) %>% nrow() # 27931 - note these are likely UACs filed with EOIR first (see doj_eoir_clean_data.R)


#######################


# inspect eoir_absentia
data %>% count(eoir_absentia, eoir_outcome) %>% arrange(eoir_absentia, desc(n))
data %>% count(eoir_absentia, eoir_outcome) %>% arrange(eoir_absentia, desc(n))


######################


# inspect status_at_entry
data %>% filter(status_at_entry == "EWI") %>% nrow() # 204329
data %>% filter(status_at_entry == "EWI") %>% count(eoir_cancellation_applied) %>% mutate(pct = n / sum(n))
data %>% filter(eoir_cancellation_applied == 1) %>% count(status_at_entry) %>% mutate(pct = n / sum(n)) %>% arrange(desc(n))


######################


# inspect eoir_appeal
data %>% count(eoir_received_flag, eoir_appeal_filed_flag)


#######################


# inspect custody
data %>% count(eoir_received_flag, eoir_custody) %>% arrange(eoir_received_flag, desc(n))


######################


# inspect entry_to_filing_days
data %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
data %>% filter(entry_to_filing_days < 0) %>% nrow() # 0


######################


# inspect filing_to_terminal_decision_days
data %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()
data %>% filter(filing_to_terminal_decision_days < 0) %>% nrow() # 0
# note that pending records have NA decision_date, which feeds outcome_date
data %>% mutate(na_filing_to_terminal_decision_days = case_when(is.na(filing_to_terminal_decision_days) ~ 1, TRUE ~ 0)) %>%
        count(na_filing_to_terminal_decision_days, outcome_bucket) %>% arrange(na_filing_to_terminal_decision_days, desc(n))
data %>% filter(is.na(decision_date), !is.na(decision_outcome)) %>% nrow() # 0
# note that admin_closed records get outcome_date from close_date, not decision_date, and sometimes close_date is NA
data %>% filter(is.na(outcome_date), !is.na(outcome_bucket), outcome_bucket != "pending") %>% count(outcome_bucket) # 71
data %>% filter(is.na(close_date), !is.na(outcome_bucket), outcome_bucket == "admin_closed") %>% nrow() # 71


#####################


# inspect eoir_case_received_to_outcome_days
data %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()
data %>% filter(eoir_case_received_to_outcome_days < 0) %>% nrow() # 0


######################


# inspect eoir_outcome_to_bia_decision_days
data %>% ggplot(data = ., aes(x = eoir_outcome_to_bia_decision_days)) + geom_histogram()
data %>% filter(eoir_outcome_to_bia_decision_days < 0) %>% nrow() # 0


#######################


# inspect eoir_case_received_to_bia_decision_days
data %>% ggplot(data = ., aes(x = eoir_case_received_to_bia_decision_days)) + geom_histogram()
data %>% filter(eoir_case_received_to_bia_decision_days < 0) %>% nrow() # 0


########################################################################################


# inspect country

# top country shares
# note that 20 countries = ~80% of filings, top 15 = 76%, top 10 = 68%, top 5 = 55%
data %>% count(citizenship_output_country_name) %>% mutate(pct = n / sum(n)) %>% arrange(desc(n)) %>% 
        mutate(pct_cum = cumsum(pct)) %>% print(n = 30)

# country_region
data %>% count(citizenship_region_name) %>% mutate(pct = n / sum(n)) %>% arrange(desc(n)) %>% 
        mutate(pct_cum = cumsum(pct)) %>% print(n = 20)
data %>% count(citizenship_region_name, citizenship_output_country_name) %>% arrange(citizenship_region_name, desc(n)) %>% print(n = nrow(.))

# ewi
data %>% group_by(citizenship) %>% summarize(country_count = n(), ewi_mean = mean(ewi_flag)) %>% arrange(desc(country_count)) %>% print(n = 30)

# cancellation
data %>% filter(eoir_received_flag == 1) %>% group_by(citizenship) %>% 
        summarize(country_count = n(), cancellation_applied_mean = mean(eoir_cancellation_applied)) %>% 
        arrange(desc(country_count)) %>% print(n = 30)

# absentia
data %>% filter(eoir_adjudicated_flag == 1) %>% group_by(citizenship) %>% 
        summarize(country_count = n(), absentia_mean = mean(eoir_absentia, na.rm = TRUE)) %>% 
        arrange(desc(country_count)) %>% print(n = 30)

# boc
# very high NA rates
data %>% select(citizenship, contains("boc_")) %>% group_by(citizenship) %>%
        summarize(opinion = mean(boc_political_opinion_flag), social = mean(boc_social_group_flag),
                  religion = mean(boc_religion_flag), family_planning = mean(boc_family_planning_flag),
                  race = mean(boc_race_flag), nationality = mean(boc_nationality_flag),
                  no_next = mean(boc_no_nexus_flag), na = mean(boc_na_flag)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count)) %>% print(n = 30)

# total processing time for those referred to eoir
data %>% mutate(total_processing = as.numeric(eoir_outcome_date - filing_date) / 365) %>%
        group_by(citizenship) %>% summarize(total_processing = mean(total_processing, na.rm = TRUE)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count))

# raio processing
data %>% filter(eoir_received_flag == 0) %>% 
        group_by(citizenship) %>% summarize(raio_processing = mean((filing_to_terminal_decision_days)/365, na.rm = TRUE)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count))

# eoir processing
data %>% filter(eoir_received_flag == 1) %>% 
        group_by(citizenship) %>% summarize(eoir_processing = mean((eoir_case_received_to_terminal_decision_days)/365, na.rm = TRUE)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count))

# raio attorney
data %>% group_by(citizenship) %>% summarize(raio_attorney = mean(attorney_flag)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count))

# eoir attorney
data %>% filter(eoir_received_flag == 1) %>% group_by(citizenship) %>% summarize(eoir_attorney = mean(eoir_ij_attorney_flag)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count))

# entry to filing
data %>% group_by(citizenship) %>% summarize(entry_to_filing = mean(entry_to_filing_days / 365, na.rm = TRUE)) %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count)) %>% print(n = 30)
data %>% filter(is.na(entry_to_filing_days)) %>% count(citizenship, name = "na_count") %>%
        left_join(., data %>% add_count(citizenship, name = "country_count") %>% distinct(citizenship, country_count),
                  by = "citizenship") %>% arrange(desc(country_count)) %>% 
        mutate(na_pct = na_count / country_count) %>% arrange(desc(country_count)) %>% print(n = 30)


########################################################################################


# cancellation
data %>% filter(eoir_cancellation_applied == 1) %>% count(status_at_entry) %>% mutate(pct = n / sum(n)) %>% arrange(desc(n))

data %>% add_dummies(eoir_outcome) %>% group_by(eoir_cancellation_applied) %>% 
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_count = sum(eoir_outcome.cancellation_of_removal)) %>%
        mutate(relief_granted_pct = relief_granted_count / adj_count,
               removal_pct = removal_count / adj_count,
               cancellation_pct = cancellation_count / adj_count)


########################################################################################


# ewi
data %>% filter(status_at_entry == "EWI") %>% count(outcome_bucket) %>% arrange(desc(n))
data %>% filter(status_at_entry == "EWI") %>% count(eoir_outcome) %>% arrange(desc(n))


#########################################################################################


# one year referral
data %>% filter(outcome_bucket == "referral_w_one_year_limit") %>% count(eoir_outcome) %>% arrange(desc(n))


#########################################################################################


# port_of_entry
data %>% count(port_of_entry) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n))
data %>% count(port_of_entry) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n)) %>% print(n = nrow(.))


#########################################################################################


# processing time by outcome

# note that filing/received_to_terminal_decision_days is NA for pending cases
data %>% filter(outcome_bucket == "pending", !is.na(filing_to_terminal_decision_days)) %>% nrow() # 0
data %>% filter(eoir_outcome == "pending", !is.na(eoir_case_received_to_terminal_decision_days)) %>% nrow() # 0


#########################################################################################


# raio processing time by filing/outcome fy

# note the trend in durations getting the long tail 
data %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram() + facet_grid(rows = vars(filing_date_fy))
data %>% group_by(filing_date_fy) %>% summarize(completed_share = mean(adjudicated_case_flag))
# note that for first few filing cohort years, raio completed > 90% in less than 365 days, so it's unlikely there's a large amount of 
# decisions made in fy 2009-2018 from pre-2009 filing cohorts, that we would therefore not have visibility on
data %>% mutate(raio_processed_less_than_365_days = case_when(as.numeric(outcome_date - filing_date) < 365 ~ 1, TRUE ~ 0)) %>%
        group_by(filing_date_fy) %>% 
        summarize(raio_processed_less_than_365_days_n = sum(raio_processed_less_than_365_days),
                  raio_processed_less_than_365_days_pct = mean(raio_processed_less_than_365_days))

# load i589_raw based on first two code blocks of i589_clean_data.R (just renaming vars, etc - no filtering or overwriting of raw data)
# note that this tracks the output table for filing_to_terminal_decision_days, so the results in the table are not data cleaning/quality issues
# it's odd though that table 8 of the rule has much larger processing times, and claims the processing time is going down??
# (though technically filing_to_terminal_decision_days also includes admin_closed, but since that's computed i589_raw doesn't have it)
i589_raw %>% filter(decision_outcome %in% c("Grant", "Referral", "Deny")) %>% distinct(anumber) %>% nrow() # 339336
i589_raw %>% filter(decision_outcome %in% c("Grant", "Referral", "Deny")) %>%
        mutate(filing_to_adj_days = as.numeric(decision_date - filing_date)) %>%
        group_by(filing_date_fy) %>% summarize(filing_to_adj_days_median = median(filing_to_adj_days, na.rm = TRUE))

################


# inspect mean - though final decision is to summarize with median
data %>% 
        group_by(filing_date_fy) %>% 
        summarize(filing_to_terminal_decision_days_mean = mean(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(filing_to_terminal_decision_days_mean = round_to_digits(filing_to_terminal_decision_days_mean, digits = 0))

data %>% 
        group_by(outcome_date_fy) %>% 
        summarize(filing_to_terminal_decision_days_mean = mean(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(filing_to_terminal_decision_days_mean = round_to_digits(filing_to_terminal_decision_days_mean, digits = 0))


data %>% 
        filter(!(outcome_bucket %in% c("referral_w_one_year_limit", "referral_w_interview"))) %>%
        group_by(outcome_date_fy) %>% 
        summarize(filing_to_terminal_decision_days_mean = mean(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(filing_to_terminal_decision_days_mean = round_to_digits(filing_to_terminal_decision_days_mean, digits = 0))

data %>% 
        filter(outcome_bucket %in% c("referral_w_one_year_limit", "referral_w_interview")) %>%
        group_by(outcome_date_fy) %>% 
        summarize(filing_to_terminal_decision_days_mean = mean(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(filing_to_terminal_decision_days_mean = round_to_digits(filing_to_terminal_decision_days_mean, digits = 0))




##########################################################################################################


# eoir processing time by filing/outcome fy
data %>% 
        filter(eoir_received_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(referral_received_to_terminal_decision_days_mean = mean(eoir_case_received_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(referral_received_to_terminal_decision_days_mean = round_to_digits(referral_received_to_terminal_decision_days_mean, digits = 0))


data %>% 
        filter(eoir_adjudicated_flag == 1) %>%
        group_by(eoir_outcome_date_fy) %>% 
        summarize(referral_received_to_terminal_decision_days_mean = mean(eoir_case_received_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(referral_received_to_terminal_decision_days_mean = round_to_digits(referral_received_to_terminal_decision_days_mean, digits = 0))




####################


# raio

# by outcome
# denials are a bit quicker, but grants and referrals take about the same time
data %>% group_by(outcome_bucket) %>% summarize(mean_duration = mean(filing_to_terminal_decision_days))
data %>% ggplot(data = ., aes(x = outcome_bucket, y = filing_to_terminal_decision_days)) + geom_boxplot()
data %>% filter(filing_date_fy < 2014) %>% ggplot(data = ., aes(x = outcome_bucket, y = filing_to_terminal_decision_days)) + geom_boxplot()

# by filing cohort fy
# durations were very low for 2009-2012 cohort (lifo), got higher for 2013-2016 cohort (~fifo), and have started to decrease for 2017-2018 cohort (~lifo)
# remember that raio staff also process RF and CF cases, etc...
data %>% filter(outcome_bucket %in% c("grant", "deny", "referral_w_interview", "referral_w_one_year_limit")) %>%
        ggplot(data = ., aes(x = as.character(filing_date_fy), y = filing_to_terminal_decision_days)) + geom_boxplot()
data %>% filter(outcome_bucket %in% c("grant", "deny", "referral_w_interview", "referral_w_one_year_limit")) %>%
        group_by(filing_date_fy) %>% summarize(mean_duration = mean(filing_to_terminal_decision_days, na.rm = TRUE))
i589_filings_and_outcomes_by_fy_cohort_table_formatted

# by outcome fy
# durations were very low for adj cases 2009-2014 (lifo), got higher for 2015-2017 (fifo), dipped in 2018 (lifo), and rising again 2019-2020 (lifo exceptions)
# remember that raio staff also process RF and CF cases, etc...
data %>% filter(outcome_bucket %in% c("grant", "deny", "referral_w_interview", "referral_w_one_year_limit")) %>%
        ggplot(data = ., aes(x = as.character(outcome_date_fy), y = filing_to_terminal_decision_days)) + geom_boxplot()
data %>% filter(outcome_bucket %in% c("grant", "deny", "referral_w_interview", "referral_w_one_year_limit")) %>%
        group_by(outcome_date_fy) %>% summarize(mean_duration = mean(filing_to_terminal_decision_days, na.rm = TRUE))
data %>% filter(outcome_bucket %in% c("grant", "deny", "referral_w_interview", "referral_w_one_year_limit")) %>%
        filter(!is.na(outcome_date_fy)) %>% 
        ggplot(data = ., aes(x = as.character(outcome_date_fy), fill = as.character(filing_date_fy))) + geom_bar()
i589_filings_and_outcomes_by_fy_table_formatted


########################


# eoir

# by outcome
data %>% 
        ggplot(data = ., aes(x = eoir_outcome, y = eoir_case_received_to_outcome_days)) + geom_boxplot()
data %>% 
        filter(eoir_case_received_date_fy < 2014) %>% ggplot(data = ., aes(x = eoir_outcome, y = eoir_case_received_to_outcome_days)) + geom_boxplot()
data %>% group_by(eoir_outcome) %>% summarize(mean_duration = mean(eoir_case_received_to_outcome_days))

# filing cohort fy
# eoir processing time appears relatively stable for all of the filing cohorts, except for the 2018 cohort, which is markedly lower (only ~ 2k cases adj though)
# from 2014-2018 there is less than 50% of cases adj; <20% for 2016-2018
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>% 
        ggplot(data = ., aes(x = as.character(filing_date_fy), y = eoir_case_received_to_outcome_days)) + geom_boxplot()
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>%
        group_by(filing_date_fy) %>% summarize(adj_count = sum(eoir_adjudicated_flag),
                mean_duration = mean(eoir_case_received_to_outcome_days, na.rm = TRUE))
eoir_referrals_and_outcomes_by_fy_cohort_table_formatted

# by case_received cohort
# this is a misleading chart, since eoir_case_received 2016 onward all have < 1000 adj_count
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>% 
        ggplot(data = ., aes(x = as.character(eoir_case_received_date_fy), y = eoir_case_received_to_outcome_days)) + geom_boxplot()
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>%
        group_by(eoir_case_received_date_fy) %>% summarize(mean_duration = mean(eoir_case_received_to_outcome_days, na.rm = TRUE),
                                                           adj_count = sum(eoir_adjudicated_flag))
eoir_referrals_and_outcomes_by_fy_cohort_table_formatted

# by outcome fy
# processing times by outcome year have been rising steadily from 2009-2018, and leveling maybe slight decrease for 2019-2020
# remember that eoir backlog also includes defensive asylum and other cases
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>% 
        ggplot(data = ., aes(x = as.character(eoir_outcome_date_fy), y = eoir_case_received_to_outcome_days)) + geom_boxplot()
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>%
        group_by(eoir_outcome_date_fy) %>% summarize(mean_duration = mean(eoir_case_received_to_outcome_days))
data %>% filter(eoir_outcome %in% c("relief_granted", "removal")) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>% 
        ggplot(data = ., aes(x = as.character(eoir_outcome_date_fy), fill = as.character(filing_date_fy))) + geom_bar()
eoir_referrals_and_outcomes_by_fy_table_formatted








