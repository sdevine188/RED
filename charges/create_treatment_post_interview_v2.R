library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(survey)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(rlang)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(partykit)
library(ROCR)

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")

# load treatment data after cleaning pre-interview variables
# see create_treatment_pre_interview_dummies.R
treatment_control <- read_csv("treatment_control_v2_20180507.csv")
glimpse(treatment_control)

# filter to just treatment
treatment <- treatment_control %>% filter(treatment_flag == 1)
dim(treatment)
glimpse(treatment)


############################################################


# inspect


# inspect outcome variables
treatment %>% group_by(Case_Outcome) %>% count() %>% arrange(desc(n))
treatment %>% group_by(SOF_Finding_SP) %>% count() %>% arrange(desc(n))
treatment %>% group_by(Suf_Evidence) %>% count() %>% arrange(desc(n))

# part 2 ISO closeout interview outcome-related variables
treatment %>% group_by(Decision) %>% count() %>% arrange(desc(n))
treatment %>% group_by(FRAUDEVID) %>% count() %>% arrange(desc(n))

# review for missing values
# note there will be missing values 
sum_na_values <- function(column) {
        sum(is.na(column))
}

treatment %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count")


######################################################


# inspect and clean post-interview variables to add

# Office
treatment %>% group_by(Office) %>% count() %>% arrange(desc(n)) %>% data.frame()
sum(is.na(treatment$Office))

# Arranged
treatment %>% group_by(Arranged) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Correct-Info
treatment %>% group_by(Correct_Info) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Dif-Spouse
treatment %>% group_by(Dif_Spouse) %>% count() %>% arrange(desc(n)) %>% data.frame()

# INA_204_G
treatment %>% group_by(INA_204_G) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Language
treatment %>% group_by(Language) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Live_Together
treatment %>% group_by(Live_Together) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Out_Status
treatment %>% group_by(Out_Status) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Proceedings
treatment %>% group_by(Proceedings) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Related
treatment %>% group_by(Related) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Decision
treatment %>% group_by(Decision) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(Decision = case_when(is.na(Decision) ~ "NA_value", TRUE ~ Decision))
treatment %>% group_by(Decision) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Hours
treatment %>% group_by(Hours) %>% count() %>% arrange(desc(n)) %>% data.frame()
treatment <- treatment %>% mutate(Hours_clean = case_when(Hours == "8H" | Hours == "8 H" ~ "8",
                        Hours == "4h" ~ "4", 
                        Hours == "6 h"  ~ "6",
                        Hours == "15+" ~ "15", Hours == "10+" ~ "10", 
                        Hours == "480" ~ NA_character_, TRUE ~ Hours))

treatment <- treatment %>% mutate(Hours_clean = as.numeric(Hours_clean))

treatment %>% group_by(Hours_clean) %>% count() %>% arrange(desc(n)) 


# INA_204_C
# convert 2 NA values to "NA"
treatment %>% group_by(INA_204_C) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(INA_204_C = case_when(is.na(INA_204_C) ~ "NA_value", TRUE ~ INA_204_C))
treatment %>% group_by(INA_204_C) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Inconsistent
# convert 2 NA values to "NA"
treatment %>% group_by(Inconsistent) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(Inconsistent = case_when(is.na(Inconsistent) ~ "NA_value", Inconsistent == "No Interview Conducted" ~ "NA_value", TRUE ~ Inconsistent))
treatment %>% group_by(Inconsistent) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Issue
# convert 2 NA values to "NA"
treatment %>% group_by(Issue) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(Issue = case_when(is.na(Issue) ~ "NA_value", TRUE ~ Issue))
treatment %>% group_by(Issue) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Request
# convert NA to "NA"
treatment %>% group_by(Request) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(Request = case_when(is.na(Request) ~ "NA_value", TRUE ~ Request))
treatment %>% group_by(Request) %>% count() %>% arrange(desc(n)) %>% data.frame()

# interview_type_joint
# convert 2 NAs
str(treatment$interview_type_joint)
treatment %>% group_by(interview_type_joint) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(interview_type_joint = case_when(is.na(interview_type_joint) ~ "NA_value", TRUE ~ as.character(interview_type_joint)))
treatment %>% group_by(interview_type_joint) %>% count() %>% arrange(desc(n)) %>% data.frame()

# interview_type_sep_int
# convert 2 NAs
treatment %>% group_by(interview_type_sep_int) %>% count() %>% arrange(desc(n)) %>% data.frame()
treatment <- treatment %>% mutate(interview_type_sep_int = case_when(is.na(interview_type_sep_int) ~ "NA_value", TRUE ~ as.character(interview_type_sep_int)))
treatment %>% group_by(interview_type_sep_int) %>% count() %>% arrange(desc(n)) %>% data.frame()

# interview_type_stokes
# convert 2 NAs
treatment %>% group_by(interview_type_stokes) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(interview_type_stokes = case_when(is.na(interview_type_stokes) ~ "NA_value", TRUE ~ as.character(interview_type_stokes)))
treatment %>% group_by(interview_type_stokes) %>% count() %>% arrange(desc(n)) %>% data.frame()

# interview_type_no_int
# only 1 yes - not worth including
treatment %>% group_by(interview_type_no_int) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(interview_type_no_int = case_when(is.na(interview_type_no_int) ~ "NA_value", TRUE ~ as.character(interview_type_no_int)))
treatment %>% group_by(interview_type_no_int) %>% count() %>% arrange(desc(n)) %>% data.frame()

# Crim_History
# convert 1 NA
treatment %>% group_by(Crim_History) %>% count() %>% arrange(desc(n)) %>% data.frame()
# treatment <- treatment %>% mutate(Crim_History = case_when(is.na(Crim_History) ~ "NA_value", TRUE ~ Crim_History))
treatment %>% group_by(Crim_History) %>% count() %>% arrange(desc(n)) %>% data.frame()

# pet_edlevel
treatment %>% group_by(pet_edlevel) %>% count() %>% arrange(desc(n)) %>% data.frame()

# ben_edlevel
treatment %>% group_by(ben_edlevel) %>% count() %>% arrange(desc(n)) %>% data.frame()

# ben_pet_edlevel_diff
treatment <- treatment %>% mutate(ben_pet_edlevel_diff = ben_edlevel - pet_edlevel)
treatment %>% group_by(ben_pet_edlevel_diff) %>% count() %>% arrange(desc(n))

# ben_employ
treatment %>% count(BEN_EMPLOY)

# pet_employ
treatment %>% count(PET_EMPLOY)


######################################################

# save clean treatment file

# write clean treatment to csv
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment_cleaned_post_interview_w_all_variables_v2", "_", current_date, ".csv")
filename
write_csv(treatment, path = filename)


#######################################################


# select variables for pre-interview model
post_interview_model_variables <- c("receipt_number", "Case_Outcome", "SOF_Finding_SP", "Suf_Evidence", "BEN_COUNTRY_OF_BIRTH", "PET_STATE", "BEN_STATE", 
                                    "BEN_SEX", "PET_SEX", "ewi", "Concurrent", "Office", "Arranged", "Correct_Info", "Dif_Spouse",
                                    "INA_204_G", "Language", "Live_Together", "Out_Status", "Proceedings", "Related", "INA_204_C",
                                    "interview_type_joint", "interview_type_sep_int",
                                    "interview_type_stokes", "Crim_History", "pet_edlevel", "ben_edlevel", "ben_pet_edlevel_diff",
                                    "BEN_LAST_VISA_APP", "FILED_G28", "ben_prior_marriage", "pet_prior_marriage",
                                    "VISA_APPROVE", "ageben", "agepet", "ben_pet_age_diff", "how_pet_got_citz",
                                    "pet_get_lpr_via_marriage", "ben_entry_status_group", "ben_immig_proceed",
                                    "time_since_ben_immig_proceed", "type_ben_immig_proceed_group", "pet_filed_prior_petition",
                                    "count_pet_marriages", "count_ben_marriages",
                                    "time_since_pet_divorce", "time_since_ben_divorce", "time_since_present_marriage",
                                    "shared_children", "cohabitation", "time_since_cohabitation", "BEN_EMPLOY",
                                    "PET_EMPLOY", "time_since_ben_last_arrival", "ben_status_last_entry",
                                    "ben_income", "pet_income", "ben_region_of_birth")

treatment_post_interview <- treatment %>% select(post_interview_model_variables)
glimpse(treatment_post_interview)

# review for missing values
sum_na_values <- function(column) {
        sum(is.na(column))
}

treatment_post_interview %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count")

# covert NAs to NA_value
# treatment_post_interview <- treatment_post_interview %>% map_dfr(.x = ., .f = ~ ifelse(is.na(.x), "NA_value", .x))
# glimpse(treatment_post_interview)
# 
# treatment_post_interview %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
#         gather(key = "variable", value = "na_count")


# save clean treatment_post_interview file
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment_cleaned_post_interview_v2", "_", current_date, ".csv")
filename
write_csv(treatment_post_interview, path = filename)


##################################################################


# remove receipt_number before creating dummy variables, then reattach afterward
treatment_post_interview_receipt_numbers <- treatment_post_interview$receipt_number
treatment_post_interview <- treatment_post_interview %>% select(-receipt_number)

# create dummy variables 
treatment_post_interview_dummies <- dummyVars(~ ., data = treatment_post_interview)
treatment_post_interview_dummies <- data.frame(predict(treatment_post_interview_dummies, newdata = treatment_post_interview))
glimpse(treatment_post_interview_dummies)

# reattach receipt_numbers
treatment_post_interview %>% select(time_since_present_marriage, ben_edlevel, pet_edlevel) %>% head()
treatment_post_interview_dummies %>% select(time_since_present_marriage, ben_edlevel, pet_edlevel) %>% head()

treatment_post_interview_dummies <- treatment_post_interview_dummies %>% mutate(receipt_number = treatment_post_interview_receipt_numbers)

# get independent variables names of treatment_post_interview_dummies for use in creating model formula (remove dependent vars)
# post_interview_model_variables_w_dummies <- names(treatment_post_interview_dummies)
# dependent_variable_index <- which(post_interview_model_variables_w_dummies %in% 
#                                           c("Case_OutcomeApproved", "Case_OutcomeDenied", "Case_OutcomePending",
#                                             "SOF_Finding_SPFF", "SOF_Finding_SPFNF", "SOF_Finding_SPInconclusive", "SOF_Finding_SPPending", 
#                                             "Suf_EvidenceNo", "Suf_EvidenceYes", "Suf_EvidenceNA_value"))
# post_interview_model_variables_w_dummies <- post_interview_model_variables_w_dummies[-dependent_variable_index]
# post_interview_model_variables_w_dummies

# review for missing values
sum_na_values <- function(column) {
        sum(is.na(column))
}

treatment_post_interview_dummies %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count") %>% filter(na_count > 0)


##############################


# save output
# create csv filename for merged data
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
filename <- str_c("treatment_post_interview_dummies_v2_", date2, ".csv")
filename

write_csv(treatment_post_interview_dummies, path = filename)

