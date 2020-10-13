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

# load i589_princ clean data for use in validating/cleaning eoir data
i589_princ <- read_csv("data/I-589/i589_princ_20200408.csv",
                       col_types = cols(reopen_date = col_date()))
i589_princ %>% glimpse()
i589_princ %>% nrow() # 672403
i589_princ %>% distinct(anumber) %>% nrow() # 672403

# load i589 raw data for use in validating/cleaning eoir data
i589 <- read_csv("data/I-589/i589_after_admin_closed_20200226.csv",
                 col_types = cols(reopen_date = col_date(), max_reopen_date = col_date()))
i589 %>% glimpse()
i589 %>% nrow() # 1128838
i589 %>% distinct(anumber) %>% nrow() # 673800

# load eoir_data_from_api
eoir_data_from_api <- read_csv("data/EOIR/eoir_data_from_api_20200324.csv")
eoir_data_from_api %>% glimpse()
eoir_data_from_api %>% nrow() # 202358
eoir_data_from_api %>% distinct(anumber) %>% nrow() # 200074


#################################################################################################################################


# load doj eoir data
eoir_raw <- read_csv(file = "data/eoir/eoir_data_from_doj/01 - PASD #20-378 Final.csv",
                   col_types = cols(IDNPROCEEDING = col_character(), IDNCASE = col_character(), anumber = col_character(),
                                    ALIEN_NBR = col_character(), LEAD_ALIEN_NBR = col_character(), OSC_DATE = col_date(format = "%m/%d/%Y"),
                                    C_INPUT_DATE = col_date(format = "%m/%d/%Y"), C_RECD_DATE = col_date(format = "%m/%d/%Y"), ALIEN_NAME = col_character(),
                                    ALIEN_FNAME = col_character(), ALIEN_LNAME = col_character(), ALIEN_MNAME = col_character(),
                                    C_BIRTHDATE = col_date(format = "%m/%d/%Y"), NAT = col_character(), CASE_TYPE = col_character(),
                                    ALIEN_STREET = col_character(), ALIEN_STREET1 = col_character(), ALIEN_STREET2 = col_character(),
                                    ALIEN_CITY = col_character(), ALIEN_STATE = col_character(), ALIEN_ZIPCODE = col_character(),
                                    GENERATION = col_character(), P_INPUT_DATE = col_date(format = "%m/%d/%Y"), BASE_CITY_CODE = col_character(),
                                    HEARING_LOC_CODE = col_character(), CUSTODY = col_character(), DEC_CODE = col_character(),
                                    OTHER_COMP = col_character(), DECISION = col_character(), COMP_DATE = col_date(format = "%m/%d/%Y"),
                                    ABSENTIA = col_character(), REC_TYPE = col_character(), `MOTION TYPE` = col_character(),
                                    MOTION_RECD_DATE = col_date(format = "%m/%d/%Y"), `MOTION DECISION` = col_character(), 
                                    `MOTION COMPLETION DATE` = col_date(format = "%m/%d/%Y"),
                                    strAppealCategory = col_character(), strAppealType = col_character(), datAppealFiled = col_date(format = "%m/%d/%Y"),
                                    strFiledBy = col_character(), datBIADecision = col_date(format = "%m/%d/%Y"), strBIADecision = col_character(),
                                    `Case Appeal Decision` = col_character()))
eoir <- eoir_raw

# inspect
eoir_raw %>% glimpse()
eoir %>% nrow() # 214457
eoir %>% distinct(anumber) %>% nrow() # 214457


#######################


# clean variable names
eoir <- eoir %>% rename(case_input_date = C_INPUT_DATE, case_received_date = C_RECD_DATE, alien_date_of_birth = C_BIRTHDATE,
                        nationality = NAT, ij_decision = DECISION, ij_other_completion = OTHER_COMP, ij_decision_date = COMP_DATE,
                        motion_type = `MOTION TYPE`, motion_decision = 'MOTION DECISION', motion_completion_date = 'MOTION COMPLETION DATE',
                appeal_category = strAppealCategory, appeal_type = strAppealType, appeal_filed_date = datAppealFiled, filed_by = strFiledBy,
                bia_decision_date = datBIADecision, bia_decision = strBIADecision, case_appeal_decision = 'Case Appeal Decision') %>%
        rename_all(.funs = str_to_lower) %>%
        mutate(anumber = str_c("A", anumber))

# inspect
eoir %>% glimpse()
eoir %>% select(contains("date")) %>% glimpse()
eoir %>% validate_anumbers(duplicates_allowed = TRUE)

# confirm that COMP_DATE = ij_decision_date
# note that the vast majority of doj_eoir records with valid ij_decision_date/COMP_DATE have a matching ij_decision_date in eoir_api
eoir %>% filter(!is.na(ij_decision_date)) %>% nrow() # 107139
eoir %>% left_join(., eoir_data_from_api %>% rename_at(.vars = vars(-anumber), .funs = ~ str_c("api_", .x)), by = "anumber") %>% 
        filter(ij_decision_date == api_ij_decision_date) %>% distinct(anumber) %>% nrow() # 102776

# confirm that case_input_date is the best one to use
eoir %>% select(anumber, case_input_date, case_received_date, osc_date) %>% filter(case_input_date > case_received_date) %>% nrow() # 22389
eoir %>% select(anumber, case_input_date, case_received_date, osc_date) %>% filter(case_input_date <= case_received_date) %>% nrow() # 185833
eoir %>% select(anumber, case_input_date, case_received_date, osc_date) %>% filter(case_input_date > osc_date) %>% nrow() # 131667
eoir %>% mutate(duration_osc_to_case_input = round(as.numeric(case_input_date - osc_date) / 365)) %>% 
        ggplot(data = ., aes(x = duration_osc_to_case_input)) + geom_bar()


################################################################################################################################################


# load attorney data for ij court 
ij_attorney <- read_csv(file = "data/eoir/eoir_data_from_doj/05 - Alien Attorney Information (Court).csv",
                        col_types = cols(IDNCASE = col_character()))
ij_attorney %>% glimpse()
ij_attorney %>% nrow() # 257755
ij_attorney %>% distinct(IDNCASE) %>% nrow() # 188840
# note that essentially every ID in this table had an attorney, so it's a simple flag
ij_attorney %>% filter(!is.na(AttorneyName)) %>% nrow() # 257753

# load attorney data for bia court
bia_attorney <- read_csv(file = "data/eoir/eoir_data_from_doj/06 - Alien Attorney Information (Board).csv",
                         col_types = cols(IDNCASE = col_character()))
bia_attorney %>% glimpse()
bia_attorney %>% nrow() # 10801
bia_attorney %>% distinct(IDNCASE) %>% nrow() # 10386
# note that all ID in this table had an attroney, so it's a simple flag
bia_attorney %>% filter(!is.na(AttorneyName)) %>% nrow() # 10801


##########################


# add ij_attorney and bia_attorney flags to eoir
eoir <- eoir %>% left_join(., ij_attorney %>% distinct(IDNCASE) %>% mutate(ij_attorney_flag = 1), by = c("idncase" = "IDNCASE")) %>%
        left_join(., bia_attorney %>% distinct(IDNCASE) %>% mutate(bia_attorney_flag = 1), by = c("idncase" = "IDNCASE")) %>%
        mutate(ij_attorney_flag = case_when(is.na(ij_attorney_flag) ~ 0, TRUE ~ ij_attorney_flag),
               bia_attorney_flag = case_when(is.na(bia_attorney_flag) ~ 0, TRUE ~ bia_attorney_flag))


#########################


# inspect
eoir %>% glimpse()
eoir %>% nrow() # 214457
eoir %>% distinct(anumber) %>% nrow() # 214457
eoir %>% count(ij_attorney_flag) # 188840
eoir %>% count(bia_attorney_flag) # 10386


################################################################################################################################################


# load applications table
applications <- read_csv("data/EOIR/eoir_data_from_doj/04 - Appl Information (ASYL_ ASYW_ WCAT_ 42A_B).csv")

# inspect
applications %>% glimpse()
applications %>% nrow() # 448819
applications %>% distinct(IDNCASE) %>% nrow() # 193052
applications %>% distinct(IDNPROCEEDING) %>% nrow() # 193052
applications %>% count(APPL_DEC) %>% arrange(desc(n))
applications %>% count(APPL_CODE) %>% arrange(desc(n))
# note that asylum (~42k), then cancellation (~12k) are the most common granted relief types, with ~4k asylum withholding, and 655 convention against torture
# in Final table, doj analysts lumped asylum witholding into relief granted, so we'll keep that
# but we will breakout cancellation as its own category since it's larger and of interest due to NPRM citing it as example of surge in non-meritorious claims
applications %>% filter(APPL_DEC %in% c("F", "G", "R")) %>% count(APPL_CODE) %>% arrange(desc(n))
applications %>% distinct(IDNCASE, APPL_DEC, APPL_CODE) %>%
        group_by(APPL_CODE) %>% count(APPL_DEC) %>% arrange(APPL_CODE, desc(n)) %>% ungroup() %>% print(n = nrow(.))

# note there are 15181 idncases in eoir but not found in applications
# brett w doj said this is because the referred person chooses not to file for asylum with EOIR, 
# so they never technically get an application in the eoir system, though they do get a ij_decision
# this explanation makes sense in a way, though there is a variety of ij_decisions for this group
# there are only 195 relief granted, and those could be cancellation_of_removal?? 
# it's not clear how else a relief_granted ij_decision could be issued if no application is filed w EOIR
eoir %>% filter(!(idncase %in% as.character(applications$IDNCASE))) %>% distinct(idncase) %>% nrow() # 15171
eoir %>% filter(!(idncase %in% as.character(applications$IDNCASE))) %>% distinct(idncase, ij_decision) %>% 
        count(ij_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir %>% filter(!(idncase %in% as.character(applications$IDNCASE))) %>% distinct(idncase, case_type) %>% 
        count(case_type) %>% arrange(desc(n)) %>% print(n = nrow(.))
# all applications idncase are in eoir
applications %>% filter(!(as.character(IDNCASE) %in% eoir$idncase)) %>% distinct(IDNCASE) %>% nrow() # 0


# note that 15394 anumbers had multiple decision_dates
applications %>% distinct(IDNCASE, DECISION_DATE) %>% count(IDNCASE) %>% arrange(desc(n))
applications %>% distinct(IDNCASE, DECISION_DATE) %>% count(IDNCASE) %>% arrange(desc(n)) %>% filter(n > 1) %>% nrow() # 15394
applications %>% distinct(IDNCASE, DECISION_DATE) %>% count(IDNCASE) %>% arrange(desc(n)) %>% filter(n > 1) %>% distinct(IDNCASE) %>%
        left_join(., applications, by = "IDNCASE") 

# note that 58708 anumbers have multiple appl_recd date
applications %>% distinct(IDNCASE, APPL_RECD_DATE) %>% count(IDNCASE) %>% arrange(desc(n))
applications %>% distinct(IDNCASE, APPL_RECD_DATE) %>% count(IDNCASE) %>% arrange(desc(n)) %>% filter(n > 1) %>% nrow() # 58708

# note that that anumbers have between 1-3 APPL_CODE types
applications %>% distinct(IDNCASE, APPL_CODE) %>% group_by(IDNCASE) %>% mutate(appl_code_n_distinct = n_distinct(APPL_CODE)) %>%
        ungroup() %>% distinct(IDNCASE, appl_code_n_distinct) %>% count(appl_code_n_distinct)

# note that 212 anumbers have multiple instances of the same APPL_CODE with difference decision_dates
applications %>% distinct(IDNCASE, APPL_CODE, DECISION_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n))
applications %>% distinct(IDNCASE, APPL_CODE, DECISION_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n)) %>% filter(n > 1) %>% nrow() # 212
applications %>% distinct(IDNCASE, APPL_CODE, DECISION_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n)) %>% distinct(IDNCASE) %>%
        left_join(., applications, by = "IDNCASE")

# note that 4118 anumbers have multiple instancse of the same APPL_CODE with different received_dates
# though note that some of these have only one decision_date
applications %>% distinct(IDNCASE, APPL_CODE, APPL_RECD_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n))
applications %>% distinct(IDNCASE, APPL_CODE, APPL_RECD_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n)) %>% filter(n > 1) %>% nrow() # 4118
applications %>% distinct(IDNCASE, APPL_CODE, APPL_RECD_DATE) %>% count(IDNCASE, APPL_CODE) %>% arrange(desc(n)) %>% distinct(IDNCASE) %>%
        left_join(., applications, by = "IDNCASE") %>% print(n = 20)

# inspect cancellation of removal
# note that 42A is permanent resident app for cancellation; 42B is alien app, so will only include 42B
# https://www.justice.gov/sites/default/files/pages/attachments/2015/07/24/eoir42a.pdf
# https://www.justice.gov/sites/default/files/pages/attachments/2015/07/24/eoir42b.pdf
# note that 41408 applied for cancellation of removal
applications %>% filter(APPL_CODE == "42B") %>% distinct(IDNCASE) %>% nrow() # 41408
# only 111 anumbers applied for 42A though, the vast majority are for 42B
applications %>% filter(APPL_CODE == "42B") %>% distinct(IDNCASE, APPL_CODE) %>% count(APPL_CODE)
# note that 276 have multiple received/decision_dates for cancellation records
applications %>% filter(APPL_CODE == "42B") %>% distinct(IDNCASE, APPL_RECD_DATE, DECISION_DATE) %>%
        count(IDNCASE) %>% filter(n > 1) %>% nrow() # 276
# looking at just 42B, only 44 anumbers have multiple decision_values - will just take the lastest
applications %>% filter(APPL_CODE == "42B") %>% distinct(IDNCASE, APPL_DEC) %>%
        count(IDNCASE) %>% filter(n > 1) %>% nrow() # 44

# confirm that final table used most recent decision_date based on this application table
# note you may need to add "eoir_" prefix to variable names, since this section was wrote after that prefix is added below, so eoir tibble is already renamed
# eoir <- eoir %>% rename_at(.vars = -anumber, .funs = ~ str_c("eoir_", .x))

# results: after dropping records with NA DECISION_DATE or NA eoir_ij_decision_date, all records in final table were most recent decision_date
applications %>% group_by(IDNCASE) %>% arrange(desc(DECISION_DATE)) %>% 
        add_group_index(group_vars = DECISION_DATE, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>% distinct(IDNCASE, DECISION_DATE) %>%
        rename(eoir_idncase = IDNCASE) %>% mutate(eoir_idncase = as.character(eoir_idncase)) %>%
        left_join(., eoir, by = "eoir_idncase") %>% 
        mutate(same_decision_date = case_when(mdy(DECISION_DATE) == eoir_ij_decision_date ~ 1, TRUE ~ 0)) %>%
        filter(!is.na(DECISION_DATE), !is.na(eoir_ij_decision_date)) %>%
        count(same_decision_date)
# note dropped records with non-NA DECISION_DATE but NA eoir_ij_decision_date is expected due mainly (>90%) to Reserved cancellation and Withdrawn, 
applications %>% group_by(IDNCASE) %>% arrange(desc(DECISION_DATE)) %>% 
        add_group_index(group_vars = DECISION_DATE, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>% distinct(IDNCASE, DECISION_DATE, APPL_DEC) %>%
        rename(eoir_idncase = IDNCASE) %>% mutate(eoir_idncase = as.character(eoir_idncase)) %>%
        left_join(., eoir, by = "eoir_idncase") %>% 
        mutate(same_decision_date = case_when(mdy(DECISION_DATE) == eoir_ij_decision_date ~ 1, TRUE ~ 0)) %>%
        filter(!is.na(DECISION_DATE), is.na(eoir_ij_decision_date)) %>% count(APPL_DEC) %>% arrange(desc(n))

# manually inspect decision_dates
applications %>% group_by(IDNCASE) %>% arrange(desc(DECISION_DATE)) %>% 
        add_group_index(group_vars = DECISION_DATE, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>% distinct(IDNCASE, DECISION_DATE) %>%
        rename(eoir_idncase = IDNCASE) %>% mutate(eoir_idncase = as.character(eoir_idncase)) %>%
        left_join(., eoir, by = "eoir_idncase") %>% 
        mutate(same_decision_date = case_when(mdy(DECISION_DATE) == eoir_ij_decision_date ~ 1, TRUE ~ 0)) %>%
        filter(!is.na(DECISION_DATE), same_decision_date == 0) %>% 
        select(anumber, eoir_idncase, eoir_case_received_date, eoir_ij_decision, eoir_ij_decision_date)
eoir %>% filter(eoir_ij_decision == "Relief Granted") %>% select(anumber, eoir_idncase, eoir_case_received_date, eoir_ij_decision, eoir_ij_decision_date)
eoir %>% filter(eoir_idncase == "5711726") %>% select(eoir_idncase, eoir_case_received_date, eoir_ij_decision, eoir_ij_decision_date)
applications %>% filter(IDNCASE == "5711726")


# confirm that final table used received date for ASYL app based on this application table
# results: oddly, most records did not match APPL_RECD_DATE and eoir_case_received_date (C_RECD_DATE),
# only explanation is that they are fundamentally different dates maybe?  decision_dates match though, and no other APPL_RECD_DATEs are in application table
# will ignore as a rabbit hole, since DOJ analysts made Final table for us to use, and the decision_dates matching is only essential thing since some are 
# copied over to eoir_outcome in cases where cancellation_granted and DEC_CODE = Reserved, since these had no eoir_ij_decision/date on a technicality
applications %>% 
        # group_by(IDNCASE) %>% arrange(desc(APPL_RECD_DATE)) %>% 
        # add_group_index(group_vars = APPL_RECD_DATE, group_name = "app_received_date_index") %>%
        # ungroup() %>% filter(app_received_date_index == 1) %>% 
        filter(APPL_CODE == "ASYL") %>%
        distinct(IDNCASE, APPL_RECD_DATE) %>%
        rename(eoir_idncase = IDNCASE) %>% mutate(eoir_idncase = as.character(eoir_idncase)) %>%
        left_join(., eoir, by = "eoir_idncase") %>% 
        mutate(same_app_received_date = case_when(mdy(APPL_RECD_DATE) == eoir_ij_decision_date ~ 1, TRUE ~ 0)) %>%
        filter(!is.na(APPL_RECD_DATE), !is.na(eoir_case_received_date)) %>%
        count(same_app_received_date)
applications %>% filter(IDNCASE == "6849995")
eoir %>% filter(eoir_idncase == "6849995") %>% select(eoir_idncase, eoir_case_received_date, eoir_case_input_date, eoir_ij_decision_date, eoir_ij_decision)

# manually inspect received_dates
applications %>% group_by(IDNCASE) %>% arrange(desc(APPL_RECD_DATE)) %>% 
        add_group_index(group_vars = APPL_RECD_DATE, group_name = "app_received_date_index") %>%
        ungroup() %>% filter(app_received_date_index == 1) %>% distinct(IDNCASE, APPL_RECD_DATE, DECISION_DATE, APPL_DEC) %>%
        rename(eoir_idncase = IDNCASE) %>% mutate(eoir_idncase = as.character(eoir_idncase)) %>%
        left_join(., eoir, by = "eoir_idncase") %>% 
        mutate(same_app_received_date = case_when(mdy(APPL_RECD_DATE) == eoir_ij_decision_date ~ 1, TRUE ~ 0)) %>%
        filter(!is.na(APPL_RECD_DATE), !is.na(eoir_case_received_date)) %>%
        select(eoir_idncase, eoir_case_received_date, eoir_case_input_date, eoir_ij_decision_date, eoir_ij_decision, APPL_RECD_DATE, DECISION_DATE, APPL_DEC)


# inspect WCAT 
applications %>% filter(APPL_CODE == "WCAT") %>% distinct(IDNCASE) %>% nrow() # 97867
# note that 97024 of 97867 WCAT applicants also had ASYL applications; and 96453 had ASYW
applications %>% filter(APPL_CODE == "WCAT") %>% distinct(IDNCASE) %>%
        left_join(., applications, by = "IDNCASE") %>% distinct(IDNCASE, APPL_CODE) %>% count(APPL_CODE) %>% arrange(desc(n))


#####################


# get most recent 42B cancellation of removal record

# get cancellation_w_decision
# note there are 19444 anumbers with a 42B APPL_CODE and a non-NA APPL_DEC, but 19523 records, so will filter down to the most recent decision
# after filtering down to most recent decision, there is still 19509 records, so will filter down to the most recent received_date
# and once filtering down to most recent received date, there is 19444 records, one for each of the 19444 anumbers
cancellation_w_decision <- applications %>% filter(APPL_CODE == "42B", !is.na(APPL_DEC)) %>% 
        distinct(IDNCASE, APPL_CODE, APPL_DEC, APPL_RECD_DATE, DECISION_DATE) %>%
        group_by(IDNCASE) %>% arrange(desc(DECISION_DATE)) %>%
        add_group_index(group_vars = DECISION_DATE, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>%
        group_by(IDNCASE) %>% arrange(desc(APPL_RECD_DATE)) %>%
        add_group_index(group_vars = APPL_RECD_DATE, group_name = "app_received_date_index") %>%
        ungroup() %>% filter(app_received_date_index == 1)
cancellation_w_decision %>% nrow() # 19444
cancellation_w_decision %>% distinct(IDNCASE) %>% nrow() # 19444

# get cancellation_wo_decision
cancellation_wo_decision <- applications %>% filter(APPL_CODE == "42B", !(IDNCASE %in% cancellation_w_decision$IDNCASE)) %>% 
        distinct(IDNCASE, APPL_CODE, APPL_DEC, APPL_RECD_DATE, DECISION_DATE) %>%
        group_by(IDNCASE) %>% arrange(desc(APPL_RECD_DATE)) %>%
        add_group_index(group_vars = APPL_RECD_DATE, group_name = "app_received_date_index") %>%
        ungroup() %>% filter(app_received_date_index == 1)
cancellation_wo_decision %>% nrow() # 21964
cancellation_wo_decision %>% distinct(IDNCASE) %>% nrow() # 21964

# combine to get cancellation_records
cancellation_records <- bind_rows(cancellation_w_decision, cancellation_wo_decision) %>% select(-c(decision_date_index, app_received_date_index)) %>%
        mutate(cancellation_applied = 1, cancellation_granted = case_when(APPL_DEC %in% c("F", "G", "R") ~ 1, TRUE ~ 0)) 


####################


# inspect
cancellation_records
cancellation_records %>% glimpse()
cancellation_records %>% nrow() # 41408
cancellation_records %>% distinct(IDNCASE) %>% nrow() # 41408
applications %>% filter(APPL_CODE == "42B") %>% distinct(IDNCASE) %>% nrow() # 41408
cancellation_records %>% count(APPL_DEC) %>% arrange(desc(n))
cancellation_records %>% count(cancellation_granted) # 12801 granted
cancellation_records %>% count(cancellation_applied) # 41408

# APPL_DEC code definitions from Brett at DOJ
# A	ABANDONMENT
# C	CONDITIONAL GRANT
# D	DENY
# F	FULL GRANT
# G	GRANT
# L	GRANT WCAT
# M	NOT ADJUDICATED
# O	OTHER
# R	RESERVED
# S	ADMIN CLOSURE
# T	COV/TRANSFER
# W	WITHDRAWN

# check to see how many records might have data quality issues due to Grant APPL_DEC for multiple APPL_CODEs, 
# eg grant entered for asylum, WCAT, and cancellation
applications %>% filter(IDNCASE %in% (cancellation_records %>% filter(APPL_DEC %in% c("F", "C", "G", "R")) %>% pull(IDNCASE)))
applications %>% filter(IDNCASE %in% (cancellation_records %>% filter(APPL_DEC %in% c("F", "C", "G", "R")) %>% pull(IDNCASE))) %>% 
        distinct(IDNCASE) %>% nrow() # 12801

# get cancellation_pivot by trimming to most recent decision/received_date
cancellation_pivot <- applications %>% filter(IDNCASE %in% (cancellation_records %>% filter(APPL_DEC %in% c("F", "C", "G", "R")) %>% pull(IDNCASE))) %>%
        distinct(IDNCASE, APPL_CODE, APPL_DEC, APPL_RECD_DATE, DECISION_DATE) %>%
        group_by(IDNCASE) %>% arrange(desc(DECISION_DATE)) %>%
        add_group_index(group_vars = DECISION_DATE, group_name = "decision_date_index") %>%
        ungroup() %>% filter(decision_date_index == 1) %>%
        group_by(IDNCASE) %>% arrange(desc(APPL_RECD_DATE)) %>%
        add_group_index(group_vars = APPL_RECD_DATE, group_name = "app_received_date_index") %>%
        ungroup() %>% filter(app_received_date_index == 1) %>% select(-c(decision_date_index, app_received_date_index)) %>%
        pivot_wider(names_from = APPL_CODE, values_from = APPL_DEC)

# inspect cancellation_pivot
cancellation_pivot
cancellation_pivot %>% nrow() # 12801
cancellation_pivot %>% distinct(IDNCASE) %>% nrow() # 12801
cancellation_pivot %>% count(`42B`)
cancellation_pivot %>% filter(!is.na(`42B`)) %>% count(`42B`, ASYL, ASYW, WCAT) %>% arrange(desc(n)) %>% print(n = nrow(.))
# note that there are only 4 anumbers where 42B is granted, along with another app_code being granted - can ignore
# takeaway is that 42B grants can be treated as legitimate flags of cancellation outcome, not a data entry issue where all app_codes get grants automatically
cancellation_pivot %>% filter(!is.na(`42B`), ASYL %in% c("F", "C", "G", "R") | ASYW %in% c("F", "C", "G", "R") | WCAT %in% c("F", "C", "G", "R")) %>% 
        count(`42B`, ASYL, ASYW, WCAT) %>% arrange(desc(n)) %>% print(n = nrow(.))


#######################


# clean cancellation_records variables and add cancellation_records to eoir
eoir <- cancellation_records %>% select(IDNCASE, cancellation_applied, cancellation_granted, DECISION_DATE) %>% 
        rename(idncase = IDNCASE, cancellation_decision_date = DECISION_DATE) %>% mutate(idncase = as.character(idncase)) %>%
        left_join(eoir, ., by = "idncase") %>% 
        mutate(cancellation_applied = case_when(is.na(cancellation_applied) ~ 0, TRUE ~ cancellation_applied),
               cancellation_granted = case_when(is.na(cancellation_granted) ~ 0, TRUE ~ cancellation_granted))


#########################


# inspect
eoir %>% glimpse()
eoir %>% nrow() # 214457
eoir %>% distinct(anumber) %>% nrow() # 214457
eoir %>% count(cancellation_applied)
eoir %>% count(cancellation_granted)
eoir %>% count(cancellation_granted, ij_decision) %>% arrange(cancellation_granted, desc(n)) %>% print(n = nrow(.))

# note that oddly ~4400 cancellation_granted have NA ij_decision - the vast majority have Reserved as app_dec,
# this explains why the ij_decision is NA, because technically reserved is not yet a final decision, since the IJ can't cancel until the cap allows
# but for our purposes of describing the outcomes, this is fair game - will keep as cancellation_granted when creating eoir_outcome variable
# note also that there are 11 anumbers with cancellation_granted = 1, but with non-cancellation ij_decision 
eoir %>% filter(cancellation_granted == 1) %>% count(ij_decision) %>% arrange(desc(n))
# note that the 12 NA APPL_DEC values are for people with multiple 42B records, but all of them also have an F,G, or R 42B record 
eoir %>% filter(cancellation_granted == 1, is.na(ij_decision)) %>% distinct(idncase) %>% mutate(idncase = as.numeric(idncase)) %>%
        left_join(., applications, by = c("idncase" = "IDNCASE")) %>%
        filter(APPL_CODE == "42B") %>% count(APPL_DEC)

# note there are 48 anumbers with ij_decision = cancellation_granted, but no APPL_DEC cancellation
eoir %>% filter(ij_decision == "Final Grant of EOIR 42B/SUSP") %>% distinct(idncase, cancellation_granted) %>% count(cancellation_granted) 

# note that Brett w DOJ said the "Final Grant of EOIR 42B/SUSP" ij_decision was recently added
# and that previously cancellation grants were entered in the database with "Relief Granted" ij_decision
# this can be seen in the charts below
eoir %>% mutate(ij_decision_date_fy = fy(ij_decision_date)) %>% 
        filter(cancellation_granted == 1, ij_decision == "Final Grant of EOIR 42B/SUSP") %>%
        ggplot(data = ., aes(x = ij_decision_date_fy)) + geom_bar()
eoir %>% mutate(ij_decision_date_fy = fy(ij_decision_date)) %>% 
        filter(cancellation_granted == 1, ij_decision == "Relief Granted") %>%
        ggplot(data = ., aes(x = ij_decision_date_fy)) + geom_bar()


################################################################################################################################################


# inspect eoir records for missingness
eoir %>% glimpse()
eoir %>% nrow() # 214457
eoir %>% distinct(anumber) %>% nrow() # 214457
eoir %>% miss_var_summary() %>% print(n = nrow(.))
# note that 6235 anumbers are missing all variables, apparently because they didn't link up in eoir
# at first i wondered if the doj data was only for what doj considers referred by uscis (eg excluding UACs), but the age distro rules out UACs 
# Brett said they pulled any record for those anumbers, and didn't put additional query filters on it, so they don't think these anumbers have an eoir record
eoir %>% filter(is.na(case_input_date)) %>% miss_var_summary() %>% print(n = nrow(.))
# confirm that 6235 have only NA values for every variable
eoir %>% map_dfc(.x = ., .f = ~ as.character(.x)) %>% 
        pivot_longer(cols = -anumber, names_to = "var", values_to = "value") %>%
        distinct(anumber, value) %>% add_count(anumber, name = "anumber_count") %>% 
        filter(is.na(value), anumber_count == 1) %>% nrow() # 6235
eoir %>% filter(is.na(case_input_date)) %>% distinct(anumber) %>% left_join(., i589_princ, by = "anumber") %>%
        mutate(age_at_decision_date = round(as.numeric(decision_date - date_of_birth) / 365)) %>%
        ggplot(data = ., aes(x = age_at_decision_date)) + geom_histogram(binwidth = 1)

# export 6235 missing anumbers
referred_anumbers_missing_from_doj_eoir_data <- eoir %>% map_dfc(.x = ., .f = ~ as.character(.x)) %>% 
        pivot_longer(cols = -anumber, names_to = "var", values_to = "value") %>%
        distinct(anumber, value) %>% add_count(anumber, name = "anumber_count") %>% 
        filter(is.na(value), anumber_count == 1) %>% distinct(anumber)
referred_anumbers_missing_from_doj_eoir_data %>% nrow() # 6235

# inspect missing anumbers
i589 %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber,
                decision_outcome == "Referral") %>% distinct(anumber) %>% nrow() # 6235
i589_princ %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% count(decision_outcome)
i589_princ %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% count(outcome_bucket)
i589_princ %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% 
        select(anumber, filing_date, interview_date, interview_outcome, decision_date, decision_outcome, service_date, close_date, close_reason)
# note that 3483 of the missing anumbers only have a single referral record in the i589_raw data, 
# so it's not the case that data processing potentially mis-coding these records can be the sole explanation for these referrals
i589 %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% 
        add_count(anumber, name = "anumber_count") %>% distinct(anumber, anumber_count) %>% count(anumber_count) # 3483
i589 %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% 
        add_count(anumber, name = "anumber_count") %>% filter(anumber_count == 1) %>% distinct(anumber, decision_outcome) %>% 
        count(decision_outcome) %>% arrange(desc(n)) # 3483

# write_csv(referred_anumbers_missing_from_doj_eoir_data, path = "data/EOIR/referred_anumbers_missing_from_doj_eoir_data.csv")

# also save a version without the leading A on the Anumber to make it easier on Brett's team
# referred_anumbers_missing_from_doj_eoir_data %>%
#         mutate(anumber = str_sub(string = anumber, start = 2)) %>%
#         write_csv(., path = "data/EOIR/referred_anumbers_missing_from_doj_eoir_data_.csv")

# check referred_anumbers_missing_from_doj_eoir_data in eoir_data_from_api
# note onlyk 3414 of 6235 referred but missing from_doj_eoir anumbers are found in eoir_api
eoir_data_from_api %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>% nrow() # 3414
# note that only 348 of 3414 have non-NA case_input or ij_decision_date in eoir_api
eoir_data_from_api %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>%
        select(anumber, case_input_date, ij_decision, ij_decision_date) %>% filter(!is.na(case_input_date) | !is.na(ij_decision_date)) %>%
        nrow() # 348
# note that 3066 of 3414 have NA values for all variables (except anumber, and batch variables)
eoir_data_from_api %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>%
        miss_var_summary() %>% print(n = nrow(.))
eoir_data_from_api %>% filter(anumber %in% referred_anumbers_missing_from_doj_eoir_data$anumber) %>%
        select(-c(batch_load_dt, batch)) %>%
        map_dfc(.x = ., .f = ~ as.character(.x)) %>% pivot_longer(cols = -anumber, names_to = "var", values_to = "value") %>%
        distinct(anumber, value) %>% add_count(anumber, name = "anumber_count") %>% 
        filter(is.na(value), anumber_count == 1) %>% nrow() # 3066

# inspect 6235 records to be dropped due to no returned data from eoir database
eoir %>% filter(is.na(case_input_date)) %>% nrow()
eoir %>% filter(is.na(case_input_date)) %>% miss_var_summary() %>% arrange(pct_miss)
eoir %>% nrow() # 214457
eoir %>% distinct(anumber) %>% nrow() # 214457


#########################


# drop 6235 anumbers that are missing all eoir variables
eoir <- eoir %>% filter(!is.na(case_input_date))


#########################


# inspect
eoir %>% nrow() # 208222
eoir %>% distinct(anumber) %>% nrow() # 208222
214457 - 6235 == 208222


################################################################################################################################################


# compare eoir and i589_princ anumbers
# note that 9 of the anumbers sent to doj_eoir were dropped from i589_princ as part of the 1397 anumbers with decision_dates preceding filing_dates that were dropped
i589_princ %>% nrow() # 672857
i589_princ %>% distinct(anumber) %>% nrow() # 672857
i589_princ %>% filter(anumber %in% eoir$anumber) %>% nrow() # 208213
eoir %>% filter(!(anumber %in% i589_princ$anumber)) %>% nrow() # 9
eoir %>% nrow() # 208222
eoir %>% distinct(anumber) %>% nrow() # 208222


############################


# drop 9 anumbers sent to doj_eoir, but dropped from i589_princ as part of the 1397 anumbers with decision_dates preceding filing_dates 
eoir <- eoir %>% filter(anumber %in% i589_princ$anumber)


############################


eoir %>% nrow() # 208213
eoir %>% distinct(anumber) %>% nrow() # 208213
208222 - 208213 == 9


################################################################################################################################################


# inspect variable
eoir %>% distinct(anumber, case_type) %>% count(case_type) %>% arrange(desc(n))
eoir %>% distinct(anumber, ij_decision) %>% count(ij_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir %>% distinct(anumber, ij_other_completion) %>% count(ij_other_completion) %>% arrange(desc(n)) %>% print(n = nrow(.))

# compare to eoir_data_from_api
eoir_data_from_api %>% glimpse()
eoir_data_from_api %>% nrow() # 202358
eoir_data_from_api %>% distinct(anumber) %>% nrow() # 200074
eoir_data_from_api %>% filter(!is.na(ij_decision_date)) %>% distinct(anumber) %>% nrow() # 104274
# note however that eoir data from doj included ~15k additional referral anumbers from raio's second data extract
eoir %>% filter(!is.na(ij_decision_date)) %>% nrow() # 107135
eoir_data_from_api %>% filter(!is.na(appeal_filed)) %>% distinct(anumber) %>% nrow() # 14108

eoir_data_from_api %>% miss_var_summary() %>% print(n = nrow(.))
eoir_data_from_api %>% distinct(anumber, case_type) %>% count(case_type) %>% arrange(desc(n))
eoir_data_from_api %>% distinct(anumber, ij_decision) %>% count(ij_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir_data_from_api %>% distinct(anumber, ij_other_completion) %>% count(ij_other_completion) %>% arrange(desc(n)) %>% print(n = nrow(.))


# find and remove dupe anumbers to prepare for get_variable_variation
api_dupes <- eoir_data_from_api %>% add_count(anumber, name = "anumber_count") %>% filter(anumber_count > 1)
api_dupes %>% distinct(anumber) %>% nrow() # 2203

# note eoir already is de-duped with one-record-per-anumber
eoir %>% distinct(anumber, case_input_date, ij_decision, ij_decision_date, alien_date_of_birth) %>%
        get_dupes(anumber) 

# get eoir_data_from_api_wo_dupes
eoir_data_from_api_wo_dupes <- eoir_data_from_api %>% filter(!(anumber %in% api_dupes$anumber)) 
eoir_data_from_api_wo_dupes %>% nrow() # 197871
eoir_data_from_api_wo_dupes %>% distinct(anumber) %>% nrow() # 197871

# get variable variation
eoir_api_and_doj_variable_variation <- eoir_data_from_api_wo_dupes %>% distinct(anumber, case_input_date, ij_decision, ij_decision_date, alien_date_of_birth) %>%
        bind_rows(., eoir %>% distinct(anumber, case_input_date, ij_decision, ij_decision_date, alien_date_of_birth) %>% 
                          filter(anumber %in% eoir_data_from_api_wo_dupes$anumber)) %>%
        get_variable_variation(id_vars = anumber)
eoir_api_and_doj_variable_variation

# get record_variation
# 22475 anumbers with variation in ij_decision, 4308 with variation in ij_decision_date, 344 w/ var in case_input_date, 328 w/ var in date_of_birth
eoir_api_and_doj_record_variation <- eoir_data_from_api_wo_dupes %>% distinct(anumber, case_input_date, ij_decision, ij_decision_date, alien_date_of_birth) %>%
        bind_rows(., eoir %>% distinct(anumber, case_input_date, ij_decision, ij_decision_date, alien_date_of_birth) %>% 
                          filter(anumber %in% eoir_data_from_api_wo_dupes$anumber)) %>%
        get_record_variation(id_vars = anumber)

# inspect records with variation
anum_w_ij_decision_variation <- eoir_api_and_doj_record_variation %>% filter(ij_decision_n_distinct > 1) %>% distinct(anumber) %>%
        left_join(., eoir, by = "anumber") %>% select(anumber, case_input_date, ij_decision, ij_decision_date) %>%
        rename_at(.vars = vars(-anumber), .funs = ~ str_c("doj_", .x)) %>%
        left_join(., eoir_data_from_api_wo_dupes, by = "anumber") %>% 
        select(anumber, starts_with("doj"), case_input_date, ij_decision, ij_other_completion, ij_decision_date)
anum_w_ij_decision_variation
anum_w_ij_decision_variation %>% nrow() # 22475
anum_w_ij_decision_variation %>% mutate(doj_case_input_date_fy = fy(doj_case_input_date)) %>% count(doj_case_input_date_fy)
# the variation is spread over several years, not just most recent case_input/decision_dates
anum_w_ij_decision_variation %>% mutate(doj_ij_decision_date_fy = fy(doj_ij_decision_date)) %>% count(doj_ij_decision_date_fy)
# the vast majority of variation is when the doj_ij_decision has a value, but the api_ij_decision has NA
anum_w_ij_decision_variation %>% count(doj_ij_decision, ij_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
anum_w_ij_decision_variation %>% filter(is.na(ij_decision)) %>% nrow() # 21946
# it seems like the api_eoir ij_other_completion variable has a lot of values (eg admin closed, prosecutorial discretion, etc) that are 
# stored in ij_decision in the doj_eoir data - this would account for some of the observed variation in ij_decision
anum_w_ij_decision_variation %>% mutate(doj_ij_decision_date_fy = fy(doj_ij_decision_date)) %>% 
        filter(doj_ij_decision_date_fy == 2016)
eoir_data_from_api %>% count(ij_other_completion) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir %>% count(ij_decision) %>% arrange(desc(n)) %>% arrange(desc(n)) %>% print(n = nrow(.))
anum_w_ij_decision_variation %>% 
        filter(ij_decision == doj_ij_decision) %>% nrow() # 0
# when carrying over ij_other_completion in api_eoir data for recrods with NA ij_decision, 14384 of the 22475 anum w ij_decision variation go away
# the remaining cases are probably similar issues, or more recent dates, etc
# overall - the doj_eoir data is very similar to api_eoir and thus pcqs_eoir - ~ >95% matching 
anum_w_ij_decision_variation %>% mutate(ij_decision = case_when(is.na(ij_decision) ~ ij_other_completion, TRUE ~ ij_decision),
                                        ij_decision = case_when(ij_decision == "Prosecutorial Discretion ? Admin Close" ~ "PD Administrative Closure",
                                                                TRUE ~ ij_decision)) %>%
        filter(ij_decision == doj_ij_decision) %>% nrow() # 14384


################################################################################################################################################


# add "eoir" as variable prefix to prepare for joining with i589 and 765 data
eoir <- eoir %>% rename_at(.vars = vars(-anumber), .funs = ~ str_c("eoir_", .x))
eoir %>% glimpse()


################################################################################################################################################


# inspect eoir_case_received_date, which is the date the NTA was filed with EOIR
# note that case_input_date is the system-generated date that the case was entered into eoir's system
# Brett from DOJ recommended using case_received_date
eoir %>% filter(is.na(eoir_case_received_date)) %>% nrow() # 0
# note there is ~200 anumbers with pre-2009 case_received_date
eoir %>% mutate(eoir_case_received_date_fy = fy(eoir_case_received_date)) %>% count(eoir_case_received_date_fy) %>% print(n = nrow(.))
eoir %>% mutate(eoir_case_received_date_fy = fy(eoir_case_received_date)) %>%
        ggplot(data = ., aes(x = eoir_case_received_date_fy)) + geom_bar()

# inspect sequence of filing_date to case_received
i589_princ %>% distinct(anumber, filing_date) %>% left_join(eoir, ., by = "anumber") %>%
        filter(eoir_case_received_date < filing_date) %>% nrow() # 27931
i589_princ %>% distinct(anumber, filing_date) %>% left_join(eoir, ., by = "anumber") %>%
        filter(eoir_case_received_date < filing_date) %>% 
        mutate(eoir_case_received_date_fy = fy(eoir_case_received_date)) %>%
        ggplot(data = ., aes(x = eoir_case_received_date_fy)) + geom_bar()
# result: the negative_filing_to_case_received cases are all mostly pre-18 yo, very consistent with UAC story
# this likely reflects the UAC surge, which uscis considers a referral, but doesn't issue NTA because they automatically are added to eoir workload
# so eoir considers UAC applications as defensive, since they're not referred by uscis, but uscis considers affirmative
# see final rule footnotes on table 8 & 9
# also, https://crsreports.congress.gov/product/pdf/R/R43599/25
# https://www.uscis.gov/humanitarian/refugees-and-asylum/asylum/minor-children-applying-asylum-themselves
i589_princ %>% distinct(anumber, filing_date, date_of_birth) %>% left_join(eoir, ., by = "anumber") %>%
        filter(eoir_case_received_date < filing_date) %>%
        mutate(age_at_filing_date = round(as.numeric(filing_date - date_of_birth) / 365)) %>%
        ggplot(data = ., aes(x = age_at_filing_date)) + geom_bar()


################################################################################################################################################


# inspect eoir_case_type
eoir %>% count(eoir_case_type) %>% arrange(desc(n))
eoir_data_from_api %>% count(case_type) %>% arrange(desc(n))


###################


# clean eoir_case_type
eoir <- eoir %>% mutate(eoir_case_type = case_when(eoir_case_type == "RMV" ~ "Removal",
                                                   eoir_case_type == "AOC" ~ "Asylum Only Case",
                                                   eoir_case_type == "WHO" ~ "Withholding Only",
                                                   eoir_case_type == "DEP" ~ "Deportation",
                                                   eoir_case_type == "RFR" ~ "Reasonable Fear Case",
                                                   eoir_case_type == "CFR" ~ "Credible Fear Review",
                                                   eoir_case_type == "EXC" ~ "Exclusion",
                                                   eoir_case_type == "REC" ~ "Rescission"))


#####################


# inspect
eoir %>% count(eoir_case_type) %>% arrange(desc(n))


################################################################################################################################################


# inspect ij_decision
eoir %>% count(eoir_ij_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir %>% count(eoir_ij_other_completion) %>% arrange(desc(n)) %>% print(n = nrow(.))
# note that 4778 "Relief Granted" and 4438 NA ij_decision values will get coded as cancellation_of_removal for eoir_outcome
eoir %>% filter(eoir_cancellation_granted == 1) %>% count(eoir_ij_decision) %>% arrange(desc(n))
# note that ij_other_completion is fully recreated in ij_decision, and so ij_other_completion is unnecessary
eoir %>% count(eoir_ij_decision, eoir_ij_other_completion) %>% arrange(desc(n)) %>% print(n = nrow(.))

# and update cancellation of removal flag to have value = 1 for those 48 anumbers with ij_decision = cancellation, but app_table had cancellation_granted_flag = 0
eoir %>% count(eoir_cancellation_granted, eoir_ij_decision) %>% arrange(eoir_cancellation_granted, desc(n)) # 48


######################


# add eoir_outcome to bucket ij_decision
# and update cancellation of removal flag to have value = 1 for those 48 anumbers with ij_decision = cancellation, but app_table had cancellation_granted_flag = 0
eoir <- eoir %>% mutate(eoir_outcome = case_when(eoir_ij_decision == "Final Grant of EOIR 42B/SUSP" | eoir_cancellation_granted == 1 ~ "cancellation_of_removal",
                                                 eoir_ij_decision %in% c("Relief Granted", "Grant") ~ "relief_granted",
                                                 eoir_ij_decision %in% c("Terminated", "PD Termination", "Terminate") ~ "terminated",
                                                 eoir_ij_decision %in% c("Remove", "Deny", "Deport") ~ "removal",
                                                 eoir_ij_decision %in% c("PD Administrative Closure", "Administrative Closing - Other") ~ "admin_closed",
                                                 eoir_ij_decision == "Voluntary Departure" ~ "voluntary_departure",
                                                 eoir_ij_decision %in% c("Failure to Prosecute (DHS cases only)",
                                                                         "Temporary Protected Status", "Other", "Other Administrative Completion",
                                                                         "Abandonment", "Withdraw", "Affirmed - DHS Decision and No Reasonable Fear",
                                                                         "Affirmed - DHS Decision and No Credible Fear",
                                                                         "Jurisdiction Transferred to the BIA", "Exclude", "Haitian", "Rescind",
                                                                         "Vacated - DHS Decision and Found Reasonable Fear") ~ "other",
                                                 is.na(eoir_ij_decision) ~ "pending"),
                        eoir_cancellation_granted = case_when(eoir_outcome == "cancellation_of_removal" ~ 1, TRUE ~ 0))


######################


# inspect
eoir %>% count(eoir_outcome) %>% arrange(desc(n))
eoir %>% count(eoir_outcome, eoir_ij_decision, eoir_cancellation_granted) %>% arrange(eoir_outcome, desc(n)) %>% print(n = nrow(.))
eoir %>% count(eoir_cancellation_granted, eoir_ij_decision) %>% arrange(eoir_cancellation_granted, desc(n)) # 48


################################################################################################################################################


# inspect to add eoir_outcome_date
# note that the 4438 cancellation_granted cases with NA ij_decisions (because the cancellation grant was Reserved) have NA ij_decision_date
# so will use cancellation_decision_date as eoir_outcome_date for these 4438
eoir %>% filter(is.na(eoir_ij_decision_date), !is.na(eoir_outcome)) %>% count(eoir_outcome)


####################


# add eoir_outcome_date
eoir <- eoir %>% mutate(eoir_outcome_date = case_when(eoir_outcome == "cancellation_of_removal" & is.na(eoir_ij_decision_date) ~ mdy(eoir_cancellation_decision_date),
                                              TRUE ~ eoir_ij_decision_date))


###################


# inspect
eoir %>% glimpse()
eoir %>% filter(is.na(eoir_outcome_date)) %>% count(eoir_outcome)


################################################################################################################################################


# inspect custody
eoir %>% count(eoir_custody) %>% arrange(desc(n))


#####################


# clean custody
eoir <- eoir %>% mutate(eoir_custody = case_when(eoir_custody == "N" ~ "never_detained",
                                                 eoir_custody == "R" ~ "released",
                                                 eoir_custody == "D" ~ "detained"))


####################


# inspect custody
eoir %>% count(eoir_custody) %>% arrange(desc(n))


################################################################################################################################################


# inspect bia/case_appeal_decision
# note that Brett from DOJ said trying to interpret the bia_decisions as favorable or unfavorable is very tricky
# he said they don't often do it themselves except in special cases, and really the consensus opinion is that reading the case file is the only way to know
# he gave an example of how the BIA can often rule "in favor" of the alien's appeal, 
# but only on a narrow technical matter that doesn't overturn the final IJ decision
# for this reason, he recommended just sticking with the solid facts about appeal filed, bia_decision_date, but not interpret bia_decisions

# see page 79 of GAO report for how to categorize bia decisions as favorable/unfavorable for alien
# https://www.gao.gov/new.items/d08940.pdf

eoir %>% count(eoir_filed_by)
eoir %>% filter(!is.na(eoir_filed_by)) %>% nrow() # 11660
eoir %>% filter(!is.na(eoir_appeal_filed_date)) %>% nrow() # 11660
# note it's unclear why ~2500 more anumbers show as filing an appeal in eoir_api eoir than it the eoir_doj data??
eoir_data_from_api %>% filter(!is.na(appeal_filed)) %>% distinct(anumber) %>% nrow() # 14108
# eoir_filed_by and eoir_appeal_filed_date are always both valid or both NA
eoir %>% mutate(valid_eoir_appeal_filed_date = case_when(!is.na(eoir_appeal_filed_date) ~ 1, TRUE ~ 0),
                valid_eoir_filed_by = case_when(!is.na(eoir_filed_by) ~ 1, TRUE ~ 0)) %>% count(valid_eoir_appeal_filed_date, valid_eoir_filed_by)

eoir %>% count(eoir_bia_decision) %>% arrange(desc(n)) %>% arrange(desc(n)) %>% print(n = 50)
# 6161 anumbers have a non-NA bia_decision
eoir %>% filter(!is.na(eoir_bia_decision)) %>% nrow() # 6161
eoir %>% count(eoir_case_appeal_decision) %>% arrange(desc(n)) %>% arrange(desc(n)) %>% print(n = 50)
# 6161 anumbers have a non-NA case_appeal_decision (same as bia_decision)
eoir %>% filter(!is.na(eoir_case_appeal_decision)) %>% nrow() # 6161
eoir %>% select(anumber, eoir_case_appeal_decision) %>% left_join(., eoir_data_from_api, by = "anumber") %>%
        select(anumber, eoir_case_appeal_decision, board_decision) %>% count(eoir_case_appeal_decision, board_decision) %>%
        filter(!is.na(eoir_case_appeal_decision), !is.na(board_decision)) %>%
        arrange(desc(n)) %>% print(n = nrow(.))


# note that more anumbers have non-NA board_decision in eoir_data_from_api
eoir_data_from_api %>% filter(!is.na(board_decision)) %>% distinct(anumber) %>% nrow() # 9249
eoir_data_from_api %>% count(board_decision) %>% arrange(desc(n)) %>% print(n = nrow(.))
eoir_data_from_api %>% filter(!is.na(board_decision)) %>% distinct(anumber) %>%
        anti_join(., eoir %>% filter(!is.na(eoir_case_appeal_decision)), by = "anumber") %>%
        left_join(., eoir_data_from_api, by = "anumber") %>% 
        select(anumber, case_input_date, ij_decision_date, ij_decision, appeal_filed, board_decision, board_decision_date) %>%
        rename_at(.vars = vars(-anumber), .funs = ~ str_c("api_", .x)) %>%
        left_join(., data, by = "anumber") %>% 
        select(anumber, filing_date, decision_date, decision_outcome, api_case_input_date, api_ij_decision, api_ij_decision_date,
               api_appeal_filed, api_board_decision, api_board_decision_date, eoir_ij_decision, eoir_ij_decision_date,
               eoir_appeal_filed_date, eoir_case_appeal_decision, eoir_bia_decision_date) %>% slice(1:10) %>% data.frame()


# inspect sequencing between bia_decision and ij_decision

# all non-NA ij_decisions have an ij_decision_date
eoir %>% filter(is.na(eoir_ij_decision_date), !is.na(eoir_ij_decision)) %>% nrow() # 0
# there are no ij_decision_dates without valid ij_decisions
eoir %>% filter(!is.na(eoir_ij_decision_date), is.na(eoir_ij_decision)) %>% nrow() # 0

# all non-NA bia_decisions have a bia_decision_date
eoir %>% filter(is.na(eoir_bia_decision_date), !is.na(eoir_case_appeal_decision)) %>% nrow() # 0
# there are no bia_decision_dates without valid bia_decisions
eoir %>% filter(!is.na(eoir_bia_decision_date), is.na(eoir_case_appeal_decision)) %>% nrow() # 0

# note that no anumbers have ij_decision_date after bia_decision_date
# so the bia_decision_date is always the last action date on a record
eoir %>% filter(eoir_ij_decision_date > eoir_bia_decision_date) %>% nrow() # 0
# 6161 anumbers have ij_decision_date before bia_decision_Date
eoir %>% filter(eoir_ij_decision_date < eoir_bia_decision_date) %>% nrow() # 6161

# duration of appeal
# 0 years - 698 anum, 1 - 3667, 2 - 1768, with like 25 spread across 3-6 years
# since the count of anumbers with completed appeals (6161) is relatively low (3%), and the majority of appeals are completed in 1 yr, 
# might not be worth further analysis in this report
eoir %>% mutate(appeal_years_duration = round(as.numeric(eoir_bia_decision_date - eoir_appeal_filed_date) / 365)) %>%
        count(appeal_years_duration)

# inspect combo of eoir_outcome, filed_by, and case_appeal_decision
eoir %>% filter(!is.na(eoir_case_appeal_decision)) %>% count(eoir_case_appeal_decision, eoir_filed_by, eoir_outcome) %>% 
        arrange(eoir_case_appeal_decision, desc(n)) %>% print(n = nrow(.))


################################################################################################################################################


# add eoir_outcome_date_fy
eoir <- eoir %>% mutate(eoir_outcome_date_fy = fy(eoir_outcome_date))


#####################


# inspect
eoir %>% glimpse()
# note there are ~ 20 anumbers with pre-2009 ij_decision_dates - can ignore
eoir %>% count(eoir_outcome_date_fy) %>% print(n = nrow(.))
eoir %>% ggplot(data = ., aes(x = eoir_outcome_date_fy)) + geom_bar()


################################################################################################################################################


# add eoir_bia_decision_date_fy
eoir <- eoir %>% mutate(eoir_bia_decision_date_fy = fy(eoir_bia_decision_date))


###################


# inspect
eoir %>% glimpse()
# note there are 2 anumbers with pre-2009 bia_decision_dates - can ignore
eoir %>% count(eoir_bia_decision_date_fy) %>% print(n = nrow(.))


################################################################################################################################################


# add eoir_case_received_date_fy
eoir <- eoir %>% mutate(eoir_case_received_date_fy = fy(eoir_case_received_date))


###################


# inspect
eoir %>% glimpse()
# note there are ~ 200 anumbers with pre-2009 case_received_dates - can ignore
eoir %>% count(eoir_case_received_date_fy) %>% print(n = nrow(.))
# note there are 6 non-pending anumbers with negative case_received_to_outcome days
eoir %>% mutate(eoir_case_received_to_outcome_days = as.numeric(eoir_outcome_date - eoir_case_received_date)) %>%
                        filter(eoir_case_received_to_outcome_days < 0) %>% nrow() # 6
eoir %>% filter(eoir_outcome != "pending", eoir_case_received_date > eoir_outcome_date) %>% nrow() # 6


################################################################################################################################################


# inspect case_received_to_outcome_days for outliers
eoir %>% mutate(eoir_case_received_to_outcome_days = as.numeric(eoir_outcome_date - eoir_case_received_date)) %>%
        filter(eoir_case_received_to_outcome_days < 0) %>% nrow() # 6
inspect_case_received_to_outcome_days <- eoir %>% mutate(eoir_case_received_to_outcome_days = as.numeric(eoir_outcome_date - eoir_case_received_date),
                                                         eoir_case_received_to_outcome_days = case_when(eoir_case_received_to_outcome_days < 0 ~ NA_real_, 
                                                                                                        TRUE ~ eoir_case_received_to_outcome_days))
inspect_case_received_to_outcome_days %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()
inspect_case_received_to_outcome_days %>% 
        filter(eoir_case_received_to_outcome_days < 5000) %>%
        ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()

# note final decision is to summarize using median
# get outlier threshold
# 2698 days is the upper outlier threshold
eoir_case_received_to_outcome_days <- inspect_case_received_to_outcome_days %>% pull(eoir_case_received_to_outcome_days)
quantile(x = eoir_case_received_to_outcome_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_case_received_to_outcome_days %>% filter(eoir_case_received_to_outcome_days > 2698) %>% nrow() # 2597
inspect_case_received_to_outcome_days %>% filter(!is.na(eoir_case_received_to_outcome_days)) %>% nrow() # 111564
2597 / 111564 # 2.3%


################


# add eoir_case_received_to_outcome_days
# will set 6 anumbers w negative days to NA
eoir <- eoir %>% mutate(eoir_case_received_to_outcome_days = as.numeric(eoir_outcome_date - eoir_case_received_date),
                        eoir_case_received_to_outcome_days = case_when(eoir_case_received_to_outcome_days < 0 ~ NA_real_,
                                                                        TRUE ~ eoir_case_received_to_outcome_days))


################


# inspect
eoir %>% glimpse()
# note that 6 anumbers got NA'd due to negative eoir_case_input_to_ij_decision_days
eoir %>% filter(!is.na(eoir_outcome_date), !is.na(eoir_case_received_date), is.na(eoir_case_received_to_outcome_days)) %>% nrow() # 6
eoir %>% filter(!is.na(eoir_outcome_date), !is.na(eoir_case_received_date), is.na(eoir_case_received_to_outcome_days)) %>%
        select(anumber, eoir_case_received_date, eoir_outcome_date)
eoir %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()


################################################################################################################################################


# inspect outcome_to_bia_decision_days for outliers
eoir %>% mutate(eoir_outcome_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_outcome_date)) %>%
        filter(eoir_outcome_to_bia_decision_days < 0) %>% nrow() # 0
inspect_outcome_to_bia_decision_days <- eoir %>% mutate(eoir_outcome_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_outcome_date),
                                                         eoir_outcome_to_bia_decision_days = case_when(eoir_outcome_to_bia_decision_days < 0 ~ NA_real_, 
                                                                                                        TRUE ~ eoir_outcome_to_bia_decision_days))
inspect_outcome_to_bia_decision_days %>% ggplot(data = ., aes(x = eoir_outcome_to_bia_decision_days)) + geom_histogram()
inspect_outcome_to_bia_decision_days %>% 
        filter(eoir_outcome_to_bia_decision_days < 1200) %>%
        ggplot(data = ., aes(x = eoir_outcome_to_bia_decision_days)) + geom_histogram()

# note final decision is to summarize with median
# get outlier threshold
# 1045 days is the upper outlier threshold
eoir_outcome_to_bia_decision_days <- inspect_outcome_to_bia_decision_days %>% pull(eoir_outcome_to_bia_decision_days)
quantile(x = eoir_outcome_to_bia_decision_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_outcome_to_bia_decision_days %>% filter(eoir_outcome_to_bia_decision_days > 1045) %>% nrow() # 12
inspect_outcome_to_bia_decision_days %>% filter(!is.na(eoir_outcome_to_bia_decision_days)) %>% nrow() # 6161
12 / 6161 # 0.2%

# inspect for negative eoir_outcome_to_bia_decision_days
eoir %>% filter(eoir_bia_decision_date < eoir_outcome_date) %>% nrow() # 0


##################


# add eoir_outcome_to_bia_decision_days
# note there were 0 negative values that needed to be NA'd
eoir <- eoir %>% mutate(eoir_outcome_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_outcome_date),
                        eoir_outcome_to_bia_decision_days = case_when(eoir_outcome_to_bia_decision_days < 0 ~ NA_real_,
                                                                          TRUE ~ eoir_outcome_to_bia_decision_days))


##################


# inspect
eoir %>% glimpse()
eoir %>% ggplot(data = ., aes(x = eoir_outcome_to_bia_decision_days)) + geom_histogram(binwidth = 1)


################################################################################################################################################


# inspect
eoir %>% mutate(eoir_outcome_to_appeal_filed_days = as.numeric(eoir_appeal_filed_date - eoir_outcome_date)) %>%
        filter(eoir_outcome_to_appeal_filed_days < 0) %>% nrow() # 0
eoir %>% mutate(eoir_outcome_to_appeal_filed_days = as.numeric(eoir_appeal_filed_date - eoir_outcome_date),
                eoir_outcome_to_appeal_filed_days = case_when(eoir_outcome_to_appeal_filed_days < 0 ~ NA_real_,
                                                              TRUE ~ eoir_outcome_to_appeal_filed_days)) %>%
        ggplot(data = ., aes(x = eoir_outcome_to_appeal_filed_days)) + geom_histogram()


#######################


# add eoir_outcome_to_appeal_filed_days
# note there were no negative values to NA
eoir <- eoir %>% mutate(eoir_outcome_to_appeal_filed_days = as.numeric(eoir_appeal_filed_date - eoir_outcome_date),
                        eoir_outcome_to_appeal_filed_days = case_when(eoir_outcome_to_appeal_filed_days < 0 ~ NA_real_,
                                                                      TRUE ~ eoir_outcome_to_appeal_filed_days))


###############


# inspect
eoir %>% ggplot(data = ., aes(x = eoir_outcome_to_appeal_filed_days)) + geom_histogram()


################################################################################################################################################


# inspect
eoir %>% mutate(eoir_appeal_filed_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_appeal_filed_date)) %>%
        filter(eoir_appeal_filed_to_bia_decision_days < 0) %>% nrow() # 0
eoir %>% mutate(eoir_appeal_filed_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_appeal_filed_date),
                eoir_appeal_filed_to_bia_decision_days = case_when(eoir_appeal_filed_to_bia_decision_days < 0 ~ NA_real_,
                                                              TRUE ~ eoir_appeal_filed_to_bia_decision_days)) %>%
        ggplot(data = ., aes(x = eoir_appeal_filed_to_bia_decision_days)) + geom_histogram()


#######################


# add eoir_appeal_filed_to_bia_decision_days
# note there were no negative values to NA
eoir <- eoir %>% mutate(eoir_appeal_filed_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_appeal_filed_date),
                        eoir_appeal_filed_to_bia_decision_days = case_when(eoir_appeal_filed_to_bia_decision_days < 0 ~ NA_real_,
                                                                      TRUE ~ eoir_appeal_filed_to_bia_decision_days))


###############


# inspect
eoir %>% ggplot(data = ., aes(x = eoir_appeal_filed_to_bia_decision_days)) + geom_histogram()


################################################################################################################################################


# inspect case_received_to_bia_decision_days for outliers
inspect_case_received_to_bia_decision_days <- eoir %>% mutate(eoir_case_received_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_case_received_date),
                                                        eoir_case_received_to_bia_decision_days = case_when(eoir_case_received_to_bia_decision_days < 0 ~ NA_real_, 
                                                                                                      TRUE ~ eoir_case_received_to_bia_decision_days))
inspect_case_received_to_bia_decision_days %>% ggplot(data = ., aes(x = eoir_case_received_to_bia_decision_days)) + geom_histogram()
inspect_case_received_to_bia_decision_days %>% 
        filter(eoir_case_received_to_bia_decision_days < 5000) %>%
        ggplot(data = ., aes(x = eoir_case_received_to_bia_decision_days)) + geom_histogram()

# note final decision is to summarize with median
# get outlier threshold
# 2742 days is the upper outlier threshold
eoir_case_received_to_bia_decision_days <- inspect_case_received_to_bia_decision_days %>% pull(eoir_case_received_to_bia_decision_days)
quantile(x = eoir_case_received_to_bia_decision_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_case_received_to_bia_decision_days %>% filter(eoir_case_received_to_bia_decision_days > 2742) %>% nrow() # 132
inspect_case_received_to_bia_decision_days %>% filter(!is.na(eoir_case_received_to_bia_decision_days)) %>% nrow() # 6161
132 / 6161 # 2%

# inspect for negative eoir_case_received_to_bia_decision_days
eoir %>% filter(eoir_bia_decision_date < eoir_case_received_date) %>% nrow() # 0


##################


# add eoir_case_received_to_bia_decision_days
# note that there were 0 negative values 
eoir <- eoir %>% mutate(eoir_case_received_to_bia_decision_days = as.numeric(eoir_bia_decision_date - eoir_case_received_date),
                        eoir_case_received_to_bia_decision_days = case_when(eoir_case_received_to_bia_decision_days < 0 ~ NA_real_,
                                                                         TRUE ~ eoir_case_received_to_bia_decision_days))


#######################a


# inspect
eoir %>% glimpse()
eoir %>% ggplot(data = ., aes(x = eoir_case_received_to_bia_decision_days)) + geom_histogram(binwidth = 1)


################################################################################################################################################


# add eoir_adjudicated_flag

# inspect
eoir %>% count(eoir_outcome) %>% arrange(desc(n))


###########################


# note that eoir guide lists terminated and vol_departure under the heading "Forms of Protection or Relief from Removal", so will include it as adjudicated case 
# https://www.justice.gov/eoir/page/file/eoir_an_agency_guide/download
eoir <- eoir %>% mutate(eoir_adjudicated_flag = case_when(eoir_outcome %in% c("relief_granted", "removal", "cancellation_of_removal", 
                                                                              "terminated", "voluntary_departure") ~ 1, 
                                                          eoir_outcome %in% c("pending", "admin_closed", "other") ~ 0))

######################


# inspect
eoir %>% count(eoir_adjudicated_flag, eoir_outcome) %>% arrange(eoir_adjudicated_flag, desc(n))


################################################################################################################################################


# inspect to add eoir_appeal_filed_flag
eoir %>% select(contains("appeal"))
eoir %>% filter(!is.na(eoir_appeal_filed_date)) %>% count(eoir_appeal_type)
eoir %>% count(eoir_appeal_type)
eoir %>% filter(!is.na(eoir_appeal_filed_date)) %>% count(eoir_appeal_category)
eoir %>% count(eoir_appeal_category)


#############################


# add eoir_appeal_filed_flag
eoir <- eoir %>% mutate(eoir_appeal_filed_flag = case_when(!is.na(eoir_appeal_filed_date) ~ 1, TRUE ~ 0))


#############################


# inspect
eoir %>% filter(!is.na(eoir_appeal_filed_date)) %>% nrow() # 11660
eoir %>% count(eoir_appeal_filed_flag) # 11660


################################################################################################################################################



# inspect case_received_to_terminal_decision_days for outliers
eoir %>% 
        mutate(eoir_case_received_to_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - eoir_case_received_date),
                                                                        !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - eoir_case_received_date),
                                                                        TRUE ~ NA_real_)) %>%
                       filter(eoir_case_received_to_terminal_decision_days < 0) %>% nrow() # 6
inspect_case_received_to_terminal_decision_days <- eoir %>% 
        mutate(eoir_case_received_to_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - eoir_case_received_date),
                                                            !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - eoir_case_received_date),
                                                            TRUE ~ NA_real_),
                eoir_case_received_to_terminal_decision_days = case_when(eoir_case_received_to_terminal_decision_days < 0 ~ NA_real_,
                                                            TRUE ~ eoir_case_received_to_terminal_decision_days))
inspect_case_received_to_terminal_decision_days %>% ggplot(data = ., aes(x = eoir_case_received_to_terminal_decision_days)) + geom_histogram()
inspect_case_received_to_terminal_decision_days %>% 
        filter(eoir_case_received_to_terminal_decision_days < 5000) %>%
        ggplot(data = ., aes(x = eoir_case_received_to_terminal_decision_days)) + geom_histogram()

# note final decision is to summarize using median
# get outlier threshold
# 2740 days is the upper outlier threshold
eoir_case_received_to_terminal_decision_days <- inspect_case_received_to_terminal_decision_days %>% pull(eoir_case_received_to_terminal_decision_days)
quantile(x = eoir_case_received_to_terminal_decision_days, na.rm = TRUE) %>% enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(iqr = `75%` - `25%`, upper_outlier = `75%` + (iqr * 1.5), lower_outlier = `25%` - (iqr * 1.5))
inspect_case_received_to_terminal_decision_days %>% filter(eoir_case_received_to_terminal_decision_days > 2740) %>% nrow() # 2438
inspect_case_received_to_terminal_decision_days %>% filter(!is.na(eoir_case_received_to_terminal_decision_days)) %>% nrow() # 111564
2438 / 111564 # 2%

# inspect for negative eoir_case_received_to_terminal_decision_days
eoir %>% filter(eoir_bia_decision_date < eoir_case_received_date) %>% nrow() # 0


##################


# add eoir_case_received_to_terminal_decision_days
# note there were 6 negative values converted to NA
eoir <- eoir %>% mutate(eoir_case_received_to_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - eoir_case_received_date),
                                                                         !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - eoir_case_received_date),
                                                                         TRUE ~ NA_real_),
                        eoir_case_received_to_terminal_decision_days = case_when(eoir_case_received_to_terminal_decision_days < 0 ~ NA_real_,
                                                                                 TRUE ~ eoir_case_received_to_terminal_decision_days))


###########################


# inspect
eoir %>% ggplot(data = ., aes(x = eoir_case_received_to_terminal_decision_days)) + geom_histogram()
eoir %>% filter(eoir_case_received_to_terminal_decision_days < 0) %>% nrow() # 0


################################################################################################################################################


# add eoir_received_flag
eoir <- eoir %>% mutate(eoir_received_flag = 1)


################################################################################################################################################


# inspect anum with eoir_case_received_date preceding eoir_outcome_date
# note there are only 6, and all are in other eoir_outcome with "failure to prosecutre" ij_decision
# likely the result of an back-dated eoir_outcome_date/ij_decision_date made for administrative reasons
# result: will drop these 6 so that core outcome tables will sum properly regarding filed/adj/pending counts
eoir %>% nrow() # 208213
eoir %>% distinct(anumber) %>% nrow() # 208213
eoir %>% filter(is.na(eoir_case_received_date)) %>% nrow() # 0
eoir %>% filter(!is.na(eoir_outcome_date), eoir_case_received_date > eoir_outcome_date) %>% nrow() # 6
eoir %>% filter(!is.na(eoir_outcome_date), eoir_case_received_date > eoir_outcome_date) %>%
        select(anumber, eoir_case_received_date, eoir_ij_decision, eoir_ij_decision_date, eoir_outcome, eoir_outcome_date)


################


# drop 6 anumbers with eoir_case_received_date preceding eoir_outcome_date
eoir <- eoir %>% filter(is.na(eoir_outcome_date) | eoir_case_received_date <= eoir_outcome_date)


#################


# inspect
eoir %>% nrow() # 208207
eoir %>% distinct(anumber) %>% nrow() # 208207
eoir %>% filter(!is.na(eoir_outcome_date), eoir_case_received_date > eoir_outcome_date) %>% nrow() # 0
eoir %>% filter(!is.na(eoir_outcome_date), eoir_case_received_date > eoir_ij_decision_date) %>% nrow() # 0
208213 - 208207 == 6


################################################################################################################################################


# inspect records with eoir_case_received_date_fy prior to 2009
# note that 307 anum had pre-2009 case_received_date_fy
# and 40 of these 307 also have pre-2009 outcome_date_fy
# also, all 40 with pre-2009 outcome_date_fy have pre-2009 filing_date_fy
# result: best decision is to drop 307 anum with pre-2009 filing/outcome_date_fy, 
# this is defensible since it indicates potential data quality/interpretational issues 
# also, this will make filing/outcome summary tables sum inutitively
# and the relatively small loss of records (0.1% of eoir records) won't significantly affect summary stats
eoir %>% filter(eoir_case_received_date_fy < 2009) %>% nrow() # 307
eoir %>% filter(eoir_case_received_date_fy < 2009, eoir_outcome_date_fy < 2009) %>% nrow() # 40
eoir %>% filter(eoir_outcome_date_fy < 2009) %>% nrow() # 40
eoir %>% nrow() # 208207
307 / 208207


#########################


# drop 307 anumbers with pre-2009 filing/outcome_date_fy
eoir <- eoir %>% filter(!(eoir_case_received_date_fy < 2009))


########################


# inspect
eoir %>% nrow() # 207900
eoir %>% distinct(anumber) %>% nrow() # 207900
eoir %>% filter(eoir_case_received_date_fy < 2009) %>% nrow() # 0
eoir %>% filter(eoir_outcome_date_fy < 2009) %>% nrow() # 0
208207 - 307 == 207900


################################################################################################################################################


# inspect cancellation_of_removal
eoir %>% filter(eoir_cancellation_applied == 1) %>% count(eoir_outcome) 
eoir %>% filter(eoir_cancellation_granted == 1) %>% count(eoir_outcome) 
eoir %>% filter(eoir_outcome == "cancellation_of_removal") %>% count(eoir_cancellation_granted)
# note there are 43 anumbers with eoir_outcome = cancellation, but cancellation_applied flag = 0
eoir %>% filter(eoir_outcome == "cancellation_of_removal") %>% count(eoir_cancellation_applied)


#######################


# update eoir_cancellation_applied flag to be 1 for all eoir_outcome = cancellation
eoir <- eoir %>% mutate(eoir_cancellation_applied = case_when(eoir_outcome == "cancellation_of_removal" & eoir_cancellation_applied == 0 ~ 1,
                                                              TRUE ~ eoir_cancellation_applied)) 



######################


# inspect
eoir %>% filter(eoir_outcome == "cancellation_of_removal") %>% count(eoir_cancellation_applied)


################################################################################################################################################


# inspect absentia
# note that NA absentia values are almost all either pending or cancellation; Y absentia values are almost all removals
# almost all the pending have NA absentia values, but 1 has Y, and 54 have 0; probably these are data quality issues, 
# but since absentia will only be included in analysis as a rate, this can be ignored rather than changed manually
eoir %>% count(eoir_absentia)
eoir %>% filter(eoir_received_flag == 1) %>% count(eoir_absentia, eoir_outcome) %>% arrange(eoir_absentia, desc(n))


##########################


# convert absentia values from Y/N to 1/0
eoir <- eoir %>% mutate(eoir_absentia = case_when(eoir_absentia == "Y" ~ 1, eoir_absentia == "N" ~ 0, is.na(eoir_absentia) ~ NA_real_))


##########################


# inspect
eoir %>% count(eoir_absentia)
eoir %>% filter(eoir_received_flag == 1) %>% count(eoir_absentia, eoir_outcome) %>% arrange(eoir_absentia, desc(n))


################################################################################################################################################


# inspect variables not requiring modifcation
eoir %>% glimpse()
eoir_data_from_api %>% glimpse()

eoir %>% count(eoir_hearing_loc_code) %>% arrange(desc(n)) %>% print(n = 50)
eoir %>% count(eoir_base_city_code) %>% arrange(desc(n)) %>% print(n = 50)
eoir %>% count(eoir_absentia) %>% arrange(desc(n)) %>% print(n = 50)
# not sure what rec_type is, but its not very informative since 671551 have NA value
eoir %>% count(eoir_rec_type) %>% arrange(desc(n)) %>% print(n = 50)
# not sure what motion_type indicates exactly, but only one non-NA value "M" for only 1306, everyone else has NA; not needed in analysis
eoir %>% count(eoir_motion_type) %>% arrange(desc(n)) %>% print(n = 50)
# same with motion_decision, only ~ 1000 have non-NA values; so probably not worth including in analysis
eoir %>% count(eoir_motion_decision) %>% arrange(desc(n)) %>% print(n = 50)
eoir_data_from_api %>% count(ij_mtr_decision)
# 11660 anumbers with "IJ" value - not sure the interpretation, but appeal_type spells out "Case Appeal" for the same 11660, with the rest NA, so it's just a flag
eoir %>% count(eoir_appeal_category) %>% arrange(desc(n)) %>% print(n = 50)
eoir %>% count(eoir_appeal_type) %>% arrange(desc(n)) %>% print(n = 50)
# filed_by has values of NA, A, I, O B, with NA being vast majority (661197), A being bulk of remaining (11232), and I (397), O (19) and B (12)
# probably A = Attorney and I = Individual
eoir %>% count(eoir_filed_by) %>% arrange(desc(n)) %>% print(n = 50)


################################################################################################################################################


# save eoir data

# inspect
eoir %>% glimpse()
eoir %>% nrow() # 207900
eoir %>% distinct(anumber) %>% nrow() # 207900

# eoir %>% write_csv(path = "data/EOIR/doj_eoir_20200408.csv")
eoir <- read_csv("data/EOIR/doj_eoir_20200408.csv")


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# validate eoir data against doj and trac data
eoir %>% nrow() # 207900
eoir %>% distinct(anumber) %>% nrow() # 207900
eoir %>% glimpse()
eoir %>% count(eoir_outcome) %>% arrange(desc(n))

# get fy breakdown for eoir_outcome
# nonte i'm still waiting for confirmation from brett at doj for their business rule to categorize eoir_outcome into grant, deny, admin_closed, other
# based on the doj public report that only shows referral/filings and grants specicially for affirmative asylum, this validates well on the grants as is
# and the i589 referrals have already been validated against doj referral/filings
# https://www.justice.gov/eoir/page/file/1106361/download
# and note that TRAC is validating worse with DOJ's public data on grants
eoir %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>% 
        summarize_at(.vars = vars(starts_with("eoir_outcome.")), .funs = sum) %>%
        filter(eoir_ij_decision_date_fy >= 2009)

# get fy breakdown for case_input
# also validates well based on filings
# note that this eoir data has slightly more filings in 2016, but that is likely due to UACs, 
# this likely reflects the UAC surge, which uscis considers a referral, but doesn't issue NTA because they automatically are added to eoir workload
# so eoir considers UAC applications as defensive, since they're not referred by uscis, but uscis considers affirmative
# see final rule footnotes on table 8 & 9
# also, https://crsreports.congress.gov/product/pdf/R/R43599/25
# https://www.uscis.gov/humanitarian/refugees-and-asylum/asylum/minor-children-applying-asylum-themselves

# note that TRAC's affirmative filings are very at odds with this eoir data and the public eoir data??
eoir %>% group_by(eoir_case_input_date_fy) %>% summarize(referral_count = n()) %>%
        filter(eoir_case_input_date_fy >= 2009)
eoir %>% ggplot(data = ., aes(x = eoir_case_input_date_fy)) + geom_bar()

