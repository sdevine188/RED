library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(scales)
library(readxl)
library(purrr)
library(ggplot2)
library(janitor)
library(datasets)
library(rlang)

# turn off scientific notation
options(scipen=999)

# setwd
setwd("H:/RED/DACA")

# read in c3_elis3
c3_elis3 <- read_csv("Data/c3_elis3_20180503.csv")
glimpse(c3_elis3)


####################################################################################


# original code to flag unable_to_extract_charge

# if(grepl("CHARGE                  1|CHARGE                  001|CHARGE                  01|CHARGE                  003|CHARGE                   04", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE NUMBER|CHARGE                  3|CHARGE                  02|CHARGE                  2|CHARGE                  7", current_keyline, ignore.case = TRUE)|
#    grepl("CHARGE                  002|CHARGE                   02|CHARGE OR DISPOSITION IS NEEDED|CHARGE INFORMATION", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE                  007|CHARGE                  9|CHARGE                  004|CHARGE                  006", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE                  4|CHARGE                  03|CHARGE DESCRIPTION|CHARGE                  005|CHARGE                  07", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE-SEE COMMENT FOR CHARGE|CHARGE                  5|CHARGE                  06|CHARGE                  09", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE                  04|CHARGE                   01|CHARGE                  01|CHARGE                  08", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE                   05|CHARGE                   06", current_keyline, ignore.case = TRUE) |
#    grepl("CHARGE                  05|CHARGE                  6|CHARGE TRACKING NUMBER|COUNTS 1|CHARGE                  8", current_keyline, ignore.case = TRUE) |
#    grepl("ARRESTED OR RECEIVED|NEW ANUMBER|ARREST TRACKING #|DISPOSITION|CHARGE END OF PART|CHARGE DATE AND/OR CHARGE ORI|CHARGE IS DESIRED|CHARGE ORI FOR FILES|CHARGES OR ARRESTS|ARREST CASE NUMBER|ARRESTING AGENCY|ARREST #0[0-9]|ARREST/  [0-9]{4}/[0-9]{2}/[0-9]{2}|ARREST DATE|MRD|STATUTE|SEVERITY", current_keyline, ignore.case = TRUE) |
#    current_keyline == "CHARGE" | current_keyline == "CHARGE LITERAL" | str_to_lower(str_sub(current_keyline, start = 1, end = 6)) == "arrest" | str_to_lower(str_sub(current_keyline, start = 1, end = 5)) == "court") {
#         daca_charges$charge_type[i] <- "keyline_does_not_list_specific_charge"

# create daca_charges_w_only_nonspecific_charge for use as flag later in processing c3_elis2
# daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge") %>% distinct(anum) %>% count() # 89802
# all_daca_charges_anum_wo_nonspecific_charge <- daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge")
# daca_charges_w_only_nonspecific_charge <- daca_charges[!(daca_charges$anum %in% all_daca_charges_anum_wo_nonspecific_charge$anum), ]

# add flag for unable_to_extract_charge
# c3_elis2 <- c3_elis2 %>% mutate(unable_to_extract_charge = ifelse(a_number %in% daca_charges_w_only_nonspecific_charge$anum, 1, 0))
# c3_elis2 %>% filter(unable_to_extract_charge == 1) %>% distinct(a_number) %>% count() # 10681
# daca_charges_w_only_nonspecific_charge %>% distinct(anum) %>% count() # 11756
# daca_charges_w_only_nonspecific_charge %>% filter(anum %in% c3_elis2$a_number) %>% distinct(anum) %>% count() # 10681

# add flag for unable_to_extract_charge
# c3_elis2 <- c3_elis2 %>% mutate(unable_to_extract_charge = ifelse(a_number %in% daca_charges_w_only_nonspecific_charge$anum, 1, 0))


#######################################################################################


daca_charges <- read_csv("daca_charges_20180503.csv")
glimpse(daca_charges) # 1,325,969
daca_charges %>% distinct(anum) %>% count() # 101558


#####################################################################################


# get crosstab on unable to extract charges
# note that unable to extract charges contains many where the charge successfully parsed, but they keyword didn't match
c3_elis3 %>% distinct(ident_report_exist, a_number, unable_to_extract_charge) %>% tabyl(ident_report_exist, unable_to_extract_charge) %>% 
        adorn_percentages() %>% adorn_totals(where = c("col", "row")) %>% adorn_ns() %>% adorn_title()

# get unable_to_extract_charge_a_numbers to investigate raw IDENT reports 
# this is not best way to flag ident reports that didn't parse, since many did correctly parse a charge, but it was not picked up by keyword
# unable_to_extract_charge_a_numbers <- c3_elis3 %>% filter(ident_report_exist == 1, unable_to_extract_charge == 1) %>% distinct(a_number) %>% pull(a_number)
# length(unable_to_extract_charge_a_numbers)

# inspect a_numbers where no potential charges were found (charge dummy flag is always 0)
anumbers_w_at_least_one_charge_flag <- daca_charges %>% group_by(anum) %>% count(charge) %>%
        ungroup() %>% filter(charge == 1) %>% distinct(anum)
dim(anumbers_w_at_least_one_charge_flag) # 93734                    

# get anumbers_w_no_charges
anumbers_w_no_charge_flag <- daca_charges %>% filter(!(anum %in% anumbers_w_at_least_one_charge_flag$anum)) %>% distinct(anum)
dim(anumbers_w_no_charge_flag) # 7824

93734 + 7824 == 101558
daca_charges %>% distinct(anum) %>% nrow() # 101558
daca_raw %>% distinct(a_number) %>% nrow() # 101566

# get summary stats on anumbers_w_no_charge_flag
c3_elis3 %>% mutate(no_charge_flag = ifelse(a_number %in% anumbers_w_no_charge_flag$anum, 1, 0)) %>%
        distinct(ident_report_exist, a_number, no_charge_flag) %>% tabyl(ident_report_exist, no_charge_flag) %>% 
        adorn_percentages() %>% adorn_totals(where = "col") %>% adorn_ns() %>% adorn_title()

# note that some daca_charges anumbers are not in c3_elis3
daca_charges %>% filter(!(anum %in% c3_elis3$a_number)) %>% distinct(anum) %>% nrow() # 3466
anumbers_w_no_charge_flag %>% filter(!(anum %in% c3_elis3$a_number)) %>% distinct(anum) %>% nrow() # 249


######################################################################################


# read in daca_raw
# read in ident data
daca1 <- read_csv("Data/I821_part1.csv", col_names = FALSE)
daca2 <- read_csv("Data/I821_part2.csv", col_names = FALSE)
daca3 <- read_csv("Data/I821_part3.csv", col_names = FALSE)
daca4 <- read_csv("Data/I821_part4.csv", col_names = FALSE)
daca5 <- read_csv("Data/I821_part5.csv", col_names = FALSE)
daca6 <- read_csv("Data/I821_part6.csv", col_names = FALSE)
daca7 <- read_csv("Data/I821_part7.csv", col_names = FALSE)
daca8 <- read_csv("Data/I821_part8.csv", col_names = FALSE)
daca9 <- read_csv("Data/I821_part9.csv", col_names = FALSE)
daca10 <- read_csv("Data/I821_part10.csv", col_names = FALSE)

# combine daca_raw
daca_raw <- rbind(daca1, daca2, daca3, daca4, daca5, daca6, daca7, daca8, daca9, daca10)
glimpse(daca_raw)

# rename variables
daca_raw <- daca_raw %>% rename(a_number = X1, receipt_number = X2, fbi_fin_number = X3, ident_report = X4)
glimpse(daca_raw) # 110848

# add row_id
daca_raw <- daca_raw %>% mutate(row_id = row_number())
glimpse(daca_raw)

# inspect specific ident report
daca_raw %>% filter(a_number == "A204757569") %>% do(cat(.$ident_report)) 
daca_raw %>% filter(a_number == "A200551269") %>% write_csv(., "daca_ident_report.csv")


###################################################################################
#####################################################################
#######################################################################


# inspect a_numbers with multiple ident reports
# they appear to just be duplicate ident reports based on a sampling
daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum) %>% distinct(anum) %>% dim()
length(anumbers_w_no_charge_flag$anum)

# inspect a_numbers with multiple ident_reports
ident_count_per_a_number <- daca_raw %>% count(a_number) %>% arrange(desc(n))
ident_count_per_a_number

ident_count_per_a_number %>% ggplot(., aes(x = n)) + geom_histogram()
ident_count_per_a_number %>% filter(n > 2) %>% ggplot(., aes(x = n)) + geom_histogram()

tally_of_ident_counts_per_a_number <- ident_count_per_a_number %>% rename(ident_count_per_a_number = n) %>% count(ident_count_per_a_number) %>% arrange(desc(n))
tally_of_ident_counts_per_a_number

# inspect ident reports
ident_count_per_a_number %>% filter(n == 5) 
daca_raw %>% filter(a_number == "A096332890") %>% write_csv(., "daca_ident_report.csv")


#################################################################################
#####################################################################
#######################################################################


# inspect daca_raw for unable_to_extract_charge_a_numbers

# get unextracted va
unextracted_va <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                    str_detect(string = ident_report, pattern = regex("virginia criminal record", ignore_case = TRUE))) %>%
        mutate(state = "va")
dim(unextracted_va)               

# get unextracted nj
unextracted_nj <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("NEW JERSEY CRIMINAL HISTORY", ignore_case = TRUE))) %>%
        mutate(state = "nj")
dim(unextracted_nj)     
   
# get unextracted ct
unextracted_ct <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("SPBI-SID#: CT", ignore_case = TRUE))) %>%
        mutate(state = "ct")
dim(unextracted_ct)                  
                    
# get unextracted ok
# unextracted_ok <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
#                                       str_detect(string = ident_report, pattern = regex("OKLAHOMA STATE BUREAU OF INVESTIGATION", ignore_case = TRUE))) %>%
#         mutate(state = "ok")
# dim(unextracted_ok) # 36 
                    
unextracted_oh <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("OHIO \\s*- STATE ID/OH", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "oh")
dim(unextracted_oh) # 277

# get unextracted co
# unextracted_co <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
#                                       str_detect(string = ident_report, pattern = regex("COLORADO BUREAU OF INVESTIGATION", ignore_case = TRUE))) %>%
#         mutate(state = "co")
# dim(unextracted_co) # 173

unextracted_co <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("COLORADO \\s*- STATE ID/CO", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "co")
dim(unextracted_co) # 184
                    
# get unextracted oh
# unextracted_oh <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
#                                       str_detect(string = ident_report, pattern = regex("STATE OF OHIO, OFFICE OF THE ATTORNEY GENERAL", ignore_case = TRUE))) %>%
#         mutate(state = "oh")
# dim(unextracted_oh) # 274

unextracted_oh <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("OHIO \\s*- STATE ID/OH", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "oh")
dim(unextracted_oh) # 277

# get unextracted az
# unextracted_az <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum,
#                                       str_detect(string = ident_report, pattern = regex("ARIZONA DEPARTMENT OF PUBLIC SAFETY", ignore_case = TRUE))) %>%
#         mutate(state = "az")
# dim(unextracted_az)

unextracted_az <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("ARIZONA \\s*- STATE ID/AZ", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "az")
dim(unextracted_az) # 1806

# get unextracted or
unextracted_or <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("OREGON \\s*- STATE ID/OR", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "or")

dim(unextracted_or) #994

# get unextracted fl
# unextracted_fl <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
#                                       str_detect(string = ident_report, pattern = regex("THIS RECORD CONTAINS FLORIDA INFORMATION ONLY", 
#                                                                                         ignore_case = TRUE))) %>%
#         mutate(state = "fl")
# dim(unextracted_fl)

unextracted_fl <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("FLORIDA \\s*- STATE ID/FL", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "fl")
dim(unextracted_fl) # 579

# get unextracted ga
# unextracted_ga <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
#                                       str_detect(string = ident_report, pattern = regex("GEORGIA CRIME INFORMATION CENTER", 
#                                                                                         ignore_case = TRUE))) %>%
#         mutate(state = "ga")
# dim(unextracted_ga)

# get unextracted ga
unextracted_ga <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("GEORGIA \\s*- STATE ID/GA", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "ga")
dim(unextracted_ga) # 614

# get unextracted ny
unextracted_ny <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                     str_detect(string = ident_report, pattern = regex("NEW YORK \\s*- STATE ID/NY", 
                                                                                       ignore_case = TRUE))) %>%
        mutate(state = "ny")
dim(unextracted_ny) # 204

# note that unextracted_ny are almost all no charges found
no_charge_ny <- unextracted_ny %>% filter(str_detect(string = ident_report, pattern = regex("OUR FILES CONTAIN NO CRIMINAL HISTORY INFORMATION FOR THIS INDIVIDUAL",
                                                                                                                  ignore_case = TRUE)) |
                                          str_detect(string = ident_report, 
                                                     pattern = regex("NOT THE SAME INDIVIDUAL ACCORDING TO OUR FILES. \\s*NO CRIMINAL HISTORY INFORMATION WILL BE DISSEMINATED",
                                                                                            ignore_case = TRUE)))  
dim(no_charge_ny) # 193

# inspect unextracted_ny to get those that do have charges
unextracted_ny %>% filter(!(a_number %in% no_charge_ny$a_number))

# get unextracted tn
unextracted_tn <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("TENNESSEE \\s*- STATE ID/TN", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "tn")
dim(unextracted_tn) # 120

# get unextracted id
unextracted_id <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("IDAHO \\s*- STATE ID/ID", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "id")
dim(unextracted_id) # 21

# get unextracted wa
unextracted_wa <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("WASHINGTON \\s*- STATE ID/WA", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "wa")
dim(unextracted_wa) # 6

# get unextracted ks
unextracted_ks <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("KANSAS \\s*- STATE ID/KS", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "ks")
dim(unextracted_ks) # 97

# get unextracted mn
unextracted_mn <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("MINNESOTA \\s*- STATE ID/MN", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "mn")
dim(unextracted_mn) # 42

# get unextracted md
unextracted_md <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("MARYLAND \\s*- STATE ID/MD", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "md")
dim(unextracted_md) # 88

# get unextracted nc
unextracted_nc <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("NORTH CAROLINA \\s*- STATE ID/NC", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "nc")
dim(unextracted_nc) # 101

# get unextracted mo
unextracted_mo <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("MISSOURI \\s*- STATE ID/MO", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "mo")
dim(unextracted_mo) # 25

# get unextracted ar
unextracted_ar <- daca_raw %>% filter(a_number %in% anumbers_w_no_charge_flag$anum, 
                                      str_detect(string = ident_report, pattern = regex("ARKANSAS \\s*- STATE ID/AR", 
                                                                                        ignore_case = TRUE))) %>%
        mutate(state = "ar")
dim(unextracted_ar) # 53


###############################################################

                    
# combine state unextracted
unextracted_known_states <- bind_rows(unextracted_va, unextracted_nj, unextracted_ct, unextracted_ok, unextracted_co, unextracted_oh, 
                                      unextracted_az, unextracted_or, unextracted_fl, unextracted_ga, unextracted_ny, unextracted_tn,
                                      unextracted_id, unextracted_wa, unextracted_ks, unextracted_mn, unextracted_md, unextracted_nc,
                                      unextracted_mo, unextracted_ar)
dim(unextracted_known_states) # 9038
glimpse(unextracted_known_states)
unextracted_known_states %>% distinct(a_number) %>% nrow() # 7567

# inspect unextracted_unknown_states
unextracted_unknown_states_a_numbers <- tibble(a_number = anumbers_w_no_charge_flag$anum) %>% 
        filter(!(a_number %in% unextracted_known_states$a_number)) %>% distinct(a_number)
dim(unextracted_unknown_states_a_numbers) # 257


7567 + 257 == 7824
length(anumbers_w_no_charge_flag$anum) # 7824
257 / 7824


################################################################


# inspect ident reports for unextracted_unknown_states_a_numbers
unextracted_unknown_states_a_numbers
set.seed(123)
unextracted_unknown_states_a_numbers %>% sample_n(., nrow(.))

# inspect specific ident report
daca_charges %>% filter(anum == "A207991992") %>% distinct(keyline, charge_type, charge)
# daca_raw %>% filter(a_number == "A204514605") %>% do(cat(.$ident_report)) 
daca_raw %>% filter(a_number == "A204236199") %>% write_csv(., "daca_ident_report2.csv")


##################################################################
#####################################################################
#######################################################################


# inspect ident reports that appear to pull only charges from single state
glimpse(daca_raw)
fbi <- daca_raw %>% 
        filter(
                # str_detect(string = ident_report, pattern = regex("THIS RAP SHEET WAS PRODUCED IN RESPONSE TO THE FOLLOWING REQUEST")),
               str_detect(string = ident_report, pattern = regex("THIS RECORD CONTAINS .{4,20} INFORMATION ONLY")))
dim(fbi) # 5892

# randomly sample anumbers
set.seed(123)
fbi %>% sample_n(., nrow(.))

# inspect specific ident report
# based on first 5 randomly sampled, all seem to be for florida only
daca_charges %>% filter(anum == "A204423856") %>% distinct(keyline, charge_type, charge)
daca_raw %>% filter(a_number == "A204423856") %>% write_csv(., "daca_ident_report2.csv")


##################################################################
#####################################################################
#######################################################################


# identify state for all ident reports

# get list of us state abbreviations
# note i have to add PR, DC, and also NB (because ident reports use NB as acronym for nebraska)
# we convert NB dummy to merge with NE dummy after flagging states
states <- tibble(state = state.abb) %>% bind_rows(., tibble(state = c("PR", "DC", "NB")))
states

# create test data
# set.seed(123)
# daca_raw_small <- daca_raw %>% sample_n(., size = 20) %>% mutate(row_id = row_number())
# glimpse(daca_raw_small)
# 
# states_small <- tibble(state = c("Z", "TX"))
# glimpse(states_small)
# 
# current_ident_report <- daca_raw_small %>% slice(1) %>% pull(ident_report)
# cat(current_ident_report)
# 
# current_state <- "NV"


##############################################


flag_state_matches <- function(.x, .y){
        current_ident_report <- .x
        current_state <- .y
        
        # create regex strings for various report formats
        current_state_regex_1 <- str_c("STATE ID/", current_state)
        current_state_regex_2 <- str_c("SID-\\s*", current_state)
        ice_regex <- str_c("\n\\s* AGENCY-ICE.*\\(", current_state)
        usss_regex <- str_c("\n\\s* AGENCY-US SECRET SERVICE.*\\(", current_state)
        cbp_regex <- str_c("\n\\s* AGENCY-CBP.*\\(", current_state)
        cbp2_regex <- str_c("\n\\s* AGENCY-CUST-BOR PAT.*\\(", current_state)
        usins_regex <- str_c("\n\\s* AGENCY-USINS.*\\(", current_state)
        dhs_regex <- str_c("\n\\s* AGENCY-DHS.*\\(", current_state)
        state_patrol_regex <- str_c("\n\\s* AGENCY-STATE PATROL.*\\(", current_state)
        police_regex <- str_c("\n\\s* AGENCY-POLICE.*\\(", current_state)
        usm_regex <- str_c("\n\\s* AGENCY-USM.*\\(", current_state)
        sheriff_regex <- str_c("\n\\s* AGENCY-SHERIFF.*\\(", current_state)
        national_park_regex <- str_c("\n\\s* AGENCY-NATIONAL PRK.*\\(", current_state)
        
        # combine regex strings
        current_state_combined_regex <- str_c(current_state_regex_1, current_state_regex_2,
                                              ice_regex, usss_regex, cbp_regex, cbp2_regex, usins_regex, dhs_regex, state_patrol_regex,
                                              police_regex, usm_regex, sheriff_regex, national_park_regex,
                                              sep = "|")
        
        # detect regex strings
        current_state_match_logical <- str_detect(string = current_ident_report, 
                                                  pattern = regex(current_state_combined_regex, ignore_case = TRUE))
        
        # convert str_detect logical output to 0/1 dummies and return
        current_state_match_numeric <- ifelse(current_state_match_logical == TRUE, 1, 0)
        current_state_match_numeric
}

call_flag_state_matches <- function(.x, .y) {
        current_row_id <- .y
        print(current_row_id)
        current_ident_report <- .x
        
        # run flag_state_matches on current_ident_report
        current_ident_report_state_match_flags <- map2(.x = current_ident_report, .y = states$state, .f = flag_state_matches)
        
        # unlist and return results
        current_ident_report_state_match_flags <- unlist(current_ident_report_state_match_flags)
        current_ident_report_state_match_flags
}


flag_state_matches_output <- map2(.x = daca_raw$ident_report, .y = daca_raw$row_id, .f = call_flag_state_matches)
flag_state_matches_output <- flag_state_matches_output %>% transpose()

# create vector of state_flag variable names
state_flag_var_names <- map(.x = states$state, .f = ~ str_c("state_", .x)) %>% unlist()
names(flag_state_matches_output) <- c(state_flag_var_names)
flag_state_matches_output <- as_tibble(flag_state_matches_output) %>% unnest()
glimpse(flag_state_matches_output)

# merge with daca_raw
daca_raw_w_state_flags <- daca_raw %>% bind_cols(., flag_state_matches_output)
glimpse(daca_raw_w_state_flags)


##########################################################


# write daca_raw_w_state_flags
setwd("H:/RED/DACA")
write_csv(daca_raw_w_state_flags, path = "Data/daca_raw_w_state_flags.csv")

# read daca_raw_w_state_flags.csv
daca_raw2 <- read_csv("Data/daca_raw_w_state_flags.csv")
glimpse(daca_raw2)


#########################################################


# inspect daca_raw2
daca_raw2 %>% filter(state_MS == 1) %>% select(a_number)
daca_raw2 %>% filter(a_number == "A204463294") %>% pull(ident_report) %>% cat()

# check values


# get rowSums across state flags
daca_raw2 <- daca_raw2 %>% mutate(state_flag_rowsum = rowSums(select(., starts_with("state"))))
glimpse(daca_raw2)

# check for ident_reports with multiple state_flags
daca_raw2 %>% arrange(desc(state_flag_rowsum)) %>% select(state_flag_rowsum)
daca_raw2 %>% count(state_flag_rowsum)
ggplot(daca_raw2, aes(x = state_flag_rowsum)) + geom_histogram()

# inspect ident_reports with multiple state_flags
setwd("H:/RED/DACA")
daca_raw2 %>% filter(state_flag_rowsum > 3) %>% select(a_number, state_flag_rowsum)
daca_raw2 %>% filter(a_number == "A093087146") %>% pull(ident_report) %>% cat()
daca_raw2 %>% filter(a_number == "A093087146") %>% select(a_number, ident_report) %>% write_csv(., path = "daca_ident_report.csv")

# inspect ident_reports with zero state_flags
daca_raw2 %>% filter(state_flag_rowsum == 0) %>% select(a_number, state_flag_rowsum) %>% slice(11:20)
daca_raw2 %>% filter(a_number == "A204418404") %>% pull(ident_report) %>% cat()


############################################################################


# # loop through ident_reports with zero state_flags to flag using ice / secret service format
# daca_raw2_w_zero_state_flags <- daca_raw2 %>% filter(state_flag_rowsum == 0)
# dim(daca_raw2_w_zero_state_flags)
# 
# daca_raw2_w_zero_state_flags <- daca_raw2_w_zero_state_flags %>% mutate(row_id = row_number())
# glimpse(daca_raw2_w_zero_state_flags)
# 
# flag_state_matches2 <- function(.x, .y){
#         current_ident_report <- .x
#         current_state <- .y
#         # current_state_regex_1 <- str_c("STATE ID/", current_state)
#         # current_state_regex_2 <- str_c("SID-\\s*", current_state)
#         ice_regex <- str_c("\n\\s* AGENCY-ICE.*\\(", current_state)
#         usss_regex <- str_c("\n\\s* AGENCY-US SECRET SERVICE.*\\(", current_state)
#         cbp_regex <- str_c("\n\\s* AGENCY-CBP.*\\(", current_state)
#         usins_regex <- str_c("\n\\s* AGENCY-USINS.*\\(", current_state)
#         dhs_regex <- str_c("\n\\s* AGENCY-DHS.*\\(", current_state)
#         state_patrol_regex <- str_c("\n\\s* AGENCY-STATE PATROL.*\\(", current_state)
#         
#         # current_state_combined_regex <- str_c(current_state_regex_1, current_state_regex_2, sep = "|")
#         current_state_combined_regex <- str_c(ice_regex, usss_regex, cbp_regex, usins_regex, dhs_regex, sep = "|")
#         current_state_match_logical <- str_detect(string = current_ident_report, 
#                                                   pattern = regex(current_state_combined_regex, ignore_case = TRUE))
#         current_state_match_numeric <- ifelse(current_state_match_logical == TRUE, 1, 0)
#         current_state_match_numeric
# }
# 
# call_flag_state_matches2 <- function(.x, .y) {
#         current_row_id <- .y
#         print(current_row_id)
#         current_ident_report <- .x
#         current_ident_report_state_match_flags <- map2(.x = current_ident_report, .y = states$state, .f = flag_state_matches2)
#         current_ident_report_state_match_flags <- unlist(current_ident_report_state_match_flags)
#         # current_ident_report_state_match_flags <- tbl(current_ident_report_state_match_flags)
#         # transpose(current_ident_report_state_match_flags)
#         current_ident_report_state_match_flags
# }
# 
# flag_state_matches_output2 <- map2(.x = daca_raw2_w_zero_state_flags$ident_report, .y = daca_raw2_w_zero_state_flags$row_id, 
#                                    .f = call_flag_state_matches2)
# flag_state_matches_output2 <- flag_state_matches_output2 %>% transpose()
# 
# # create vector of state_flag variable names
# state_flag_var_names <- map(.x = states$state, .f = ~ str_c("state_", .x)) %>% unlist()
# names(flag_state_matches_output2) <- c(state_flag_var_names)
# flag_state_matches_output2 <- as_tibble(flag_state_matches_output2) %>% unnest()
# glimpse(flag_state_matches_output2)
# 
# # merge with daca_raw
# daca_raw2_w_zero_state_flags <- daca_raw2_w_zero_state_flags %>% select(-(starts_with("state")))
# glimpse(daca_raw2_w_zero_state_flags)
# 
# daca_raw2_w_zero_state_flags <- daca_raw2_w_zero_state_flags %>% bind_cols(., flag_state_matches_output2)
# glimpse(daca_raw2_w_zero_state_flags)
# 
# getwd()
# write_csv(daca_raw2_w_zero_state_flags, "Data/daca_raw2_w_zero_state_flags.csv")
# 
# # inspect daca_raw2_w_zero_state_flags
# # get rowSums across state flags
# daca_raw2_w_zero_state_flags <- daca_raw2_w_zero_state_flags %>% mutate(state_flag_rowsum = rowSums(select(., starts_with("state"))))
# glimpse(daca_raw2_w_zero_state_flags)
# 
# # inspect daca_raw2_w_zero_state_flags
# daca_raw2_w_zero_state_flags %>% count(state_flag_rowsum)
# 
# # inspect ident_reports with zero state_flags
# set.seed(123)
# daca_raw2_w_zero_state_flags %>% filter(state_flag_rowsum == 0) %>% select(a_number, state_flag_rowsum) %>% sample_n(., size = 10)
# daca_raw2_w_zero_state_flags %>% filter(a_number == "A079336594") %>% pull(ident_report) %>% cat()
# 
# 
# 
# 
# 
