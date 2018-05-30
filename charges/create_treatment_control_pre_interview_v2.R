library(dplyr)
library(stringr)
library(readr)
library(janitor)
library(lubridate)
library(rlang)
library(readxl)
library(tidyr)
library(caret)
library(purrr)

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")

# look at file names
list.files()

# read in treatment data
sample_frame_raw <- read_csv("Analysis_20180405.csv")
sample_frame <- sample_frame_raw

glimpse(sample_frame )
head(sample_frame )
dim(sample_frame )

#################################################


# read in sample control group to get receipt numbers
# note that sebnem's control group sheet has 396 obs, vs Reid's samp = CNTL with 401 obs.  
# reid is using 401 obs, with NA values for I130 variables for the difference
excel_sheets("I-130 Petition Data for Control Group 3.26.2018.xlsx")
sample_control <- read_excel("I-130 Petition Data for Control Group 3.26.2018.xlsx", sheet = "EDMS Data")
dim(sample_control)
glimpse(sample_control)
head(sample_control)

# remove first row containing variable descriptions
sample_control <- sample_control %>% filter(!(row_number() == 1))
dim(sample_control)

# pull sample_control_from_frame 
sample_frame %>% tabyl(samp)
sample_control_from_frame <- sample_frame %>% filter(samp == "CNTL")
dim(sample_control_from_frame)

# compare sample_control and sample_control_from_frame
# there are two receipts on sample_control but not in reid's analysis file, one is there but has extra "A" on end of receipt number
# the second does not appear to be there - will ask reid why not??
sample_control %>% anti_join(., sample_control_from_frame, by = c("RECEIPT_NUMBER" = "receipt_number")) %>% select(RECEIPT_NUMBER)
sample_control_from_frame %>% anti_join(., sample_control, by = c("receipt_number" = "RECEIPT_NUMBER")) %>% select(receipt_number)

# since there are several Rejected receipt numbers on Sebnem's control group spreadsheet,
# use reid's samp variable to identify 401 control group obs
# add flag on sample_frame for sample_control
sample_frame <- sample_frame %>% mutate(sample_control_flag = ifelse(samp == "CNTL", 1, 0))
sample_frame %>% tabyl(sample_control_flag)


##################################################


# read in treatment group to get receipt numbers
# excel_sheets("I 130 Petition Data for Treatment Group 3.26.18.xlsx")
# treatment <- read_excel("I 130 Petition Data for Treatment Group 3.26.18.xlsx", sheet = "EDMS Data")
# dim(treatment)
# glimpse(treatment)

# pull treatment obs from frame
treatment_from_frame <- sample_frame %>% filter(finalsample == 1)
dim(treatment_from_frame)

# compare treatment and treatment_from_frame

# treatment %>% anti_join(., treatment_from_frame, by = c("RECEIPT_NUMBER" = "receipt_number")) %>% dim()

# create flag on sample_frame using reid's finalsample variable
sample_frame <- sample_frame %>% mutate(treatment_flag = ifelse(finalsample == 1, 1, 0))
sample_frame %>% tabyl(treatment_flag)


###################################################


# add flag for frame_control
sample_frame <- sample_frame %>% mutate(frame_control_flag = ifelse(treatment_flag == 0, 1, 0))
sample_frame %>% tabyl(frame_control_flag)


##################################################


# inspect 
sample_frame %>% tabyl(treatment_flag)
sample_frame %>% tabyl(sample_control_flag)
sample_frame %>% tabyl(frame_control_flag)


#################################################


# filter to just treatment_flag == 1 | sample_control_flag == 1
sample_frame <- sample_frame %>% filter(treatment_flag == 1 | sample_control_flag == 1)


###################################################################
####################################################################
##################################################################


# inspect and clean variables


# clean treatment group and create variables

# inspect SOF_Finding_SP and SOFFindings
# SOF_Finding_SP should be updated version
sample_frame %>% group_by(SOF_Finding_SP) %>% count()
sample_frame %>% group_by(SOFFindings) %>% count()


# looks like value of "Fraud Found" should be converted to "FF"
sample_frame$SOF_Finding_SP[sample_frame$SOF_Finding_SP == "Fraud Found"]
sample_frame$SOF_Finding_SP[sample_frame$SOF_Finding_SP == "Fraud Found"] <- "FF"
sample_frame %>% group_by(SOF_Finding_SP) %>% count()

# per sebnem, the ibfa fraud definition is:
# either SOF = FF & Outcome = Denied or SOF = Inconclusive & Outcome = Denied & Intent to Deny Letter specified marrigage-fraud
# in practice, SOF = FF & Outcome = Denied or SOF = Inconclusive & Outcome = Denied is sufficient
# because there were no cases where SOF = Inconclusive & Outcome = Denied but Letter != marriage-fraud
# any FF/Approve combos will be labeled Inconclusive
# any FF/Pending combos will be labeled Inconclusive

# note as of 20180329, there are 8 FF/Approve that will be labeled Inconclusive
# and 8 Inconclusive/Denied combos that will be labeled FF
# and 3 FF/Pending combos that will be labeled Inconclusive
sample_frame %>% filter(SOF_Finding_SP == "Inconclusive") %>% tabyl(SOF_Finding_SP, Case_Outcome)
sample_frame %>% filter(SOF_Finding_SP == "FF") %>% tabyl(SOF_Finding_SP, Case_Outcome)

# note case_when does not convert values until after all conditions are evaluated, 
# so having FF/Approved converted to Inconclusive would not artificially affect next case_when rule of Inconclusive/Denied = FF
# but for safety, I'll split up the mutate calls
sample_frame <- sample_frame %>% mutate(SOF_Finding_SP = case_when(SOF_Finding_SP == "FF" & Case_Outcome == "Approved" ~ "Inconclusive", 
                                                             TRUE ~ SOF_Finding_SP)) %>%
        mutate(SOF_Finding_SP = case_when(SOF_Finding_SP == "FF" & Case_Outcome == "Pending" ~ "Inconclusive",
                                          TRUE ~ SOF_Finding_SP)) %>%
        mutate(SOF_Finding_SP = case_when(SOF_Finding_SP == "FF" & Case_Outcome == "Denied" |
                                                  SOF_Finding_SP == "Inconclusive" & Case_Outcome == "Denied" ~ "FF",
                                          TRUE ~ SOF_Finding_SP)) 

sample_frame %>% tabyl(SOF_Finding_SP)
sample_frame %>% tabyl(Case_Outcome)
sample_frame %>% tabyl(Case_Outcome, SOF_Finding_SP) %>% adorn_title()

# per sebnem, need to drop the 5 cases with outcome = pending
sample_frame <- sample_frame %>% filter(is.na(Case_Outcome) | Case_Outcome != "Pending")

sample_frame %>% tabyl(SOF_Finding_SP) %>% adorn_totals()
sample_frame %>% tabyl(Case_Outcome)
sample_frame %>% tabyl(Case_Outcome, SOF_Finding_SP) %>% adorn_title()


###########################################################################


# add I130_B8 variable (date of present marriage) from sebnem's file for sample control group, because reid's file the date appears corrupted

# clean I130_B8 variable from sebnem's file
# note, i checked I-130 pdf to get marriage year for MSC1691409846 
sample_control <- sample_control %>% mutate(I130_B8_date = as.character(as.Date(as.numeric(I130_B8), origin = "1899-12-30")))
sample_control %>% distinct(I130_B8_date, I130_B8) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_B8_date, I130_B8) %>% filter(is.na(I130_B8_date))
sample_control <- sample_control %>% mutate(I130_B8_date = case_when(I130_B8 == "1/223/2016" ~ "2016-01-01",
                                                                     I130_B8 == "7/23/216" ~ "2016-07-23",
                                                                     I130_B8 == "5/17/216" ~ "2016-05-17",
                                                                     I130_B8 == "3/15/201" ~ "2016-03-15",
                                                                     I130_B8 == "6/823/2016" ~ "2016-06-01",
                                                                     I130_B8 == "14/15/2016" ~ "2016-01-15",
                                                                     I130_B8 == "611/2016" ~ "2016-01-01",
                                                                     I130_B8 == "99999999" ~ NA_character_, TRUE ~ I130_B8_date)) %>%
        mutate(I130_B8_date = ymd(I130_B8_date))

sample_control %>% select(RECEIPT_NUMBER, I130_B8_date, I130_B8)
sample_control %>% select(RECEIPT_NUMBER, I130_B8_date, I130_B8) %>% filter(is.na(I130_B8_date))

# merge I130_B8_date to sample_frame
sample_frame <- sample_control %>% select(RECEIPT_NUMBER, I130_B8_date) %>%  
        left_join(sample_frame, ., by = c("receipt_number" = "RECEIPT_NUMBER"))

# inspect
sample_frame %>% filter(sample_control_flag == 1) %>% select(receipt_number, I130_B8, I130_B8_date)
sample_frame %>% filter(sample_control_flag == 1, is.na(I130_B8_date)) %>% select(receipt_number, I130_B8, I130_B8_date)


#################################################################


# convert I130_B8 for treatment group
sample_frame %>% filter(treatment_flag == 1) %>% distinct(I130_B8)
sample_frame %>% filter(treatment_flag == 1) %>% summarize(na_count = sum(is.na(I130_B8)))
sample_frame %>% filter(sample_control_flag == 1) %>% summarize(na_count = sum(is.na(I130_B8_date)))

sample_frame <- sample_frame %>% mutate(I130_B8_date = case_when(treatment_flag == 1 ~ ymd(I130_B8), TRUE ~ I130_B8_date))

# inspect
sample_frame %>% filter(treatment_flag == 1) %>% select(receipt_number, treatment_flag, I130_B8, I130_B8_date)
sample_frame %>% filter(sample_control_flag == 1) %>% select(receipt_number, sample_control_flag, I130_B8, I130_B8_date)

sample_frame %>% filter(sample_control_flag == 1) %>% summarize(na_count = sum(is.na(I130_B8_date)))
sample_frame %>% filter(treatment_flag == 1) %>% summarize(na_count = sum(is.na(I130_B8_date)))


#################################################################


# create variable months_since_present_marriage

# find time difference
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% select(I130_B8_date, CONVERTED_RECEIVED_DATE)
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% distinct(CONVERTED_RECEIVED_DATE)
sample_frame <- sample_frame %>% mutate(months_since_present_marriage = case_when(sample_control_flag == 1 | treatment_flag == 1 ~ 
                                                                                          interval(start = I130_B8_date, end = CONVERTED_RECEIVED_DATE) %/% months(1)))

# inspect time difference
# note there is difference of -4 months, maybe a planned marriage?? - will leave it as is
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% select(I130_B8_date, CONVERTED_RECEIVED_DATE, months_since_present_marriage) %>%
        arrange(months_since_present_marriage)
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% select(I130_B8_date, CONVERTED_RECEIVED_DATE, months_since_present_marriage) %>%
        arrange(desc(months_since_present_marriage))

sample_frame %>% ggplot(., aes(x = months_since_present_marriage)) + geom_histogram()
sample_frame %>% filter(sample_control_flag == 1) %>% ggplot(., aes(x = months_since_present_marriage)) + geom_histogram()
sample_frame %>% filter(treatment_flag == 1) %>% ggplot(., aes(x = months_since_present_marriage)) + geom_histogram()


##############################################


# create variable time_since_present_marraige to bucket months_since_present_marriage
sample_frame <- sample_frame %>% 
        mutate(time_since_present_marriage = months_since_present_marriage / 12)
sample_frame %>% tabyl(time_since_present_marriage)

sample_frame %>% ggplot(., aes(x = time_since_present_marriage)) + geom_histogram(binwidth = 1)


################################################################


# create ben_pet_age_diff
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(ageben)
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(agepet)

sample_frame <- sample_frame %>% mutate(ben_pet_age_diff = abs(as.numeric(ageben) - as.numeric(agepet)))

# inspect ben_pet_age_diff
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>%  tabyl(ben_pet_age_diff) 
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>%
        ggplot(data = ., aes(x = ben_pet_age_diff)) + geom_histogram()


##########################


# create variable months_since_ben_immig_proceed
str(sample_frame$I130_C16b)
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(I130_C16b)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I130_C16b)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_C16b)


# convert to date
sample_frame <- sample_frame %>% mutate(I130_C16b = as.character(I130_C16b)) %>% 
        mutate(I130_C16b_date = case_when(is.na(I130_C16b) ~ NA_character_, 
                                     I130_C16b == "99999999" ~ NA_character_, 
                                     I130_C16b == "88888888" ~ "NA_value",
                                     I130_C16b == "19959999" ~ "19950701",
                                     I130_C16b == "20149999" ~ "20140701",
                                     I130_C16b == "20129999" ~ "20120701",
                                     I130_C16b == "20089999" ~ "20080701",
                                     I130_C16b == "20019999" ~ "20010701",
                                     I130_C16b == "20011099" ~ "20011001",
                                     I130_C16b == "20110399" ~ "20110301",
                                     I130_C16b == "PENDING" ~ NA_character_,
                                     str_sub(string = I130_C16b, start = -4) == "/201" ~ NA_character_,
                                     str_sub(string = I130_C16b, start = -4) == "/200" ~ NA_character_,
                                     str_sub(string = I130_C16b, start = -3) == "/20" ~ NA_character_,
                                     I130_C16b == "1994" ~ "19940101",
                                     I130_C16b == "2000" ~ "20000101",
                                     I130_C16b == "2008" ~ "20080101",
                                     I130_C16b == "2012" ~ "20120101",
                                     I130_C16b == "2019" ~ NA_character_,
                                     I130_C16b == "2004//10" ~ "20041001",
                                     I130_C16b == "1/1/1996" ~ "19960101",
                                     I130_C16b == "2/2/1998" ~ "19980202",
                                     I130_C16b == "2/6/1996" ~ "19960206",
                                     I130_C16b == "6/8/2016" ~ "20160608",
                                     I130_C16b == "8/2/2016" ~ "20160802",
                                     I130_C16b == "8/8/2008" ~ "20080808",
                                     I130_C16b == "9/4/2014" ~ "20140904",
                                     I130_C16b == "9/5/2012" ~ "20120905",
                                     I130_C16b == "N" ~ NA_character_,
                                     TRUE ~ I130_C16b))

sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(I130_C16b_date) 

sample_frame <- sample_frame %>% mutate(I130_C16b_date = ymd(I130_C16b_date))

sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(I130_C16b_date) 
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I130_C16b_date)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_C16b_date)

sample_frame %>% filter(is.na(I130_C16b_date), sample_control_flag == 1 | treatment_flag == 1) %>% 
        distinct(sample_control_flag, treatment_flag, I130_C16b, I130_C16b_date) %>% data.frame()


# find time difference
sample_frame <- sample_frame %>% 
        mutate(months_since_ben_immig_proceed = interval(start = I130_C16b_date, end = CONVERTED_RECEIVED_DATE) %/% months(1))

# inspect time difference
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% tabyl(months_since_ben_immig_proceed)
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1, months_since_ben_immig_proceed < 0) %>% 
        select(months_since_ben_immig_proceed, CONVERTED_RECEIVED_DATE, I130_C16b, I130_C16b_date)
sample_frame %>% ggplot(., aes(x = months_since_ben_immig_proceed)) + geom_histogram()
sample_frame %>% filter(!is.na(I130_C16b_date)) %>% select(CONVERTED_RECEIVED_DATE, I130_C16b_date, months_since_ben_immig_proceed)


###########################################


# create variable time_since_ben_immig_proceed to bucket months_since_ben_immig_proceed
sample_frame %>% ggplot(., aes(x = months_since_ben_immig_proceed)) + geom_histogram(binwidth = 12) + 
        scale_x_continuous(breaks = seq(0, max(sample_frame$months_since_ben_immig_proceed, na.rm = TRUE), 12))

sum(is.na(sample_frame$months_since_ben_immig_proceed))

sample_frame <- sample_frame %>%
        mutate(time_since_ben_immig_proceed = months_since_ben_immig_proceed / 12) 

sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% 
        tabyl(time_since_ben_immig_proceed) 
sample_frame %>% filter(sample_control_flag == 1 | treatment_flag == 1) %>% summarize(na_count = sum(is.na(time_since_ben_immig_proceed)))


############################################


# create variable months_since_pet_divorce
str(sample_frame$I130_B12)
sample_frame %>% tabyl(I130_B12)

# add I130_B12 variable (date of present marriage) from sebnem's file for sample control group, because reid's file the date appears corrupted

# clean I130_B12 variable from sebnem's file
sample_control <- sample_control %>% mutate(I130_B12_date = as.character(as.Date(as.numeric(I130_B12), origin = "1899-12-30")))
sample_control %>% distinct(I130_B12_date, I130_B12) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_B12_date, I130_B12) %>% filter(is.na(I130_B12_date)) %>% 
        distinct(I130_B12_date, I130_B12)
sample_control <- sample_control %>% mutate(I130_B12_date = case_when(I130_B12 == "10/9/015" ~ "2015-10-09",
                                                                      I130_B12 == "12/10/213" ~ "2013-12-10",
                                                                      I130_B12 == "5/811/2004" ~ "2004-05-01",
                                                                      I130_B12 == "5816/2013" ~ "2013-01-01",
                                                                      I130_B12 == "7/25/214" ~ "2014-07-25",
                                                                      I130_B12 == "99999999" ~ NA_character_,
                                                                      I130_B12 == "999999999" ~ NA_character_,
                                                                     I130_B12 == "N" ~ NA_character_, TRUE ~ I130_B12_date)) 

sample_control %>% select(RECEIPT_NUMBER, I130_B12_date, I130_B12)
sample_control %>% select(RECEIPT_NUMBER, I130_B12_date, I130_B12) %>% filter(is.na(I130_B12_date)) %>% distinct(I130_B12_date, I130_B12)

# merge I130_B12_date to sample_frame
sample_frame <- sample_control %>% select(RECEIPT_NUMBER, I130_B12_date) %>%  
        left_join(sample_frame, ., by = c("receipt_number" = "RECEIPT_NUMBER"))

# inspect
sample_frame %>% filter(sample_control_flag == 1) %>% select(receipt_number, I130_B12, I130_B12_date)
sample_frame %>% filter(sample_control_flag == 1, is.na(I130_B12_date)) %>% distinct(I130_B12, I130_B12_date)


###############################################################


# clean I130_B12 for treatment group and convert to date
str(sample_frame$I130_B12_date)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_B12)
sample_frame <- sample_frame %>% mutate(I130_B12_date = case_when(I130_B12 == "99999999" ~ NA_character_, 
                                                            I130_B12 == "BLANK" ~ NA_character_,
                                                            I130_B12 == "20111199" ~ "20111101",
                                                            I130_B12 == "20160199" ~ "20160101",
                                                            I130_B12 == "20159999" ~ "20150701",
                                                            I130_B12 == "20071131" ~ "20071130",
                                                            I130_B12 == "20061299" ~ "20061201",
                                                            sample_control_flag == 1 ~ I130_B12_date,
                                                            TRUE ~ as.character(I130_B12)))

sample_frame %>% tabyl(I130_B12_date)
sample_frame %>% filter(sample_control_flag == 1) %>% select(receipt_number, I130_B12, I130_B12_date)

sample_frame <- sample_frame %>% mutate(I130_B12_date = ymd(I130_B12_date))
sample_frame %>% filter(is.na(I130_B12_date)) %>% distinct(I130_B12, I130_B12_date)
sample_frame %>% tabyl(I130_B12_date)

# find time difference
# note will get a warning due to the NA's
sample_frame <- sample_frame %>% 
        mutate(months_since_pet_divorce = interval(start = I130_B12_date, end = CONVERTED_RECEIVED_DATE) %/% months(1))

# inspect time difference
sample_frame %>% filter(is.na(months_since_pet_divorce)) %>% select(I130_B12_date, months_since_pet_divorce) %>% 
        distinct(I130_B12_date, months_since_pet_divorce)
sample_frame %>% filter(!(is.na(months_since_pet_divorce))) %>% select(I130_B12_date, CONVERTED_RECEIVED_DATE, months_since_pet_divorce) 
sample_frame %>% group_by(months_since_pet_divorce) %>% count() %>% data.frame(.)

sample_frame %>% ggplot(., aes(x = months_since_pet_divorce)) + geom_histogram()
sample_frame %>% filter(!is.na(I130_B12_date)) %>% select(CONVERTED_RECEIVED_DATE, I130_B12_date, months_since_pet_divorce)


###################################


# create variable time_since_pet_divorce to bucket months_since_pet_divorce
sample_frame %>% ggplot(., aes(x = months_since_pet_divorce)) + geom_histogram(binwidth = 12) + 
        scale_x_continuous(breaks = seq(0, max(sample_frame$months_since_pet_divorce, na.rm = TRUE), 12))

sum(is.na(sample_frame$months_since_pet_divorce))

sample_frame <- sample_frame %>% 
        mutate(time_since_pet_divorce = months_since_pet_divorce / 12)

sample_frame %>% tabyl(time_since_pet_divorce)
sum(is.na(sample_frame$time_since_pet_divorce))


#################################


# create variable months_since_ben_divorce for sample_control
str(sample_frame$I130_C12)
sample_frame %>% tabyl(I130_C12)

# add I130_C12 variable (date of present marriage) from sebnem's file for sample control group, because reid's file the date appears corrupted

# clean I130_C12 variable from sebnem's file
sample_control <- sample_control %>% mutate(I130_C12_date = as.character(as.Date(as.numeric(I130_C12), origin = "1899-12-30")))
sample_control %>% select(RECEIPT_NUMBER, I130_C12_date, I130_C12)
sample_control %>% distinct(I130_C12_date, I130_C12) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_C12_date, I130_C12) %>% filter(is.na(I130_C12_date)) %>% 
        distinct(I130_C12_date, I130_C12)
sample_control <- sample_control %>% mutate(I130_C12_date = case_when(I130_C12 == "1/22/216" ~ "2016-01-22",
                                                                      I130_C12 == "99999999" ~ NA_character_,
                                                                      I130_C12 == "2015" ~ "2015-01-01",
                                                                      I130_C12 == "N" ~ NA_character_, TRUE ~ I130_C12_date)) 

sample_control %>% distinct(I130_C12_date, I130_C12) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_C12_date, I130_C12) %>% filter(is.na(I130_C12_date)) %>% distinct(I130_C12_date, I130_C12)

# merge I130_C12_date to sample_frame
sample_frame <- sample_control %>% select(RECEIPT_NUMBER, I130_C12_date) %>%  
        left_join(sample_frame, ., by = c("receipt_number" = "RECEIPT_NUMBER"))

# inspect
sample_frame %>% filter(sample_control_flag == 1) %>% distinct(I130_C12, I130_C12_date) %>% data.frame()
sample_frame %>% filter(sample_control_flag == 1, is.na(I130_C12_date)) %>% distinct(I130_C12, I130_C12_date)


#############################################################


# create variable months_since_ben_divorce for treatment group
str(sample_frame$I130_C12)
sample_frame %>% count(I130_C12)

# convert to date
# also convert to character (like other I130 date variables)
sample_frame %>% filter(treatment_flag == 1) %>% distinct(I130_C12, I130_C12_date) %>% data.frame()
sample_frame <- sample_frame %>% mutate(I130_C12_date = case_when(I130_C12 == "99999999" ~ NA_character_, 
                                                            I130_C12 == "20090799" ~ "20090701",
                                                            sample_control_flag == 1 ~ I130_C12_date,
                                                            TRUE ~ as.character(I130_C12)))

sample_frame %>% group_by(I130_C12_date) %>% count() %>% data.frame(.)
sample_frame <- sample_frame %>% mutate(I130_C12_date = ymd(I130_C12_date))
sample_frame %>% filter(is.na(I130_C12_date)) %>% distinct(I130_C12, I130_C12_date)
sample_frame %>% distinct(I130_C12, I130_C12_date) %>% data.frame()

# find time difference
sample_frame <- sample_frame %>% mutate(months_since_ben_divorce = interval(start = I130_C12_date, end = CONVERTED_RECEIVED_DATE) %/% months(1))

# inspect time difference
sample_frame %>% filter(is.na(months_since_ben_divorce)) %>% select(I130_C12_date, months_since_ben_divorce) %>% 
        distinct(I130_C12_date, months_since_ben_divorce)
sample_frame %>% filter(!(is.na(months_since_ben_divorce))) %>% select(I130_C12_date, CONVERTED_RECEIVED_DATE, months_since_ben_divorce) 
sample_frame %>% group_by(months_since_ben_divorce) %>% count() %>% data.frame(.)

sample_frame %>% ggplot(., aes(x = months_since_ben_divorce)) + geom_histogram()


##############################################


# create variable time_since_ben_divorce to bucket months_since_ben_divorce
sample_frame %>% ggplot(., aes(x = months_since_ben_divorce)) + geom_histogram(binwidth = 12) + 
        scale_x_continuous(breaks = seq(0, max(sample_frame$months_since_ben_divorce, na.rm = TRUE), 12))

sum(is.na(sample_frame$months_since_ben_divorce))

sample_frame <- sample_frame %>% 
        mutate(time_since_ben_divorce = months_since_ben_divorce / 12)

sample_frame %>% tabyl(time_since_ben_divorce)
sum(is.na(sample_frame$time_since_ben_divorce))


##############################################


# create variable months_since_ben_last_arrival
# note this variable is not coded for sample_control
str(sample_frame$I485_DATE_LA)
sample_frame %>% tabyl(I485_DATE_LA)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I485_DATE_LA)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I485_DATE_LA)


# convert to date
# also convert to character (like other I130 date variables)
sample_frame$I485_DATE_LA <- as.character(sample_frame$I485_DATE_LA)

# there are over 60 with day values = 99, so replace with 01
sample_frame <- sample_frame %>%
        mutate(I485_DATE_LA = ifelse(str_sub(I485_DATE_LA, start = -2) == "99",
                                     str_c(str_sub(I485_DATE_LA, start = 1, end = -3), "01"), I485_DATE_LA))

sample_frame %>% group_by(I485_DATE_LA) %>% count() %>% data.frame(.)


sample_frame <- sample_frame %>% mutate(I485_DATE_LA_date = case_when(I485_DATE_LA == "99999901" ~ NA_character_,
                                                                I485_DATE_LA == "88888888" ~ NA_character_,
                                                                I485_DATE_LA == "SA" ~ NA_character_,
                                                                I485_DATE_LA == "1998" ~ "19980701",
                                                                I485_DATE_LA == "20009901" ~ "20000701",
                                                                I485_DATE_LA == "19899901" ~ "19890701",
                                                                I485_DATE_LA == "19959901" ~ "19950701",
                                                                TRUE ~ I485_DATE_LA))

sample_frame %>% tabyl(I485_DATE_LA_date)
sample_frame <- sample_frame %>% mutate(I485_DATE_LA_date = ymd(I485_DATE_LA_date))
sample_frame %>% filter(is.na(I485_DATE_LA_date)) %>% distinct(I485_DATE_LA, I485_DATE_LA_date)
sample_frame %>% group_by(I485_DATE_LA_date) %>% count()

# find time difference
sample_frame <- sample_frame %>% 
        mutate(months_since_ben_last_arrival = interval(start = I485_DATE_LA_date, end = CONVERTED_RECEIVED_DATE) %/% months(1))

# inspect time difference
# will get warning because of NA
sample_frame %>% distinct(I485_DATE_LA_date, CONVERTED_RECEIVED_DATE, months_since_ben_last_arrival) %>% data.frame()
sample_frame %>% filter(is.na(months_since_ben_last_arrival)) %>% distinct(I485_DATE_LA_date, months_since_ben_last_arrival)
sample_frame %>% group_by(months_since_ben_last_arrival) %>% count() %>% data.frame(.)

sample_frame %>% ggplot(., aes(x = months_since_ben_last_arrival)) + geom_histogram()
sample_frame %>% filter(!is.na(I485_DATE_LA_date)) %>% select(CONVERTED_RECEIVED_DATE, I485_DATE_LA_date, months_since_ben_last_arrival)


#####################################################


# create variable time_since_ben_last_arrival  to bucket months_since_ben_last_arrival
sample_frame %>% ggplot(., aes(x = months_since_ben_last_arrival)) + geom_histogram(binwidth = 12) + 
        scale_x_continuous(breaks = seq(0, max(sample_frame$months_since_ben_last_arrival, na.rm = TRUE), 12))

sum(is.na(sample_frame$months_since_ben_last_arrival))

sample_frame <- sample_frame %>% 
        mutate(time_since_ben_last_arrival = months_since_ben_last_arrival / 12)

sample_frame %>% tabyl(time_since_ben_last_arrival)
sum(is.na(sample_frame$time_since_ben_last_arrival))


###############################################################


# create variable months_since_cohabitation to bucket I130_C21_Start for sample_control
str(sample_frame$I130_C21_Start)
sample_frame %>% tabyl(I130_C21_Start)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I130_C21_Start)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_C21_Start)

# add I130_C21_Start variable from sebnem's file for sample control group, because reid's file the date appears corrupted

# clean I130_C21_Start variable from sebnem's file
sample_control <- sample_control %>% mutate(I130_C21_Start_date = as.character(as.Date(as.numeric(I130_C21_Start), origin = "1899-12-30")))
sample_control %>% select(RECEIPT_NUMBER, I130_C21_Start_date, I130_C21_Start)
sample_control %>% distinct(I130_C21_Start_date, I130_C21_Start) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_C21_Start_date, I130_C21_Start) %>% filter(is.na(I130_C21_Start_date)) %>% 
        distinct(I130_C21_Start_date, I130_C21_Start)
sample_control <- sample_control %>% mutate(I130_C21_Start_date = case_when(I130_C21_Start == "6/20015" ~ NA_character_,
                                                                      I130_C21_Start == "99999999" ~ NA_character_,
                                                                      I130_C21_Start == "2016" ~ "20160101",
                                                                      I130_C21_Start == "2012" ~ "20120101",
                                                                      I130_C21_Start == "2013" ~ "20130101",
                                                                      I130_C21_Start == "2014" ~ "20160101",
                                                                      I130_C21_Start == "2015" ~ "20160101",
                                                                      I130_C21_Start == "2011" ~ "20160101",
                                                                      I130_C21_Start == "2009" ~ "20160101",
                                                                      I130_C21_Start == "2001" ~ "20160101",
                                                                      TRUE ~ I130_C21_Start_date)) 

sample_control %>% distinct(I130_C21_Start_date, I130_C21_Start) %>% data.frame()
sample_control %>% select(RECEIPT_NUMBER, I130_C21_Start_date, I130_C21_Start) %>% filter(is.na(I130_C21_Start_date)) %>% 
        distinct(I130_C21_Start_date, I130_C21_Start)

# merge I130_C21_Start_date to sample_frame
sample_frame <- sample_control %>% select(RECEIPT_NUMBER, I130_C21_Start_date) %>%  
        left_join(sample_frame, ., by = c("receipt_number" = "RECEIPT_NUMBER"))

# inspect
sample_frame %>% filter(sample_control_flag == 1) %>% distinct(I130_C21_Start, I130_C21_Start_date) %>% data.frame()
sample_frame %>% filter(sample_control_flag == 1, is.na(I130_C21_Start_date)) %>% distinct(I130_C21_Start, I130_C21_Start_date)



#################################################################3


# create variable months_since_cohabitation to bucket I130_C21_Start for treatment group

# there are over 60 with day values = 99, so replace with 01
sample_frame <- sample_frame %>% 
        mutate(I130_C21_Start = ifelse(str_sub(I130_C21_Start, start = -2) == "99", 
                                       str_c(str_sub(I130_C21_Start, start = 1, end = -3), "01"), I130_C21_Start))

sample_frame %>% select(I130_C21_Start)

sample_frame <- sample_frame %>% mutate(I130_C21_Start_date = case_when(I130_C21_Start == "99999901" ~ NA_character_,
                                                                  I130_C21_Start == "88888888" ~ NA_character_,
                                                                  I130_C21_Start == "999901" ~ NA_character_,
                                                                  I130_C21_Start == "BLANK" ~ NA_character_, 
                                                                  I130_C21_Start == "201509" ~ "20150901",
                                                                  I130_C21_Start == "201511" ~ "20151101",
                                                                  I130_C21_Start == "201605" ~ "20160501",
                                                                  I130_C21_Start == "20139901" ~ "20130701",
                                                                  I130_C21_Start == "20159901" ~ "20150701",
                                                                  I130_C21_Start == "20129901" ~ "20120701",
                                                                  I130_C21_Start == "20149901" ~ "20140701",
                                                                  I130_C21_Start == "20119901" ~ "20110701",
                                                                  sample_control_flag == 1 ~ I130_C21_Start_date,
                                                                  TRUE ~ I130_C21_Start))

sample_frame %>% group_by(I130_C21_Start_date) %>% count() %>% data.frame(.)
sample_frame <- sample_frame %>% mutate(I130_C21_Start_date = ymd(I130_C21_Start_date))
sample_frame %>% filter(is.na(I130_C21_Start_date)) %>% distinct(I130_C21_Start, I130_C21_Start_date)
sample_frame %>% group_by(I130_C21_Start_date) %>% count()

# set any future dates as NA
sample_frame %>% filter(I130_C21_Start_date > "2017-01-01") %>% select(I130_C21_Start, I130_C21_Start_date)
sample_frame <- sample_frame %>% mutate(I130_C21_Start_date = case_when(I130_C21_Start_date > CONVERTED_RECEIVED_DATE ~ ymd(NA),
                                                                        TRUE ~ I130_C21_Start_date))

# find time difference
# will get warning because of NA
sample_frame <- sample_frame %>% 
        mutate(months_since_cohabitation = interval(start = I130_C21_Start_date, end = CONVERTED_RECEIVED_DATE) %/% months(1))

# inspect time difference
sample_frame %>% distinct(I130_C21_Start_date, CONVERTED_RECEIVED_DATE, months_since_cohabitation)
sample_frame %>% filter(is.na(months_since_cohabitation)) %>% distinct(I130_C21_Start, I130_C21_Start_date, months_since_cohabitation)
sample_frame %>% group_by(months_since_cohabitation) %>% count() %>% data.frame(.)

sample_frame %>% ggplot(., aes(x = months_since_cohabitation)) + geom_histogram()
sample_frame %>% filter(!is.na(I130_C21_Start_date)) %>% select(CONVERTED_RECEIVED_DATE, I130_C21_Start_date, months_since_cohabitation)


##########################################


# create variable time_since_cohabitation to bucket months_since_cohabitation
sample_frame %>% ggplot(., aes(x = months_since_cohabitation)) + geom_histogram(binwidth = 12) + 
        scale_x_continuous(breaks = seq(0, max(sample_frame$months_since_cohabitation, na.rm = TRUE), 12))

sum(is.na(sample_frame$months_since_cohabitation))

sample_frame <- sample_frame %>% 
        mutate(time_since_cohabitation = months_since_cohabitation / 12)

sample_frame %>% tabyl(time_since_cohabitation)
sum(is.na(sample_frame$time_since_cohabitation))


############################################################


# create ben_entry_status_group variable bucketing I130_C14Calc
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I130_C14Calc)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_C14Calc)

sample_frame %>% tabyl(I130_C14Calc) 
sample_frame <- sample_frame %>% 
        mutate(ben_entry_status_group = case_when(is.na(I130_C14Calc) ~ "NA_value",
                                                I130_C14Calc == "B2" ~ "Visitor",
                                                  I130_C14Calc == "F1" ~ "Student",
                                                  I130_C14Calc == "VW" ~ "Visitor",
                                                  I130_C14Calc == "J1" ~ "Student",
                                                  I130_C14Calc == "PAROLED" ~ "Parolee",
                                                  I130_C14Calc == "E2" ~ "Employment",
                                                  I130_C14Calc == "H1B" ~ "Employment",
                                                  I130_C14Calc == "DACA" ~ "Parolee",
                                                  I130_C14Calc == "TPS" ~ "Parolee",
                                                  I130_C14Calc == "ADVANCE PAROLE" ~ "Parolee",
                                                  I130_C14Calc == "G4" ~ "Employment",
                                                  I130_C14Calc == "K1" ~ "Immigrant",
                                                  I130_C14Calc == "TEMPORARY WORKER" ~ "Employment",
                                                  I130_C14Calc == "TN" ~ "Visitor",
                                                  I130_C14Calc == "B2 VISITOR-OVERSTAY" ~ "Visitor",
                                                  I130_C14Calc == "H2B" ~ "Employment",
                                                  I130_C14Calc == "H4" ~ "Employment",
                                                  I130_C14Calc == "K1 OVERSTAY" ~ "Immigrant",
                                                  I130_C14Calc == "L" ~ "Employment",
                                                  I130_C14Calc == "L2" ~ "Employment",
                                                  I130_C14Calc == "O-1" ~ "Employment",
                                                  I130_C14Calc == "P1" ~ "Employment",
                                                  I130_C14Calc == "VISITOR-OVERSTAY" ~ "Visitor",
                                                  I130_C14Calc == "A" ~ "Employment",
                                                  I130_C14Calc == "Advanced parole" ~ "Parolee",
                                                  I130_C14Calc == "B2 VISITOR-OVERSTAY" ~ "Visitor",
                                                  I130_C14Calc == "B2 VISITOR-OVERSTAY" ~ "Visitor",
                                                  I130_C14Calc == "N" ~ "Immigrant",
                                                  I130_C14Calc == "O1" ~ "Employment",
                                                  I130_C14Calc == "P" ~ "Employment",
                                                  I130_C14Calc == "PIP" ~ "Parolee",
                                                  I130_C14Calc == "Paroled" ~ "Parolee",
                                                  I130_C14Calc == "Paroled in Place" ~ "Parolee",
                                                  I130_C14Calc == "R" ~ "Employment",
                                                  I130_C14Calc == "UNK" ~ "NA_value",
                                                  I130_C14Calc == "WT" ~ "Visitor",
                                                  I130_C14Calc == "parolee" ~ "Parolee",
                                                  I130_C14Calc == "Y" ~ "Immigrant",
                                                  TRUE ~ I130_C14Calc))
sample_frame %>% tabyl(ben_entry_status_group) %>% arrange(desc(n))


#################################################


# create ewi dummy variable based on I130_C14Calc
sample_frame %>% tabyl(I130_C14Calc)
sample_frame <- sample_frame %>% mutate(ewi = case_when(is.na(I130_C14Calc) ~ "NA_value", I130_C14Calc == "EWI" ~ "EWI", TRUE ~ "Non-EWI"))
sample_frame %>% tabyl(ewi)


###################################################


# create BEN_EMPLOY_group to bucket BEN_EMPLOY
# no recoding necessary
# NA for sample_control
sample_frame %>% group_by(BEN_EMPLOY) %>% count()
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(BEN_EMPLOY)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(BEN_EMPLOY)


#########################################################


# create PET_EMPLOY_group to bucket BEN_EMPLOY
# no recoding necessary
# NA for sample_control
sample_frame %>% group_by(PET_EMPLOY) %>% count()
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(PET_EMPLOY)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(PET_EMPLOY)


#############################################################


# create pet_income to bucket I864_PART6_2_SPONSOR
# NA for sample_control
sample_frame %>% group_by(I864_PART6_2_SPONSOR) %>% count()
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I864_PART6_2_SPONSOR)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I864_PART6_2_SPONSOR)

str(sample_frame$I864_PART6_2_SPONSOR)
as.numeric(sample_frame$I864_PART6_2_SPONSOR)
# will convert characters to NAs since they cannot convert to numeric
# filter to income < 500k since otherwise there's a > $1 mil outlier that dwarfs scale
# treatment %>% ggplot(., aes(x = as.numeric(I864_PART6_2_SPONSOR))) + geom_histogram() +
#         scale_x_continuous(breaks = seq(0, max(as.numeric(treatment$I864_PART6_2_SPONSOR), na.rm = TRUE), 100000))
sample_frame %>% filter(as.numeric(I864_PART6_2_SPONSOR) < 500000) %>% 
        ggplot(., aes(x = as.numeric(I864_PART6_2_SPONSOR))) + geom_histogram() +
        scale_x_continuous(breaks = seq(0, 400000, 50000))

sample_frame %>% filter(as.numeric(I864_PART6_2_SPONSOR) < 100000) %>% 
        ggplot(., aes(x = as.numeric(I864_PART6_2_SPONSOR))) + geom_histogram() +
        scale_x_continuous(breaks = seq(0, 100000, 10000))

sample_frame %>% filter(I864_PART6_2_SPONSOR %in% c("BLANK", "U", "SA")) %>% group_by(I864_PART6_2_SPONSOR) %>% count()

# clean up 
sample_frame <- sample_frame %>% mutate(pet_income = case_when(
        I864_PART6_2_SPONSOR == "SA" ~ NA_character_,
        I864_PART6_2_SPONSOR == "BLANK" ~ NA_character_,
        I864_PART6_2_SPONSOR == "U" ~ NA_character_,
        TRUE ~ I864_PART6_2_SPONSOR))

# convert to numeric
sample_frame <- sample_frame %>% mutate(pet_income = as.numeric(pet_income))

# inspect pet_income
sample_frame %>% group_by(pet_income) %>% count()
sample_frame %>% filter(is.na(pet_income)) %>% distinct(pet_income, I864_PART6_2_SPONSOR)


############################################################


# create ben_income to bucket BEN_WAGE
# na for sample_control
sample_frame %>% tabyl(BEN_WAGE)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(BEN_WAGE)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(BEN_WAGE)

sample_frame %>% filter(as.numeric(BEN_WAGE) < 500000) %>% 
        ggplot(., aes(x = as.numeric(BEN_WAGE))) + geom_histogram() +
        scale_x_continuous(breaks = seq(0, 400000, 50000))

sample_frame %>% filter(as.numeric(BEN_WAGE) < 100000) %>% 
        ggplot(., aes(x = as.numeric(BEN_WAGE))) + geom_histogram() +
        scale_x_continuous(breaks = seq(0, 100000, 10000))

sample_frame %>% filter(BEN_WAGE %in% c("BLANK", "U", "SA")) %>% group_by(BEN_WAGE) %>% count()

# clean
sample_frame <- sample_frame %>% mutate(ben_income = case_when(BEN_WAGE == "SA" | BEN_WAGE == "Not Reported" | BEN_WAGE == "BLANK" | BEN_WAGE == "Unknown" ~ NA_character_, 
                                                         TRUE ~ BEN_WAGE))

# convert to numeric
sample_frame <- sample_frame %>% mutate(ben_income = as.numeric(ben_income))

# inspect ben_income
sample_frame %>% tabyl(ben_income)
sample_frame %>% filter(is.na(ben_income)) %>% distinct(ben_income, BEN_WAGE)


###########################################################


# create ben_status_last_entry to bucket I485_PART3_A_LSE
# NA for sample_control
sample_frame %>% tabyl(I485_PART3_A_LSE)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I485_PART3_A_LSE)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I485_PART3_A_LSE)

sample_frame <- sample_frame %>% 
        mutate(ben_status_last_entry = case_when(is.na(I485_PART3_A_LSE) ~ NA_character_,
                grepl("DACA", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Parolee",
         grepl("B1", I485_PART3_A_LSE, ignore.case = TRUE) | grepl("B2", I485_PART3_A_LSE, ignore.case = TRUE) |
                 grepl("VISITOR", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Visitor",
         grepl("ADJ", I485_PART3_A_LSE, ignore.case = TRUE) | grepl("AOS", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Adjustment",
         grepl("E2", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Employment",
         grepl("F1", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Student",
         grepl("H2B|H1B", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Employment",
         grepl("J1", I485_PART3_A_LSE, ignore.case = TRUE) | grepl("J2", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Student",
         grepl("TPS|Parole", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Parolee",
         grepl("WT", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Visitor",
         grepl("K1", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Immigrant",
         grepl("L1", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Employment",
         grepl("O1|O-1", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Employment",
         grepl("STUDENT", I485_PART3_A_LSE, ignore.case = TRUE) ~ "Student",
         I485_PART3_A_LSE == "OVERSTAY" | I485_PART3_A_LSE == "OVERSTAYED" | I485_PART3_A_LSE == "VISA OVER STAY" ~ "Undefined Overstay",
         I485_PART3_A_LSE == "TN visa" | I485_PART3_A_LSE == "TN-1" ~ "Visitor",
         str_detect(string = I485_PART3_A_LSE, pattern = "EWI") ~ "EWI",
         str_detect(string = I485_PART3_A_LSE, pattern = "PIP") ~ "Parolee",
         I485_PART3_A_LSE == "VISA WAIVER" ~ "Visitor",
         I485_PART3_A_LSE == "SA" ~ "NA_value", 
         TRUE ~ "Other"))

# inspect ben_status_last_entry
sample_frame %>% tabyl(ben_status_last_entry)
sample_frame %>% filter(ben_status_last_entry == "Other") %>% count(I485_PART3_A_LSE, ben_status_last_entry)


#################################################################


# bucket BEN_COUNTRY_OF_BIRTH by region
sample_frame %>% tabyl(BEN_COUNTRY_OF_BIRTH)

# create placeholder variable
sample_frame$ben_region_of_birth <- "NA_value"


for(i in 1:nrow(sample_frame)) {
        
        print(i)
        
        if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AFARS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AFRIC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANTAR' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BENIN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANGOL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DAHOM' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BOTSW' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BURKI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UPPER' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BURUN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAMER' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAPEV' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CVI'   | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAR'   | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAFRI' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CENTR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CHAD'  | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ALGER' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANGOL'  | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COMOR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DECON' | 
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CONGO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DJIBO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DRC-C' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FSOMA' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'EGYPT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'EQUAT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FERNA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'RIOMU'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ERITRI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ETHIO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GABON' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GAMBI'|  
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GHANA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUINE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UINEA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BISSA' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUIBI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PGUIN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COTED' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IVORY'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KENYA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LESOT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LIBER' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LIBYA' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MADAG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MALAW' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MALI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MAURI'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GAMBI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MAUTA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MAUTI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IFNI' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MOROC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TANGI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MOZAM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NAMIB' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIGER' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIGE'  | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIGEI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIGIA'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'RWAND' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PRINC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAOTO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SENEG'| 
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SEYCH' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SIERR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOMAL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SUDAN'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOSUD' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOMAL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SWAZI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TOGO' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TUNIS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UGAND' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ZAMBI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NRHOD'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ZIMBA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SRHOD' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] ==  'SNA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TANZA' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TANGA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PEMBA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WSAHA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SPSAH' |
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAFRI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ZAIRE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CONKI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ERITR'|
           sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ZANZI') {
                
                sample_frame$ben_region_of_birth[i] <- "Africa"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ADEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AFGHA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ASIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BAHRA' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KAMPU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'INDOC' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BANGL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BHUTA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BRUNE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BURMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAMBO' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CHINA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'HONGK' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MACAO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MACAU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MAINL'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'INDIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANDAM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GOA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JUNAG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NICOB'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SIKKI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'INDON' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IRBAR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JAVA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PTIMO'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TIMOR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WNGUI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IRAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IRAQ' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ISRAE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ISRAJ' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JAPAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BONIN'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'HABOM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'OKINA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'RYUKY'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SHIKO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VOLCA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JORDA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARABJ' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NPALE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PALES'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KAZAK' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KYRGY' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KUWAI' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LAOS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UAE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UAL'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AFGHA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARABI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARABJ'| 
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARMEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AZERB' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CYPRU'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ETMOR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GEORG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KOREA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LEBAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MALAY' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MALDI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MYANM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MONGO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MUSCA'| 
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NEPAL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NKORE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NVIET' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'OMAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PAKIS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PHILI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'QATAR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAUDI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SKORE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SRILA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SVIET' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SYRIA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TAIWA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TAJIK' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'THAIL'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TIMOR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AZERB' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CYPRU'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SINGA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TURKE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TURKM'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UZBEK' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VIETN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WNGUI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'YEMEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ADEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NYEME'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SANAA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CEYLO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TIBET'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FORMO') {
                
                sample_frame$ben_region_of_birth[i] <- "Asia"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COSTA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'EL SA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ELSAL' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUATE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'HONDU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PANAM'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SALVA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NICAR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BELIZ'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MEXIC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARGEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BOLIV'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BRAZI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CHILE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COLOM'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ECUAD' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FALKL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FGUIA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FRGUI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUYAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NEGUI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PERU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SURIN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'URUGU'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VENEZ' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PARAG' ) {
                
                sample_frame$ben_region_of_birth[i] <- "Central and South America"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CANAD' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GREEN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BVI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANGUI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ARUBA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BAHAM'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANTIL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BARBA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BARBU'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BERMU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BVIRG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAICO'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CAYMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CUBA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DOMIN'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'INICA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GRENA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUADE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'HAITI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JAMAI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MARTI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MIQUE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MONTS'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NANTI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NEVIS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOMBR'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STKIT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STLUC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STPIE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STVIN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TOBAG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TRINI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TURKS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAINT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BRITI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANTIG' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAINT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BRITI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FSTMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STMAR') {
                
                sample_frame$ben_region_of_birth[i] <- "North America"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FSM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'RALIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CHRIS' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COCOS' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COOK'  | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'COOKI'| 
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FIJI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FPOLY' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'POLYN'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FUTUN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KIRIB' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MARSH'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MICRO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NAURU' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NEWCA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NEWZE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIUE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NORFO' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PACIF' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PALAU'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PAPUA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PITCA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOLOM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TOKEL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TONGA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'TUVAL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UISLA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VANUA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WALLI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AUSTR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MAYOT'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'REUNI') {
                
                sample_frame$ben_region_of_birth[i] <- "Oceania"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UK' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ALBAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SCOTL'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ANDOR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BELAR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BELGI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BOSNI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BULGA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'BYELA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CHANN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CROAT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CZECH'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'CZREP' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'DENMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ESTON'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FAROE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FINLA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FRANC'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GERMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GIBRA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GREEC'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUERN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'HUNGA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ICELA'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'IRELA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'JERSE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KOSOV'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ISLE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ITALY' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LATVI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LIECH' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LITHU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LUXEM'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MACED' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MALTA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MOLDO'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MONAC' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MONTE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NETHE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NIREL' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NORWA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'POLAN'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PORTU' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ROMAN' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'RUSSI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAKHA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SANMA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SERBI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SLOVA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SLOVE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SPAIN'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SRBIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STRIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SWEDE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SVALB' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SWITZ' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VATIC'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UKRAI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'USSR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'YUGOS'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SNA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOVIE' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'FRENC'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'KV' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GDR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WGERM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WALES') {
                
                sample_frame$ben_region_of_birth[i] <- "Europe"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PR' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'PUERT' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'ASAMO'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'MARIA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'AMERI' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'NMARI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAIPA' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'USVI-' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'US'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'GUAM' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SAMOA' |  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'VIRGI'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WSAMO') {
                
                sample_frame$ben_region_of_birth[i] <- "US Protectorate"
                
        } else if(sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'UNKNO' |
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'SOUTH' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'LAO P' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'WESTE'|
                  sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'EAST' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == '' | sample_frame$BEN_COUNTRY_OF_BIRTH[i] == 'STATE') {
                
                sample_frame$ben_region_of_birth[i] <- "Unknown"
                
        } else {
                sample_frame$ben_region_of_birth[i] <- "NA_value"
        }
}

sample_frame %>% tabyl(ben_region_of_birth)
sample_frame %>% filter(ben_region_of_birth == "Africa") %>% tabyl(BEN_COUNTRY_OF_BIRTH)


###########################################################


# bucket ben_country_of_birth to top ??
sample_frame %>% group_by(BEN_COUNTRY_OF_BIRTH) %>% count() %>% arrange(desc(n)) %>% mutate(quintile = ntile(x = n, n = 5)) %>% head()
sample_frame %>% group_by(BEN_COUNTRY_OF_BIRTH, treatment_flag) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% 
        mutate(pct_rank = percent_rank(n), pct = n / sum(n), cum_pct = cumsum(pct)) %>% head(20)

# leave countries with at least 15 obs, bucket the rest as "Other" - 15 is arbitrary number Reid set as threshold for unreliable estimate flagging, so good as any

# get list of countries w > 15 obs to keep
keep_countries_list <- sample_frame %>% group_by(BEN_COUNTRY_OF_BIRTH, treatment_flag) %>% count() %>% arrange(desc(n)) %>% ungroup() %>%
        filter(n >= 15, treatment_flag == 1) %>% pull(BEN_COUNTRY_OF_BIRTH)

sample_frame <- sample_frame %>% mutate(ben_country_of_birth_grouped = case_when(BEN_COUNTRY_OF_BIRTH %in% keep_countries_list ~ BEN_COUNTRY_OF_BIRTH,
                                                                                TRUE ~ "Other"))
sample_frame %>% count(ben_country_of_birth_grouped)
sample_frame %>% filter(treatment_flag == 1) %>% count(ben_country_of_birth_grouped)
sample_frame %>% filter(sample_control_flag == 1) %>% count(ben_country_of_birth_grouped)


##########################################################


# clean several variables to replace NA values with "NA"

# clean I130_C16a
sample_frame %>% count(I130_C16a)
sample_frame %>% filter(treatment_flag == 1) %>% count(I130_C16a)
sample_frame %>% filter(sample_control_flag == 1) %>% count(I130_C16a)

sample_frame <- sample_frame %>% mutate(I130_C16a = case_when(is.na(I130_C16a) ~ "NA_value", I130_C16a == "y" ~ "Y", TRUE ~ I130_C16a))

sample_frame %>% count(I130_C16a)
sample_frame %>% filter(treatment_flag == 1) %>% count(I130_C16a)
sample_frame %>% filter(sample_control_flag == 1) %>% count(I130_C16a)


##############################################################


# clean I130_C16c (type_ben_immig_proceed) 
sample_frame %>% tabyl(I130_C16c)
sample_frame %>% filter(treatment_flag == 1) %>% tabyl(I130_C16c)
sample_frame %>% filter(sample_control_flag == 1) %>% tabyl(I130_C16c)

sample_frame %>% distinct(I130_C16a, I130_C16c) 
sample_frame <- sample_frame %>% 
        mutate(type_ben_immig_proceed_group = case_when(is.na(I130_C16c) ~ "NA_value",
                str_detect(string = I130_C16c, pattern = regex("Blank|BLANK", ignore_case = TRUE)) ~ "Blank",
                I130_C16c == "999" ~ "NA_value",
                str_detect(string = I130_C16c, pattern = regex("E/D|exlus", ignore_case = TRUE)) ~ "Excl_Deport",
                str_detect(string = I130_C16c, pattern = regex("Judic|JP", ignore_case = TRUE)) ~ "Judicial",
                I130_C16c == "N" ~ "NA_value",
                str_detect(string = I130_C16c, pattern = regex("Rem|remov", ignore_case = TRUE)) ~ "Removal",
                I130_C16c == "resis" ~ "Rescission",
                I130_C16c == "Y" ~ "NA_value",
                TRUE ~ I130_C16c))

sample_frame %>% count(I130_C16a, type_ben_immig_proceed_group)
sample_frame %>% group_by(type_ben_immig_proceed_group) %>% count()


#####################################################################


sample_frame %>% count(BEN_LAST_VISA_APP)
sample_frame <- sample_frame %>% mutate(BEN_LAST_VISA_APP = case_when(is.na(BEN_LAST_VISA_APP) ~ "NA_value",
                                        str_detect(string = BEN_LAST_VISA_APP, pattern = regex("B1|B2")) ~ "Visitor",
                                        str_detect(string = BEN_LAST_VISA_APP, pattern = regex("H1B|H2B|H4|G4|E2|L1|L2|P1")) ~ "Employment",
                                        str_detect(string = BEN_LAST_VISA_APP, pattern = regex("J1|J2|F1")) ~ "Student",
                                        str_detect(string = BEN_LAST_VISA_APP, pattern = regex("K1")) ~ "Immigrant",
                                        TRUE ~ "Other"))
sample_frame %>% count(BEN_LAST_VISA_APP)

                                        
######################################################################


# clean VISA_APPROVE
sample_frame %>% count(VISA_APPROVE)
sample_frame <- sample_frame %>% mutate(VISA_APPROVE = case_when(is.na(VISA_APPROVE) ~ "NA_value", VISA_APPROVE == "Yes" ~ "YES", TRUE ~ VISA_APPROVE))
sample_frame %>% count(VISA_APPROVE) 


###################################################################


# clean Class
# don't use Class because it doesn't have comparable variable for control
# also it has issues in that is only takes 2 levels of EWI or Visa, but neglects parolee, etc
# better to use EWI vs non-EWI, but we'll use this from I130_C14Calc variable instead
sample_frame %>% group_by(Class) %>% count()
# sample_frame <- sample_frame %>% mutate(Class = case_when(is.na(Class) ~ "NA_value", Class == "Visa" ~ "Non-EWI", TRUE ~ Class))
# sample_frame %>% group_by(Class) %>% count()
# sample_frame %>% tabyl(Class, Case_Outcome) %>% adorn_percentages()


################################################################


# clean BEN_STATE
sample_frame %>% group_by(BEN_STATE) %>% count() %>% data.frame()
sample_frame <- sample_frame %>% mutate(BEN_STATE = case_when(is.na(BEN_STATE) ~ "NA_value", TRUE ~ BEN_STATE))
sample_frame %>% group_by(BEN_STATE) %>% count()
sample_frame %>% filter(is.na(BEN_STATE)) %>% count()


################################################################


# clean I130_C21_Calc (cohabitation) 
sample_frame %>% group_by(I130_C21_Calc) %>% count()
sample_frame <- sample_frame %>% mutate(I130_C21_Calc = case_when(is.na(I130_C21_Calc) ~ "NA_value", 
                                                                  I130_C21_Calc == "y" ~ "Y", 
                                                                  I130_C21_Calc == "YY" ~ "Y", 
                                                                  TRUE ~ I130_C21_Calc))
sample_frame %>% group_by(I130_C21_Calc) %>% count()


################################################################


# clean ageben
str(sample_frame$ageben)
sample_frame %>% distinct(ageben) 
sample_frame %>% filter(is.na(ageben))
sample_frame$ageben <- as.numeric(sample_frame$ageben)


##################################################################


# clean agepet
str(sample_frame$agepet)
sample_frame %>% filter(is.na(agepet))
sample_frame$agepet <- as.numeric(sample_frame$agepet)
sample_frame %>% count(agepet) %>% data.frame()

##################################################################


# clean I130_C11a_Calc (ben_prior_marriage)
sample_frame %>% group_by(I130_C11a_Calc) %>% count()
sample_frame <- sample_frame %>% mutate(I130_C11a_Calc = case_when(is.na(I130_C11a_Calc) ~ "NA_value", I130_C11a_Calc == "y" ~ "Y", TRUE ~ I130_C11a_Calc))
sample_frame %>% group_by(I130_C11a_Calc) %>% count()


#################################################################


# clean Suf_Evidence to convert NA to character
sample_frame %>% group_by(Suf_Evidence) %>% count()


##############################################


# clean I130_B13 (how_pet_got_citz)
sample_frame %>% count(I130_B13)
sample_frame <- sample_frame %>% mutate(I130_B13 = case_when(is.na(I130_B13) ~ "NA_value", I130_B13 == "LPR" ~ "N", 
                                                             I130_B13 == "still" ~ "NA_value", I130_B13 == "M" ~ "NA_value",
                                                             TRUE ~ I130_B13))
sample_frame %>% count(I130_B13)


##############################################


# clean FILED_G28
sample_frame %>% count(FILED_G28)
sample_frame <- sample_frame %>% mutate(FILED_G28 = case_when(is.na(FILED_G28) ~ "NA_value", FILED_G28 == "y" ~ "Y", TRUE ~ FILED_G28))
sample_frame %>% count(FILED_G28)


#############################################


# check I130_B14B
sample_frame %>% count(I130_B14B)
sample_frame <- sample_frame %>% mutate(I130_B14B = case_when(is.na(I130_B14B) ~ "NA_value", I130_B14B == "NO" ~ "N", 
                                                              I130_B14B == "No" ~ "N", I130_B14B == "y" ~ "Y",
                                                             TRUE ~ I130_B14B))
sample_frame %>% count(I130_B14B)


####################################################


# check I130_D2
sample_frame %>% count(I130_D2)
sample_frame <- sample_frame %>% mutate(I130_D2 = case_when(is.na(I130_D2) ~ "NA_value", I130_D2 == "n" ~ "N", TRUE ~ I130_D2))
sample_frame %>% count(I130_D2)


####################################################


# check I130_B11a_Calc
sample_frame %>% count(I130_B11a_Calc)
sample_frame <- sample_frame %>% mutate(I130_B11a_Calc = case_when(is.na(I130_B11a_Calc) ~ "NA_value", I130_B11a_Calc == "y" ~ "Y", TRUE ~ I130_B11a_Calc))
sample_frame %>% count(I130_B11a_Calc)


#######################################################


# check I130_B11b_Calc
sample_frame %>% count(I130_B11b_Calc)


#######################################################


# check I130_C11b_Calc
sample_frame %>% count(I130_C11b_Calc)


#######################################################


# check I130_C17_Calc
sample_frame %>% count(I130_C17_Calc)
sample_frame <- sample_frame %>% mutate(I130_C17_Calc = case_when(is.na(I130_C17_Calc) ~ "NA_value", I130_C17_Calc == "y" ~ "Y", TRUE ~ I130_C17_Calc))
sample_frame %>% count(I130_C17_Calc)


##########################################################


# check BEN_SEX
sample_frame %>% count(BEN_SEX)
sample_frame <- sample_frame %>% mutate(BEN_SEX = case_when(is.na(BEN_SEX) ~ "NA_value", BEN_SEX == "Y" ~ "NA_value", TRUE ~ BEN_SEX))
sample_frame %>% count(BEN_SEX)


##########################################################


# check PET_SEX
sample_frame %>% count(PET_SEX)
sample_frame <- sample_frame %>% mutate(PET_SEX = case_when(is.na(PET_SEX) ~ "NA_value", TRUE ~ PET_SEX))
sample_frame %>% count(PET_SEX)


####################################################
#########################################################
###########################################################


# clean names of I-130 variables
sample_frame <- sample_frame %>% mutate(how_pet_got_citz = I130_B13, pet_get_lpr_via_marriage = I130_B14B, 
                                  ben_entry_status = I130_C14Calc, ben_immig_proceed = I130_C16a, 
                                  type_ben_immig_proceed = I130_C16c, pet_filed_prior_petition = I130_D2,
                                  pet_prior_marriage = I130_B11a_Calc, count_pet_marriages = I130_B11b_Calc,
                                  ben_prior_marriage = I130_C11a_Calc, count_ben_marriages = I130_C11b_Calc,
                                  shared_children = I130_C17_Calc, cohabitation = I130_C21_Calc)

glimpse(sample_frame)

# check pre_interview variables
# look for categorical variables with too many levels that may need to be bucketed, or converted to dummies, or both to avoid overfitting
sample_frame %>% count(SOF_Finding_SP) %>% data.frame()
sample_frame %>% count(Case_Outcome) %>% data.frame()
sample_frame %>% count(Suf_Evidence) %>% data.frame()
sample_frame %>% count(BEN_COUNTRY_OF_BIRTH) %>% data.frame()
sample_frame %>% count(ben_country_of_birth_grouped) %>% data.frame()
sample_frame %>% count(PET_STATE) %>% data.frame()
sample_frame %>% count(BEN_STATE) %>% data.frame()
sample_frame %>% count(BEN_SEX) %>% data.frame()
sample_frame %>% count(PET_SEX) %>% data.frame()
sample_frame %>% count(ewi) %>% data.frame()
sample_frame %>% count(Concurrent) %>% data.frame()
sample_frame %>% count(BEN_LAST_VISA_APP) %>% data.frame()
sample_frame %>% count(FILED_G28) %>% data.frame()
sample_frame %>% count(ben_prior_marriage) %>% data.frame()
sample_frame %>% count(pet_prior_marriage) %>% data.frame()
sample_frame %>% count(VISA_APPROVE) %>% data.frame()
sample_frame %>% count(ageben) %>% data.frame()
sample_frame %>% count(agepet) %>% data.frame()
sample_frame %>% count(ben_pet_age_diff) %>% data.frame()
sample_frame %>% count(how_pet_got_citz) %>% data.frame()
sample_frame %>% count(pet_get_lpr_via_marriage) %>% data.frame()
sample_frame %>% count(ben_entry_status_group) %>% data.frame()
sample_frame %>% count(ben_immig_proceed) %>% data.frame()
sample_frame %>% count(time_since_ben_immig_proceed) %>% data.frame()
sample_frame %>% count(time_since_ben_divorce) %>% data.frame()
sample_frame %>% count(time_since_present_marriage) %>% data.frame()


########################################################


# because control used outcome variable, but treatment uses Case_Outcome, overwrite Case_OutcomeDenied with values from outcome
sample_frame %>% filter(sample_control_flag == 1) %>% count(outcome)
sample_frame %>% filter(sample_control_flag == 1) %>% count(Case_Outcome)

sample_frame %>% filter(treatment_flag == 1) %>% count(outcome)
sample_frame %>% filter(treatment_flag == 1) %>% count(Case_Outcome)

sample_frame <- sample_frame %>% mutate(Case_Outcome = case_when(sample_control_flag == 1 ~ outcome, TRUE ~ Case_Outcome)) %>%
        mutate(Case_Outcome = case_when(Case_Outcome == "A" ~ "Approved", Case_Outcome == "D" ~ "Denied", Case_Outcome == "R" ~ NA_character_, TRUE ~ Case_Outcome))

# check
sample_frame %>% filter(sample_control_flag == 1) %>% count(outcome)
sample_frame %>% filter(sample_control_flag == 1) %>% count(Case_Outcome)

sample_frame %>% filter(treatment_flag == 1) %>% count(Case_Outcome)


#################################################################


# save sample_frame
# write sample_frame_v2 to csv
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment_control_v2", "_", current_date, ".csv")
filename
write_csv(sample_frame, path = filename)



#################################################################


# select variables for pre-interview model
pre_interview_model_variables <- c("receipt_number", "Case_Outcome", "SOF_Finding_SP", "Suf_Evidence", "BEN_COUNTRY_OF_BIRTH", "ben_country_of_birth_grouped", "PET_STATE", "BEN_STATE", 
                                   "BEN_SEX", "PET_SEX", "ewi", "Concurrent",
                                   "BEN_LAST_VISA_APP", "FILED_G28", "ben_prior_marriage", "pet_prior_marriage",
                                   "VISA_APPROVE", "ageben", "agepet", "ben_pet_age_diff", "how_pet_got_citz",
                                   "pet_get_lpr_via_marriage", "ben_entry_status_group", "ben_immig_proceed",
                                   "time_since_ben_immig_proceed", "type_ben_immig_proceed_group", "pet_filed_prior_petition",
                                   "count_pet_marriages", "count_ben_marriages",
                                   "time_since_pet_divorce", "time_since_ben_divorce", "time_since_present_marriage",
                                   "shared_children", "cohabitation", "time_since_cohabitation", "BEN_EMPLOY",
                                   "PET_EMPLOY", "time_since_ben_last_arrival", "ben_status_last_entry",
                                   "ben_income", "pet_income", "ben_region_of_birth")

# create separate treatment and control 
treatment <- sample_frame %>% filter(treatment_flag == 1)
dim(treatment)

control <- sample_frame %>% filter(sample_control_flag == 1)
dim(control)

# subset treatment and control to just pre_interview_variables
treatment_pre_interview <- treatment %>% select(pre_interview_model_variables)
dim(treatment_pre_interview)

control_pre_interview <- control %>% select(pre_interview_model_variables)
dim(control_pre_interview)

# review for missing values
sum_na_values <- function(column) {
        sum(is.na(column))
}

treatment_pre_interview %>% map_dfr(.x = ., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count")

control_pre_interview %>% map_dfr(.x = ., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count")

# write clean treatment and control to csv
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment_pre_interview_v2", "_", current_date, ".csv")
filename
write_csv(treatment_pre_interview, path = filename)

current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("control_pre_interview_v2", "_", current_date, ".csv")
filename
write_csv(control_pre_interview, path = filename)


####################################################################


# compare categorical levels to ensure control does not have any unique levels not found in treatment

# create get_unique_levels function
get_unique_levels <- function(.x) {
        unique_values <- unique(.x)
        unique_values_padded <- c(unique_values, rep(NA, times = nrow(control_pre_interview) - length(unique_values)))
        unique_values_padded
}

# apply get_unique_levels function
control_pre_interview_unique_levels <- control_pre_interview %>% map(.x = ., .f = get_unique_levels) %>% bind_cols()
treatment_pre_interview_unique_levels <- treatment_pre_interview %>% map(.x = ., .f = get_unique_levels) %>% bind_cols()

# keep only character or factor variables
control_pre_interview_unique_levels <- control_pre_interview_unique_levels[ , sapply(control_pre_interview_unique_levels, class) %in%
                                                                                    c("character", "factor")]
treatment_pre_interview_unique_levels <- treatment_pre_interview_unique_levels[ , sapply(treatment_pre_interview_unique_levels, class) %in%
                                                                                        c("character", "factor")]

# drop receipt_numbers
control_pre_interview_unique_levels <- control_pre_interview_unique_levels %>% select(-receipt_number)
treatment_pre_interview_unique_levels <- treatment_pre_interview_unique_levels %>% select(-receipt_number)

# find unique levels in control_pre_interview but not in treatment
get_unique_control_levels <- function(.x, .y) {
        x_tbl <- tibble(.x)
        y_tbl <- tibble(.y)
        unique_control_levels <- x_tbl %>% anti_join(x = ., y = y_tbl, by = c(".x" = ".y"))
        unique_control_levels
}

unique_control_levels <- map2(.x = control_pre_interview_unique_levels, .y = treatment_pre_interview_unique_levels, 
                              .f = get_unique_control_levels)
unique_control_levels <- Filter(f = function(x) { nrow(x) != 0 }, x = unique_control_levels)


# create replacement variables for control_pre_interview with unique levels set to NA
replace_unique_control_levels_with_na <- function(.x, .y) {
        var_name_sym <- sym(.y)
        var_tbl <- .x
        control_pre_interview %>% mutate(!!var_name_sym := case_when(!!var_name_sym %in% var_tbl$.x ~ NA_character_, 
                                        TRUE ~ !!var_name_sym)) %>% select(!!var_name_sym)
}

unique_control_levels_names <- names(unique_control_levels)

replacement_control_levels_tbl <- map2(.x = unique_control_levels, .y = unique_control_levels_names, 
                                   .f = replace_unique_control_levels_with_na) %>% bind_cols()

# replace control_pre_interview variables with replacement_control_levels_tbl
unique_control_levels_names_syms <- syms(unique_control_levels_names)
control_pre_interview2 <- control_pre_interview %>% select(-c(!!!unique_control_levels_names_syms)) %>% 
        bind_cols(., replacement_control_levels_tbl)

# inspect
control_pre_interview2 %>% anti_join(x = control_pre_interview, y = ., by = c("PET_STATE" = "PET_STATE")) %>% distinct(PET_STATE)
control_pre_interview %>% distinct(PET_STATE)
control_pre_interview2 %>% distinct(PET_STATE)
unique_control_levels

control_pre_interview2 %>% 
        anti_join(x = control_pre_interview, y = ., by = c("type_ben_immig_proceed_group" = "type_ben_immig_proceed_group")) %>% 
        distinct(type_ben_immig_proceed_group)
control_pre_interview %>% distinct(type_ben_immig_proceed_group)
control_pre_interview2 %>% distinct(type_ben_immig_proceed_group)
unique_control_levels

# overwrite control_pre_interview
control_pre_interview <- control_pre_interview2


############################################################


# create dummy variables for treatment

# detach receipt numbers before converting to dummies, then reattach afterward
dim(treatment_pre_interview)
treatment_receipt_numbers <- treatment_pre_interview %>% pull(receipt_number)
length(treatment_receipt_numbers)
treatment_pre_interview <- treatment_pre_interview %>% select(-receipt_number)
dim(treatment_pre_interview)

# create dummies
treatment_pre_interview_dummies_model <- dummyVars(~ ., data = treatment_pre_interview)
treatment_pre_interview_dummies <- data.frame(predict(object = treatment_pre_interview_dummies_model, newdata = treatment_pre_interview))
glimpse(treatment_pre_interview_dummies)
dim(treatment_pre_interview_dummies)

# reattach receipt_numbers
treatment_pre_interview_dummies <- treatment_pre_interview_dummies %>% mutate(receipt_number = treatment_receipt_numbers)
dim(treatment_pre_interview_dummies)
treatment_pre_interview_dummies %>% select(receipt_number) %>% head()

# write sample_frame_pre_interview_dummies to csv
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment_pre_interview_dummies_v2", "_", current_date, ".csv")
filename
write_csv(treatment_pre_interview_dummies, path = filename)


############################################################


# create dummy variables for control

# detach receipt numbers before converting to dummies, then reattach afterward
dim(control_pre_interview)
control_receipt_numbers <- control_pre_interview %>% pull(receipt_number)
length(control_receipt_numbers)
control_pre_interview <- control_pre_interview %>% select(-receipt_number)
dim(control_pre_interview)

# drop variables where all obs are NA, since dummyVars can't handle them, then reattach afterward
sum_na_values <- function(column) {
        sum(is.na(column))
}

# get count of na values
control_pre_interview_na_counts <- control_pre_interview %>% map_dfr(.x = ., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count")
control_pre_interview_na_counts

# find variables with all NA values
control_pre_interview_all_na_vars <- control_pre_interview_na_counts %>% filter(na_count == nrow(control_pre_interview)) %>% pull(variable)
control_pre_interview_all_na_vars_syms <- syms(control_pre_interview_all_na_vars)

# drop variables with all NA values
dim(control_pre_interview)
control_pre_interview <- control_pre_interview %>% select(-c(!!!control_pre_interview_all_na_vars_syms))
dim(control_pre_interview)

# drop variables with only NA_value (or any with just one level) since dummyVars can't handle them, then reattach afterward
count_var_levels <- function(column){
        column %>% n_distinct()
}

# apply count_var_levels function
control_pre_interview_NA_value_vars <- control_pre_interview %>% map_dfr(.x = ., .f = count_var_levels) %>% 
        gather(key = variable, value = count_distinct) %>% filter(count_distinct == 1) %>% pull(variable)

control_pre_interview_NA_value_vars_syms <- syms(control_pre_interview_NA_value_vars)
control_pre_interview %>% select(!!!control_pre_interview_NA_value_vars_syms)

dim(control_pre_interview)
control_pre_interview <- control_pre_interview %>% select(-c(!!!control_pre_interview_NA_value_vars_syms))
dim(control_pre_interview)

# create dummies
control_pre_interview_dummies_model <- dummyVars(~ ., data = control_pre_interview)
control_pre_interview_dummies <- data.frame(predict(object = control_pre_interview_dummies_model, newdata = control_pre_interview))
glimpse(control_pre_interview_dummies)
dim(control_pre_interview_dummies)

# reattach receipt_numbers
dim(control_pre_interview_dummies)
control_pre_interview_dummies <- control_pre_interview_dummies %>% mutate(receipt_number = control_receipt_numbers)
dim(control_pre_interview_dummies)
control_pre_interview_dummies %>% select(receipt_number) %>% head()

# reattach all_na_vars
control_pre_interview_all_na_vars_tbl <- map(.x = control_pre_interview_all_na_vars, .f = ~ tibble(!!.x := rep(NA, times = nrow(control_pre_interview)))) %>%
        bind_cols()
glimpse(control_pre_interview_all_na_vars_tbl)
dim(control_pre_interview_dummies)
control_pre_interview_dummies <- bind_cols(control_pre_interview_dummies, control_pre_interview_all_na_vars_tbl)
dim(control_pre_interview_dummies)

# reattach NA_value_vars
control_pre_interview_NA_value_vars_tbl <- map(.x = control_pre_interview_NA_value_vars, .f = ~ tibble(!!.x := rep("NA_value", times = nrow(control_pre_interview)))) %>%
        bind_cols()
glimpse(control_pre_interview_NA_value_vars_tbl)
dim(control_pre_interview_dummies)
control_pre_interview_dummies <- bind_cols(control_pre_interview_dummies, control_pre_interview_NA_value_vars_tbl)
dim(control_pre_interview_dummies)


########################################################


# add placeholder variables for any dummy variable in treatment but not in control, so that treatment and control have same # variables
dim(control_pre_interview_dummies)
dim(treatment_pre_interview_dummies)

# get names in treatment/control_dummies
control_pre_interview_dummies_names_tbl <- tibble(names = names(control_pre_interview_dummies))
treatment_pre_interview_dummies_names_tbl <- tibble(names = names(treatment_pre_interview_dummies))

# check names in treatment_dummies only
vars_in_treatment_dummies_only <- anti_join(x = treatment_pre_interview_dummies_names_tbl, 
                                            y = control_pre_interview_dummies_names_tbl, by = "names")
vars_in_treatment_dummies_only %>% data.frame()

# check names in control_dummies only
vars_in_control_dummies_only <- anti_join(x = control_pre_interview_dummies_names_tbl, 
                                            y = treatment_pre_interview_dummies_names_tbl, by = "names")
vars_in_control_dummies_only

# # create syms of vars in only control/treatment for use in functions below
vars_in_control_dummies_only_syms <- syms(vars_in_control_dummies_only$names)
vars_in_treatment_dummies_only_syms <- syms(vars_in_treatment_dummies_only$names)

# build function to loop through vars_in_treatment_dummies_only and create tibble with NA value to control_pre_interview_dummies
add_na_placeholder_variables_for_vars_in_treatment_dummies_only <- function(.x) {
        var_name_sym <- sym(.x)
        tibble(!!var_name_sym := rep(NA, times = control_pre_interview_dummies_nrow))
}

control_pre_interview_dummies_nrow <- nrow(control_pre_interview_dummies)

vars_in_treatment_dummies_only_for_adding_to_control <- map_dfc(.x = vars_in_treatment_dummies_only$names, 
                                                                .f = add_na_placeholder_variables_for_vars_in_treatment_dummies_only)

# drop vars_in_control_dummies_only and replace with vars_in_treatment_dummies_only w/ NA values
control_pre_interview_dummies <- control_pre_interview_dummies %>% select(-c(!!!vars_in_control_dummies_only_syms)) %>%
        bind_cols(., vars_in_treatment_dummies_only_for_adding_to_control)

glimpse(control_pre_interview_dummies)
dim(control_pre_interview_dummies)
dim(treatment_pre_interview_dummies)

# check to confirm same variables in treatment/control dummies
# get names in treatment/control_dummies
control_pre_interview_dummies_names_tbl <- tibble(names = names(control_pre_interview_dummies))
treatment_pre_interview_dummies_names_tbl <- tibble(names = names(treatment_pre_interview_dummies))

# check names in treatment_dummies only
vars_in_treatment_dummies_only <- anti_join(x = treatment_pre_interview_dummies_names_tbl, 
                                            y = control_pre_interview_dummies_names_tbl, by = "names")
vars_in_treatment_dummies_only %>% data.frame()

# check names in control_dummies only
vars_in_control_dummies_only <- anti_join(x = control_pre_interview_dummies_names_tbl, 
                                          y = treatment_pre_interview_dummies_names_tbl, by = "names")
vars_in_control_dummies_only


##########################################################


# ensure the class of all variables in the control is the same as the treatment

# build function to loop through treatment and control to get variable classes
get_variable_classes <- function(.x) {
        class(.x)
}

# get variable classes for treatment and control
treatment_pre_interview_dummies_var_classes <- map_dfc(.x = treatment_pre_interview_dummies, .f = get_variable_classes)
treatment_pre_interview_dummies_var_classes %>% gather(key = "variable", value = "value", .) %>% distinct(value)

control_pre_interview_dummies_var_classes <- map_dfc(.x = control_pre_interview_dummies, .f = get_variable_classes)
control_pre_interview_dummies_var_classes %>% gather(key = "variable", value = "value", .) %>% distinct(value)

# sort treatment/control_var_classes in same order
control_pre_interview_dummies_var_classes <- control_pre_interview_dummies_var_classes %>% select(names(treatment_pre_interview_dummies_var_classes))
unique(names(treatment_pre_interview_dummies_var_classes) == names(control_pre_interview_dummies_var_classes))

# build function to compare treatment and control variable classes
check_treatment_control_same_variable_classes <- function(.x, .y) {
        .x == .y
}

# compare treatment/control variable classes
treatment_control_same_variable_classes <- map2_dfr(.x = treatment_pre_interview_dummies_var_classes, .y = control_pre_interview_dummies_var_classes, 
                                          .f = check_treatment_control_same_variable_classes) %>% gather(key = variable, value = value, .)
                                       
treatment_control_same_variable_classes %>% filter(value == "FALSE")
treatment_control_same_variable_classes %>% filter(value == "TRUE")

# build function to overwrite control class to match treatment class
overwrite_control_class_w_treatment_class <- function(.x) {
        var_name_sym <- sym(.x)
        treatment_var_class <- treatment_pre_interview_dummies_var_classes %>% pull(!!var_name_sym)

        # if(treatment_var_class == "character"){
        #         return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.character(!!var_name_sym)))
        # }
        # if(treatment_var_class == "factor"){
        #         return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.factor(!!var_name_sym)))
        # }
        # if(treatment_var_class == "numeric"){
        #         return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.numeric(!!var_name_sym)))
        # }
        # if(treatment_var_class == "integer"){
        #         return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.integer(!!var_name_sym)))
        # }
        
        if(treatment_var_class == "character"){
                return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.character(!!var_name_sym)))
        } else if(treatment_var_class == "factor"){
                return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.factor(!!var_name_sym)))
        } else if(treatment_var_class == "numeric"){
                return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.numeric(!!var_name_sym)))
        } else if(treatment_var_class == "integer"){
                return(control_pre_interview_dummies %>% select(!!var_name_sym) %>% mutate(!!var_name_sym := as.integer(!!var_name_sym)))
        } else { return(control_pre_interview_dummies %>% select(!!var_name_sym)) }
}

control_pre_interview_dummies %>% pull(time_since_ben_last_arrival) %>% class()
treatment_pre_interview_dummies %>% pull(time_since_ben_last_arrival) %>% class()

# overwrite control class to match treatment class
control_pre_interview_dummies2 <- map_dfc(.x = names(treatment_pre_interview_dummies_var_classes), .f = overwrite_control_class_w_treatment_class)

control_pre_interview_dummies2 %>% pull(time_since_ben_last_arrival) %>% class()

# check to confirm same classes in treatment/control now
# get variable classes for treatment and control
treatment_pre_interview_dummies_var_classes <- map_dfc(.x = treatment_pre_interview_dummies, .f = get_variable_classes)
treatment_pre_interview_dummies_var_classes %>% gather(key = "variable", value = "value", .) %>% distinct(value)

control_pre_interview_dummies_var_classes <- map_dfc(.x = control_pre_interview_dummies2, .f = get_variable_classes)
control_pre_interview_dummies_var_classes %>% gather(key = "variable", value = "value", .) %>% distinct(value)

# compare treatment/control variable classes
treatment_control_same_variable_classes <- map2_dfr(.x = treatment_pre_interview_dummies_var_classes, .y = control_pre_interview_dummies_var_classes, 
                                                    .f = check_treatment_control_same_variable_classes) %>% gather(key = variable, value = value, .)

treatment_control_same_variable_classes %>% filter(value == "FALSE")
treatment_control_same_variable_classes %>% filter(value == "TRUE")


# overwrite control_pre_interview_dummies
control_pre_interview_dummies <- control_pre_interview_dummies2


########################################################


# write control_pre_interview_dummies to csv
glimpse(control_pre_interview_dummies)

current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("control_pre_interview_dummies_v2", "_", current_date, ".csv")
filename
write_csv(control_pre_interview_dummies, path = filename)










