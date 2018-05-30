library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(scales)
library(readxl)
library(purrr)
library(ggplot2)



# turn off scientific notation
options(scipen=999)

# setwd
setwd("H:/RED/DACA")
list.files()
list.files("Data")

# read in functions
source("quarter.R")

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

# inspect a_number
# all 8 non-format a_number are truly mis-formatted (non-NA)
daca_raw %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% dim()
daca_raw %>% filter(!(grepl("^A[0-9]{9}", a_number)))
daca_raw_non_format_a_numbers <- daca_raw %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% pull(a_number)
daca_raw_non_format_a_numbers
length(daca_raw_non_format_a_numbers)

# check for duplicates
# there are 9,282 duplicates
sum(duplicated(daca_raw$a_number)) 
daca_raw %>% distinct(a_number) %>% count()
9282 + 101566 == 110848

# inspect daca ident_report
# only 2 records in daca_raw have no ident_report, both are non-duplicates
# note the 2 a_numbers without ident_report are not among non-format a_number
glimpse(daca_raw)
sum(is.na(daca_raw$ident_report))
daca_raw <- daca_raw %>% mutate(ident_report_exist = case_when(is.na(ident_report) == TRUE ~ 0, TRUE ~ 1)) 
daca_raw %>% summarize(total_ident_report_exist = sum(ident_report_exist), total_records = n())
daca_raw %>% distinct(a_number, ident_report_exist) %>% summarize(total_ident_report_exist = sum(ident_report_exist), total_records = n())

# inspect a_number with no ident_report
daca_raw %>% filter(ident_report_exist == 0) %>% select(a_number) 
daca_raw_no_ident_report_exist <- daca_raw %>% filter(ident_report_exist == 0) %>% pull(a_number) 
daca_raw_no_ident_report_exist
daca_raw %>% filter(a_number %in% daca_raw_no_ident_report_exist)



######################################################################


# read in elis data

# read in elis 2016 - 2018
elis <- read_csv("Data/I-821D_ELIS_RECEIPTS__16_to_18.csv")
elis_placeholder <- elis
# elis <- elis_placeholder
head(elis)
glimpse(elis) # 720,642

# rename
elis <- elis %>% 
        rename(a_number = A_NUMBER, status_date = status_DATE, benefit_status = CASE_STUS_CD_DESC, benefit_status_code = CASE_STATUS_CODE)

# output original table of count by benefit_status
elis_original_benefit_status_count_table <- elis %>% group_by(benefit_status) %>% count() %>% arrange(desc(n))
elis_original_benefit_status_count_table
# write_csv(elis_original_benefit_status_count_table, "Data/merge_summary_output/elis_original_benefit_status_count_table.csv")

# filter elis to just benefit_status = approved, in_process, denied, pending, terminated to match c3 (except in_process)
# overwrite in_process to be coded as pending, to match c3
elis %>% group_by(benefit_status) %>% count() %>% arrange(desc(n))
elis <- elis %>% 
        filter(benefit_status %in% c("Approved", "In Process", "Denied", "Pending", "Fee Payment Issues",
                                              "Terminated")) %>%
        mutate(benefit_status = case_when(benefit_status == "In Process" ~ "Pending", TRUE ~ benefit_status))
glimpse(elis) # 720,642
elis %>% group_by(benefit_status) %>% count() %>% arrange(desc(n))


# inspect a_number
# non NA or non-format a_number
elis %>% summarize(na_a_number = sum(is.na(a_number)))
elis %>% filter(is.na(a_number)) %>% head()
elis %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% dim()
elis %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% distinct(a_number)

# inspect duplicates
# there are 8450 duplicates
sum(duplicated(elis$a_number)) # 8,399
elis %>% distinct(a_number) %>% dim() # 712,243 
712243  + 8399 == 720642

# inspect date_received and status_date
sum(is.na(elis$received_date))
sum(is.na(elis$status_date))

# inspect received_date
elis_a_number_w_multiple_rec_fy <- elis %>% group_by(a_number) %>% distinct(received_date) %>% count() %>% arrange(desc(n)) %>% 
        filter(n > 1)
elis_a_number_w_multiple_rec_fy
elis %>% filter(a_number == "A099609300") # this has multiple receipt numbers, received dates, multiple benefit_status

elis %>% filter(benefit_status == "Pending", a_number %in% elis_a_number_w_multiple_rec_fy$a_number) %>% data.frame() %>% head() 
elis %>% filter(a_number == "A099720819")
elis %>% filter(a_number == "A207709063")
elis %>% filter(a_number == "A204240097") # note he has two approvals 3 months apart, with no deny etc??

# convert date_received to date, rec_fy variable
elis$received_date <- mdy(elis$received_date)
elis <- elis %>% mutate(rec_fy_qtr = quarter(received_date, fiscal_start = 10, with_year = TRUE)) %>%
        separate(col = rec_fy_qtr, into = c("rec_fy", "rec_qtr"), sep = "\\.", remove = FALSE)
glimpse(elis)

# convert status_date to date, create act_fy variable
elis$status_date <- mdy(elis$status_date)
elis <- elis %>% mutate(act_fy_qtr = quarter(status_date, fiscal_start = 10, with_year = TRUE)) %>%
        separate(col = act_fy_qtr, into = c("act_fy", "act_qtr"), sep = "\\.", remove = FALSE)
glimpse(elis)

# check rec_fy and act_fy
elis %>% filter(rec_qtr == 4) %>% select(received_date, rec_fy, rec_qtr)
elis %>% filter(rec_qtr == 1) %>% select(received_date, rec_fy, rec_qtr)

elis %>% filter(act_qtr == 4) %>% select(status_date, act_fy, act_qtr)
elis %>% filter(act_qtr == 1) %>% select(status_date, act_fy, act_qtr)

# inspect
elis %>% group_by(benefit_status) %>% count() %>% arrange(desc(n))
elis %>% group_by(rec_fy) %>% count()


###########################################################################


# read in c3 data
c3 <- read_csv("Data/KEEPING_I821D_Updated.csv")
c3_placeholder <- c3
head(c3)
glimpse(c3) # 2,650,038

# output table of original c3 count by benefit status
c3_original_benefit_status_count_table <- c3 %>% group_by(CURRENT_STATUS) %>% count() %>% arrange(desc(n))
c3_original_benefit_status_count_table 
# write_csv(c3_original_benefit_status_count_table, "Data/merge_summary_output/c3_original_benefit_status_count_table.csv")

# convert REC_DATE and ACT_DATE to dates
c3$REC_DATE <- dmy(c3$REC_DATE)
c3$ACT_DATE <- dmy(c3$ACT_DATE)
glimpse(c3)

# inspect c3
c3 %>% group_by(CURRENT_STATUS) %>% count() %>% arrange(desc(n))
c3 %>% filter(PART_2_2 %in% c(1, 2)) %>% group_by(CURRENT_STATUS) %>% count() %>% arrange(desc(n))
c3 %>% filter(PART_2_2 == 1) %>% dim()
c3 %>% group_by(REC_FY) %>% count()
c3 %>% group_by(ACT_FY) %>% count()
c3 %>% group_by(PART_2_1) %>% count()
c3 %>% group_by(PART_2_2) %>% count()

# remove records with benefit_status = admin_closed 
c3 <- c3 %>% filter(CURRENT_STATUS != "ADMIN CLOSED")
c3 %>% group_by(CURRENT_STATUS) %>% count() %>% arrange(desc(n))

# inspect a_number
# all non-formatted a_numbers are NA
c3 %>% summarize(na_a_number = sum(is.na(BEN_A_NUMBER)))
c3 %>% filter(is.na(BEN_A_NUMBER))
c3 %>% filter(!(grepl("^A[0-9]{9}", BEN_A_NUMBER))) %>% dim() # 23
c3 %>% filter(!(grepl("^A[0-9]{9}", BEN_A_NUMBER))) %>% distinct(BEN_A_NUMBER)

# inspect duplicates
glimpse(c3) # 2,648,812
sum(duplicated(c3$BEN_A_NUMBER)) # 1,850,001
c3 %>% distinct(BEN_A_NUMBER) %>% count() # 798,811
798811 + 1850001 == 2648812

# inspect REC_FY and ACT_FY
# no missing values for either
sum(is.na(c3$REC_FY))
sum(is.na(c3$ACT_FY))

c3_a_number_w_multiple_rec_fy <- c3 %>% group_by(BEN_A_NUMBER) %>% distinct(REC_FY) %>% count() %>% arrange(desc(n))
c3_a_number_w_multiple_rec_fy <- c3_a_number_w_multiple_rec_fy %>% filter(n > 1)
c3_a_number_w_multiple_rec_fy
c3 %>% filter(BEN_A_NUMBER == "A200564686") %>% data.frame()
c3 %>% filter(BEN_A_NUMBER == "A204279818") %>% data.frame() # this has multiple recipt numbers, received dates, and current_status values

c3 %>% filter(CURRENT_STATUS == "*PEN", BEN_A_NUMBER %in% c3_a_number_w_multiple_rec_fy$BEN_A_NUMBER)
c3 %>% filter(BEN_A_NUMBER == "A204276813") %>% data.frame() # this has pending, then later an approval, pending act_fy is later date
c3 %>% filter(BEN_A_NUMBER == "A204286835") %>% data.frame() # this has pending, then later an approval, pending act_fy is historic date


######################################################################


# combine c3 and elis a_number
c3_extract <- c3 %>% select(BEN_A_NUMBER, RECEIPT_NUMBER, CURRENT_STATUS, REC_DATE, REC_FY, ACT_DATE, ACT_FY) %>% 
        rename(a_number = BEN_A_NUMBER, receipt_number = RECEIPT_NUMBER, benefit_status = CURRENT_STATUS, rec_date = REC_DATE, rec_fy = REC_FY, 
               act_date = ACT_DATE, act_fy = ACT_FY) %>% mutate(database = "c3")
elis_extract <- elis %>% select(a_number, USCIS_RCPT_NBR, benefit_status, received_date, rec_fy, status_date, act_fy) %>% mutate(database = "elis") %>%
        rename(receipt_number = USCIS_RCPT_NBR, rec_date = received_date, act_date = status_date)

c3_elis <- rbind(c3_extract, elis_extract)
head(c3_elis)
glimpse(c3_elis) # 3,369,454
720642 + 2648812 == 3369454

# check fy
c3_elis %>% group_by(rec_fy) %>% count()
c3_elis %>% group_by(act_fy) %>% count()

c3_elis %>% filter(database == "c3") %>% group_by(rec_fy) %>% count()
c3_elis %>% filter(database == "elis") %>% group_by(rec_fy) %>% count()

c3_elis %>% filter(database == "c3") %>% group_by(act_fy) %>% count()
c3_elis %>% filter(database == "elis") %>% group_by(act_fy) %>% count()

# check a_number
# all 28 non-format a_numbers are NA
c3_elis %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% dim() # 23
c3_elis %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% distinct(a_number)
sum(is.na(c3_elis$a_number)) # 23

# inspect duplicates
c3_elis %>% distinct(a_number) %>% dim() # 888766
sum(duplicated(c3_elis$a_number)) # 2,480,688
2480688 + 888766 == 3369454

# rename benefit_status values to be the same
c3_elis %>% group_by(benefit_status) %>% count()
c3_elis <- c3_elis %>% mutate(benefit_status = case_when(benefit_status == "*PEN" ~ "Pending", 
                        benefit_status == "APP" ~ "Approved", benefit_status == "DEN" ~ "Denied", TRUE ~ benefit_status))
c3_elis %>% group_by(benefit_status) %>% count()

# output table of c3_elis count by benefit status
c3_elis_benefit_status_count_table <- c3_elis %>% group_by(benefit_status) %>% count() %>% arrange(desc(n))
c3_elis_benefit_status_count_table
# write_csv(c3_elis_benefit_status_count_table, "Data/merge_summary_output/c3_elis_benefit_status_count_table.csv")

# inspect benefit status by year
c3_elis %>% group_by(rec_fy, benefit_status) %>% count() %>% data.frame()
c3_elis %>% filter(database == "c3") %>% group_by(rec_fy, benefit_status) %>% count() %>% data.frame()
c3_elis %>% filter(database == "elis") %>% group_by(rec_fy, benefit_status) %>% count() %>% data.frame()

# flag c3_elis records with ident_report in daca_raw
glimpse(daca_raw)
daca_raw %>% group_by(ident_report_exist) %>% count()
ident_report_exist_a_numbers <- daca_raw %>% filter(ident_report_exist == 1) %>% pull(a_number)
length(ident_report_exist_a_numbers)

c3_elis <- c3_elis %>% mutate(ident_report_exist = case_when(a_number %in% ident_report_exist_a_numbers ~ 1, TRUE ~ 0))
glimpse(c3_elis)

# inspect c3_elis ident_report_exist
c3_elis %>% group_by(ident_report_exist) %>% count() # 335,210
c3_elis %>% filter(ident_report_exist == 1) %>% distinct(a_number) %>% count() # 98,090

# write c3_elis to file
# write_csv(c3_elis, "Data/c3_elis.csv")
# c3_elis <- read_csv("Data/c3_elis.csv")

#########################################################################


# inspect 
elis %>% dim() # 720,642
c3 %>% dim() # 2,648,812
c3_elis %>% dim() # 3,369,454
daca_raw %>% dim() # 110,848


elis %>% distinct(a_number) %>% dim() # 712,243
c3 %>% distinct(BEN_A_NUMBER) %>% dim() # 798,811
c3_elis %>% distinct(a_number) %>% dim() # 888,766
daca_raw %>% distinct(a_number) %>% dim() # 101,566


#############################################################


# compare to c3_elis and daca_raw a_num

# daca_raw in c3_elis
daca_raw_in_c3_elis_index <- which(daca_raw$a_number %in% c3_elis$a_number)
daca_raw_in_c3_elis <- daca_raw[daca_raw_in_c3_elis_index, ]
total_daca_raw_in_c3_elis <- dim(daca_raw_in_c3_elis)[1] # 107,256
total_daca_raw_in_c3_elis
unique_daca_raw_in_c3_elis <- daca_raw_in_c3_elis %>% distinct(a_number) %>% dim() %>% .[1] # 98,092
unique_daca_raw_in_c3_elis

# daca_raw_in_c3_elis2 <- daca_raw[daca_raw$a_number %in% c3_elis$a_number, ]
# dim(daca_raw_in_c3_elis2) 
# daca_raw_in_c3_elis2 %>% distinct(a_number) %>% dim() 

# daca_raw not in c3_elis
daca_raw_not_in_c3_elis <- daca_raw[!(daca_raw$a_number %in% c3_elis$a_number), ]
total_daca_raw_not_in_c3_elis <- dim(daca_raw_not_in_c3_elis)[1] # 3,579
total_daca_raw_not_in_c3_elis
unique_daca_raw_not_in_c3_elis <- daca_raw_not_in_c3_elis %>% distinct(a_number) %>% dim() %>% .[1] # 3,461
unique_daca_raw_not_in_c3_elis

# daca_raw_not_in_c3_elis_distinct <- daca_raw_not_in_c3_elis %>% distinct(a_number)
# write_csv(daca_raw_not_in_c3_elis_distinct , path = "Data/matching_w_ried/daca_raw_not_in_c3_elis_distinct.csv")
# c3_elis %>% filter(a_number %in% daca_raw_not_in_c3_elis_distinct)

# total daca_raw in/out c3_elis
total_daca_raw_in_c3_elis + total_daca_raw_not_in_c3_elis == 110848

# total unique daca_raw in/out c3_elis
unique_daca_raw_in_c3_elis + unique_daca_raw_not_in_c3_elis == 101566

# check if daca_raw_non_format_a_numbers are in c3_elis
# none of the non-format a_numbers are in c3_elis
daca_raw_non_format_a_numbers %in% c3_elis$a_number

# check if daca_raw a_number with no ident_report are in c3_elis
# both are in c3_elis
daca_raw_no_ident_report_exist %in% c3_elis$a_number


#######################################################


# c3_elis in daca
c3_elis_in_daca_raw_index <- which(c3_elis$a_number %in% daca_raw$a_number)
c3_elis_in_daca_raw <- c3_elis[c3_elis_in_daca_raw_index, ]
total_c3_elis_in_daca_raw <- dim(c3_elis_in_daca_raw)[1] # 335,210
total_c3_elis_in_daca_raw
unique_c3_elis_in_daca_raw <- c3_elis_in_daca_raw %>% distinct(a_number) %>% dim() %>% .[1] # 98,105
unique_c3_elis_in_daca_raw

# c3_elis not in daca
c3_elis_not_in_daca_raw <- c3_elis[!(c3_elis$a_number %in% daca_raw$a_number), ]
total_c3_elis_not_in_daca_raw <- dim(c3_elis_not_in_daca_raw)[1] # 3,035,058 
total_c3_elis_not_in_daca_raw
unique_c3_elis_not_in_daca_raw <- c3_elis_not_in_daca_raw %>% distinct(a_number) %>% dim() %>% .[1] # 790,850
unique_c3_elis_not_in_daca_raw

# total c3_elis in/out daca_raw
total_c3_elis_in_daca_raw + total_c3_elis_not_in_daca_raw == 3369454


# percent total c3_elis in/out daca_raw
total_c3_elis_in_daca_raw / (total_c3_elis_in_daca_raw + total_c3_elis_not_in_daca_raw)
total_c3_elis_not_in_daca_raw / (total_c3_elis_in_daca_raw + total_c3_elis_not_in_daca_raw)



# total unique c3_elis in/out daca_raw
unique_c3_elis_in_daca_raw + unique_c3_elis_not_in_daca_raw == 888766

# percent unique c3_elis in/out daca_raw
unique_c3_elis_in_daca_raw / (unique_c3_elis_in_daca_raw + unique_c3_elis_not_in_daca_raw)
unique_c3_elis_not_in_daca_raw / (unique_c3_elis_in_daca_raw + unique_c3_elis_not_in_daca_raw)


######################################################################


# get overall percentage of total/unique c3_elis with ident_report_exist

# get percentage of total c3_elis with ident_report_exist
c3_elis %>% group_by(ident_report_exist) %>% count() %>% ungroup() %>% mutate(pct = n / sum(n))

# get percentage of unique c3_elis with ident_report_exist
c3_elis %>% distinct(a_number, ident_report_exist) %>% group_by(ident_report_exist) %>% count() %>% ungroup() %>% mutate(pct = n / sum(n))


###########################################################


# get percentage of total apps with ident_report_exist by FY

# create fy table
# c3_elis_fy_table <- c3_elis %>% group_by(rec_fy) %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                                                            pct_w_ident_report = count_w_ident_report / n(),
#                                                                                            count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                                            pct_wo_ident_report = count_wo_ident_report / n())
# c3_elis_fy_table 
# 
# # create totals table
# c3_elis_totals_table <- c3_elis %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                                           pct_w_ident_report = count_w_ident_report / n(),
#                                                                           count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                           pct_wo_ident_report = count_wo_ident_report / n()) %>%
#         mutate(rec_fy = "Total") %>% select(rec_fy, everything())
# c3_elis_totals_table
# 
# # combine fy and totals table
# c3_elis_table <- rbind(c3_elis_fy_table, c3_elis_totals_table)
# c3_elis_table 
# 
# # format counts and pct
# c3_elis_table <- c3_elis_table %>% mutate(total_count = comma(total_count), count_w_ident_report = comma(count_w_ident_report),
#                                     pct_w_ident_report = percent(pct_w_ident_report), 
#                                     count_wo_ident_report = comma(count_wo_ident_report),
#                                     pct_wo_ident_report = percent(pct_wo_ident_report))
# c3_elis_table
# 
# # write table
# write_csv(c3_elis_table , "Data/merge_summary_output/c3_elis_table.csv")


##############################################################


# get percentage of c3 total apps with ident_report_exist by FY

# create fy table
# c3_fy_table <- c3_elis %>% filter(database == "c3") %>% group_by(rec_fy) %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                                                            pct_w_ident_report = count_w_ident_report / n(),
#                                                                                            count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                                            pct_wo_ident_report = count_wo_ident_report / n())
# c3_fy_table 
# 
# # create totals table
# c3_totals_table <- c3_elis %>% filter(database == "c3") %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                                           pct_w_ident_report = count_w_ident_report / n(),
#                                                                           count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                           pct_wo_ident_report = count_wo_ident_report / n()) %>%
#         mutate(rec_fy = "Total") %>% select(rec_fy, everything())
# c3_totals_table
# 
# # combine fy and totals table
# c3_table <- rbind(c3_fy_table, c3_totals_table)
# c3_table 
# 
# # format counts and pct
# c3_table <- c3_table %>% mutate(total_count = comma(total_count), count_w_ident_report = comma(count_w_ident_report),
#                                     pct_w_ident_report = percent(pct_w_ident_report), 
#                                     count_wo_ident_report = comma(count_wo_ident_report),
#                                     pct_wo_ident_report = percent(pct_wo_ident_report))
# c3_table
# 
# # write table
# write_csv(c3_table , "Data/merge_summary_output/c3_table.csv")


####################################################################


# get percentage of elis total apps with ident_report_exist by FY

# create fy table
# elis_fy_table <- c3_elis %>% filter(database == "elis") %>% group_by(rec_fy) %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                pct_w_ident_report = count_w_ident_report / n(),
#                                                count_wo_ident_report = n() - sum(ident_report_exist),
#                                                pct_wo_ident_report = count_wo_ident_report / n())
# elis_fy_table 
# 
# # create totals table
# elis_totals_table <- c3_elis %>% filter(database == "elis") %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                                           pct_w_ident_report = count_w_ident_report / n(),
#                                                                           count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                           pct_wo_ident_report = count_wo_ident_report / n()) %>%
#         mutate(rec_fy = "Total") %>% select(rec_fy, everything())
# elis_totals_table
# 
# # combine fy and totals table
# elis_table <- rbind(elis_fy_table, elis_totals_table)
# elis_table 
# 
# # format counts and pct
# elis_table <- elis_table %>% mutate(total_count = comma(total_count), count_w_ident_report = comma(count_w_ident_report),
#                                     pct_w_ident_report = percent(pct_w_ident_report), 
#                                     count_wo_ident_report = comma(count_wo_ident_report),
#                                     pct_wo_ident_report = percent(pct_wo_ident_report))
# elis_table
# 
# # write table
# write_csv(elis_table , "Data/merge_summary_output/elis_table.csv")


#####################################################################


# get percentage of total apps with ident_report_exist by FY by status

# create fy table
# c3_elis_fy_status_table <- c3_elis %>% group_by(rec_fy, benefit_status) %>% summarize(total_count = n(), 
#                                                                                       count_w_ident_report = sum(ident_report_exist),
#                                                                pct_w_ident_report = count_w_ident_report / n(),
#                                                                count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                pct_wo_ident_report = count_wo_ident_report / n()) 
# c3_elis_fy_status_table 
# 
# # reshape to get status_count columns
# c3_elis_fy_status_table <- c3_elis_fy_status_table %>% gather(key = variable, value = value, -c(rec_fy, benefit_status)) %>%
#         unite(col = benefit_status_variable, c(benefit_status, variable)) %>%
#         spread(key = benefit_status_variable, value = value) %>%
#         select(rec_fy, `Approved_total_count`, `Approved_count_w_ident_report`, `Approved_pct_w_ident_report`,
#                `Approved_count_wo_ident_report`, `Approved_pct_wo_ident_report`, 
#                `Denied_total_count`, `Denied_count_w_ident_report`, `Denied_pct_w_ident_report`,
#                `Denied_count_wo_ident_report`, `Denied_pct_wo_ident_report`, 
#                `Pending_total_count`, `Pending_count_w_ident_report`, `Pending_pct_w_ident_report`,
#                `Pending_count_wo_ident_report`, `Pending_pct_wo_ident_report`,
#                `Admin Closed_total_count`, `Admin Closed_count_w_ident_report`, `Admin Closed_pct_w_ident_report`,
#                `Admin Closed_count_wo_ident_report`, `Admin Closed_pct_wo_ident_report`) %>% 
#         mutate(`Approved_total_count` = comma(`Approved_total_count`), `Approved_count_w_ident_report` = comma(`Approved_count_w_ident_report`),
#                `Approved_pct_w_ident_report` = percent(`Approved_pct_w_ident_report`), 
#                `Approved_count_wo_ident_report` = comma(`Approved_count_wo_ident_report`),
#                `Approved_pct_wo_ident_report` = percent(`Approved_pct_wo_ident_report`),
#                `Denied_total_count` = comma(`Denied_total_count`), `Denied_count_w_ident_report` = comma(`Denied_count_w_ident_report`),
#                `Denied_pct_w_ident_report` = percent(`Denied_pct_w_ident_report`), 
#                `Denied_count_wo_ident_report` = comma(`Denied_count_wo_ident_report`),
#                `Denied_pct_wo_ident_report` = percent(`Denied_pct_wo_ident_report`),
#                `Pending_total_count` = comma(`Pending_total_count`), `Pending_count_w_ident_report` = comma(`Pending_count_w_ident_report`),
#                `Pending_pct_w_ident_report` = percent(`Pending_pct_w_ident_report`), 
#                `Pending_count_wo_ident_report` = comma(`Pending_count_wo_ident_report`),
#                `Pending_pct_wo_ident_report` = percent(`Pending_pct_wo_ident_report`),
#                `Admin Closed_total_count` = comma(`Admin Closed_total_count`), `Admin Closed_count_w_ident_report` = comma(`Admin Closed_count_w_ident_report`),
#                `Admin Closed_pct_w_ident_report` = percent(`Admin Closed_pct_w_ident_report`), 
#                `Admin Closed_count_wo_ident_report` = comma(`Admin Closed_count_wo_ident_report`),
#                `Admin Closed_pct_wo_ident_report` = percent(`Admin Closed_pct_wo_ident_report`))
# 
# c3_elis_fy_status_table
# 
# # write table
# write_csv(c3_elis_fy_status_table, "Data/merge_summary_output/c3_elis_fy_status_table.csv")


###################################################################


# get percentage of c3 apps with ident_report_exist by FY by status

# create fy table
# c3_fy_status_table <- c3_elis %>% filter(database == "c3") %>% group_by(rec_fy, benefit_status) %>% summarize(total_count = n(), 
#                                                                                       count_w_ident_report = sum(ident_report_exist),
#                                                                                       pct_w_ident_report = count_w_ident_report / n(),
#                                                                                       count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                                       pct_wo_ident_report = count_wo_ident_report / n()) 
# c3_fy_status_table 
# 
# # reshape to get status_count columns
# c3_fy_status_table <- c3_fy_status_table %>% gather(key = variable, value = value, -c(rec_fy, benefit_status)) %>%
#         unite(col = benefit_status_variable, c(benefit_status, variable)) %>%
#         spread(key = benefit_status_variable, value = value) %>%
#         select(rec_fy, `Approved_total_count`, `Approved_count_w_ident_report`, `Approved_pct_w_ident_report`,
#                `Approved_count_wo_ident_report`, `Approved_pct_wo_ident_report`, 
#                `Denied_total_count`, `Denied_count_w_ident_report`, `Denied_pct_w_ident_report`,
#                `Denied_count_wo_ident_report`, `Denied_pct_wo_ident_report`, 
#                `Pending_total_count`, `Pending_count_w_ident_report`, `Pending_pct_w_ident_report`,
#                `Pending_count_wo_ident_report`, `Pending_pct_wo_ident_report`,
#                `Admin Closed_total_count`, `Admin Closed_count_w_ident_report`, `Admin Closed_pct_w_ident_report`,
#                `Admin Closed_count_wo_ident_report`, `Admin Closed_pct_wo_ident_report`) %>% 
#         mutate(`Approved_total_count` = comma(`Approved_total_count`), `Approved_count_w_ident_report` = comma(`Approved_count_w_ident_report`),
#                `Approved_pct_w_ident_report` = percent(`Approved_pct_w_ident_report`), 
#                `Approved_count_wo_ident_report` = comma(`Approved_count_wo_ident_report`),
#                `Approved_pct_wo_ident_report` = percent(`Approved_pct_wo_ident_report`),
#                `Denied_total_count` = comma(`Denied_total_count`), `Denied_count_w_ident_report` = comma(`Denied_count_w_ident_report`),
#                `Denied_pct_w_ident_report` = percent(`Denied_pct_w_ident_report`), 
#                `Denied_count_wo_ident_report` = comma(`Denied_count_wo_ident_report`),
#                `Denied_pct_wo_ident_report` = percent(`Denied_pct_wo_ident_report`),
#                `Pending_total_count` = comma(`Pending_total_count`), `Pending_count_w_ident_report` = comma(`Pending_count_w_ident_report`),
#                `Pending_pct_w_ident_report` = percent(`Pending_pct_w_ident_report`), 
#                `Pending_count_wo_ident_report` = comma(`Pending_count_wo_ident_report`),
#                `Pending_pct_wo_ident_report` = percent(`Pending_pct_wo_ident_report`),
#                `Admin Closed_total_count` = comma(`Admin Closed_total_count`), `Admin Closed_count_w_ident_report` = comma(`Admin Closed_count_w_ident_report`),
#                `Admin Closed_pct_w_ident_report` = percent(`Admin Closed_pct_w_ident_report`), 
#                `Admin Closed_count_wo_ident_report` = comma(`Admin Closed_count_wo_ident_report`),
#                `Admin Closed_pct_wo_ident_report` = percent(`Admin Closed_pct_wo_ident_report`))
# 
# c3_fy_status_table
# 
# # write table
# write_csv(c3_fy_status_table, "Data/merge_summary_output/c3_fy_status_table.csv")
# 

###############################################################


# get percentage of elis apps with ident_report_exist by FY by status

# create fy table
# elis_fy_status_table <- c3_elis %>% filter(database == "elis") %>% group_by(rec_fy, benefit_status) %>% summarize(total_count = n(), 
#                                                                                  count_w_ident_report = sum(ident_report_exist),
#                                                                                  pct_w_ident_report = count_w_ident_report / n(),
#                                                                                  count_wo_ident_report = n() - sum(ident_report_exist),
#                                                                                  pct_wo_ident_report = count_wo_ident_report / n()) 
# elis_fy_status_table 
# 
# # reshape to get status_count columns
# elis_fy_status_table <- elis_fy_status_table %>% gather(key = variable, value = value, -c(rec_fy, benefit_status)) %>%
#         unite(col = benefit_status_variable, c(benefit_status, variable)) %>%
#         spread(key = benefit_status_variable, value = value) %>%
#         select(rec_fy, `Approved_total_count`, `Approved_count_w_ident_report`, `Approved_pct_w_ident_report`,
#                `Approved_count_wo_ident_report`, `Approved_pct_wo_ident_report`, 
#                `Denied_total_count`, `Denied_count_w_ident_report`, `Denied_pct_w_ident_report`,
#                `Denied_count_wo_ident_report`, `Denied_pct_wo_ident_report`, 
#                `Pending_total_count`, `Pending_count_w_ident_report`, `Pending_pct_w_ident_report`,
#                `Pending_count_wo_ident_report`, `Pending_pct_wo_ident_report`,
#                `Admin Closed_total_count`, `Admin Closed_count_w_ident_report`, `Admin Closed_pct_w_ident_report`,
#                `Admin Closed_count_wo_ident_report`, `Admin Closed_pct_wo_ident_report`) %>% 
#         mutate(`Approved_total_count` = comma(`Approved_total_count`), `Approved_count_w_ident_report` = comma(`Approved_count_w_ident_report`),
#                `Approved_pct_w_ident_report` = percent(`Approved_pct_w_ident_report`), 
#                `Approved_count_wo_ident_report` = comma(`Approved_count_wo_ident_report`),
#                `Approved_pct_wo_ident_report` = percent(`Approved_pct_wo_ident_report`),
#                `Denied_total_count` = comma(`Denied_total_count`), `Denied_count_w_ident_report` = comma(`Denied_count_w_ident_report`),
#                `Denied_pct_w_ident_report` = percent(`Denied_pct_w_ident_report`), 
#                `Denied_count_wo_ident_report` = comma(`Denied_count_wo_ident_report`),
#                `Denied_pct_wo_ident_report` = percent(`Denied_pct_wo_ident_report`),
#                `Pending_total_count` = comma(`Pending_total_count`), `Pending_count_w_ident_report` = comma(`Pending_count_w_ident_report`),
#                `Pending_pct_w_ident_report` = percent(`Pending_pct_w_ident_report`), 
#                `Pending_count_wo_ident_report` = comma(`Pending_count_wo_ident_report`),
#                `Pending_pct_wo_ident_report` = percent(`Pending_pct_wo_ident_report`),
#                `Admin Closed_total_count` = comma(`Admin Closed_total_count`), `Admin Closed_count_w_ident_report` = comma(`Admin Closed_count_w_ident_report`),
#                `Admin Closed_pct_w_ident_report` = percent(`Admin Closed_pct_w_ident_report`), 
#                `Admin Closed_count_wo_ident_report` = comma(`Admin Closed_count_wo_ident_report`),
#                `Admin Closed_pct_wo_ident_report` = percent(`Admin Closed_pct_wo_ident_report`))
# 
# elis_fy_status_table
# 
# # write table
# write_csv(elis_fy_status_table, "Data/merge_summary_output/elis_fy_status_table.csv")



##########################################################




# pull a_numbers that were approved/denied/pending, then create dummy in c3_elis for ever-approved/denied/pending
# c3_elis %>% distinct(a_number) %>% dim()
# 
# ever_approved_a_numbers <- c3_elis %>% filter(benefit_status == "Approved") %>% distinct(a_number) %>% pull(a_number)
# length(ever_approved_a_numbers)
# 
# ever_denied_a_numbers <- c3_elis %>% filter(benefit_status == "Denied") %>% distinct(a_number) %>% pull(a_number)
# length(ever_denied_a_numbers)
# 
# ever_pending_a_numbers <- c3_elis %>% filter(benefit_status == "Pending") %>% distinct(a_number) %>% pull(a_number)
# length(ever_pending_a_numbers)
# 
# ever_admin_closed_a_numbers <- c3_elis %>% filter(benefit_status == "Admin Closed") %>% distinct(a_number) %>% pull(a_number)
# length(ever_admin_closed_a_numbers)
# 
# # add ever_approved/denied/pending/admin_closed variables to c3_elis
# c3_elis <- c3_elis %>% mutate(ever_approved = ifelse(a_number %in% ever_approved_a_numbers, 1, 0),
#                                ever_denied = ifelse(a_number %in% ever_denied_a_numbers, 1, 0),
#                                ever_pending = ifelse(a_number %in% ever_pending_a_numbers, 1, 0),
#                                ever_admin_closed = ifelse(a_number %in% ever_admin_closed_a_numbers, 1, 0))
# glimpse(c3_elis)
# 
# # inspect
# c3_elis %>% distinct(a_number, ever_approved) %>% group_by(ever_approved) %>% count() # 809,402
# c3_elis %>% distinct(a_number, ever_denied) %>% group_by(ever_denied) %>% count() # 80,646
# c3_elis %>% distinct(a_number, ever_pending) %>% group_by(ever_pending) %>% count() # 52,310
# c3_elis %>% distinct(a_number, ever_admin_closed) %>% group_by(ever_admin_closed) %>% count() # 1,252


#########################################################################



# get counts of unique a_number by status
# c3_elis %>% distinct(a_number) %>% dim()
# c3_elis %>% distinct(a_number, ident_report_exist, ever_approved, ever_denied, ever_pending, ever_admin_closed) %>% dim()
# 
# c3_elis_a_number_status_table <- c3_elis %>% distinct(a_number, ever_approved, ever_denied, ever_pending, ever_admin_closed) %>% 
#         summarize(count_unique_a_number = n(), count_ever_approved = sum(ever_approved), 
#                   pct_ever_approved = sum(ever_approved) / n(), count_never_approved = sum(ever_approved == 0),
#                   pct_never_approved = sum(ever_approved == 0) / n(), count_ever_denied = sum(ever_denied),
#                   pct_ever_denied = sum(ever_denied) / n(), count_never_denied = sum(ever_denied == 0),
#                   pct_never_denied = sum(ever_denied == 0) / n(), count_ever_pending = sum(ever_pending),
#                   pct_ever_pending = sum(ever_pending) / n(), count_never_pending = sum(ever_pending == 0),
#                   pct_never_pending = sum(ever_pending == 0) / n(), count_ever_admin_closed = sum(ever_admin_closed),
#                   pct_ever_admin_closed = sum(ever_admin_closed) / n(), count_never_admin_closed = sum(ever_admin_closed == 0),
#                   pct_never_admin_closed = sum(ever_admin_closed == 0) / n()) %>% 
#         mutate(count_unique_a_number = comma(count_unique_a_number), count_ever_approved = comma(count_ever_approved),
#                pct_ever_approved = percent(pct_ever_approved), count_never_approved = comma(count_never_approved),
#                pct_never_approved = percent(pct_never_approved),
#                count_unique_a_number = comma(count_unique_a_number), count_ever_denied = comma(count_ever_denied),
#                pct_ever_denied = percent(pct_ever_denied), count_never_denied = comma(count_never_denied),
#                pct_never_denied = percent(pct_never_denied),
#                count_unique_a_number = comma(count_unique_a_number), count_ever_pending = comma(count_ever_pending),
#                pct_ever_pending = percent(pct_ever_pending), count_never_pending = comma(count_never_pending),
#                pct_never_pending = percent(pct_never_pending),
#                count_unique_a_number = comma(count_unique_a_number), count_ever_admin_closed = comma(count_ever_admin_closed),
#                pct_ever_admin_closed = percent(pct_ever_admin_closed), count_never_admin_closed = comma(count_never_admin_closed),
#                pct_never_admin_closed = percent(pct_never_admin_closed))
# 
# c3_elis_a_number_status_table %>% data.frame()
# 
# # write table
# write_csv(c3_elis_a_number_status_table, "Data/merge_summary_output/c3_elis_a_number_status_table.csv")


####################################################################


# get counts of unique a_number by status by ident_report_exist
# c3_elis %>% distinct(a_number, ever_approved, ident_report_exist) %>% filter(ever_approved == 1) %>% dim()
# c3_elis %>% distinct(a_number, ever_approved, ident_report_exist) %>% filter(ever_approved == 1, ident_report_exist == 1) %>% dim()
# 77156 / 809402
# c3_elis %>% distinct(a_number, ever_approved, ident_report_exist) %>% filter(ever_approved == 0, ident_report_exist == 1) %>% dim()
# 
# c3_elis_a_number_status_ident_report_table <- c3_elis %>% distinct(a_number, ident_report_exist, ever_approved, ever_denied, ever_pending, ever_admin_closed) %>% 
#         summarize(count_unique_a_number = n(), count_w_ident_report = sum(ident_report_exist), 
#                   pct_w_ident_report = sum(ident_report_exist) / n(), 
#                   count_ever_approved_w_ident_report = sum(ever_approved == 1 & ident_report_exist == 1), 
#                   pct_ever_approved_w_ident_report = sum(ever_approved == 1 & ident_report_exist == 1) / sum(ever_approved == 1), 
#                   count_never_approved_w_ident_report = sum(ever_approved == 0 & ident_report_exist == 1),
#                   pct_never_approved_w_ident_report = sum(ever_approved == 0 & ident_report_exist == 1) / sum(ever_approved == 0), 
#                   count_ever_denied_w_ident_report = sum(ever_denied == 1 & ident_report_exist == 1), 
#                   pct_ever_denied_w_ident_report = sum(ever_denied == 1 & ident_report_exist == 1) / sum(ever_denied == 1), 
#                   count_never_denied_w_ident_report = sum(ever_denied == 0 & ident_report_exist == 1),
#                   pct_never_denied_w_ident_report = sum(ever_denied == 0 & ident_report_exist == 1) / sum(ever_denied == 0), 
#                   count_ever_pending_w_ident_report = sum(ever_pending == 1 & ident_report_exist == 1), 
#                   pct_ever_pending_w_ident_report = sum(ever_pending == 1 & ident_report_exist == 1) / sum(ever_pending == 1), 
#                   count_never_pending_w_ident_report = sum(ever_pending == 0 & ident_report_exist == 1),
#                   pct_never_pending_w_ident_report = sum(ever_pending == 0 & ident_report_exist == 1) / sum(ever_pending == 0),
#                   count_ever_admin_closed_w_ident_report = sum(ever_admin_closed == 1 & ident_report_exist == 1), 
#                   pct_ever_admin_closed_w_ident_report = sum(ever_admin_closed == 1 & ident_report_exist == 1) / sum(ever_admin_closed == 1), 
#                   count_never_admin_closed_w_ident_report = sum(ever_admin_closed == 0 & ident_report_exist == 1),
#                   pct_never_admin_closed_w_ident_report = sum(ever_admin_closed == 0 & ident_report_exist == 1) / sum(ever_admin_closed == 0)) %>%
#                 mutate(count_unique_a_number = comma(count_unique_a_number), count_w_ident_report = comma(count_w_ident_report),
#                        pct_w_ident_report = percent(pct_w_ident_report),
#                        count_ever_approved_w_ident_report = comma(count_ever_approved_w_ident_report),
#                        pct_ever_approved_w_ident_report = percent(pct_ever_approved_w_ident_report),
#                        count_never_approved_w_ident_report = comma(count_never_approved_w_ident_report),
#                        pct_never_approved_w_ident_report = percent(pct_never_approved_w_ident_report),
#                        count_ever_denied_w_ident_report = comma(count_ever_denied_w_ident_report),
#                        pct_ever_denied_w_ident_report = percent(pct_ever_denied_w_ident_report),
#                        count_never_denied_w_ident_report = comma(count_never_denied_w_ident_report),
#                        pct_never_denied_w_ident_report = percent(pct_never_denied_w_ident_report),
#                        count_ever_pending_w_ident_report = comma(count_ever_pending_w_ident_report),
#                        pct_ever_pending_w_ident_report = percent(pct_ever_pending_w_ident_report),
#                        count_never_pending_w_ident_report = comma(count_never_pending_w_ident_report),
#                        pct_never_pending_w_ident_report = percent(pct_never_pending_w_ident_report),
#                        count_ever_admin_closed_w_ident_report = comma(count_ever_admin_closed_w_ident_report),
#                        pct_ever_admin_closed_w_ident_report = percent(pct_ever_admin_closed_w_ident_report),
#                        count_never_admin_closed_w_ident_report = comma(count_never_admin_closed_w_ident_report),
#                        pct_never_admin_closed_w_ident_report = percent(pct_never_admin_closed_w_ident_report))
# 
# c3_elis_a_number_status_ident_report_table %>% data.frame()
# 
# # write table
# write_csv(c3_elis_a_number_status_ident_report_table, "Data/merge_summary_output/c3_elis_a_number_status_ident_report_table.csv")


######################################################################


# create variable for latest_benefit_status
glimpse(c3_elis)

# inspect act_date and benefit_status
sum(is.na(c3_elis$act_date))
sum(is.na(c3_elis$benefit_status))

# get latest_benefit_status and latest_act_date
c3_elis_latest <- c3_elis %>% 
        distinct(a_number, act_date, benefit_status) %>%
        arrange(a_number, desc(act_date)) %>% 
        rename(latest_benefit_status = benefit_status, latest_act_date = act_date) %>% group_by(a_number) %>% slice(1) %>% ungroup()

glimpse(c3_elis_latest)

# check latest
c3_elis %>% distinct(a_number) %>% count()
c3_elis_latest %>% distinct(a_number) %>% count()


c3_elis %>% filter(a_number == "A204184923")
c3_elis_latest %>% filter(a_number == "A204184923")

# combine c3_elis_latest_multiple_benefit_status with c3_elis
c3_elis <- c3_elis %>% left_join(., c3_elis_latest, by = c("a_number" = "a_number"))

# check merge
glimpse(c3_elis)
sum(is.na(c3_elis$latest_benefit_status))
sum(is.na(c3_elis$latest_act_date))

c3_elis %>% filter(!is.na(latest_benefit_status)) %>% distinct(a_number) %>% dim()
c3_elis %>% filter(!is.na(latest_act_date)) %>% distinct(a_number) %>% dim()

# convert latest_act_date to date and get latest_act_fy and latest_act_qtr
c3_elis <- c3_elis %>% mutate(latest_act_fy_qtr = quarter(latest_act_date, fiscal_start = 10, with_year = TRUE)) %>%
        separate(col = latest_act_fy_qtr, into = c("latest_act_fy", "latest_act_qtr"), sep = "\\.", remove = FALSE)
glimpse(c3_elis)

# check latest_act_fy
c3_elis %>% filter(latest_act_qtr == 4) %>% select(latest_act_date, latest_act_fy, latest_act_qtr) %>% head()
c3_elis %>% filter(latest_act_qtr == 1) %>% select(latest_act_date, latest_act_fy, latest_act_qtr) %>% head()

# act_fy_count <- c3_elis %>% distinct(a_number, lastest_act_fy) %>% group_by(a_number) %>% count() %>% arrange(desc(n)) 
# act_fy_count 


####################################################################


# read in scops file of revocations
scops_path <- str_c(getwd(), "Data/2128 Crim_Gang_Terminations_Cumulative.xlsx", sep = "/")
scops_path

# remove sheets identified via inspection to have termination dates that do not read in properly due to format
scops_sheets_all <- scops_path %>% excel_sheets()
length(scops_sheets_all)
excluded_sheets <- c("Crim_Gang_Terms_501_53117(70)", "Crim_Gang_terms_1101_113015(30)")

scops_sheets <- scops_sheets_all[!(scops_sheets_all %in% excluded_sheets)]
length(scops_sheets)

scops_sheets_excluded <- scops_sheets_all[scops_sheets_all %in% excluded_sheets]
length(scops_sheets_excluded)

# read in scops_worksheets
# scops_worksheets <- scops_path %>% excel_sheets() %>% set_names() %>% map(., .f = ~ read_excel(path = scops_path, sheet = .x))
scops_worksheets <- scops_sheets %>% set_names() %>%
        map_dfr(., .f = ~ read_excel(path = scops_path, sheet = .x), .id = "origin_sheet")

glimpse(scops_worksheets)
names(scops_worksheets)

# scops <- read_csv("Data/scops_criminal_terminations.csv")
# glimpse(scops)

scops_worksheets_excluded <- scops_sheets_excluded %>% set_names() %>%
        map_dfr(., .f = ~ read_excel(path = scops_path, sheet = .x), .id = "origin_sheet")
glimpse(scops_worksheets_excluded)
unique(scops_worksheets_excluded$`Termination Date (notice sent in claims)`)

scops_worksheets_excluded_1 <- scops_worksheets_excluded %>% 
        filter(!(`Termination Date (notice sent in claims)` %in% c("11/04/2015 and 11/18/2015", "5/16/217"))) %>%
        mutate(`Termination Date (notice sent in claims)` = as_date(as.numeric(`Termination Date (notice sent in claims)`), origin = "1899-12-30")) %>%
        select(-X__1)
glimpse(scops_worksheets_excluded_1)

scops_worksheets_excluded_2 <- scops_worksheets_excluded %>% 
        filter(`Termination Date (notice sent in claims)` == "11/04/2015 and 11/18/2015") %>%
        mutate(`Termination Date (notice sent in claims)` = "2015-11-04") %>% select(-X__1)
glimpse(scops_worksheets_excluded_2)

scops_worksheets_excluded_3 <- scops_worksheets_excluded %>% 
        filter(`Termination Date (notice sent in claims)` == "5/16/217") %>%
        mutate(`Termination Date (notice sent in claims)` = "2017-05-16") %>% select(-X__1)
glimpse(scops_worksheets_excluded_3)

# combine scops_worksheets with scops_worksheets_excluded_1/2/3
scops <- rbind(scops_worksheets, scops_worksheets_excluded_1, scops_worksheets_excluded_2, scops_worksheets_excluded_3)
glimpse(scops)

# convert and inspect dates
unique(scops$`Termination Date (notice sent in claims)`)
scops <- scops %>% mutate(`Termination Date (notice sent in claims)` = ymd_hms(`Termination Date (notice sent in claims)`))
glimpse(scops)


# rename variables
scops <- scops %>% rename(receipt_number = `Receipt number`, a_number = `Bene A File`, fy = FY, description = `DESCRIPTION OF ISSUE`,
                          termination_date = `Termination Date (notice sent in claims)`)
glimpse(scops)

# correct typo in a_number
scops <- scops %>% mutate(a_number = case_when(a_number == "A204324788`" ~ "A204324788", TRUE ~ a_number))
scops %>% filter(a_number == "A204324788")

# remove records with NA a_number
sum(is.na(scops$a_number))
scops <- scops %>% filter(!is.na(a_number))
glimpse(scops)


# inspect a_number
# 2090 unique anumbers
# 24 distinct duplicated a_numbers
# 22 of 24 have multiple distinct termination dates
# all 22 have exactly 2 termination dates, so we can just add termination_date2 / description2 variable
glimpse(scops)

scops %>% distinct(a_number) %>% count()
sum(duplicated(scops$a_number))

scops_duplicated <- duplicated(scops$a_number)
sum(scops_duplicated)
scops_duplicated_a_numbers <- scops %>% filter(scops_duplicated) %>% distinct(a_number) %>% pull(a_number)
length(scops_duplicated_a_numbers)
scops_duplicated_a_numbers
scops %>% filter(a_number %in% scops_duplicated_a_numbers) %>% select(a_number, termination_date) %>% arrange(a_number) %>% data.frame()
scops %>% filter(a_number %in% scops_duplicated_a_numbers) %>% distinct(a_number, termination_date) %>% group_by(a_number) %>% 
        filter(!is.na(a_number)) %>% count() %>% arrange(desc(n)) %>% data.frame()

glimpse(scops)
scops %>% distinct(a_number) %>% count()
sum(scops_duplicated)
24 + 2090 == 2114

# inspect a_number format
scops %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% dim()
scops %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% distinct(a_number)


# inspect scops variables
scops %>% map_dfr(., .f = ~ sum(is.na(.x)))

# inspect NA termination date
# confirmed that both termination_dates are blank in the original file
scops %>% filter(is.na(termination_date))


# create variables for scops a_numbers with multiple distinct termination dates
# drop records with NA a_number
scops_multiple_termination_date_a_numbers <- scops %>% filter(a_number %in% scops_duplicated_a_numbers) %>% 
        distinct(a_number, termination_date) %>% group_by(a_number) %>% 
        count() %>% arrange(desc(n)) %>% filter(n > 1, !is.na(a_number)) %>% pull(a_number)
length(scops_multiple_termination_date_a_numbers)

# flag multiple termination dates
scops <- scops %>% mutate(multiple_termination_dates = ifelse(a_number %in% scops_multiple_termination_date_a_numbers, 1, 0))
scops %>% filter(multiple_termination_dates == 1) %>% distinct(a_number) %>% count()

# get termination/description/receipt 1
scops_termination_date1 <- scops %>% arrange(a_number, termination_date) %>% group_by(a_number) %>% slice(1) %>% ungroup() %>%
        rename(termination_date1 = termination_date, description1 = description, scops_receipt_number1 = receipt_number) %>% 
        select(a_number, termination_date1, description1, scops_receipt_number1)
glimpse(scops_termination_date1)
scops_termination_date1 %>% distinct(a_number) %>% count()

# get termination/description/receipt 2
scops_termination_date2 <- scops %>% filter(a_number %in% scops_multiple_termination_date_a_numbers) %>%
        arrange(a_number, termination_date) %>% group_by(a_number) %>% slice(2) %>% ungroup() %>%
        rename(termination_date2 = termination_date, description2 = description, scops_receipt_number2 = receipt_number) %>% 
        select(a_number, termination_date2, description2, scops_receipt_number2)
glimpse(scops_termination_date2)
scops_termination_date2 %>% distinct(a_number) %>% count()

# add variables for termination date/description 1 and 2
scops <- scops %>% left_join(., scops_termination_date1, by = c("a_number" = "a_number"))
scops %>% select(a_number, termination_date, termination_date1)

scops <- scops %>% left_join(., scops_termination_date2, by = c("a_number" = "a_number"))
scops %>% select(a_number, termination_date, termination_date1, termination_date2) %>% arrange(a_number)
scops %>% filter(!is.na(termination_date2)) %>% select(a_number, termination_date, termination_date1, termination_date2) %>% arrange(a_number)
scops %>% filter(!is.na(description2)) %>% select(a_number, description, description1, description2) %>% arrange(a_number) %>%
        slice(1:2) %>% data.frame()
scops %>% filter(!is.na(scops_receipt_number2)) %>% select(a_number, receipt_number, scops_receipt_number1, scops_receipt_number2) %>% 
        arrange(a_number) %>% data.frame()

# inspect how many a_numbers with multiple termination_dates have multiple receipt_numbers
scops_termination_date2 %>% distinct(a_number) %>% count()
scops %>% filter(!is.na(termination_date2)) %>% count()
scops %>% group_by(multiple_termination_dates) %>% count()

# 5 receipt_numbers are single, so 3 anumbers have mutliple receipt numbers
scops %>% filter(multiple_termination_dates == 1) %>% group_by(receipt_number) %>% count() %>% arrange(n)
scops %>% distinct(a_number, receipt_number) %>% group_by(a_number) %>% count() %>% arrange(desc(n))




##########################################################################


# create c3_elis2, which is filtered c3_elis to remove 23 records without a_number
# this lets us merge scops by a_number, and also allows for easy tabulation at a_number level
sum(is.na(c3_elis$a_number))

c3_elis2 <- c3_elis %>% filter(!is.na(a_number))
glimpse(c3_elis2)
glimpse(c3_elis)


##############################################################


# create flag in c3_elis for scops termination

# get scops unique record
scops_termination_records <- scops %>% 
        distinct(a_number, termination_date1, description1, scops_receipt_number1, termination_date2, description2, scops_receipt_number2) %>%
        arrange(a_number)
scops_termination_records 

scops_termination_records_duplicated <- duplicated(scops_termination_records$a_number)
sum(scops_termination_records_duplicated)

scops_termination_records <- scops_termination_records %>% filter(!scops_termination_records_duplicated)
scops_termination_records %>% filter(!is.na(termination_date2)) %>% arrange(a_number) %>% data.frame() %>% head()
scops %>% filter(a_number == "A078606020") %>% select(a_number, termination_date, termination_date1, termination_date2) %>% data.frame()

# inspect scops_termination_records 
sum(is.na(scops_termination_records$a_number))
glimpse(scops_termination_records) 
scops_termination_records %>% distinct(a_number) %>% count()
scops_termination_records %>% data.frame() %>% head() 

# merge scops_a_number_termination_date with c3_elis
# c3_elis2 <- c3_elis2 %>% select(-c(termination_date1, termination_date2, description1, description2, scops_terminated))
c3_elis2 <- c3_elis2 %>% left_join(., scops_termination_records, by = c("a_number" = "a_number"))
glimpse(c3_elis2)

# create dummy for scops_terminated
c3_elis2 <- c3_elis2 %>% mutate(scops_terminated = ifelse(a_number %in% scops_termination_records$a_number, 1, 0))

# note that 2086 unique a_numbers are flagged scops_terminated, but scops has 2090 unique a_numbers
c3_elis2 %>% group_by(scops_terminated) %>% count()
c3_elis2 %>% distinct(a_number, scops_terminated) %>% group_by(scops_terminated) %>% count()

# find scops a_numbers not in c3_elis2_scops_terminated 
c3_elis2_scops_terminated_a_numbers <- c3_elis2 %>% filter(scops_terminated == 1) %>% distinct(a_number) %>% pull(a_number)
length(unique(c3_elis2_scops_terminated_a_numbers))

# pull subset of 4 unique scops matching c3_elis2_scops_terminated_a_numbers
scops %>% filter(!(a_number %in% c3_elis2_scops_terminated_a_numbers)) %>% distinct(a_number) %>% data.frame()
scops_not_in_c3_elis2 <- scops %>% filter(!(a_number %in% c3_elis2_scops_terminated_a_numbers)) %>% distinct(a_number) %>% pull(a_number)
scops_not_in_c3_elis2

# confirm if c3_elis2_scops_terminated_a_numbers are in scops_a_number_termination_date
length(c3_elis2_scops_terminated_a_numbers)
length(unique(scops_termination_records$a_number))

c3_elis2_scops_terminated_a_numbers[!(c3_elis2_scops_terminated_a_numbers %in% scops_termination_records$a_number)]
scops_termination_records$a_number[!(scops_termination_records$a_number %in% c3_elis2_scops_terminated_a_numbers)]

# inspect a_number with typo in scops
# this a_number now fixed in earlier code after reading in scops file
# c3_elis2 %>% filter(a_number == "A204324788")


# look for scops_not_in_c3_elis in original c3 and elis files

# note 2 of the 4 missing are in c3_placeholder w/ status admin_closed
glimpse(c3_placeholder) 
c3_placeholder %>% filter(BEN_A_NUMBER %in% scops_not_in_c3_elis2) %>% distinct(BEN_A_NUMBER) %>% data.frame()
c3_placeholder %>% filter(BEN_A_NUMBER %in% scops_not_in_c3_elis2) %>% distinct(CURRENT_STATUS) %>% data.frame()
c3_placeholder %>% filter(BEN_A_NUMBER %in% scops_not_in_c3_elis2, CURRENT_STATUS %in% c("APP", "*PEN")) %>% distinct(BEN_A_NUMBER)

# none of the 4 missing are in elis_placeholder
glimpse(elis_placeholder) 
elis_placeholder %>% filter(A_NUMBER %in% scops_not_in_c3_elis2) %>% data.frame()




# inspect scops_terminated
c3_elis2 %>% group_by(scops_terminated) %>% count()
c3_elis2 %>% distinct(a_number, scops_terminated) %>% group_by(scops_terminated) %>% count()


c3_elis2 %>% filter(scops_terminated == 1) %>% group_by(latest_benefit_status) %>% count()
c3_elis2 %>% filter(scops_terminated == 1) %>% distinct(a_number, latest_benefit_status) %>% 
        group_by(latest_benefit_status) %>% count()
c3_elis2 %>% filter(scops_terminated == 1, latest_benefit_status == "Approved") %>% distinct(a_number)
c3_elis2 %>% filter(a_number == "A204184923") %>% data.frame()
c3_elis2 %>% filter(a_number == "A204414038") %>% data.frame() # "DUI conviction"
c3_elis2 %>% filter(a_number == "A201290190") %>% data.frame() # "convicted domestic battery"
c3_elis2 %>% filter(a_number == "A099660387") %>% data.frame() # "due to public safety concern"
c3_elis2 %>% filter(a_number == "A204232682") %>% data.frame() # "NTA issued"
c3_elis2 %>% filter(a_number == "A204241905") %>% data.frame() # homicide "charge pending"

# write file with a_numbers having latest status approved, despite scops termination
# c3_elis_latest_approved_pending_despite_scops_termination <- c3_elis2 %>% 
#         filter(scops_terminated == 1, latest_benefit_status %in% c("Approved", "Pending")) %>% arrange(latest_benefit_status, a_number)
# c3_elis_latest_approved_pending_despite_scops_termination
# c3_elis_latest_approved_pending_despite_scops_termination %>% distinct(a_number) %>% count()

# write_csv(c3_elis_latest_approved_pending_despite_scops_termination, "Data/merge_summary_output/c3_elis_latest_approved_pending_despite_scops_termination.csv")


c3_placeholder %>% filter(BEN_A_NUMBER == "A099660387") %>% data.frame()
elis_placeholder %>% filter(a_number == "A099660387")


########################################################################


# read in c3 terminations
list.files("Data")
c3_term <- read_csv("Data/c3_terminations.csv")
glimpse(c3_term)

# rename variables
c3_term <- c3_term %>% rename(action_date = CONVERTED_ACTION_DATE_IN, a_number = BEN_A_NUMBER, receipt_number = RECEIPT_NUMBER,
                              action_text = ACTION_TEXT, action = ACTION)
glimpse(c3_term)


# inspect c3_term
sum(is.na(c3_term$a_number))
sum(is.na(c3_term$action_date))
sum(is.na(c3_term$receipt_number))
sum(is.na(c3_term$action))
sum(is.na(c3_term$action_text))

c3_term %>% distinct(a_number) %>% count()
c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% distinct(a_number) %>% count()


c3_term %>% distinct(action, action_text)
c3_term %>% group_by(action_text) %>% count() %>% arrange(desc(n))
c3_term %>% group_by(action) %>% count() %>% arrange(desc(n))

# create flag for c3_terminated
c3_term <- c3_term %>% mutate(c3_terminated = 1)

# merge c3_terminated flag with c3_elis2
# 23 unique anumbers in c3_term but not in c3_elis2
# but all are in c3, and were dropped because ADMIN_CLOSED
in_c3_elis2_and_in_c3_term <- c3_elis2$a_number[c3_elis2$a_number %in% c3_term$a_number]
length(in_c3_elis2_and_in_c3_term)
length(unique(in_c3_elis2_and_in_c3_term))

in_c3_term_and_in_c3_elis2 <- c3_term$a_number[c3_term$a_number %in% c3_elis2$a_number]
length(in_c3_term_and_in_c3_elis2)
length(unique(in_c3_term_and_in_c3_elis2))

in_c3_term_but_not_c3_elis2 <- c3_term$a_number[!(c3_term$a_number %in% c3_elis2$a_number)]
length(in_c3_term_but_not_c3_elis2)
length(unique(in_c3_term_but_not_c3_elis2))

in_c3_term_but_not_c3 <- c3_term$a_number[!(c3_term$a_number %in% c3$BEN_A_NUMBER)]
length(in_c3_term_but_not_c3)
length(unique(in_c3_term_but_not_c3))
c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_but_not_c3_elis2) %>% select(BEN_A_NUMBER, CURRENT_STATUS)
c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_but_not_c3_elis2) %>% group_by(CURRENT_STATUS) %>% count()

c3_term %>% filter(a_number %in% in_c3_term_but_not_c3_elis2) %>% data.frame()

# note there is 7 in c3_term but no c3_elis2 after filtering out case_terminated_status_acquired
c3_term %>% filter(action_text == "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% count()
c3_term %>% filter(action_text == "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% distinct(a_number) %>% count()
c3_term %>% filter(action_text == "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% distinct(a_number) %>% 
        filter(a_number %in% c3_elis2$a_number) %>% count()


in_c3_term_filtered_but_not_c3_elis2 <- c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS", 
                                                           a_number %in% in_c3_term_but_not_c3_elis2) %>%
        distinct(a_number) %>% pull(a_number)
c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_filtered_but_not_c3_elis2) %>% select(BEN_A_NUMBER, CURRENT_STATUS)

# merge c3_term to c3_elis2
glimpse(c3_elis2)
glimpse(c3_term)

# originally, we had excluded "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS", 
# but 2 of them are on SCOPS terminated file, so we'll include as c3/smart_terminated going forward
# c3_elis2 <- c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% 
#         distinct(a_number, c3_terminated) %>% left_join(c3_elis2, ., by = c("a_number" = "a_number")) %>% 
#         mutate(c3_terminated = case_when(is.na(c3_terminated) ~ 0, TRUE ~ c3_terminated))

c3_elis2 <- c3_term %>% 
        distinct(a_number, c3_terminated) %>% left_join(c3_elis2, ., by = c("a_number" = "a_number")) %>% 
        mutate(c3_terminated = case_when(is.na(c3_terminated) ~ 0, TRUE ~ c3_terminated))
glimpse(c3_elis2)

# inspect merge
c3_elis2 %>% group_by(c3_terminated) %>% count() # 13482 
c3_elis2 %>% filter(c3_terminated == 1) %>% distinct(a_number) %>% count() # 3812

c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% distinct(a_number) %>% count()
length(in_c3_term_filtered_but_not_c3_elis2)
# 7 unique anumbers in c3_term but not in c3_elis2 - all are admin_closed in c3

c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS") %>% distinct(a_number) %>% count()
elis %>% filter(benefit_status == "Terminated") %>% distinct(a_number) %>% count()
c3_elis2 %>% filter(benefit_status == "Terminated", benefit_status != "Fee Payment Issues") %>% distinct(a_number) %>% count()
3790 + 431 == 4221

# create variable for earliest and latest termination action/date
# c3_term_earliest_action <- c3_term %>% group_by(a_number) %>% arrange(action_date) %>% slice(1) %>%
#         rename(latest_term_action_date = action_date, latest_term_action_text = action_text)
# 
# # inspect earliest_action
# head(c3_term_earliest_action)
# sum(duplicated(c3_term_earliest_action$a_number))
# c3_term %>% filter(a_number == "A044533764")
# 
# # create variable for latest and latest termination action/date
# c3_term_latest_action <- c3_term %>% group_by(a_number) %>% arrange(desc(action_date)) %>% slice(1) %>% 
#         rename(earliest_term_action_date = action_date, earliest_term_action_text = action_text)
# 
# # inspect earliest_action
# head(c3_term_latest_action)
# sum(duplicated(c3_term_latest_action$a_number))
# c3_term %>% filter(a_number == "A044533764")


############################################################


# write final c3_elis2 to file
# write_csv(c3_elis2, "Data/c3_elis2.csv")
# c3_elis2 <- read_csv("Data/c3_elis2.csv")


#####################################################################


# create table of count/pct w/ ident_report by benefit status
# glimpse(c3_elis2)
# 
# # check total applications
# # note the difference in total applications is 23, which is the count of NA a_number
# 
# c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         group_by(latest_benefit_status) %>% count()
# 
# c3_elis %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         group_by(latest_benefit_status) %>% count()
# 
# 155700 - 155677 == 23
# sum(is.na(c3_elis$a_number))
# 
# 
# # check unique a_numbers
# # note the difference in count of unique a_number is due to removal of NA a_number
# 
# c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number, latest_benefit_status) %>%
#         group_by(latest_benefit_status) %>% count()
# 
# c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number) %>% count()
# 
# 770803 + 66672 + 51277 == 888752
# 
# c3_elis %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number, latest_benefit_status) %>%
#         group_by(latest_benefit_status) %>% count()
# 
# c3_elis %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number) %>% count()
# 
# 770803 + 66672 + 51278 == 888753
# 
# # check after collapsing benefit_status
# c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number, ident_report_exist, latest_benefit_status, latest_act_fy) %>% 
#         group_by(latest_benefit_status) %>% count()
# 
# 
# 
# 
# 
# # create table
# c3_elis2_latest_status_fy_table <-  c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number, ident_report_exist, latest_benefit_status, latest_act_fy) %>%
#         group_by(latest_act_fy, latest_benefit_status) %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                            pct_w_ident_report = count_w_ident_report / n())
# c3_elis2_latest_status_fy_table 
# 
# # reshape table to wide format
# c3_elis2_latest_status_fy_table <- c3_elis2_latest_status_fy_table %>% gather(key = variable, value = value, -c(latest_act_fy, latest_benefit_status)) %>%
#         unite(col = "latest_benefit_status_w_variable", c(latest_benefit_status, variable)) %>%
#         spread(key = latest_benefit_status_w_variable, value = value) %>%
#         select(latest_act_fy, Approved_total_count, Approved_count_w_ident_report, Approved_pct_w_ident_report,
#                Denied_total_count, Denied_count_w_ident_report, Denied_pct_w_ident_report,
#                Pending_total_count, Pending_count_w_ident_report, Pending_pct_w_ident_report) %>% ungroup() %>%
#         mutate(Approved_total_count = comma(Approved_total_count),
#                Approved_count_w_ident_report = comma(Approved_count_w_ident_report), 
#                Approved_pct_w_ident_report = percent(Approved_pct_w_ident_report),
#                Denied_total_count = comma(Denied_total_count),
#                Denied_count_w_ident_report = comma(Denied_count_w_ident_report), 
#                Denied_pct_w_ident_report = percent(Denied_pct_w_ident_report),
#                Pending_total_count = comma(Pending_total_count),
#                Pending_count_w_ident_report = comma(Pending_count_w_ident_report), 
#                Pending_pct_w_ident_report = percent(Pending_pct_w_ident_report))
# 
# c3_elis2_latest_status_fy_table %>% data.frame(.)
# 
# # get totals table
# c3_elis2_latest_status_totals_table <- c3_elis2 %>% filter(latest_benefit_status != "Fee Payment Issues") %>%
#         mutate(latest_benefit_status = case_when(latest_benefit_status == "Terminated" ~ "Denied", TRUE ~ latest_benefit_status)) %>% 
#         distinct(a_number, ident_report_exist, latest_benefit_status, latest_act_fy) %>%
#         group_by(latest_benefit_status) %>% summarize(total_count = n(), count_w_ident_report = sum(ident_report_exist),
#                                                       pct_w_ident_report = count_w_ident_report / n())
# 
# c3_elis2_latest_status_totals_table
# 
# c3_elis2_latest_status_totals_table <- c3_elis2_latest_status_totals_table %>% gather(key = variable, value = value, -c(latest_benefit_status)) %>%
#         unite(col = "latest_benefit_status_w_variable", c(latest_benefit_status, variable)) %>%
#         spread(key = latest_benefit_status_w_variable, value = value) %>%
#         mutate(latest_act_fy = "All FY") %>%
#         select(latest_act_fy, Approved_total_count, Approved_count_w_ident_report, Approved_pct_w_ident_report,
#                Denied_total_count, Denied_count_w_ident_report, Denied_pct_w_ident_report,
#                Pending_total_count, Pending_count_w_ident_report, Pending_pct_w_ident_report) %>% ungroup() %>%
#         mutate(Approved_total_count = comma(Approved_total_count),
#                Approved_count_w_ident_report = comma(Approved_count_w_ident_report), 
#                Approved_pct_w_ident_report = percent(Approved_pct_w_ident_report),
#                Denied_total_count = comma(Denied_total_count),
#                Denied_count_w_ident_report = comma(Denied_count_w_ident_report), 
#                Denied_pct_w_ident_report = percent(Denied_pct_w_ident_report),
#                Pending_total_count = comma(Pending_total_count),
#                Pending_count_w_ident_report = comma(Pending_count_w_ident_report), 
#                Pending_pct_w_ident_report = percent(Pending_pct_w_ident_report))
# 
# c3_elis2_latest_status_totals_table 
# 
# # combine c3_elis2_latest_status_fy_table and c3_elis2_latest_status_totals_table
# c3_elis2_latest_status_fy_totals_table <- rbind(c3_elis2_latest_status_fy_table, c3_elis2_latest_status_totals_table)
# c3_elis2_latest_status_fy_totals_table %>% data.frame()
# 
# # write file
# write_csv(c3_elis2_latest_status_fy_totals_table, "Data/merge_summary_output/c3_elis2_latest_status_fy_totals_table.csv")


####################################################################


# load daca_charges
daca_charges1 <- read_csv("Data/daca_charges_part1.csv")
daca_charges2 <- read_csv("Data/daca_charges_part2.csv")
daca_charges3 <- read_csv("Data/daca_charges_part3.csv")
daca_charges4 <- read_csv("Data/daca_charges_part4.csv")
daca_charges5 <- read_csv("Data/daca_charges_part5.csv")
daca_charges6 <- read_csv("Data/daca_charges_part6.csv")
daca_charges7 <- read_csv("Data/daca_charges_part7.csv")
daca_charges8 <- read_csv("Data/daca_charges_part8.csv")
daca_charges9 <- read_csv("Data/daca_charges_part9.csv")
daca_charges10 <- read_csv("Data/daca_charges_part10.csv")
daca_charges <- rbind(daca_charges1, daca_charges2, daca_charges3, daca_charges4, daca_charges5, daca_charges6, daca_charges7, daca_charges8, daca_charges9, daca_charges10)

# inspect
glimpse(daca_charges)
daca_charges %>% distinct(anum) %>% count()
daca_charges %>% data.frame() %>% head()

# inspect charge_type
daca_charges %>% filter(charge_type == "deport_alien_nonimmigrant_overstay_ewi_removal") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "terrorist") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame() %>% head(50)
daca_charges %>% filter(charge_type == "rape") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame()
daca_charges %>% filter(charge_type == "assault") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "gang") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "weapon") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "hit_and_run") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% head(20)
daca_charges %>% filter(charge_type == "homicide_murder_att_murder") %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame()


#######################################################################


# fix issues with current version of daca_charges
# issues are based on decisions made after daca.R was run to match keylines with keywords
# but since running daca.R is time-consuming, i'm just updated the output manually here
# note that i did update daca.R to reflect the fixes though, so if it's ever run again, the fixes below likely won't be necessary/or work


# fix issue with vehicle theft
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance", str_detect(keyline, regex("theft", ignore_case = TRUE))) %>%
        select(anum, keyline, charge_type)
veh_theft_a_numbers <- daca_charges %>% 
        filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance", str_detect(keyline, regex("theft", ignore_case = TRUE)))
veh_theft_a_numbers %>% distinct(anum) %>% count() # 440
daca_charges %>% filter(anum %in% veh_theft_a_numbers$anum) %>% distinct(charge_type) # 58

daca_charges %>% filter(charge_type == "theft_larceny") %>% distinct(anum) %>% count() # 10717
daca_charges %>% filter(anum %in% veh_theft_a_numbers$anum, charge_type == "theft_larceny") %>% distinct(anum) %>% count() # 125

daca_charges <- daca_charges %>% 
        mutate(charge_type = case_when(charge_type == "traffic_speed_moving_vehic_driving_license_insurance" & str_detect(keyline, regex("theft", ignore_case = TRUE)) ~
                                               "theft_larceny", TRUE ~ charge_type))
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance", str_detect(keyline, regex("theft", ignore_case = TRUE))) %>%
        distinct(anum) %>% count() # 0
daca_charges %>% filter(charge_type == "theft_larceny") %>% distinct(anum) %>% count() # 11032
11032 - 10717 + 125 == 440
daca_charges %>% filter(anum %in% veh_theft_a_numbers$anum) %>% distinct(charge_type) # 58


####################################################


# fix issue with traffic_speed_moving_vehic_driving_license_insurance_dui
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance_dui") %>% distinct(anum) %>% count() # 215
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance") %>% distinct(anum) %>% count() # 36307
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance_dui") %>% distinct(keyline)

driving_w_dui_a_numbers <- daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance_dui")
driving_w_dui_a_numbers %>% distinct(anum) %>% count() # 215
driving_w_dui_a_numbers %>% distinct(charge_type)
daca_charges %>% filter(anum %in% driving_w_dui_a_numbers$anum) %>% distinct(charge_type) # 27
daca_charges %>% filter(anum %in% driving_w_dui_a_numbers$anum, charge_type == "traffic_speed_moving_vehic_driving_license_insurance") %>% distinct(anum) %>% count() # 199

daca_charges <- daca_charges %>% 
        mutate(charge_type = case_when(charge_type == "traffic_speed_moving_vehic_driving_license_insurance_dui" ~ "traffic_speed_moving_vehic_driving_license_insurance", 
                                       TRUE ~ charge_type)) 
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance_dui") %>% distinct(anum) %>% count() # 0
daca_charges %>% filter(charge_type == "traffic_speed_moving_vehic_driving_license_insurance") %>% distinct(anum) %>% count() # 36323
daca_charges %>% filter(anum %in% driving_w_dui_a_numbers$anum) %>% distinct(charge_type) # 26
36323 - 36307 + 199 == 215


###############################################


# fix issue with health_and_safety - collapse health_and_safety into NA charge_type
daca_charges %>% filter(charge_type == "health_and_safety") %>% distinct(anum) %>% count() # 177
daca_charges %>% filter(is.na(charge_type)) %>% distinct(anum) %>% count() # 10190
health_safety_a_numbers <- daca_charges %>% filter(charge_type == "health_and_safety")
health_safety_a_numbers %>% distinct(anum) %>% count() # 177
daca_charges %>% filter(anum %in% health_safety_a_numbers$anum) %>% distinct(charge_type) # 32
daca_charges %>% filter(anum %in% health_safety_a_numbers$anum, is.na(charge_type)) %>% distinct(anum) %>% count() # 13
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "health_and_safety" ~ NA_character_, TRUE ~ charge_type))
daca_charges %>% filter(charge_type == "health_and_safety") %>% distinct(anum) %>% count() # 0
daca_charges %>% filter(is.na(charge_type)) %>% distinct(anum) %>% count() # 10354
daca_charges %>% filter(anum %in% health_safety_a_numbers$anum) %>% distinct(charge_type) # 31
10354 - 10190 + 13 == 177


##############################################


# fix issue with rape - collapse statutory rape into sexual abuse
# need to use the confirmed filter where charge_type == "rape" due to keylines with STATUTE being ignored
daca_charges %>% filter(charge_type == "rape") %>% distinct(anum) %>% count() # 249
daca_charges %>% filter(charge_type == "sexual_contact_with_minor") %>% distinct(anum) %>% count() # 275

statutory_rape_a_numbers <- daca_charges %>% 
        filter(str_detect(keyline, regex("stat rape|statutory rape|RAPE OF A CHILD|RAPE OF CHILD", ignore_case = TRUE)),
               charge_type == "rape") %>%
        distinct(anum)
statutory_rape_a_numbers %>%  distinct(anum) %>% count() # 81

daca_charges %>% filter(charge_type == "sexual_contact_with_minor" & anum %in% statutory_rape_a_numbers$anum) %>% distinct(anum) %>% count() # 15

daca_charges %>% 
        filter(str_detect(keyline, regex("stat rape|statutory rape|RAPE OF A CHILD|RAPE OF CHILD", ignore_case = TRUE)),
               !(anum %in% statutory_rape_a_numbers$anum)) %>% select(anum, keydate, keyline, charge_type)

daca_charges %>% filter(charge_type == "rape" & anum %in% statutory_rape_a_numbers$anum) %>% distinct(anum) %>% count() # 81
daca_charges <- daca_charges %>% 
        mutate(charge_type = case_when(charge_type == "rape" & anum %in% statutory_rape_a_numbers$anum ~ "sexual_contact_with_minor",
                                       TRUE ~ charge_type))
daca_charges %>% filter(charge_type == "sexual_contact_with_minor") %>% distinct(anum) %>% count() # 341
daca_charges %>% filter(charge_type == "rape") %>% distinct(anum) %>% count() # 168
249 - 168 == 81
341 - 275 + 15 == 81


##############################################


# fix issue with gang - split out organized_criminal_activity 
daca_charges %>% filter(charge_type == "gang") %>% select(anum, keyline, charge_type) # 302

# count org crime
daca_charges %>% filter(charge_type == "gang", 
                        str_detect(string = keyline, 
                                   pattern = regex("ORGANIZED CRIMINAL ACTIV|organized crim|org crime", ignore_case = TRUE))) %>% 
        select(anum, keyline, charge_type) # 134

# count gang
daca_charges %>% filter(charge_type == "gang", 
                        str_detect(string = keyline, 
                                   pattern = regex("ST GANG|STREET GANG", ignore_case = TRUE))) %>% 
        select(anum, keyline, charge_type) # 168

# count org_crime and gang
daca_charges %>% filter(charge_type == "gang", 
                        str_detect(string = keyline, 
                                   pattern = regex("ORGANIZED CRIMINAL ACTIV|organized crim|org crime", ignore_case = TRUE)),
                        str_detect(string = keyline, 
                                   pattern = regex("ST GANG|STREET GANG", ignore_case = TRUE))) %>% 
        select(anum, keyline, charge_type) # 0

134 + 168 == 302
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "gang" & 
                str_detect(string = keyline, pattern = regex("ORGANIZED CRIMINAL ACTIV|organized crim|org crime", 
                ignore_case = TRUE)) ~ "organized_criminal_activity", TRUE ~ charge_type))

daca_charges %>% filter(charge_type == "gang") %>% nrow() # 168
daca_charges %>% filter(charge_type == "organized_criminal_activity") %>% nrow() # 134


##############################################################


# fix issue with murders (negligent murders, assault to murder/attempted to murder)
daca_charges %>% filter(charge_type == "homicide_murder_att_murder") %>% count(keyline) %>% arrange(desc(n)) %>% data.frame()

# count homicide_murder_att_murder
daca_charges %>% filter(charge_type == "homicide_murder_att_murder") %>% nrow() # 148

# count attempted murder/assault to murder and pre-existing assault
daca_charges %>% filter(charge_type == "homicide_murder_att_murder", 
                        str_detect(string = keyline, pattern = regex("att|BURGLARY: ENTER BLDG TO MURDER/RAPE/ETC|DRIVE BY SHOOTING|ASSAULT TO MURDER|ASLT TO MURDER|ASSAULT WITH INTENT TO MURDER", ignore_case = TRUE))) %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 36

daca_charges %>% filter(charge_type == "assault") %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 9232

# reassign charge_type for attempted_murder/assault to murder as assault
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "homicide_murder_att_murder" & 
        str_detect(string = keyline, pattern = regex("att|BURGLARY: ENTER BLDG TO MURDER/RAPE/ETC|DRIVE BY SHOOTING|ASSAULT TO MURDER|ASLT TO MURDER|ASSAULT WITH INTENT TO MURDER", ignore_case = TRUE)) ~ "assault", TRUE ~ charge_type)) 

# inspect attempted_murder
daca_charges %>% filter(charge_type == "assault") %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 9268

36 + 9232 == 9268


##################


# count negligent homicide and pre-existing manslaughter
daca_charges %>% filter(charge_type == "homicide_murder_att_murder", 
                        str_detect(string = keyline, pattern = regex("neg|mansl", ignore_case = TRUE))) %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 10

daca_charges %>% filter(charge_type == "manslaughter") %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 16

# reassign negligent homicide as manslaugter
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "homicide_murder_att_murder" & 
                str_detect(string = keyline, pattern = regex("neg|mansl", ignore_case = TRUE)) ~ "manslaughter", TRUE ~ charge_type))

# inspect manslaughter
daca_charges %>% filter(charge_type == "manslaughter") %>%
        count(keyline) %>% arrange(desc(n)) %>% summarize(count = sum(n)) # 26

16 + 10 == 26


################


# count homicide_murder_att_murder
daca_charges %>% filter(charge_type == "homicide_murder_att_murder") %>% nrow() # 102
148 - 36 - 10 == 102

# rename homicide_murder_att_murder to be murder
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "homicide_murder_att_murder" ~ "murder", 
                                                                TRUE ~ charge_type))

# inspect homicide_murder_att_murder and murder
daca_charges %>% filter(charge_type == "homicide_murder_att_murder") %>% nrow() # 0
daca_charges %>% filter(charge_type == "murder") %>% nrow() # 102

# final inspection of murder
daca_charges %>% filter(charge_type == "murder") %>% count(keyline) %>% arrange(desc(n)) %>% data.frame()


############################################################


# fix issues with arson

# inspect all charge_type arson
daca_charges %>% filter(charge_type == "arson") %>% count(keyline) %>% arrange(desc(n)) %>% data.frame()
daca_charges %>% filter(charge_type == "arson") %>% nrow() # 124
daca_charges %>% filter(charge_type == "arson") %>% distinct(anum) %>% nrow() # 75

# inspect all charge_type arson without the word arson
daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% select(keyline, charge_type) %>%
        data.frame()
daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% nrow() # 23
daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% distinct(anum) %>% nrow() # 14

# find count of anumbers with multiple charge type arson where at least one contains "arson" and at least one does not, so they show up in both anum counts above
arson_anumbers <- daca_charges %>% filter(charge_type == "arson", str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE))) %>% distinct(anum)
non_arson_anumbers <- daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% distinct(anum)
arson_anumbers %>% inner_join(., non_arson_anumbers) # 4

# change arson charge_type to NA if "arson" is not in keyline
daca_charges <- daca_charges %>% mutate(charge_type = case_when(charge_type == "arson" & 
                                        !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE))) ~ NA_character_,
                                        TRUE ~ charge_type))

# inspect results of change
daca_charges %>% filter(charge_type == "arson") %>% nrow() # 101
daca_charges %>% filter(charge_type == "arson") %>% distinct(anum) %>% nrow() # 65
daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% nrow() # 0
daca_charges %>% filter(charge_type == "arson", !(str_detect(string = keyline, pattern = regex("arson", ignore_case = TRUE)))) %>% distinct(anum) %>% nrow() # 0

124 - 23 == 101
75 - 14 + 4 == 65


#############################################################


# write daca_charges
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("daca_charges", "_", current_date, ".csv")
filename
write_csv(daca_charges, filename)

# get a_numbers where no charge could be extracted
# note this previous code is incorrect, since it only counts anum with just "keyline_does_not_list_specific_charge",
# but will exclude anum with "keyline_does_not_list_specific_charge" and an NA value from a keyline that did not match any keywords

# daca_charges_anum_count <- daca_charges %>% group_by(anum) %>% count()
# daca_charges_anum_w_single_keyline <- daca_charges_anum_count %>% filter(n == 1) %>% pull(anum)
# length(daca_charges_anum_w_single_keyline)
# daca_charges %>% filter(anum %in% daca_charges_anum_w_single_keyline) %>% distinct(keyline)


###########################################################


# create daca_charges_w_only_nonspecific_charge for use as flag later in processing c3_elis2
daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge") %>% distinct(anum) %>% count() # 89802
all_daca_charges_anum_wo_nonspecific_charge <- daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge")
daca_charges_w_only_nonspecific_charge <- daca_charges[!(daca_charges$anum %in% all_daca_charges_anum_wo_nonspecific_charge$anum), ]
daca_charges_w_only_nonspecific_charge %>% distinct(anum) %>% count() # 11756



###########################################################


# inspect daca_charges


daca_charges %>% filter(anum %in% c3_elis2$a_number) %>% distinct(anum) %>% count() # 98082
daca_charges_w_only_nonspecific_charge %>% filter(anum %in% c3_elis2$a_number) %>% distinct(anum) %>% count() # 10681

daca_charges_w_only_nonspecific_charge %>% distinct(charge_type) 
daca_charges_w_only_nonspecific_charge %>% filter(anum %in% all_daca_charges_anum_wo_nonspecific_charge$anum)
all_daca_charges_anum_wo_nonspecific_charge %>% filter(anum %in% daca_charges_w_only_nonspecific_charge$anum)
11756 + 89802 == 101558

# note there are 3466 unique anumbers in daca_charges but not in c3_elis2 - could be because of filters on admin_closed, etc
length(unique(daca_charges$anum[!(daca_charges$anum %in% c3_elis2$a_number)]))
daca_charges_not_in_c3_elis2 <- unique(daca_charges$anum[!(daca_charges$anum %in% c3_elis2$a_number)])
length(daca_charges_not_in_c3_elis2)
length(unique(c3_elis2$a_number[!(c3_elis2$a_number %in% daca_charges$anum)]))

# check for 3466 a_numbers in daca_charges but not c3_elis2 to see if they're in original c3/elis files
# 3428 of 3466 are not in c3_elis_original_a_numbers
c3_original_a_numbers <- c3 %>% distinct(BEN_A_NUMBER) %>% pull(BEN_A_NUMBER)
elis_original_a_numbers <- elis %>% distinct(a_number) %>% pull(a_number)
c3_elis_original_a_numbers <- c(c3_original_a_numbers, elis_original_a_numbers)
length(unique(c3_elis_original_a_numbers))
length(unique(daca_charges$anum[!(daca_charges$anum %in% c3_elis_original_a_numbers)]))
daca_charges_not_in_c3_elis_original <- unique(daca_charges$anum[!(daca_charges$anum %in% c3_elis_original_a_numbers)])
length(daca_charges_not_in_c3_elis_original)

# inspect the 38 a_numbers in daca_charges and in c3_original but not in c3_elis2  - all are ADMIN_CLOSED
3466-3428
length(daca_charges_not_in_c3_elis_original)
length(daca_charges_not_in_c3_elis2)
daca_charges_in_c3_elis_original_not_in_c3_elis2 <- unique(daca_charges_not_in_c3_elis2[!(daca_charges_not_in_c3_elis2 %in% daca_charges_not_in_c3_elis_original)])
length(daca_charges_in_c3_elis_original_not_in_c3_elis2)
c3 %>% filter(BEN_A_NUMBER %in% daca_charges_in_c3_elis_original_not_in_c3_elis2) %>% distinct(CURRENT_STATUS)


########################################################


# clean daca_charges for merging with c3_elis2 to create c3_elis3

# pivot daca_charges to get charge_type values as variables
daca_charges_pivot <- daca_charges %>% select(anum, charge_type) %>% mutate(charge_type_flag = 1) %>% distinct(anum, charge_type, charge_type_flag) %>% 
        spread(key = charge_type, value = charge_type_flag)

daca_charges_pivot %>% distinct(anum) %>% count() # 101558
glimpse(daca_charges_pivot)


# create lookup tables of a_number latest_charge_date and earliest_charge_date
daca_charges_earliest_keydate <- daca_charges %>% group_by(anum) %>% arrange(keydate) %>% slice(1) %>% select(anum, keydate) %>% ungroup() %>% 
        rename(earliest_keydate = keydate)
glimpse(daca_charges_earliest_keydate)
head(daca_charges_earliest_keydate)
sum(is.na(daca_charges_earliest_keydate$earliest_keydate)) # 3 NAs
daca_charges %>% filter(anum == "A016016947")

daca_charges_latest_keydate <- daca_charges %>% group_by(anum) %>% arrange(desc(keydate)) %>% slice(1) %>% select(anum, keydate) %>% ungroup() %>% 
        rename(latest_keydate = keydate)
glimpse(daca_charges_latest_keydate)
head(daca_charges_latest_keydate)
sum(is.na(daca_charges_latest_keydate$latest_keydate)) # 3 NAs
daca_charges %>% filter(anum == "A016016947")

# convert NA values of charge_type dummies created automatically by spread into zeros
sum(daca_charges_pivot$homicide_murder_att_murder, na.rm = TRUE)
daca_charges_pivot <- daca_charges_pivot %>% mutate_if(.predicate = ~ any(is.na(.x)), .funs = ~ ifelse(is.na(.x), 0, .x))
daca_charges_pivot
# sum(daca_charges_pivot$homicide_murder_att_murder, na.rm = TRUE)

# rename the NA charge_type variable
daca_charges_pivot <- daca_charges_pivot %>% rename(NA_charge_type = `<NA>`)
glimpse(daca_charges_pivot)

# create daca_charge_flag
daca_charges_pivot <- daca_charges_pivot %>% mutate(daca_charge_flag = 1)

# inspect a_numbers
daca_charges_pivot %>% distinct(a_number) %>% count() # 101558
daca_charges %>% distinct(anum) %>% count() # 101558
length(unique(daca_charges$anum[daca_charges$anum %in% c3_elis2$a_number]))

# merge daca_charges_pivot to c3_elis2
glimpse(c3_elis2)
c3_elis2 <- c3_elis2 %>% left_join(., daca_charges_pivot, by = c("a_number" = "anum"))
glimpse(c3_elis2)

# convert NA to zero for c3_elis2 records without values for charge_type_variables
charge_type_variables <- names(daca_charges_pivot)[-1]
charge_type_variables
c3_elis2 <- c3_elis2 %>% mutate_at(.vars = vars(charge_type_variables), .funs = ~ ifelse(is.na(.x), 0, .x))
glimpse(c3_elis2)

# merge daca_charges_earliest/latest_keydate to c3_elis2
glimpse(c3_elis2)
c3_elis2 <- c3_elis2 %>% left_join(., daca_charges_earliest_keydate, by = c("a_number" = "anum"))
glimpse(c3_elis2)

glimpse(c3_elis2)
c3_elis2 <- c3_elis2 %>% left_join(., daca_charges_latest_keydate, by = c("a_number" = "anum"))
glimpse(c3_elis2)

# create earliest/latest_keydate FY
c3_elis2 <- c3_elis2 %>% mutate(earliest_keydate_fy_qtr = quarter(earliest_keydate, fiscal_start = 10, with_year = TRUE)) %>%
        separate(col = earliest_keydate_fy_qtr, into = c("earliest_keydate_fy", "earliest_keydate_qtr"), sep = "\\.", remove = FALSE) 

c3_elis2 <- c3_elis2 %>% mutate(latest_keydate_fy_qtr = quarter(latest_keydate, fiscal_start = 10, with_year = TRUE)) %>%
        separate(col = latest_keydate_fy_qtr, into = c("latest_keydate_fy", "latest_keydate_qtr"), sep = "\\.", remove = FALSE) 

# check rec_fy and act_fy
c3_elis2 %>% filter(earliest_keydate_qtr == 4) %>% select(earliest_keydate, earliest_keydate_fy, earliest_keydate_qtr)
c3_elis2 %>% filter(earliest_keydate_qtr == 1) %>% select(earliest_keydate, earliest_keydate_fy, earliest_keydate_qtr)

c3_elis2 %>% filter(latest_keydate_qtr == 4) %>% select(latest_keydate, latest_keydate_fy, latest_keydate_qtr)
c3_elis2 %>% filter(latest_keydate_qtr == 1) %>% select(latest_keydate, latest_keydate_fy, latest_keydate_qtr)

# convert NA values of daca_charges_flag to 0
c3_elis2 <- c3_elis2 %>% mutate(daca_charge_flag = ifelse(is.na(daca_charge_flag), 0, daca_charge_flag))
c3_elis2 %>% group_by(daca_charge_flag) %>% count()
c3_elis2 %>% distinct(a_number, daca_charge_flag) %>% group_by(daca_charge_flag) %>% count()

# collapse traffic_speed_moving_vehic_driving_license_insurance_dui flag into traffic_speed_moving_vehic_driving_license_insurance
# c3_elis2 %>% group_by(traffic_speed_moving_vehic_driving_license_insurance_dui) %>% count()
# c3_elis2 %>% group_by(traffic_speed_moving_vehic_driving_license_insurance) %>% count()
# c3_elis2 %>% filter(traffic_speed_moving_vehic_driving_license_insurance == 0, 
#                                           traffic_speed_moving_vehic_driving_license_insurance_dui == 1) %>% count()
# 
# c3_elis2 <- c3_elis2 %>% 
#         mutate(traffic_speed_moving_vehic_driving_license_insurance = case_when(traffic_speed_moving_vehic_driving_license_insurance_dui == 1 ~ 1,
#                                                                                 TRUE ~ traffic_speed_moving_vehic_driving_license_insurance))
# c3_elis2 %>% group_by(traffic_speed_moving_vehic_driving_license_insurance) %>% count()
# 124069 - 124012 == 57

# collapse health_and_safety into NA charge_type
# c3_elis2 %>% group_by(health_and_safety) %>% count()
# c3_elis2 %>% group_by(NA_charge_type) %>% count()
# c3_elis2 %>% filter(health_and_safety == 1, NA_charge_type == 1) %>% count()
# c3_elis2 <- c3_elis2 %>% mutate(NA_charge_type = ifelse(health_and_safety == 1, 1, NA_charge_type))
# 29700 - 29113 + 42

# add flag for unable_to_extract_charge
c3_elis2 <- c3_elis2 %>% mutate(unable_to_extract_charge = ifelse(a_number %in% daca_charges_w_only_nonspecific_charge$anum, 1, 0))
c3_elis2 %>% filter(unable_to_extract_charge == 1) %>% distinct(a_number) %>% count() # 10681
daca_charges_w_only_nonspecific_charge %>% distinct(anum) %>% count() # 11756
daca_charges_w_only_nonspecific_charge %>% filter(anum %in% c3_elis2$a_number) %>% distinct(anum) %>% count() # 10681





# inspect charge_type
# charge_type_count <- c3_elis2 %>% 
#         filter(daca_charge_flag == 1) %>% select(a_number, charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1) %>% group_by(charge_type) %>% count() %>% arrange(desc(n)) %>% ungroup()
# charge_type_count
# charge_type_count %>% filter(charge_type %in% c("intimidating_witness", "undefined_treat_attempted_crime_conspiracy",
#                                                 "offensive_touching", "sex_abuse_unlawful", "sex_assault",
#                                                 "sexual_contact_with_minor"))

# collapse charge_type
c3_elis2 %>% 
        mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
               `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
               `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
               `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
                                                                          inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
               `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
                                           electronic_communication_device == 1 ~ 1, TRUE ~ 0),
               `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
                                                                                 human_trafficking == 1 ~ 1, TRUE ~ 0),
               `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
                                                                              perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
               `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
                                                                                  lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
               `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
                                                                                        undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
               `Sexual abuse, statutory rape, etc` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
                                                                       sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
               `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
                                                              sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
               `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
                                                                                  stalking == 1 ~ 1, TRUE ~ 0),
               `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
                                                                    unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
               `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
               `Undefined ordinance` = case_when(undefined_ordinance == 1 ~ 1, TRUE ~ 0)) %>% 
        rename(Murder = murder, `Manslaughter/negligent homicide` = manslaughter, Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
               `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
               Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
               `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
               `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
               `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
               `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
               Gambling = gambling, Gang = gang, `Organized criminal activity` = organized_criminal_activity,
               `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
               Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
               `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, `Rape` = rape,
               `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
               `Resisting/interfering/evading police, etc` = resisting_interference_evading,
               `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
               `Theft, larceny, etc` = theft_larceny, 
               `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon,
               `Undefined` = undefined_charge_literal)
        
# create vector of collapsed_charge_type_variables
collapsed_charge_type_variables <- c("Accessory, accomplice, hindering, etc", "Contempt, bench warrant, bail, etc",
                                     "Battery", "Offenses against family and children", "Other", "Undefined", "Kidnapping, trafficking, false imprisonment",
                                     "Murder", "Manslaughter/negligent homicide", "Fraud, money-laundering, corruption, etc",
                                     "Indecent exposure, lewd/lascivious acts, etc", 
                                     "Undefined threats, attempted crime/conspiracy, etc", "Sexual abuse, statutory rape, etc", 
                                     "Illegal sex-related acts",
                                     "Harassment, restraining order violation, etc", "Tresspass, unlawful entry, etc",
                                     "Undefined juvenile", "Undefined ordinance",
                                     "Arson", "Assault", "Burglary", "Child pornography", "Contributing to the delinquency of a minor",
                                     "Vandalism", "Immigration-related", "Disorderly conduct", "Drug-related", "Driving under the influence",
                                     "Embezzlement", "Failure to appear", "Fail to comply/obey, etc", "Forgery, counterfeiting, etc", "Obstruction, fabrication, false claim, etc",
                                     "Gambling", "Gang", "Organized criminal activity", "Hit and Run", "Liquor-related", "Loitering", "Motor vehicle theft",
                                     "Unable to extract any charge", "Probation/parole/curfew violation, etc", "Rape", 
                                     "Reckless conduct/endangerment, etc", 
                                     "Resisting/interfering/evading police, etc", "Riot, unlawful assembly, etc", "Robbery", "Stolen property",
                                     "Theft, larceny, etc", "Other driving-related", "Weapon-related")
length(unique(collapsed_charge_type_variables)) # 51
collapsed_charge_type_variables[duplicated(collapsed_charge_type_variables)]


#############################################################################

# 
# # create charge_type freq table
# charge_type_variables <- names(daca_charges_pivot)[-1]
# charge_type_variables <- charge_type_variables[!(charge_type_variables == "traffic_speed_moving_vehic_driving_license_insurance_dui")]
# c3_elis2 %>% filter(a_number == "A016016947") %>% 
#         select(a_number, rec_date, act_date, benefit_status)
# daca_charges %>% filter(anum == "A016016947") %>% data.frame()
# 
# 
# glimpse(c3_elis2)
# 
# # check for multiple a_number records for a given rec_fy
# a_number_rec_fy_count <- c3_elis2 %>% filter(daca_charge_flag == 1) %>% select(a_number, rec_fy, charge_type_variables) %>% 
#         group_by(a_number, rec_fy) %>% count() %>% arrange(desc(n))
# a_number_rec_fy_count   
# c3_elis2 %>% filter(a_number == "A206050012") %>% select(a_number, rec_date, benefit_status)
# 
# # w rec_fy
# charge_type_by_rec_fy_table <- c3_elis2 %>% filter(daca_charge_flag == 1) %>% select(a_number, rec_fy, charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(charge_type_variables)) %>% distinct(a_number, rec_fy, charge_type, value) %>%
#         filter(value == 1) %>% group_by(rec_fy, charge_type) %>% count() %>% ungroup() %>%
#         spread(key = rec_fy, value = n)
# 
# # remove health_and_safety charge because it's a court statement for drug charge
# daca_raw %>% filter(a_number == "A204424162") %>% do(cat(.$ident_report)) # in part 1
# charge_type_by_rec_fy_table <- charge_type_by_rec_fy_table %>% 
#         filter(!(charge_type %in% c("health_and_safety", "keyline_does_not_list_specific_charge")))
# glimpse(charge_type_by_rec_fy_table)
# 
# # w act_fy
# # charge_type_by_act_fy_table <- c3_elis2 %>% filter(daca_charge_flag == 1) %>% select(a_number, act_fy, charge_type_variables) %>%
# #         gather(key = charge_type, value = value, c(charge_type_variables)) %>% distinct(a_number, act_fy, charge_type, value) %>%
# #         filter(value == 1) %>% group_by(act_fy, charge_type) %>% count() %>% ungroup() %>%
# #         spread(key = act_fy, value = n)
# 
# # clean version
# collapsed_charge_type_by_rec_fy_table <- c3_elis2 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                Battery = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                Other = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | undefined_charge_literal == 1 | 
#                                          electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                   sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, Rape = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon) %>%
#         gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, rec_fy, charge_type, value) %>%
#         filter(value == 1) %>% group_by(rec_fy, charge_type) %>% count() %>% ungroup() %>%
#         spread(key = rec_fy, value = n) %>% arrange(desc(`2012`)) %>% rename(`Charge type` = charge_type) %>% 
#         mutate_at(.vars = vars(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), .funs = ~ comma(.x))
# 
# collapsed_charge_type_by_rec_fy_table
# 
# # write table
# write_csv(collapsed_charge_type_by_rec_fy_table, "Data/collapsed_charge_type_by_rec_fy_table.csv")


#################################################################


# inspect c3_elis2 and create ever_smart_terminated variable

# inspect ident_report_exist and daca_charge_flag
c3_elis2 %>% distinct(a_number, ident_report_exist) %>% group_by(ident_report_exist) %>% count()
c3_elis2 %>% distinct(a_number, daca_charge_flag) %>% group_by(daca_charge_flag) %>% count()
c3_elis2 %>% filter(ident_report_exist == 1, daca_charge_flag == 0) %>% distinct(a_number) %>% count()
# note there are 2 unique anum with ident_report_exist = 0, but daca_charge_flag = 1?? maybe reid corrected anum from daca_raw
c3_elis2 %>% filter(ident_report_exist == 0, daca_charge_flag == 1) %>% distinct(a_number) %>% count()
c3_elis2 %>% filter(ident_report_exist == 0, daca_charge_flag == 1) %>% 
        select(a_number, benefit_status, ident_report_exist, daca_charge_flag)

# create smart_terminated variable
c3_elis2 %>% group_by(benefit_status) %>% count()
c3_elis2 %>% group_by(c3_terminated) %>% count()
c3_elis2 <- c3_elis2 %>% mutate(smart_terminated = case_when(benefit_status == "Terminated" | c3_terminated == 1 ~ 1, TRUE ~ 0))

# inspect
# note there are 24 anumbers with both a c3_terminated and smart_terminated, which is legit
c3_elis2 %>% group_by(smart_terminated) %>% count()
c3_elis2 %>% filter(c3_terminated == 1, benefit_status == "Terminated") %>% 
        select(a_number, benefit_status, database, c3_terminated, smart_terminated) %>% data.frame()
c3_elis2 %>% filter(c3_terminated == 1, benefit_status == "Terminated") %>% 
        select(a_number, benefit_status, database, c3_terminated, smart_terminated) %>% distinct(a_number) %>% count()
c3_term_and_elis_term_a_numbers <- c3_elis2 %>% filter(c3_terminated == 1, benefit_status == "Terminated") %>% 
        select(a_number, benefit_status, database, c3_terminated, smart_terminated) %>% distinct(a_number) %>% 
        distinct(a_number) %>% pull(a_number)
c3_elis2 %>% filter(a_number %in% c3_term_and_elis_term_a_numbers) %>% 
        select(a_number, benefit_status, database, c3_terminated, smart_terminated) %>% arrange(a_number)

# create ever_smart_terminated flag
smart_terminated_a_numbers <- c3_elis2 %>% filter(smart_terminated == 1) %>% pull(a_number)
length(smart_terminated_a_numbers)
length(unique(smart_terminated_a_numbers))

c3_elis2 <- c3_elis2 %>% mutate(ever_smart_terminated = ifelse(a_number %in% smart_terminated_a_numbers, 1, 0))
glimpse(c3_elis2)

# inspect
c3_elis2 %>% filter(ever_smart_terminated == 1) %>% distinct(a_number) %>% count() # 4219
c3_elis2 %>% filter(c3_terminated == 1, benefit_status == "Terminated") %>% 
        select(a_number, benefit_status, database, c3_terminated, smart_terminated) %>% distinct(a_number) %>% count() # 24

c3_elis2 %>% filter(c3_terminated == 1) %>% distinct(a_number) %>% count() # 3812
c3_term %>% distinct(a_number) %>% count() # 3835
c3_term %>% filter(a_number %in% c3_elis2$a_number) %>% distinct(a_number) %>% count() # 3812
elis %>% filter(benefit_status == "Terminated") %>% distinct(a_number) %>% count() # 431
c3_elis2 %>% filter(benefit_status == "Terminated") %>% distinct(a_number) %>% count() # 431
3812 + 431 - 24 == 4219




# 23 unique anumbers in c3_term but not in c3_elis2
# but all are in c3, and were dropped because ADMIN_CLOSED
in_c3_term_but_not_c3_elis2 <- c3_term$a_number[!(c3_term$a_number %in% c3_elis2$a_number)]
length(in_c3_term_but_not_c3_elis2)
length(unique(in_c3_term_but_not_c3_elis2))

in_c3_term_but_not_c3 <- c3_term$a_number[!(c3_term$a_number %in% c3$BEN_A_NUMBER)]
length(in_c3_term_but_not_c3)
length(unique(in_c3_term_but_not_c3))
c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_but_not_c3_elis2) %>% select(BEN_A_NUMBER, CURRENT_STATUS)
c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_but_not_c3_elis2) %>% select(BEN_A_NUMBER, CURRENT_STATUS) %>% distinct(CURRENT_STATUS)

c3_term %>% filter(a_number %in% in_c3_term_but_not_c3_elis2) %>%
        distinct(a_number) %>% count()

# note there is 7 in c3_term but not c3_elis2 after filtering out case_terminated_status_acquired - all ADMIN_CLOSED
# in_c3_term_filtered_but_not_c3_elis2 <- c3_term %>% filter(action_text != "CASE TERMINATED; STATUS ACQUIRED THROUGH OTHER MEANS", a_number %in% in_c3_term_but_not_c3_elis2) %>%
#         distinct(a_number) %>% pull(a_number)
# c3 %>% filter(BEN_A_NUMBER %in% in_c3_term_filtered_but_not_c3_elis2) %>% select(BEN_A_NUMBER, CURRENT_STATUS)



##################################################################


# write final c3_elis2 to file as c3_elis3
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("c3_elis3", "_", current_date, ".csv")
filename
write_csv(c3_elis2, filename)
# c3_elis3 <- read_csv("Data/c3_elis3_DATE.csv")


#####################################################################


# # create daca_charges_pct_table
# glimpse(c3_elis3)
# 
# # get a_number counts for denominators in sub-populations of interest
# count_daca_charge_flag_a_numbers <- c3_elis3 %>% filter(daca_charge_flag == 1) %>% distinct(a_number) %>% count() %>% pull(n)
# count_ever_denied_a_numbers <- c3_elis3 %>% filter(benefit_status == "Denied") %>% distinct(a_number) %>% count() %>% pull(n)
# count_ever_smart_terminated_a_numbers <- c3_elis3 %>% filter(ever_smart_terminated == 1) %>% distinct(a_number) %>% count() %>% pull(n)
# count_scops_terminated_a_numbers <- c3_elis3 %>% filter(scops_terminated == 1) %>% distinct(a_number) %>% count() %>% pull(n)
# count_all_daca_a_numbers <- c3_elis3 %>% distinct(a_number) %>% count() %>% pull(n)
# 
# # # build a_number_charge_table
# # a_number_charge_table <- c3_elis3 %>% filter(daca_charge_flag == 1) %>% select(a_number, charge_type_variables) %>%
# #         gather(key = charge_type, value = value, c(charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
# #         filter(value == 1)
# # a_number_charge_table
# 
# # build ident_charge_count_table
# ident_charge_count_table <- c3_elis3 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse, statutory rape, etc` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                                        sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_ordinance == 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, `Rape` = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon,
#                `Undefined` = undefined_charge_literal) %>%
#         filter(daca_charge_flag == 1) %>% select(a_number, collapsed_charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()
# 
# # build count_pct table for ident
# ident_count_pct <- ident_charge_count_table %>% mutate(count_ident = n, pct_ident = count_ident / count_daca_charge_flag_a_numbers) %>%
#         select(-n)
# ident_count_pct
# dim(ident_count_pct)
# 
# # build smart_charge_count_table
# smart_charge_count_table <- c3_elis3 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse, statutory rape, etc` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                                        sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_ordinance == 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, `Rape` = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon,
#                `Undefined` = undefined_charge_literal) %>%
#         filter(daca_charge_flag == 1, ever_smart_terminated == 1) %>% 
#         select(a_number, collapsed_charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()
# 
# # build count_pct table for smart
# smart_count_pct <- smart_charge_count_table %>% mutate(count_smart = n, pct_smart = count_smart / count_ever_smart_terminated_a_numbers) %>%
#         select(-n)
# smart_count_pct
# dim(smart_count_pct)
# 
# # build denial_charge_count_table
# denial_charge_count_table <- c3_elis3 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse, statutory rape, etc` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                                        sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_ordinance == 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, `Rape` = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon,
#                `Undefined` = undefined_charge_literal) %>%
#         filter(daca_charge_flag == 1, benefit_status == "Denied") %>% 
#         select(a_number, collapsed_charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()
# 
# # build count_pct table for denial
# denial_count_pct <- denial_charge_count_table %>% mutate(count_denial = n, pct_denial = count_denial / count_ever_denied_a_numbers) %>%
#         select(-n)
# denial_count_pct
# dim(denial_count_pct)
# 
# # build scops_charge_count_table
# scops_charge_count_table <- c3_elis3 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse, statutory rape, etc` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                                        sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile == 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_ordinance == 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, `Rape` = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon,
#                `Undefined` = undefined_charge_literal) %>%
#         filter(daca_charge_flag == 1, scops_terminated == 1) %>% 
#         select(a_number, collapsed_charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()
# 
# # build count_pct table for scops
# scops_count_pct <- scops_charge_count_table %>% mutate(count_scops = n, pct_scops = count_scops / count_scops_terminated_a_numbers) %>%
#         select(-n)
# scops_count_pct
# dim(scops_count_pct)
# 
# # build count_pct table for all_daca
# all_daca_count_pct <- ident_charge_count_table %>% mutate(count_ident = n, pct_all_daca = count_ident / count_all_daca_a_numbers) %>%
#         select(-c(n, count_ident))
# all_daca_count_pct
# dim(all_daca_count_pct)
# 
# # compile into single table
# daca_charges_count_pct_table <- ident_count_pct %>% left_join(., smart_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., scops_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., denial_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., all_daca_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table
# 
# # build totals row
# daca_charges_count_pct_totals_row <- data.frame(charge_type = "Totals", count_ident = count_daca_charge_flag_a_numbers,
#                                        pct_ident = count_daca_charge_flag_a_numbers / count_daca_charge_flag_a_numbers,
#                                        count_smart = count_ever_smart_terminated_a_numbers,
#                                        pct_smart = count_ever_smart_terminated_a_numbers / count_ever_smart_terminated_a_numbers,
#                                        count_scops = count_scops_terminated_a_numbers,
#                                        pct_scops = count_scops_terminated_a_numbers / count_scops_terminated_a_numbers,
#                                        count_denial = count_ever_denied_a_numbers,
#                                        pct_denial = count_ever_denied_a_numbers / count_ever_denied_a_numbers,
#                                        pct_all_daca = count_daca_charge_flag_a_numbers / count_all_daca_a_numbers)
# glimpse(daca_charges_count_pct_totals_row) 
# 
# # add daca_charges_count_pct_totals_row 
# daca_charges_count_pct_table <- rbind(daca_charges_count_pct_totals_row, daca_charges_count_pct_table)
# daca_charges_count_pct_table
# 
# # remove health_and_safety charge because it's a court statement for drug charge
# # daca_raw %>% filter(a_number == "A204424162") %>% do(cat(.$ident_report)) # in part 1
# # daca_charges_count_pct_table <- daca_charges_count_pct_table %>% 
# #         filter(!(charge_type %in% c("health_and_safety", "keyline_does_not_list_specific_charge")))
# # glimpse(daca_charges_count_pct_table)
# 
# # inspect join
# daca_charges_count_pct_table %>% filter(is.na(pct_smart))  
# 
# # convert NA to zero
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% mutate_at(.vars = vars(-charge_type), .funs = ~ ifelse(is.na(.x), 0, .x))
# daca_charges_count_pct_table
# 
# # clean table
# daca_charges_count_pct_table_clean <- daca_charges_count_pct_table %>% rename(count_smart_terminated = count_smart, pct_smart_terminated = pct_smart, 
#                                         count_scops_terminated = count_scops, pct_scops_terminated = pct_scops) %>%
#         arrange(desc(count_ident)) %>%
#         mutate(count_ident = comma(count_ident), pct_ident = str_c(round(pct_ident * 100, digits = 2), "%", sep = ""), 
#                count_smart_terminated = comma(count_smart_terminated), pct_smart_terminated = percent(pct_smart_terminated),
#                count_scops_terminated = comma(count_scops_terminated), pct_scops_terminated = percent(pct_scops_terminated),
#                count_denial = comma(count_denial), pct_denial = percent(pct_denial),
#                pct_all_daca = str_c(round(pct_all_daca * 100, digits = 2), "%", sep = "")) %>% 
#         mutate(pct_ident = case_when(pct_ident == "0%" ~ "0.00%", pct_ident == "100%" ~ "100.00%", TRUE ~ pct_ident),
#                pct_all_daca = case_when(pct_all_daca == "0%" ~ "0.00%", pct_all_daca == "100%" ~ "100.00%", TRUE ~ pct_all_daca))
#         rename(`Charge type` = charge_type, `Number of Requestors w/ IDENT report` = count_ident,
#                         `Percent of Requestors w/ IDENT report` = pct_ident,
#                 `Count of Requestors w/ any terminations in C3/ELIS` = count_smart_terminated, 
#                 `Percent of Requestors w/ any terminations in C3/ELIS` = pct_smart_terminated,
#                 `Count of Requestors w/ any terminations in SCOPS records` = count_scops_terminated, 
#                 `Percent of Requestors w/ any terminations in SCOPS records` = pct_scops_terminated,
#                 `Count of Requestors w/ any denials in C3/ELIS` = count_denial, 
#                 `Percent of Requestors w/ any denials in C3/ELIS` = pct_denial,
#                 `Percent of all DACA Requestors` = pct_all_daca) %>% 
#         select(-c(`Count of Requestors w/ any terminations in SCOPS records`, `Percent of Requestors w/ any terminations in SCOPS records`,
#                   `Count of Requestors w/ any terminations in C3/ELIS`, `Percent of Requestors w/ any terminations in C3/ELIS`,
#                   `Count of Requestors w/ any denials in C3/ELIS`, `Percent of Requestors w/ any denials in C3/ELIS`))
# daca_charges_count_pct_table_clean
# 
# # write to file
# write_csv(daca_charges_count_pct_table_clean, "Data/collapsed_daca_charges_count_pct_table.csv")


################################################################


# # create daca_charges_in_c3_elis3 which will be used to create table with count of arrests per a_number and count of unique charges per a_number
# c3_elis3 <- read_csv("c3_elis3.csv")
# daca_charges <- read_csv("daca_charges.csv")
# 
# glimpse(c3_elis3)
# glimpse(daca_charges)
# 
# # get 98092 a_numbers in c3_elis3 that are also in daca_charages
# # note there are 3466 a_numbers in daca_charges that are not in c3_elis3 - a few are in c3_original
# c3_elis3_daca_charges_a_numbers <- c3_elis3 %>% filter(daca_charge_flag == 1) %>% distinct(a_number)
# nrow(c3_elis3_daca_charges_a_numbers) # 98092
# daca_charges %>% distinct(anum) %>% count() # 101558
# 
# # create daca_charges_in_c3_elis3
# daca_charges_in_c3_elis3 <- daca_charges %>% filter(anum %in% c3_elis3_daca_charges_a_numbers$a_number)
# glimpse(daca_charges_in_c3_elis3)
# daca_charges_in_c3_elis3 %>% distinct(anum) %>% count() # 98092
# 
# # inspect daca_charges keydate to use as flag for unique arrest event
# # there 918 NA keydates, but otherwise all good formats
# # of the 918 with NA keydates, there are only 3 anum that have ONLY NA keydates
# sum(is.na(daca_charges_in_c3_elis3$keydate))
# daca_charges_in_c3_elis3 %>% filter(!is.na(keydate)) %>% distinct(anum) %>% count() # 98089
# daca_charges_in_c3_elis3_na_keydate <- daca_charges_in_c3_elis3 %>% filter(!is.na(keydate)) %>% select(anum)
# daca_charges_in_c3_elis3 %>% filter(!(anum %in% daca_charges_in_c3_elis3_na_keydate$anum)) 
# daca_charges_in_c3_elis3 %>% filter(!(anum %in% daca_charges_in_c3_elis3_na_keydate$anum)) %>% distinct(anum) %>% count() # 3
# 
# # note that NA dates appear to be overlooked by str_detect regex
# # x <- data.frame(dates = c(ymd("2014-10-05"), ymd("2013-10-05"), NA))
# # x %>% filter(str_detect(dates, regex("[0-9]{3}-[0-9]{2}-[0-9]{2}")))
# daca_charges_in_c3_elis3 %>% filter(!is.na(keydate), !(str_detect(keydate, regex("[0-9]{3}-[0-9]{2}-[0-9]{2}"))))
# daca_charges_w_unformatted_keydate <- daca_charges_in_c3_elis3 %>% 
#         filter(!(str_detect(keydate, regex("[0-9]{3}-[0-9]{2}-[0-9]{2}"))))
# daca_charges_w_unformatted_keydate
# 
# # create filter for informative charge_type
# # on second thought - this charge_type_filter may not be useful; workarounds are in place where needed
# daca_charges_in_c3_elis3 %>% distinct(charge_type) %>% arrange(charge_type) %>% data.frame()
# charge_type_filter <- daca_charges_in_c3_elis3 %>% distinct(charge_type) %>% 
#         filter(charge_type != "keyline_does_not_list_specific_charge", !is.na(charge_type)) %>% pull(charge_type)
# length(charge_type_filter) # 73
# charge_type_filter
# 
# # inspect charge_type_filter
# daca_charges_in_c3_elis3 %>% distinct(anum) %>% count() # 98092
# daca_charges_in_c3_elis3 %>% filter(charge_type %in% charge_type_filter) %>% distinct(anum) %>% count() # 87411
# 
# # there are 10681 records which have only "keyline_does_not_list_specific_charge", or which have "keyline_does_not_list_specific_charge" and NA charge_type
# # the charge_type_filter excludes these, so 98092 - 10681 = 87411
# daca_charges_in_c3_elis3 %>% filter(charge_type != "keyline_does_not_list_specific_charge") %>% distinct(anum) %>% count() # 87411
# daca_charges_anum_wo_nonspecific_charge <- daca_charges_in_c3_elis3 %>% filter(charge_type != "keyline_does_not_list_specific_charge")
# daca_c3_elis3_w_only_nonspecific_charge <- daca_charges_in_c3_elis3[!(daca_charges_in_c3_elis3$anum %in% daca_charges_anum_wo_nonspecific_charge$anum), ]
# daca_c3_elis3_w_only_nonspecific_charge %>% distinct(anum) %>% count() # 10681
# daca_c3_elis3_w_only_nonspecific_charge %>% distinct(charge_type) 
# daca_c3_elis3_w_only_nonspecific_charge %>% filter(anum %in% daca_charges_anum_wo_nonspecific_charge$anum)
# daca_charges_anum_wo_nonspecific_charge %>% filter(anum %in% daca_c3_elis3_w_only_nonspecific_charge$anum)
# 98092 - 87411 == 10681

# daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge") %>% distinct(anum) %>% count() # 89856
# all_daca_charges_anum_wo_nonspecific_charge <- daca_charges %>% filter(charge_type != "keyline_does_not_list_specific_charge")
# daca_charges_w_only_nonspecific_charge <- daca_charges[!(daca_charges$anum %in% all_daca_charges_anum_wo_nonspecific_charge$anum), ]
# daca_charges_w_only_nonspecific_charge %>% distinct(anum) %>% count() # 11702
# daca_charges_w_only_nonspecific_charge %>% distinct(charge_type) 
# daca_charges_w_only_nonspecific_charge %>% filter(anum %in% all_daca_charges_anum_wo_nonspecific_charge$anum)
# all_daca_charges_anum_wo_nonspecific_charge %>% filter(anum %in% daca_charges_w_only_nonspecific_charge$anum)
# 89856 + 11702 == 101558

###################################################################


# # create table with count of arrests per a_number 
# 
# # get count of arrests per a_number
# arrest_count_per_a_number <- daca_charges_in_c3_elis3 %>% filter(!(charge_type == "keyline_does_not_list_specific_charge")) %>% 
#         distinct(anum, keydate) %>% group_by(anum) %>% count() %>% ungroup()
# arrest_count_per_a_number %>% arrange(desc(n))
# arrest_count_per_a_number %>% arrange(n)
# arrest_count_per_a_number %>% distinct(anum) %>% count() # 87589
# 
# # create arrest_count_per_a_number_nonspecific
# # the charge_type_filter excludes a_numbers with only "keyline_does_not_list_specific_charge", or which have "keyline_does_not_list_specific_charge" and NA charge_type
# arrest_count_per_a_number_nonspecific <- daca_charges_in_c3_elis3 %>% filter(anum %in% daca_charges_w_only_nonspecific_charge$anum) %>% distinct(anum, keydate) %>% 
#         group_by(anum) %>% count() %>% ungroup()
# arrest_count_per_a_number_nonspecific %>% distinct(anum) %>% count() # 10503
# arrest_count_per_a_number_nonspecific %>% arrange(n)
# arrest_count_per_a_number_nonspecific %>% arrange(desc(n))
# ggplot(arrest_count_per_a_number_nonspecific, aes(x = n)) + geom_histogram()
# 
# # inspect arrest_count_per_a_number_nonspecific
# daca_raw %>% filter(a_number == "A201282990") %>% do(cat(.$ident_report))
# daca_charges %>% filter(anum == "A201282990") %>% distinct(keyline, charge_type) 
# 
# # inspect a_numbers with high arrest count
# daca_charges_in_c3_elis3 %>% filter(anum == "A200551269") %>% select(anum, keydate, keyline, charge_type) %>% arrange(keydate) %>% data.frame()
# daca_charges_in_c3_elis3 %>% filter(anum == "A200551269") %>% select(anum, keydate, keyline, charge_type) %>% 
#         filter(charge_type %in% charge_type_filter) %>% arrange(keydate) %>% data.frame()
# daca_charges_in_c3_elis3 %>% filter(anum == "A200551269") %>% distinct(keydate) %>% count()
# daca_charges_in_c3_elis3 %>% filter(anum == "A200551269", charge_type %in% charge_type_filter) %>% distinct(keydate) %>% count()
# 
# 
# c3_elis3 %>% filter(a_number == "A200551269") %>% arrange(act_date) %>% 
#         select(a_number, receipt_number, benefit_status, act_date, ever_smart_terminated, scops_terminated, termination_date1, description1) %>% data.frame()
# 
# daca_raw %>% filter(a_number == "A200551269") %>% do(cat(.$ident_report)) 
# daca_raw %>% filter(a_number == "A200551269") %>% write_csv(., "daca_ident_report.csv")
# 
# # merge arrest_count_per_a_number and arrest_count_per_a_number_nonspecific
# arrest_count_per_a_number_all <- rbind(arrest_count_per_a_number, arrest_count_per_a_number_nonspecific)
# glimpse(arrest_count_per_a_number_all)
# 
# # inspect merge
# daca_charges_in_c3_elis3 %>% distinct(anum) %>% count()
# arrest_count_per_a_number %>% distinct(anum) %>% count()
# arrest_count_per_a_number_nonspecific %>% distinct(anum) %>% count()
# arrest_count_per_a_number_all %>% distinct(anum) %>% count()
# 87589 + 10503 == 98092
# 
# # inspect anum that have ONLY NA keydates
# # they are counted as having 1 arrest, despite NA keydate, because the filter just does a count of distinct(a_numbers, keydate), 
# # so they have single entry as (anum, keydate=NA)
# arrest_count_per_a_number_all %>% filter(anum %in% c("A204216087", "A204212169", "A204211996"))
# 
# # inspect arrest_count_per_a_number_all
# arrest_count_per_a_number_all %>% distinct(n) %>% arrange(desc(n)) %>% data.frame() 
# sum(is.na(arrest_count_per_a_number_all$n))
# arrest_count_per_a_number_all %>% arrange(desc(n))
# 
# 
# # get tally of a_numbers per arrest_count
# a_number_per_arrest_tally <- arrest_count_per_a_number_all %>% group_by(n) %>% count() %>% ungroup() %>% rename(arrest_count = n, a_number_count = nn)
# a_number_per_arrest_tally %>% arrange(desc(arrest_count)) %>% data.frame()
# a_number_per_arrest_tally %>% summarize(total = sum(a_number_count))
# 
# # create histogram
# arrest_count_per_a_number_all %>% filter(n > 10) %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1, color = "white", fill = "blue") + theme_classic()
# 
# arrest_count_per_a_number_all %>% 
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1, color = "white", fill = "blue") + theme_classic()
# 
# arrest_count_per_a_number_all %>% 
#         ggplot(., aes(x = n)) + geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", fill = "blue")
# 
# arrest_count_per_a_number_all %>% filter(n > 10) %>%
#         ggplot(., aes(x = n)) + geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", fill = "blue")
# 
# arrest_count_per_a_number_all %>% mutate(n = n + .001) %>% mutate(log_n = log(n)) %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1, color = "white", fill = "blue")
# 
# 
# arrest_count_per_a_number_all %>% mutate(n = n + 20) %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1, color = "white", fill = "blue") + scale_y_log10() + theme_classic()
# 
# arrest_count_per_a_number_all %>% filter(n > 10) %>% mutate(n = n + 2) %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1, color = "white", fill = "blue") + scale_y_log10() + theme_classic()




##########################################################################

# 
# # get count of unique charges per a_number
# 
# # inspect
# daca_charges_in_c3_elis3 %>% filter(!(charge_type == "keyline_does_not_list_specific_charge")) %>% distinct(anum) %>% count() # 87411
# daca_charges_in_c3_elis3 %>% filter(!(charge_type == "keyline_does_not_list_specific_charge")) %>% distinct(charge_type) %>% count() # 73
# daca_charges_in_c3_elis3 %>% filter(!(charge_type == "keyline_does_not_list_specific_charge")) %>% distinct(charge_type) %>% arrange(charge_type) %>% pull(charge_type)
# 
# daca_charges_in_c3_elis3 %>% filter(anum %in% daca_c3_elis3_w_only_nonspecific_charge$anum) %>% distinct(anum) %>% count() # 10681
# daca_charges_in_c3_elis3 %>% filter(anum %in% daca_c3_elis3_w_only_nonspecific_charge$anum) %>% distinct(charge_type) %>% count() # 2
# daca_charges_in_c3_elis3 %>% filter(anum %in% daca_c3_elis3_w_only_nonspecific_charge$anum) %>% distinct(charge_type)
# 87411 + 10681 == 98092
# 
# # get count per_a_number
# charge_count_per_a_number <- daca_charges_in_c3_elis3 %>% filter(!(charge_type == "keyline_does_not_list_specific_charge")) %>% 
#         mutate(charge_type_collapsed = case_when(charge_type == "bail" | charge_type == "contempt_bench_warrant" ~ "Contempt, bench warrant, bail, etc",
#                                                  charge_type == "battery" | charge_type == "fighting" | charge_type == "mayhem" ~ "Battery",
#                                                  charge_type == "child_abuse" | charge_type == "domestic_violence" | charge_type == "inflict_corporeal_injury" ~
#                                                          "Offenses against family and children",
#                                                  charge_type == "cruelty_to_animals" | charge_type == "dog_fighting" | charge_type == "undefined_charge_literal" |
#                                                          charge_type == "electronic_communication_device" ~ "Other",
#                                                  charge_type == "false_imprisonment" | charge_type == "kidnap" | charge_type == "unlawful_restraint" |
#                                                          charge_type == "human_trafficking" ~ "Kidnapping, trafficking, false imprisonment",
#                                                  charge_type == "homicide_murder_att_murder" | charge_type == "manslaughter" ~ "Homicide / manslaughter (incl. attempted)",
#                                                  charge_type == "fraud_false_pretense_corruption_money_laundering" | charge_type == "perjury" | charge_type == "impersonate" ~
#                                                          "Fraud, money-laundering, corruption, etc",
#                                                  charge_type == "indecent_exposure_peeping" | charge_type == "lewd_lascivious_acts" | charge_type == "moral_turpitude" ~ 
#                                                          "Indecent exposure, lewd/lascivious acts, etc",
#                                                  charge_type == "intimidating_witness" | charge_type == "undefined_treat_attempted_crime_conspiracy" |
#                                                          charge_type == "terrorist" ~ "Undefined threats, attempted crime/conspiracy, etc",
#                                                  charge_type == "offensive_touching" | charge_type == "sex_abuse_unlawful" | charge_type == "sex_assault" | charge_type == "sexual_contact_with_minor" ~
#                                                          "Sexual abuse",
#                                                  charge_type == "oral_copulation" | charge_type == "prostitution_pandering_brothel" | charge_type == "sodomy" | charge_type == "solicitation" ~
#                                                          "Illegal sex-related acts",
#                                                  charge_type == "harassment_restrain_order_violation" | charge_type == "stalking" ~ "Harassment, restraining order violation, etc",
#                                                  charge_type == "trespass" | charge_type == "unlawful_entry_home_invasion_enter_noncomm_dwelling" ~ "Trespass, unlawful entry, etc",
#                                                  TRUE ~ charge_type)) %>%
#         group_by(anum, charge_type_collapsed) %>% count() %>% ungroup() %>% arrange(anum)
# 
# # inspect charge_count_per_a_number
# charge_count_per_a_number
# charge_count_per_a_number %>% distinct(anum) %>% count() # 87411
# charge_count_per_a_number %>% arrange(desc(n))
# 
# charge_count_per_a_number %>% distinct(charge_type_collapsed) %>% data.frame()
# charge_count_per_a_number %>% filter(is.na(charge_type_collapsed)) %>% distinct(charge_type) %>% data.frame()
# 
# 
# # get count per_a_number w nonspecific charge
# # inspect below, then set charge count = 0 since they have nonspecific charges
# charge_count_per_a_number_nonspecific <- daca_charges_in_c3_elis3 %>% filter(anum %in% daca_c3_elis3_w_only_nonspecific_charge$anum) %>% 
#         group_by(anum, charge_type) %>% count() %>% ungroup() %>% arrange(anum)
# charge_count_per_a_number_nonspecific
# charge_count_per_a_number_nonspecific %>% distinct(anum) %>% count() # 10681
# charge_count_per_a_number_nonspecific %>% distinct(charge_type) 
# charge_count_per_a_number_nonspecific %>% arrange(desc(n))
# 
# charge_count_per_a_number_nonspecific <- charge_count_per_a_number_nonspecific %>% distinct(anum) %>% mutate(n = 0)
# charge_count_per_a_number_nonspecific
# charge_count_per_a_number_nonspecific %>% distinct(anum) %>% count() # 10681
# glimpse(charge_count_per_a_number_nonspecific)
# 
# # create arrest_count_per_a_number_nonspecific
# # the charge_type_filter excludes a_numbers with only "keyline_does_not_list_specific_charge", or which have "keyline_does_not_list_specific_charge" and NA charge_type
# # arrest_count_per_a_number_nonspecific <- daca_charges_in_c3_elis3 %>% filter(anum %in% daca_charges_w_only_nonspecific_charge$anum) %>% distinct(anum, keydate) %>% 
# #         group_by(anum) %>% count() %>% ungroup()
# # arrest_count_per_a_number_nonspecific 
# # arrest_count_per_a_number_nonspecific %>% distinct(anum) %>% count()
# # 
# 
# 
# # inspect anum with high charge count
# daca_charges %>% filter(anum == "A204248990")
# 
# c3_elis3 %>% filter(a_number == "A204248990") %>% arrange(act_date) %>% 
#         select(a_number, receipt_number, benefit_status, act_date, ever_smart_terminated, scops_terminated, termination_date1, description1) %>% data.frame()
# 
# daca_raw %>% filter(a_number == "A204248990") %>% do(cat(.$ident_report)) 
# daca_raw %>% filter(a_number == "A204248990") %>% write_csv(., "daca_ident_report.csv")
# 
# 
# 
# 
# 
# 
# # get count of charge_type per a_number
# charge_type_tally_per_a_number <- charge_count_per_a_number %>% group_by(anum) %>% count() %>% ungroup() %>% rename(n = nn) %>% arrange(desc(n))
# charge_type_tally_per_a_number
# charge_type_tally_per_a_number %>% distinct(anum) %>% count() # 87411
# 
# # merge charge_type_per_anumber and charge_type_per_a_number_nonspecific
# charge_type_tally_per_a_number_all <- rbind(charge_type_tally_per_a_number, charge_count_per_a_number_nonspecific)
# charge_type_tally_per_a_number_all
# charge_type_tally_per_a_number_all %>% distinct(anum) %>% count() # 98092
# charge_type_tally_per_a_number_all %>% arrange(n)
# charge_type_tally_per_a_number_all %>% arrange(desc(n))
# 
# # inspect anum with high unique charge count
# daca_charges %>% filter(anum == "A207245626", charge_type %in% charge_type_filter) %>% select(anum, keydate, keyline, charge_type) %>% 
#         arrange(keydate) %>% data.frame()
# 
# daca_charges %>% filter(anum == "A207245626", charge_type %in% charge_type_filter) %>% select(anum, keydate, keyline, charge_type) %>% 
#         arrange(keydate) %>% distinct(charge_type) %>% data.frame()
# 
# c3_elis3 %>% filter(a_number == "A207245626") %>% arrange(act_date) %>% 
#         select(a_number, receipt_number, benefit_status, act_date, ever_smart_terminated, scops_terminated, termination_date1, description1) %>% data.frame()
# 
# daca_raw %>% filter(a_number == "A207245626") %>% do(cat(.$ident_report)) 
# daca_raw %>% filter(a_number == "A207245626") %>% write_csv(., "daca_ident_report.csv")
# 
# 
# 
# 
# 
# 
# # get count of a_numbers per charge_type tally
# count_a_number_per_charge_type_tally <- charge_type_tally_per_a_number_all %>% group_by(n) %>% count() %>% 
#         rename(charge_type_tally = n, n = nn) %>% ungroup() %>% arrange(charge_type_tally)
# count_a_number_per_charge_type_tally
# count_a_number_per_charge_type_tally %>% summarize(total = sum(n)) # 98092
# 
# # inspect
# charge_type_tally_per_a_number_all %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1) 
# 
# charge_type_tally_per_a_number_all %>%
#         ggplot(., aes(x = n)) + geom_histogram(binwidth = 1) + scale_x_discrete(breaks = c(seq(from = 1, to = 18, by = 1)), labels = c(seq(from = 1, to = 18, by = 1)))
# 
# 
# 
# 
# # get average count of arrests per charge_type_count bucket
# 
# arrests <- daca_charges %>% filter(anum %in% c3_elis3$a_number, arrested == 1, keydate > "1900-01-01", keydate < "2019-01-01")
# arrests
# arrests %>% distinct(anum, keydate) # 149502
# arrests %>% distinct(anum) %>% count() # 91468
# arrests %>% distinct(keydate) %>% arrange(keydate)
# arrests %>% distinct(keydate) %>% arrange(desc(keydate))
# 
# # write file for reid
# arrests_distinct <- arrests %>% distinct(anum, keydate, arrested)
# arrests_distinct
# # write_csv(arrests_distinct, "Data/daca_charges_arrests_per_a_number.csv")
# 
# # arrests_unfiltered <- daca_charges %>% filter(anum %in% c3_elis3$a_number, arrested == 1)
# # arrests_unfiltered %>% distinct(anum, keydate) # 149585
# 
# arrest_count_per_a_number <- arrests %>%  
#         distinct(anum, keydate) %>% group_by(anum) %>% count() %>% ungroup()
# 
# arrest_count_per_a_number
# glimpse(arrest_count_per_a_number) # 91467
# arrest_count_per_a_number %>% arrange(desc(n))
# arrest_count_per_a_number %>% distinct(a_number) %>% count() # 91467
# arrest_count_per_a_number %>% distinct(n)
# 
# # inspect anumbers without arrest flag and valid keydate
# non_arrests <- daca_charges %>% filter(anum %in% c3_elis3$a_number, !(anum %in% arrests$anum))
# non_arrests %>% distinct(anum) %>% count() # 6625
# 91467 + 6625 == 98092
# non_arrests %>% distinct(charge_type)
# non_arrests %>% select(anum, keydate, arrested, keyline, charge_type)
# # note this method will miss some valid arrests that dont have arrest_flag turned on (see below)
# daca_charges %>% filter(anum == "A204468998") %>% select(anum, keydate, arrested, keyline, charge_type)
# daca_raw %>% filter(a_number == "A204468998") %>% do(cat(.$ident_report)) 
# 
# # add anumbers without arrest flag and valid keydate as having 0 arrests
# non_arrests_count_per_a_number <- non_arrests %>% distinct(anum) %>% mutate(n = 0)
# non_arrests_count_per_a_number
# non_arrests_count_per_a_number %>% distinct(anum) %>% count() # 6625
# arrest_count_per_a_number_all <- rbind(arrest_count_per_a_number, non_arrests_count_per_a_number)
# arrest_count_per_a_number_all %>% distinct(a_number) %>% count() # 98092
# arrest_count_per_a_number_all %>% arrange(desc(n))
# 
# # inspect anum with high arrest count
# daca_charges %>% filter(anum == "A200551269", charge_type %in% charge_type_filter) %>% select(anum, keydate, keyline, charge_type, arrested) %>% 
#         arrange(keydate) %>% data.frame()
# 
# c3_elis3 %>% filter(a_number == "A200551269") %>% arrange(act_date) %>% 
#         select(a_number, receipt_number, benefit_status, act_date, ever_smart_terminated, scops_terminated, termination_date1, description1) %>% data.frame()
# 
# daca_raw %>% filter(a_number == "A200551269") %>% do(cat(.$ident_report)) 
# daca_raw %>% filter(a_number == "A200551269") %>% write_csv(., "daca_ident_report.csv")
# 
# # merge arrest_count_per_a_number with charge_type_tally_per_a_number_all
# 
# # rename variables
# arrest_count_per_a_number_all <- arrest_count_per_a_number_all %>% rename(arrest_count = n)
# arrest_count_per_a_number_all
# arrest_count_per_a_number_all %>% filter(arrest_count > 0) # 91467
# arrest_count_per_a_number_all %>% distinct(anum) %>% count() # 98092
# 
# # write to file to match with reid
# # write_csv(arrest_count_per_a_number_all, "Data/daca_charges_arrest_count_per_a_number_all.csv")
# 
# charge_type_tally_per_a_number_all <- charge_type_tally_per_a_number_all %>% rename(charge_type_tally = n)
# charge_type_tally_per_a_number_all
# charge_type_tally_per_a_number_all %>% distinct(anum) %>% count() # 98092
# 
# merged_arrest_charge_type_count <- left_join(arrest_count_per_a_number_all, charge_type_tally_per_a_number_all, 
#                                              by = c("anum" = "anum"))
# glimpse(merged_arrest_charge_type_count)
# merged_arrest_charge_type_count %>% distinct(anum) %>% count() # 98092
# 
# # get average arrest count by charge_type_count buckets
# merged_arrest_charge_type_count_summary <- merged_arrest_charge_type_count %>% group_by(charge_type_tally) %>% 
#         summarize(a_number_count = n(), avg_arrests = mean(arrest_count))
# merged_arrest_charge_type_count_summary
# 
# merged_arrest_charge_type_count_summary %>% summarize(total = sum(a_number_count))
# 
# # inspect anum with charge_type_count = 15 and arrest count avg = 24
# charge_type_tally_per_a_number_all %>% filter(charge_type_tally == 15)
# 
# daca_charges %>% filter(anum == "A200551269", charge_type %in% charge_type_filter) %>% select(anum, keydate, keyline, charge_type) %>% 
#         arrange(keydate) %>% data.frame()
# 
# daca_charges %>% filter(anum == "A200551269", arrested == 1) %>% distinct(anum, keydate, arrested) %>% 
#         arrange(keydate) %>% data.frame()
# 
# c3_elis3 %>% filter(a_number == "A200551269") %>% arrange(act_date) %>% 
#         select(a_number, receipt_number, benefit_status, act_date, ever_smart_terminated, scops_terminated, termination_date1, description1) %>% data.frame()
# 
# daca_raw %>% filter(a_number == "A089231975") %>% do(cat(.$ident_report)) 
# daca_raw %>% filter(a_number == "A089231975") %>% write_csv(., "daca_ident_report.csv")
# 
# # inspect anum with charge_type_count = 0 and avg_arrest = .8
# charge_type_tally_zero <- charge_type_tally_per_a_number_all %>% filter(charge_type_tally == 0)
# 
# daca_charges %>% filter(anum %in% charge_type_tally_zero$anum, arrested == 1) %>% select(anum, keydate, arrested, keyline) %>% 
#         arrange(anum, keydate)
# 
# daca_charges %>% filter(anum %in% charge_type_tally_zero$anum) %>% select(anum, keydate, arrested, keyline, charge_type) %>% 
#         arrange(anum, keydate) %>% data.frame()
# 
# 
# 
# 
# 
# # create table with count of anumbers per charge_type tally
# 
# # add percentages
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary %>% 
#         mutate(pct_of_all_a_numbers = a_number_count / sum(a_number_count)) %>% 
#         select(charge_type_tally, a_number_count, pct_of_all_a_numbers, avg_arrests)
# merged_arrest_charge_type_count_summary_table
# 
# # get weighted average charge count
# merged_arrest_charge_type_count_summary_table %>% mutate(charge_type_tally_weighted = charge_type_tally * a_number_count) %>% 
#         filter(charge_type_tally != 0) %>%
#         summarize(charge_type_tally_weighted_average = sum(charge_type_tally_weighted) / sum(a_number_count)) # 1.67 (excluding charge_type_tally = 0)
# 
# # add totals row
# merged_arrest_charge_type_count_summary_total <- merged_arrest_charge_type_count_summary_table %>% summarize(a_number_count = sum(a_number_count))
# merged_arrest_charge_type_count_summary_total <- data.frame(charge_type_tally = "Total", 
#                                                  a_number_count = merged_arrest_charge_type_count_summary_total, 
#                                                  pct_of_all_a_numbers = sum(merged_arrest_charge_type_count_summary_table$pct_of_all_a_numbers),
#                                                  avg_arrests = 0)
# merged_arrest_charge_type_count_summary_total
# 
# # combine table with totals row
# merged_arrest_charge_type_count_summary_table <- rbind(merged_arrest_charge_type_count_summary_table, merged_arrest_charge_type_count_summary_total)
# merged_arrest_charge_type_count_summary_table %>% data.frame()
# 
# # change charge_type_tally = 0 to "Unable to extract charge from IDENT report"
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary_table %>% 
#         mutate(charge_type_tally = case_when(charge_type_tally == "0" ~ "0*", TRUE ~ charge_type_tally))
# merged_arrest_charge_type_count_summary_table 
# 
# # format table
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary_table %>%
#         mutate(a_number_count = comma(a_number_count), pct_of_all_a_numbers = percent(pct_of_all_a_numbers), 
#                avg_arrests = round(avg_arrests, digits = 2))
# merged_arrest_charge_type_count_summary_table
# 
# # convert small percentages to "<0.01%"
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary_table %>%
#         mutate(pct_of_all_a_numbers = case_when(pct_of_all_a_numbers < .01 ~ "<0.01%", TRUE ~ as.character(pct_of_all_a_numbers)))
# merged_arrest_charge_type_count_summary_table
# 
# # remove total row for avg_arrests
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary_table %>% 
#         mutate(avg_arrests = case_when(charge_type_tally == "Total" ~ "", TRUE ~ as.character(avg_arrests)))
# merged_arrest_charge_type_count_summary_table
# 
# # rename headers
# merged_arrest_charge_type_count_summary_table <- merged_arrest_charge_type_count_summary_table %>% 
#         rename("Number of unique charge types per A-Number" = charge_type_tally,
#                "Number of A-Numbers w/ IDENT reports" = a_number_count, "Percent of A-Numbers w/ IDENT reports" = pct_of_all_a_numbers,
#                "Average number of arrests per A-Number w/ IDENT report" = avg_arrests)
# merged_arrest_charge_type_count_summary_table %>% data.frame()
# 
# # write table
# write_csv(merged_arrest_charge_type_count_summary_table, "Data/number_unique_charges_per_a_number_table.csv")
# # need to format in excel to get final table - collapse charge_type >= 13
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # get number of approvals/denials/terminations per anumber
# 
# # add c3_terminated_receipt variable
# # note all records are termination-related
# c3_term %>% distinct(action_text)
# sum(is.na(c3_term$action_text))
# 
# c3_term_extract <- c3_term %>% filter(a_number %in% daca_charges$anum) %>% select(a_number, receipt_number, action_date, action_text) %>% 
#         mutate(c3_term_extract_flag = 1) %>%
#         rename(c3_term_a_number = a_number, c3_term_receipt_number = receipt_number,
#                c3_term_action_date = action_date, c3_term_action_text = action_text)
# c3_term_extract # 4077
# c3_term_extract %>% distinct(c3_term_a_number) %>% count() # 1828
# c3_term_extract %>% distinct(a_number, c3_term_receipt_number) %>% count() # 1833
# sum(duplicated(c3_term_extract$c3_term_receipt_number)) # 2244
# c3_term_extract %>% arrange(c3_term_receipt_number)
# 
# # get distinct combos of a_num, receipt_number, c3_term_flag, since many receipt_numbers have multiple rows for term notice ordered/sent/etc
# c3_term_extract <- c3_term_extract %>% distinct(c3_term_a_number, c3_term_receipt_number, c3_term_extract_flag)
# c3_term_extract # 1833
# c3_term_extract %>% distinct(c3_term_a_number, c3_term_receipt_number) # 1833
# c3_term_extract %>% distinct(c3_term_receipt_number) # 1833
# c3_term_extract %>% distinct(c3_term_a_number) # 1828
# 
# # inspect receipt_numbers in c3_elis3
# sum(duplicated(c3_elis3$receipt_number))
# c3_elis3_dup_receipt_numbers <- unique(c3_elis3$receipt_number[duplicated(c3_elis3$receipt_number)])
# c3_elis3_dup_receipt_numbers
# c3_elis3 %>% select(a_number, receipt_number) %>% arrange(a_number, receipt_number)
# c3_elis3 %>% select(a_number, receipt_number, act_date, benefit_status) %>% filter(receipt_number %in% c3_elis3_dup_receipt_numbers)
# 
# # only one benefit_status/act_date per receipt_number in c3_elis3
# c3_elis3_receipt_number_count <- c3_elis3 %>% distinct(a_number, receipt_number, act_date, benefit_status) %>% filter(receipt_number %in% c3_elis3_dup_receipt_numbers) %>%
#         group_by(receipt_number) %>% count() %>% arrange(desc(n))
# c3_elis3_receipt_number_count
# 
# # merge c3_term_extract to c3_elis3_daca_charges
# daca_charges_benefit_status_count_per_a_number <- c3_elis3 %>% filter(a_number %in% daca_charges$anum) %>%
#         left_join(., c3_term_extract, by = c("receipt_number" = "c3_term_receipt_number"))
# 
# 
# glimpse(daca_charges_benefit_status_count_per_a_number) # 335,216
# daca_charges_benefit_status_count_per_a_number %>% distinct(a_number) %>% count() # 98092
# daca_charges_benefit_status_count_per_a_number %>% distinct(receipt_number) %>% count() # 198403
# daca_charges_benefit_status_count_per_a_number %>% distinct(a_number, receipt_number) %>% count() # 198403
# daca_charges_benefit_status_count_per_a_number %>% filter(c3_term_extract_flag == 1) %>% distinct(a_number, receipt_number, benefit_status,
#                                 c3_terminated, c3_term_extract_flag) # 1821
# daca_charges_benefit_status_count_per_a_number %>% filter(c3_term_extract_flag == 1) %>% distinct(a_number, receipt_number) # 1821
# daca_charges_benefit_status_count_per_a_number %>% filter(c3_term_extract_flag == 1) %>% distinct(receipt_number) # 1821
# daca_charges_c3_term_a_numbers <- daca_charges_benefit_status_count_per_a_number %>% filter(c3_term_extract_flag == 1) %>% distinct(a_number) # 1817
# daca_charges_c3_term_a_numbers %>% distinct(a_number) %>% count() # 1817
# 
# # inspect 11 c3_term_extract a_numbers that did not merge with daca_charges_c3_term_a_numbers
# c3_term_extract %>% filter(!(c3_term_a_number %in% daca_charges_c3_term_a_numbers$a_number)) %>% 
#         distinct(c3_term_a_number) # 11
# c3_term_extract %>% filter(c3_term_a_number %in% c3_elis3$a_number, c3_term_a_number %in% daca_charges$anum) %>% distinct(c3_term_a_number) # 1821
# c3_term_extract %>% filter(c3_term_a_number %in% c3_elis3$a_number, !(c3_term_a_number %in% daca_charges_c3_term_a_numbers)) %>%
#                                    distinct(c3_term_a_number)
# 
# c3_term_extract %>% filter(!(c3_term_receipt_number %in% c3_elis3$receipt_number)) %>% distinct(c3_term_receipt_number) # 12
# 1817 + 11 == 1828
# 1821 + 12 == 1833
# 
# # create variable for benefit_status_revised
# daca_charges_benefit_status_count_per_a_number <- daca_charges_benefit_status_count_per_a_number %>% 
#         distinct(a_number, receipt_number, act_date, benefit_status, c3_term_extract_flag) %>% 
#         mutate(benefit_status_revised = case_when(c3_term_extract_flag == 1 ~ "Terminated", TRUE ~ benefit_status))
# 
# # inspect benefit_status_revised
# glimpse(daca_charges_benefit_status_count_per_a_number) # 198403
# daca_charges_benefit_status_count_per_a_number %>% distinct(a_number) %>% count() # 98092
# daca_charges_benefit_status_count_per_a_number %>% distinct(receipt_number) %>% count() # 198403
# 
# daca_charges_benefit_status_count_per_a_number %>% group_by(benefit_status_revised) %>% count()
# daca_charges_benefit_status_count_per_a_number %>% group_by(benefit_status) %>% count()
# daca_charges_benefit_status_count_per_a_number %>% filter(c3_term_extract_flag == 1) %>% count() # 1821
# 
# 160139 - 159967 # 172
# 26451 - 24813 # 1638
# 11659 - 11648 # 11
# 172 + 1638 + 11 == 1821
# 1975 - 154 == 1821
# 
# # inspect receipt_numbers where benefit_status == "Approved" but c3_term_extract_flag == 1
# daca_charges_benefit_status_count_per_a_number %>% filter(benefit_status == "Approved" & c3_term_extract_flag == 1) %>%
#         distinct(a_number, receipt_number, act_date, benefit_status, benefit_status_revised)
# 
# # get count of approved, denied, terminated for each a_number
# daca_charges_benefit_status_count_per_a_number2 <- daca_charges_benefit_status_count_per_a_number %>% group_by(benefit_status_revised) %>%
#         
# 
# 
# # create lookup table extract with a_number and 


##########################################################################


# # collapse charge_type
# c3_elis2 %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                Battery = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                Other = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | undefined_charge_literal == 1 | 
#                                          electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
#                `Homicide / manslaughter (incl. attempted)` = case_when(homicide_murder_att_murder == 1 | manslaughter == 1 ~ 1, TRUE ~ 0),
#                `Fraud, money-laundering, corruption, etc` = case_when(fraud_false_pretense_corruption_money_laundering == 1 |
#                                                                               perjury == 1 | impersonate == 1 ~ 1, TRUE ~ 0),
#                `Indecent exposure, lewd/lascivious acts, etc` = case_when(indecent_exposure_peeping == 1 | 
#                                                                                   lewd_lascivious_acts == 1 | moral_turpitude == 1 ~ 1, TRUE ~ 0),
#                `Undefined threats, attempted crime/conspiracy, etc` = case_when(intimidating_witness == 1 | 
#                                                                                         undefined_treat_attempted_crime_conspiracy == 1 | terrorist == 1 ~ 1, TRUE ~ 0),
#                `Sexual abuse` = case_when(offensive_touching == 1 | sex_abuse_unlawful == 1 | sex_assault == 1 |
#                                                   sexual_contact_with_minor == 1 ~ 1, TRUE ~ 0),
#                `Illegal sex-related acts` = case_when(oral_copulation == 1 | prostitution_pandering_brothel == 1 |
#                                                               sodomy == 1 | solicitation == 1 ~ 1, TRUE ~ 0),
#                `Harassment, restraining order violation, etc` = case_when(harassment_restrain_order_violation == 1 |
#                                                                                   stalking == 1 ~ 1, TRUE ~ 0),
#                `Tresspass, unlawful entry, etc` = case_when(trespass == 1 | 
#                                                                     unlawful_entry_home_invasion_enter_noncomm_dwelling == 1 ~ 1, TRUE ~ 0),
#                `Undefined juvenile` = case_when(undefined_juvenile = 1 ~ 1, TRUE ~ 0),
#                `Undefined ordinance` = case_when(undefined_ordinance = 1 ~ 1, TRUE ~ 0)) %>% 
#         rename(Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang,
#                `Hit and Run` = hit_and_run, `Liquor-related` = intox_alco_liq,
#                Loitering = loitering, `Motor vehicle theft` = motor_vehicle_theft, `Unable to extract any charge` = unable_to_extract_charge,
#                `Probation/parole/curfew violation, etc` = probation_parole_violation_curfew_court_remand, Rape = rape,
#                `Reckless conduct/endangerment, etc` = reckless_endangerment_conduct, 
#                `Resisting/interfering/evading police, etc` = resisting_interference_evading,
#                `Riot, unlawful assembly, etc` = riot_mob_unlawful_assembly, Robbery = robbery, `Stolen property` = stolen_property,
#                `Theft, larceny, etc` = theft_larceny, 
#                `Other driving-related` = traffic_speed_moving_vehic_driving_license_insurance, `Weapon-related` = weapon)
# 
# # create vector of collapsed_charge_type_variables
# collapsed_charge_type_variables <- c("Accessory, accomplice, hindering, etc", "Contempt, bench warrant, bail, etc",
#                                      "Battery", "Offenses against family and children", "Other", "Kidnapping, trafficking, false imprisonment",
#                                      "Homicide / manslaughter (incl. attempted)", "Fraud, money-laundering, corruption, etc",
#                                      "Indecent exposure, lewd/lascivious acts, etc", 
#                                      "Undefined threats, attempted crime/conspiracy, etc", "Sexual abuse", 
#                                      "Illegal sex-related acts",
#                                      "Harassment, restraining order violation, etc", "Tresspass, unlawful entry, etc",
#                                      "Undefined juvenile", "Undefined ordinance",
#                                      "Arson", "Assault", "Burglary", "Child pornography", "Contributing to the delinquency of a minor",
#                                      "Vandalism", "Immigration-related", "Disorderly conduct", "Drug-related", "Driving under the influence",
#                                      "Embezzlement", "Failure to appear", "Fail to comply/obey, etc", "Forgery, counterfeiting, etc", "Obstruction, fabrication, false claim, etc",
#                                      "Gambling", "Gang", "Hit and Run", "Liquor-related", "Loitering", "Motor vehicle theft",
#                                      "Unable to extract any charge", "Probation/parole/curfew violation, etc", "Rape", 
#                                      "Reckless conduct/endangerment, etc", 
#                                      "Resisting/interfering/evading police, etc", "Riot, unlawful assembly, etc", "Robbery", "Stolen property",
#                                      "Theft, larceny, etc", "Other driving-related", "Weapon-related")
# 
# 
