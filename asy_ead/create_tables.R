library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(naniar)
library(testthat)
library(scales)
library(openxlsx)
library(UpSetR)
library(fs)


# setwd
setwd("X:/Asylum_EAD_study")
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")


options(scipen=999)


# load validate_anumbers function
source("code/helper_scripts/validate_anumbers.R")

# load get_invalid_anumbers function
source("code/helper_scripts/get_invalid_anumbers.R")

# load as_percent function
source("code/helper_scripts/as_percent.R")

# load round_to_digits()
source("code/helper_scripts/round_to_digits.R")

# load add_group_index()
source("code/helper_scripts/add_group_index.R")

# load get_variation_functions()
source("code/helper_scripts/get_variation_functions.R")

# load fy()
source("code/helper_scripts/fy.R")

# load add_dummies()
source("code/helper_scripts/add_dummies.R")

# load prd_format function
source("code/helper_scripts/prd_format/prd_format_20200728.R")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# read in joined data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


#//////////////////////////


# read in i589_dep
i589_dep <- read_csv("data/I-589/i589_dep_20200408.csv",
                       col_types = cols(reopen_date = col_date()))

# inspect
i589_dep %>% glimpse()
i589_dep %>% nrow() # 287759
i589_dep %>% distinct(dep_anumber) %>% nrow() # 287759


#/#/#/#/#/#/#/#/#/#/#/#/#/


# read in ead data
# ead <- read_csv("data/I-765/i765_pa_raw.csv")

# inspect 
ead %>% glimpse()
ead %>% nrow() # 1222322
ead %>% distinct(anumber) %>% nrow()
data %>% distinct(anumber) %>% nrow()
ead %>% count(class_preference)
ead %>% count(part_2_1)
ead %>% count(ben_state)
ead %>% filter(is.na(ben_state)) %>% nrow()
ead %>% validate_anumbers("BEN_A_NUMBER")


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# create i589_filings_and_outcomes_by_fy_table ####

# inspect
data %>% count(outcome_bucket) %>% arrange(desc(n))
data %>% count(adjudicated_case_flag, outcome_bucket)
data %>% count(filing_date_fy)
data %>% count(eoir_outcome_date_fy)
# note that 71 admin_closed have NA outcome_dates, so for the first row of table, adj_count + admin_closed + new_pending will not exactly equal filing_count
# and for subsequent FY, this equation isn't expected to hold, since adjudicated outcomes could be for prior pending pool for cohort
data %>% mutate(na_outcome_date_fy = case_when(is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        count(na_outcome_date_fy, outcome_bucket)
data %>% mutate(na_outcome_date_fy = case_when(is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        filter(na_outcome_date_fy == 1, outcome_bucket == "admin_closed") %>% count(filing_date_fy)        


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get filings_by_fy
filings_by_fy <- data %>% 
        group_by(filing_date_fy) %>% count() %>% rename(fy = filing_date_fy, filing_count = n) %>% ungroup()
filings_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((filing_date_fy != outcome_date_fy) | is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(filing_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% filter(new_pending_flag == 1) %>%
        rename(fy = filing_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy

# get outcomes count/pct by outcome_fy
i589_filings_and_outcomes_by_fy_table <- data %>% 
        add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                grant_count = sum(outcome_bucket.grant),
                denial_count = sum(outcome_bucket.deny),
                referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                admin_closed_count = sum(outcome_bucket.admin_closed)) %>% 
        ungroup() %>%
        filter(!is.na(outcome_date_fy)) %>% 
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        rename(fy = outcome_date_fy) %>%
        left_join(., filings_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(cum_filing_count = cumsum(replace_na(filing_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_pending_count = cum_filing_count - cum_adj_count - cum_admin_closed_count) %>%
        select(-c(cum_filing_count, cum_adj_count, cum_admin_closed_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_fy_table

# test that adj_count equals the row sum of grants, denials, and referrals (note admin_closed are excluded since asylum officers dont evaluate them on merits)
expect_equal(object = i589_filings_and_outcomes_by_fy_table %>% pull(adj_count),
             expected = i589_filings_and_outcomes_by_fy_table %>% mutate(sum_of_outcomes = grant_count + denial_count + referral_count) %>%
                                                                         pull(sum_of_outcomes))

# test that grant/denial/referral rates sum to 1
expect_equal(object = i589_filings_and_outcomes_by_fy_table %>% mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>% 
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_fy_table %>% nrow()))


# test that for first row, filings - adj_count - admin_closed == new_pending_count
# note we don't expect this to sum the same for subsequent years, because adj_count includes adjudications of prior year filings
expect_equal(object = i589_filings_and_outcomes_by_fy_table %>% slice(1) %>% 
                     mutate(calculated_new_pending = filing_count - adj_count - admin_closed_count) %>% pull(calculated_new_pending),
             expected = i589_filings_and_outcomes_by_fy_table %>% slice(1) %>% pull(new_pending_count))

# check that cumulative sum of filing_count always equals cum sums of adj_count, admin_closed_count, and cum_pending_count
expect_equal(object = i589_filings_and_outcomes_by_fy_table %>% 
                     mutate(cum_adj_count = cumsum(adj_count),
                            cum_admin_closed_count = cumsum(admin_closed_count), 
                            cum_outcome_count = cum_adj_count + cum_admin_closed_count + cum_pending_count) %>%
                     pull(cum_outcome_count),
             expected = i589_filings_and_outcomes_by_fy_table %>% mutate(cum_filing_count = cumsum(replace_na(filing_count, replace = "0"))) %>% 
                     pull(cum_filing_count))
                
# check that sum of filing_count equals number of records in data
expect_equal(object = i589_filings_and_outcomes_by_fy_table %>% 
                     summarize(filing_count_sum = sum(filing_count, na.rm = TRUE)) %>%
                     pull(filing_count_sum),
             expected = data %>% nrow())     


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get i589_filings_and_outcomes_by_fy_table_unformatted
i589_filings_and_outcomes_by_fy_table_unformatted <- i589_filings_and_outcomes_by_fy_table %>% 
        select(fy, filing_count, adj_count, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, new_pending_count, cum_pending_count)

# get i589_filings_and_outcomes_by_fy_table_formatted
i589_filings_and_outcomes_by_fy_table_formatted <- i589_filings_and_outcomes_by_fy_table_unformatted %>%       
        mutate(filing_count = case_when(is.na(filing_count) ~ "-", TRUE ~ comma(filing_count)),
               adj_count = comma(adj_count),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               new_pending_count = case_when(is.na(new_pending_count) ~ "-", TRUE ~ comma(new_pending_count)),
               cum_pending_count = comma(cum_pending_count)) %>%
        select(fy, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, new_pending_count, cum_pending_count) %>%
        rename(FY = fy, Filed = filing_count, Adjudicated = adj_count, 
               Granted  = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Added to pending" = new_pending_count,
               "Cumulative pending" = cum_pending_count) 

# inspect
i589_filings_and_outcomes_by_fy_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by FY

# create initial workbook
workbook <- createWorkbook()

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                      "the respective counts as a share of adjudicated cases.",
                      sep = ""),
                "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_fy_table_formatted, 
                       output_sheet_names = "i589_outcomes_by_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = list(c(rep(NA, times = nrow(i589_filings_and_outcomes_by_fy_table_formatted) + 1), 15)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_fy_cohort_table ####
i589_filings_and_outcomes_by_fy_cohort_table <- data %>% add_dummies(outcome_bucket) %>% group_by(filing_date_fy) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_fy_cohort_table
i589_filings_and_outcomes_by_fy_cohort_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_fy_cohort_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_fy_cohort_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_fy_cohort_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_fy_cohort_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_fy_cohort_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_fy_cohort_table %>% pull(adj_count))

# test that sum of filing_count equals the number of i189 records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_fy_cohort_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_fy_cohort_table_unformatted
i589_filings_and_outcomes_by_fy_cohort_table_unformatted <- i589_filings_and_outcomes_by_fy_cohort_table %>% 
        select(filing_date_fy, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_fy_cohort_table_formatted
i589_filings_and_outcomes_by_fy_cohort_table_formatted <- i589_filings_and_outcomes_by_fy_cohort_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted  = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_fy_cohort_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                      "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                      "the respective counts as a share of adjudicated cases.",
                      sep = ""),
                "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_fy_cohort_table_formatted, 
                       output_sheet_names = "i589_outcomes_by_cohort_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_for_ewi_by_fy_cohort_table ####
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table <- data %>% 
        filter(status_at_entry == "EWI") %>%
        add_dummies(outcome_bucket) %>% group_by(filing_date_fy) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% pull(adj_count))

# test that sum of filing_count equals the number of i189 records records in dataset
expect_equal(object = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% filter(status_at_entry == "EWI") %>% nrow())

# manually inspect outliers
data %>% filter(filing_date_fy == 2013, status_at_entry == "EWI") %>% 
        summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag == 1), grant_count = sum(outcome_bucket == "grant"),
                  referral_count = sum(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit"))) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count, referral_pct = referral_count / adj_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_unformatted
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_unformatted <- i589_filings_and_outcomes_for_ewi_by_fy_cohort_table %>% 
        select(filing_date_fy, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_formatted
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_formatted <- i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted  = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_formatted 


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants who entered without inspection, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))
# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_for_ewi_by_fy_cohort_table_formatted, 
                       output_sheet_names = "i589_outcomes_ewi_by_cohort_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_country_table ####
i589_filings_and_outcomes_by_country_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
        add_dummies(outcome_bucket) %>% group_by(citizenship_output_country_name) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count) %>%
        arrange(desc(filing_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_country_table
i589_filings_and_outcomes_by_country_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_country_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_country_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_country_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_country_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_country_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_country_table %>% pull(adj_count))

# test that sum of filing_count equals the number of i189 records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_country_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Ecuador") %>% summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag),
                                                                            grant_count = sum(outcome_bucket == "grant")) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count)
                                                                            
data %>% filter(citizenship_output_country_name == "Venezuela") %>% summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag),
                                                                              grant_count = sum(outcome_bucket == "grant")) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_country_table_unformatted
i589_filings_and_outcomes_by_country_table_unformatted <- i589_filings_and_outcomes_by_country_table %>% 
        select(citizenship_output_country_name, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_country_table_formatted
i589_filings_and_outcomes_by_country_table_formatted <- i589_filings_and_outcomes_by_country_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count)) %>%
        select(citizenship_output_country_name, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("Applicant country" = citizenship_output_country_name,
               Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_country_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by top I-589 filing countries

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_country_table_formatted, 
                       output_sheet_names = "i589_outcomes_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_region_table ####
i589_filings_and_outcomes_by_region_table <- data %>%  
        add_dummies(outcome_bucket) %>% group_by(citizenship_region_name) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count) %>%
        arrange(desc(filing_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_region_table
i589_filings_and_outcomes_by_region_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_region_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_region_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_region_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_region_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_region_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_region_table %>% pull(adj_count))

# test that sum of filing_count equals the number of i189 records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_region_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% nrow())

# inspect outliers
data %>% filter(citizenship_region_name == "Asia") %>% count(citizenship_output_country_name) %>% arrange(desc(n))
data %>% filter(citizenship_region_name == "Asia") %>% summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag),
                                                                            grant_count = sum(outcome_bucket == "grant")) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count)

data %>% filter(citizenship_region_name == "North America") %>% count(citizenship_output_country_name) %>% arrange(desc(n))
data %>% filter(citizenship_region_name == "North America") %>% summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag),
                                                                              grant_count = sum(outcome_bucket == "grant")) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count)

data %>% filter(citizenship_region_name == "Caribbean") %>% count(citizenship_output_country_name) %>% arrange(desc(n))
data %>% filter(citizenship_region_name == "Caribbean") %>% summarize(filed_count = n(), adj_count = sum(adjudicated_case_flag),
                                                                          grant_count = sum(outcome_bucket == "grant")) %>%
        mutate(adj_pct = adj_count / filed_count, grant_pct = grant_count / adj_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_region_table_unformatted
i589_filings_and_outcomes_by_region_table_unformatted <- i589_filings_and_outcomes_by_region_table %>% 
        select(citizenship_region_name, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_region_table_formatted
i589_filings_and_outcomes_by_region_table_formatted <- i589_filings_and_outcomes_by_region_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count)) %>%
        select(citizenship_region_name, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("Applicant region" = citizenship_region_name,
               Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted  = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_region_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by region

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_region_table_formatted , 
                       output_sheet_names = "i589_outcomes_by_region", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_income_table ####
i589_filings_and_outcomes_by_income_table <- data %>%
        add_dummies(outcome_bucket) %>% group_by(income_group_bucket) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count) %>%
        arrange(desc(filing_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_income_table
i589_filings_and_outcomes_by_income_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_income_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_income_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_income_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_income_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_income_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_income_table %>% pull(adj_count))

# test that sum of filing_count equals the number of  records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_income_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_income_table_unformatted
i589_filings_and_outcomes_by_income_table_unformatted <- i589_filings_and_outcomes_by_income_table %>% 
        select(income_group_bucket, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_income_table_formatted
i589_filings_and_outcomes_by_income_table_formatted <- i589_filings_and_outcomes_by_income_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(income_group_bucket, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("Income group" = income_group_bucket,
               Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_income_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by country income groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: World Bank; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_income_table_formatted, 
                       output_sheet_names = "i589_outcomes_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_homicide_table ####
i589_filings_and_outcomes_by_homicide_table <- data %>%
        add_dummies(outcome_bucket) %>% group_by(homicide_bucket) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count) %>%
        arrange(desc(filing_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_homicide_table
i589_filings_and_outcomes_by_homicide_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_homicide_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_homicide_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_homicide_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_homicide_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_homicide_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_homicide_table %>% pull(adj_count))

# test that sum of filing_count equals the number of  records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_homicide_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_homicide_table_unformatted
i589_filings_and_outcomes_by_homicide_table_unformatted <- i589_filings_and_outcomes_by_homicide_table %>% 
        select(homicide_bucket, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_homicide_table_formatted
i589_filings_and_outcomes_by_homicide_table_formatted <- i589_filings_and_outcomes_by_homicide_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                              homicide_bucket == "1.1 to 10" ~ 2,
                                              homicide_bucket == "10.1 to 20" ~ 3,
                                              homicide_bucket == "20.1 to 40" ~ 4,
                                              homicide_bucket == "> 40" ~ 5,
                                          homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(homicide_bucket, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("Homicide rate per 100k population" = homicide_bucket,
               Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_homicide_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by country homicide groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_homicide_table_formatted , 
                       output_sheet_names = "i589_outcomes_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15)), custom_row_height = list(c(50)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create i589_filings_and_outcomes_by_prcl_table ####
i589_filings_and_outcomes_by_prcl_table <- data %>%
        add_dummies(outcome_bucket) %>% group_by(prcl_bucket) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed),
                  pending_count = sum(outcome_bucket.pending)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count,
               adj_pct = adj_count / filing_count) %>%
        arrange(desc(filing_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_filings_and_outcomes_by_prcl_table
i589_filings_and_outcomes_by_prcl_table %>% glimpse()

# test that sum of grant/deny/admin_closed/referral/pending = filed
expect_equal(object = i589_filings_and_outcomes_by_prcl_table %>% 
                     mutate(outcome_sum = grant_count + denial_count + referral_count + admin_closed_count + pending_count) %>%
                     pull(outcome_sum),
             expected = i589_filings_and_outcomes_by_prcl_table %>% pull(filing_count))

# test that sum of grant/denial/referral pcts = 1
expect_equal(object = i589_filings_and_outcomes_by_prcl_table %>% 
                     mutate(pct_sum = grant_pct + denial_pct + referral_pct) %>%
                     pull(pct_sum),
             expected = rep(1, times = i589_filings_and_outcomes_by_prcl_table %>% nrow()))

# test that sum of grant/denial/referral equals adj_count
expect_equal(object = i589_filings_and_outcomes_by_prcl_table %>%
                     mutate(adj_sum = grant_count + denial_count + referral_count) %>% pull(adj_sum),
             expected = i589_filings_and_outcomes_by_prcl_table %>% pull(adj_count))

# test that sum of filing_count equals the number of  records records in dataset
expect_equal(object = i589_filings_and_outcomes_by_prcl_table %>% summarize(filing_count_sum = sum(filing_count)) %>% pull(filing_count_sum),
             expected = data %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_filings_and_outcomes_by_prcl_table_unformatted
i589_filings_and_outcomes_by_prcl_table_unformatted <- i589_filings_and_outcomes_by_prcl_table %>% 
        select(prcl_bucket, filing_count, adj_count, adj_pct, grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct,
               admin_closed_count, pending_count)


# get i589_filings_and_outcomes_by_prcl_table_formatted
i589_filings_and_outcomes_by_prcl_table_formatted <- i589_filings_and_outcomes_by_prcl_table_unformatted %>%       
        mutate(filing_count = comma(filing_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               grant_pct = as_percent(grant_pct, digits = 0),
               denial_pct = as_percent(denial_pct, digits = 0),
               referral_pct = as_percent(referral_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               admin_closed_count = comma(admin_closed_count),
               pending_count = comma(pending_count),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                          prcl_bucket == "25 to 49" ~ 2,
                                          prcl_bucket == "50 to 74" ~ 3,
                                          prcl_bucket == "75 to 100" ~ 4,
                                      prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(prcl_bucket, filing_count, adj_count, grant_count, denial_count, referral_count, 
               admin_closed_count, pending_count) %>%
        rename("Total\npolitical rights\n& civil liberties\nscore" = prcl_bucket,
               Filed = filing_count, 
               Adjudicated = adj_count, 
               Granted = grant_count, 
               Denied = denial_count, 
               Referred = referral_count, 
               "Admin. closed" = admin_closed_count,
               "Pending" = pending_count) 

# inspect
i589_filings_and_outcomes_by_prcl_table_formatted 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by country political rights / civil liberties groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ", 
                                        "The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: Freedom House; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_filings_and_outcomes_by_prcl_table_formatted, 
                       output_sheet_names = "i589_outcomes_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = c(15), 
                       custom_row_height = c(75, rep(NA, times = nrow(i589_filings_and_outcomes_by_prcl_table_formatted)), 40), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_fy_table ####

# inspect
data %>% count(eoir_outcome) %>% arrange(desc(n))

# inspect eoir_case_received_date
# note that 302 anumbers with pre-2009 case_received_dates will be excluded from table
data %>% filter(eoir_received_flag == 1, is.na(eoir_case_received_date)) %>% nrow() # 0
data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received = n()) %>% print(n = nrow(.))
data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received = n()) %>% 
        filter(eoir_case_received_date_fy < 2009) %>% summarize(sum = sum(referral_received))

# inspect raio referrals with eoir_received
# note also that ~6000 (~3%) anumbers referred by RAIO do not have a matching EOIR record
data %>% count(eoir_received_flag, outcome_bucket) %>% arrange(eoir_received_flag, desc(n))
data %>% filter(outcome_bucket %in% c("referral_w_interview", "referral_w_one_year_limit")) %>% nrow() # 214239
data %>% filter(!is.na(eoir_case_received_date)) %>% nrow() # 208073
data %>% count(eoir_received_flag) # 208073
214239 - 208073 == 6166
6166 / 214239

# check grant rate by adj duration to see if there's risk of bias from early year adjudication pool being unrepresentative 
# eg, maybe only easy denials get adjudicated in the same year as referral, which could bias 2009, though not later years once steady state mix is achieved
data %>% glimpse() 
data %>% mutate(eoir_case_received_to_outcome_years = ceiling(eoir_case_received_to_outcome_days / 365)) %>%
        count(eoir_case_received_to_outcome_years)
# inspect distribution of case_received_to_outcome_years by case_received_fy
data %>% mutate(eoir_case_received_to_outcome_years = ceiling(eoir_case_received_to_outcome_days / 365)) %>%
        group_by(eoir_case_received_date_fy) %>% summarize(median_case_received_to_outcome_years = median(eoir_case_received_to_outcome_years, na.rm = TRUE))
data %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days, y = factor(eoir_case_received_date_fy), height = ..density..)) +
        geom_density_ridges(stat = "density", trim = TRUE)
# inspect rates by received_to_outcome_years - there does appear to be some variability in rates attributed to case duration
# so the relatively high proportion of short duration cases in early referral_fy cohorts could add bias
# will need to add footnote caveating 
data %>% mutate(eoir_case_received_to_outcome_years = ceiling(eoir_case_received_to_outcome_days / 365)) %>%
        add_dummies(eoir_outcome) %>%
        group_by(eoir_case_received_to_outcome_years) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag == 1),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  relief_granted_rate = relief_granted_count / adj_count,
                  removal_rate = removal_count / adj_count,
                  cancellation_of_removal_rate = cancellation_of_removal_count / adj_count,
                  terminated_rate = terminated_count / adj_count,
                  voluntary_departure_rate = voluntary_departure_count / adj_count) %>%
        select(eoir_case_received_to_outcome_years, contains("_rate"))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get referral_received_by_fy
referral_received_by_fy <- data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = eoir_case_received_date_fy)
referral_received_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((eoir_case_received_date_fy != eoir_outcome_date_fy) | is.na(eoir_outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(eoir_case_received_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% 
        filter(new_pending_flag == 1, eoir_case_received_date_fy >= 2009) %>%
        rename(fy = eoir_case_received_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy


# get eoir_referrals_and_outcomes_by_fy_table
eoir_referrals_and_outcomes_by_fy_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other)) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>%
        rename(fy = eoir_outcome_date_fy) %>%
        left_join(., referral_received_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               admin_closed_other_count = admin_closed_count + other_count,
               cum_referral_received_count = cumsum(replace_na(referral_received_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_other_count = cumsum(replace_na(other_count, replace = "0")),
               cum_pending_count = cum_referral_received_count - cum_adj_count - cum_admin_closed_count - cum_other_count) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, new_pending_count, cum_pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_fy_table
eoir_referrals_and_outcomes_by_fy_table %>% data.frame()
eoir_referrals_and_outcomes_by_fy_table %>% glimpse()
eoir_referrals_and_outcomes_by_fy_table %>% select(fy, other_count, admin_closed_count)

# test that sum of counts for relief/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_fy_table %>% 
                     mutate(sum = relief_granted_count + removal_vol_dep_count + terminated_count) %>%
                        pull(sum),
             expected = eoir_referrals_and_outcomes_by_fy_table %>% pull(adj_count))

# test that sum of rates for relief/removal/cancellation/terminated/vol_dep equals 1
expect_equal(object = eoir_referrals_and_outcomes_by_fy_table %>% 
                     mutate(rate_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(rate_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_fy_table %>% nrow()))

# test that for first row, referrals - adj_count - admin_closed_other == new_pending_count
# note we don't expect this to sum the same for subsequent years, because adj_count includes adjudications of prior year filings
expect_equal(object = eoir_referrals_and_outcomes_by_fy_table %>% slice(1) %>% 
                     mutate(calculated_new_pending = referral_received_count - adj_count - admin_closed_other_count) %>% pull(calculated_new_pending),
             expected = eoir_referrals_and_outcomes_by_fy_table %>% slice(1) %>% pull(new_pending_count))

# check that cumulative sum of referral_count always equals cum sums of adj_count, admin_closed_count, other_count, and cum_pending_count
expect_equal(object = eoir_referrals_and_outcomes_by_fy_table %>% 
                     mutate(cum_adj_count = cumsum(adj_count),
                            cum_admin_closed_other_count = cumsum(admin_closed_other_count), 
                            cum_outcome_count = cum_adj_count + cum_admin_closed_other_count + cum_pending_count) %>%
                     pull(cum_outcome_count),
             expected = eoir_referrals_and_outcomes_by_fy_table %>% 
                     mutate(cum_referral_received_count = cumsum(referral_received_count)) %>% pull(cum_referral_received_count))


# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_fy_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# eoir_referrals_and_outcomes_by_fy_table_unformatted
eoir_referrals_and_outcomes_by_fy_table_unformatted <- eoir_referrals_and_outcomes_by_fy_table %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
                terminated_count, terminated_rate,
               admin_closed_other_count, new_pending_count, cum_pending_count)

# get eoir_referrals_and_outcomes_by_fy_table_formatted
eoir_referrals_and_outcomes_by_fy_table_formatted <- eoir_referrals_and_outcomes_by_fy_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_count = comma(adj_count),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count),
               new_pending_count = comma(new_pending_count),
               cum_pending_count = comma(cum_pending_count)) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, new_pending_count, cum_pending_count) %>%
        rename(FY = fy, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count,
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               Terminated = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count, 
               "Added to pending" = new_pending_count, "Cumulative pending" = cum_pending_count)

# inspect
eoir_referrals_and_outcomes_by_fy_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by FY

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_fy_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_fy_cohort_table ####

# get referral_received_by_filing_fy
referral_received_by_filing_fy <- data %>% group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_by_filing_fy

# get eoir_referrals_and_outcomes_by_fy_cohort_table
eoir_referrals_and_outcomes_by_fy_cohort_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
                relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
              removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_fy_cohort_table
eoir_referrals_and_outcomes_by_fy_cohort_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_fy_cohort_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_fy_cohort_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_fy_cohort_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_fy_cohort_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_fy_cohort_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_fy_cohort_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_fy_cohort_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_fy_cohort_table_unformatted
eoir_referrals_and_outcomes_by_fy_cohort_table_unformatted <- eoir_referrals_and_outcomes_by_fy_cohort_table %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_fy_cohort_table_formatted
eoir_referrals_and_outcomes_by_fy_cohort_table_formatted <- eoir_referrals_and_outcomes_by_fy_cohort_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count), 
               fy = str_c(fy, " cohort")) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = fy, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_fy_cohort_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ", 
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_fy_cohort_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_cohort_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, 
                       custom_row_height = list(c(rep(NA, times = nrow(eoir_referrals_and_outcomes_by_fy_cohort_table_formatted) + 1), 40)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table ####

# inspect
data %>% filter(status_at_entry == "EWI") %>% count(eoir_received_flag)

# get referral_received_for_ewi_by_filing_fy
referral_received_for_ewi_by_filing_fy <- data %>% filter(status_at_entry == "EWI") %>%
        group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_for_ewi_by_filing_fy

# get eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table <- data %>% 
        filter(status_at_entry == "EWI") %>%
        add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_for_ewi_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1, status_at_entry == "EWI") %>% nrow())

# inspect
data %>% filter(status_at_entry == "EWI", filing_date_fy == 2009, eoir_received_flag == 1, eoir_outcome == "pending") %>% nrow()
data %>% filter(status_at_entry == "EWI", filing_date_fy == 2009, eoir_received_flag == 1, eoir_outcome == "pending") %>%
        select(filing_date, status_at_entry, decision_outcome, eoir_case_received_date, eoir_ij_decision, eoir_ij_decision_date)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_unformatted
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_unformatted <- eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_formatted
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_formatted <- eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count), 
               fy = str_c(fy, " cohort")) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = fy, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants who entered the U.S. without inspection, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                      "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                      "the respective counts as a share of adjudicated cases.",
                      sep = ""),
                "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_for_ewi_by_fy_cohort_table_formatted, 
                       output_sheet_names = "eoir_outcomes_ewi_by_cohort_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table ####

# inspect
# note that 2955 with outcome_bucket = referral_w_one_year_limit, did not get received by eoir
data %>% filter(outcome_bucket == "referral_w_one_year_limit") %>% count(eoir_received_flag)

# get referral_received_for_referral_w_one_year_limit_by_filing_fy
referral_received_for_referral_w_one_year_limit_by_filing_fy <- data %>% 
        filter(outcome_bucket == "referral_w_one_year_limit", eoir_received_flag == 1) %>%
        group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_for_referral_w_one_year_limit_by_filing_fy

# get eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table <- data %>% 
        filter(outcome_bucket == "referral_w_one_year_limit", eoir_received_flag == 1) %>%
        add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_for_referral_w_one_year_limit_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1, outcome_bucket == "referral_w_one_year_limit") %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted <- eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_formatted
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_formatted <- eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count), 
               fy = str_c(fy, " cohort")) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = fy, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants who were referred by USCIS based on the one year filing deadline, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_for_referral_w_one_year_limit_by_fy_cohort_table_formatted, 
                       output_sheet_names = "eoir_outcomes_ref_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table ####

# inspect
data %>% count(eoir_received_flag, eoir_outcome) %>% arrange(eoir_received_flag, desc(n))
data %>% count(eoir_cancellation_applied, eoir_received_flag) %>% arrange(eoir_cancellation_applied, desc(n))

# get referral_received_for_cancellation_of_removal_appl_by_filing_fy
referral_received_for_cancellation_of_removal_appl_by_filing_fy <- data %>% 
        filter(eoir_cancellation_applied == 1) %>%
        group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_for_cancellation_of_removal_appl_by_filing_fy

# get eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table <- data %>% 
        filter(eoir_cancellation_applied == 1) %>%
        add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_for_cancellation_of_removal_appl_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_cancellation_applied == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted <- eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table %>%
        select(fy, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted <- eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count), 
               fy = str_c(fy, " cohort")) %>%
        select(fy, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("I-589 FY\nfiling cohort" = fy, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants who applied for cancellation of removal, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted, 
                       output_sheet_names = "eoir_outcomes_cor_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_country_table ####

# get referral_received_by_filing_fy
referral_received_by_country <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
        group_by(citizenship_output_country_name) %>%
        summarize(referral_received_count = sum(eoir_received_flag))
referral_received_by_country

# get eoir_referrals_and_outcomes_by_country_table
eoir_referrals_and_outcomes_by_country_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
        add_dummies(eoir_outcome) %>%
        group_by(citizenship_output_country_name) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        ungroup() %>%
        left_join(referral_received_by_country, ., by = "citizenship_output_country_name") %>%
        arrange(desc(referral_received_count)) %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(citizenship_output_country_name, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#

# inspect
eoir_referrals_and_outcomes_by_country_table
eoir_referrals_and_outcomes_by_country_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_country_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_country_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_country_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_country_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_country_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_country_table %>% pull(adj_count))

# check that sum of referrals received in table equals sum of referrals received in data
expect_equal(object = eoir_referrals_and_outcomes_by_country_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
                     filter(eoir_received_flag == 1) %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "China", eoir_received_flag == 1) %>%
        add_dummies(eoir_outcome) %>%
        summarize(referred_count = n(), adj_count = sum(eoir_adjudicated_flag), 
                  grant_count = sum(eoir_outcome.relief_granted),
                  cor_count = sum(eoir_outcome.cancellation_of_removal)) %>%
        mutate(grant_count = grant_count + cor_count,
                adj_pct = adj_count / referred_count,
               grant_pct = grant_count / adj_count)
data %>% filter(citizenship_output_country_name == "Mexico", eoir_received_flag == 1) %>%
        add_dummies(eoir_outcome) %>%
        summarize(referred_count = n(), adj_count = sum(eoir_adjudicated_flag), 
                  grant_count = sum(eoir_outcome.relief_granted),
                  cor_count = sum(eoir_outcome.cancellation_of_removal)) %>%
        mutate(grant_count = grant_count + cor_count,
               adj_pct = adj_count / referred_count,
               grant_pct = grant_count / adj_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_country_table_unformatted
eoir_referrals_and_outcomes_by_country_table_unformatted <- eoir_referrals_and_outcomes_by_country_table %>%
        select(citizenship_output_country_name, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_country_table_formatted
eoir_referrals_and_outcomes_by_country_table_formatted <- eoir_referrals_and_outcomes_by_country_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count)) %>%
        select(citizenship_output_country_name, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("Applicant country" = citizenship_output_country_name, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_country_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by top I-589 filing countries

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_country_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, 
                       custom_row_height = list(c(40)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_region_table ####

# get referral_received_by_filing_fy
referral_received_by_region <- data %>% group_by(citizenship_region_name) %>% summarize(referral_received_count = sum(eoir_received_flag)) 
referral_received_by_region

# get eoir_referrals_and_outcomes_by_region_table
eoir_referrals_and_outcomes_by_region_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(citizenship_region_name) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        left_join(., referral_received_by_region, by = "citizenship_region_name") %>%
        arrange(desc(referral_received_count)) %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(citizenship_region_name, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_region_table
eoir_referrals_and_outcomes_by_region_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_region_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_region_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_region_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_region_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_region_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_region_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_region_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# manually inspect outliers
data %>% filter(citizenship_region_name == "Asia") %>% count(citizenship_output_country_name) %>% arrange(desc(n))
data %>% filter(citizenship_region_name == "Asia", eoir_received_flag == 1) %>% 
        add_dummies(eoir_outcome) %>%
        summarize(referral_received_count = n(), adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  removal_count = sum(eoir_outcome.removal), 
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        mutate(adj_pct = adj_count / referral_received_count, relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               relief_granted_pct = relief_granted_count / adj_count,
               removal_vol_dep_pct = removal_vol_dep_count / adj_count) %>%
        select(-c(cancellation_of_removal_count, removal_count, voluntary_departure_count))


data %>% filter(citizenship_region_name == "Central America") %>% count(citizenship_output_country_name) %>% arrange(desc(n))
data %>% filter(citizenship_region_name == "Central America", eoir_received_flag == 1) %>% 
        add_dummies(eoir_outcome) %>%
        summarize(referral_received_count = n(), adj_count = sum(eoir_adjudicated_flag),
                        relief_granted_count = sum(eoir_outcome.relief_granted),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  removal_count = sum(eoir_outcome.removal), 
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        mutate(adj_pct = adj_count / referral_received_count, relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               relief_granted_pct = relief_granted_count / adj_count,
               removal_vol_dep_pct = removal_vol_dep_count / adj_count) %>%
        select(-c(cancellation_of_removal_count, removal_count, voluntary_departure_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_region_table_unformatted
eoir_referrals_and_outcomes_by_region_table_unformatted <- eoir_referrals_and_outcomes_by_region_table %>%
        select(citizenship_region_name, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_region_table_formatted
eoir_referrals_and_outcomes_by_region_table_formatted <- eoir_referrals_and_outcomes_by_region_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count)) %>%
        select(citizenship_region_name, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("Applicant country region" = citizenship_region_name, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_region_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by applicant region

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_region_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_region", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, 
                       custom_row_height = list(c(rep(NA, times = nrow(eoir_referrals_and_outcomes_by_region_table_formatted) + 1), 30)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_income_table ####

# get referral_received_by_filing_fy
referral_received_by_income <- data %>% group_by(income_group_bucket) %>% summarize(referral_received_count = sum(eoir_received_flag)) 
referral_received_by_income

# get eoir_referrals_and_outcomes_by_income_table
eoir_referrals_and_outcomes_by_income_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(income_group_bucket) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        left_join(., referral_received_by_income, by = "income_group_bucket") %>%
        arrange(desc(referral_received_count)) %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(income_group_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_income_table
eoir_referrals_and_outcomes_by_income_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_income_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_income_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_income_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_income_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_income_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_income_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_income_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_income_table_unformatted
eoir_referrals_and_outcomes_by_income_table_unformatted <- eoir_referrals_and_outcomes_by_income_table %>%
        select(income_group_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_income_table_formatted
eoir_referrals_and_outcomes_by_income_table_formatted <- eoir_referrals_and_outcomes_by_income_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(income_group_bucket, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("Income group" = income_group_bucket, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_income_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by country income grouping

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: World Bank; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_income_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, 
                       custom_row_height = list(c(rep(NA, times = nrow(eoir_referrals_and_outcomes_by_income_table_formatted) + 1), 40)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_homicide_table ####

# get referral_received_by_filing_fy
referral_received_by_homicide <- data %>% group_by(homicide_bucket) %>% summarize(referral_received_count = sum(eoir_received_flag)) 
referral_received_by_homicide

# get eoir_referrals_and_outcomes_by_homicide_table
eoir_referrals_and_outcomes_by_homicide_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(homicide_bucket) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        left_join(., referral_received_by_homicide, by = "homicide_bucket") %>%
        arrange(desc(referral_received_count)) %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(homicide_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_homicide_table
eoir_referrals_and_outcomes_by_homicide_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_homicide_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_homicide_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_homicide_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_homicide_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_homicide_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_homicide_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_homicide_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_homicide_table_unformatted
eoir_referrals_and_outcomes_by_homicide_table_unformatted <- eoir_referrals_and_outcomes_by_homicide_table %>%
        select(homicide_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_homicide_table_formatted
eoir_referrals_and_outcomes_by_homicide_table_formatted <- eoir_referrals_and_outcomes_by_homicide_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                              homicide_bucket == "1.1 to 10" ~ 2,
                                              homicide_bucket == "10.1 to 20" ~ 3,
                                              homicide_bucket == "20.1 to 40" ~ 4,
                                              homicide_bucket == "> 40" ~ 5,
                                          homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(homicide_bucket, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("Homicide rate\nper\n100k population" = homicide_bucket, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_homicide_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by country homicide grouping

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                  "Source: United Nations; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_homicide_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15)), custom_row_height = list(c(60)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_referrals_and_outcomes_by_prcl_table ####

# get referral_received_by_filing_fy
referral_received_by_prcl <- data %>% group_by(prcl_bucket) %>% summarize(referral_received_count = sum(eoir_received_flag)) 
referral_received_by_prcl

# get eoir_referrals_and_outcomes_by_prcl_table
eoir_referrals_and_outcomes_by_prcl_table <- data %>% add_dummies(eoir_outcome) %>%
        group_by(prcl_bucket) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other),
                  pending_count = sum(eoir_outcome.pending)) %>%
        left_join(., referral_received_by_prcl, by = "prcl_bucket") %>%
        arrange(desc(referral_received_count)) %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               admin_closed_other_count = admin_closed_count + other_count) %>%
        select(prcl_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate,
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate, 
               admin_closed_other_count, pending_count)


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
eoir_referrals_and_outcomes_by_prcl_table
eoir_referrals_and_outcomes_by_prcl_table %>% glimpse()

# test that sum of grant/removal/cancellation/terminated/vol_dep/admin_closed/pending = filed
expect_equal(object = eoir_referrals_and_outcomes_by_prcl_table %>% 
                     mutate(outcome_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count + admin_closed_other_count + pending_count) %>%
                     pull(outcome_sum),
             expected = eoir_referrals_and_outcomes_by_prcl_table %>% pull(referral_received_count))

# test that sum of grant/removal/cancellation/terminated/vol_dep pcts = 1
expect_equal(object = eoir_referrals_and_outcomes_by_prcl_table %>% 
                     mutate(pct_sum = relief_granted_rate + removal_vol_dep_rate + terminated_rate) %>%
                     pull(pct_sum),
             expected = rep(1, times = eoir_referrals_and_outcomes_by_prcl_table %>% nrow()))

# test that sum of grant/removal/cancellation/terminated/vol_dep equals adj_count
expect_equal(object = eoir_referrals_and_outcomes_by_prcl_table %>%
                     mutate(adj_sum = relief_granted_count + removal_vol_dep_count + 
                                    terminated_count) %>% pull(adj_sum),
             expected = eoir_referrals_and_outcomes_by_prcl_table %>% pull(adj_count))

# check that sum of filing_count equals count of eoir_received_flag == 1
expect_equal(object = eoir_referrals_and_outcomes_by_prcl_table %>% 
                     summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_referrals_and_outcomes_by_prcl_table_unformatted
eoir_referrals_and_outcomes_by_prcl_table_unformatted <- eoir_referrals_and_outcomes_by_prcl_table %>%
        select(prcl_bucket, referral_received_count, adj_count, adj_pct, relief_granted_count, relief_granted_rate, 
               removal_vol_dep_count, removal_vol_dep_rate,
               terminated_count, terminated_rate,
               admin_closed_other_count, pending_count)

# get eoir_referrals_and_outcomes_by_prcl_table_formatted
eoir_referrals_and_outcomes_by_prcl_table_formatted <- eoir_referrals_and_outcomes_by_prcl_table_unformatted %>% 
        mutate(referral_received_count = comma(referral_received_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               relief_granted_rate = as_percent(relief_granted_rate, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_rate) == 3, " (", "   ("), relief_granted_rate, ")"),
               removal_vol_dep_rate = as_percent(removal_vol_dep_rate, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_rate) == 3, " (", "   ("), removal_vol_dep_rate, ")"),
               terminated_rate = as_percent(terminated_rate, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_rate) == 3, " (", "   ("), terminated_rate, ")"),
               admin_closed_other_count = comma(admin_closed_other_count), 
               pending_count = comma(pending_count),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                          prcl_bucket == "25 to 49" ~ 2,
                                          prcl_bucket == "50 to 74" ~ 3,
                                          prcl_bucket == "75 to 100" ~ 4,
                                      prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(prcl_bucket, referral_received_count, adj_count, relief_granted_count, removal_vol_dep_count, 
               terminated_count, admin_closed_other_count, pending_count) %>%
        rename("Total\npolitical rights\n& civil liberties\nscore" = prcl_bucket, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Relief granted" = relief_granted_count, 
               "Removal / Voluntary departure" = removal_vol_dep_count, 
               "Terminated" = terminated_count, 
               "Admin. closed / Other" = admin_closed_other_count,
               "Pending" = pending_count)

# inspect
eoir_referrals_and_outcomes_by_prcl_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by country political rights / civil liberties grouping

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ", 
                                        "The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases.",
                                        sep = ""),
                                "Source: Freedom House; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_referrals_and_outcomes_by_prcl_table_formatted, 
                       output_sheet_names = "eoir_outcomes_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = c(15), custom_row_height = c(75, rep(NA, times = nrow(eoir_referrals_and_outcomes_by_prcl_table_formatted)), 40), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create overall_asylum_or_relief_by_cohort_fy_table ####

# get all_applicants_overall_asylum_or_relief
all_applicants_overall_asylum_or_relief <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                  overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        mutate(completed_case_pct = completed_case_count / filed_count,
                overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
        select(filing_date_fy, filed_count, completed_case_count, completed_case_pct, overall_asylum_or_relief_count, overall_asylum_or_relief_pct)

# inspect
all_applicants_overall_asylum_or_relief


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get ewi_overall_asylum_or_relief
ewi_overall_asylum_or_relief <- data %>% 
        filter(status_at_entry == "EWI") %>%
        group_by(filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                  overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        mutate(completed_case_pct = completed_case_count / filed_count,
               overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
        select(filing_date_fy, overall_asylum_or_relief_pct) %>%
        rename("Overall relief\ngranted rate\namong\n EWI" = overall_asylum_or_relief_pct)


# inspect
ewi_overall_asylum_or_relief


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get cancellation_of_removal_applicants_overall_asylum_or_relief
cancellation_of_removal_applicants_overall_asylum_or_relief <- data %>% 
        filter(eoir_cancellation_applied == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                  overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(completed_case_share = completed_case_count / filed_count,
               overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
        select(filing_date_fy, overall_asylum_or_relief_pct) %>%
        rename("Overall relief\ngranted rate\namong\ncancellation of removal applicants" = overall_asylum_or_relief_pct)


# inspect
cancellation_of_removal_applicants_overall_asylum_or_relief


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get referral_w_one_year_limit_applicants_overall_asylum_or_relief
referral_w_one_year_overall_asylum_or_relief <- data %>% 
        filter(outcome_bucket == "referral_w_one_year_limit") %>%
        group_by(filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                  overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(completed_case_share = completed_case_count / filed_count,
               overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
        select(filing_date_fy, overall_asylum_or_relief_pct) %>%
        rename("Overall relief\ngranted rate\namong\nreferrals for 1-year filing deadline" = overall_asylum_or_relief_pct)


# inspect
referral_w_one_year_overall_asylum_or_relief


#/#/#/#/#/#/#/#/#/#/#/#/#/


# combine into overall_asylum_or_relief_by_cohort_fy_table
overall_asylum_or_relief_by_cohort_fy_table <- all_applicants_overall_asylum_or_relief %>% 
        left_join(., ewi_overall_asylum_or_relief, by = "filing_date_fy") %>%
        left_join(., referral_w_one_year_overall_asylum_or_relief, by = "filing_date_fy") %>%
        left_join(., cancellation_of_removal_applicants_overall_asylum_or_relief, by = "filing_date_fy")


#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
overall_asylum_or_relief_by_cohort_fy_table 
overall_asylum_or_relief_by_cohort_fy_table %>% glimpse()

# test that sum of filed_count in table equals total i-589 applications in data
expect_equal(object = overall_asylum_or_relief_by_cohort_fy_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# test that sum of completed cases in table equals sum of completed cases in data
expect_equal(object = overall_asylum_or_relief_by_cohort_fy_table %>% summarize(completed_case_count_sum = sum(completed_case_count)) %>%
                     pull(completed_case_count_sum),
             expected = data %>% filter(outcome_bucket %in% c("grant", "deny") | 
                                                eoir_outcome %in% c("relief_granted", "removal", "cancellation_of_removal", 
                                                                    "terminated", "voluntary_departure")) %>% nrow())

# test that sum of overall_asylum/relief cases in table equals sum of overall_asylum/relief cases in data
expect_equal(object = overall_asylum_or_relief_by_cohort_fy_table %>% 
                     summarize(overall_asylum_or_relief_count_sum = sum(overall_asylum_or_relief_count)) %>%
                     pull(overall_asylum_or_relief_count_sum),
             expected = data %>% filter(outcome_bucket == "grant" | 
                                                eoir_outcome %in% c("cancellation_of_removal", "relief_granted")) %>% nrow())

# inspect
data %>% filter(filing_date_fy == 2014, outcome_bucket == "referral_w_one_year_limit") %>% 
        summarize(filed_count = n(), 
                  adj_count = sum(eoir_outcome %in% c("relief_granted", "cancellation_of_removal", "removal", "terminated", "voluntary_departure")),
                  relief_granted_count = sum(eoir_outcome %in% c("relief_granted", "cancellation_of_removal")),
                  relief_granted_pct = relief_granted_count / adj_count)


#/#/#/#/#/#/#/#/#/#/


# get overall_asylum_or_relief_by_cohort_fy_table_formatted
overall_asylum_or_relief_by_cohort_fy_table_formatted <- overall_asylum_or_relief_by_cohort_fy_table %>%
        mutate(filed_count = comma(filed_count),
               completed_case_pct = as_percent(completed_case_pct, digits = 0),
               completed_case_count = str_c(comma(completed_case_count), ifelse(nchar(completed_case_pct) == 3, " (", "   ("), completed_case_pct, ")"),
               overall_asylum_or_relief_count = comma(overall_asylum_or_relief_count),
               overall_asylum_or_relief_pct = as_percent(overall_asylum_or_relief_pct, digits = 0)) %>%
        mutate_at(.vars = vars(-one_of(c("filing_date_fy", "filed_count", "completed_case_count", "completed_case_pct", 
                "overall_asylum_or_relief_count", "overall_asylum_or_relief_pct"))), 
                .funs = ~ as_percent(., digits = 0)) %>%
        mutate(filing_date_fy = str_c(filing_date_fy, "cohort", sep = " ")) %>%
        select(-completed_case_pct) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, "Filed" = filed_count, "Terminally adjudicated cases" = completed_case_count,
               "Overall relief\ngranted\namong\nterminally adjudicated cases" = overall_asylum_or_relief_count,
               "Overall relief\ngranted rate\namong\nterminally adjudicated cases" = overall_asylum_or_relief_pct)
        
               
# inspect
overall_asylum_or_relief_by_cohort_fy_table_formatted
overall_asylum_or_relief_by_cohort_fy_table_formatted %>% glimpse()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: Overall relief granted rate for I-589 applicants, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: 'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Terminally adjudicated cases' percentage in parenthesis is the count of terminally adjudicated cases ", 
                                        "as a share of filings. ",
                                        "Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ", 
                                        "had overall relief granted from either RAIO or EOIR.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = overall_asylum_or_relief_by_cohort_fy_table_formatted, 
                       output_sheet_names = "overall_relief_by_cohort_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), rep(12, times = 5))), 
                       custom_row_height = list(c(100, rep(NA, times = nrow(overall_asylum_or_relief_by_cohort_fy_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create country_overall_asylum_or_relief_by_cohort_fy_table ####
country_overall_asylum_or_relief <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                  overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                          distinct(citizenship_output_country_name),
                  ., by = "citizenship_output_country_name") %>%
        mutate(completed_case_share = completed_case_count / filed_count,
               overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
       pivot_longer(cols = -c(filing_date_fy, citizenship_output_country_name), names_to = "var", values_to = "value") %>%
        mutate(var = case_when(var == "overall_asylum_or_relief_pct" ~ "overall relief %", TRUE ~ var),
                var = str_c(citizenship_output_country_name, var, sep = " ")) %>%
        select(-citizenship_output_country_name) %>%
        pivot_wider(names_from = var, values_from = value)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
country_overall_asylum_or_relief
country_overall_asylum_or_relief %>% glimpse()

# test that sum of filed_count in the table equals the total filings in the data
expect_equal(object = country_overall_asylum_or_relief %>% select(contains("filed_count")) %>%
                     mutate(filed_count_fy_sum = rowSums(.)) %>% 
                     summarize(filed_count_sum = sum(filed_count_fy_sum)) %>%
                     pull(filed_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     summarize(filed_count_sum = sum(n)) %>% pull(filed_count_sum))

# test that sum of completed_case_count in the table equals the total completions in the data
expect_equal(object = country_overall_asylum_or_relief %>% select(contains("completed_case_count")) %>%
                     mutate(completed_case_count_fy_sum = rowSums(.)) %>% 
                     summarize(completed_case_count_sum = sum(completed_case_count_fy_sum)) %>%
                     pull(completed_case_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
                     filter(outcome_bucket %in% c("grant", "deny") | 
                                    eoir_outcome %in% c("relief_granted", "removal", "cancellation_of_removal", 
                                                        "terminated", "voluntary_departure")) %>% nrow())

# test that sum of asylum_or_relief_count in the table equals the total asylum/relief in the data
expect_equal(object = country_overall_asylum_or_relief %>% select(contains("asylum_or_relief_count")) %>%
                     mutate(asylum_or_relief_count_fy_sum = rowSums(.)) %>% 
                     summarize(asylum_or_relief_count_sum = sum(asylum_or_relief_count_fy_sum)) %>%
                     pull(asylum_or_relief_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
                     filter(outcome_bucket == "grant" | 
                                    eoir_outcome %in% c("cancellation_of_removal", "relief_granted")) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get country_overall_asylum_or_relief_unformatted
country_overall_asylum_or_relief_unformatted <- country_overall_asylum_or_relief %>%
        select(filing_date_fy, contains("overall relief %"))
country_overall_asylum_or_relief_unformatted 

# get country_overall_asylum_or_relief_formatted
country_overall_asylum_or_relief_formatted <- country_overall_asylum_or_relief_unformatted %>%
        mutate_at(.vars = vars(contains("overall relief %")), .funs = ~ as_percent(., digits = 0)) %>%
        rename_at(.vars = vars(contains("overall relief %")), .funs = ~ str_replace_all(string = ., pattern = " overall relief %", replacement = "")) %>%
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy)

# inspect
country_overall_asylum_or_relief_formatted 
country_overall_asylum_or_relief_formatted %>% glimpse()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: Overall relief granted rate across top I-589 filing countries, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ", 
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, ", 
                                        "or termination outcome from EOIR. ",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_overall_asylum_or_relief_formatted, 
                       output_sheet_names = "country_overall_relief", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, 7, 8, 7, NA, 7, 8, 7, 7, 8, 7)), 
                       custom_row_height = list(c(rep(NA, times = nrow(country_overall_asylum_or_relief_formatted) + 1), 55)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create region_overall_asylum_or_relief_by_cohort_fy_table ####
region_overall_asylum_or_relief <- data %>% 
        group_by(citizenship_region_name, filing_date_fy) %>% 
        summarize(filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE),
                        overall_asylum_or_relief_count = sum(overall_relief_granted_flag, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(completed_case_share = completed_case_count / filed_count,
               overall_asylum_or_relief_pct = overall_asylum_or_relief_count / completed_case_count) %>%
        pivot_longer(cols = -c(filing_date_fy, citizenship_region_name), names_to = "var", values_to = "value") %>%
        mutate(var = case_when(var == "overall_asylum_or_relief_pct" ~ "overall relief %", TRUE ~ var),
               var = str_c(citizenship_region_name, var, sep = " "),
               value = case_when(is.nan(value) ~ NA_real_, TRUE ~ value)) %>%
        select(-citizenship_region_name) %>%
        pivot_wider(names_from = var, values_from = value)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
region_overall_asylum_or_relief
region_overall_asylum_or_relief %>% glimpse()

# test that sum of filed_count in the table equals the total filings in the data
expect_equal(object = region_overall_asylum_or_relief %>% select(contains("filed_count")) %>%
                     mutate(filed_count_fy_sum = rowSums(.)) %>% 
                     summarize(filed_count_sum = sum(filed_count_fy_sum)) %>%
                     pull(filed_count_sum),
             expected = data %>% nrow())

# test that sum of completed_case_count in the table equals the total completions in the data
expect_equal(object = region_overall_asylum_or_relief %>% select(contains("completed_case_count")) %>%
                     mutate(completed_case_count_fy_sum = rowSums(.)) %>% 
                     summarize(completed_case_count_sum = sum(completed_case_count_fy_sum)) %>%
                     pull(completed_case_count_sum),
             expected = data %>% 
                     filter(outcome_bucket %in% c("grant", "deny") | 
                                    eoir_outcome %in% c("relief_granted", "removal", "cancellation_of_removal", 
                                                        "terminated", "voluntary_departure")) %>% nrow())

# test that sum of asylum_or_relief_count in the table equals the total asylum/relief in the data
expect_equal(object = region_overall_asylum_or_relief %>% select(contains("asylum_or_relief_count")) %>%
                     mutate(asylum_or_relief_count_fy_sum = rowSums(.)) %>% 
                     summarize(asylum_or_relief_count_sum = sum(asylum_or_relief_count_fy_sum)) %>%
                     pull(asylum_or_relief_count_sum),
             expected = data %>% 
                     filter(outcome_bucket == "grant" | 
                                    eoir_outcome %in% c("cancellation_of_removal", "relief_granted")) %>% nrow())

# test that there are no NANs
expect_equal(object = region_overall_asylum_or_relief %>% map(.x = ., .f = ~ is.nan(.x)) %>% bind_cols() %>% 
        summarize_at(.vars = vars(-filing_date_fy), .funs = sum) %>% 
        pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
        filter(value > 0) %>% nrow(),
        expected = 0)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get region_overall_asylum_or_relief_unformatted
region_overall_asylum_or_relief_unformatted <- region_overall_asylum_or_relief %>%
        select(filing_date_fy, contains("overall relief %"))
region_overall_asylum_or_relief_unformatted 

# get region_overall_asylum_or_relief_formatted
region_overall_asylum_or_relief_formatted <- region_overall_asylum_or_relief_unformatted %>%
        mutate_at(.vars = vars(contains("overall relief %")), .funs = ~ as_percent(., digits = 0)) %>%
        mutate_at(.vars = vars(contains("overall relief %")), .funs = ~ case_when(. == "NA%" ~ "-", TRUE ~ .)) %>%
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename_at(.vars = vars(contains("overall relief %")), .funs = ~ str_replace_all(string = ., pattern = " overall relief %", replacement = "")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy)

# inspect
region_overall_asylum_or_relief_formatted 
region_overall_asylum_or_relief_formatted %>% glimpse()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: Overall relief granted rate across I-589 applicant regions, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: Regions having zero applications with overall relief granted are marked with an '-'.",
                                "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = region_overall_asylum_or_relief_formatted, 
                       output_sheet_names = "region_overall_relief", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = NULL, custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create i589_outcomes_by_attorney_by_cohort_fy_table ####

# inspect
data %>% count(attorney_flag)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get i589_outcomes_by_attorney_by_cohort_fy_table
i589_outcomes_by_attorney_by_cohort_fy_table <- data %>% add_dummies(outcome_bucket) %>% group_by(attorney_flag) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        ungroup() %>% mutate(filing_pct = filing_count / sum(filing_count), 
                             adj_pct = adj_count / filing_count,
                             grant_pct = grant_count / adj_count, 
                             denial_pct = denial_count / adj_count, 
                             referral_pct = referral_count / adj_count) %>%
        select(attorney_flag, filing_count, filing_pct, adj_count, adj_pct, filing_to_terminal_decision_days_median, 
               grant_count, grant_pct, denial_count, denial_pct, referral_count, referral_pct)
        

#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
i589_outcomes_by_attorney_by_cohort_fy_table

# test that sum of filing_count = sum of i-589 applicants
expect_equal(object = i589_outcomes_by_attorney_by_cohort_fy_table %>% summarize(filing_count_sum = sum(filing_count)) %>%
                     pull(filing_count_sum),
             expected = data %>% nrow())

# test that sum of adj_count = sum of adjudicated_flag
expect_equal(object = i589_outcomes_by_attorney_by_cohort_fy_table %>% summarize(adj_sum = sum(adj_count)) %>%
                     pull(adj_sum),
             expected = data %>% filter(adjudicated_case_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get i589_outcomes_by_attorney_by_cohort_fy_table_formatted
i589_outcomes_by_attorney_by_cohort_fy_table_formatted <- i589_outcomes_by_attorney_by_cohort_fy_table %>% 
        mutate(filing_pct = as_percent(filing_pct, digits = 0),
               filing_count = str_c(comma(filing_count), ifelse(nchar(filing_pct) == 3, " (", "   ("), filing_pct, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               filing_to_terminal_decision_days_median = comma(filing_to_terminal_decision_days_median),
               grant_pct = as_percent(grant_pct, digits = 0),
               grant_count = str_c(comma(grant_count), ifelse(nchar(grant_pct) == 3, " (", "   ("), grant_pct, ")"),
               denial_pct = as_percent(denial_pct, digits = 0),
               denial_count = str_c(comma(denial_count), ifelse(nchar(denial_pct) == 3, " (", "   ("), denial_pct, ")"),
               referral_pct = as_percent(referral_pct, digits = 0),
               referral_count = str_c(comma(referral_count), ifelse(nchar(referral_pct) == 3, " (", "   ("), referral_pct, ")"),
               attorney_flag = case_when(attorney_flag == 0 ~ "No", attorney_flag == 1 ~ "Yes")) %>%
        select(-c(filing_pct, adj_pct, grant_pct, denial_pct, referral_pct)) %>%
        rename("Applicant has attorney for RAIO processing?" = attorney_flag, "Filed" = filing_count,
                "Adjudicated" = adj_count,
               "Median days\nfrom I-589 filing\nto RAIO\ncompletion" = filing_to_terminal_decision_days_median,
               "Granted" = grant_count, "Denied" = denial_count, "Referred" = referral_count)

# inspect
i589_outcomes_by_attorney_by_cohort_fy_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: RAIO outcomes for I-589 applicants, by attorney status

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",
                                        "The 'Adjudicated' percentage in parenthesis is the share of filings that have been adjudicated. ",
                                        "The 'Granted', 'Denied', and 'Referred' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases. ",
                                        "RAIO completions are those cases either granted, referred, denied, or administratively closed by RAIO; ", 
                                        "pending cases are excluded.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i589_outcomes_by_attorney_by_cohort_fy_table_formatted, 
                       output_sheet_names = "i589_by_attorney", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 4), 15)), 
                       custom_row_height = list(c(90, rep(NA, times = nrow(i589_outcomes_by_attorney_by_cohort_fy_table_formatted)), 55)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_outcomes_by_attorney_by_cohort_fy_table ####

# inspect
data %>% count(eoir_ij_attorney_flag)
data %>% count(eoir_absentia, eoir_outcome) %>% arrange(eoir_absentia, desc(n))

#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get eoir_outcomes_by_attorney_by_cohort_fy_table
eoir_outcomes_by_attorney_by_cohort_fy_table <- data %>% add_dummies(eoir_outcome) %>% group_by(eoir_ij_attorney_flag) %>%
        summarize(referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  eoir_case_received_to_terminal_decision_days_median = median(eoir_case_received_to_terminal_decision_days, na.rm = TRUE),
                  appeal_count = sum(eoir_appeal_filed_flag),
                  absentia_count = sum(eoir_absentia, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_pct = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_pct = removal_vol_dep_count / adj_count,
               terminated_pct = terminated_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               referral_received_share = referral_received_count / sum(referral_received_count),
               appeal_pct = appeal_count / adj_count,
               absentia_pct = absentia_count / adj_count) %>%
        select(eoir_ij_attorney_flag, referral_received_count, referral_received_share, adj_count, adj_pct, 
               eoir_case_received_to_terminal_decision_days_median, 
               relief_granted_count, relief_granted_pct, 
               removal_vol_dep_count, removal_vol_dep_pct, terminated_count, terminated_pct, 
               absentia_pct, appeal_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_outcomes_by_attorney_by_cohort_fy_table

# test that sum of referral_received_count = sum of eoir cases received
expect_equal(object = eoir_outcomes_by_attorney_by_cohort_fy_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>%
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# test that sum of adj_count = sum of eoir_adjudicated_flag
expect_equal(object = eoir_outcomes_by_attorney_by_cohort_fy_table %>% summarize(adj_sum = sum(adj_count)) %>%
                     pull(adj_sum),
             expected = data %>% filter(eoir_adjudicated_flag == 1) %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_outcomes_by_attorney_by_cohort_fy_table_formatted
eoir_outcomes_by_attorney_by_cohort_fy_table_formatted <- eoir_outcomes_by_attorney_by_cohort_fy_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"), 
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               eoir_case_received_to_terminal_decision_days_median = comma(eoir_case_received_to_terminal_decision_days_median),
               relief_granted_pct = as_percent(relief_granted_pct, digits = 0),
               relief_granted_count = str_c(comma(relief_granted_count), ifelse(nchar(relief_granted_pct) == 3, " (", "   ("), relief_granted_pct, ")"),
               removal_vol_dep_pct = as_percent(removal_vol_dep_pct, digits = 0),
               removal_vol_dep_count = str_c(comma(removal_vol_dep_count), ifelse(nchar(removal_vol_dep_pct) == 3, " (", "   ("), removal_vol_dep_pct, ")"),
               terminated_pct = as_percent(terminated_pct, digits = 0),
               terminated_count = str_c(comma(terminated_count), ifelse(nchar(terminated_pct) == 3, " (", "   ("), terminated_pct, ")"),
               absentia_pct = as_percent(absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_ij_attorney_flag = case_when(eoir_ij_attorney_flag == 0 ~ "No", eoir_ij_attorney_flag == 1 ~ "Yes")) %>%
        select(-c(referral_received_share, adj_pct, relief_granted_pct, removal_vol_dep_pct, terminated_pct)) %>%
        rename("Applicant has attorney for EOIR processing?" = eoir_ij_attorney_flag, "Referrals received" = referral_received_count,
               "Adjudicated" = adj_count, 
               "Median days from referral received to EOIR completion" = eoir_case_received_to_terminal_decision_days_median,
               "Relief\ngranted" = relief_granted_count, "Removal / Voluntary departure" = removal_vol_dep_count, "Terminated" = terminated_count,
               "In absentia" = absentia_pct, "Appealed" = appeal_pct)

# inspect
eoir_outcomes_by_attorney_by_cohort_fy_table_formatted


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: EOIR outcomes for I-589 applicants, by attorney status

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Adjudicated' percentage in parenthesis is the share of referrals received that have been adjudicated. ",
                                        "The 'Relief granted', 'Removal / Voluntary departure', and 'Terminated' percentages in parenthesis are ", 
                                        "the respective counts as a share of adjudicated cases. ",
                                        "The 'In absentia' percentage is the share of adjudicated cases with an in absentia ruling. ",
                                        "The 'Appealed' percentage is the share of adjudicated cases that filed an appeal. ",
                                        "EOIR completions are those cases with either relief granted, removal / voluntary departure, ", 
                                        "terminated, or administratively closed outcomes from EOIR; pending cases are excluded. ",
                                        "The 'Median days from referral received to EOIR completion' calculation includes ", 
                                        "processing time for completed appeals.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_outcomes_by_attorney_by_cohort_fy_table_formatted, 
                       output_sheet_names = "eoir_by_attorney", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(11, 14, 12, NA, 13, 13, 10, 7, 8)), 
                       custom_row_height = list(c(90, rep(NA, times = nrow(eoir_outcomes_by_attorney_by_cohort_fy_table_formatted)), 80)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_fy_cohort_table ####
raio_metrics_by_fy_cohort_table <- data %>% 
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
                  ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
                  median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
                  referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                  referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
                  raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(filing_date_fy, filed_count,
               filed_share, adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_fy_cohort_table 
raio_metrics_by_fy_cohort_table %>% glimpse()

# test that sum of filed_count in table equals total records for in data 
expect_equal(object = raio_metrics_by_fy_cohort_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# test that sum of adj_count in table equals total records adj by raio
expect_equal(object = raio_metrics_by_fy_cohort_table %>% summarize(adj_count_sum = sum(adj_count)) %>% pull(adj_count_sum),
             expected = data %>% filter(adjudicated_case_flag == 1) %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_fy_cohort_table_formatted
raio_metrics_by_fy_cohort_table_formatted <- raio_metrics_by_fy_cohort_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_count = comma(ewi_count), ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(ewi_count, ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_count = comma(referral_w_one_year_limit_count),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(referral_w_one_year_limit_count, 
                                                       ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, filed_count, ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered\nwithout\ninspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on\n1-year filing deadline" = referral_w_one_year_limit_count,
               "Applicant has attorney for RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_fy_cohort_table_formatted
raio_metrics_by_fy_cohort_table_formatted %>% glimpse()


# ////////////////////////


# title: RAIO outcome metrics, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_fy_cohort_table_formatted, 
                       output_sheet_names = "raio_metrics_by_fy_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_fy_cohort_table_formatted)), 52)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_fy_cohort_table ####

# get eoir_metrics_by_fy_cohort_table
eoir_metrics_by_fy_cohort_table <- data %>% 
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(filing_date_fy, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_fy_cohort_table
eoir_metrics_by_fy_cohort_table %>% data.frame()
eoir_metrics_by_fy_cohort_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_fy_cohort_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# test that sum of adj_count in table equals total records eoir adj
expect_equal(object = eoir_metrics_by_fy_cohort_table %>% summarize(adj_count_sum = sum(adj_count)) %>% 
                     pull(adj_count_sum),
             expected = data %>% filter(eoir_received_flag == 1, eoir_adjudicated_flag == 1) %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_fy_cohort_table_formatted
eoir_metrics_by_fy_cohort_table_formatted <- eoir_metrics_by_fy_cohort_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, referral_received_count, 
               cancellation_of_removal_applied_count, adj_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_fy_cohort_table_formatted
eoir_metrics_by_fy_cohort_table_formatted %>% glimpse()


# ////////////////////////


# title: EOIR outcome metrics, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_fy_cohort_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_fy_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 5), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_country_table ####
raio_metrics_by_country_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(citizenship_output_country_name) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
               ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
               median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
               referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
               referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
               raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        arrange(desc(filed_count)) %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(citizenship_output_country_name, filed_count,
               filed_share, adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_country_table 
raio_metrics_by_country_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = raio_metrics_by_country_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_country_table_formatted
raio_metrics_by_country_table_formatted <- raio_metrics_by_country_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), 
                     ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(comma(ewi_count), ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(comma(referral_w_one_year_limit_count), 
                                                     ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0)) %>%
        select(citizenship_output_country_name, filed_count, ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("Applicant country" = citizenship_output_country_name,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered\nwithout\ninspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on\n1-year filing deadline" = referral_w_one_year_limit_count,
               "Applicant has attorney for RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_country_table_formatted
raio_metrics_by_country_table_formatted %>% glimpse()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: RAIO outcome metrics, by top I-589 filing countries

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_country_table_formatted, 
                       output_sheet_names = "raio_metrics_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_country_table_formatted)), 52)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_country_table ####

# get eoir_metrics_by_country_table
eoir_metrics_by_country_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(citizenship_output_country_name) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        arrange(desc(referral_received_count)) %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(citizenship_output_country_name, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_country_table
eoir_metrics_by_country_table %>% data.frame()
eoir_metrics_by_country_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_country_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
                     filter(eoir_received_flag == 1) %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_country_table_formatted
eoir_metrics_by_country_table_formatted <- eoir_metrics_by_country_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0)) %>%
        select(citizenship_output_country_name, referral_received_count, 
               cancellation_of_removal_applied_count, adj_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("Applicant country" = citizenship_output_country_name,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_country_table_formatted


# ////////////////////////


# title: EOIR outcome metrics, by top I-589 filing countries

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_country_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 5), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_country_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_region_table ####

# get raio_metrics_by_region_table
raio_metrics_by_region_table <- data %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(citizenship_region_name) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
                  ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
                  median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
                  referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                  referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
                  raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        arrange(desc(filed_count)) %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(citizenship_region_name, filed_count, filed_share,
               adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_region_table 
metrics_by_region_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = raio_metrics_by_region_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# inspect outliers
data %>% filter(citizenship_region_name == "Asia") %>% summarize(filed_count = n(), ewi_count = sum(status_at_entry == "EWI"))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_region_table_formatted
raio_metrics_by_region_table_formatted <- raio_metrics_by_region_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), 
                                   ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(comma(ewi_count), ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(comma(referral_w_one_year_limit_count), 
                                                       ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0)) %>%
        select(citizenship_region_name, filed_count, ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("Applicant region" = citizenship_region_name,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered\nwithout\ninspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on\n1-year filing deadline" = referral_w_one_year_limit_count,
               "Applicant has attorney for RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_region_table_formatted


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: RAIO outcome metrics, by applicant region

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_region_table_formatted, 
                       output_sheet_names = "raio_metrics_by_region", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_region_table_formatted)), 52)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_region_table ####

# get eoir_metrics_by_region_table
eoir_metrics_by_region_table <- data %>%
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(citizenship_region_name) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        arrange(desc(referral_received_count)) %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(citizenship_region_name, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_region_table
eoir_metrics_by_region_table %>% data.frame()
eoir_metrics_by_region_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_region_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_region_table_formatted
eoir_metrics_by_region_table_formatted <- eoir_metrics_by_region_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0)) %>%
        select(citizenship_region_name, referral_received_count, adj_count, 
               cancellation_of_removal_applied_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("Applicant region" = citizenship_region_name,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_region_table_formatted


# ////////////////////////


# title: EOIR outcome metrics, by applicant region

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_region_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_region", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 5), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_region_table_formatted)), 55)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_income_table ####
raio_metrics_by_income_table <- data %>% 
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(income_group_bucket) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
                  ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
                  median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
                  referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                  referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
                  raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        arrange(desc(filed_count)) %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(income_group_bucket, filed_count,
               filed_share, adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_income_table 
raio_metrics_by_income_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = raio_metrics_by_income_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_income_table_formatted
raio_metrics_by_income_table_formatted <- raio_metrics_by_income_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), 
                                   ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(comma(ewi_count), ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(comma(referral_w_one_year_limit_count), 
                                                       ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(income_group_bucket, filed_count, ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("Income group" = income_group_bucket,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered\nwithout\ninspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on\n1-year filing deadline" = referral_w_one_year_limit_count,
               "Applicant has attorney for RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_income_table_formatted %>% data.frame()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: RAIO outcome metrics, by applicant country income groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: World Bank; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_income_table_formatted, 
                       output_sheet_names = "raio_metrics_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_income_table_formatted)), 52)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_homicide_table ####
raio_metrics_by_homicide_table <- data %>% 
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(homicide_bucket) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
                  ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
                  median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
                  referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                  referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
                  raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        arrange(desc(filed_count)) %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(homicide_bucket, filed_count,
               filed_share, adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_homicide_table 
raio_metrics_by_homicide_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = raio_metrics_by_homicide_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())


# inspect outlier
data %>% filter(homicide_bucket == "10.1 to 20") %>% count(citizenship_output_country_name) %>% arrange(desc(n))

#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_homicide_table_formatted
raio_metrics_by_homicide_table_formatted <- raio_metrics_by_homicide_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), 
                                   ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(comma(ewi_count), ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(comma(referral_w_one_year_limit_count), 
                                                       ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                              homicide_bucket == "1.1 to 10" ~ 2,
                                              homicide_bucket == "10.1 to 20" ~ 3,
                                              homicide_bucket == "20.1 to 40" ~ 4,
                                              homicide_bucket == "> 40" ~ 5,
                                              homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(homicide_bucket, filed_count, ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("Homicide rate per 100k population" = homicide_bucket,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered without inspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on 1-year filing deadline" = referral_w_one_year_limit_count,
               "% with attorney during RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_homicide_table_formatted %>% data.frame()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: RAIO outcome metrics, by applicant country homicide groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_homicide_table_formatted, 
                       output_sheet_names = "raio_metrics_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 3), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_homicide_table_formatted)), 52)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_metrics_by_prcl_table ####
raio_metrics_by_prcl_table <- data %>% 
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(prcl_bucket) %>%
        summarize(filed_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  adj_pct = adj_count / filed_count,
                  ewi_count = sum(status_at_entry.EWI), ewi_pct = ewi_count / filed_count,
                  median_entry_to_filing_days = median(entry_to_filing_days, na.rm = TRUE),
                  referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                  referral_w_one_year_limit_pct = referral_w_one_year_limit_count / adj_count,
                  raio_attorney_pct = mean(attorney_flag)) %>%
        ungroup() %>%
        arrange(desc(filed_count)) %>%
        mutate(filed_share = filed_count / (data %>% nrow())) %>%
        select(prcl_bucket, filed_count,
               filed_share, adj_count, adj_pct,
               ewi_count, ewi_pct, median_entry_to_filing_days, 
               referral_w_one_year_limit_count, referral_w_one_year_limit_pct, raio_attorney_pct)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


raio_metrics_by_prcl_table 
raio_metrics_by_prcl_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = raio_metrics_by_prcl_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())


# inspect outlier
# data %>% filter(prcl_bucket == "10.1 to 20") %>% count(citizenship_output_country_name) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get raio_metrics_by_prcl_table_formatted
raio_metrics_by_prcl_table_formatted <- raio_metrics_by_prcl_table %>% 
        mutate(filed_share = as_percent(filed_share, digits = 0),
               filed_count = str_c(comma(filed_count), 
                                   ifelse(nchar(filed_share) == 3, " (", "   ("), filed_share, ")"),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(comma(adj_count), 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               ewi_pct = as_percent(ewi_pct, digits = 0),
               ewi_count = str_c(comma(ewi_count), ifelse(nchar(ewi_pct) == 3, " (", "   ("), ewi_pct, ")"),
               median_entry_to_filing_days = comma(median_entry_to_filing_days),
               referral_w_one_year_limit_pct = as_percent(referral_w_one_year_limit_pct, digits = 0),
               referral_w_one_year_limit_count = str_c(comma(referral_w_one_year_limit_count), 
                                                       ifelse(nchar(referral_w_one_year_limit_pct) == 3, " (", "   ("), referral_w_one_year_limit_pct, ")"),
               raio_attorney_pct = as_percent(raio_attorney_pct, digits = 0),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                          prcl_bucket == "25 to 49" ~ 2,
                                          prcl_bucket == "50 to 74" ~ 3,
                                          prcl_bucket == "75 to 100" ~ 4,
                                          prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(prcl_bucket, filed_count,  ewi_count, 
               median_entry_to_filing_days, adj_count, referral_w_one_year_limit_count, raio_attorney_pct) %>%
        rename("Total political rights & civil liberties score" = prcl_bucket,
               "Filed" = filed_count, 
               "Adjudicated" = adj_count,
               "Entered\nwithout\ninspection" = ewi_count, "Median days from entering U.S. to filing I-589" = median_entry_to_filing_days,
               "Referred to EOIR based on\n1-year filing deadline" = referral_w_one_year_limit_count,
               "Applicant has attorney for RAIO processing" = raio_attorney_pct)

# inspect
raio_metrics_by_prcl_table_formatted %>% data.frame()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# title: RAIO outcome metrics, by applicant country political rights / civil liberties groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                        "The 'Filed' percentage in parenthesis is the share of total filings. ",  
                                        "The 'Entered without inspection' and 'Adjudicated' percentages in parenthesis are ",
                                        "the respective counts as a share of filings. ",
                                        "The 'Referred to EOIR based on 1-year filing deadline' percentage in parenthesis is ", 
                                        "the respective count as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for RAIO processing' percentage is the share of filings where the ",
                                        "applicant has an attorney for RAIO processing.",
                                        sep = ""),
                                  "Source: Freedom House; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_metrics_by_prcl_table_formatted, 
                       output_sheet_names = "raio_metrics_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(11, rep(NA, times = 2), 10)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(raio_metrics_by_prcl_table_formatted)), 65)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_income_table ####

# get eoir_metrics_by_income_table
eoir_metrics_by_income_table <- data %>%
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(income_group_bucket) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        arrange(desc(referral_received_count)) %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(income_group_bucket, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_income_table
eoir_metrics_by_income_table %>% data.frame()
eoir_metrics_by_income_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_income_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# test that share of total referrals received sums to 1
expect_equal(object = eoir_metrics_by_income_table %>% 
                     summarize(sum_referral_received_share = sum(referral_received_share, na.rm = TRUE)) %>%
                     pull(sum_referral_received_share),
             expected = 1)

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_income_table_formatted
eoir_metrics_by_income_table_formatted <- eoir_metrics_by_income_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(income_group_bucket, referral_received_count, adj_count, 
               cancellation_of_removal_applied_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("Income group" = income_group_bucket,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_income_table_formatted %>% data.frame()


# ////////////////////////


# title: EOIR outcome metrics, by applicant country income groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_income_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, rep(NA, times = 4), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_income_table_formatted)), 55)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_homicide_table ####

# get eoir_metrics_by_homicide_table
eoir_metrics_by_homicide_table <- data %>%
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(homicide_bucket) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        arrange(desc(referral_received_count)) %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(homicide_bucket, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_homicide_table
eoir_metrics_by_homicide_table %>% data.frame()
eoir_metrics_by_homicide_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_homicide_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# test that share of total referrals received sums to 1
expect_equal(object = eoir_metrics_by_homicide_table %>% 
                     summarize(sum_referral_received_share = sum(referral_received_share, na.rm = TRUE)) %>%
                     pull(sum_referral_received_share),
             expected = 1)

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_homicide_table_formatted
eoir_metrics_by_homicide_table_formatted <- eoir_metrics_by_homicide_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                              homicide_bucket == "1.1 to 10" ~ 2,
                                              homicide_bucket == "10.1 to 20" ~ 3,
                                              homicide_bucket == "20.1 to 40" ~ 4,
                                              homicide_bucket == "> 40" ~ 5,
                                              homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(homicide_bucket, referral_received_count, adj_count, 
               cancellation_of_removal_applied_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("Homicide per 100k population" = homicide_bucket,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_homicide_table_formatted %>% data.frame()


# ////////////////////////


# title: EOIR outcome metrics, by applicant country homicide groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_homicide_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 5), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_homicide_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create eoir_metrics_by_prcl_table ####

# get eoir_metrics_by_prcl_table
eoir_metrics_by_prcl_table <- data %>%
        filter(eoir_received_flag == 1) %>%
        add_dummies(vars(status_at_entry, outcome_bucket)) %>%
        group_by(prcl_bucket) %>%
        summarize(filed_count = n(),
                  referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  adj_pct = adj_count / referral_received_count,
                  cancellation_of_removal_applied_count = sum(eoir_cancellation_applied, na.rm = TRUE),
                  cancellation_of_removal_applied_pct = cancellation_of_removal_applied_count / referral_received_count,
                  cancellation_of_removal_granted_count = sum(eoir_cancellation_granted, na.rm = TRUE),
                  cancellation_of_removal_granted_pct = cancellation_of_removal_granted_count / adj_count,
                  eoir_attorney_pct = mean(eoir_ij_attorney_flag),
                  in_absentia_count = sum(eoir_absentia, na.rm = TRUE),
                  in_absentia_pct = in_absentia_count / adj_count,
                  appeal_count = sum(eoir_appeal_filed_flag),
                  appeal_pct = appeal_count / adj_count) %>%
        ungroup() %>%
        arrange(desc(referral_received_count)) %>%
        mutate(referral_received_share = referral_received_count / (data %>% filter(eoir_received_flag == 1) %>% nrow())) %>%
        select(prcl_bucket, 
               referral_received_count, referral_received_share, adj_count, adj_pct,
               cancellation_of_removal_applied_count,
               cancellation_of_removal_applied_pct, cancellation_of_removal_granted_count,
               cancellation_of_removal_granted_pct, eoir_attorney_pct, in_absentia_count, in_absentia_pct, appeal_count, appeal_pct)


#/#/#/#/#/#/#/#/#/#/


# inspect
eoir_metrics_by_prcl_table
eoir_metrics_by_prcl_table %>% data.frame()
eoir_metrics_by_prcl_table %>% glimpse()

# test that sum of filed_count in table equals total records for top countries in data 
expect_equal(object = eoir_metrics_by_prcl_table %>% summarize(referral_received_count_sum = sum(referral_received_count)) %>% 
                     pull(referral_received_count_sum),
             expected = data %>% filter(eoir_received_flag == 1) %>% nrow())

# test that share of total referrals received sums to 1
expect_equal(object = eoir_metrics_by_prcl_table %>% 
                     summarize(sum_referral_received_share = sum(referral_received_share, na.rm = TRUE)) %>%
                     pull(sum_referral_received_share),
             expected = 1)

# inspect outliers
data %>% filter(citizenship_output_country_name == "Venezuela") %>% count(status_at_entry) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get eoir_metrics_by_prcl_table_formatted
eoir_metrics_by_prcl_table_formatted <- eoir_metrics_by_prcl_table %>% 
        mutate(referral_received_share = as_percent(referral_received_share, digits = 0),
               referral_received_count = str_c(comma(referral_received_count), 
                                               ifelse(nchar(referral_received_share) == 3, " (", "   ("), referral_received_share, ")"),
               adj_count = comma(adj_count),
               adj_pct = as_percent(adj_pct, digits = 0),
               adj_count = str_c(adj_count, 
                                 ifelse(nchar(adj_pct) == 3, " (", "   ("), adj_pct, ")"),
               cancellation_of_removal_applied_count = comma(cancellation_of_removal_applied_count),
               cancellation_of_removal_applied_pct = as_percent(cancellation_of_removal_applied_pct, digits = 0),
               cancellation_of_removal_applied_count = str_c(cancellation_of_removal_applied_count, 
                                                             ifelse(nchar(cancellation_of_removal_applied_pct) == 3, " (", "   ("), cancellation_of_removal_applied_pct, ")"),
               cancellation_of_removal_granted_count = comma(cancellation_of_removal_granted_count),
               cancellation_of_removal_granted_pct = as_percent(cancellation_of_removal_granted_pct, digits = 0),
               cancellation_of_removal_granted_count = str_c(cancellation_of_removal_granted_count, 
                                                             ifelse(nchar(cancellation_of_removal_granted_pct) == 3, " (", "   ("), cancellation_of_removal_granted_pct, ")"),
               in_absentia_pct = as_percent(in_absentia_pct, digits = 0),
               appeal_pct = as_percent(appeal_pct, digits = 0),
               eoir_attorney_pct = as_percent(eoir_attorney_pct, digits = 0),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                          prcl_bucket == "25 to 49" ~ 2,
                                          prcl_bucket == "50 to 74" ~ 3,
                                          prcl_bucket == "75 to 100" ~ 4,
                                          prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(prcl_bucket, referral_received_count, adj_count, 
               cancellation_of_removal_applied_count, cancellation_of_removal_granted_count, in_absentia_pct, appeal_pct, eoir_attorney_pct) %>%
        rename("Total political rights & civil liberties score" = prcl_bucket,
               "Referrals received" = referral_received_count, 
               "Adjudicated" = adj_count,
               "Cancellation of removal applied for" = cancellation_of_removal_applied_count,
               "Cancellation of removal granted" = cancellation_of_removal_granted_count,
               "In absentia" = in_absentia_pct,
               "Appeals" = appeal_pct,
               "Applicant has attorney for EOIR processing" = eoir_attorney_pct)

# inspect
eoir_metrics_by_prcl_table_formatted %>% data.frame()


# ////////////////////////


# title: EOIR outcome metrics, by applicant country political rights / civil liberties groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                        "The 'Referrals received' percentage in parenthesis is the share of total referrals received. ", 
                                        "The 'Cancellation of removal applied for' percentage in parenthesis is the count as a share of referrals received. ",
                                        "The 'Adjudicated', 'Cancellation of removal granted', ",
                                        "'In absentia', and 'Appeals' percentages are the respective counts as a share of adjudicated cases. ",
                                        "The 'Applicant has attorney for EOIR processing' percentage is the share of referrals received where the ",
                                        "applicant has an attorney for EOIR processing.",
                                        sep = ""),
                                  "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = eoir_metrics_by_prcl_table_formatted, 
                       output_sheet_names = "eoir_metrics_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(12, rep(NA, times = 4), 7, 7)), 
                       custom_row_height = list(c(80, rep(NA, times = nrow(eoir_metrics_by_prcl_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create country_filing_share_by_cohort_fy_table ####

# get filings_by_fy
filings_by_fy <- data %>%
        group_by(filing_date_fy) %>% count() %>% ungroup() %>% rename(fy = filing_date_fy, total_filing_count = n)
filings_by_fy

country_filing_share_by_cohort_fy_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>%
        summarize(filing_count = n()) %>%
        ungroup() %>%
        # arrange(filing_date_fy, desc(filing_count)) %>%
        left_join(., filings_by_fy, by = c("filing_date_fy" = "fy")) %>%
        mutate(filing_share = filing_count / total_filing_count) %>%
        select(filing_date_fy, citizenship_output_country_name, filing_share) %>%
        left_join(data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                          distinct(citizenship_output_country_name),
                  ., by = "citizenship_output_country_name") %>%        
        pivot_wider(names_from = citizenship_output_country_name, values_from = filing_share)


#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect
country_filing_share_by_cohort_fy_table

# test that pcts across all row sum < 1
expect_equal(object = country_filing_share_by_cohort_fy_table %>% select(-filing_date_fy) %>% 
                     mutate(row_sums = rowSums(.), less_than_1 = case_when(row_sums < 1 ~ 1, TRUE ~ 0)) %>%
                     distinct(less_than_1) %>% pull(less_than_1),
             expected = 1)

# manually inspect outliers
data %>% filter(filing_date_fy == 2009) %>% count(citizenship_output_country_name) %>% 
        mutate(share = n / sum(n)) %>% filter(citizenship_output_country_name %in% c("China", "Mexico", "Ecuador"))
data %>% filter(filing_date_fy == 2016) %>% count(citizenship_output_country_name) %>% 
        mutate(share = n / sum(n)) %>% filter(citizenship_output_country_name %in% c("Guatemala", "India", "Venezuela"))


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get country_filing_share_by_cohort_fy_table_formatted
country_filing_share_by_cohort_fy_table_formatted <- country_filing_share_by_cohort_fy_table %>%
        mutate_at(.vars = vars(-filing_date_fy), .funs = ~ as_percent(., digits = 0)) %>%
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy)

# inspect
country_filing_share_by_cohort_fy_table_formatted 


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: I-589 filing share for top filing countries, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_filing_share_by_cohort_fy_table_formatted, 
                       output_sheet_names = "country_filing_share_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(16, 6, 8, 6, 9, 8, 8, 6, 6, 7, 6)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_fy_cohort_table ####

# inspect

# note that 78179 anumbers are missing date_of_entry, and so are also missing age_at_entry
data %>% filter(is.na(date_of_birth)) %>% nrow() # 0
data %>% filter(is.na(age_at_entry)) %>% nrow() # 78179
data %>% filter(is.na(date_of_entry)) %>% nrow() # 78179
data %>% summarize(min_age_at_entry = min(age_at_entry, na.rm = TRUE),
                   max_age_at_entry = max(age_at_entry, na.rm = TRUE),
                   min_age_at_filing = min(age_at_filing),
                   max_age_at_filing = max(age_at_filing))
data %>% ggplot(data = ., aes(x = age_at_entry)) + geom_histogram() 
data %>% ggplot(data = ., aes(x = age_at_filing)) + geom_histogram() 
data %>% mutate(age_at_entry = ceiling(age_at_entry)) %>% count(age_at_entry) %>% print(n = nrow(.))
data %>% mutate(age_at_filing = ceiling(age_at_filing)) %>% count(age_at_filing) %>% print(n = nrow(.))


# inspect dep_child/spouse
data %>% count(has_dep_child)
data %>% count(dep_child_count)
data %>% count(has_dep_spouse)

# inspect median_child_age_at_i159 filing
# note it's not really known whether principal actually had the child at time of entry, or took custody later, so will omit that as a descriptive stat
i589_dep %>% filter(dep_relationship_desc == "Child") %>% ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
i589_dep %>% filter(dep_relationship_desc == "Spouse") %>% ggplot(data = ., aes(x = dep_age_at_princ_filing)) + geom_histogram()
i589_dep %>% filter(is.na(dep_age_at_princ_filing)) %>% nrow() # 0
i589_dep %>% summarize(min_child_age = min(dep_age_at_princ_filing[dep_relationship_desc == "Child"]), 
                       max_child_age = max(dep_age_at_princ_filing[dep_relationship_desc == "Child"]))
# note there are 4 negative ages and < 20 over age 22; but these won't change the mean by much
i589_dep %>% filter(dep_relationship_desc == "Child") %>% mutate(child_age_at_filing = ceiling(dep_age_at_princ_filing)) %>%
        count(child_age_at_filing) %>% print(n = nrow(.))

# inspect ij_attorney_flag - confirm that anumbers with pending eoir_outcomes can have ij attorneys
# so downward time trend in ij attorney is not an artifact of more recent cases not being completed, or anything like that??
data %>% count(eoir_ij_attorney_flag, eoir_outcome) %>% arrange(eoir_ij_attorney_flag, desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get demographics_by_fy_cohort_table
demographics_by_fy_cohort_table <- data %>% mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
                age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
                age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(filing_date_fy) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child)) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_fy_cohort_table
demographics_by_fy_cohort_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_fy_cohort_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)
                     
# test that age at entry < age at filing
expect_equal(object = demographics_by_fy_cohort_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_fy_cohort_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_fy_cohort_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var >= 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value >= 0) %>% nrow(),
             expected = demographics_by_fy_cohort_table %>% ncol())


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_fy_cohort_table_unformatted
demographics_by_fy_cohort_table_unformatted <- demographics_by_fy_cohort_table %>% 
        select(filing_date_fy, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_fy_cohort_table_unformatted

# get demographics_by_fy_cohort_table_formatted
demographics_by_fy_cohort_table_formatted <- demographics_by_fy_cohort_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_fy_cohort_table_formatted
demographics_by_fy_cohort_table_formatted %>% data.frame()


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_fy_cohort_table_formatted, 
                       output_sheet_names = "demographics_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_for_ewi_by_fy_cohort_table ####

# get demographics_for_ewi_by_fy_cohort_table
demographics_for_ewi_by_fy_cohort_table <- data %>% 
        filter(status_at_entry == "EWI") %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
           age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
           age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(filing_date_fy) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = 0,
                  mean_b1_tourism_visa = 0,
                  mean_f1_student_visa = 0,
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_for_ewi_by_fy_cohort_table
demographics_for_ewi_by_fy_cohort_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_for_ewi_by_fy_cohort_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_for_ewi_by_fy_cohort_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_for_ewi_by_fy_cohort_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_for_ewi_by_fy_cohort_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var >= 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value >= 0) %>% nrow(),
             expected = demographics_for_ewi_by_fy_cohort_table %>% ncol())

# inspect outliers
data %>% filter(filing_date_fy == "2015", status_at_entry == "EWI") %>% 
        summarize(median_age_at_entry = median(age_at_entry, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_for_ewi_by_fy_cohort_table_unformatted
demographics_for_ewi_by_fy_cohort_table_unformatted <- demographics_for_ewi_by_fy_cohort_table %>% 
        select(filing_date_fy, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_for_ewi_by_fy_cohort_table_unformatted

# get demographics_for_ewi_by_fy_cohort_table_formatted
demographics_for_ewi_by_fy_cohort_table_formatted <- demographics_for_ewi_by_fy_cohort_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct) %>%
        select(-c("Entering U.S. on B2 business visa", "Entering U.S. on B1 tourism visa", "Entering U.S. on F1 student visa",
                  "Entering without inspection"))

# inspect
demographics_for_ewi_by_fy_cohort_table_formatted


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants who entered without inspection, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_for_ewi_by_fy_cohort_table_formatted, 
                       output_sheet_names = "demographics_ewi_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_for_referral_w_one_year_limit_by_fy_cohort_table ####

# get demographics_for_referral_w_one_year_limit_by_fy_cohort_table
demographics_for_referral_w_one_year_limit_by_fy_cohort_table <- data %>% 
        filter(outcome_bucket == "referral_w_one_year_limit") %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(filing_date_fy) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_for_referral_w_one_year_limit_by_fy_cohort_table
demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% ncol())

# inspect outliers
data %>% filter(filing_date_fy == "2015", outcome_bucket == "referral_w_one_year_limit") %>% 
        summarize(median_age_at_entry = median(age_at_entry, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted
demographics_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted <- demographics_for_referral_w_one_year_limit_by_fy_cohort_table %>% 
        select(filing_date_fy, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted

# get demographics_for_referral_w_one_year_limit_by_fy_cohort_table_formatted
demographics_for_referral_w_one_year_limit_by_fy_cohort_table_formatted <- demographics_for_referral_w_one_year_limit_by_fy_cohort_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_for_referral_w_one_year_limit_by_fy_cohort_table_formatted


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants who were referred by USCIS based on the one year filing deadline, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_for_referral_w_one_year_limit_by_fy_cohort_table_formatted, 
                       output_sheet_names = "demographics_1yr_ref_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_for_cancellation_of_removal_appl_by_fy_cohort_table ####

# get demographics_for_cancellation_of_removal_appl_by_fy_cohort_table
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table <- data %>% 
        filter(eoir_cancellation_applied == 1) %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(filing_date_fy) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% ncol())

# inspect outliers
data %>% filter(filing_date_fy == "2015", eoir_cancellation_applied == 1) %>% 
        summarize(mean_age_at_entry = mean(age_at_entry, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted <- demographics_for_cancellation_of_removal_appl_by_fy_cohort_table %>% 
        select(filing_date_fy, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted

# get demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted <- demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants who applied for cancellation of removal, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_for_cancellation_of_removal_appl_by_fy_cohort_table_formatted, 
                       output_sheet_names = "demographics_cor_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_country_table ####

# get demographics_by_country_table
demographics_by_country_table <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% 
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(citizenship_output_country_name) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_country_table
demographics_by_country_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_country_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_by_country_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_country_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_country_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_by_country_table %>% ncol())

# inspect outliers
data %>% filter(citizenship_output_country_name == "China") %>% 
        summarize(median_age_at_entry = median(age_at_entry, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_country_table_unformatted
demographics_by_country_table_unformatted <- demographics_by_country_table %>% 
        select(citizenship_output_country_name, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_country_table_unformatted

# get demographics_by_country_table_formatted
demographics_by_country_table_formatted <- demographics_by_country_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0)) %>%
        rename("Applicant country" = citizenship_output_country_name, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_country_table_formatted


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by applicant country

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_country_table_formatted, 
                       output_sheet_names = "demographics_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_region_table ####

# get demographics_by_region_table
demographics_by_region_table <- data %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(citizenship_region_name) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_region_table
demographics_by_region_table %>% data.frame()
demographics_by_region_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_region_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_by_region_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_region_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_region_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_by_region_table %>% ncol())

# inspect outliers
data %>% filter(citizenship_output_country_name == "China", outcome_bucket == "pending" | eoir_outcome == "pending") %>% 
        summarize(median_age_at_entry = median(age_at_entry, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_region_table_unformatted
demographics_by_region_table_unformatted <- demographics_by_region_table %>% 
        select(citizenship_region_name, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_region_table_unformatted

# get demographics_by_region_table_formatted
demographics_by_region_table_formatted <- demographics_by_region_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0)) %>%
        rename("Applicant region" = citizenship_region_name, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_region_table_formatted


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by applicant region

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_region_table_formatted, 
                       output_sheet_names = "demographics_by_region", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_income_table ####

# get demographics_by_income_table
demographics_by_income_table <- data %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(income_group_bucket) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_income_table
demographics_by_income_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_income_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_by_income_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_income_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_income_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_by_income_table %>% ncol())


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_income_table_unformatted
demographics_by_income_table_unformatted <- demographics_by_income_table %>% 
        select(income_group_bucket, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_income_table_unformatted

# get demographics_by_income_table_formatted
demographics_by_income_table_formatted <- demographics_by_income_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(-income_group_index) %>%
        rename("Income group" = income_group_bucket, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_income_table_formatted %>% data.frame()


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by applicant country income groupings

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: World Bank; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_income_table_formatted, 
                       output_sheet_names = "demographics_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_homicide_table ####

# get demographics_by_homicide_table
demographics_by_homicide_table <- data %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(homicide_bucket) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_homicide_table
demographics_by_homicide_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_homicide_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_by_homicide_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_homicide_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_homicide_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_by_homicide_table %>% ncol())


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_homicide_table_unformatted
demographics_by_homicide_table_unformatted <- demographics_by_homicide_table %>% 
        select(homicide_bucket, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_homicide_table_unformatted

# get demographics_by_homicide_table_formatted
demographics_by_homicide_table_formatted <- demographics_by_homicide_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                              homicide_bucket == "1.1 to 10" ~ 2,
                                              homicide_bucket == "10.1 to 20" ~ 3,
                                              homicide_bucket == "20.1 to 40" ~ 4,
                                              homicide_bucket == "> 40" ~ 5,
                                          homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(-homicide_index) %>%
        rename("Homicide rate per 100k population" = homicide_bucket, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_homicide_table_formatted %>% data.frame()


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by applicant country homicide groupings

# create footnote_table
footnote_table <- tibble(text = c("Note: Percentages are the respective counts as a share of filings.",
                                  "Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_homicide_table_formatted, 
                       output_sheet_names = "demographics_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create demographics_by_prcl_table ####

# get demographics_by_prcl_table
demographics_by_prcl_table <- data %>%
        mutate(female = case_when(gender == "Female" ~ 1, TRUE ~ 0),
               age_at_entry = case_when(age_at_entry > 120 ~ NA_real_, TRUE ~ age_at_entry),
               age_at_filing = case_when(age_at_filing > 120 ~ NA_real_, TRUE ~ age_at_filing)) %>%
        add_dummies(status_at_entry) %>%
        group_by(prcl_bucket) %>%
        summarize(female_pct = mean(female),
                  median_age_at_entry = median(age_at_entry, na.rm = TRUE),
                  median_age_at_filing = median(age_at_filing, na.rm = TRUE),
                  mean_b2_business_visa = mean(status_at_entry.B2),
                  mean_b1_tourism_visa = mean(status_at_entry.B1),
                  mean_f1_student_visa = mean(status_at_entry.F1),
                  mean_ewi = mean(status_at_entry.EWI),
                  has_dep_spouse_pct = mean(has_dep_spouse),
                  has_dep_child_pct = mean(has_dep_child),
                  mean_dep_child_count_for_those_w_child = mean(dep_child_count[has_dep_child == 1])) %>%
        ungroup()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# test
demographics_by_prcl_table
demographics_by_prcl_table %>% glimpse()

# test that rates are > 0 and < 1
expect_equal(object = demographics_by_prcl_table %>% 
                     select(female_pct, has_dep_spouse_pct, has_dep_child_pct) %>%
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(var < 0 | var > 1) %>% nrow()) %>% as_tibble() %>%
                     pivot_longer(cols = everything(.), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = 0)

# test that age at entry < age at filing
expect_equal(object = demographics_by_prcl_table %>% 
                     filter(median_age_at_entry < median_age_at_filing) %>% nrow(),
             expected = demographics_by_prcl_table %>% nrow())

# test that there all numbers are positive and non-NA
expect_equal(object = demographics_by_prcl_table %>% 
                     map(.x = ., .f = ~ tibble(var = .x) %>% filter(!is.na(var), var > 0) %>% nrow()) %>%
                     as_tibble() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                     filter(value > 0) %>% nrow(),
             expected = demographics_by_prcl_table %>% ncol())


#/#/#/#/#/#/#/#/#/#/#/#/#/


# get demographics_by_prcl_table_unformatted
demographics_by_prcl_table_unformatted <- demographics_by_prcl_table %>% 
        select(prcl_bucket, female_pct, median_age_at_entry, median_age_at_filing, has_dep_spouse_pct, 
               has_dep_child_pct, mean_b2_business_visa, mean_b1_tourism_visa, mean_f1_student_visa, mean_ewi)
demographics_by_prcl_table_unformatted

# get demographics_by_prcl_table_formatted
demographics_by_prcl_table_formatted <- demographics_by_prcl_table_unformatted %>% 
        mutate(female_pct = as_percent(female_pct, digits = 0),
               has_dep_spouse_pct = as_percent(has_dep_spouse_pct, digits = 0),
               has_dep_child_pct = as_percent(has_dep_child_pct, digits = 0),
               median_age_at_entry = round_to_digits(median_age_at_entry, digits = 1),
               median_age_at_filing = round_to_digits(median_age_at_filing, digits = 1),
               mean_b2_business_visa = as_percent(mean_b2_business_visa, digits = 0),
               mean_b1_tourism_visa = as_percent(mean_b1_tourism_visa, digits = 0),
               mean_f1_student_visa = as_percent(mean_f1_student_visa, digits = 0),
               mean_ewi = as_percent(mean_ewi, digits = 0),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                          prcl_bucket == "25 to 49" ~ 2,
                                          prcl_bucket == "50 to 74" ~ 3,
                                          prcl_bucket == "75 to 100" ~ 4,
                                          prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(-prcl_index) %>%
        rename("Total political rights & civil liberties score" = prcl_bucket, "Female" = female_pct, 
               "Median age when last entering U.S." = median_age_at_entry, 
               "Median age when filing\nI-589" = median_age_at_filing,
               "Entering U.S. on B2 business visa" = mean_b2_business_visa,
               "Entering U.S. on B1 tourism visa" = mean_b1_tourism_visa,
               "Entering U.S. on F1 student visa" = mean_f1_student_visa,
               "Entering without inspection" = mean_ewi,
               "Married when filing\nI-589" = has_dep_spouse_pct, 
               "Has dependent child when filing\nI-589" = has_dep_child_pct)

# inspect
demographics_by_prcl_table_formatted %>% data.frame()


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: Demographics for I-589 applicants, by applicant country political rights / civil liberties groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                  "Percentages are the respective counts as a share of filings.",
                                  sep = ""),
                                  "Source: Freedom House; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = demographics_by_prcl_table_formatted, 
                       output_sheet_names = "demographics_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(15, 7, 9, 9, 7, rep(9, times = 3), 7, 9)), 
                       custom_row_height = list(c(rep(NA, times = nrow(demographics_by_prcl_table_formatted) + 1), 15)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_eoir_outcome_durations_by_filing_fy_table ####

# get raio_adj_share
raio_adj_share <- data %>% group_by(filing_date_fy) %>%
        summarize(filing_count = n(),
                  raio_adj_count = sum(adjudicated_case_flag)) %>%
        mutate(raio_adj_pct = raio_adj_count / filing_count,
               raio_adj_pct = as_percent(raio_adj_pct, digits = 0)) %>%
        select(filing_date_fy, raio_adj_pct)

# inspect
raio_adj_share


#/////////////////////////////


# get eoir_adj_share
eoir_adj_share <- data %>% group_by(filing_date_fy) %>%
        summarize(referral_received_count = sum(eoir_received_flag),
                  eoir_adj_count = sum(eoir_adjudicated_flag)) %>%
        mutate(eoir_adj_pct = eoir_adj_count / referral_received_count,
               eoir_adj_pct = as_percent(eoir_adj_pct, digits = 0)) %>%
        select(filing_date_fy, eoir_adj_pct)

# inspect
eoir_adj_share


#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect entry_to_filing
# note that outliers were not removed from entry_to_filing_days because it's importantly bimodal b/c of cancellation of removal applicants,
# and we wanted to still be able to summarize the outer values in crosstabs
# so median will be used to summarize entry_to_filing
data %>% ggplot(data = ., aes(x = entry_to_filing_days)) + geom_histogram()
data %>% ggplot(data = ., aes(x = entry_to_filing_days)) + stat_ecdf()
data %>% summarize(entry_to_filing_days_mean = mean(entry_to_filing_days, na.rm = TRUE),
                   entry_to_filing_days_median = median(entry_to_filing_days, na.rm = TRUE))


#/#/#/#/#/#/#/#/#/#/#/#/#/


# create entry_to_filing_duration_by_filing_fy_table
entry_to_filing_duration_by_filing_fy_table <- data %>% group_by(filing_date_fy) %>%
        summarize(entry_to_filing_days_median = median(entry_to_filing_days, na.rm = TRUE)) %>%
        mutate(entry_to_filing_days_median = round(entry_to_filing_days_median, digits = 0))

# inspect
entry_to_filing_duration_by_filing_fy_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect filing_to_terminal_decision_days
# note that all pending outcomes have NA filing_to_terminal_decision_days 
# (also 71 admin_closed do, which is odd, but not worth inspecting - probably was negative and got NA'd)
data %>% filter(!is.na(filing_to_terminal_decision_days)) %>% count(outcome_bucket)
data %>% filter(is.na(filing_to_terminal_decision_days)) %>% count(outcome_bucket)

# inspect distribution
data %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()
data %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + stat_ecdf()

# also note that there is bimodal pattern for filing_to_terminal_decision dates shows up starting in 2015 (FIFO processing)
data %>% filter(outcome_date_fy == 2014) %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()
data %>% filter(outcome_date_fy == 2016) %>% ggplot(data = ., aes(x = filing_to_terminal_decision_days)) + geom_histogram()


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# create filing_to_terminal_decision_days_by_filing_fy_table
filing_to_terminal_decision_days_by_filing_fy_table <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        mutate(filing_to_terminal_decision_days_median = round(filing_to_terminal_decision_days_median, digits = 0))

# inspect
filing_to_terminal_decision_days_by_filing_fy_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect distribution for eoir_case_received_to_terminal_decision_days
data %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()
data %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + stat_ecdf()
data %>% filter(eoir_outcome_date_fy == 2011) %>% ggplot(data = ., aes(x = eoir_case_received_to_outcome_days)) + geom_histogram()

# create eoir_outcome_durations_by_filing_fy_table
eoir_outcome_durations_by_filing_fy_table <- data %>% 
        filter(eoir_received_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(referral_received_to_outcome_days_median = round(median(eoir_case_received_to_outcome_days, na.rm = TRUE))) %>%
        mutate(referral_received_to_outcome_days_median = round(referral_received_to_outcome_days_median, digits = 0))

# inspect 
eoir_outcome_durations_by_filing_fy_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect eoir_outcome_to_appeal_filed_days
data %>% ggplot(data = ., aes(x = eoir_outcome_to_appeal_filed_days)) + geom_histogram()
data %>% filter(eoir_appeal_filed_flag == 0) %>% count(eoir_outcome_to_appeal_filed_days)


#/#/#/#/#/#/#/#/#/


# create eoir_outcome_to_appeal_filed_days_by_filing_fy_table
eoir_outcome_to_appeal_filed_days_by_filing_fy_table <- data %>% group_by(filing_date_fy) %>% 
        summarize(eoir_outcome_to_appeal_filed_days_median = round(median(eoir_outcome_to_appeal_filed_days, na.rm = TRUE)))


#/#/#/#/#/#/#/#/


# inspect
eoir_outcome_to_appeal_filed_days_by_filing_fy_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect eoir_appeal_filed_to_bia_decision_days
data %>% ggplot(data = ., aes(x = eoir_appeal_filed_to_bia_decision_days)) + geom_histogram()
data %>% filter(eoir_appeal_filed_flag == 0) %>% count(eoir_appeal_filed_to_bia_decision_days)


#/#/#/#/#/#/#/#/#/


# create eoir_appeal_filed_to_bia_decision_days_by_filing_fy_table
eoir_appeal_filed_to_bia_decision_days_by_filing_fy_table <- data %>% group_by(filing_date_fy) %>% 
        summarize(eoir_appeal_filed_to_bia_decision_days_median = round(median(eoir_appeal_filed_to_bia_decision_days, na.rm = TRUE)))


#/#/#/#/#/#/#/#/


# inspect
eoir_appeal_filed_to_bia_decision_days_by_filing_fy_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# combine into raio_eoir_outcome_durations_by_filing_fy_table
raio_eoir_outcome_durations_by_filing_fy_table <- entry_to_filing_duration_by_filing_fy_table %>%
        left_join(., raio_adj_share, by = "filing_date_fy") %>% 
        left_join(., filing_to_terminal_decision_days_by_filing_fy_table, by = "filing_date_fy") %>%
        left_join(., eoir_adj_share, by = "filing_date_fy") %>% 
        left_join(., eoir_outcome_durations_by_filing_fy_table, by = "filing_date_fy") %>%
        left_join(., eoir_outcome_to_appeal_filed_days_by_filing_fy_table, by = "filing_date_fy") %>%
        left_join(., eoir_appeal_filed_to_bia_decision_days_by_filing_fy_table, by = "filing_date_fy")

# inspect
raio_eoir_outcome_durations_by_filing_fy_table
raio_eoir_outcome_durations_by_filing_fy_table %>% glimpse()


#/#/#/#/#/#/#/#/#/#/


# get raio_eoir_outcome_durations_by_filing_fy_table_formatted
raio_eoir_outcome_durations_by_filing_fy_table_formatted <- raio_eoir_outcome_durations_by_filing_fy_table %>%
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Adjudications by RAIO, as a share of filings" = raio_adj_pct,
               "Median days from entering U.S. to filing\nI-589" = entry_to_filing_days_median,
               "Adjudications by EOIR, as a share of referrals received" = eoir_adj_pct,
               "Median days from filing I-589 to RAIO completion" = filing_to_terminal_decision_days_median,
               "Median days from referral received to EOIR completion" = referral_received_to_outcome_days_median,
               "For appealed cases, median days from EOIR completion to appeal filed" = eoir_outcome_to_appeal_filed_days_median,
               "For appealed cases, median days from appeal filed to BIA completion" = eoir_appeal_filed_to_bia_decision_days_median)

# inspect
raio_eoir_outcome_durations_by_filing_fy_table_formatted %>% data.frame()


# ////////////////////////


# title: RAIO and EOIR processing durations, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: RAIO completions are those cases either granted, referred, denied, or administratively closed by RAIO; ", 
                                        "pending cases are excluded. ",
                                        "EOIR completions are those cases with either relief granted, removal / voluntary departure, terminated, ",
                                        "or administratively closed outcomes from EOIR; pending cases are excluded.", sep = ""),
                                "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = raio_eoir_outcome_durations_by_filing_fy_table_formatted, 
                       output_sheet_names = "raio_eoir_duration_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, NA, 11, NA, 11)), 
                       custom_row_height = list(c(125, rep(NA, times = nrow(raio_eoir_outcome_durations_by_filing_fy_table_formatted)), 40)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# # create filing_to_terminal_decision_days_by_outcome_fy_table 
# filing_to_terminal_decision_days_by_outcome_fy_table <- data %>% 
#         group_by(outcome_date_fy) %>% 
#         summarize(filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
#         mutate(filing_to_terminal_decision_days_median = round_to_digits(filing_to_terminal_decision_days_median, digits = 0)) %>%
#         filter(outcome_date_fy <= 2018)
# 
# 
# #/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # inspect
# filing_to_terminal_decision_days_by_outcome_fy_table
# data %>% mutate(filing_to_terminal_decision_days_flag = case_when(!is.na(filing_to_terminal_decision_days) ~ 1, TRUE ~ 0)) %>%
#         count(filing_to_terminal_decision_days_flag, outcome_bucket)
# data %>% 
#         filter(outcome_bucket %in% c("admin_closed", "deny", "grant")) %>%
#         group_by(outcome_date_fy) %>% 
#         summarize(filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
#         mutate(filing_to_terminal_decision_days_median = round_to_digits(filing_to_terminal_decision_days_median, digits = 0)) %>%
#         filter(outcome_date_fy <= 2018)
# 
# #/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # add sheet
# sheet <- "raio_duration_by_fy"
# addWorksheet(wb = workbook, sheet = sheet)
# 
# # add label
# label <- c("DRAFT PRE-DECISIONAL/DELIBERATIVE WORK PRODUCT")
# writeData(wb = workbook, sheet = sheet, x = label, 
#           borders = "none", borderStyle = "thin", startRow = 1, startCol = 1)
# 
# # add title
# title <- c("I-589 filing to RAIO completion time interval by FY, for the FY 2009-2018 I-589 filing cohort")
# writeData(wb = workbook, sheet = sheet, x = title, 
#           borders = "none", borderStyle = "thin", startRow = 3, startCol = 1)
# 
# # add table
# writeData(wb = workbook, sheet = sheet, x = raio_eoir_outcome_durations_by_filing_fy_table_formatted , 
#           borders = "all", borderStyle = "thin", startRow = 5, startCol = 1)
# 
# # inspect workbook
# openXL(workbook)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# # create eoir_outcome_durations_by_outcome_fy_table
# eoir_outcome_durations_by_outcome_fy_table <- data %>% 
#         filter(eoir_received_flag == 1) %>%
#         group_by(eoir_outcome_date_fy) %>% 
#         summarize(referral_received_to_outcome_days_median = median(eoir_case_received_to_outcome_days, na.rm = TRUE)) %>%
#         filter(eoir_outcome_date_fy <= 2018)
# 
# # inspect 
# eoir_outcome_durations_by_outcome_fy_table
# 
# 
# #/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # create eoir_outcome_to_appeal_filed_days_by_outcome_fy_table
# eoir_outcome_to_appeal_filed_days_by_outcome_fy_table <- data %>% group_by(eoir_outcome_date_fy) %>% 
#         summarize(eoir_outcome_to_appeal_filed_days_median = median(eoir_outcome_to_appeal_filed_days, na.rm = TRUE)) %>% 
#         filter(eoir_outcome_date_fy <= 2018)
# 
# 
# #/#/#/#/#/#/#/#/
# 
# 
# # inspect
# eoir_outcome_to_appeal_filed_days_by_outcome_fy_table
# 
# 
# #/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # inspect eoir_appeal_filed_to_bia_decision_days_by_outcome_fy
# # note that for outcome FY 2009-2017 approx 100% of appeals are completed
# # but it drops to 50% for outcome FY 2018, and then < 2% for outcome FY 2019/2020
# # so will drop outcome FY 2019/2020 from table showing processing times, since it could create confusion
# data %>% filter(is.na(eoir_bia_decision), !is.na(eoir_bia_decision_date)) %>% nrow() # 0
# data %>% group_by(eoir_outcome_date_fy) %>% summarize(appeal_filed_sum = sum(eoir_appeal_filed_flag),
#                                                       bia_decision_sum = sum(!is.na(eoir_bia_decision)),
#                                                       appeals_completed_pct = bia_decision_sum / appeal_filed_sum)
# 
# 
# #/#/#/#/#/#/#/#/#/
# 
# 
# # create eoir_appeal_filed_to_bia_decision_days_by_outcome_fy_table
# eoir_appeal_filed_to_bia_decision_days_by_outcome_fy_table <- data %>% group_by(eoir_outcome_date_fy) %>% 
#         summarize(eoir_appeal_filed_to_bia_decision_days_median = median(eoir_appeal_filed_to_bia_decision_days, na.rm = TRUE)) %>%
#         filter(eoir_outcome_date_fy <= 2018)
# 
# 
# #/#/#/#/#/#/#/#/
# 
# 
# # inspect
# eoir_appeal_filed_to_bia_decision_days_by_outcome_fy_table
# 
# 
# #/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # combine into eoir_outcome_durations_by_outcome_fy_table_unformatted
# eoir_outcome_durations_by_outcome_fy_table_unformatted <- eoir_outcome_durations_by_outcome_fy_table %>%
#         left_join(., eoir_outcome_to_appeal_filed_days_by_outcome_fy_table, by = "eoir_outcome_date_fy") %>%
#         left_join(., eoir_appeal_filed_to_bia_decision_days_by_outcome_fy_table, by = "eoir_outcome_date_fy")
# 
# # inspect
# eoir_outcome_durations_by_outcome_fy_table_unformatted
# 
# 
# #/#/#/#/#/#/#/#/#/#/
# 
# 
# # get eoir_outcome_durations_by_outcome_fy_table_formatted
# eoir_outcome_durations_by_outcome_fy_table_formatted <- eoir_outcome_durations_by_outcome_fy_table_unformatted %>%
#         mutate(referral_received_to_outcome_days_median = comma(referral_received_to_outcome_days_median),
#                eoir_appeal_filed_to_bia_decision_days_median = comma(eoir_appeal_filed_to_bia_decision_days_median)) %>%
#         rename("FY" = eoir_outcome_date_fy,
#                "Median days from referral received to EOIR completion" = referral_received_to_outcome_days_median,
#                "For appealed cases, median days from EOIR completion to appeal filed" = eoir_outcome_to_appeal_filed_days_median,
#                "For appealed casees, median days from appeal filed to BIA completion" = eoir_appeal_filed_to_bia_decision_days_median)
# 
# # inspect
# eoir_outcome_durations_by_outcome_fy_table_formatted 
# 
# 
# #/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
# 
# 
# # add sheet
# sheet <- "eoir_durations_by_fy"
# addWorksheet(wb = workbook, sheet = sheet)
# 
# # add label
# label <- c("DRAFT PRE-DECISIONAL/DELIBERATIVE WORK PRODUCT")
# writeData(wb = workbook, sheet = sheet, x = label, 
#           borders = "none", borderStyle = "thin", startRow = 1, startCol = 1)
# 
# # add title
# title <- c("EOIR time intervals by FY")
# writeData(wb = workbook, sheet = sheet, x = title, 
#           borders = "none", borderStyle = "thin", startRow = 3, startCol = 1)
# 
# # add table
# writeData(wb = workbook, sheet = sheet, x = raio_eoir_outcome_durations_by_outcome_fy_table_formatted , 
#           borders = "all", borderStyle = "thin", startRow = 5, startCol = 1)
# 
# # inspect workbook
# openXL(workbook)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create raio_durations_by_outcome
 


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create ead_durations_by_filing_fy_table ####

# load avg_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table
# created in ead_scratchpad.R
avg_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table <- read_csv("data/I-765/avg_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table_20200503.csv")
avg_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table

# drop mean, keep median
median_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table <- avg_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table %>%
        select(filing_date_fy, median_filing_to_initial_ead_filing_days)
median_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# load avg_initial_ead_processing_days_by_filing_fy_cohort_table
# created in ead_scratchpad.R
avg_initial_ead_processing_days_by_filing_fy_cohort_table <- read_csv("data/I-765/avg_initial_ead_processing_days_by_filing_fy_cohort_table_20200503.csv")
avg_initial_ead_processing_days_by_filing_fy_cohort_table

# drop mean, keep median
median_initial_ead_processing_days_by_filing_fy_cohort_table <- avg_initial_ead_processing_days_by_filing_fy_cohort_table %>%
        select(filing_date_fy, median_initial_ead_processing_days)
median_initial_ead_processing_days_by_filing_fy_cohort_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# load avg_renewal_ead_processing_days_by_filing_fy_cohort_table
# created in ead_scratchpad.R
avg_renewal_ead_processing_days_by_filing_fy_cohort_table <- read_csv("data/I-765/avg_renewal_ead_processing_days_by_filing_fy_cohort_table_20200503.csv")
avg_renewal_ead_processing_days_by_filing_fy_cohort_table

# drop mean, keep median
median_renewal_ead_processing_days_by_filing_fy_cohort_table <- avg_renewal_ead_processing_days_by_filing_fy_cohort_table %>%
        select(filing_date_fy, median_renewal_ead_processing_days)
median_renewal_ead_processing_days_by_filing_fy_cohort_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# load avg_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table
# created in ead_scratchpad.R
avg_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table <- read_csv("data/I-765/avg_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table_20200503.csv")
avg_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table

# drop mean, keep median
median_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table <- avg_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table %>%
        select(filing_date_fy, median_i589_filing_to_initial_ead_approval_days)
median_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# combine to create ead_durations_by_filing_fy_table 
ead_durations_by_filing_fy_table <- median_i589_filing_to_initial_ead_filing_days_by_filing_fy_cohort_table %>%
        left_join(., median_initial_ead_processing_days_by_filing_fy_cohort_table, by = "filing_date_fy") %>%
        left_join(., median_renewal_ead_processing_days_by_filing_fy_cohort_table, by = "filing_date_fy") %>%
        left_join(., median_i589_filing_to_initial_ead_approval_days_by_filing_fy_cohort_table, by = "filing_date_fy")
ead_durations_by_filing_fy_table        


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get ead_durations_by_filing_fy_table_formatted
ead_durations_by_filing_fy_table_formatted <- ead_durations_by_filing_fy_table %>% 
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy, 
               "Median days from\nI-589 filing\nto initial\nI-765 filing" = median_filing_to_initial_ead_filing_days,
               "Median days from\ninitial\nI-765 filing\nto outcome" = median_initial_ead_processing_days,
               "Median days from\nrenewal\nI-765 filing\nto outcome" = median_renewal_ead_processing_days,
               "Median days from\nI-589 filing\nto initial\nI-765 approval" = median_i589_filing_to_initial_ead_approval_days)

# inspect
ead_durations_by_filing_fy_table_formatted


# ////////////////////////


# title: EAD processing durations, by I-589 FY filing cohort 

# create footnote_table
footnote_table <- tibble(text = c("Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_durations_by_filing_fy_table_formatted, 
                       output_sheet_names = "ead_duration_by_filing_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(12, 20, 20, 20, 20)), custom_row_height = list(c(75)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/
#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create state_metrics_table ####

# read in ead princ/dep data
ead <- read_csv("data/I-765/i765_pa_raw.csv")
ead_dep <- read_csv("data/I-765/i765_da_raw.csv")

# combine princ/dep to get eads_by_state
eads_by_state <-  ead_dep %>% select(dep_anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>% 
        rename(anumber = dep_anumber) %>% 
        filter(anumber %in% i589_dep$dep_anumber) %>%
        bind_rows(., ead %>% select(anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>%
                          filter(anumber %in% data$anumber)) %>%
        filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"))


#/#/#/#/#/#/#/#/#/#/#/#


# inspect 
eads_by_state %>% glimpse()
eads_by_state %>% nrow() # 1457838
eads_by_state %>% distinct(anumber) %>% nrow() # 735182
data %>% distinct(anumber) %>% nrow() # 672403
i589_dep %>% distinct(anumber) %>% nrow() # 170873
672403 + 170873 == 843276
eads_by_state %>% count(CURRENT_STATUS)
eads_by_state %>% count(part_2_1)
eads_by_state %>% count(ead_len) %>% 
        arrange(desc(n)) %>% slice(1:30) %>% print(n = nrow(.))
eads_by_state %>% count(ead_len) %>% arrange(desc(ead_len))
eads_by_state %>% mutate(ead_len2 = as.numeric(valid_to - valid_from), match = case_when(ead_len == ead_len2 ~ 1, TRUE ~ 0)) %>% 
        filter(!is.na(ead_len2)) %>% count(match)
eads_by_state %>% filter(ead_len > 745) %>% nrow() # 584
eads_by_state %>% filter(ead_len > 745) %>% count(ead_len) %>% arrange(desc(n))
eads_by_state %>% filter(ead_len < 350) %>% nrow() # 298
eads_by_state %>% filter(ead_len < 350) %>% count(ead_len) %>% arrange(desc(n))
eads_by_state %>% filter(ead_len > 400, ead_len < 700) %>% nrow() # 548
eads_by_state %>% filter(is.na(valid_from), !is.na(ead_len)) %>% nrow() # 0
eads_by_state %>% filter(is.na(valid_to), !is.na(ead_len)) %>% nrow() # 0
eads_by_state %>% filter(is.na(valid_to), !is.na(valid_from)) %>% nrow() # 0
eads_by_state %>% filter(!is.na(valid_to), is.na(valid_from)) %>% nrow() # 19
eads_by_state %>% 
        ggplot(data = ., aes(x = ead_len)) + geom_histogram()
eads_by_state %>% filter() %>% 
        ggplot(data = ., aes(x = ead_len)) + stat_ecdf()
eads_by_state %>% filter() %>% count(ead_len) %>% 
        arrange(desc(n)) %>% slice(1:10) %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = factor(as.character(ead_len)), .x = n, .desc = TRUE), y = n)) + geom_col()


# inspect ben_state
eads_by_state %>% 
        distinct(anumber, ben_state) %>% count(ben_state) %>% arrange(desc(n)) %>%
        mutate(pct = n / sum(n), cum_pct = cumsum(pct)) %>% print(n = nrow(.))
eads_by_state %>% 
        select(ben_state) %>% anti_join(tibble(state = c(state.abb, "PR", "DC")), ., by = c("state" = "ben_state"))


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# the vast majority of ead_len is either 365 or 730 as expected - will round the 4054 records with odd ead_len to 365 or 730, whichever is closest
eads_by_state <- eads_by_state %>% mutate(ead_len_clean = case_when(ead_len < 365 ~ 365, 
                                                                    ead_len >= 365 & ead_len <= 547.5 ~ 365, 
                                                                    ead_len > 547.5 ~ 730, 
                                                                    TRUE ~ NA_real_))


#/#/#/#/#/#/#/#/#/#/#/#/#/#


# inspect 
eads_by_state %>% count(ead_len_clean)
# note the vast majority of "cleaned" ead_len were 729 and 364, and most of the rest were a few days off
eads_by_state %>% filter(!is.na(ead_len), ead_len != ead_len_clean, !(ead_len %in% c(729, 364))) %>% nrow() # 4054
eads_by_state %>% filter(!is.na(ead_len), ead_len != ead_len_clean, 
                         !(ead_len %in% c(729, 364, 366))) %>% select(valid_from, valid_to, ead_len, ead_len_clean)
eads_by_state %>% filter(!is.na(ead_len), ead_len != ead_len_clean) %>% count(ead_len) %>% arrange(desc(n))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect for eligibility_fy_1 and eligibility_fy_2

# note that we could just sum the approved receipt numbers by ben_state, but that would double count 
# those with multiple approved records for a given fy (which it turns out is only ~ 15k)
# originally i was going to use the eligibility FY directly, but final decision is to just sum up all approved EADS by state over all years
# and show that as a share of sum of all laborforce by state over all years
# so the eligibility_fy_1/2 stuff below is maybe unnecessary now, but it does still have the benefit of allowing us to 
# drop the 15k records with duplicate overlapping eligibility periods

# get fy cutoff for getting credited with eligibility in a given fy
# result: must have valid_from date between 10/1 and 7/2 to get credited with eligibility for that FY
# any later that 7/2, and the majority of the 365 day elibility period would fall in the subsequent FY
ymd("2010-09-30") - 182.5 # 2010-07-02


#/#/#/#/#/#/#/#/#/#/


# get eligibility_fy_1 and eligibility_fy_2
eads_by_state <- eads_by_state %>% mutate(valid_from_year = year(valid_from), valid_from_month = month(valid_from),
                                          fy_eligibility_cutoff = case_when(valid_from_month >= 10 ~ ymd(str_c(valid_from_year + 1, "-07-02")),
                                                                            valid_from_month < 10 ~ ymd(str_c(valid_from_year, "-07-02"))),
                                          eligibility_fy_1 = case_when(valid_from < fy_eligibility_cutoff ~ fy(valid_from),
                                                                       valid_from > fy_eligibility_cutoff ~ fy(valid_from) + 1),
                                          eligibility_fy_2 = case_when(ead_len_clean == 730 ~ eligibility_fy_1 + 1))


#/#/#/#/#/#/#/#/#/#/


# inspect
eads_by_state %>% glimpse()
eads_by_state %>% count(eligibility_fy_1)
eads_by_state %>% count(eligibility_fy_2)
eads_by_state %>% pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(valid_to)) %>%
        select(anumber, valid_from, valid_to, eligibility_period, eligibility_fy) %>% sample_n(20)
eads_by_state %>% filter(!is.na(valid_to)) %>% select(valid_from, valid_to, ead_len_clean, eligibility_fy_1, eligibility_fy_2) %>% sample_n(20)
eads_by_state %>% count(anumber, eligibility_fy_1) %>% arrange(desc(n))
# note there are 14718 duplicate anumber/eligibility_fy_1 records, not a huge deal, but will call distinct() on these below to avoid double-counting
eads_by_state %>% count(anumber, eligibility_fy_1) %>% filter(n > 1) %>% nrow() # 14718


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect top_states_for_i589_filing
data %>% count(state) %>% arrange(desc(n)) %>% print(n = 20)
data %>% count(state) %>% anti_join(., tibble(state = state.abb), by = "state")
# note that top 10 states by i589 filing is almost same as top 10 states for i765 filing (slight order diff, and WA/GA are different only bc of cutoff at 10)
# will use top 10 for i589 filing in analysis
eads_by_state %>% pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(eligibility_fy)) %>% distinct(anumber, eligibility_fy, ben_state) %>%
        count(ben_state) %>% arrange(desc(n)) %>% print(n = 20)

# check state filing counts by year to see whether annual avg filing counts is distorting/missing any trends/blips
# result: yes, avg annual filing/entry counts are missing huge increases in big states
# eg annual avg filing for florida is 109k, but actual annual filings range from 25k to 300k
# better to show raw totals for filings/entries, and then use annual avg only for ead/workforce stats
data %>% count(filing_date_fy, state) %>% arrange(desc(n)) %>% 
        left_join(data %>% count(state) %>% arrange(desc(n)) %>% slice(1:10) %>% select(state), ., by = "state") %>%
        ggplot(data = ., aes(x = filing_date_fy, y = n, color = state)) + geom_line()


#/#/#/#/#/#/#/#/#/#/


# get top_states_for_i589_filing
top_states_for_i589_filing <- data %>% count(state) %>% arrange(desc(n)) %>% rename(i589_filing_count = n) %>%
        mutate(i589_filing_count_annual_avg = i589_filing_count / 10,
                i589_filing_count_as_pct_of_all_i589_filing = i589_filing_count / sum(i589_filing_count)) %>% slice(1:10)
top_states_for_i589_filing


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# add state_of_entry by parsing port_of_entry, and inspect
# note i manually fix some odd state_of_entry parsings, but only bother with top 10 filing states
data <- data %>% mutate(state_of_entry = str_replace(string = port_of_entry, pattern = regex(".*, "), replacement = ""),
                        state_of_entry = str_replace(string = state_of_entry, pattern = regex(" \\(.*"), replacement = ""),
                        state_of_entry = case_when(state_of_entry == "WASHINGTON DC" ~ "DC",
                                                   state_of_entry == "OTAY MESA CA" ~ "CA",
                                                   state_of_entry == "FL       ARUBA PRECLEARANCE" ~ "FL",
                                                   state_of_entry == "FL   PRECLEARANCE" ~ "FL",
                                                   state_of_entry == "PASO DEL NORTE,TX" ~ "TX",
                                                   state_of_entry == "WASHINGTON ,DC PRECLEARANCE" ~ "DC",
                                                   state_of_entry == "SEATTLE AIRPORT" ~ "WA",
                                                   state_of_entry == "OAKLAND CNTY INTL AIRPRT(USER FEE)" ~ "CA",
                                                   state_of_entry == "WA SEAPORT" ~ "WA",
                                                   state_of_entry == "FL SEAPLANE BASE" ~ "FL",
                                                   state_of_entry == "NY WHIRLPOOL BRDG" ~ "NY",
                                                   state_of_entry == "WA VICTORIA PRECLEAR" ~ "WA",
                                                   state_of_entry == "WORLD TRADE BRIDGE - LAREDO" ~ "TX", TRUE ~ state_of_entry)) 


#/#/#/#/#/#/#/#/#/#


# inspect
# the only remaining port_of_entry values that didn't parse are non-top 10 states that i didn't bother manually correcting
data %>% anti_join(., tibble(state = c(state.abb, "DC", "UNKNOWN")), by = c("state_of_entry" = "state")) %>% 
        count(state_of_entry) %>% arrange(desc(n)) %>% print(n = nrow(.))
data %>% count(state_of_entry) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n)) %>% print(n = 20)

# note that sum(state_of_entry_count) below is 672403, the same as sum(filing_count)
# i just divide by state_of_entry_count for convenience since the variable is already there
# but effectively, the entry/filing_count_as_pct variables are both the respective entry/filing counts as a share of total filings
# and noting it this way in the footnotes will be less confusing
data %>% count(state_of_entry) %>% arrange(desc(n)) %>% rename(state_of_entry_count = n) %>% 
         mutate(state_of_entry_count_as_pct_of_all_state_of_entry = state_of_entry_count / sum(state_of_entry_count)) %>% 
        summarize(sum = sum(state_of_entry_count))


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# add state_of_entry_count and state_of_entry_pct to top_states_for_i589_filing
top_states_for_i589_filing <- data %>% count(state_of_entry) %>% arrange(desc(n)) %>% rename(state_of_entry_count = n) %>% 
        mutate(state_of_entry_count_as_pct_of_all_state_of_entry = state_of_entry_count / sum(state_of_entry_count)) %>%
        left_join(top_states_for_i589_filing, ., by = c("state" = "state_of_entry")) %>%
        select(state, state_of_entry_count, state_of_entry_count_as_pct_of_all_state_of_entry, 
               i589_filing_count, i589_filing_count_annual_avg, i589_filing_count_as_pct_of_all_i589_filing)
top_states_for_i589_filing


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get step 1 of avg_duration_of_ead_eligibility_period
# note this is deliberately broken into two steps to allow for inspection mid-way
avg_duration_of_ead_eligibility_period <- eads_by_state %>% 
        pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(eligibility_fy), eligibility_fy <= 2019) %>% distinct(anumber, eligibility_fy, ben_state) %>%
        group_by(anumber, ben_state) %>% mutate(eligibility_fy_n_distinct = n_distinct(eligibility_fy)) %>%
        ungroup() 


#/#/#/#/#/#/#/#/#/#/#/


# inspect
# note that only 1573 anumbers have eads in more than one state in a given fy, and the max number of simultaneous state eligibilities is 2, which seems ok to ignore
avg_duration_of_ead_eligibility_period %>% print(n = 20)
avg_duration_of_ead_eligibility_period %>% arrange(desc(eligibility_fy_n_distinct))
avg_duration_of_ead_eligibility_period %>% count(anumber, eligibility_fy) %>% arrange(desc(n)) # 2 is max simultaneous state eligibilities
avg_duration_of_ead_eligibility_period %>% count(anumber, eligibility_fy) %>% filter(n > 1) %>% distinct(anumber) %>% nrow() # 1573
avg_duration_of_ead_eligibility_period %>% filter(anumber == "A072404488")
avg_duration_of_ead_eligibility_period %>% filter(anumber == "A089922518")
avg_duration_of_ead_eligibility_period %>% distinct(anumber, ben_state, eligibility_fy_n_distinct) %>%
        ggplot(data = ., aes(x = eligibility_fy_n_distinct)) + geom_histogram()

# note the mean/median_eligibility_fy_count_per_anumber is similiar (2 vs 2-3ish)
# result: will use mean since it shows somewhat more variation
avg_duration_of_ead_eligibility_period %>% distinct(anumber, ben_state, eligibility_fy_n_distinct) %>%
        group_by(ben_state) %>%
        summarize(mean_eligibility_fy_n_distinct = mean(eligibility_fy_n_distinct), median_eligibility_fy_n_distinct = median(eligibility_fy_n_distinct))
avg_duration_of_ead_eligibility_period %>% distinct(anumber, ben_state, eligibility_fy_n_distinct) %>%
        group_by(ben_state) %>%
        summarize(mean_eligibility_fy_n_distinct = mean(eligibility_fy_n_distinct), median_eligibility_fy_n_distinct = median(eligibility_fy_n_distinct)) %>%
        arrange(desc(avg_eligibility_fy_count_per_anumber))


#/#/#/#/#/#/#/#/#/#/#


# get step 2 of avg_duration_of_ead_eligibility_period and join with top_states_for_i589_filing
top_states_for_i589_filing <- avg_duration_of_ead_eligibility_period %>% distinct(anumber, ben_state, eligibility_fy_n_distinct) %>%
        group_by(ben_state) %>% summarize(avg_eligibility_fy_count_per_anumber = mean(eligibility_fy_n_distinct)) %>%
        ungroup() %>% left_join(top_states_for_i589_filing, ., by = c("state" = "ben_state"))
top_states_for_i589_filing


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#


# get state_metrics_table, adding total_approved_eads, annual_avg_approved_eads, and pct_of_total_approved_eads to top_states_for_i589_filing
# note that eads_by_state is filtered down to records with eligibility_fy <= 2019, so only fully completely ead years are included
state_metrics_table <- eads_by_state %>% 
        pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(eligibility_fy), eligibility_fy <= 2019) %>% distinct(anumber, eligibility_fy, ben_state) %>%
        count(ben_state) %>% rename(fy_ead_approvals_sum = n) %>%
        mutate(fy_ead_approvals_annual_avg = fy_ead_approvals_sum / 11) %>% 
        left_join(top_states_for_i589_filing, ., by = c("state" = "ben_state"))
state_metrics_table


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# create get_state_avg_laborforce_by_fy function
get_state_avg_laborforce_by_fy <- function(current_path) {
        
        # get state_abbreviations
        state_abbreviations <- tibble(state_name = state.name, state = state.abb) %>%
                bind_rows(., tibble(state_name = "Puerto Rico", state = "PR"), tibble(state_name = "District of Columbia", state = "DC"))
        
        # get current_state_name
        current_state_name <- read_excel(path = current_path, col_names = "state", range = "B8") %>% 
                pull(state)
        print(current_state_name)
        
        # get current_state_data
        current_state_data <- read_excel(path = current_path, range = "A12:n25")
        current_state_data
        
        # calculate average laborforce by fy
        current_state_data <- current_state_data %>% pivot_longer(cols = -Year, names_to = "month", values_to = "count") %>%
                mutate(month_numeric = case_when(month == "Jan" ~ 1, 
                                                 month == "Feb" ~ 2,
                                                 month == "Mar" ~ 3,
                                                 month == "Apr" ~ 4,
                                                 month == "May" ~ 5,
                                                 month == "Jun" ~ 6,
                                                 month == "Jul" ~ 7,
                                                 month == "Aug" ~ 8,
                                                 month == "Sep" ~ 9,
                                                 month == "Oct" ~ 10,
                                                 month == "Nov" ~ 11,
                                                 month == "Dec" ~ 12),
                       date = str_c(Year, "-", month_numeric, "-01"), 
                       date = ymd(date), fy = fy(date)) %>%
                filter(month != "Annual") %>%
                group_by(fy) %>% summarize(fy_avg = mean(count)) %>%
                ungroup() %>% filter(fy > 2008, fy <= 2019) %>%
                mutate(state_name = current_state_name) %>%
                left_join(., state_abbreviations, by = "state_name")
        
        # return current_state_data
        return(current_state_data)
}


#/#/#/#/#/#/#/#/#/#/#


# get laborforce data by mapping over laborforce files calling get_state_avg_laborforce_by_fy
laborforce <- map(.x = dir_ls("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead/data/population/us_states_laborforce"),
    .f = ~ get_state_avg_laborforce_by_fy(current_path = .x)) %>% bind_rows() 


#/#/#/#/#/#/#/#/#/#/


# inspect
laborforce
laborforce %>% count(state) %>% distinct(n) # 11
laborforce %>% distinct(state) %>% nrow() # 52
laborforce %>% anti_join(., tibble(state = state.abb), by = "state") %>% distinct(state_name, state) # just PR and DC
laborforce %>% count(fy)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get laborforce_sum that sums the fy_annual_avg for each state
laborforce_sum <- laborforce %>% 
        group_by(state) %>% summarize(fy_laborforce_sum = sum(fy_avg), laborforce_mean = mean(fy_avg), 
                                      laborforce_annual_avg = fy_laborforce_sum / 11) %>% ungroup()
laborforce_sum


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# join laborforce_sum with state_metrics_table 
# add approved_eads_as_pct_of_laborforce
state_metrics_table <- state_metrics_table %>% left_join(., laborforce_sum %>% select(state, fy_laborforce_sum, laborforce_annual_avg), by = "state") %>%
        mutate(fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum = fy_ead_approvals_sum / fy_laborforce_sum,
               avg_fy_ead_approvals_per_100k_laborforce = fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum * 100000)


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# inspect state_metrics_table
state_metrics_table %>% data.frame()


#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/#/


# get state_metrics_table_formatted
state_metrics_table_formatted <- state_metrics_table %>% select(state, state_of_entry_count, state_of_entry_count_as_pct_of_all_state_of_entry,
                               i589_filing_count, i589_filing_count_as_pct_of_all_i589_filing,
                               fy_ead_approvals_annual_avg, 
                               laborforce_annual_avg, fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum, avg_eligibility_fy_count_per_anumber) %>%
        mutate(state_of_entry_count = comma(state_of_entry_count), 
               state_of_entry_count_as_pct_of_all_state_of_entry = as_percent(state_of_entry_count_as_pct_of_all_state_of_entry, digits = 1),
               state_of_entry_count = str_c(state_of_entry_count, 
                                            ifelse(nchar(state_of_entry_count_as_pct_of_all_state_of_entry) == 5, " (", "   ("), 
                                            state_of_entry_count_as_pct_of_all_state_of_entry, ")"),
               i589_filing_count = comma(i589_filing_count),
               i589_filing_count_as_pct_of_all_i589_filing = as_percent(i589_filing_count_as_pct_of_all_i589_filing, digits = 1),
               i589_filing_count = str_c(i589_filing_count, 
                                            ifelse(nchar(i589_filing_count_as_pct_of_all_i589_filing) == 5, " (", "   ("), 
                                            i589_filing_count_as_pct_of_all_i589_filing, ")"),
               fy_ead_approvals_annual_avg = round_to_digits(fy_ead_approvals_annual_avg, digits = 0),
               laborforce_annual_avg = comma(laborforce_annual_avg), 
               fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum = as_percent(fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum, digits = 2),
               avg_eligibility_fy_count_per_anumber = round_to_digits(avg_eligibility_fy_count_per_anumber, digits = 1)) %>%
        select(state, state_of_entry_count, i589_filing_count, fy_ead_approvals_annual_avg, laborforce_annual_avg, 
               fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum, avg_eligibility_fy_count_per_anumber) %>%
        rename(State = state, "I-589 principals entering U.S. via Port of Entry in state" = state_of_entry_count,
               "I-589 principals residing in state when filing" = i589_filing_count,
               "Avg. annual count of I-589 principals and dependents with EAD" = fy_ead_approvals_annual_avg,
               "Avg. annual state laborforce" = laborforce_annual_avg, 
               "Avg. annual count of I-589 principals and dependents with EADs as a share of state laborforce" = fy_ead_approvals_sum_as_pct_of_fy_laborforce_sum,
               "Avg. number of years I-589 principals and dependents have approved EADs in state" = avg_eligibility_fy_count_per_anumber)

# inspect
state_metrics_table_formatted %>% data.frame()


# ////////////////////////


# title: I-589 applicants and EADs, by top I-589 filing state of residence

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals entering U.S. via Port of Entry in state '",
                                        "and 'I-589 principals residing in state when filing' percentages ",
                                        "in parenthesis are the respective counts as a share of total filings. "),
                                "Source: Dept. of Labor - Bureau of Labor Statistics; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = state_metrics_table_formatted, 
                       output_sheet_names = "state_metrics", 
                       text_cols = NULL, 
                       custom_col_width = list(c(rep(NA, times = 5), 13, 13)), 
                       custom_row_height = list(c(105, rep(NA, times = nrow(state_metrics_table_formatted)), 30)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create state_i589_share_of_laborforce_by_fy ####

anumber_w_ead_count_by_state_and_fy <- eads_by_state %>% 
        pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(eligibility_fy), eligibility_fy <= 2019) %>% distinct(anumber, eligibility_fy, ben_state) %>%
        count(ben_state, eligibility_fy) %>% rename(anumbers_w_ead_eligibility = n)


#////////////////////////////


# inspect
anumber_w_ead_count_by_state_and_fy
anumber_w_ead_count_by_state_and_fy %>% glimpse()

# note that a handful of territories are in anumber_w_ead_count_by_state_and_fy but not in laborforce
# since laborforce only has 50 states + DC and PR; 
# not an issue though, since table will be limited to top 10 i589 filing states, and territories are not in top
anumber_w_ead_count_by_state_and_fy %>% anti_join(., laborforce, by = c("ben_state" = "state")) %>% count(ben_state)
laborforce %>% filter(state == "VI") %>% nrow() # 0
laborforce %>% distinct(state) %>% nrow() # 52
laborforce %>% filter(state %in% c("DC", "PR")) %>% distinct(state) %>% nrow() # 2

laborforce %>% anti_join(., anumber_w_ead_count_by_state_and_fy, by = c("state" = "ben_state")) %>% count(state) %>% nrow() # 0


#////////////////////////////


# get anumber_w_ead_count_by_top_state_and_fy, filtering down to top i589 filing states and joining fy_avg state laborforce
anumber_w_ead_count_by_top_state_and_fy <- anumber_w_ead_count_by_state_and_fy %>% filter(ben_state %in% top_states_for_i589_filing$state) %>%
        left_join(., laborforce, by = c("ben_state" = "state", "eligibility_fy" = "fy")) %>%
        rename("fy_avg_laborforce" = "fy_avg") %>%
        mutate(eads_as_share_of_laborforce = anumbers_w_ead_eligibility / fy_avg_laborforce) %>%
        left_join(top_states_for_i589_filing %>% select(state), ., by = c("state" = "ben_state"))
        

#////////////////////////////


# inspect
anumber_w_ead_count_by_top_state_and_fy
anumber_w_ead_count_by_top_state_and_fy %>% count(state)
anumber_w_ead_count_by_top_state_and_fy %>% filter(state == "CA", eligibility_fy == "2013") # 9844 / 18630039 / .000528
anumber_w_ead_count_by_state_and_fy %>% filter(ben_state == "CA", eligibility_fy == "2013") # 9844
laborforce %>% filter(state == "CA", fy == 2013) # 18630039
9844 / 18630039

# check against state_metrics_table created above
state_metrics_table_formatted 
state_metrics_table_formatted %>% glimpse()
state_metrics_table
state_metrics_table %>% glimpse()

# test that state avg anumbers_w_ead counts match with state_metrics_table from above
expect_equal(object = anumber_w_ead_count_by_top_state_and_fy %>% group_by(state) %>% 
        summarize(anumbers_w_ead_eligibility_mean = mean(anumbers_w_ead_eligibility)) %>% arrange(desc(anumbers_w_ead_eligibility_mean)) %>%
                pull(anumbers_w_ead_eligibility_mean),
        expected = state_metrics_table %>% arrange(desc(fy_ead_approvals_annual_avg)) %>% pull(fy_ead_approvals_annual_avg))

# test that state total anumbers_w_ead counts match with state_metrics_table from above
expect_equal(object = anumber_w_ead_count_by_top_state_and_fy %>% group_by(state) %>% 
                     summarize(anumbers_w_ead_eligibility_sum = sum(anumbers_w_ead_eligibility)) %>% arrange(desc(anumbers_w_ead_eligibility_sum)) %>%
                     pull(anumbers_w_ead_eligibility_sum),
             expected = state_metrics_table %>% arrange(desc(fy_ead_approvals_sum)) %>% pull(fy_ead_approvals_sum))


#////////////////////////////


# get anumber_w_ead_count_by_top_state_and_fy_formatted
anumber_w_ead_count_by_top_state_and_fy_formatted <- anumber_w_ead_count_by_top_state_and_fy %>%
        select(state, eligibility_fy, eads_as_share_of_laborforce) %>%
        mutate(eads_as_share_of_laborforce = case_when(eads_as_share_of_laborforce < .0001 ~ "< 0.01%",
                                                       TRUE ~ as_percent(eads_as_share_of_laborforce, digits = 2))) %>%
        pivot_wider(id_cols = eligibility_fy, names_from = state, values_from = eads_as_share_of_laborforce) %>%
        rename("FY" = eligibility_fy)

# inspect
anumber_w_ead_count_by_top_state_and_fy_formatted


#////////////////////////////


# title: I-589 principals and dependents with EAD eligibility as a share of state laborforce 

# create footnote_table
footnote_table <- tibble(text = c("Source: Dept. of Labor - Bureau of Labor Statistics; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = anumber_w_ead_count_by_top_state_and_fy_formatted, 
                       output_sheet_names = "ead_share_state_laborforce", 
                       text_cols = NULL, 
                       custom_col_width = list(c(7, rep(8, times = nrow(anumber_w_ead_count_by_top_state_and_fy_formatted) - 1))), 
                       custom_row_height = list(c(40)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create income_filing_share_by_fy_cohort_table ####
income_filing_share_by_fy_cohort_table <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_income_count = sum(`income_group_bucket.Low income`),
                  lower_middle_income_count = sum(`income_group_bucket.Lower middle income`),
                  upper_middle_income_count = sum(`income_group_bucket.Upper middle income`),
                  high_income_count = sum(`income_group_bucket.High income`),
                  na_income_count = sum(income_group_bucket.NA)) %>%
        ungroup() %>%
        mutate(low_income_pct = low_income_count / filed_count,
               lower_middle_income_pct = lower_middle_income_count / filed_count,
               upper_middle_income_pct = upper_middle_income_count / filed_count,
               high_income_pct = high_income_count / filed_count,
               na_income_pct = na_income_count / filed_count) %>% 
        select(filing_date_fy, filed_count, low_income_count, low_income_pct, lower_middle_income_count, lower_middle_income_pct, 
               upper_middle_income_count, upper_middle_income_pct, high_income_count, high_income_pct,
               na_income_count, na_income_pct)


# //////////////////


# inspect
income_filing_share_by_fy_cohort_table

# test that filed_count sums to total record count
expect_equal(object = income_filing_share_by_fy_cohort_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# test that income bucket counts sum to filed count for each fy
expect_equal(object = income_filing_share_by_fy_cohort_table %>% 
                     mutate(income_bucket_sum = low_income_count + lower_middle_income_count + upper_middle_income_count + 
                                    high_income_count + na_income_count) %>% pull(income_bucket_sum),
             expected = income_filing_share_by_fy_cohort_table %>% pull(filed_count))

# test that income bucket pcts sum to 1 for each fy
expect_equal(object = income_filing_share_by_fy_cohort_table %>% 
                     mutate(income_bucket_pct_sum = low_income_pct + lower_middle_income_pct + upper_middle_income_pct + 
                                    high_income_pct + na_income_pct) %>% pull(income_bucket_pct_sum),
             expected = rep(1, times = nrow(income_filing_share_by_fy_cohort_table)))


# //////////////////////////


# get income_filing_share_by_fy_cohort_table_formatted
income_filing_share_by_fy_cohort_table_formatted <- income_filing_share_by_fy_cohort_table %>% 
        mutate(filed_count = comma(filed_count),
               filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_income_pct = as_percent(low_income_pct, digits = 0),
               lower_middle_income_pct = as_percent(lower_middle_income_pct, digits = 0),
               upper_middle_income_pct = as_percent(upper_middle_income_pct, digits = 0),
               high_income_pct = as_percent(high_income_pct, digits = 0),
               na_income_pct = as_percent(na_income_pct, digits = 0),
               low_income_count = str_c(comma(low_income_count), ifelse(nchar(low_income_pct) == 3, " (", "   ("), low_income_pct, ")"),
               lower_middle_income_count = str_c(comma(lower_middle_income_count), ifelse(nchar(lower_middle_income_pct) == 3, " (", "   ("), lower_middle_income_pct, ")"),
               upper_middle_income_count = str_c(comma(upper_middle_income_count), ifelse(nchar(upper_middle_income_pct) == 3, " (", "   ("), upper_middle_income_pct, ")"),
               high_income_count = str_c(comma(high_income_count), ifelse(nchar(high_income_pct) == 3, " (", "   ("), high_income_pct, ")"),
               na_income_count = str_c(comma(na_income_count), ifelse(nchar(na_income_pct) == 3, " (", "   ("), na_income_pct, ")")) %>% 
        select(filing_date_fy, filed_count, low_income_count, lower_middle_income_count, upper_middle_income_count, high_income_count, na_income_count) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Total\nfiled" = filed_count,
               "Low\nincome" = low_income_count, 
               "Lower middle\nincome" = lower_middle_income_count,
               "Upper middle\nincome" = upper_middle_income_count,
               "High\nincome" = high_income_count,
               "No data on\ncountry/FY income" = na_income_count)

# inspect
income_filing_share_by_fy_cohort_table_formatted


# ////////////////////////


# title: I-589 filings across country income groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: The percentages in parenthesis are the respective counts as a share of total filings.",
                                "Source: World Bank; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = income_filing_share_by_fy_cohort_table_formatted, 
                       output_sheet_names = "income_filings_by_fy_cohort", 
           text_cols = NULL, custom_col_width = list(c(rep(NA, times = 3), 14, 14)), custom_row_height = NULL, 
           style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
           col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create homicide_filing_share_by_fy_cohort_table ####
homicide_filing_share_by_fy_cohort_table <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_homicide_count = sum(`homicide_bucket.= 1`),
                  lower_middle_homicide_count = sum(`homicide_bucket.1.1 to 10`),
                  middle_homicide_count = sum(`homicide_bucket.10.1 to 20`),
                  upper_middle_homicide_count = sum(`homicide_bucket.20.1 to 40`),
                  high_homicide_count = sum(`homicide_bucket.> 40`),
                  na_homicide_count = sum(homicide_bucket.NA)) %>%
        ungroup() %>%
        mutate(low_homicide_pct = low_homicide_count / filed_count,
               lower_middle_homicide_pct = lower_middle_homicide_count / filed_count,
               middle_homicide_pct = middle_homicide_count / filed_count,
               upper_middle_homicide_pct = upper_middle_homicide_count / filed_count,
               high_homicide_pct = high_homicide_count / filed_count,
               na_homicide_pct = na_homicide_count / filed_count) %>% 
        select(filing_date_fy, filed_count, low_homicide_count, low_homicide_pct, 
               lower_middle_homicide_count, lower_middle_homicide_pct, 
               middle_homicide_count, middle_homicide_pct, 
               upper_middle_homicide_count, upper_middle_homicide_pct, 
               high_homicide_count, high_homicide_pct, na_homicide_count, na_homicide_pct)


# //////////////////


# inspect
homicide_filing_share_by_fy_cohort_table

# test that filed_count sums to total record count
expect_equal(object = homicide_filing_share_by_fy_cohort_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# test that homicide bucket counts sum to filed count for each fy
expect_equal(object = homicide_filing_share_by_fy_cohort_table %>% 
                     mutate(homicide_bucket_sum = low_homicide_count + lower_middle_homicide_count + middle_homicide_count + upper_middle_homicide_count + 
                                    high_homicide_count + na_homicide_count) %>% pull(homicide_bucket_sum),
             expected = homicide_filing_share_by_fy_cohort_table %>% pull(filed_count))

# test that homicide bucket pcts sum to 1 for each fy
expect_equal(object = homicide_filing_share_by_fy_cohort_table %>% 
                     mutate(homicide_bucket_pct_sum = low_homicide_pct + lower_middle_homicide_pct + middle_homicide_pct + upper_middle_homicide_pct + 
                                    high_homicide_pct + na_homicide_pct) %>% pull(homicide_bucket_pct_sum),
             expected = rep(1, times = nrow(homicide_filing_share_by_fy_cohort_table)))


# //////////////////////////


# get homicide_filing_share_by_fy_cohort_table_formatted
homicide_filing_share_by_fy_cohort_table_formatted <- homicide_filing_share_by_fy_cohort_table %>% 
        mutate(filed_count = comma(filed_count),
               filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_homicide_pct = as_percent(low_homicide_pct, digits = 0),
               lower_middle_homicide_pct = as_percent(lower_middle_homicide_pct, digits = 0),
               middle_homicide_pct = as_percent(middle_homicide_pct, digits = 0),
               upper_middle_homicide_pct = as_percent(upper_middle_homicide_pct, digits = 0),
               high_homicide_pct = as_percent(high_homicide_pct, digits = 0),
               na_homicide_pct = as_percent(na_homicide_pct, digits = 0),
               low_homicide_count = str_c(comma(low_homicide_count), ifelse(nchar(low_homicide_pct) == 3, " (", "   ("), low_homicide_pct, ")"),
               lower_middle_homicide_count = str_c(comma(lower_middle_homicide_count), ifelse(nchar(lower_middle_homicide_pct) == 3, " (", "   ("), lower_middle_homicide_pct, ")"),
               middle_homicide_count = str_c(comma(middle_homicide_count), ifelse(nchar(middle_homicide_pct) == 3, " (", "   ("), middle_homicide_pct, ")"),
               upper_middle_homicide_count = str_c(comma(upper_middle_homicide_count), ifelse(nchar(upper_middle_homicide_pct) == 3, " (", "   ("), upper_middle_homicide_pct, ")"),
               high_homicide_count = str_c(comma(high_homicide_count), ifelse(nchar(high_homicide_pct) == 3, " (", "   ("), high_homicide_pct, ")"),
               na_homicide_count = str_c(comma(na_homicide_count), ifelse(nchar(na_homicide_pct) == 3, " (", "   ("), na_homicide_pct, ")")) %>% 
        select(filing_date_fy, filed_count, low_homicide_count, lower_middle_homicide_count, 
               middle_homicide_count, upper_middle_homicide_count, high_homicide_count, na_homicide_count) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Total\nfiled" = filed_count,
               "Less than 1\nhomicide\nper 100k\npopulation" = low_homicide_count, 
               "1.1 to 10\nhomicides\nper 100k\npopulation" = lower_middle_homicide_count,
               "10.1 to 20\nhomicides\nper 100k\npopulation" = middle_homicide_count,
               "20.1 to 40\nhomicides\nper 100k\npopulation" = upper_middle_homicide_count,
               "> 40\nhomicides\nper 100k\npopulation" = high_homicide_count,
               "No data on\ncountry/FY\nhomicides" = na_homicide_count)

# inspect
homicide_filing_share_by_fy_cohort_table_formatted


# ////////////////////////


# title: I-589 filings across country homicide groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Note: The percentages in parenthesis are the respective counts as a share of total filings.",
                                  "Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = homicide_filing_share_by_fy_cohort_table_formatted, 
                       output_sheet_names = "homicide_filings_by_fy_cohort", 
                       text_cols = NULL, custom_col_width = NULL, custom_row_height = list(c(75)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create prcl_filing_share_by_fy_cohort_table ####
prcl_filing_share_by_fy_cohort_table <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_prcl_count = sum(`prcl_bucket.< 25`),
                  lower_middle_prcl_count = sum(`prcl_bucket.25 to 49`),
                  upper_middle_prcl_count = sum(`prcl_bucket.50 to 74`),
                  high_prcl_count = sum(`prcl_bucket.75 to 100`),
                  na_prcl_count = sum(prcl_bucket.NA)) %>%
        ungroup() %>%
        mutate(low_prcl_pct = low_prcl_count / filed_count,
               lower_middle_prcl_pct = lower_middle_prcl_count / filed_count,
               upper_middle_prcl_pct = upper_middle_prcl_count / filed_count,
               high_prcl_pct = high_prcl_count / filed_count,
               na_prcl_pct = na_prcl_count / filed_count) %>% 
        select(filing_date_fy, filed_count, low_prcl_count, low_prcl_pct, 
               lower_middle_prcl_count, lower_middle_prcl_pct, 
               upper_middle_prcl_count, upper_middle_prcl_pct, 
               high_prcl_count, high_prcl_pct, na_prcl_count, na_prcl_pct)


# //////////////////


# inspect
prcl_filing_share_by_fy_cohort_table

# test that filed_count sums to total record count
expect_equal(object = prcl_filing_share_by_fy_cohort_table %>% summarize(filed_count_sum = sum(filed_count)) %>% pull(filed_count_sum),
             expected = data %>% nrow())

# test that prcl bucket counts sum to filed count for each fy
expect_equal(object = prcl_filing_share_by_fy_cohort_table %>% 
                     mutate(prcl_bucket_sum = low_prcl_count + lower_middle_prcl_count + upper_middle_prcl_count + 
                                    high_prcl_count + na_prcl_count) %>% pull(prcl_bucket_sum),
             expected = prcl_filing_share_by_fy_cohort_table %>% pull(filed_count))

# test that prcl bucket pcts sum to 1 for each fy
expect_equal(object = prcl_filing_share_by_fy_cohort_table %>% 
                     mutate(prcl_bucket_pct_sum = low_prcl_pct + lower_middle_prcl_pct + upper_middle_prcl_pct + 
                                    high_prcl_pct + na_prcl_pct) %>% pull(prcl_bucket_pct_sum),
             expected = rep(1, times = nrow(prcl_filing_share_by_fy_cohort_table)))


# //////////////////////////


# get prcl_filing_share_by_fy_cohort_table_formatted
prcl_filing_share_by_fy_cohort_table_formatted <- prcl_filing_share_by_fy_cohort_table %>% 
        mutate(filed_count = comma(filed_count),
               filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_prcl_pct = as_percent(low_prcl_pct, digits = 0),
               lower_middle_prcl_pct = as_percent(lower_middle_prcl_pct, digits = 0),
               upper_middle_prcl_pct = as_percent(upper_middle_prcl_pct, digits = 0),
               high_prcl_pct = as_percent(high_prcl_pct, digits = 0),
               na_prcl_pct = as_percent(na_prcl_pct, digits = 0),
               low_prcl_count = str_c(comma(low_prcl_count), ifelse(nchar(low_prcl_pct) == 3, " (", "   ("), low_prcl_pct, ")"),
               lower_middle_prcl_count = str_c(comma(lower_middle_prcl_count), ifelse(nchar(lower_middle_prcl_pct) == 3, " (", "   ("), lower_middle_prcl_pct, ")"),
               upper_middle_prcl_count = str_c(comma(upper_middle_prcl_count), ifelse(nchar(upper_middle_prcl_pct) == 3, " (", "   ("), upper_middle_prcl_pct, ")"),
               high_prcl_count = str_c(comma(high_prcl_count), ifelse(nchar(high_prcl_pct) == 3, " (", "   ("), high_prcl_pct, ")"),
               na_prcl_count = str_c(comma(na_prcl_count), ifelse(nchar(na_prcl_pct) == 3, " (", "   ("), na_prcl_pct, ")")) %>% 
        select(filing_date_fy, filed_count, low_prcl_count, lower_middle_prcl_count, 
               upper_middle_prcl_count, high_prcl_count, na_prcl_count) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Total\nfiled" = filed_count,
               "Total PRCL\nscore:\n< 25" = low_prcl_count, 
               "Total PRCL\nscore:\n25 to 49" = lower_middle_prcl_count,
               "Total PRCL\nscore:\n50 to 74" = upper_middle_prcl_count,
               "Total PRCL\nscore:\n75 to 100" = high_prcl_count,
               "No data on\ncountry/FY\ntotal PRCL score" = na_prcl_count)

# inspect
prcl_filing_share_by_fy_cohort_table_formatted


# ////////////////////////


# title: I-589 filings across country political rights / civil liberties groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                  "The percentages in parenthesis are the respective counts as a share of total filings.", sep = ""),
                                  "Source: Freedom House; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = prcl_filing_share_by_fy_cohort_table_formatted, 
                       output_sheet_names = "prcl_filings_by_fy_cohort", 
                       text_cols = NULL, custom_col_width = list(c(NA, NA, rep(14, times = 4))), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create income_overall_relief_granted_by_fy_cohort_table ####
income_overall_relief_granted_by_fy_cohort_table <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(low_income_adj_count = sum(`income_group_bucket.Low income`[terminal_adjudicated_flag == 1]),
                  low_income_overall_relief_granted_count = sum(`income_group_bucket.Low income`[overall_relief_granted_flag == 1]),
                  lower_middle_income_adj_count = sum(`income_group_bucket.Lower middle income`[terminal_adjudicated_flag == 1]),
                  lower_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Lower middle income`[overall_relief_granted_flag == 1]),
                  upper_middle_income_adj_count = sum(`income_group_bucket.Upper middle income`[terminal_adjudicated_flag == 1]),
                  upper_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Upper middle income`[overall_relief_granted_flag == 1]),
                  high_income_adj_count = sum(`income_group_bucket.High income`[terminal_adjudicated_flag == 1]),
                  high_income_overall_relief_granted_count = sum(`income_group_bucket.High income`[overall_relief_granted_flag == 1]),
                  na_income_adj_count = sum(`income_group_bucket.NA`[terminal_adjudicated_flag == 1]),
                  na_income_overall_relief_granted_count = sum(`income_group_bucket.NA`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(low_income_overall_relief_granted_pct = low_income_overall_relief_granted_count / low_income_adj_count,
               lower_middle_income_overall_relief_granted_pct = lower_middle_income_overall_relief_granted_count / 
                       lower_middle_income_adj_count,
               upper_middle_income_overall_relief_granted_pct = upper_middle_income_overall_relief_granted_count / 
                       upper_middle_income_adj_count,
               high_income_overall_relief_granted_pct = high_income_overall_relief_granted_count / 
                       high_income_adj_count,
               na_income_overall_relief_granted_pct = na_income_overall_relief_granted_count / 
                       na_income_adj_count) %>% 
        select(filing_date_fy, 
               low_income_overall_relief_granted_count, low_income_overall_relief_granted_pct, 
               lower_middle_income_overall_relief_granted_count, lower_middle_income_overall_relief_granted_pct, 
               upper_middle_income_overall_relief_granted_count, upper_middle_income_overall_relief_granted_pct,
               high_income_overall_relief_granted_count, high_income_overall_relief_granted_pct,
               na_income_overall_relief_granted_count, na_income_overall_relief_granted_pct)


# //////////////////


# inspect
income_overall_relief_granted_by_fy_cohort_table

# test that aggregate sum of income groups equals total count of records with overall_relief_granted
expect_equal(object = income_overall_relief_granted_by_fy_cohort_table %>% 
                     summarize(low_income_sum = sum(low_income_overall_relief_granted_count),
                               lower_middle_income_sum = sum(lower_middle_income_overall_relief_granted_count),
                               upper_middle_income_sum = sum(upper_middle_income_overall_relief_granted_count),
                               high_income_sum = sum(high_income_overall_relief_granted_count),
                               na_income_sum = sum(na_income_overall_relief_granted_count)) %>% 
                     mutate(total_sum = rowSums(.)) %>% pull(total_sum),
             expected = data %>% filter(overall_relief_granted_flag == 1) %>% nrow())


# //////////////////////////


# get income_overall_relief_granted_by_fy_cohort_table_formatted
income_overall_relief_granted_by_fy_cohort_table_formatted <- income_overall_relief_granted_by_fy_cohort_table %>% 
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_income_overall_relief_granted_pct = as_percent(low_income_overall_relief_granted_pct, digits = 0),
               lower_middle_income_overall_relief_granted_pct = as_percent(lower_middle_income_overall_relief_granted_pct, digits = 0),
               upper_middle_income_overall_relief_granted_pct = as_percent(upper_middle_income_overall_relief_granted_pct, digits = 0),
               high_income_overall_relief_granted_pct = as_percent(high_income_overall_relief_granted_pct, digits = 0),
               na_income_overall_relief_granted_pct = as_percent(na_income_overall_relief_granted_pct, digits = 0)) %>% 
        select(filing_date_fy, low_income_overall_relief_granted_pct, lower_middle_income_overall_relief_granted_pct, 
               upper_middle_income_overall_relief_granted_pct, high_income_overall_relief_granted_pct, 
               na_income_overall_relief_granted_pct) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Low\nincome" = low_income_overall_relief_granted_pct, 
               "Lower middle\nincome" = lower_middle_income_overall_relief_granted_pct,
               "Upper middle\nincome" = upper_middle_income_overall_relief_granted_pct,
               "High\nincome" = high_income_overall_relief_granted_pct,
               "No data on country/FY\nincome" = na_income_overall_relief_granted_pct)

# inspect
income_overall_relief_granted_by_fy_cohort_table_formatted


# ////////////////////////


# title: Overall relief granted rate across I-589 applicant country income groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ", 
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, ", 
                                        "or termination outcome from EOIR. ",
                                        sep = ""),
                                  "Source: World Bank; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = income_overall_relief_granted_by_fy_cohort_table_formatted, 
                       output_sheet_names = "income_overall_relief", 
                       text_cols = NULL, custom_col_width = list(c(NA, rep(12, times = 5))), 
                       custom_row_height = list(c(rep(NA, times = nrow(income_overall_relief_granted_by_fy_cohort_table_formatted) + 1), 75)),
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create homicide_overall_relief_granted_by_fy_cohort_table ####
homicide_overall_relief_granted_by_fy_cohort_table <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(low_homicide_adj_count = sum(`homicide_bucket.= 1`[terminal_adjudicated_flag == 1]),
                  low_homicide_overall_relief_granted_count = sum(`homicide_bucket.= 1`[overall_relief_granted_flag == 1]),
                  lower_middle_homicide_adj_count = sum(`homicide_bucket.1.1 to 10`[terminal_adjudicated_flag == 1]),
                  lower_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.1.1 to 10`[overall_relief_granted_flag == 1]),
                  middle_homicide_adj_count = sum(`homicide_bucket.10.1 to 20`[terminal_adjudicated_flag == 1]),
                  middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.10.1 to 20`[overall_relief_granted_flag == 1]),
                  upper_middle_homicide_adj_count = sum(`homicide_bucket.20.1 to 40`[terminal_adjudicated_flag == 1]),
                  upper_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.20.1 to 40`[overall_relief_granted_flag == 1]),
                  high_homicide_adj_count = sum(`homicide_bucket.> 40`[terminal_adjudicated_flag == 1]),
                  high_homicide_overall_relief_granted_count = sum(`homicide_bucket.> 40`[overall_relief_granted_flag == 1]),
                  na_homicide_adj_count = sum(`homicide_bucket.NA`[terminal_adjudicated_flag == 1]),
                  na_homicide_overall_relief_granted_count = sum(`homicide_bucket.NA`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(low_homicide_overall_relief_granted_pct = low_homicide_overall_relief_granted_count / low_homicide_adj_count,
               lower_middle_homicide_overall_relief_granted_pct = lower_middle_homicide_overall_relief_granted_count / 
                       lower_middle_homicide_adj_count,
               middle_homicide_overall_relief_granted_pct = middle_homicide_overall_relief_granted_count / 
                       middle_homicide_adj_count,
               upper_middle_homicide_overall_relief_granted_pct = upper_middle_homicide_overall_relief_granted_count / 
                       upper_middle_homicide_adj_count,
               high_homicide_overall_relief_granted_pct = high_homicide_overall_relief_granted_count / 
                       high_homicide_adj_count,
               na_homicide_overall_relief_granted_pct = na_homicide_overall_relief_granted_count / 
                       na_homicide_adj_count) %>% 
        select(filing_date_fy, low_homicide_overall_relief_granted_count, low_homicide_overall_relief_granted_pct, 
               lower_middle_homicide_overall_relief_granted_count, lower_middle_homicide_overall_relief_granted_pct, 
               middle_homicide_overall_relief_granted_count, middle_homicide_overall_relief_granted_pct, 
               upper_middle_homicide_overall_relief_granted_count, upper_middle_homicide_overall_relief_granted_pct, 
               high_homicide_overall_relief_granted_count, high_homicide_overall_relief_granted_pct,
               na_homicide_overall_relief_granted_count, na_homicide_overall_relief_granted_pct)


# //////////////////


# inspect
homicide_overall_relief_granted_by_fy_cohort_table

# test that cumulative sum of homicide groups equals total count of records with overall_relief_granted
expect_equal(object = homicide_overall_relief_granted_by_fy_cohort_table %>% 
                     summarize(low_homicide_sum = sum(low_homicide_overall_relief_granted_count),
                               lower_middle_homicide_sum = sum(lower_middle_homicide_overall_relief_granted_count),
                               middle_homicide_sum = sum(middle_homicide_overall_relief_granted_count),
                               upper_middle_homicide_sum = sum(upper_middle_homicide_overall_relief_granted_count),
                               high_homicide_sum = sum(high_homicide_overall_relief_granted_count),
                               na_homicide_sum = sum(na_homicide_overall_relief_granted_count)) %>% 
                     mutate(total_sum = rowSums(.)) %>% pull(total_sum),
             expected = data %>% filter(overall_relief_granted_flag == 1) %>% nrow())


# //////////////////////////


# get homicide_overall_relief_granted_by_fy_cohort_table_formatted
homicide_overall_relief_granted_by_fy_cohort_table_formatted <- homicide_overall_relief_granted_by_fy_cohort_table %>% 
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_homicide_overall_relief_granted_pct = as_percent(low_homicide_overall_relief_granted_pct, digits = 0),
               lower_middle_homicide_overall_relief_granted_pct = as_percent(lower_middle_homicide_overall_relief_granted_pct, digits = 0),
               middle_homicide_overall_relief_granted_pct = as_percent(middle_homicide_overall_relief_granted_pct, digits = 0),
               upper_middle_homicide_overall_relief_granted_pct = as_percent(upper_middle_homicide_overall_relief_granted_pct, digits = 0),
               high_homicide_overall_relief_granted_pct = as_percent(high_homicide_overall_relief_granted_pct, digits = 0),
               na_homicide_overall_relief_granted_pct = as_percent(na_homicide_overall_relief_granted_pct, digits = 0)) %>% 
        select(filing_date_fy, low_homicide_overall_relief_granted_pct, lower_middle_homicide_overall_relief_granted_pct, 
               middle_homicide_overall_relief_granted_pct,
               upper_middle_homicide_overall_relief_granted_pct, high_homicide_overall_relief_granted_pct,
               na_homicide_overall_relief_granted_pct) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Less than 1\nhomicide\nper 100k\npopulation" = low_homicide_overall_relief_granted_pct, 
               "1.1 to 10\nhomicides\nper 100k\npopulation" = lower_middle_homicide_overall_relief_granted_pct,
               "10.1 to 20\nhomicides\nper 100k\npopulation" = middle_homicide_overall_relief_granted_pct,
               "20.1 to 40\nhomicides\nper 100k\npopulation" = upper_middle_homicide_overall_relief_granted_pct,
               "> 40\nhomicides\nper 100k\npopulation" = high_homicide_overall_relief_granted_pct,
               "No data on\ncountry/FY homicides" = na_homicide_overall_relief_granted_pct)

# inspect
homicide_overall_relief_granted_by_fy_cohort_table_formatted


# ////////////////////////

# title: Overall relief granted rate across I-589 applicant country homicide groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ", 
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, ", 
                                        "or termination outcome from EOIR. ",
                                        sep = ""),
                                  "Source: United Nations; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = homicide_overall_relief_granted_by_fy_cohort_table_formatted, 
                       output_sheet_names = "homicide_overall_relief", 
                       text_cols = NULL, custom_col_width = list(c(NA, rep(12, times = 5))), 
                       custom_row_height = list(c(75, rep(NA, times = nrow(homicide_overall_relief_granted_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create prcl_overall_relief_granted_by_fy_cohort_table ####
prcl_overall_relief_granted_by_fy_cohort_table <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(low_prcl_adj_count = sum(`prcl_bucket.< 25`[terminal_adjudicated_flag == 1]),
                  low_prcl_overall_relief_granted_count = sum(`prcl_bucket.< 25`[overall_relief_granted_flag == 1]),
                  lower_middle_prcl_adj_count = sum(`prcl_bucket.25 to 49`[terminal_adjudicated_flag == 1]),
                  lower_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.25 to 49`[overall_relief_granted_flag == 1]),
                  upper_middle_prcl_adj_count = sum(`prcl_bucket.50 to 74`[terminal_adjudicated_flag == 1]),
                  upper_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.50 to 74`[overall_relief_granted_flag == 1]),
                  high_prcl_adj_count = sum(`prcl_bucket.75 to 100`[terminal_adjudicated_flag == 1]),
                  high_prcl_overall_relief_granted_count = sum(`prcl_bucket.75 to 100`[overall_relief_granted_flag == 1]),
                  na_prcl_adj_count = sum(`prcl_bucket.NA`[terminal_adjudicated_flag == 1]),
                  na_prcl_overall_relief_granted_count = sum(`prcl_bucket.NA`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(low_prcl_overall_relief_granted_pct = low_prcl_overall_relief_granted_count / low_prcl_adj_count,
               lower_middle_prcl_overall_relief_granted_pct = lower_middle_prcl_overall_relief_granted_count / 
                       lower_middle_prcl_adj_count,
               upper_middle_prcl_overall_relief_granted_pct = upper_middle_prcl_overall_relief_granted_count / 
                       upper_middle_prcl_adj_count,
               high_prcl_overall_relief_granted_pct = high_prcl_overall_relief_granted_count / 
                       high_prcl_adj_count,
               na_prcl_overall_relief_granted_pct = na_prcl_overall_relief_granted_count / 
                       na_prcl_adj_count)%>% 
        select(filing_date_fy, low_prcl_overall_relief_granted_count, low_prcl_overall_relief_granted_pct, 
               lower_middle_prcl_overall_relief_granted_count, lower_middle_prcl_overall_relief_granted_pct, 
               upper_middle_prcl_overall_relief_granted_count, upper_middle_prcl_overall_relief_granted_pct, 
               high_prcl_overall_relief_granted_count, high_prcl_overall_relief_granted_pct,
               na_prcl_overall_relief_granted_count, na_prcl_overall_relief_granted_pct)


# //////////////////


# inspect
prcl_overall_relief_granted_by_fy_cohort_table

# test that cumulative sum of prcl groups equals total count of records with overall_relief_granted
expect_equal(object = prcl_overall_relief_granted_by_fy_cohort_table %>% 
                     summarize(low_prcl_sum = sum(low_prcl_overall_relief_granted_count),
                               lower_middle_prcl_sum = sum(lower_middle_prcl_overall_relief_granted_count),
                               upper_middle_prcl_sum = sum(upper_middle_prcl_overall_relief_granted_count),
                               high_prcl_sum = sum(high_prcl_overall_relief_granted_count),
                               na_prcl_sum = sum(na_prcl_overall_relief_granted_count)) %>% 
                     mutate(total_sum = rowSums(.)) %>% pull(total_sum),
             expected = data %>% filter(overall_relief_granted_flag == 1) %>% nrow())


# //////////////////////////


# get prcl_overall_relief_granted_by_fy_cohort_table_formatted
prcl_overall_relief_granted_by_fy_cohort_table_formatted <- prcl_overall_relief_granted_by_fy_cohort_table %>% 
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort"),
               low_prcl_overall_relief_granted_pct = as_percent(low_prcl_overall_relief_granted_pct, digits = 0),
               lower_middle_prcl_overall_relief_granted_pct = as_percent(lower_middle_prcl_overall_relief_granted_pct, digits = 0),
               upper_middle_prcl_overall_relief_granted_pct = as_percent(upper_middle_prcl_overall_relief_granted_pct, digits = 0),
               high_prcl_overall_relief_granted_pct = as_percent(high_prcl_overall_relief_granted_pct, digits = 0),
               na_prcl_overall_relief_granted_pct = as_percent(na_prcl_overall_relief_granted_pct, digits = 0)) %>% 
        select(filing_date_fy, low_prcl_overall_relief_granted_pct, lower_middle_prcl_overall_relief_granted_pct, 
               upper_middle_prcl_overall_relief_granted_pct, high_prcl_overall_relief_granted_pct,
               na_prcl_overall_relief_granted_pct) %>%
        rename("I-589 FY\nfiling cohort" = filing_date_fy,
               "Total PRCL score:\n< 25" = low_prcl_overall_relief_granted_pct, 
               "Total PRCL score:\n25 to 49" = lower_middle_prcl_overall_relief_granted_pct,
               "Total PRCL score:\n50 to 74" = upper_middle_prcl_overall_relief_granted_pct,
               "Total PRCL score:\n75 to 100" = high_prcl_overall_relief_granted_pct,
               "No data on\ncountry/FY\ntotal PRCL score" = na_prcl_overall_relief_granted_pct)

# inspect
prcl_overall_relief_granted_by_fy_cohort_table_formatted


# ////////////////////////


# title: Overall relief granted rate across I-589 applicant country political rights / civil liberties (PRCL) groupings, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                        "Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ", 
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, ", 
                                        "or termination outcome from EOIR. ",
                                        sep = ""),
                                  "Source: Freedom House; DOJ EOIR; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = prcl_overall_relief_granted_by_fy_cohort_table_formatted, 
                       output_sheet_names = "prcl_overall_relief", 
                       text_cols = NULL, custom_col_width = list(c(NA, rep(12, times = 5))), 
                       custom_row_height = list(c(75, rep(NA, times = nrow(prcl_overall_relief_granted_by_fy_cohort_table_formatted)), 80)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()
        


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_income_group_by_cy ####

country_income_group_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(income_group_bucket) %>%
        ungroup() %>%
        mutate(income_group_bucket = case_when(is.na(income_group_bucket) ~ "-",
                                               income_group_bucket == "Low income" ~ "L",
                                               income_group_bucket == "Lower middle income" ~ "LM",
                                               income_group_bucket == "Upper middle income" ~ "UM",
                                               income_group_bucket == "High income" ~ "H",
                                               TRUE ~ NA_character_)) %>%
        select(filing_date_cy, citizenship_output_country_name, income_group_bucket) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = income_group_bucket) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_income_group_by_cy


# ////////////////////////


# title: I-589 applicant country income groupings, by CY

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The applicant country income grouping labels are abbreviated as follows: ",
                                  "low income (L), lower middle income (LM), upper middle income (UM), and high income (H)."),
                                "Source: World Bank; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_income_group_by_cy, 
                       output_sheet_names = "country_income_group_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 7, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_income_by_cy ####

country_income_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(gni_per_capita) %>%
        ungroup() %>%
        mutate(gni_per_capita = dollar(gni_per_capita),
               gni_per_capita = case_when(gni_per_capita == "$NA" ~ "-", TRUE ~ gni_per_capita)) %>%
        select(filing_date_cy, citizenship_output_country_name, gni_per_capita) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = gni_per_capita) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_income_by_cy


# ////////////////////////


# title: I-589 applicant country GNI per capita levels, by CY

# create footnote_table
footnote_table <- tibble(text = c("Note: Country/calendar year combinations for which data is unavailable are marked with an '-'.",
                                  "Source: World Bank; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_income_by_cy, 
                       output_sheet_names = "country_income_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 7, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_homicide_group_by_cy ####

country_homicide_group_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(homicide_bucket) %>%
        ungroup() %>%
        mutate(homicide_bucket = case_when(is.na(homicide_bucket) ~ "-",
                                           homicide_bucket == "\u2264 1" ~ "< 1",
                                           homicide_bucket == "1.1 to 10" ~ "1.1-10",
                                           homicide_bucket == "10.1 to 20" ~ "10.1-20",
                                           homicide_bucket == "20.1 to 40" ~ "20.1-40",
                                           TRUE ~ homicide_bucket)) %>%
        select(filing_date_cy, citizenship_output_country_name, homicide_bucket) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = homicide_bucket) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_homicide_group_by_cy


# ////////////////////////


# title: I-589 applicant country homicide groupings, by CY

# create footnote_table
footnote_table <- tibble(text = c("Note: Country/calendar year combinations for which data is unavailable are marked with an '-'.",
                                "Source: United Nations; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_homicide_group_by_cy, 
                       output_sheet_names = "country_homicide_group_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 9, 9, 7, 8, 9, 8, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_homicide_by_cy ####

country_homicide_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(homicide_rate_per_100k) %>%
        ungroup() %>%
        mutate(homicide_rate_per_100k = comma(homicide_rate_per_100k, accuracy = .1), 
               homicide_rate_per_100k = case_when(homicide_rate_per_100k == "NA" ~ "-", TRUE ~ homicide_rate_per_100k)) %>%
        select(filing_date_cy, citizenship_output_country_name, homicide_rate_per_100k) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = homicide_rate_per_100k) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_homicide_by_cy


# ////////////////////////


# title: I-589 applicant country homicide rate, by CY

# create footnote_table
footnote_table <- tibble(text = c("Note: Country/calendar year combinations for which data is unavailable are marked with an '-'.",
                                "Source: United Nations; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_homicide_by_cy, 
                       output_sheet_names = "country_homicide_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 7, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_prcl_group_by_cy ####

country_prcl_group_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(prcl_bucket) %>%
        ungroup() %>%
        mutate(prcl_bucket = case_when(is.na(prcl_bucket) ~ "-",
                                       prcl_bucket == "25 to 49" ~ "25-49",
                                       prcl_bucket == "50 to 74" ~ "50-74",
                                       prcl_bucket == "75 to 100" ~ "75-100",
                                       TRUE ~ prcl_bucket)) %>%
        select(filing_date_cy, citizenship_output_country_name, prcl_bucket) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = prcl_bucket) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_prcl_group_by_cy


# ////////////////////////


# title: I-589 applicant country political rights / civil liberties groupings, by CY

# create footnote_table
footnote_table <- tibble(text = c("Source: Freedom House; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_prcl_group_by_cy, 
                       output_sheet_names = "country_prcl_group_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 9, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_prcl_by_cy ####

country_prcl_by_cy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_cy, citizenship_output_country_name) %>%
        distinct(total_prcl_score) %>%
        ungroup() %>%
        mutate(total_prcl_score = case_when(is.na(total_prcl_score) ~ "-", TRUE ~ as.character(total_prcl_score))) %>%
        select(filing_date_cy, citizenship_output_country_name, total_prcl_score) %>% 
        pivot_wider(names_from = citizenship_output_country_name, values_from = total_prcl_score) %>%
        arrange(filing_date_cy) %>%
        rename("Filing date\nCY" = filing_date_cy)

# inspect
country_prcl_by_cy


# ////////////////////////


# title: I-589 applicant country political rights / civil liberties total scores, by CY

# create footnote_table
footnote_table <- tibble(text = c("Source: Freedom House; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_prcl_by_cy, 
                       output_sheet_names = "country_prcl_by_cy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 7, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_per_capita_filing_by_cohort_fy ####

# get country_populations
country_populations <- read_csv("data/population/countries_and_country_income_groups/API_SP.POP.TOTL_DS2_en_csv_v2_1068829.csv", skip = 4) %>%
        rename(country_name = `Country Name`) %>%
        mutate(country_name = case_when(country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Venezuela, RB" ~ "Venezuela", 
                                        TRUE ~ country_name)) %>%
        select(-c(`Country Code`, `Indicator Name`, `Indicator Code`, X65)) %>%
        pivot_longer(cols = -country_name, names_to = "mid_year", values_to = "country_population") %>%
        mutate(mid_year = as.numeric(mid_year))
        


# ///////////////////////////////


# inspect
country_populations
country_populations %>% glimpse()

# check that un country names are compatible with uscis country names
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% 
        anti_join(., country_populations %>% distinct(country_name), by = c("citizenship_output_country_name" = "country_name"))

country_populations %>% filter(str_detect(string = country_name, pattern = regex("Venez|Egypt"))) %>% pull(country_name)
        

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_per_capita_filing_by_cohort_fy
country_per_capita_filing_by_cohort_fy <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>% count() %>%
        ungroup() %>%
        left_join(., country_populations, by = c("citizenship_output_country_name" = "country_name", "filing_date_fy" = "mid_year")) %>%
        rename(filed_count = n) %>%
        mutate(filings_per_100k_pop = (filed_count / country_population) * 100000) %>%
        select(filing_date_fy, citizenship_output_country_name, filings_per_100k_pop) %>%
        left_join(data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>% select(citizenship_output_country_name),
                  ., by = "citizenship_output_country_name") %>%
        pivot_wider(id_cols = filing_date_fy, names_from = citizenship_output_country_name, values_from = filings_per_100k_pop) %>%
        map_at(.at = vars(-filing_date_fy), .f = ~ round_to_digits(x = .x, digits = 2)) %>%
        bind_cols() %>%
        rename("I-589 FY filing cohort" = filing_date_fy)
        

country_per_capita_filing_by_cohort_fy
        
        
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# title: I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = country_per_capita_filing_by_cohort_fy, 
                       output_sheet_names = "country_per_capita_filing_by_fy", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 7, 8, 7, 9, 7, 8, 7, 7, 7, 7)), custom_row_height = NULL, 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metrics_by_fy_cohort_table ####

# note that the code assembling the ead_metrics dataset is copied from the state_metrics table above

# read in ead princ/dep data
ead <- read_csv("data/I-765/i765_pa_raw.csv")
ead_dep <- read_csv("data/I-765/i765_da_raw.csv")

# combine princ/dep to get ead_metrics
ead_metrics <-  ead_dep %>% select(dep_anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>% 
        rename(anumber = dep_anumber) %>% mutate(origin = "dep") %>% 
        filter(anumber %in% i589_dep$dep_anumber) %>%
        bind_rows(., ead %>% select(anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>%
                          mutate(origin = "princ") %>% filter(anumber %in% data$anumber)) %>%
        filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"))


#///////////////////////////////////////


# inspect
ead_dep %>% select(dep_anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>% 
        rename(anumber = dep_anumber) %>% mutate(origin = "dep") %>% 
        filter(anumber %in% i589_dep$dep_anumber) %>%
        bind_rows(., ead %>% select(anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>%
                          mutate(origin = "princ") %>% filter(anumber %in% data$anumber)) %>%
        filter(part_2_1 %in% c("A", "C")) %>% count(CURRENT_STATUS) # 1457838 approved, 95667 denied, 95921 NA?
ead_metrics %>% nrow() # 1457838
ead_metrics %>% distinct(anumber, receipt_number) %>% nrow() # 1457838
ead_metrics %>% distinct(anumber) %>% nrow() # 735182
ead_metrics %>% count(origin) # 424963 dep / 1032875 princ
ead_metrics %>% distinct(anumber, origin) %>% count(origin) # 233681 dep; 501501 princ
ead_metrics %>% distinct(anumber, receipt_number) %>% nrow() # 1457838
ead_metrics %>% distinct(receipt_number) %>% nrow() # 1457807 note that there are some duplicate receipt numbers 
# note there are 62 ead receipt numbers with two different anumbers associated to them
# the anumbers seem to be sequential, so either they are derivatives with sequential anumbers? odd, but will just remove the duplicates below
ead_metrics %>% group_by(receipt_number) %>% mutate(anumber_n_distinct = n_distinct(anumber)) %>% 
        ungroup() %>%
        filter(anumber_n_distinct > 1) %>% 
        select(anumber, receipt_number, anumber_n_distinct, valid_to, valid_from, CURRENT_STATUS, part_2_1, origin) %>%
        arrange(receipt_number)
ead_metrics %>% add_count(receipt_number, name = "receipt_number_count") %>% filter(receipt_number_count > 1) %>% count(origin) # 31 dep/princ, will drop dep
ead_metrics %>% add_count(receipt_number, name = "receipt_number_count") %>%
        filter(!(receipt_number_count > 1 & origin == "dep")) %>% 
        summarize(record_count = n(), receipt_number_n_distinct = n_distinct(receipt_number)) # 1457807
# note that about 5600 anumbers have a renewal but no initial in ead_metrics - less than 1% of 735k anumbers - can ignore
ead_metrics %>% distinct(anumber) %>% nrow() # 735182
ead_metrics %>% filter(part_2_1 == "A") %>% distinct(anumber) %>% nrow()
ead_metrics %>% count(part_2_1)


ead %>% nrow() # 1222322
ead %>% filter(CURRENT_STATUS == "Approved") %>% nrow() # 1045581
ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C")) %>% nrow() # 1032965
ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% nrow() # 1032875
ead %>% filter(CURRENT_STATUS == "Approved") %>% count(part_2_1) # 504019 initial, 528946 extension (this is what scott cacluated in his initial table)
ead %>% filter(CURRENT_STATUS == "Approved", anumber %in% data$anumber) %>% count(part_2_1) # 503965 initial, 528910 extension
ead %>% filter(CURRENT_STATUS == "Denied") %>% count(part_2_1) # 59110 initial, 15159 extensions (this is what scott cacluated in his initial table)
ead %>% filter(CURRENT_STATUS == "Denied", anumber %in% data$anumber) %>% count(part_2_1) # 59093 initial, 15153 extensions 
ead %>% distinct(anumber) %>% nrow() # 522724
ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C")) %>% distinct(anumber) %>% nrow() # 501571
ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% distinct(anumber) %>% nrow() # 501501
ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% nrow() # 1032875
ead %>% filter(is.na(ead_len)) %>% nrow() # 185129

ead_dep %>% nrow() # 449891
ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), dep_anumber %in% i589_dep$dep_anumber) %>% nrow() # 424963
ead_dep %>% filter(CURRENT_STATUS == "Approved", dep_anumber %in% i589_dep$dep_anumber) %>% count(part_2_1) # 234906 initial; 190057 extension
ead_dep %>% distinct(dep_anumber) %>% nrow() # 239212
ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), dep_anumber %in% i589_dep$dep_anumber) %>% distinct(dep_anumber) %>% nrow() # 233681
ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), dep_anumber %in% i589_dep$dep_anumber) %>% nrow() # 424963

data %>% distinct(anumber) %>% nrow() # 672403
i589_dep %>% distinct(dep_anumber) %>% nrow() # 287759

# there are 130 anumbers in ead but not in i589 data, and 7 anumbers in ead_dep but not in i589_dep
# but these are removed in ead_metrics, so no issue
ead %>% filter(!(anumber %in% data$anumber), !(anumber %in% i589_dep$anumber)) %>% nrow() # 130
ead_dep %>% filter(!(dep_anumber %in% data$anumber), !(dep_anumber %in% i589_dep$dep_anumber)) %>% nrow() # 7
ead_metrics %>% filter(!(anumber %in% data$anumber), !(anumber %in% i589_dep$dep_anumber)) %>% nrow() # 0

# there are also 10945 ead records with NA valid_from date; concerning, but can ignore since they have valid anumber/receipt_number/approval
ead_metrics %>% filter(is.na(valid_from)) %>% nrow()
ead_metrics %>% distinct(anumber, origin) %>% count(anumber) %>% filter(n > 1) # 0

i589_dep %>% nrow() # 287759
i589_dep %>% distinct(dep_anumber) %>% nrow() # 287759

data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403

233687 / 287759 # 81% of dep get approved EAD
501571 / 672403 # 75% of principals get approved EAD

# note only 7% of submitted c8 I-765 are denied
# scott said the overall i765 filing counts were weird though, because of the 101414 with NA for current status
# and so his tables omitted filing counts, and just focused on approved, so a denial rate is not something he included
ead %>% count(CURRENT_STATUS)
75276 / (1045581 + 75276 + 51) 


#///////////////////////////////////////


# drop 31 dep records that are duplicated receipt_numbers with princ records (see inspection above)
# also get ead_len_clean 
# note the vast majority of ead_len is either 365 or 730 as expected - will round the 4054 records with odd ead_len to 365 or 730, whichever is closest
ead_metrics <- ead_metrics %>% mutate(ead_len_clean = case_when(ead_len < 365 ~ 365, 
                                                                    ead_len >= 365 & ead_len <= 547.5 ~ 365, 
                                                                    ead_len > 547.5 ~ 730, 
                                                                    TRUE ~ NA_real_)) %>%
                        add_count(receipt_number, name = "receipt_number_count") %>%
                        filter(!(receipt_number_count > 1 & origin == "dep")) 


#///////////////////////////////////////


# inspect
ead_metrics %>% nrow() # 1457807
ead_metrics %>% distinct(anumber, receipt_number) %>% nrow() # 1457807
ead_metrics %>% distinct(anumber, origin) %>% count(origin) # 501501 princ anumbers, 233676 dep anumbers
ead_metrics %>% count(origin) # 1032875 princ receipts, 424932 dep receipts

# inspect ead_len
ead_metrics %>% count(ead_len) %>% ggplot(data = ., aes(x = ead_len)) + geom_histogram()
ead_metrics %>% count(ead_len_clean)
# note the vast majority of "cleaned" ead_len were 729 and 364, and most of the rest were a few days off
ead_metrics %>% filter(!is.na(ead_len), ead_len != ead_len_clean, !(ead_len %in% c(729, 364))) %>% nrow() # 4054
ead_metrics %>% filter(!is.na(ead_len), ead_len != ead_len_clean, 
                        !(ead_len %in% c(729, 364, 366))) %>% select(valid_from, valid_to, ead_len, ead_len_clean)
ead_metrics %>% filter(!is.na(ead_len), ead_len != ead_len_clean) %>% count(ead_len) %>% arrange(desc(n))


#///////////////////////////////////////


# get i589_princ_dep_extract to join to ead_metrics
i589_princ_dep_extract <- i589_dep %>% select(anumber, dep_anumber, dep_date_of_birth, dep_citizenship_output_country_name) %>%
        mutate(origin = "dep") %>%
        left_join(., data %>% select(anumber, filing_date_fy, ewi_flag, outcome_bucket, eoir_outcome, 
                                     eoir_cancellation_applied, eoir_received_flag, income_group_bucket, homicide_bucket, prcl_bucket), 
                  by = "anumber") %>% 
        select(-c(anumber)) %>% rename(anumber = dep_anumber, date_of_birth = dep_date_of_birth, 
                                       citizenship_output_country_name = dep_citizenship_output_country_name) %>%
        bind_rows(data %>% select(anumber, date_of_birth, citizenship_output_country_name, filing_date_fy, ewi_flag,
                                  outcome_bucket, eoir_outcome, eoir_cancellation_applied, eoir_received_flag,
                                  income_group_bucket, homicide_bucket, prcl_bucket) %>% mutate(origin = "princ"))
        

#///////////////////////////////////////


# inspect
i589_princ_dep_extract 
i589_princ_dep_extract %>% glimpse()
i589_princ_dep_extract %>% nrow() # 960162
i589_princ_dep_extract %>% distinct(anumber) %>% nrow() # 960158
i589_princ_dep_extract %>% count(origin)
data %>% nrow() # 672403
i589_dep %>% nrow() # 287759
672403 + 287759 == 960162

# note there are four anumbers in both princ/dep list
# these will be dropped with distinct()
data %>% filter(anumber %in% i589_dep$dep_anumber) %>% nrow() # 4
i589_princ_dep_extract %>% distinct(anumber) %>% nrow() # 960158
i589_princ_dep_extract %>% add_count(anumber) %>% filter(n > 1)
i589_princ_dep_extract %>% add_count(anumber) %>% filter(!(n > 1 & origin == "dep")) %>% nrow() # 960158


#///////////////////////////////////////


# remove the 4 princ/dep with same anumbers - keeps the princ record, drops the dep record
i589_princ_dep_extract <- i589_princ_dep_extract %>% add_count(anumber) %>% filter(!(n > 1 & origin == "dep")) %>% select(-n)


#///////////////////////////////////////


# inspect
i589_princ_dep_extract %>% distinct(anumber) %>% nrow() # 960158
i589_princ_dep_extract %>% nrow() # 960158


#///////////////////////////////////////


# join i589_princ_dep_extract to ead_metrics
ead_metrics <- ead_metrics %>% mutate(i589_princ_flag = case_when(origin == "princ" ~ 1, TRUE ~ 0),
                       i589_dep_flag = case_when(origin == "dep" ~ 1, TRUE ~ 0)) %>%
        select(-origin) %>%
        left_join(., i589_princ_dep_extract, by = "anumber")



#///////////////////////////////////////


# inspect
ead_metrics %>% glimpse()
ead_metrics %>% nrow() # 1457807
ead_metrics %>% distinct(receipt_number) %>% nrow() # 1457807
ead_metrics %>% distinct(anumber, receipt_number) %>% nrow() # 1457807
ead_metrics %>% distinct(anumber) %>% nrow() # 735177


ead_metrics %>% filter(is.na(date_of_birth)) %>% select(anumber, date_of_birth, dep_date_of_birth, origin) # only 96 princ/dep without DOB, can ignore
ead_metrics %>% filter(is.na(date_of_birth)) %>% distinct(dep_date_of_birth)


#///////////////////////////////////////


# add eligibility_duration, receipt_number_count, and under_14 flag to ead_metrics
# note that DOL Fair Labor Standards Act sets 14 yo as the general minimum age to work 
# https://www.dol.gov/general/topic/youthlabor/agerequirements#:~:text=As%20a%20general%20rule%2C%20the,under%20the%20age%20of%2016.
ead_metrics <- ead_metrics %>% group_by(anumber) %>% 
        mutate(eligibility_duration = sum(ead_len_clean), 
               receipt_number_count = n_distinct(receipt_number)) %>% 
        ungroup() %>%
        mutate(age_at_eligibility_end_date = as.numeric(valid_to - date_of_birth) / 365,
               under_14_at_eligibility_end_date_flag = case_when(age_at_eligibility_end_date < 14 ~ 1, TRUE ~ 0)) %>%
        group_by(anumber) %>%
        mutate(ever_under_14_at_eligibility_end_date_flag = case_when(sum(under_14_at_eligibility_end_date_flag) > 0 ~ 1, TRUE ~ 0)) %>%
        ungroup() %>% arrange(anumber)


#//////////////////////////////////////


# inspect
ead_metrics
ead_metrics %>% glimpse()
ead_metrics %>% nrow() # 1457807
ead_metrics %>% distinct(anumber, receipt_number) %>% nrow() # 1457807
ead_metrics %>% distinct(anumber) %>% nrow() # 735177
ead_metrics %>% filter(receipt_number_count == 1) %>% count(eligibility_duration)
ead_metrics %>% distinct(anumber, receipt_number_count) %>% count(receipt_number_count) %>% arrange(desc(n))
ead_metrics %>% distinct(anumber, receipt_number_count) %>% nrow() # 735177
ead_metrics %>% ggplot(data = ., aes(x = age_at_eligibility_end_date)) + geom_histogram()
ead_metrics %>% filter(under_14_at_eligibility_end_date_flag == 1) %>% 
        select(anumber, valid_to, date_of_birth, age_at_eligibility_end_date, under_14_at_eligibility_end_date_flag)
ead_metrics %>% filter(under_14_at_eligibility_end_date_flag == 1) %>% 
        ggplot(data = ., aes(x = age_at_eligibility_end_date)) + geom_histogram()
ead_metrics %>% count(age_at_eligibility_end_date) %>% arrange(age_at_eligibility_end_date)
ead_metrics %>% filter(age_at_eligibility_end_date < 14) %>% sample_n(20) %>% select(anumber, valid_to, date_of_birth, age_at_eligibility_end_date)
ead_metrics %>% 
        distinct(anumber, i589_princ_flag, i589_dep_flag, filing_date_fy, ewi_flag, outcome_bucket, eoir_cancellation_applied, eoir_received_flag,
                 income_group_bucket, homicide_bucket, prcl_bucket, receipt_number_count, eligibility_duration, 
                 ever_under_14_at_eligibility_end_date_flag) %>% 
        nrow() # 735177


#//////////////////////////////////////


# save ead_metrics
# ead_metrics %>% write_csv(path = "data/I-765/ead_metrics_20200903.csv")

# load ead_metrics
# ead_metrics <- read_csv(file = "data/I-765/ead_metrics_20200903.csv")


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# get i589_princ_and_dep_count_by_filing_cohort
i589_princ_and_dep_count_by_filing_cohort <- data %>% group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                   dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_princ_and_dep_count_by_filing_cohort

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_princ_and_dep_count_by_filing_cohort %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_princ_and_dep_count_by_filing_cohort %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_fy_cohort_table
ead_metrics_by_fy_cohort_table <- ead_metrics %>% 
        distinct(anumber, i589_princ_flag, i589_dep_flag, filing_date_fy, receipt_number_count, eligibility_duration) %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count_by_filing_cohort, ., by = "filing_date_fy") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count)


#///////////////////////////////////////


# inspect
ead_metrics_by_fy_cohort_table
ead_metrics_by_fy_cohort_table %>% data.frame()

# note that 5604 anumbers (0.8%) only have renewals, but no initial ead
ead_metrics %>% filter(part_2_1 == "C") %>% distinct(anumber) %>%
        anti_join(., ead_metrics %>% filter(part_2_1 == "A") %>% distinct(anumber), by = "anumber") %>%
        left_join(., ead_metrics, by = "anumber") %>% count(part_2_1) # 9218 receipt numbers

ead_metrics %>% filter(part_2_1 == "C") %>% distinct(anumber) %>%
        anti_join(., ead_metrics %>% filter(part_2_1 == "A") %>% distinct(anumber), by = "anumber") %>% nrow() # 5604 anumbers

ead_metrics %>% distinct(anumber) %>% nrow() # 735177
5604 / 735177 # 0.8% of anumbers

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_fy_cohort_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% 
                     distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_fy_cohort_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                                           dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% 
                     distinct(dep_anumber) %>% nrow())


# note that ead approval rates appear to have increased from 2009 to 2014 91%, then stay > 90% through 2020
# not clear if the low approval rate in beginning is due to some kind of data quality issue or a real trend?
ead %>% filter(!is.na(CURRENT_STATUS)) %>% add_dummies(CURRENT_STATUS) %>% group_by(REC_FY) %>% 
        summarize(filed = n(), approved = sum(CURRENT_STATUS.Approved)) %>% mutate(approval_rate = approved / filed)

# the average approval_rate across REC_FY cohorts is 87%
ead %>% filter(!is.na(CURRENT_STATUS)) %>% add_dummies(CURRENT_STATUS) %>% group_by(REC_FY) %>% 
        summarize(filed = n(), approved = sum(CURRENT_STATUS.Approved)) %>% mutate(approval_rate = approved / filed) %>% 
        summarize(approval_rate_mean = mean(approval_rate))

# there really has been an increase though in the share of principals filing for an ead, 
# likely related to increasing backlog delays, though possibly also nonmeritorious filers exploiting backlog to seek EAD
ead %>% filter(!is.na(CURRENT_STATUS)) %>% distinct(anumber) %>% mutate(ead_filed_flag = 1) %>% 
        left_join(data, ., by = "anumber") %>% mutate(ead_filed_flag = case_when(is.na(ead_filed_flag) ~ 0, TRUE ~ ead_filed_flag)) %>% 
        group_by(filing_date_fy) %>% summarize(ead_filed_rate = mean(ead_filed_flag))


#///////////////////////////////////////


# get ead_metrics_by_fy_cohort_table_formatted
ead_metrics_by_fy_cohort_table_formatted <- ead_metrics_by_fy_cohort_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
                completed_case_pct, eligibility_duration_years_median) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_by_fy_cohort_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants and dependents, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_by_fy_cohort_table_formatted, 
                       output_sheet_names = "ead_metrics_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metrics_ewi_by_cohort_table ####

# get i589_ewi_princ_and_dep_count_by_filing_cohort
i589_ewi_princ_and_dep_count_by_filing_cohort <- data %>% filter(ewi_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_ewi_princ_and_dep_count_by_filing_cohort

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_ewi_princ_and_dep_count_by_filing_cohort %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% filter(ewi_flag == 1) %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_ewi_princ_and_dep_count_by_filing_cohort %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% filter(anumber %in% (data %>% filter(ewi_flag == 1) %>% pull(anumber))) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_ewi_by_fy_cohort_table
ead_metrics_ewi_by_fy_cohort_table <- ead_metrics %>% filter(ewi_flag == 1) %>%
        distinct(anumber, i589_princ_flag, i589_dep_flag, filing_date_fy, receipt_number_count, eligibility_duration) %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_ewi_princ_and_dep_count_by_filing_cohort, ., by = "filing_date_fy") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count)


#///////////////////////////////////////


# inspect
ead_metrics_ewi_by_fy_cohort_table
ead_metrics_ewi_by_fy_cohort_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_ewi_by_fy_cohort_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber,
                                       anumber %in% (data %>% filter(ewi_flag == 1) %>% pull(anumber))) %>% 
                     distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_ewi_by_fy_cohort_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                                           dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number),
                                           dep_anumber %in% (i589_dep %>% filter(status_at_entry == "EWI") %>% pull(dep_anumber))) %>% 
                     distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_ewi_by_fy_cohort_table_formatted
ead_metrics_ewi_by_fy_cohort_table_formatted <- ead_metrics_ewi_by_fy_cohort_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of\nI-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_ewi_by_fy_cohort_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants who entered without inspection, and their dependents, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_ewi_by_fy_cohort_table_formatted, 
                       output_sheet_names = "ead_metrics_ewi_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_ewi_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metrics_cor_by_cohort_table ####

# get i589_cor_princ_and_dep_count_by_filing_cohort
i589_cor_princ_and_dep_count_by_filing_cohort <- data %>% filter(eoir_cancellation_applied == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_cor_princ_and_dep_count_by_filing_cohort

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_cor_princ_and_dep_count_by_filing_cohort %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% filter(eoir_cancellation_applied == 1) %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_cor_princ_and_dep_count_by_filing_cohort %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% filter(anumber %in% (data %>% filter(eoir_cancellation_applied == 1) %>% pull(anumber))) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_cor_by_fy_cohort_table
ead_metrics_cor_by_fy_cohort_table <- ead_metrics %>% filter(eoir_cancellation_applied == 1) %>%
        distinct(anumber, i589_princ_flag, i589_dep_flag, filing_date_fy, receipt_number_count, eligibility_duration) %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_cor_princ_and_dep_count_by_filing_cohort, ., by = "filing_date_fy") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count)


#///////////////////////////////////////


# inspect
ead_metrics_cor_by_fy_cohort_table
ead_metrics_cor_by_fy_cohort_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_cor_by_fy_cohort_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber,
                                       anumber %in% (data %>% filter(eoir_cancellation_applied == 1) %>% pull(anumber))) %>% 
                     distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_cor_by_fy_cohort_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                                           dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number),
                                           dep_anumber %in% (data %>% filter(eoir_cancellation_applied == 1) %>% select(anumber) %>%
                                                                     left_join(., i589_dep, by = "anumber") %>% pull(dep_anumber))) %>% 
                     distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_cor_by_fy_cohort_table_formatted
ead_metrics_cor_by_fy_cohort_table_formatted <- ead_metrics_cor_by_fy_cohort_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_cor_by_fy_cohort_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants who applied for cancellation of removal, and their dependents, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_cor_by_fy_cohort_table_formatted, 
                       output_sheet_names = "ead_metrics_cor_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_cor_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metricis_1yr_ref_by_cohort_table ####

# get i589_1yr_ref_princ_and_dep_count_by_filing_cohort
i589_1yr_ref_princ_and_dep_count_by_filing_cohort <- data %>% 
        mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0)) %>%
        filter(referral_w_1yr_limit_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_1yr_ref_princ_and_dep_count_by_filing_cohort

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_1yr_ref_princ_and_dep_count_by_filing_cohort %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% 
                     mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0)) %>% 
                     filter(referral_w_1yr_limit_flag == 1) %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_1yr_ref_princ_and_dep_count_by_filing_cohort %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% 
                     filter(anumber %in% (data %>% 
                        mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0)) %>% 
                                                                  filter(referral_w_1yr_limit_flag == 1) %>% pull(anumber))) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_1yr_ref_by_fy_cohort_table
ead_metrics_1yr_ref_by_fy_cohort_table <- ead_metrics %>% 
        mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0)) %>%
        filter(referral_w_1yr_limit_flag == 1) %>%
        distinct(anumber, i589_princ_flag, i589_dep_flag, filing_date_fy, receipt_number_count, eligibility_duration) %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_1yr_ref_princ_and_dep_count_by_filing_cohort, ., by = "filing_date_fy") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count)


#///////////////////////////////////////


# inspect
ead_metrics_1yr_ref_by_fy_cohort_table
ead_metrics_1yr_ref_by_fy_cohort_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_1yr_ref_by_fy_cohort_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber,
                anumber %in% 
                (data %>% mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0)) %>% 
                                                        filter(referral_w_1yr_limit_flag == 1) %>% pull(anumber))) %>% 
                     distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_1yr_ref_by_fy_cohort_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                           dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number),
                           dep_anumber %in% 
                           (data %>% mutate(referral_w_1yr_limit_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, 
                                                                                  TRUE ~ 0)) %>% 
                                    filter(referral_w_1yr_limit_flag == 1) %>% select(anumber) %>%
                                             left_join(., i589_dep, by = "anumber") %>% pull(dep_anumber))) %>% 
                     distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_1yr_ref_by_fy_cohort_table_formatted
ead_metrics_1yr_ref_by_fy_cohort_table_formatted <- ead_metrics_1yr_ref_by_fy_cohort_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               filing_date_fy = str_c(filing_date_fy, " cohort")) %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_1yr_ref_by_fy_cohort_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants who were referred by USCIS based on the one year filing deadline, and their dependents, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_1yr_ref_by_fy_cohort_table_formatted, 
                       output_sheet_names = "ead_metrics_1yr_ref_by_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_1yr_ref_by_fy_cohort_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metrics_by_country ####

# get i589_princ_and_dep_count_by_country
i589_princ_and_dep_count_by_country <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(citizenship_output_country_name, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_princ_and_dep_count_by_country

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_princ_and_dep_count_by_country %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                     distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_princ_and_dep_count_by_country %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% 
                     filter(anumber %in% (data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                            distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>% pull(anumber))) %>% 
                     nrow())


#///////////////////////////////////////


# get ead_metrics_by_country_table
top_countries <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% pull(citizenship_output_country_name)

ead_metrics_by_country_table <- ead_metrics %>% 
        filter(citizenship_output_country_name %in% top_countries) %>%
        distinct(anumber, i589_princ_flag, i589_dep_flag, citizenship_output_country_name, receipt_number_count, eligibility_duration) %>% 
        group_by(citizenship_output_country_name) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count_by_country, ., by = "citizenship_output_country_name") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count) %>%
        arrange(desc(princ_i589_count))
        



#///////////////////////////////////////


# inspect
ead_metrics_by_country_table
ead_metrics_by_country_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_country_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber,
                            anumber %in% 
                                    (data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
                                             distinct(citizenship_output_country_name) %>% 
                                             left_join(., data, by = "citizenship_output_country_name") %>% 
                                             pull(anumber))) %>% distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
# note this test fails due to a slight (< 1.0%) mismatch (1371 / 150450) - will ignore for now
dep_anumbers_from_top_countries <- i589_dep %>% filter(citizenship_output_country_name %in% top_countries) %>% pull(dep_anumber)
expect_equal(object = ead_metrics_by_country_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                            dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number),
                            dep_anumber %in% dep_anumbers_from_top_countries) %>% distinct(dep_anumber) %>% nrow())

# check honduras as an example
honduras_anumbers <- data %>% filter(citizenship_output_country_name == "Honduras") %>% pull(anumber)
honduras_anumbers %>% length() # 27269
ead %>% filter(anumber %in% honduras_anumbers, CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C")) %>% distinct(anumber) %>% nrow() # 17191
17191 / 27269 

honduras_dep_anumbers <- i589_dep %>% filter(citizenship_output_country_name == "Honduras") %>% pull(dep_anumber)
honduras_dep_anumbers %>% length() # 5536
ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                   dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number), 
                   dep_anumber %in% honduras_dep_anumbers) %>% distinct(dep_anumber) %>% nrow() # 3906
honduras_dep_anumbers_v2 <- i589_dep %>% filter(anumber %in% honduras_anumbers) %>% pull(dep_anumber)
honduras_dep_anumbers_v2 %>% length() # 5536
ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                   dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number), 
                   dep_anumber %in% honduras_dep_anumbers) %>% distinct(dep_anumber) %>% nrow()


#///////////////////////////////////////


# get ead_metrics_by_country_table_formatted
ead_metrics_by_country_table_formatted <- ead_metrics_by_country_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count)) %>%
        select(citizenship_output_country_name, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("Applicant country" = citizenship_output_country_name,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_by_country_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants and their dependents, by top I-589 filing countries

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_by_country_table_formatted, 
                       output_sheet_names = "ead_metrics_by_country", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(NA, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_by_country_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ead_metrics_by_income_table ####

# get i589_princ_and_dep_count_by_income
i589_princ_and_dep_count_by_income <- data %>% 
        group_by(income_group_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(income_group_bucket, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_princ_and_dep_count_by_income

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_princ_and_dep_count_by_income %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_princ_and_dep_count_by_income %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_income_table
ead_metrics_by_income_table <- ead_metrics %>% 
        distinct(anumber, i589_princ_flag, i589_dep_flag, income_group_bucket, receipt_number_count, eligibility_duration) %>% 
        group_by(income_group_bucket) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count_by_income, ., by = "income_group_bucket") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count) %>%
        arrange(desc(princ_i589_count))


#///////////////////////////////////////


# inspect
ead_metrics_by_income_table
ead_metrics_by_income_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_income_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_income_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                            dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_income_table_formatted
ead_metrics_by_income_table_formatted <- ead_metrics_by_income_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               income_group_bucket = case_when(is.na(income_group_bucket) ~ "Unknown", 
                                               income_group_bucket == "Low income" ~ "Low",
                                               income_group_bucket == "Lower middle income" ~ "Lower middle",
                                               income_group_bucket == "Upper middle income" ~ "Upper middle",
                                               income_group_bucket == "High income" ~ "High",
                                               TRUE ~ income_group_bucket),
               income_group_index = case_when(income_group_bucket == "Low" ~ 1,
                                              income_group_bucket == "Lower middle" ~ 2,
                                              income_group_bucket == "Upper middle" ~ 3,
                                              income_group_bucket == "High" ~ 4,
                                              income_group_bucket == "Unknown" ~ 5)) %>%
        arrange(income_group_index) %>%
        select(income_group_bucket, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("Income group" = income_group_bucket,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_by_income_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants and their dependents, by applicant country income groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: World Bank; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_by_income_table_formatted, 
                       output_sheet_names = "ead_metrics_by_income", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_by_income_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ead_metrics_by_homicide_table ####

# get i589_princ_and_dep_count_by_homicide
i589_princ_and_dep_count_by_homicide <- data %>% 
        group_by(homicide_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(homicide_bucket, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_princ_and_dep_count_by_homicide

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_princ_and_dep_count_by_homicide %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_princ_and_dep_count_by_homicide %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_homicide_table
ead_metrics_by_homicide_table <- ead_metrics %>% 
        distinct(anumber, i589_princ_flag, i589_dep_flag, homicide_bucket, receipt_number_count, eligibility_duration) %>% 
        group_by(homicide_bucket) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count_by_homicide, ., by = "homicide_bucket") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count) %>%
        arrange(desc(princ_i589_count))


#///////////////////////////////////////


# inspect
ead_metrics_by_homicide_table
ead_metrics_by_homicide_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_homicide_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_homicide_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                            dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_homicide_table_formatted
ead_metrics_by_homicide_table_formatted <- ead_metrics_by_homicide_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               homicide_bucket = case_when(is.na(homicide_bucket) ~ "Unknown", TRUE ~ homicide_bucket),
               homicide_index = case_when(homicide_bucket == "\u2264 1" ~ 1,
                                          homicide_bucket == "1.1 to 10" ~ 2,
                                          homicide_bucket == "10.1 to 20" ~ 3,
                                          homicide_bucket == "20.1 to 40" ~ 4,
                                          homicide_bucket == "> 40" ~ 5,
                                          homicide_bucket == "Unknown" ~ 6)) %>%
        arrange(homicide_index) %>%
        select(homicide_bucket, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("Homicide rate per 100k population" = homicide_bucket,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_by_homicide_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants and their dependents, by applicant country homicide groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: United Nations; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_by_homicide_table_formatted, 
                       output_sheet_names = "ead_metrics_by_homicide", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_by_homicide_table_formatted)), 65)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ead_metrics_by_prcl_table ####

# get i589_princ_and_dep_count_by_prcl
i589_princ_and_dep_count_by_prcl <- data %>% 
        group_by(prcl_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  filed_count = n(), completed_case_count = sum(terminal_adjudicated_flag, na.rm = TRUE)) %>% 
        ungroup() %>% mutate(completed_case_pct = as_percent(completed_case_count / filed_count, digits = 0)) %>%
        select(prcl_bucket, princ_i589_count, dep_i589_count, completed_case_pct) 


#///////////////////////////////////////


# inspect
i589_princ_and_dep_count_by_prcl

# test that sum of princ_i589_count equals the records in i589 data
expect_equal(object = i589_princ_and_dep_count_by_prcl %>% summarize(princ_sum = sum(princ_i589_count)) %>% pull(princ_sum),
             expected = data %>% nrow())

# test that sum of dep_i589_count equals the records in i589_dep
expect_equal(object = i589_princ_and_dep_count_by_prcl %>% summarize(dep_sum = sum(dep_i589_count)) %>% pull(dep_sum),
             expected = i589_dep %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_prcl_table
ead_metrics_by_prcl_table <- ead_metrics %>% 
        distinct(anumber, i589_princ_flag, i589_dep_flag, prcl_bucket, receipt_number_count, eligibility_duration) %>% 
        group_by(prcl_bucket) %>% 
        summarize(princ_ead_headcount = sum(i589_princ_flag, na.rm = TRUE), 
                  dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  eligibility_duration_years_median = median(eligibility_duration, na.rm = TRUE) / 365) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count_by_prcl, ., by = "prcl_bucket") %>%
        mutate(princ_ead_pct = princ_ead_headcount / princ_i589_count,
               dep_ead_pct = dep_ead_headcount / dep_i589_count) %>%
        arrange(desc(princ_i589_count))


#///////////////////////////////////////


# inspect
ead_metrics_by_prcl_table
ead_metrics_by_prcl_table %>% data.frame()

# test that princ_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_prcl_table %>% summarize(princ_ead_headcount_sum = sum(princ_ead_headcount)) %>% pull(princ_ead_headcount_sum),
             expected = ead %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% distinct(anumber) %>% nrow())

# test that dep_ead_count equals approved initial/renewal eads from raw ead file
expect_equal(object = ead_metrics_by_prcl_table %>% summarize(dep_ead_headcount_sum = sum(dep_ead_headcount)) %>% pull(dep_ead_headcount_sum),
             expected = ead_dep %>% 
                     filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                            dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% distinct(dep_anumber) %>% nrow())


#///////////////////////////////////////


# get ead_metrics_by_prcl_table_formatted
ead_metrics_by_prcl_table_formatted <- ead_metrics_by_prcl_table %>% 
        mutate(princ_ead_pct = as_percent(princ_ead_pct, digits = 0),
               dep_ead_pct = as_percent(dep_ead_pct, digits = 0),
               princ_ead_headcount = str_c(comma(princ_ead_headcount), ifelse(nchar(princ_ead_pct) == 3, " (", "   ("), princ_ead_pct, ")"),
               dep_ead_headcount = str_c(comma(dep_ead_headcount), ifelse(nchar(dep_ead_pct) == 3, " (", "   ("), dep_ead_pct, ")"),
               princ_i589_count = comma(princ_i589_count),
               dep_i589_count = comma(dep_i589_count),
               prcl_bucket = case_when(is.na(prcl_bucket) ~ "Unknown", TRUE ~ prcl_bucket),
               prcl_index = case_when(prcl_bucket == "< 25" ~ 1,
                                      prcl_bucket == "25 to 49" ~ 2,
                                      prcl_bucket == "50 to 74" ~ 3,
                                      prcl_bucket == "75 to 100" ~ 4,
                                      prcl_bucket == "Unknown" ~ 5)) %>%
        arrange(prcl_index) %>%
        select(prcl_bucket, princ_i589_count, dep_i589_count, princ_ead_headcount, dep_ead_headcount, 
               completed_case_pct, eligibility_duration_years_median) %>%
        rename("Political rights / civil liberties score" = prcl_bucket,
               "I-589 principals" = princ_i589_count,
               "I-589 dependents" = dep_i589_count,
               "I-589 principals\nwho received\nat least one\napproved EAD" = princ_ead_headcount,
               "I-589 dependents\nwho received\nat least one\napproved EAD" = dep_ead_headcount,
               "Share of I-589 filings completed" = completed_case_pct,
               "Among those with an approved EAD,\nmedian number of years authorized to work" = eligibility_duration_years_median)

# inspect
ead_metrics_by_prcl_table_formatted


#///////////////////////////////////////


# title: EAD approvals among I-589 principal applicants and their dependents, by applicant country political rights / civil liberties groupings

# create footnote_table
footnote_table <- tibble(text = c(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                        "The 'I-589 principals who received at least one approved EAD' percentage in parenthesis ",
                                        "is the respective count as a share of I-589 principals. The ",
                                        "'I-589 dependents who received at least one approved EAD' percentages in parenthesis ",
                                        "is the count as a share of I-589 dependents. ",
                                        "'Share of I-589 filings completed' is the share of I-589 filings with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, ",
                                        "voluntary departure, or termination outcome from EOIR. ",
                                        "The 'Median number of years with an approved EAD' ",
                                        "summarizes both I-589 principals and dependents.", sep = ""),
                                  "Source: Freedom House; USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = ead_metrics_by_prcl_table_formatted, 
                       output_sheet_names = "ead_metrics_by_prcl", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(13, 9, 9, 14, 14, 10, 18)), 
                       custom_row_height = list(c(85, rep(NA, times = nrow(ead_metrics_by_prcl_table_formatted)), 80)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create i765_outcomes_by_fy_cohort_table ####

# get i589_princ_dep_extract (note this was also created in create_ead_metrics_by_fy_cohort_table above, but recreating here for clarity)
i589_princ_dep_extract <- i589_dep %>% select(anumber, dep_anumber, dep_date_of_birth, dep_citizenship_output_country_name) %>%
        mutate(origin = "dep") %>%
        left_join(., data %>% select(anumber, filing_date_fy, ewi_flag, outcome_bucket, eoir_outcome, 
                                     eoir_cancellation_applied, eoir_received_flag, income_group_bucket, homicide_bucket, prcl_bucket), 
                  by = "anumber") %>% 
        select(-c(anumber)) %>% rename(anumber = dep_anumber, date_of_birth = dep_date_of_birth, 
                                       citizenship_output_country_name = dep_citizenship_output_country_name) %>%
        bind_rows(data %>% select(anumber, date_of_birth, citizenship_output_country_name, filing_date_fy, ewi_flag,
                                  outcome_bucket, eoir_outcome, eoir_cancellation_applied, eoir_received_flag,
                                  income_group_bucket, homicide_bucket, prcl_bucket) %>% mutate(origin = "princ")) %>% 
        add_count(anumber) %>% filter(!(n > 1 & origin == "dep")) %>% select(-n)

# inspect
i589_princ_dep_extract 
i589_princ_dep_extract %>% distinct(anumber) %>% nrow() # 960158
i589_princ_dep_extract %>% nrow() # 960158


#/////////////////////////////////////////////


# get ead_denials
# note that ead_metrics only included approvals, so denials will be calculated from ead and ead_dep source datasets, 
# note ead_denials is combined in the same as ead_metrics was above, just with the steps condensed
# that that ~60 receipt_numbers have "Revoked" status, these will be dropped and only Denied receipts counted
ead_denials <- ead_dep %>% select(dep_anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>% 
        rename(anumber = dep_anumber) %>% mutate(origin = "dep") %>% 
        filter(anumber %in% i589_dep$dep_anumber) %>%
        bind_rows(., ead %>% select(anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>%
                          mutate(origin = "princ") %>% filter(anumber %in% data$anumber)) %>%
        filter(CURRENT_STATUS == "Denied", part_2_1 %in% c("A", "C")) %>%
        mutate(i589_princ_flag = case_when(origin == "princ" ~ 1, TRUE ~ 0),
                       i589_dep_flag = case_when(origin == "dep" ~ 1, TRUE ~ 0)) %>%
        select(-origin) %>%
        left_join(., i589_princ_dep_extract, by = "anumber")


#/////////////////////////////////////////


# inspect
ead_denials
ead_denials %>% glimpse()
ead_denials %>% nrow() # 95667
ead_denials %>% distinct(anumber, receipt_number) %>% nrow() # 95667
ead_denials %>% distinct(origin, receipt_number) %>% nrow() # 95667
ead_denials %>% distinct(anumber) %>% nrow() # 80009
ead_denials %>% distinct(origin, anumber) %>% nrow() # 80009
ead_denials %>% distinct(origin, anumber) %>% count(origin) # 61763 princ, 18246 dep


#/////////////////////////////////////////


# get ever_under_14_dep
ever_under_14_dep <- ead_metrics %>% distinct(filing_date_fy, anumber, ever_under_14_at_eligibility_end_date_flag, i589_dep_flag) %>%
        group_by(filing_date_fy) %>% 
        summarize(dep_ead_headcount = sum(i589_dep_flag, na.rm = TRUE),
                  ever_under_14_dep_count = sum(ever_under_14_at_eligibility_end_date_flag[i589_dep_flag == 1], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(ever_under_14_dep_pct = ever_under_14_dep_count / dep_ead_headcount, digits = 0)


#/////////////////////////////////////////


# inspect
ever_under_14_dep

# test that ever_under_14_dep_count total is calculated correctly
expect_equal(object = ever_under_14_dep %>% summarize(ever_under_14_dep_count_sum = sum(ever_under_14_dep_count)) %>% 
                     pull(ever_under_14_dep_count_sum),
             expected = ead_metrics %>% filter(i589_dep_flag == 1, ever_under_14_at_eligibility_end_date_flag == 1) %>% distinct(anumber) %>% nrow())


#/////////////////////////////////////////


# get i765_outcomes_by_fy_cohort_table
i765_outcomes_by_fy_cohort_table <- ead_metrics %>% mutate(princ_initial = case_when(i589_princ_flag == 1 & part_2_1 == "A" ~ 1, TRUE ~ 0),
                       princ_renewal = case_when(i589_princ_flag == 1 & part_2_1 == "C" ~ 1, TRUE ~ 0),
                       dep_initial = case_when(i589_dep_flag == 1 & part_2_1 == "A" ~ 1, TRUE ~ 0),
                       dep_renewal = case_when(i589_dep_flag == 1 & part_2_1 == "C" ~ 1, TRUE ~ 0)) %>%
        group_by(filing_date_fy) %>%
        summarize(princ_initial_sum = sum(princ_initial),
                  princ_renewal_sum = sum(princ_renewal),
                  dep_initial_sum = sum(dep_initial),
                  dep_renewal_sum = sum(dep_renewal)) %>%
        ungroup() %>%
        left_join(., ead_denials %>% count(filing_date_fy) %>% rename(denial_count = n), by = "filing_date_fy") %>%
        left_join(., ever_under_14_dep %>% select(filing_date_fy, ever_under_14_dep_count, ever_under_14_dep_pct), by = "filing_date_fy")


#/////////////////////////////////////////


# inspect
i765_outcomes_by_fy_cohort_table
i765_outcomes_by_fy_cohort_table %>% glimpse()
# note that the avg_princ/dep_renewals doesn't seem too interesting, also 
i765_outcomes_by_fy_cohort_table %>%
        mutate(avg_princ_renewals = princ_renewal_sum / princ_initial_sum,
               avg_dep_renewals = dep_renewal_sum / dep_initial_sum)

# note that 8912 anumbers (1.2%) have multiple approved initial filings
# there are 18208 of these receipt numbers constituting multiple approved initial filings, which is 1.2% of receipts
ead_metrics %>% filter(part_2_1 == "A") %>% count(anumber) %>% filter(n > 1) %>% nrow() # 8912
ead_metrics %>% distinct(anumber) %>% nrow() # 735177
8912 / 735177
ead_metrics %>% filter(part_2_1 == "A") %>% add_count(anumber) %>% filter(n > 1) %>% nrow() # 18208
ead_metrics %>% distinct(receipt_number) %>% nrow() # 1457807
18208 / 1457807
ead_metrics %>% nrow() # 1457807
# note that the vast majority of anumbers with multiple approved initial filings had only 2 
ead_metrics %>% filter(part_2_1 == "A") %>% count(anumber, name = "anumber_receipt_count") %>% count(anumber_receipt_count) %>% arrange(desc(n))


# test that sum of princ initial and renewals equals receipt count in raw ead dataset
expect_equal(object = i765_outcomes_by_fy_cohort_table %>% summarize(princ_initial_total = sum(princ_initial_sum),
                                                                     princ_renewal_total = sum(princ_renewal_sum)) %>%
                     mutate(princ_total = princ_initial_total + princ_renewal_total) %>% pull(princ_total),
             expected = ead %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% nrow())

# test that sum of dep initial and renewals equals receipt count in raw ead dataset
expect_equal(object = i765_outcomes_by_fy_cohort_table %>% summarize(dep_initial_total = sum(dep_initial_sum),
                                                                     dep_renewal_total = sum(dep_renewal_sum)) %>%
                     mutate(dep_total = dep_initial_total + dep_renewal_total) %>% pull(dep_total),
             expected = ead_dep %>% filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C"), 
                                           dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% nrow())

# test that sum of denial_count equals sum of princ and dep denials in raw data
# note the denial count is off by exactly 1 anumber - will come back to find out the issue, but low priority for now
expect_equal(object = i765_outcomes_by_fy_cohort_table %>% summarize(denial_sum = sum(denial_count)) %>% pull(denial_sum),
             expected = sum(ead %>% filter(CURRENT_STATUS == "Denied", part_2_1 %in% c("A", "C"), anumber %in% data$anumber) %>% nrow(),
                            ead_dep %>% filter(CURRENT_STATUS == "Denied", part_2_1 %in% c("A", "C"), 
                                               dep_anumber %in% i589_dep$dep_anumber, !(receipt_number %in% ead$receipt_number)) %>% nrow()))


#/////////////////////////////////////////


# get i765_outcomes_by_fy_cohort_table_formatted
i765_outcomes_by_fy_cohort_table_formatted <- i765_outcomes_by_fy_cohort_table %>%
        mutate(filing_date_fy = str_c(filing_date_fy, " cohort"),
                princ_initial_sum = comma(princ_initial_sum),
               princ_renewal_sum = comma(princ_renewal_sum),
               dep_initial_sum = comma(dep_initial_sum),
               dep_renewal_sum = comma(dep_renewal_sum),
               denial_count = comma(denial_count),
               ever_under_14_dep_pct = as_percent(ever_under_14_dep_pct, digits = 0)) %>%
        select(-ever_under_14_dep_count) %>%
        rename("I-589 FY filing cohort" = filing_date_fy,
               "Initial EAD approvals for I-589 principals" = princ_initial_sum,
               "Renewal EAD approvals for I-589 principals" = princ_renewal_sum,
               "Initial EAD approvals for I-589 dependents" = dep_initial_sum,
               "Renewal EAD approvals for I-589 dependents" = dep_renewal_sum,
               "Initial and renewal EAD denials for\nI-589 principals and dependents" = denial_count,
               "Share of I-589\ndependents with\nan approved EAD\nwho are under\n14 years old " = ever_under_14_dep_pct)

# inspect
i765_outcomes_by_fy_cohort_table_formatted


#///////////////////////////////////////


# title: I-765 outcomes for I-589 principal applicants and dependents, by I-589 FY filing cohort

# create footnote_table
footnote_table <- tibble(text = c("Source: USCIS RAIO; USCIS OP&S"))

# save with prd_format
workbook <- prd_format(workbook = workbook, tables = i765_outcomes_by_fy_cohort_table_formatted, 
                       output_sheet_names = "i765_outcomes_by_fy_cohort", 
                       summary_rows = NULL, summary_cols = NULL, 
                       text_cols = NULL, font_size = 10,
                       custom_col_width = list(c(rep(NA, times = 5), 14, 15)), 
                       custom_row_height = list(c(95)), 
                       style_table = NULL, footnote_table = footnote_table, superscript_table = NULL, 
                       col_width_padding = 1, min_col_width = 8.43, overwrite = TRUE)

# inspect
workbook %>% openXL()


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////





# save workbook
# workbook %>% saveWorkbook(file = "output/tables/asylum_ead_tables_20200824.xlsx", overwrite = TRUE)

# load workbook
workbook <- loadWorkbook(file = "output/tables/asylum_ead_tables_20200824.xlsx")

# inspect
workbook %>% openXL()





