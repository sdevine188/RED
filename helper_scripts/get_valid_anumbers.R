# # load get_valid_anumbers function
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_valid_anumbers.R")
# setwd(current_wd)

library(tidyverse)
library(stringr)
library(rlang)
library(testthat)
library(janitor)


# create get_valid_anumbers function
get_valid_anumbers <- function(tbl, anumber_var = "anumber", duplicates_allowed = FALSE) {
        
        # set anumber variable name
        anumber_var_sym <- sym(anumber_var)
        
        # add row_number as unique identifier for deconflicting valid_anumbers and non_dupe_anumbers
        tbl <- tibble(row_number = seq(from = 1, to = nrow(tbl))) %>% bind_cols(., tbl)
        
        # get valid anumbers
        valid_anumbers <- tbl %>% filter(str_detect(string = !!anumber_var_sym, pattern = regex("^(A|a)[0-9]{9}$")))
        
        # handle duplicates_allowed, or cases where duplicates are not allowed but there happen to be no duplicates
        # handling cases without duplicates here prevents getting the seemingly unsuppressable warning from get_dupes when there are no duplicates
        if(duplicates_allowed == TRUE | tbl %>% distinct(!!anumber_var_sym) %>% nrow() == nrow(tbl)) {
                return(valid_anumbers %>% select(-row_number))
        }
        
        if(duplicates_allowed == FALSE & tbl %>% distinct(!!anumber_var_sym) %>% nrow() != nrow(tbl)) {
                
                # get dupe_anumbers
                dupe_anumbers <- tbl %>% get_dupes(!!anumber_var_sym) %>% select(-dupe_count) 
                
                # drop any records contained in valid_anumbers and dupe_anumbers
                valid_anumbers <- valid_anumbers %>% anti_join(., dupe_anumbers, by = anumber_var)
                return(valid_anumbers %>% select(-row_number))
        }
}


# test

# # valid w/ "a_number"
# test_tbl <- tibble(a_number = c("A123456789", "A987654321"))
# test_tbl %>% get_valid_anumbers(tbl = ., anumber_var = "a_number")
# test_tbl %>% get_valid_anumbers(tbl = .)
# 
# # valid w "anumber"
# test_tbl <- tibble(anumber = c("A123456789", "A987654321"))
# test_tbl %>% get_valid_anumbers(tbl = .)
# test_tbl %>% get_valid_anumbers(tbl = ., anumber_var = "a_number")
# 
# # leading line break
# test_tbl <- tibble(anumber = c("\nA123456789", "A987654321"))
# test_tbl %>% get_valid_anumbers()
# 
# # leading space
# test_tbl <- tibble(anumber = c(" A123456789", "A987654321"))
# test_tbl %>% get_valid_anumbers()
# 
# # 10 digits
# test_tbl <- tibble(anumber = c("A1234567890", "A987654321"))
# test_tbl %>% get_valid_anumbers()
# 
# # 8 digits
# test_tbl <- tibble(anumber = c("A12345678", "A987654321"))
# test_tbl %>% get_valid_anumbers()
# 
# # NA value
# test_tbl <- tibble(anumber = c(NA, "A987654321"))
# test_tbl %>% get_valid_anumbers()
# 
# # duplicate anumber
# test_tbl <- tibble(anumber = c("A123456789", "A123456789"))
# test_tbl %>% get_valid_anumbers()
# test_tbl %>% get_valid_anumbers(duplicates_allowed = TRUE)
# 
# # just an "A"
# test_tbl <- tibble(anumber = c("A123456789", "A"))
# test_tbl %>% get_valid_anumbers()
# 
# correct/duplicate anumber, incorrect/duplicate anumbers, an incorrect anumber
test_tbl <- tibble(anumber = c("A023456788", "A023456789", "A023456789", "A123456789x", "A123456789z", "A123456789z"))
test_tbl %>% get_valid_anumbers()
test_tbl %>% get_valid_anumbers(duplicates_allowed = TRUE)
