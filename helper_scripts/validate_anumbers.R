# # load validate_anumbers function
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("validate_anumbers.R")
# setwd(current_wd)

library(tidyverse)
library(stringr)
library(rlang)
library(testthat)

# create function to validate a_numbers
validate_anumbers <- function(tbl, anumber_var = "anumber", duplicates_allowed = FALSE) {
        
        # set anumber variable name
        anumber_var_sym <- sym(anumber_var)
        
        # validate a_numbers with test
        test_that("validate a_numbers have correct format", {
                # all anumbers are valid
                expect_equal(tbl %>% filter(str_detect(string = !!anumber_var_sym, pattern = regex("^(A|a)[0-9]{9}$"))) %>% 
                                     nrow(), expected = nrow(tbl))
        })
        
        # handle duplicates_allowed
        if(duplicates_allowed == FALSE) {
                test_that("validate a_numbers are not duplicated", {
                        # no duplicated anumbers
                        expect_equal(tbl %>% distinct(!!anumber_var_sym) %>% nrow(), expected = nrow(tbl))
                })
        }
}



# test

# # valid w/ "a_number"
# test_tbl <- tibble(a_number = c("A123456789", "A987654321"))
# test_tbl %>% validate_anumbers(tbl = ., anumber_var = "a_number")
# test_tbl %>% validate_anumbers(tbl = .)
# 
# # valid w "anumber"
# test_tbl <- tibble(anumber = c("A123456789", "A987654321"))
# test_tbl %>% validate_anumbers(tbl = .)
# test_tbl %>% validate_anumbers(tbl = ., anumber_var = "a_number")
#
# # leading line break
# test_tbl <- tibble(anumber = c("\nA123456789", "A987654321"))
# test_tbl %>% validate_anumbers()
# 
# # leading space
# test_tbl <- tibble(anumber = c(" A123456789", "A987654321"))
# test_tbl %>% validate_anumbers()
# 
# # 10 digits
# test_tbl <- tibble(anumber = c("A1234567890", "A987654321"))  
# test_tbl %>% validate_anumbers()
# 
# # 8 digits
# test_tbl <- tibble(anumber = c("A12345678", "A987654321"))                                          
# test_tbl %>% validate_anumbers()
# 
# NA value
# test_tbl <- tibble(anumber = c(NA, "A987654321"))
# test_tbl %>% validate_anumbers()
# 
# # duplicate anumber
# test_tbl <- tibble(anumber = c("A123456789", "A123456789"))                                          
# test_tbl %>% validate_anumbers()
# test_tbl %>% validate_anumbers(duplicates_allowed = TRUE)
#
# # just an "A"
# test_tbl <- tibble(anumber = c("A123456789", "A"))
# test_tbl %>% validate_anumbers()

