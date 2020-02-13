library(tidyverse)
library(lubridate)
library(flextable)
library(officer)
library(fs)
library(janitor)
library(scales)
library(testthat)

# load validate_anumbers function
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("validate_anumbers.R")
setwd(current_wd)

# load get_invalid_anumbers function
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("get_invalid_anumbers.R")
setwd(current_wd)

# load as_percent()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("as_percent.R")
setwd(current_wd)

# load add_totals_row()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("add_totals_row.R")
setwd(current_wd)

# load save_flextable()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("save_flextable.R")
setwd(current_wd)


options(scipen=999)

# setwd
# setwd("H:/RED/CIGP")
# setwd("C:/users/sjdevine/Work Folders/Desktop/cigp")
# setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/RED/CIGP")
setwd("X:/CIGP Eval Study/fy_2015")


##########################################################


# overall notes on analysis
# i will combine NA and unknown values, 
# NA/Unk will be kept when making tables, but dropped for charts
# NAs will be dropped when calculating medians/means 
# for reference, OIS does include Unknown values in tables, but not always in charts
# i will not use confidence intervals and hypothesis tests
# ois does not insist on using confidence intervals or tests in descriptive stat tables
# https://www.dhs.gov/sites/default/files/publications/Naturalizations_2017.pdf


##########################################################


# load cigp
dir_ls("data")
cigp <- read_csv("data/cigp_combined_data_subset_20190610.csv")
cigp %>% glimpse()
cigp %>% validate_anumbers(anumber_var = "a_number")
cigp %>% dim() # 32540

cigp %>% map(.x = ., .f = ~ sum(is.na(.x))) %>% enframe() %>% unnest() %>% arrange(value) %>% data.frame()


###########################################################


# number of participants
cigp %>% distinct(a_number) %>% nrow() # 32540
cigp %>% count(no_services) # 0

# note that there are still several which appear to be missing from data from either ois, c3, or c4
cigp %>% map(.x = ., .f = ~ sum(is.na(.x))) %>% enframe() %>% unnest() %>% arrange(value) %>% data.frame()

# note that several of those missing IDTADMIT for instance appear to have DOS visa packet
cigp %>% filter(is.na(IDTADMIT)) %>% select(a_number, IDTADMIT, ICOB, c3_country_name)
# though there are some that are on ois with IDTADMIT that also have visa packet, so it doesn't seem like visa packets are completely excluded?? eg A212272940
cigp %>% filter(!is.na(IDTADMIT)) %>% select(a_number, IDTADMIT, ICOB, c3_country_name)


###########################################################


# number of grantees
cigp %>% distinct(grantee, cohort, award, award_fy, grantee_city, grantee_state) %>% arrange(grantee)
cigp %>% distinct(grantee) %>% nrow() # 80
cigp %>% group_by(award_fy) %>% summarize(n = n_distinct(grantee))


###########################################################


# number of grantees by fiscal year
cigp %>% distinct(grantee, award_fy) %>% count(award_fy)


###########################################################


# award amount
cigp %>% distinct(grantee, award) %>% 
        mutate(award_numeric = as.numeric(str_replace_all(string = award, pattern = regex("\\$|,", ignore_case = TRUE), replacement = ""))) %>%
        summarize(award_mean = mean(award_numeric), award_median = median(award_numeric), award_max = max(award_numeric), award_min = min(award_numeric))

cigp %>% distinct(grantee, award, award_fy) %>% 
        mutate(award_numeric = as.numeric(str_replace_all(string = award, pattern = regex("\\$|,", ignore_case = TRUE), replacement = ""))) %>%
        group_by(award_fy) %>%
        summarize(award_mean = mean(award_numeric), award_median = median(award_numeric), award_max = max(award_numeric), award_min = min(award_numeric))

cigp %>% distinct(grantee, award) %>% 
        mutate(award_numeric = as.numeric(str_replace_all(string = award, pattern = regex("\\$|,", ignore_case = TRUE), replacement = ""))) %>%
        ggplot(data = ., aes(x = award_numeric)) + geom_histogram()

cigp %>% distinct(grantee, award) %>% 
        mutate(award_numeric = as.numeric(str_replace_all(string = award, pattern = regex("\\$|,", ignore_case = TRUE), replacement = ""))) %>%
        ggplot(data = ., aes(y = award_numeric)) + geom_boxplot()


###########################################################


# count of states served
cigp %>% distinct(grantee_state) %>% nrow() # 32

# distribution of participants across states
cigp %>% group_by(grantee_state) %>% mutate(state_count = n_distinct(row_number())) %>% ungroup() %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = grantee_state, .x = state_count))) + geom_bar() + coord_flip()
cigp %>% group_by(grantee_state) %>% mutate(state_count = n_distinct(row_number())) %>% ungroup() %>%       
        ggplot(data = ., aes(x = fct_reorder(.f = grantee_state, .x = state_count, .desc = TRUE))) + stat_ecdf(aes(group = 1))

# for report, get state shares
cigp %>% group_by(grantee_state) %>% mutate(state_count = n_distinct(row_number())) %>% ungroup() %>%
        distinct(grantee_state, state_count) %>% arrange(state_count) %>% mutate(state_share = state_count / sum(state_count),
        cum_state_share = cumsum(state_share)) %>% arrange(desc(cum_state_share)) %>% data.frame()


###########################################################


# distribution of ages
cigp %>% glimpse()

# there are 71 with age < 18, and additional 38 with NA age_at_program_entry
# for report, these 109 will be dropped from age table
cigp %>% filter(is.na(age_at_program_entry)) %>% nrow() # 38
cigp %>% filter(age_at_program_entry < 18) %>% nrow() # 71
cigp %>% filter(age_at_program_entry < 18 | is.na(age_at_program_entry)) %>% nrow() # 109

# use the pre-existing age bins from fy 2014 report and ois report
# https://www.dhs.gov/sites/default/files/publications/Naturalizations_2017.pdf
age_bin_tbl <- cigp %>% mutate(
                age_18_to_24 = ifelse(age_at_program_entry >= 18 & age_at_program_entry <= 24, 1, 0),
                age_25_to_34 = ifelse(age_at_program_entry >= 25 & age_at_program_entry <= 34, 1, 0),
                age_35_to_44 = ifelse(age_at_program_entry >= 35 & age_at_program_entry <= 44, 1, 0),
                age_45_to_54 = ifelse(age_at_program_entry >= 45 & age_at_program_entry <= 54, 1, 0),
                age_55_to_64 = ifelse(age_at_program_entry >= 55 & age_at_program_entry <= 64, 1, 0),
                age_65_or_older = ifelse(age_at_program_entry >= 65, 1, 0),
                age_bin = case_when(age_18_to_24 == 1 ~ "age_18_to_24", age_25_to_34 == 1 ~ "age_25_to_34",
                                    age_35_to_44 == 1 ~ "age_35_to_44", age_45_to_54 == 1 ~ "age_45_to_54",
                                    age_55_to_64 == 1 ~ "age_55_to_64", age_65_or_older == 1 ~ "age_65_or_older", TRUE ~ NA_character_)) 

# inspect age_bin_tbl
# note there are 175 with NA age_bin 
age_bin_tbl %>% count(age_bin)

# inspect age_at_program_entry for outliers
# there are several very young outliers and one very old outlier, 
# i tried to confirm their ages in pcqs 
# 1 year old was 1 yr old
# 5 yr old was 5; the 135 yo seemed like data issue; the 101 yo seemed like he was actually 104 at program entry
# 104 yo was 104 - seems good to go, just not clear why the young kids were entred into the program, 
# maybe eager grantees logged them if they came with parents, or maybe they just wanted english/civic services and they let it go??
age_bin_tbl %>% count(age_at_program_entry) %>% arrange(age_at_program_entry)
age_bin_tbl %>% count(age_at_program_entry) %>% arrange(desc(age_at_program_entry))

age_bin_tbl %>% filter(age_at_program_entry == 1) %>% select(a_number, IAGE, IDTADMIT, age_at_program_entry, DOB_DT, BEN_DATE_OF_BIRTH)
age_bin_tbl %>% filter(age_at_program_entry == 5) %>% select(a_number, IAGE, IDTADMIT, age_at_program_entry, DOB_DT, BEN_DATE_OF_BIRTH)

age_bin_tbl %>% count(age_at_program_entry) %>% arrange(desc(age_at_program_entry))
age_bin_tbl %>% filter(age_at_program_entry > 100) %>% select(a_number, IAGE, IDTADMIT, age_at_program_entry)

age_bin_tbl %>% ggplot(data = ., aes(x = age_at_program_entry)) + stat_ecdf()
age_bin_tbl %>% ggplot(data = ., aes(y = age_at_program_entry)) + geom_boxplot()


###################


age_bin_tbl %>%
        select(IAGE, IDTADMIT, years_as_lpr_at_program_entry, age_at_program_entry) %>%
        ggplot(data = ., aes(x = age_at_program_entry)) + stat_ecdf() +
        scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5))

age_bin_tbl %>%
        select(IAGE, IDTADMIT, years_as_lpr_at_program_entry, age_at_program_entry) %>%
        ggplot(data = ., aes(x = age_at_program_entry)) + geom_histogram(binwidth = 1) + 
        scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5))

age_bin_tbl %>%
        select(IAGE, IDTADMIT, years_as_lpr_at_program_entry, age_at_program_entry) %>% skim()

# for report, median age is calculated after dropping those 118 with age_at_program_entry = NA, or age < 18 
# note little difference between mean and median
age_bin_tbl %>% filter(age_at_program_entry < 18 | is.na(age_at_program_entry)) %>% nrow() # 109
age_bin_tbl %>% filter(age_at_program_entry >= 18, !is.na(age_at_program_entry)) %>% 
        summarize(age_at_program_entry_mean = mean(age_at_program_entry, na.rm = TRUE), 
                  age_at_program_entry_median = median(age_at_program_entry, na.rm = TRUE))

age_bin_tbl %>%
        select(IAGE, IDTADMIT, years_as_lpr_at_program_entry, age_at_program_entry) %>% 
        summarize(quantiles = list(enframe(quantile(x = age_at_program_entry, na.rm = TRUE)))) %>% unnest()

# for report, overall age_distribution, leaving in NA values
age_bin_tbl %>% count(age_bin)
# age_bin_tbl %>% filter(age_at_program_entry >= 18, !is.na(age_at_program_entry)) %>% 
#         count(age_bin) %>% mutate(pct = n / sum(n))
age_bin_tbl %>% filter(is.na(age_at_program_entry) | !(age_at_program_entry < 18)) %>%
        count(age_bin) %>% mutate(pct = n / sum(n), cum_pct = cumsum(pct)) 

# for report, get gender_age_bin_tbl
# note there are only 12 Gender = U, and 17 gender NA
# for report, gender = u and NA gender will be dropped from graph, though both will remain for table
age_bin_tbl %>% count(gender_at_program_entry)
age_bin_tbl %>% filter(is.na(age_bin)) %>% nrow() # 109
age_bin_tbl %>% filter(is.na(age_bin) | age_at_program_entry < 18 | IGENDER == "U") %>% nrow() # 140
gender_age_bin_tbl <- age_bin_tbl %>% filter(age_at_program_entry >= 18,
                                             gender_at_program_entry %in% c("M", "F")) %>% 
        group_by(gender_at_program_entry) %>% count(age_bin) %>% 
        mutate(pct = n / sum(n)) %>% ungroup()
gender_age_bin_tbl

# for report, inspect gender shares
cigp %>% mutate(gender_at_program_entry = case_when(is.na(gender_at_program_entry) ~ "U", 
                                TRUE ~ gender_at_program_entry)) %>% count(gender_at_program_entry) %>% 
        group_by(gender_at_program_entry) %>%
        summarize(sum_n = sum(n)) %>% mutate(share = sum_n / sum(sum_n))

# get age distribution by gender chart
gender_age_bin_tbl %>% ggplot(data = ., aes(x = age_bin, y = pct * 100, fill = gender_at_program_entry)) + 
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) 
        

###########################################################


# marital status
# note 63 have NA marital status
# for report, NA values stay in, which is what happens when I summarize for the demographics table due to na.rm = TRUE
cigp %>% count(marital_status_at_program_entry) 
cigp %>% count(marital_status_at_program_entry) %>% 
        mutate(pct = n / sum(n)) %>% arrange(desc(pct))


###########################################################


# occupation
# note that 8134 not reported, and 1386 NA
# for report these will be lumped into unknown
cigp %>% count(occ_at_program_entry) %>% filter(is.na(occ_at_program_entry) | occ_at_program_entry == "Not reported")
cigp %>% count(occ_at_program_entry) %>% mutate(occ_at_program_entry = case_when(occ_at_program_entry == "Not reported" ~ "Unknown",
                                                                                 is.na(occ_at_program_entry) ~ "Unknown",
                                                                                 TRUE ~ occ_at_program_entry)) %>% 
        mutate(pct = n / sum(n)) %>% arrange(desc(pct)) %>% data.frame()

cigp <- cigp %>% mutate(occ_at_program_entry = case_when(occ_at_program_entry == "Not reported" ~ "Unknown",
                                                         is.na(occ_at_program_entry) ~ "Unknown",
                                                         TRUE ~ occ_at_program_entry)) %>%
        mutate(occ_group = case_when(occ_at_program_entry == "Unknown" ~ "Unknown",
                occ_at_program_entry == "Unemployed" ~ "Unemployed", occ_at_program_entry == "Homemakers" ~ "Homemakers",
                                      occ_at_program_entry == "Handlers, equipment cleaners, helpers & lab" ~ "Service occupations",
                                      occ_at_program_entry == "Food & beverage prep and service" ~ "Service occupations",
                                      
                                      # note that I change definition from "Students or children" to just "Students" to avoid confusion 
                                      # since children can't file N400
                                      occ_at_program_entry == "Students or children" ~ "Students", 
                                      
                                      occ_at_program_entry == "Health service" ~ "Service occupations", 
                                      occ_at_program_entry == "Retirees" ~ "Retirees",
                                      occ_at_program_entry == "Marketing and sales personnel" ~ "Sales and office occupations",
                                      occ_at_program_entry == "Cleaning & building service" ~ "Service occupations",
                                      occ_at_program_entry == "Transportation & material moving" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Personal service" ~ "Service occupations",
                                      occ_at_program_entry == "Machine operators & tenders" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Administrative support occ. inc. clerical" ~ "Sales and office occupations",
                                      occ_at_program_entry == "Construction" ~ "Construction, extraction, maintenance, and repair occupations",
                                      occ_at_program_entry == "Health technicians" ~ "Service occupations",
                                      occ_at_program_entry == "Executive, administrative, managerial" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Mechanics & repairers, computer repair" ~ "Construction, extraction, maintenance, and repair occupations",
                                      occ_at_program_entry == "Management Support" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Teachers, except college" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Childcare workers, private household" ~ "Service occupations",
                                      occ_at_program_entry == "Precision production" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Make-up artists, hairdressers, wardrobe artists" ~ "Service occupations",
                                      occ_at_program_entry == "Accountants and auditors" ~ "Service occupations",
                                      occ_at_program_entry == "Dentists, optom., podiatrists, etc." ~ "Service occupations",
                                      occ_at_program_entry == "Social, recreation, religious workers" ~ "Service occupations",
                                      occ_at_program_entry == "Private household service (excluding child care)" ~ "Service occupations",
                                      occ_at_program_entry == "Engineers, other specified" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Protective service" ~ "Service occupations",
                                      occ_at_program_entry == "Agriculture and horticultural workers" ~ "Farming, fishing, and forestry occupations",
                                      occ_at_program_entry == "Registered nurses" ~ "Service occupations",
                                      occ_at_program_entry == "Other performers" ~ "Service occupations",
                                      occ_at_program_entry == "Computer programers, technologists & technicians exc. health" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Electrical and electronic engineers" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Pharmacists, dieticians, PA, therapists" ~ "Service occupations",
                                      occ_at_program_entry == "Teachers, college & university" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Industrial engineers" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Extractive" ~ "Construction, extraction, maintenance, and repair occupations",
                                      occ_at_program_entry == "Librarians, archivists, & curators" ~ "Sales and office occupations",
                                      occ_at_program_entry == "Mechanical & marine engineers, naval arch." ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Architects and surveyors" ~ "Construction, extraction, maintenance, and repair occupations",
                                      occ_at_program_entry == "Athletes & related workers" ~ "Service occupations",
                                      occ_at_program_entry == "Writers, artists, composers" ~ "Service occupations",
                                      occ_at_program_entry == "Actors, actresses, directors" ~ "Service occupations",
                                      occ_at_program_entry == "Editors, reporters, PR, announcers" ~ "Service occupations",
                                      occ_at_program_entry == "Military" ~ "Military",
                                      occ_at_program_entry == "Civil engineers" ~ "Construction, extraction, maintenance, and repair occupations",
                                      occ_at_program_entry == "Dancers, choreographers" ~ "Service occupations",
                                      occ_at_program_entry == "Chemical, nuclear, petrol. engineers" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Physicians" ~ "Service occupations",
                                      occ_at_program_entry == "Aerospace engineers" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Forestry & logging" ~ "Farming, fishing, and forestry occupations",
                                      occ_at_program_entry == "Fishers, hunters, trappers" ~ "Farming, fishing, and forestry occupations",
                                      occ_at_program_entry == "Musicians, singers, opera performers, composers" ~ "Service occupations",
                                      occ_at_program_entry == "Physical therapists" ~ "Service occupations",
                                      occ_at_program_entry == "Physical scientists" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Vocational & educational counselors" ~ "Service occupations",
                                      occ_at_program_entry == "Computer, mathematical, O.R. scientists" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Social scientists & urban planners" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Life scientists" ~ "Production, transportation, and material moving occupations",
                                      occ_at_program_entry == "Lawyers & judges" ~ "Management, professional, and related occupations",
                                      occ_at_program_entry == "Not reported" ~ "Unknown",
                                      TRUE ~ NA_character_)) 

# for report, will include NA as unknown
# note that ois profile on naturalized citizens presents this Unknown category
# https://www.dhs.gov/profiles-naturalized-citizens-2015-country
cigp %>% count(occ_group) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n))
14.6 + 9.4 + 5 + 3.1

# get pct not employed for some reason
cigp %>% count(occ_group) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n)) %>%
        filter(occ_group %in% c("Unemployed", "Homemakers", "Students", "Retirees")) %>%
        summarize(pct_sum = sum(pct))


###########################################################


# region of birth

# inspect 
# for report, NA and unknown are lumped into unknown
# note there are 71 unknown
cigp %>% mutate(region_of_birth_at_program_entry = case_when(is.na(region_of_birth_at_program_entry) ~ "Unknown", 
                TRUE ~ region_of_birth_at_program_entry)) %>% count(region_of_birth_at_program_entry) %>% 
                        arrange(desc(n)) %>% mutate(pct = n / sum(n), cum_pct = cumsum(pct))
33.1  + 19.4 + 12.4 + 11.4


###########################################################


# country of birth
# note there are 38 NA country_name_at_program_entry  
cigp %>% filter(is.na(country_name_at_program_entry)) %>% nrow() # 38
cigp %>% count(country_name_at_program_entry) %>% 
        arrange(desc(n)) %>% mutate(pct = n / sum(n), cum_pct = cumsum(pct))

# i verified in pcqs the anumbers have COB Bhutan and Burma, also that's what cigp grantee data shows too
cigp %>% filter(ICOB == "BHUTA") %>% count(grantee) %>% arrange(desc(n))
cigp %>% filter(ICOB == "BHUTA") %>% select(a_number, country_of_birth, ICOB)
cigp %>% filter(ICOB == "BURMA") %>% count(grantee) %>% arrange(desc(n))
cigp %>% filter(ICOB == "BURMA") %>% select(a_number, country_of_birth, ICOB)

# for report, get count of countries from world_ne_sovereign (see create_report_maps.R script)
# this excludes several of the territories, renamed countries, etc that are included in cigp country_name_at_program_entry, 
# and matches how many sovereign countries are colored on the map 
cigp %>% filter(!is.na(country_name_at_program_entry)) %>% count(country_name_at_program_entry) %>% 
        arrange(desc(n)) %>% mutate(pct = n / sum(n), cum_pct = cumsum(pct))

# note that 74 participants in cigp from northern mariana, us, unknown, and virgin islands, us, did not join with world_ne_sovereign
# this is noted and can be inspected in create_report_maps.R when creating the world_map and the country_bar_chart
# so world_ne_sovereign shows a total of 32466 participants
32466 + 74 == 32540
world_ne_sovereign %>% glimpse()
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% summarize(sum_participants = sum(n)) # 32466
# note i use nrow(cigp) as the denominator instead of sum(n) (32540 vs 32466 as discussed above)

# get country shares
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% arrange(desc(n)) %>% mutate(pct = n / nrow(cigp), cum_pct = cumsum(pct)) %>%
        data.frame() # 160
19.2 + 6.7 + 5.3 + 4.6 + 4.3 + 4.1 + 3.8 + 3.8

# inspect country_name_at_entry not found in world_ne_sovereign 
# note they are not found because the country names were updated in world_ne_sovereign
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% select(SOVEREIGNT) %>%
        anti_join(cigp %>% count(country_name_at_program_entry) %>% select(-n), ., by = c("country_name_at_program_entry" = "SOVEREIGNT")) %>% 
        data.frame()

# check sum of other category on country_bar_chart
# note that because the 74 paricipants from cigp cited above did not match world_ne_soverign countries (us, virgin islands, unknown, etc)
# these 74 must be added manually to the other category
world_ne_sovereign %>% filter(n < 170) %>% distinct(SOVEREIGNT, n) %>% summarize(sum = sum(n)) # 3878
world_ne_sovereign %>% filter(n >= 170) %>% distinct(SOVEREIGNT, n) %>% summarize(sum = sum(n)) # 28588
3878 + 28588 == 32466
3878 + 28588 + 74 == 32450
3878 + 74 # other category count for report


###########################################################


# class of admission
# note 639 NA coa_group
# for report, NA will be removed they will be categorized as unknown
cigp %>% count(coa_group) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n), cum_pct = cumsum(pct))


###########################################################


# years_as_lpr
cigp %>% count(years_as_lpr_at_program_entry) %>% arrange(years_as_lpr_at_program_entry)
cigp %>% filter(years_as_lpr_at_program_entry < 3) %>% select(IDTADMIT, years_as_lpr_at_program_entry)
cigp %>% count(years_as_lpr_at_program_entry) %>% arrange(desc(years_as_lpr_at_program_entry))
cigp %>% filter(years_as_lpr_at_program_entry > 60) %>% select(IDTADMIT, years_as_lpr_at_program_entry)

# note there are 132 NA years_as_lpr, all of which were missing IDTADMIT in ois data
cigp %>% filter(is.na(years_as_lpr_at_program_entry)) %>% nrow() # 132
cigp %>% filter(is.na(IDTADMIT)) %>% nrow() # 132

# plot years_in_lpr
cigp %>% ggplot(data = ., aes(y = years_as_lpr_at_program_entry)) + geom_boxplot()
cigp %>% ggplot(data = ., aes(x = years_as_lpr_at_program_entry)) + stat_ecdf()
cigp %>% ggplot(data = ., aes(x = years_as_lpr_at_program_entry)) + geom_histogram(binwidth = 1)

# for report, calculate median (after removing NAs)
cigp %>% summarize(years_as_lpr_quantiles = list(enframe(quantile(years_as_lpr_at_program_entry, na.rm = TRUE)))) %>% unnest()
cigp %>% summarize(years_as_lpr_mean = mean(years_as_lpr_at_program_entry, na.rm = TRUE),
                   years_as_lpr_median = median(years_as_lpr_at_program_entry, na.rm = TRUE))


###########################################################


# cagp services used
cigp %>% count(no_services)
cigp %>% mutate(cigp_serv = case_when(citizenship_services_flag == 1 & application_services_flag == 0 ~ "citz_serv_only",
                citizenship_services_flag == 0 & application_services_flag == 1 ~ "appl_serv_only",
                citizenship_services_flag == 1 & application_services_flag == 1 ~ "both_citz_and_appl_serv", TRUE ~ NA_character_)) %>% 
        count(cigp_serv) %>% mutate(pct = n / sum(n))
cigp %>% count(application_services_flag) %>% mutate(pct = n / sum(n))


###########################################################
###########################################################
###########################################################


# get outcomes for report

# load c4_app_table and c4_viable_app_table
c4_app_table <- read_csv("data/c4_app_table.csv")
c4_viable_app_table <- read_csv("data/c4_viable_app_table.csv")
c4_viable_app_table_w_test_sequence_and_outcome <- read_csv("data/c4_viable_app_table_w_test_sequence_and_outcome.csv")

# inspect
c4_viable_app_table_w_test_sequence_and_outcome %>% nrow() # 25129
c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR) %>% nrow() # 22676


#######################################################


# get n400_submitted_after_program_entry
n400_submitted_after_program_entry <- c4_viable_app_table_w_test_sequence_and_outcome %>% 
        filter(MR_RECV_DTIME >= date_earliest_program_entry) 
n400_submitted_after_program_entry %>% distinct(ALIEN_NBR) %>% nrow()
n400_submitted_after_program_entry
20480 / 32540

# note 144 submitted 2 apps since program entry; only one submitted three apps since program entry, no anumbers had more than 3 apps
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(MR_RECV_DTIME >= date_earliest_program_entry) %>% 
        distinct(ALIEN_NBR, APP_ID) %>% count(ALIEN_NBR) %>% filter(n > 1) %>%
        arrange(desc(n))

# note 196 anumbers had two viable apps during study period (only one had 3, none had more)
c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR, APP_ID) %>% count(ALIEN_NBR) %>% filter(n > 1) %>%
        arrange(desc(n))

# get viable_n400_submitted_prior_to_program_entry
viable_n400_submitted_prior_to_program_entry <- c4_viable_app_table_w_test_sequence_and_outcome %>% filter(MR_RECV_DTIME < date_earliest_program_entry) 
viable_n400_submitted_prior_to_program_entry %>% distinct(ALIEN_NBR) %>% nrow() # 2246
# note 843 anumbers had prior_to_program_entry apps submitted during fy2015
viable_n400_submitted_prior_to_program_entry %>% filter(MR_RECV_DTIME < "2014-10-01") %>% distinct(ALIEN_NBR) %>% nrow() # 843

# get viable_n400_submitted_only_prior_to_program_entry
viable_n400_submitted_prior_to_program_entry %>% anti_join(., n400_submitted_after_program_entry, by = "ALIEN_NBR") %>% distinct(ALIEN_NBR) %>% nrow() # 2196
2196 / 32540 # note it was rounded up to 6.8 for report so that after_program_entry + only_prior_to_program_entry percentage = 69.7 (22676 / 32540)

# get viable_n400_during_study_period
c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR) %>% nrow() # 22676
20480 + 2196 == 22676

# get proportion of anumbers who had viable app during study
22676 / 32540

# get proportion of anumbers with viable apps who actually took the test
# note there are 63 records where passed_test == 1 but NA TEST_DT, because i had to use non-NA CERT_NBR as equivalent to passed_test = 1
# when creating c4_viable_app_table_w_test_sequence_and_outcome, to avoid cases where ENGL and CITZ results didn't clearly show passing, but they had CERT_NBR
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(!is.na(TEST_DT) | passed_test == 1) %>% distinct(ALIEN_NBR) %>% nrow() # 22384
22384 / 22676

# get proportion of anumbers with viable apps who took test only one time, and those who took it two times (across all viable applications)
c4_viable_app_table_w_test_sequence_and_outcome %>% count(sequential_test_date)
c4_viable_app_table_w_test_sequence_and_outcome %>% 
        # note need to convert NA values of sequential_test_date to 0 before using max(),
        # otherwise i need to use na.rm = TRUE, and for those anumbers with only NA values, max() returns -Inf, which gets more complicated to handle
        mutate(sequential_test_date = ifelse(is.na(sequential_test_date), 0, sequential_test_date)) %>%
        group_by(ALIEN_NBR) %>% mutate(max_sequential_test_date = max(sequential_test_date)) %>% ungroup() %>%
        distinct(ALIEN_NBR, max_sequential_test_date) %>% count(max_sequential_test_date) %>% mutate(pct = n / sum(n))
20151 + 2233 + 292 == 22676
20151 / 22676
2233 / 22676
292 / 22676
88.9 + 9.8 + 1.3 == 100
(20151 + 2233) / 22676
88.9 + 9.8

# get proportion of those with viable apps who passed test
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(passed_test == 1) %>% distinct(ALIEN_NBR) %>% nrow() # 21177
21177 / 22676


##########################################


# get proportion of anumbers with viable app who passed on the first attempt
passed_on_first_test <- c4_viable_app_table_w_test_sequence_and_outcome %>% filter(sequential_test_date == 1 & passed_test == 1) 
passed_on_first_test %>% distinct(ALIEN_NBR) %>% nrow() # 21165
21165 / 22676

# get proportion of anumbers with viable app who passed on the second attempt
passed_on_second_test <- c4_viable_app_table_w_test_sequence_and_outcome %>% filter(sequential_test_date == 2 & passed_test == 1) 
passed_on_second_test %>% distinct(ALIEN_NBR) %>% nrow() # 1448
1448 / 22676

# find those anumbers on both who are double_counted
double_counted <- inner_join(passed_on_first_test %>% select(ALIEN_NBR), passed_on_second_test %>% select(ALIEN_NBR), by = "ALIEN_NBR") 
double_counted %>% distinct(ALIEN_NBR) %>% nrow() # 1436
1436 / 22676

# note there is 0 records with passed_test == 1 but NA sequential_test_date
no_sequential_test_date <- c4_viable_app_table_w_test_sequence_and_outcome %>% filter(passed_test == 1, is.na(sequential_test_date)) %>% distinct(ALIEN_NBR) 
no_sequential_test_date %>% distinct(ALIEN_NBR) %>% nrow() # 0

# get proportion of anumbers with viable app who passed on either first or second attempt
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(passed_test == 1) %>% distinct(ALIEN_NBR) %>% nrow() # 21177
21177 / 22676
# confirm this by adding passed_on_first_test + passed_on_second_test - on_both_passed_on_first/second 
21165 + 1448 - 1436 == 21177
21165 / 22676
1448 / 22676
1436 / 22676
.933 + .064 - .063 # == .999

# for the report when citing proportion with viable apps who passed on first/second try 
# note i subtract the proportion who show in data as passing on BOTH first and second (data quality issue??) from the proportion passing on first test
(21165 - 1436) / 22676 # first try
1448 / 22676 # second try
.87 + .064 # == .934


##########################################


# get proportion of anumbers with viable app who went on to get citizenshiph from their app
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(!is.na(CERT_NBR)) %>% distinct(ALIEN_NBR) %>% nrow() # 20833
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(!is.na(CERT_NBR)) %>% count(passed_test)
20833 / 22676


###########################################################


# waivers for natz test
c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR, viable_app_english_test_wavied_ever_flag) %>% 
        count(viable_app_english_test_wavied_ever_flag) # 2368
2368 / 32540

c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR, viable_app_civics_test_wavied_ever_flag) %>%
        count(viable_app_civics_test_wavied_ever_flag) # 1344
1344 / 32540

c4_viable_app_table_w_test_sequence_and_outcome %>% 
        distinct(ALIEN_NBR, viable_app_english_test_wavied_ever_flag, viable_app_civics_test_wavied_ever_flag) %>%
        mutate(both_english_and_civics_test_waived = case_when(viable_app_english_test_wavied_ever_flag == 1 &
                                                                        viable_app_civics_test_wavied_ever_flag == 1 ~ 1, 
                                                              TRUE ~ 0)) %>%
        count(both_english_and_civics_test_waived) # 1272
1272 / 32540



###########################################################


# n-400 fee waived
cigp %>% count(viable_app_fee_waived_flag) %>% mutate(pct = n / sum(n))
cigp %>% count(viable_app_during_study_period) %>% mutate(pct = n / sum(n))
# 22676

# for report
cigp %>% filter(viable_app_during_study_period == 1) %>% count(viable_app_fee_waived_flag) %>% mutate(pct = n / sum(n))
11952 / 22676


###########################################


# get proportion of overall participants who naturalized based on viable app
c4_viable_app_table_w_test_sequence_and_outcome %>% filter(!is.na(CERT_NBR)) %>% distinct(ALIEN_NBR) %>% nrow() # 20833
cigp %>% nrow() # 32540
20833 / 32540


################################################################################################
################################################################################################
################################################################################################


# create tables

# i originally converted ois report to word to get fonts/colors https://www.dhs.gov/sites/default/files/publications/Naturalizations_2017.pdf
# text is Joanna MT, tables are Trebuchet MS, 
# but then RED settled on Source Sans Pro, which I used to update the final output tables below
# note that the code below does save intermediate flextable outputs, but the final outputs don't rely on them
# so when i updated the final output tables, i did not spend time going back to also update the intermediate flextable outputs
# also i refined the save method to use save_as_docx instead of save_flextable, etc

# load convert_color_from_rgb_to_hex()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("convert_color_from_rgb_to_hex.R")
setwd(current_wd)

# blue color
convert_color_from_rgb_to_hex(0, 82, 136) # "#005288"


#################################################################################################


# get outcomes_by_region_table
cigp %>% glimpse()
cigp %>% count(region_of_birth_at_program_entry) %>% arrange(desc(n))

outcomes_by_region_table <- cigp %>% 
        mutate(region_of_birth_at_program_entry = case_when(region_of_birth_at_program_entry %in% c(NA, "Unknown") ~ "Unknown",
                                                             TRUE ~ region_of_birth_at_program_entry)) %>%
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
                appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
                both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(region_of_birth_at_program_entry) %>% 
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>%  arrange(desc(n))
        
outcomes_by_region_table


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_region_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_region_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_region_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_region_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})

test_that("n viable_app in table from cigp data matches n viable_app from c4_viable_app_table_w_test_sequence_and_outcome used in report text", {
        expect_equal(outcomes_by_region_table %>% mutate(n_viable_app = n * pct_viable_app) %>% select(region_of_birth_at_program_entry, n_viable_app) %>% 
                summarize(n = sum(n_viable_app)) %>% pull(n),
                expected = c4_viable_app_table_w_test_sequence_and_outcome %>% distinct(ALIEN_NBR) %>% nrow())
})

test_that("n natz in table from cigp data matches n natz from c4_viable_app_table_w_test_sequence_and_outcome used in report text", {
        expect_equal(outcomes_by_region_table %>% mutate(n_natz = n * pct_natz) %>% select(region_of_birth_at_program_entry, n_natz) %>% 
                             summarize(n = sum(n_natz)) %>% pull(n),
                     expected = c4_viable_app_table_w_test_sequence_and_outcome %>% filter(!is.na(CERT_NBR)) %>% distinct(ALIEN_NBR) %>% nrow())
})


################


# convert outcomes_by_region_table to flextable
outcomes_by_region_table <- outcomes_by_region_table %>% 
        mutate(n = comma(n),
                pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Region of birth" = region_of_birth_at_program_entry, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_region_table_ft <- outcomes_by_region_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_region_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_region_table_ft    

# save ft
# save_flextable(outcomes_by_region_table_ft, filename_wo_ext = "output/outcomes_by_region_table", zoom = 10)


#######################################################################################


# get outcomes_by_age_table

# note that 71 anumbers with age < 18 will be excluded from table since they were not technically eligible for services, 
# although they did receive services, it'd be confusing - will footnote the exclusion though
cigp %>% filter(age_at_program_entry < 18) %>% nrow() # 71

outcomes_by_age_table <- cigp %>% filter(age_at_program_entry >= 18 | is.na(age_at_program_entry)) %>% mutate(
        age_18_to_24 = ifelse(age_at_program_entry >= 18 & age_at_program_entry <= 24, 1, 0),
        age_25_to_34 = ifelse(age_at_program_entry >= 25 & age_at_program_entry <= 34, 1, 0),
        age_35_to_44 = ifelse(age_at_program_entry >= 35 & age_at_program_entry <= 44, 1, 0),
        age_45_to_54 = ifelse(age_at_program_entry >= 45 & age_at_program_entry <= 54, 1, 0),
        age_55_to_64 = ifelse(age_at_program_entry >= 55 & age_at_program_entry <= 64, 1, 0),
        age_65_or_older = ifelse(age_at_program_entry >= 65, 1, 0),
        age_bin = case_when(age_18_to_24 == 1 ~ "18 to 24 years", age_25_to_34 == 1 ~ "25 to 34 years",
                            age_35_to_44 == 1 ~ "35 to 44 years", age_45_to_54 == 1 ~ "45 to 54 years",
                            age_55_to_64 == 1 ~ "55 to 64 years", age_65_or_older == 1 ~ "65 years and over", TRUE ~ "Unknown")) %>%
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(age_bin) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(age_bin)
outcomes_by_age_table    


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_age_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n) + (cigp %>% filter(age_at_program_entry < 18) %>% nrow()), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_age_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_age_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_age_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_age_table to flextable
outcomes_by_age_table <- outcomes_by_age_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Age" = age_bin, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_age_table_ft <- outcomes_by_age_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_age_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_age_table_ft    

# save ft
# save_flextable(outcomes_by_age_table_ft, filename_wo_ext = "output/outcomes_by_age_table", zoom = 10)


##########################################################################################


# get outcomes_by_coa_table
cigp %>% count(coa_group) %>% arrange(desc(n)) %>% data.frame()

outcomes_by_coa_table <- cigp %>% 
        mutate(coa_group = case_when(coa_group %in% c("IRCA legalization", "Diversity",
                      "Cancellation of removal", NA, "Other",
                      "Parolees", "IRCA legalization dependents",
                      "Nicaraguan and Central American Relief Act (NACARA Section 202, P.L. 105-100)",
                      "Haitian Refugee Immigration Fairness Act (HRIFA, P.L. 105-277)") ~ "Other (Diversity visa, IRCA, parolees, unknown, etc.)",
                      TRUE ~ coa_group)) %>% 
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(coa_group) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(desc(n))
outcomes_by_coa_table     


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_coa_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_coa_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_coa_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_coa_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_age_table to flextable
outcomes_by_coa_table <- outcomes_by_coa_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Class of Admission" = coa_group, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_coa_table_ft <- outcomes_by_coa_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_coa_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_coa_table_ft    

# save ft
# save_flextable(outcomes_by_coa_table_ft, filename_wo_ext = "output/outcomes_by_coa_table")


##########################################################################################


# get outcomes_by_occ_table
cigp %>% count(occ_group) %>% arrange(desc(n)) %>% data.frame()

outcomes_by_occ_table <- cigp %>% 
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(occ_group) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(desc(n))
outcomes_by_occ_table  


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_occ_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_occ_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_occ_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_occ_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_age_table to flextable
outcomes_by_occ_table <- outcomes_by_occ_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Occupation" = occ_group, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_occ_table_ft <- outcomes_by_occ_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_occ_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_occ_table_ft    

# save ft
# save_flextable(outcomes_by_occ_table_ft, filename_wo_ext = "output/outcomes_by_occ_table")


#########################################################################################


# get outcomes_by_years_as_lpr_table

# note table will exclude 630 with years as lpr < 3 years, since they're technically not eligible to natz, and it'd be confusing - will footnote exclusion
cigp %>% filter(years_as_lpr_at_program_entry < 3) %>% nrow() # 630

cigp %>% count(years_as_lpr_at_program_entry) %>% arrange(years_as_lpr_at_program_entry) 
cigp %>% count(years_as_lpr_at_program_entry) %>% arrange(desc(years_as_lpr_at_program_entry))
cigp %>% filter(is.na(years_as_lpr_at_program_entry)) %>% nrow() # 132
cigp %>% ggplot(data = ., aes(x = years_as_lpr_at_program_entry)) + geom_histogram(binwidth = 1)
cigp %>% filter(years_as_lpr_at_program_entry > 30) %>% select(a_number, years_as_lpr_at_program_entry)
cigp %>% filter(years_as_lpr_at_program_entry >=3 & years_as_lpr_at_program_entry < 5) %>% nrow()
cigp %>% filter(years_as_lpr_at_program_entry == 1, viable_app_during_study_period == 1) %>% 
        select(a_number, years_as_lpr_at_program_entry, IDTADMIT, viable_app_w_cert_nbr_flag)

outcomes_by_years_as_lpr_table <- cigp %>% filter(years_as_lpr_at_program_entry >= 3 | is.na(years_as_lpr_at_program_entry)) %>%
        mutate(years_as_lpr_bin = case_when(years_as_lpr_at_program_entry == 3 ~ "3 years",
                                            years_as_lpr_at_program_entry == 4 ~ "4 years",
                                            years_as_lpr_at_program_entry == 5 ~ "5 years",
                                            years_as_lpr_at_program_entry == 6 ~ "6 years",
                                            years_as_lpr_at_program_entry == 7 ~ "7 years",
                                            years_as_lpr_at_program_entry == 8 ~ "8 years",
                                            years_as_lpr_at_program_entry == 9 ~ "9 years",
               years_as_lpr_at_program_entry >= 10 & years_as_lpr_at_program_entry < 20 ~ "10 to 19 years",
               years_as_lpr_at_program_entry >= 20 ~ "20 years or more")) %>% 
        mutate(years_as_lpr_bin = case_when(is.na(years_as_lpr_bin) ~ "Unknown", TRUE ~ years_as_lpr_bin)) %>% 
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(years_as_lpr_bin) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(desc(n)) %>%
        mutate(sequence = c(8, 2, 3, 9, 4, 1, 5, 7, 6, 10)) %>% arrange(sequence) %>% select(-sequence)
outcomes_by_years_as_lpr_table 


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_years_as_lpr_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n) + 
                             (cigp %>% filter(years_as_lpr_at_program_entry < 3) %>% nrow()), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_years_as_lpr_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_years_as_lpr_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_years_as_lpr_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_age_table to flextable
outcomes_by_years_as_lpr_table <- outcomes_by_years_as_lpr_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Years in LPR status" = years_as_lpr_bin, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz) 

outcomes_by_years_as_lpr_table_ft <- outcomes_by_years_as_lpr_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_years_as_lpr_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_years_as_lpr_table_ft    

# save ft
# save_flextable(outcomes_by_years_as_lpr_table_ft, filename_wo_ext = "output/outcomes_by_years_as_lpr_table")


##########################################################################################


# get outcomes_by_gender_table
cigp %>% count(gender_at_program_entry)

outcomes_by_gender_table <- cigp %>% mutate(gender_at_program_entry = case_when(gender_at_program_entry == "U" | is.na(gender_at_program_entry) ~ "Unknown",
                                                gender_at_program_entry == "F" ~ "Female", gender_at_program_entry == "M" ~ "Male",                                   
                                                                        TRUE ~ gender_at_program_entry)) %>%
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(gender_at_program_entry) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(desc(n))
outcomes_by_gender_table  


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_gender_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1 (or close after rounding)", {
        expect_equal(outcomes_by_gender_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_gender_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_gender_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_age_table to flextable
outcomes_by_gender_table <- outcomes_by_gender_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Gender" = gender_at_program_entry, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_gender_table_ft <- outcomes_by_gender_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_gender_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_gender_table_ft    

# save ft
# save_flextable(outcomes_by_gender_table_ft, filename_wo_ext = "output/outcomes_by_gender_table")


##########################################################################################


# get outcomes_by_marital_status_table
cigp %>% count(marital_status_at_program_entry)

outcomes_by_marital_status_table <- cigp %>% 
        mutate(marital_status_at_program_entry = case_when(is.na(marital_status_at_program_entry) | marital_status_at_program_entry == "UNKNOWN" ~ "Unknown",
                                                   marital_status_at_program_entry == "DIVORCED" ~ "Divorced", 
                                                   marital_status_at_program_entry == "MARRIED" ~ "Married",
                                                   marital_status_at_program_entry == "SEPARATED" ~ "Separated",
                                                   marital_status_at_program_entry == "SINGLE" ~ "Single",
                                                   marital_status_at_program_entry == "WIDOWED" ~ "Widowed",
                                                                                TRUE ~ marital_status_at_program_entry)) %>%
        mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
               appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
               both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        group_by(marital_status_at_program_entry) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>% ungroup() %>% arrange(desc(n))
outcomes_by_marital_status_table  


###############


# test 
test_that("n sums to nrow(cigp)", {
        expect_equal(outcomes_by_marital_status_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("pct_of_all_cagp sums to 1", {
        expect_equal(outcomes_by_marital_status_table %>% summarize(sum_pct_of_all_cagp = sum(pct_of_all_cagp)) %>% pull(sum_pct_of_all_cagp), 
                     expected = 1)
})

test_that("service dummy shares sum to 1", {
        expect_equal(outcomes_by_marital_status_table %>% select(pct_citz_serv_only, pct_appl_serv_only, pct_both_citz_and_appl_serv) %>%
                             mutate(row_number = row_number()) %>% group_by(row_number) %>% nest() %>% select(data) %>%
                             pmap_dfr(.l = ., .f = function(data, ...) { data %>% mutate(row_sum = rowSums(.))}) %>% distinct(row_sum) %>% pull(row_sum), 
                     expected = 1)
        
})

test_that("pct_viable_app is greater than or equal to pct_natz", {
        expect_equal(outcomes_by_marital_status_table %>% mutate(test = ifelse(pct_viable_app >= pct_natz, 1, 0)) %>% distinct(test) %>% pull(test), 
                     expected = 1)
})


################


# convert outcomes_by_marital_status_table to flextable
outcomes_by_marital_status_table <- outcomes_by_marital_status_table %>% 
        mutate(n = comma(n),
               pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
               pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
               pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
               pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
               pct_viable_app = as_percent(pct_viable_app, digits = 1),
               pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Marital status" = marital_status_at_program_entry, "Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz)

outcomes_by_marital_status_table_ft <- outcomes_by_marital_status_table %>% regulartable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Trebuchet MS", font.size = 14)) %>%
        style(i = seq(from = 1, to = nrow(outcomes_by_marital_status_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 14)) %>%
        add_footer_row(values = "Source: USCIS analysis of CAGP program data and applications for LPR status and naturalization of the Department of Homeland Security.", colwidths = ncol(outcomes_by_region_table)) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Trebuchet MS")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        autofit()
outcomes_by_marital_status_table_ft    

# save ft
# save_flextable(outcomes_by_marital_status_table_ft, filename_wo_ext = "output/outcomes_by_marital_status_table")


##########################################################################################


# get outcomes_total_table_a row
# note this is also used for the total row in table b, since the only difference between table a and b is the groups being summarized
outcomes_total_table_a <- cigp %>% mutate(citz_serv_only = ifelse(citizenship_services_flag == 1 & application_services_flag == 0, 1, 0),
                appl_serv_only = ifelse(citizenship_services_flag == 0 & application_services_flag == 1, 1, 0),
                both_citz_and_appl_serv = ifelse(citizenship_services_flag == 1 & application_services_flag == 1, 1, 0)) %>%
        summarize(n = n(), pct_of_all_cagp = n / nrow(.), 
                  pct_citz_serv_only = mean(citz_serv_only, na.rm = TRUE), 
                  pct_appl_serv_only = mean(appl_serv_only, na.rm = TRUE), 
                  pct_both_citz_and_appl_serv = mean(both_citz_and_appl_serv, na.rm = TRUE),
                  pct_viable_app = mean(viable_app_during_study_period),
                  pct_natz = mean(viable_app_w_cert_nbr_flag)) %>%
        mutate(n = comma(n),
                   pct_of_all_cagp = as_percent(pct_of_all_cagp, digits = 1), 
                   pct_citz_serv_only = as_percent(pct_citz_serv_only, digits = 1), 
                   pct_appl_serv_only = as_percent(pct_appl_serv_only, digits = 1), 
                   pct_both_citz_and_appl_serv = as_percent(pct_both_citz_and_appl_serv, digits = 1),
                   pct_viable_app = as_percent(pct_viable_app, digits = 1),
                   pct_natz = as_percent(pct_natz, digits = 1)) %>%
        rename("Number of CAGP participants" = n,
               "Percent of all CAGP participants" = pct_of_all_cagp, 
               "Percent receiving only citizenship instruction services" = pct_citz_serv_only,
               "Percent receiving only naturalization application services" = pct_appl_serv_only,
               "Percent receiving both citizenship instruction and naturalization application services" = 
                       pct_both_citz_and_appl_serv,
               "Percent with viable applications during FY 2015 CAGP service period" = 
                       pct_viable_app,
               "Percent who naturalized based on viable application" =
                       pct_natz) %>% bind_cols(tibble(Total = rep(x = "", times = nrow(.))), .) %>%
        mutate(Total = "Total")
outcomes_total_table_a 


##########################################################################################
##########################################################################################


# get outcomes_table_a_ft
outcomes_table_a_ft <- outcomes_by_years_as_lpr_table %>% rename("Region of birth" = "Years in LPR status") %>%
        bind_rows(outcomes_by_region_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        bind_rows(outcomes_by_coa_table %>% rename("Region of birth" = "Class of Admission"), .) %>% 
        bind_rows(outcomes_by_region_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        bind_rows(outcomes_by_region_table, .) %>% 
        bind_rows(outcomes_by_region_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        rename(" " = "Region of birth") %>%
        bind_rows(outcomes_total_table_a %>% rename(" " = "Total"), .) %>%
        mutate(row_number = row_number(), 
               ` ` = case_when(row_number == 2 ~ "Region of birth",
                               row_number == 2 + nrow(outcomes_by_region_table) + 1 ~ "Class of Admission when granted LPR status",
                               row_number == 2 +nrow(outcomes_by_region_table) + 1 + 
                                       nrow(outcomes_by_coa_table) + 1 ~ "Years in LPR status",
                               TRUE ~ ` `)) %>%
        select(-row_number) %>%
        flextable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        align(part = "body", j = 1, align = "left") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8)) %>%
        style(i = c(seq(from = 2, to = 2 + nrow(outcomes_by_region_table), by = 2),
                    seq(from = 2 + nrow(outcomes_by_region_table) + 1, 
                        to = 2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table), by = 2),
                    seq(from = 2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table) + 1, 
                        to = 2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table) + 1 + nrow(outcomes_by_years_as_lpr_table), by = 2)),
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 8)) %>%
        style(part = "body", i = c(2,
                                   2 + nrow(outcomes_by_region_table) + 1, 
                                   2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table) + 1),
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8),
              pr_c = fp_cell(background.color = "#005288")) %>% 
        merge_h_range(i = c(2, 2 + nrow(outcomes_by_region_table) + 1, 
                            2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table) + 1),
                      j1 = 1, j2 = ncol(outcomes_by_region_table), part = "body") %>%
        add_footer_row(colwidths = ncol(outcomes_by_region_table), values = "Source: USCIS Lawful Permanent Resident and naturalization data; CAGP data.") %>%
        add_footer_row(colwidths = ncol(outcomes_by_region_table), values = "placeholder", top = FALSE) %>%
        compose(i = 2, part = "footer", 
                value = as_paragraph(as_chunk("a ", props = fp_text(font.size = 8, vertical.align = "superscript", color = "#808080",
                                                                    font.family = "Source Sans Pro")),
                                     as_chunk("The 'Years in LPR status' section of the table excludes 630 CAGP participants with less than 3 years in LPR status as of 2015.",
                                              props = fp_text(font.size = 8, color = "#808080", font.family = "Source Sans Pro")))) %>%
        compose(i = 2 + nrow(outcomes_by_region_table) + 1 + nrow(outcomes_by_coa_table) + 1, part = "body", 
                j = which(names(outcomes_by_region_table) == "Region of birth"),
                value = as_paragraph(as_chunk("Years in LPR status", props = fp_text(color = "#ffffff", bold = FALSE, 
                                                                                     font.family = "Source Sans Pro", font.size = 8)),
                                     as_chunk("a", props = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", 
                                                                   font.size = 8, vertical.align = "superscript")))) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Source Sans Pro", font.size = 8, color = "#808080")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        width(width = c(1.3, .8, .8, .8, .9, .9, .8, .8))


outcomes_table_a_ft

# save ft
# outcomes_table_a_ft %>% save_as_docx(path = "output/outcomes_table_a.docx")


######################################################################################


# get outcomes_table_b_ft
outcomes_table_b_ft <- outcomes_by_occ_table %>% rename("Gender" = "Occupation") %>%
        bind_rows(outcomes_by_gender_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        bind_rows(outcomes_by_age_table %>% rename("Gender" = "Age"), .) %>% 
        bind_rows(outcomes_by_gender_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        bind_rows(outcomes_by_marital_status_table %>% rename("Gender" = "Marital status"), .) %>% 
        bind_rows(outcomes_by_gender_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        bind_rows(outcomes_by_gender_table, .) %>% 
        bind_rows(outcomes_by_gender_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        rename(" " = "Gender") %>%
        bind_rows(outcomes_total_table_a %>% rename(" " = "Total"), .) %>%
        mutate(row_number = row_number(), 
               ` ` = case_when(row_number == 2 ~ "Gender",
                               row_number == 2 + nrow(outcomes_by_gender_table) + 1 ~ "Marital status",
                               row_number == 2 + nrow(outcomes_by_gender_table) + 1 + 
                                       nrow(outcomes_by_marital_status_table) + 1 ~ "Age",
                               row_number == 2 + nrow(outcomes_by_gender_table) + 1 + 
                                       nrow(outcomes_by_marital_status_table) + 1 +
                                       nrow(outcomes_by_age_table) + 1 ~ "Occupation",
                               TRUE ~ ` `)) %>%
        select(-row_number) %>%
        flextable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        align(part = "body", j = 1, align = "left") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8)) %>%
        style(i = c(seq(from = 2, to = 2 + nrow(outcomes_by_gender_table), by = 2),
                    seq(from = 2 + nrow(outcomes_by_gender_table) + 1, 
                        to = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table), by = 2),
                    seq(from = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1, 
                        to = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1 + nrow(outcomes_by_age_table), by = 2),
                    seq(from = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1 + nrow(outcomes_by_age_table) + 1, 
                        to = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1 + nrow(outcomes_by_age_table) + 1 +
                                nrow(outcomes_by_occ_table), by = 2)),
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", pr_t = fp_text(font.size = 8)) %>%
        style(part = "body", i = c(2, 2 + nrow(outcomes_by_gender_table) + 1, 
                                   2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1,
                                   2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1 +
                                           nrow(outcomes_by_age_table) + 1),
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8),
              pr_c = fp_cell(background.color = "#005288")) %>%
        merge_h_range(i = c(2, 2 + nrow(outcomes_by_gender_table) + 1, 
                            2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1,
                            2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1 +
                                    nrow(outcomes_by_age_table) + 1),
                      j1 = 1, j2 = ncol(outcomes_by_gender_table), part = "body") %>%
        add_footer_row(colwidths = ncol(outcomes_by_gender_table), 
                       values = "Source: USCIS Lawful Permanent Resident and naturalization data; CAGP data.") %>%
        add_footer_row(colwidths = ncol(outcomes_by_gender_table), values = "placeholder", top = FALSE) %>%
        compose(i = 2, part = "footer", 
                value = as_paragraph(as_chunk("a ", props = fp_text(font.size = 8, vertical.align = "superscript", color = "#808080",
                                                                    font.family = "Source Sans Pro")),
                                     as_chunk("The 'Age' section of the table excludes 71 CAGP participants who were less than 18 years old as of 2015.",
                                              props = fp_text(color = "#808080", font.size = 8, font.family = "Source Sans Pro")))) %>%
        compose(i = 2 + nrow(outcomes_by_gender_table) + 1 + nrow(outcomes_by_marital_status_table) + 1, part = "body", 
                j = which(names(outcomes_by_gender_table) == "Gender"),
                value = as_paragraph(as_chunk("Age", props = fp_text(color = "#ffffff", bold = FALSE, 
                                                                     font.family = "Source Sans Pro", font.size = 8)),
                                     as_chunk("a", props = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", 
                                                                   font.size = 8, vertical.align = "superscript")))) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Source Sans Pro", font.size = 8, color = "#808080")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        width(width = c(1.3, .8, .8, .8, .9, .9, .8, .8))

outcomes_table_b_ft

# save ft
# outcomes_table_b_ft %>% save_as_docx(path = "output/outcomes_table_b.docx")


##################################################################################################
###################################################################################################


# create demographics_table
cigp %>% count(region_of_birth_at_program_entry) %>% arrange(desc(n))
cigp %>% count(viable_app_fee_waived_flag)
cigp %>% count(marital_status_at_program_entry) %>% arrange(desc(n))
cigp %>% count(coa_group) %>% arrange(desc(n))
cigp %>% count(occ_group) %>% arrange(desc(n))

demographics_table <- cigp %>% mutate(region_of_birth_at_program_entry = ifelse(is.na(region_of_birth_at_program_entry), "Unknown", region_of_birth_at_program_entry)) %>%
        group_by(region_of_birth_at_program_entry) %>% 
        mutate(married_flag = case_when(marital_status_at_program_entry == "MARRIED" ~ 1, TRUE ~ 0),
               single_flag = case_when(marital_status_at_program_entry == "SINGLE" ~ 1, TRUE ~ 0),
               divorced_flag = case_when(marital_status_at_program_entry == "DIVORCED" ~ 1, TRUE ~ 0),
               female_flag = case_when(gender_at_program_entry == "F" ~ 1, TRUE ~ 0),
               immed_relative_of_citz = case_when(coa_group == "Immediate relatives of U.S. citizens" ~ 1, TRUE ~ 0),
               refugee_and_asylee = case_when(coa_group == "Refugees and asylees" ~ 1, TRUE ~ 0),
               family_sponsored = case_when(coa_group == "Family-sponsored preferences" ~ 1, TRUE ~ 0),
               service_flag = case_when(occ_group == "Service occupations" ~ 1, TRUE ~ 0),
               unemployed_flag = case_when(occ_group == "Unemployed" ~ 1, TRUE ~ 0),
               homemaker_flag = case_when(occ_group == "Homemakers" ~ 1, TRUE ~ 0)) %>%
        group_by(region_of_birth_at_program_entry) %>%
        summarize(n = n(), pct_female = mean(female_flag, na.rm = TRUE), median_age = median(age_at_program_entry, na.rm = TRUE),
                  pct_single = mean(single_flag, na.rm = TRUE), pct_married = mean(married_flag, na.rm = TRUE),
                  pct_divorced = mean(divorced_flag, na.rm = TRUE), pct_service = mean(service_flag, na.rm = TRUE),
                  pct_unemployed = mean(unemployed_flag, na.rm = TRUE), pct_homemaker = mean(homemaker_flag, na.rm = TRUE),
                  pct_fee_waived = mean(viable_app_fee_waived_flag, na.rm = TRUE),
                  pct_immed_relative_of_citz = mean(immed_relative_of_citz, na.rm = TRUE), pct_refugee_and_asylee = mean(refugee_and_asylee, na.rm = TRUE),
                  pct_family_sponsored = mean(family_sponsored, na.rm = TRUE), median_years_as_lpr = median(years_as_lpr_at_program_entry, na.rm = TRUE)) %>%
        arrange(desc(n)) %>% select(-c(pct_divorced, pct_homemaker, pct_service))
demographics_table


################


# test
test_that("n sums to nrow(cigp)", {
        expect_equal(demographics_table %>% summarize(sum_n = sum(n)) %>% pull(sum_n), 
                     expected = cigp %>% nrow())
})

test_that("sum of pct_fee_waived from table matches total fee_waived mentioned in report", {
        expect_equal(demographics_table %>% mutate(n_fee_waived = n * pct_fee_waived) %>% 
                             summarize(sum_n_fee_waived = sum(n_fee_waived)) %>% pull(sum_n_fee_waived),
                     expected = cigp %>% filter(viable_app_during_study_period == 1) %>% count(viable_app_fee_waived_flag) %>% 
                             filter(viable_app_fee_waived_flag == 1) %>% pull(n))
})

test_that("pct_fee_waived in table is less pct_viable_app_submitted in table_1", {
        expect_equal(demographics_table %>% select(region_of_birth_at_program_entry, pct_fee_waived) %>% 
                             left_join(., cigp %>% 
                                               mutate(region_of_birth_at_program_entry = case_when(region_of_birth_at_program_entry %in% c(NA, "Unknown") ~ "Unknown",
                                                                                                   TRUE ~ region_of_birth_at_program_entry)) %>% 
                                               group_by(region_of_birth_at_program_entry) %>% 
                                               summarize(pct_submitted_viable_app = mean(viable_app_during_study_period, na.rm = TRUE)), 
                                       by = "region_of_birth_at_program_entry") %>% mutate(test = ifelse(pct_fee_waived < pct_submitted_viable_app, 1, 0)) %>%
                             distinct(test) %>% pull(test),
                     expected = 1)
})


###############


# get demographic_total_table
demographic_total_table <- cigp %>% mutate(region_of_birth_at_program_entry = ifelse(is.na(region_of_birth_at_program_entry), "Unknown", region_of_birth_at_program_entry)) %>%
        mutate(married_flag = case_when(marital_status_at_program_entry == "MARRIED" ~ 1, TRUE ~ 0),
               single_flag = case_when(marital_status_at_program_entry == "SINGLE" ~ 1, TRUE ~ 0),
               divorced_flag = case_when(marital_status_at_program_entry == "DIVORCED" ~ 1, TRUE ~ 0),
               female_flag = case_when(gender_at_program_entry == "F" ~ 1, TRUE ~ 0),
               immed_relative_of_citz = case_when(coa_group == "Immediate relatives of U.S. citizens" ~ 1, TRUE ~ 0),
               refugee_and_asylee = case_when(coa_group == "Refugees and asylees" ~ 1, TRUE ~ 0),
               family_sponsored = case_when(coa_group == "Family-sponsored preferences" ~ 1, TRUE ~ 0),
               service_flag = case_when(occ_group == "Service occupations" ~ 1, TRUE ~ 0),
               unemployed_flag = case_when(occ_group == "Unemployed" ~ 1, TRUE ~ 0),
               homemaker_flag = case_when(occ_group == "Homemakers" ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(), pct_female = mean(female_flag, na.rm = TRUE), median_age = median(age_at_program_entry, na.rm = TRUE),
                  pct_single = mean(single_flag, na.rm = TRUE), pct_married = mean(married_flag, na.rm = TRUE),
                  pct_divorced = mean(divorced_flag, na.rm = TRUE), pct_service = mean(service_flag, na.rm = TRUE),
                  pct_unemployed = mean(unemployed_flag, na.rm = TRUE), pct_homemaker = mean(homemaker_flag, na.rm = TRUE),
                  pct_fee_waived = mean(viable_app_fee_waived_flag, na.rm = TRUE),
                  pct_immed_relative_of_citz = mean(immed_relative_of_citz, na.rm = TRUE), pct_refugee_and_asylee = mean(refugee_and_asylee, na.rm = TRUE),
                  pct_family_sponsored = mean(family_sponsored, na.rm = TRUE), median_years_as_lpr = median(years_as_lpr_at_program_entry, na.rm = TRUE)) %>%
        select(-c(pct_divorced, pct_homemaker, pct_service)) %>% bind_cols(tibble(" " = "Total"), .) %>%
        mutate(n = comma(n),
               pct_female = as_percent(pct_female, digits = 1),
               median_age = as.character(round(median_age, digits = 1)),
               pct_single = as_percent(pct_single, digits = 1), pct_married = as_percent(pct_married, digits = 1),
               # pct_divorced = as_percent(pct_divorced, digits = 1), 
               # pct_service = as_percent(pct_service, digits = 1),
               # pct_homemaker = as_percent(pct_homemaker, digits = 1),
               pct_unemployed = as_percent(pct_unemployed, digits = 1),
               pct_fee_waived = as_percent(pct_fee_waived, digits = 1),
               pct_immed_relative_of_citz = as_percent(pct_immed_relative_of_citz, digits = 1),
               pct_refugee_and_asylee = as_percent(pct_refugee_and_asylee, digits = 1),
               pct_family_sponsored = as_percent(pct_family_sponsored, digits = 1),
               median_years_as_lpr = as.character(round(median_years_as_lpr, digits = 1)))
demographic_total_table


###############



# convert demographics_table to flextable
demographics_table_ft <- demographics_table %>% 
        mutate(n = comma(n),
               pct_female = as_percent(pct_female, digits = 1),
               median_age = as.character(round(median_age, digits = 1)),
               pct_single = as_percent(pct_single, digits = 1), pct_married = as_percent(pct_married, digits = 1),
               # pct_divorced = as_percent(pct_divorced, digits = 1), 
               # pct_service = as_percent(pct_service, digits = 1),
               # pct_homemaker = as_percent(pct_homemaker, digits = 1),
               pct_unemployed = as_percent(pct_unemployed, digits = 1),
               pct_fee_waived = as_percent(pct_fee_waived, digits = 1),
               pct_immed_relative_of_citz = as_percent(pct_immed_relative_of_citz, digits = 1),
               pct_refugee_and_asylee = as_percent(pct_refugee_and_asylee, digits = 1),
               pct_family_sponsored = as_percent(pct_family_sponsored, digits = 1),
               median_years_as_lpr = as.character(round(median_years_as_lpr, digits = 1))) %>%
        bind_rows(demographics_table %>% map_dfc(.x = ., .f = ~ " "), .) %>%
        rename(" " = region_of_birth_at_program_entry) %>%
        bind_rows(demographic_total_table, .) %>%
        mutate(row_number = row_number(), ` ` = case_when(row_number == 2 ~ "Region of birth", TRUE ~ ` `)) %>%
        select(-row_number) %>%
        rename("Number of CAGP participants" = n,
               "Percent female" = pct_female,
               "Median age" = median_age, 
               "Percent single" = pct_single,
               "Percent married" = pct_married, 
               "Percent unemployed" = pct_unemployed, "Percent w/ Form N-400 fee waived" = pct_fee_waived,
               "Percent w/ Immediate relatives of U.S. citizens COA" = pct_immed_relative_of_citz,
               "Percent w/ Refugees and asylees COA" = pct_refugee_and_asylee,
               "Percent w/ Family-sponsored preferences COA" = pct_family_sponsored,
               "Median years in LPR status" = median_years_as_lpr) %>%
        flextable() %>% 
        align(part = "header", i = 1, align = "center") %>%
        align(part = "body", j = 1, align = "left") %>%
        style(part = "header", i = 1, pr_c = fp_cell(background.color = "#005288"),
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8)) %>%
        style(part = "body", pr_t = fp_text(font.size = 8)) %>%
        style(i = seq(from = 4, to = 2 + nrow(demographics_table), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#e6e6e6")) %>%
        style(part = "body", i = 2,
              pr_t = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", font.size = 8),
              pr_c = fp_cell(background.color = "#005288")) %>%
        merge_h_range(i = 2, j1 = 1, j2 = ncol(demographics_table), part = "body") %>%
        add_footer_row(colwidths = ncol(demographics_table), 
                       values = "Source: USCIS Lawful Permanent Resident and naturalization data; CAGP data.") %>%
        add_footer_row(colwidths = ncol(demographics_table), values = "placeholder", top = FALSE) %>%
        compose(i = 2, part = "footer", 
                value = as_paragraph(as_chunk("* ", props = fp_text(font.size = 8, vertical.align = "superscript", color = "#808080",
                                                                    font.family = "Source Sans Pro")),
                                     as_chunk("These columns represent CAGP participants' Class of Admission when granted LPR status.",
                                              props = fp_text(color = "#808080", font.size = 8, font.family = "Source Sans Pro")))) %>%
        compose(part = "header", i = 1, 
                j = which(names(demographics_table) == "pct_immed_relative_of_citz"),
                value = as_paragraph(as_chunk("Percent w/ Immediate relatives of U.S. citizens COA", props = fp_text(color = "#ffffff", bold = FALSE, 
                                                                                                                     font.family = "Source Sans Pro", font.size = 8)),
                                     as_chunk("*", props = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", 
                                                                   font.size = 8, vertical.align = "superscript")))) %>%
        compose(part = "header", i = 1, 
                j = which(names(demographics_table) == "pct_refugee_and_asylee"),
                value = as_paragraph(as_chunk("Percent w/ Refugees and asylees COA", props = fp_text(color = "#ffffff", bold = FALSE, 
                                                                                                     font.family = "Source Sans Pro", font.size = 8)),
                                     as_chunk("*", props = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", 
                                                                   font.size = 8, vertical.align = "superscript")))) %>%
        compose(part = "header", i = 1, 
                j = which(names(demographics_table) == "pct_family_sponsored"),
                value = as_paragraph(as_chunk("Percent w/  Family-sponsored preferences COA", props = fp_text(color = "#ffffff", bold = FALSE, 
                                                                                                              font.family = "Source Sans Pro", font.size = 8)),
                                     as_chunk("*", props = fp_text(color = "#ffffff", bold = FALSE, font.family = "Source Sans Pro", 
                                                                   font.size = 8, vertical.align = "superscript")))) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Source Sans Pro", font.size = 8, color = "#808080")) %>%
        hline_bottom(part = "body", border = fp_border(color = "#808080", width = 2)) %>%
        width(width = c(.7, .75, .55, .5, .55, .55, .75, .65, .65, .65, .7, .5))

demographics_table_ft

# save ft
# demographics_table_ft %>% save_as_docx(path = "output/demographics_table_ft.docx")
