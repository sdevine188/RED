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
library(GGally)


# setwd
setwd("X:/Asylum_EAD_study")

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


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


# read in joined data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


# load income data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
income_raw <- read_excel("data/country_conditions/OGHIST - world_bank_gni_per_capita_historical_income_categories.xls", 
           sheet = "Country Analytical History", range = "B6:AH229")
income_raw


###############


# get income
# note there is an asterix in the spreadsheet (LM*), but only applies to Yemen in 1987/1988 - i handle it in case_when below just to be clean, but can ignore
income <- income_raw %>% slice(6:nrow(.)) %>% rename("country" = "Data for calendar year :") %>% 
        pivot_longer(cols = -country, names_to = "calendar_year", values_to = "income_group") %>%
        mutate(income_group = case_when(income_group == ".." ~ NA_character_, 
                                        income_group == "L" ~ "low_income",
                                        income_group == "LM" ~ "lower_middle_income",
                                        income_group == "LM*" ~ "lower_middle_income",
                                        income_group == "UM" ~ "upper_middle_income",
                                        income_group == "H" ~ "high_income",
                                        TRUE ~ income_group),
               income_group_bucket = case_when(income_group == "low_income" ~ "Low income",
                                               income_group == "lower_middle_income" ~ "Lower middle income",
                                               income_group == "upper_middle_income" ~ "Upper middle income",
                                               income_group == "high_income" ~ "High income"),
               calendar_year = as.numeric(calendar_year))
income
income %>% glimpse()
income %>% miss_var_summary()
# note there are a handful of countries with missing income_group/year combos during our timeframe, but it's negligible 
# (only 9 countries missing any; only 7 are missing > 1)
income %>% filter(is.na(income_group), calendar_year >= 2008) %>% distinct(country)
income %>% filter(is.na(income_group), calendar_year >= 2008) %>% count(country) %>% arrange(desc(n))
income %>% distinct(income_group, income_group_bucket)


#########################################################################################################################################


# get income_group_thresholds
# note the thresholds arent used elsewhere in script or analyses, but just pulled to inspect and confirm gni per capita matches the groupings
income_group_thresholds <- income_raw %>% slice(1:5) %>% 
        rename(income_group = "Data for calendar year :") %>% 
        pivot_longer(cols = -income_group, names_to = "calendar_year", values_to = "income_range")
income_group_thresholds
income_group_thresholds %>% glimpse()
income_group_thresholds %>% distinct(income_group)
income_group_thresholds %>% filter(income_group == "Upper middle income (UM)", calendar_year >= 2008)
income %>% filter(country == "China", calendar_year >= 2008)


####################################################################################################################################


# check country names against citizenship_output_country_name
data %>% anti_join(., income %>% distinct(country), by = c("citizenship_output_country_name" = "country")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.))
income %>% distinct(country) %>% arrange(country) %>% print(n = nrow(.))
income %>% filter(str_detect(string = country, pattern = "Syria")) %>% distinct(country)


#####################


# clean country names
# note that the world bank income spreadsheet has info in separate secton at bottom on former countries like USSR, Yugoslavia, Czechloslovakia
# but because these countries dissolved, there is no recent income data for them, only the new countries that resulted
# so in I-589 data where citizenship is listed as a former country, i just assign a resulting country, but don't know for sure if it's accurate
# checking some examples though (e.g. serbia and montenegro, czech republic and slovakia, etc) they appear to have very similar income groups in study period
income <- income %>% mutate(country_clean = case_when(country == "Egypt, Arab Rep." ~ "Egypt",
                                            country == "Iran, Islamic Rep." ~ "Iran",
                                            country == "Venezuela, RB" ~ "Venezuela",
                                            country == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
                                            country == "Syria" ~ "Syrian Arab Republic",
                                            country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                            country == "Russian Federation" ~ "Russia",
                                            country == "Bahamas, The" ~ "Bahamas",
                                            country == "Gambia, The" ~ "Gambia",
                                            country == "Congo, Rep." ~ "Congo (Brazzaville)",
                                            country == "Myanmar" ~ "Burma",
                                            country == "Taiwan, China" ~ "Taiwan",
                                            country == "Korea, Dem. Rep." ~ "North Korea",
                                            country == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                            country == "St. Lucia" ~ "Saint Lucia",
                                            country == "Lao PDR" ~ "Laos",
                                            country == "Syrian Arab Republic" ~ "Syria",
                                            country == "Yemen, Rep." ~ "Yemen",
                                            country == "Korea, Rep." ~ "South Korea",
                                            country == "Czech Republic" ~ "Czechia",
                                            country == "North Macedonia" ~ "Macedonia",
                                            country == "Slovak Republic" ~ "Slovakia",
                                            country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                            country == "Hong Kong SAR, China" ~ "Hong Kong",
                                            country == "Czech Republic" ~ "Czechia",
                                            country == "Brunei Darussalam" ~ "Brunei",
                                            country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                            country == "Eswatini" ~ "Swaziland",
                                            country == "São Tomé and Principe" ~ "Sao Tome and Principe",
                                            TRUE ~ country))


##############


# inspect
# note that will need to make changes to data for income merge for serbia & montenegro, soviet union, and czechoslovakia
# also note that stateless and unknown citizenship in data will obviously get no income group
data %>% anti_join(., income %>% distinct(country_clean), by = c("citizenship_output_country_name" = "country_clean")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.))
data %>% filter(citizenship_output_country_name %in% c("Stateless", "Unknown")) %>% nrow() # 2128

# check anti_join with country_clean after cleaning data for former countries
# result: all good, the only unmatched are Stateless and Unknown
data %>% mutate(citizenship_output_country_name_for_income_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        anti_join(., income %>% distinct(country_clean, calendar_year), 
                  by = c("citizenship_output_country_name_for_income_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>%
        distinct(citizenship_output_country_name)


##################


# get inspect_data_w_income_group
inspect_data_w_income_group <- data %>% mutate(citizenship_output_country_name_for_income_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        left_join(., income %>% distinct(country_clean, calendar_year, income_group), 
                  by = c("citizenship_output_country_name" = "country_clean", "filing_date_cy" = "calendar_year"))

inspect_data_w_income_group %>% nrow() # 672403
inspect_data_w_income_group %>% distinct(citizenship_output_country_name, filing_date_cy, income_group) %>% 
        filter(citizenship_output_country_name == "China") %>% arrange(filing_date_cy)
income %>% filter(country == "China", calendar_year >= 2008)

# note there are 68 countries with multiple income groups over study period, with the max number of income_groups being 3
# when filtered down to top 10 I-589 countries, only China, Ecuador, and Guatamala have 2 income_groups
# and for these three countries, there is a clear majority of years in one income_group, 
# so for overall charts they can easily be bucketed without avg underlying income levels
income %>% filter(calendar_year >= 2008) %>% distinct(country, income_group) %>% count(country) %>% arrange(desc(n)) %>% print(n = 20)
income %>% filter(calendar_year >= 2008) %>% distinct(country, income_group) %>% count(country) %>% filter(n > 1) %>% nrow() # 68
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., income, by = c("citizenship_output_country_name" = "country")) %>%
        filter(calendar_year >= 2008) %>% distinct(citizenship_output_country_name, income_group) %>% 
        count(citizenship_output_country_name) %>% arrange(desc(n)) %>% print(n = 20)
income %>% filter(country %in% c("China", "Guatemala", "Ecuador")) %>% filter(calendar_year >= 2008) %>% print(n = nrow(.))


####################################################################################################################################


# load gni_raw
gni_raw <- read_excel("data/country_conditions/API_NY.GNP.PCAP.CD_DS2_en_excel_v2_1120986.xls", sheet = "Data", range = "A4:BK268")
gni_raw

# pivot_longer
gni <- gni_raw %>% rename(country = `Country Name`) %>% select(-c(`Country Code`, `Indicator Name`, `Indicator Code`)) %>%
        pivot_longer(cols = -country, names_to = "calendar_year", values_to = "gni_per_capita") %>%
        mutate(calendar_year = as.numeric(calendar_year))


###############


# inspect
# note 39 countries have NA gni_per_capita in study window, but nearly all are understandably small or peripheral countries
gni %>% filter(calendar_year >= 2008) %>% miss_var_summary()
gni %>% filter(calendar_year >= 2008, is.na(gni_per_capita)) %>% count(country) %>% arrange(desc(n)) %>% print(n = nrow(.))


####################################################################################################################################


# check names for joining gni to income
# will clean 5 of the 6 mismatches in join code below, but note that gni has no reference to taiwan; but it's only 50 I-589s, so can ignore
income %>% nrow() # 6976
income %>% anti_join(., gni, by = "country") %>% distinct(country)
gni %>% filter(str_detect(string = country, pattern = "Korea")) %>% distinct(country)
data %>% filter(citizenship_output_country_name == "Taiwan") %>% nrow() # 50

# check after cleaning
# results only taiwan remains un-matched
gni %>% mutate(country = case_when(country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                                   country == "Curacao" ~ "Curaçao",
                                   country == "Faroe Islands" ~ "Faeroe Islands",
                                   country == "Sao Tome and Principe" ~ "São Tomé and Principe",
                                   country == "Korea, Dem. People’s Rep." ~ "Korea, Dem. Rep.",
                                   TRUE ~ country)) %>% 
        anti_join(income, ., by = "country") %>% distinct(country)


######################



# join gni to income
income <- gni %>% mutate(country = case_when(country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                                   country == "Curacao" ~ "Curaçao",
                                   country == "Faroe Islands" ~ "Faeroe Islands",
                                   country == "Sao Tome and Principe" ~ "São Tomé and Principe",
                                   country == "Korea, Dem. People’s Rep." ~ "Korea, Dem. Rep.",
                                   TRUE ~ country)) %>%
        left_join(income, ., by = c("country", "calendar_year"))


######################


# inspect
income %>% filter(calendar_year >= 2008) %>% miss_var_summary()
income %>% filter(country == "China", calendar_year >= 2008)
income %>% nrow() # 6976


####################################################################################################################################


# save income
income %>% write_csv("data/country_conditions/income_20200531.csv")


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


# load homicide data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
homicide_raw <- read_excel("data/country_conditions/homicide_total_rate_and_count.xlsx")

# inspect
homicide_raw
homicide_raw %>% distinct(Level)
homicide_raw %>% distinct(Indicator)


###############


# pivot count/rate to be seperate columns, so they can both be joined to data on a single row
homicide <- homicide_raw %>% mutate(Indicator = case_when(Indicator == "Homicide count" ~ "homicide_count",
                                          Indicator == "Homicide rate" ~ "homicide_rate_per_100k"),
                                    Value = as.numeric(Value)) %>%
        pivot_wider(names_from = "Indicator", values_from = "Value") %>%
        rename(country = Territory, calendar_year = Year) %>% select(country, calendar_year, homicide_count, homicide_rate_per_100k)


###############


# inspect
homicide
homicide %>% miss_var_summary()
homicide %>% arrange(desc(homicide_rate)) %>% print(n = 30)
# note that small countries like grenada/dominica can have high homicide rates with very low homicide counts
# shouldn't be an issue for analysis though, which will just bucket countries
homicide %>% filter(calendar_year >= 2008) %>% arrange(desc(homicide_rate)) %>% print(n = 30)
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., homicide, by = c("citizenship_output_country_name" = "country")) %>% 
        filter(calendar_year >= 2008) %>% print(n = nrow(.))


###########################################################################################################################


# check country names against citizenship_output_country_name
data %>% anti_join(., homicide %>% distinct(country), by = c("citizenship_output_country_name" = "country")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.))
homicide %>% distinct(country) %>% arrange(country) %>% print(n = nrow(.))
homicide %>% filter(str_detect(string = country, pattern = "Viet")) %>% distinct(country)


#################


# add country_clean
homicide <- homicide %>% mutate(country_clean = case_when(country == "Viet Nam" ~ "Vietnam",
                                              country == "Iran (Islamic Republic of)" ~ "Iran",
                                              country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                              country == "Syrian Arab Republic" ~ "Syria",
                                              country == "Russian Federation" ~ "Russia",
                                              country == "Republic of Moldova" ~ "Moldova",
                                              country == "Myanmar" ~ "Burma",
                                              country == "China, Taiwan Province of China" ~ "Taiwan",
                                              country == "Republic of Korea" ~ "South Korea",
                                              country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                              country == "United Republic of Tanzania" ~ "Tanzania",
                                              country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                                              country == "Republic of North Macedonia" ~ "Macedonia",
                                              country == "Kosovo under UNSCR 1244" ~ "Kosovo",
                                              country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong",
                                              country == "Brunei Darussalam" ~ "Brunei",
                                              country == "Eswatini" ~ "Swaziland", 
                                              TRUE ~ country))


#################


# note when filtering homicide data down to 2008-2018, homicide data does not include 26 - 3 (former countries) = 23 countries found in i589 data
# note that data will need to create citizenship_output_country_name_for_homicide w/ clean serbia and montenegro, czechoslovakia, soviet union
data %>% anti_join(., homicide %>% filter(calendar_year >= 2008) %>% distinct(country_clean), by = c("citizenship_output_country_name" = "country_clean")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.)) # 26
# note that 23168 anumbers (3.4%) are from african or other countries that UN does not have homicide data for - can't be helped...
data %>% anti_join(., homicide %>% distinct(country_clean), by = c("citizenship_output_country_name" = "country_clean")) %>%
        filter(!(citizenship_output_country_name %in% c("Serbia and Montenegro (former)",
                                                        "Czechoslovakia (former)",
                                                        "Soviet Union (former)"))) %>% nrow() # 23168
23168 / 672403 # 3.4%
# note that over all countries in homicide data (more than in i589 data), 123 of 205 countries dont have all 2008-2018 study window years (11 years); 
# of countries in i589 data, 50% have all 11 years; 75% have 6 years or more - not perfect, but workable
# note though that all the top 10 countries have homicide data; 7 have all 11 years, 2 have 10 years, and 1 (egypt) has only 5 years (stopped at 2012 w/ protests)
homicide %>% distinct(country_clean, calendar_year) %>% filter(country_clean == "United States of America", calendar_year >= 2008) %>% print(n = nrow(.)) # 11
homicide %>% distinct(country_clean, calendar_year) %>% filter(calendar_year >= 2008) %>% count(country_clean) %>% 
        left_join(data %>% distinct(citizenship_output_country_name), ., by = c("citizenship_output_country_name" = "country_clean")) %>%
        arrange(n) %>% print(n = nrow(.))
homicide %>% distinct(country_clean, calendar_year) %>% filter(calendar_year >= 2008) %>% count(country_clean) %>%
        left_join(data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>% select(citizenship_output_country_name), .,
                  by = c("citizenship_output_country_name" = "country_clean"))
homicide %>% distinct(country_clean, calendar_year) %>% filter(country_clean == "Egypt") # stopped at 2012 w/ protests
homicide %>% distinct(country_clean, calendar_year) %>% filter(calendar_year >= 2008) %>% count(country_clean) %>%
        left_join(data %>% distinct(citizenship_output_country_name), ., by = c("citizenship_output_country_name" = "country_clean")) %>%
        ggplot(data = ., aes(x = n)) + geom_histogram(binwidth = 1)
homicide %>% distinct(country_clean, calendar_year) %>% filter(calendar_year >= 2008) %>% count(country_clean) %>%
        left_join(data %>% distinct(citizenship_output_country_name), ., by = c("citizenship_output_country_name" = "country_clean")) %>%
        ggplot(data = ., aes(x = n)) + stat_ecdf()

# check anti_join with country_clean after cleaning data for former countries
# result: 107 countries/calendar_year combos in i589 data dont have match in homicide
# 23 of the 107 have no homicide data for cy 2008-2018; the rest have some data, but not for all years
data %>% mutate(citizenship_output_country_name_for_homicide_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                              citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                              citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                              TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        anti_join(., homicide %>% distinct(country_clean, calendar_year), 
                  by = c("citizenship_output_country_name_for_homicide_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>%
        distinct(citizenship_output_country_name) %>% nrow() # 107

# note that most of the missing country/cy combos are for smaller i589 volume countries
data %>% mutate(citizenship_output_country_name_for_homicide_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        anti_join(., homicide %>% distinct(country_clean, calendar_year), 
                  by = c("citizenship_output_country_name_for_homicide_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>%
        count(citizenship_output_country_name, filing_date_cy) %>% arrange(citizenship_output_country_name, filing_date_cy) %>% print(n = nrow(.))

# there are 96155 i589 records (14%) with missing homicide country/cy combos 
data %>% mutate(citizenship_output_country_name_for_homicide_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                              citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                              citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                              TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        anti_join(., homicide %>% distinct(country_clean, calendar_year), 
                  by = c("citizenship_output_country_name_for_homicide_join" = "country_clean", "filing_date_cy" = "calendar_year")) %>% nrow() # 96155
96155 / 672403 # 14%


###################


# get inspect_data_w_homicide 
inspect_data_w_homicide <- data %>% mutate(citizenship_output_country_name_for_income_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        left_join(., homicide %>% distinct(country_clean, calendar_year, homicide_count, homicide_rate), 
                  by = c("citizenship_output_country_name" = "country_clean", "filing_date_cy" = "calendar_year"))

inspect_data_w_homicide %>% nrow() # 672403
inspect_data_w_homicide %>% glimpse()
inspect_data_w_homicide %>% distinct(citizenship_output_country_name, filing_date_cy, homicide_count, homicide_rate) %>% 
        filter(citizenship_output_country_name == "China") %>% arrange(filing_date_cy)
homicide %>% filter(country == "China", calendar_year >= 2008)


############################################################################################################################


# inspect to get homicide_bucket
# will apply homicide_rate_per_100k buckets used by UN in 2019 homicide report here: https://dataunodc.un.org/content/data/homicide/homicide-rate
homicide %>% filter(homicide_count > 1000) %>% ggplot(data = ., aes(x = homicide_rate_per_100k)) + geom_histogram()
homicide %>% filter(homicide_count > 1000) %>% ggplot(data = ., aes(x = homicide_rate_per_100k)) + stat_ecdf()


####################


# get homicide_bucket
homicide <- homicide %>% mutate(homicide_bucket = case_when(round(homicide_rate_per_100k, digits = 1) <= 1 ~ "\u2264 1",
                                                round(homicide_rate_per_100k, digits = 1) > 1 & round(homicide_rate_per_100k, digits = 1) <= 10 ~ "1.1 to 10",
                                                round(homicide_rate_per_100k, digits = 1) > 10 & round(homicide_rate_per_100k, digits = 1) <= 20 ~ "10.1 to 20",
                                                round(homicide_rate_per_100k, digits = 1) > 20 & round(homicide_rate_per_100k, digits = 1) <= 40 ~ "20.1 to 40",
                                                round(homicide_rate_per_100k, digits = 1) > 40 ~ "> 40"))


####################


# inspect
homicide
homicide %>% distinct(homicide_rate_per_100k, homicide_bucket)
homicide %>% distinct(homicide_bucket)
homicide %>% filter(country_clean == "China")


############################################################################################################################


# save homicide
homicide %>% write_csv("data/country_conditions/homicide_20200531.csv")


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# load freedom data (political rights / civil liberties) 
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
freedom_raw <- read_excel("data/country_conditions/2020_Aggregate_Category_and_Subcategory_Scores_FIW_2003-2020.xlsx", sheet = "FIW06-20", range = "A1:S3130")
freedom_raw
freedom_raw %>% glimpse()

# get freedom
# note that Index tab of worksheet explains how to interpret Edition variables as survey year
# "This workbook lists all publicly available data by survey year (e.g. survey FIW2003 analyzes calendar year 2002)."
# and the tab we pull data from is titled "FIW06-20, with the Edition variable ranging from 2006 - 2020
# so FIW2006 analyzes calendar year 2005 etc; so for our timeframe FY 2009-2018, we want FIW2010-FIW2019
freedom <- freedom_raw %>% rename(total_prcl_score = Total, country = `Country/Territory`, report_edition_year = Edition,
                                  pr_rating = `PR Rating`, cl_rating = `CL Rating`, pr_score = PR, cl_score = CL, freedom_status = Status) %>%
        mutate(survey_calendar_year = report_edition_year - 1) %>%
        select(country, report_edition_year, survey_calendar_year, pr_rating, pr_score, cl_rating, cl_score, total_prcl_score, freedom_status)
freedom


####################################################################################################################################


# check country names against citizenship_output_country_name
data %>% anti_join(., freedom %>% distinct(country), by = c("citizenship_output_country_name" = "country")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.))
freedom %>% distinct(country) %>% arrange(country) %>% print(n = nrow(.))
freedom %>% filter(str_detect(string = country, pattern = "Vincent")) %>% distinct(country)

# note also that freedom data lists Cape Verde for all years except 2020 edition/2019 survey, which lists the new Cabo Verde
# this didn't come up in anti_join check above because there is at least one instance of cabo_verde in freedom data
# will convert freedom data to be all cabo verde, to match data which is all cabo verde
freedom %>% filter(str_detect(string = country_clean, pattern = "Verde"))
data %>% filter(str_detect(string = citizenship_output_country_name, pattern = "Verde")) %>% distinct(citizenship_output_country_name)


#####################


# clean country names
# note that some countries dissolved in past, like USSR, Yugoslavia, Czechloslovakia,
# but because these countries dissolved, there is no recent income data for them, only the new countries that resulted
# so in I-589 data where citizenship is listed as a former country, i just assign a resulting country, but don't know for sure if it's accurate
# checking some examples though (e.g. serbia and montenegro, czech republic and slovakia, etc) they appear to have very similar income groups in study period
freedom <- freedom %>% mutate(country_clean = case_when(country == "The Gambia" ~ "Gambia",
                                                        country == "Myanmar" ~ "Burma",
                                                        country == "St. Lucia" ~ "Saint Lucia",
                                                        country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                                                        country == "Czech Republic" ~ "Czechia",
                                                        country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                                        country == "Cape Verde" ~ "Cabo Verde",
                                                        TRUE ~ country))
                                                        

#####################


# inspect
# note that will need to make changes to data for income merge for serbia & montenegro, soviet union, and czechoslovakia
# also note that stateless and unknown citizenship in data will obviously get no income group
data %>% anti_join(., freedom %>% distinct(country_clean), by = c("citizenship_output_country_name" = "country_clean")) %>%
        distinct(citizenship_output_country_name) %>% print(n = nrow(.))
data %>% filter(citizenship_output_country_name %in% c("Stateless", "Unknown")) %>% nrow() # 2128

# check anti_join with country_clean after cleaning data for former countries
# result: all good, the only unmatched are Stateless and Unknown
data %>% mutate(citizenship_output_country_name_for_freedom_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                            TRUE ~ citizenship_output_country_name),
                filing_date_cy = year(filing_date)) %>%
        anti_join(., freedom %>% distinct(country_clean, survey_calendar_year), 
                  by = c("citizenship_output_country_name_for_freedom_join" = "country_clean", "filing_date_cy" = "survey_calendar_year")) %>%
        distinct(citizenship_output_country_name)


#######################


# get inspect_data_w_freedom
inspect_data_w_freedom <- data %>% mutate(citizenship_output_country_name_for_income_join = case_when(citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Serbia",
                                                                                                           citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                                                                                           citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                                                                                           TRUE ~ citizenship_output_country_name),
                                               filing_date_cy = year(filing_date)) %>%
        left_join(., freedom %>% distinct(country_clean, survey_calendar_year, total_prcl_score), 
                  by = c("citizenship_output_country_name" = "country_clean", "filing_date_cy" = "survey_calendar_year"))

inspect_data_w_freedom  %>% nrow() # 672403
inspect_data_w_freedom  %>% distinct(citizenship_output_country_name, filing_date_cy, total_prcl_score) %>% 
        filter(citizenship_output_country_name == "China") %>% arrange(filing_date_cy)
freedom %>% filter(country == "China", survey_calendar_year >= 2008) %>% arrange(survey_calendar_year)


##########################


# inspect
# note that max total_prcl_score is 100, and ecdf shows it's roughly equal distribution
# so will make decision to bucket countries into <25 prcl, 26 - 50, 51-75, 76-100 
freedom %>% ggplot(data = ., aes(x = total_prcl_score)) + geom_histogram()
freedom %>% ggplot(data = ., aes(x = total_prcl_score)) + stat_ecdf()
freedom %>% arrange(desc(total_prcl_score))
freedom %>% filter(total_prcl_score > 90) %>% distinct(country_clean) %>% print(n = nrow(.))
freedom %>% arrange(total_prcl_score)
freedom %>% filter(total_prcl_score < 10) %>% distinct(country_clean) %>% print(n = nrow(.))

# check top 10 countries
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., freedom, by = c("citizenship_output_country_name" = "country_clean")) %>%
        distinct(country, freedom_status)

data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., freedom, by = c("citizenship_output_country_name" = "country_clean")) %>%
        filter(survey_calendar_year >= 2008, survey_calendar_year <= 2018) %>%
        ggplot(data = ., aes(x = survey_calendar_year, y = total_prcl_score)) + geom_line() + facet_grid(rows = vars(country))


#####################


# add prcl_bucket 
freedom <- freedom %>% mutate(prcl_bucket = case_when(total_prcl_score < 25 ~ "< 25",
                                                         total_prcl_score >= 25 & total_prcl_score < 50 ~ "25 to 49",
                                                         total_prcl_score >= 50 & total_prcl_score < 75 ~ "50 to 74",
                                                         total_prcl_score >= 75 ~ "75 to 100"))


#####################


# inspect
freedom
freedom %>% distinct(prcl_bucket)
freedom %>% filter(country_clean == "China") %>% select(country, report_edition_year, total_prcl_score, prcl_bucket)


#####################

# save freedom
freedom %>% write_csv("data/country_conditions/freedom_20200531.csv")


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# read in joined data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect relationships between country conditions


##########################


# correlations
data %>% distinct(filing_date_cy, gni_per_capita, homicide_rate_per_100k, total_prcl_score) %>% 
        select(-filing_date_cy) %>% ggpairs()
data %>% distinct(filing_date_cy, outcome_bucket, gni_per_capita, homicide_rate_per_100k, total_prcl_score) %>% 
        select(-filing_date_cy) %>% ggpairs(mapping = aes(color = outcome_bucket))
data %>% distinct(filing_date_cy, eoir_outcome, gni_per_capita, homicide_rate_per_100k, total_prcl_score) %>% 
        select(-filing_date_cy) %>% ggpairs(mapping = aes(color = eoir_outcome))


######################


# distributions
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name) %>%
        summarize(filing_count = n(), 
                  avg_gni_per_capita = mean(gni_per_capita, na.rm = TRUE),
                  avg_homicide_rate = mean(homicide_rate_per_100k, na.rm = TRUE), 
                  avg_prcl_score = mean(total_prcl_score, na.rm = TRUE))
# all top 10 countries are normalish on income; india, honduras, egypt, and haiti are low
data %>% group_by(citizenship_output_country_name) %>%
        summarize(avg_gni_per_capita = mean(gni_per_capita, na.rm = TRUE)) %>%
        ggplot(data = ., aes(x = avg_gni_per_capita)) + geom_histogram()
# note that mexico, guatemala, honduras, venezuela, and el salvador are outliers in homicide
data %>% group_by(citizenship_output_country_name) %>%
        summarize(avg_homicide_rate = mean(homicide_rate_per_100k, na.rm = TRUE)) %>%
        ggplot(data = ., aes(x = avg_homicide_rate)) + geom_histogram()
# china is really low on prcl; egypt and venezuela are low too
data %>% group_by(citizenship_output_country_name) %>%
        summarize(avg_prcl_score = mean(total_prcl_score, na.rm = TRUE)) %>%
        ggplot(data = ., aes(x = avg_prcl_score)) + geom_histogram()


#############################


# scatter
# all countries
data %>% group_by(citizenship_output_country_name) %>%
        summarize(filing_count = n(), 
                  avg_gni_per_capita = mean(gni_per_capita, na.rm = TRUE),
                  avg_homicide_rate = mean(homicide_rate_per_100k, na.rm = TRUE), 
                  avg_prcl_score = mean(total_prcl_score, na.rm = TRUE)) %>%
        ggplot(data = ., aes(x = avg_homicide_rate, y = avg_gni_per_capita, size = avg_prcl_score)) +
        geom_point(alpha = 1/10) + scale_size(range = c(0, 20))

# top 10 filing coutnries
data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name) %>%
        summarize(filing_count = n(), 
                  avg_gni_per_capita = mean(gni_per_capita, na.rm = TRUE),
                  avg_homicide_rate = mean(homicide_rate_per_100k, na.rm = TRUE), 
                  avg_prcl_score = mean(total_prcl_score, na.rm = TRUE)) %>%
        ggplot(data = ., aes(x = avg_homicide_rate, y = avg_gni_per_capita, size = avg_prcl_score, 
                             label = citizenship_output_country_name)) +
        geom_point(alpha = 1/10) + geom_text() + scale_size(range = c(0, 20))


