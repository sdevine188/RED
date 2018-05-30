library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(scales)
library(readxl)
library(purrr)
library(ggplot2)



# how to update table to show only charges that occured prior to most recent approval, instead of all charges for anumbers with arrest prior to most recent approval
# in merge_daca_files, use daca_charges as a lookup table
# run function that takes the latest approval date for every anum (which should be in c3_elis3 already), and filters daca_charges to that anum and only charges prior to date
# then use those charge types to populate a the dummy charge type matrix that shows only charges prior to most recent approval


# turn off scientific notation
options(scipen=999)

# setwd
setwd("H:/RED/DACA")

# as_percent function
source("as_percent.R")

# read in c3_elis3
c3_elis3 <- read_csv("Data/c3_elis3_20180503.csv")
glimpse(c3_elis3) # 3,369,431
c3_elis3 %>% distinct(a_number) %>% count() # 888765

# read in reid's file flagging current approvals (excluding expired, which he manually calculated), and those with arrests prior to approval
ia_file <- read_csv("Data/DACAFLAGS_02MAY2018.csv")
ia_file <- ia_file %>% mutate(ia_file_flag = 1)

# inspect ia_file
glimpse(ia_file) # 888765
ia_file %>% distinct(anum) %>% count() # 888765
ia_file %>% group_by(statall) %>% count() %>% arrange(desc(n))
ia_file %>% mutate(last_character = str_sub(statall, start = nchar(statall))) %>% distinct(last_character)
ia_file %>% count(pop345a) # 53792

# old code
# read in reid's IA file showing anumbers with pattern ...Arrest...Approval-end
# ia_file <- read_csv("Data/IA_file_request.csv")
# ia_file <- ia_file %>% mutate(ia_file_flag = 1)

# old code
# inspect ia_file
# glimpse(ia_file) # 51972
# ia_file %>% distinct(anum) %>% count() # 51972
# ia_file %>% group_by(statall) %>% count() %>% arrange(desc(n))
# ia_file %>% mutate(last_character = str_sub(statall, start = nchar(statall))) %>% distinct(last_character)


######################################################################


# merge c3_elis3 with ia_file and filter to just ia records
c3_elis3_ia_only <- c3_elis3 %>% left_join(., ia_file, by = c("a_number" = "anum")) 
glimpse(c3_elis3_ia_only) # 3,369,431

# filter to just Pop345a = 1
c3_elis3_ia_only %>% distinct(a_number, appr) %>% count(appr)
c3_elis3_ia_only %>% distinct(a_number, ApriorI) %>% count(ApriorI)
c3_elis3_ia_only %>% distinct(a_number, pop345a) %>% count(pop345a)

c3_elis3_ia_only <- c3_elis3_ia_only %>% filter(pop345a == 1)
glimpse(c3_elis3_ia_only) # 205,315
c3_elis3_ia_only %>% distinct(a_number) %>% nrow() # 53792

# old code
# filter to just ia_file_flag = 1
# c3_elis3_ia_only <- c3_elis3_ia_only %>% filter(ia_file_flag == 1)
# c3_elis3_ia_only %>% distinct(a_number) %>% count() # 51972
# glimpse(c3_elis3_ia_only) # 200435



######################################################################


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


###############################################################################


# read in daca_charges
daca_charges <- read_csv("Data/daca_charges_20180503.csv")
glimpse(daca_charges) # 1,325,969
daca_charges %>% distinct(anum) %>% count() # 101558

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




# inspect charge_types
daca_charges %>% filter(anum %in% c3_elis3_ia_only$a_number & charge_type == "arson") %>% 
        group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame()

daca_charges %>% filter(anum %in% c3_elis3_ia_only$a_number & charge_type == "rape") %>% 
        select(anum, keydate, keyline, charge_type) %>% data.frame()

daca_raw %>% filter(a_number == "A088786527") %>% do(cat(.$ident_report)) 
daca_raw %>% filter(a_number == "A200551269") %>% write_csv(., "daca_ident_report.csv")

# homicide_murder_att_murder
# robbery
# rape
# arson

###############################################################################


# create table of anumber count by charge type for IA only

# get a_number counts for denominators in sub-populations of interest
count_daca_charge_flag_a_numbers <- c3_elis3_ia_only %>% filter(daca_charge_flag == 1) %>% distinct(a_number) %>% count() %>% pull(n)
count_ever_denied_a_numbers <- c3_elis3_ia_only %>% filter(benefit_status == "Denied") %>% distinct(a_number) %>% count() %>% pull(n)
# count_ever_smart_terminated_a_numbers <- c3_elis3_ia_only %>% filter(ever_smart_terminated == 1) %>% distinct(a_number) %>% count() %>% pull(n)
# count_scops_terminated_a_numbers <- c3_elis3_ia_only %>% filter(scops_terminated == 1) %>% distinct(a_number) %>% count() %>% pull(n)
count_all_daca_a_numbers <- c3_elis3 %>% distinct(a_number) %>% count() %>% pull(n)

# # build a_number_charge_table
# a_number_charge_table <- c3_elis3_ia_only %>% filter(daca_charge_flag == 1) %>% select(a_number, charge_type_variables) %>%
#         gather(key = charge_type, value = value, c(charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
#         filter(value == 1)
# a_number_charge_table

# build ident_charge_count_table
ident_charge_count_table <- c3_elis3_ia_only %>% 
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
               `Undefined` = undefined_charge_literal) %>%
        gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
        filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()

# build count_pct table for ident
ident_count_pct <- ident_charge_count_table %>% mutate(count_ident = n, pct_ident = count_ident / count_daca_charge_flag_a_numbers) %>%
        select(-n)
ident_count_pct
dim(ident_count_pct)

# build smart_charge_count_table
# smart_charge_count_table <- c3_elis3_ia_only %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
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
#         rename(Murder = murder, Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang, `Organized criminal activity` = organized_criminal_activity,
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

# build denial_charge_count_table
denial_charge_count_table <- c3_elis3_ia_only %>% 
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
               `Undefined` = undefined_charge_literal) %>%
        filter(daca_charge_flag == 1, benefit_status == "Denied") %>% 
        select(a_number, collapsed_charge_type_variables) %>%
        gather(key = charge_type, value = value, c(collapsed_charge_type_variables)) %>% distinct(a_number, charge_type, value) %>%
        filter(value == 1) %>% group_by(charge_type) %>% count() %>% ungroup()

# build count_pct table for denial
denial_count_pct <- denial_charge_count_table %>% mutate(count_denial = n, pct_denial = count_denial / count_ever_denied_a_numbers) %>%
        select(-n)
denial_count_pct
dim(denial_count_pct)

# build scops_charge_count_table
# scops_charge_count_table <- c3_elis3_ia_only %>% 
#         mutate(`Accessory, accomplice, hindering, etc` = ifelse(accessory_hindering_accomplice_after_fact == 1, 1, 0),
#                `Contempt, bench warrant, bail, etc` = case_when(bail == 1 | contempt_bench_warrant == 1 ~ 1, TRUE ~ 0),
#                `Battery` = case_when(battery == 1 | fighting == 1 | mayhem == 1 ~ 1, TRUE ~ 0), 
#                `Offenses against family and children` = case_when(child_abuse == 1 | domestic_violence == 1 |
#                                                                           inflict_corporeal_injury == 1 ~ 1, TRUE ~ 0), 
#                `Other` = case_when(cruelty_to_animals == 1 | dog_fighting == 1 | 
#                                            electronic_communication_device == 1 ~ 1, TRUE ~ 0),
#                `Kidnapping, trafficking, false imprisonment` = case_when(false_imprisonment == 1 | kidnap == 1 | unlawful_restraint == 1 |
#                                                                                  human_trafficking == 1 ~ 1, TRUE ~ 0),
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
#         rename(Murder = murder, Arson = arson, Assault = assault, Burglary = burglary, `Child pornography` = child_porn,
#                `Contributing to the delinquency of a minor` = contrib_delinquency_of_minor, 
#                Vandalism = vandal, `Immigration-related` = deport_alien_nonimmigrant_overstay_ewi_removal, 
#                `Disorderly conduct` = disturb_breach_of_peace_disorderly_noise, `Drug-related` = drug, 
#                `Driving under the influence` = DUI, Embezzlement = embezzlement, `Failure to appear` = `fail to appear`,
#                `Fail to comply/obey, etc` = fail_to_comply_obey, `Forgery, counterfeiting, etc` = forge_counterfeit,  
#                `Obstruction, fabrication, false claim, etc` = false_report_information_obstruct_false_id, 
#                Gambling = gambling, Gang = gang, `Organized criminal activity` = organized_criminal_activity,
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

# build count_pct table for all_daca
all_daca_count_pct <- ident_charge_count_table %>% mutate(count_ident = n, pct_all_daca = count_ident / count_all_daca_a_numbers) %>%
        select(-c(n, count_ident))
all_daca_count_pct
dim(all_daca_count_pct)

# compile into single table
daca_charges_count_pct_table <- ident_count_pct %>% left_join(., denial_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- ident_count_pct %>% left_join(., smart_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., scops_count_pct, by = c("charge_type" = "charge_type"))
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., denial_count_pct, by = c("charge_type" = "charge_type"))
daca_charges_count_pct_table <- daca_charges_count_pct_table %>% left_join(., all_daca_count_pct, by = c("charge_type" = "charge_type"))
daca_charges_count_pct_table

# build totals row
daca_charges_count_pct_totals_row <- data.frame(charge_type = "Totals", count_ident = count_daca_charge_flag_a_numbers,
                                                pct_ident = count_daca_charge_flag_a_numbers / count_daca_charge_flag_a_numbers,
                                                # count_smart = count_ever_smart_terminated_a_numbers,
                                                # pct_smart = count_ever_smart_terminated_a_numbers / count_ever_smart_terminated_a_numbers,
                                                # count_scops = count_scops_terminated_a_numbers,
                                                # pct_scops = count_scops_terminated_a_numbers / count_scops_terminated_a_numbers,
                                                count_denial = count_ever_denied_a_numbers,
                                                pct_denial = count_ever_denied_a_numbers / count_ever_denied_a_numbers,
                                                pct_all_daca = count_daca_charge_flag_a_numbers / count_all_daca_a_numbers)
glimpse(daca_charges_count_pct_totals_row) 

# add daca_charges_count_pct_totals_row 
daca_charges_count_pct_table <- rbind(daca_charges_count_pct_totals_row, daca_charges_count_pct_table)
daca_charges_count_pct_table %>% data.frame()

# remove health_and_safety charge because it's a court statement for drug charge
# daca_raw %>% filter(a_number == "A204424162") %>% do(cat(.$ident_report)) # in part 1
# daca_charges_count_pct_table <- daca_charges_count_pct_table %>% 
#         filter(!(charge_type %in% c("health_and_safety", "keyline_does_not_list_specific_charge")))
# glimpse(daca_charges_count_pct_table)

# inspect join
# daca_charges_count_pct_table %>% filter(is.na(pct_smart))  

# convert NA to zero
daca_charges_count_pct_table <- daca_charges_count_pct_table %>% mutate_at(.vars = vars(-charge_type), .funs = ~ ifelse(is.na(.x), 0, .x))
daca_charges_count_pct_table

# clean table
daca_charges_count_pct_table_clean <- daca_charges_count_pct_table %>% 
        # rename(count_smart_terminated = count_smart, pct_smart_terminated = pct_smart, 
        #                 count_scops_terminated = count_scops, pct_scops_terminated = pct_scops) %>%
        arrange(desc(count_ident)) %>%
        mutate(count_ident = comma(count_ident), pct_ident = as_percent(pct_ident), 
               # count_smart_terminated = comma(count_smart_terminated), pct_smart_terminated = as_percent(pct_smart_terminated),
               # count_scops_terminated = comma(count_scops_terminated), pct_scops_terminated = as_percent(pct_scops_terminated),
               count_denial = comma(count_denial), pct_denial = as_percent(pct_denial),
               pct_all_daca = as_percent(pct_all_daca)) %>% 
        # on the fly change of charge type from "Other driving-related" to "Driving-related (excl. DUI)"
        mutate(charge_type = case_when(charge_type == "Other driving-related" ~ "Driving-related (excl. DUI)",                
                                       TRUE ~ as.character(charge_type))) %>%
        rename(`Charge type` = charge_type, `Count of Requestors w/ IDENT report` = count_ident,
                                                                `Percent of Requestors w/ IDENT report` = pct_ident,
                                                                # `Count of Requestors w/ any terminations in C3/ELIS` = count_smart_terminated, 
                                                                # `Percent of Requestors w/ any terminations in C3/ELIS` = pct_smart_terminated,
                                                                # `Count of Requestors w/ any terminations in SCOPS records` = count_scops_terminated, 
                                                                # `Percent of Requestors w/ any terminations in SCOPS records` = pct_scops_terminated,
                                                                `Count of Requestors w/ any denials in C3/ELIS` = count_denial, 
                                                                `Percent of Requestors w/ any denials in C3/ELIS` = pct_denial,
                                                                `Percent of all DACA Requestors` = pct_all_daca) %>% 
        select(-c(`Percent of all DACA Requestors`, `Count of Requestors w/ any denials in C3/ELIS`, 
                  `Percent of Requestors w/ any denials in C3/ELIS`))
                # `Count of Requestors w/ any terminations in SCOPS records`, `Percent of Requestors w/ any terminations in SCOPS records`,
                #   `Count of Requestors w/ any terminations in C3/ELIS`, `Percent of Requestors w/ any terminations in C3/ELIS`,
                #   `Count of Requestors w/ any denials in C3/ELIS`, `Percent of Requestors w/ any denials in C3/ELIS`,
                #   `Percent of all DACA Requestors`))
      
daca_charges_count_pct_table_clean

# write to file
write_csv(daca_charges_count_pct_table_clean, "Data/collapsed_daca_charges_approved_w_prior_ident_count_pct_table_20180503.csv")

