library(dplyr)
library(readr)
library(stringr)

# setwd
setwd("H:/RED/DACA")
list.files()

# read in clean data
list.files("./Data")
daca_clean1 <- read_csv("./Data/I821clean1_csv.csv")
daca_clean2 <- read_csv("./Data/I821clean2_csv.csv")
daca_clean3 <- read_csv("./Data/I821clean3_csv.csv")
daca_clean4 <- read_csv("./Data/I821clean4_csv.csv")
daca_clean5 <- read_csv("./Data/I821clean5_csv.csv")
daca_clean6 <- read_csv("./Data/I821clean6_csv.csv")
daca_clean7 <- read_csv("./Data/I821clean7_csv.csv")
daca_clean8 <- read_csv("./Data/I821clean8_csv.csv")
daca_clean9 <- read_csv("./Data/I821clean9_csv.csv")
daca_clean10 <- read_csv("./Data/I821clean10_csv.csv")

daca <- daca_clean8
glimpse(daca)
head(daca, 10) %>% data.frame()
tail(daca, 10)
dim(daca)
length(unique(daca$anum))


# read in raw data
list.files("./Data")
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

daca_raw <- daca1
names(daca_raw)
glimpse(daca_raw)
head(daca_raw)
names(daca_raw)

# rename variables
# will get warnings since ident_report is not blank for first few rows
daca_raw <- daca_raw %>% rename(a_number = X1, receipt_number = X2, fbi_fin_number = X3, ident_report = X4)
head(daca_raw)

# inspect 
length(unique(daca_raw$a_number))



###############################################################


# inspect daca
head(daca)
daca %>% group_by(keyline) %>% count() %>% arrange(desc(n))

# check for inproper anum
daca %>% filter(!(grepl("^A[0-9]{9}", anum))) %>% dim()
daca %>% filter(!(grepl("^A[0-9]{9}", anum)))


# inspect daca_raw
daca_raw$ident_report[2]
cat(daca_raw$ident_report[1])
writeLines(daca_raw$ident_report[50])

daca_raw %>% filter(!(grepl("^A[0-9]{9}", a_number))) %>% dim()
daca_raw %>% filter(!(grepl("^A[0-9]{9}", a_number)))


################################################################


# find any a-numbers in daca but not daca_raw, and vice versa
length(unique(daca_raw$a_number))
length(unique(daca$anum))

daca %>% distinct(anum) %>% head()
glimpse(daca)

daca_raw %>% distinct(a_number) %>% head()
glimpse(daca_raw)


# in_daca_but_not_in_daca_raw seem to be unformatted a_numbers
in_daca_but_not_in_daca_raw <- daca$anum[!(daca$anum %in% daca_raw$a_number)]
in_daca_but_not_in_daca_raw
length(in_daca_but_not_in_daca_raw)

#
in_daca_raw_but_not_in_daca <- data.frame(a_number = daca_raw$a_number[!(daca_raw$a_number %in% daca$anum)])
in_daca_raw_but_not_in_daca
nrow(in_daca_raw_but_not_in_daca)
# write_csv(in_daca_raw_but_not_in_daca, "Data\in_daca_raw_but_not_in_daca_part3.csv")

# inspect
daca_raw %>% filter(a_number == "A204446494") %>% do(cat(.$ident_report))
daca_raw %>% filter(a_number == "A204409715") %>% do(cat(.$ident_report))
daca_raw %>% filter(a_number == "A204582327") %>% do(cat(.$ident_report))

daca %>% filter(anum == "A204446494")


#################################################################


# inspect daca a_numbers without valid keyline
head(daca)
daca_anum_count <- daca %>% group_by(anum) %>% count() %>% arrange(n) %>% ungroup()
daca_anum_count %>% filter(n == 1)
daca_anum_count %>% filter(n == 1) %>% slice(75)
daca_anum_count %>% filter(n == 1) %>% tail()
daca_anum_without_charge <- daca_anum_count %>% filter(n == 1)
write_csv(daca_anum_without_charge, "daca_part8_anum_without_charge.csv")

# inspect those anum with only 1 record
daca %>% filter(anum == "A204559334")
daca_raw %>% filter(a_number == "A207068716") %>% slice(1) %>% do(cat(.$ident_report))
# daca_raw %>% filter(a_number == "A204442985") %>% slice(1) %>% pull(ident_report)

daca_raw %>% filter(grepl("columbus pd", ident_report, ignore.case = TRUE))

#################################################################


# regex on keywords

# https://www2.fbi.gov/ucr/cius_04/appendices/appendix_02.html
# fbi uniform crime report categories

# test_daca <- daca[1:5000, ]
# test_daca$charge_type <- NA

daca_charges <- daca
dim(daca_charges)
daca_charges$charge_type <- NA
head(daca_charges)


for(i in 1:nrow(daca_charges)) {
        current_keyline <- daca_charges$keyline[i]
        print(str_c("row ", i, ": ", current_keyline))
        
        if(grepl("CHARGE                  1|CHARGE                  001|CHARGE                  01|CHARGE                  003|CHARGE                   04", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE NUMBER|CHARGE                  3|CHARGE                  02|CHARGE                  2|CHARGE                  7", current_keyline, ignore.case = TRUE)|
           grepl("CHARGE                  002|CHARGE                   02|CHARGE OR DISPOSITION IS NEEDED|CHARGE INFORMATION", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE                  007|CHARGE                  9|CHARGE                  004|CHARGE                  006", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE                  4|CHARGE                  03|CHARGE DESCRIPTION|CHARGE                  005|CHARGE                  07", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE-SEE COMMENT FOR CHARGE|CHARGE                  5|CHARGE                  06|CHARGE                  09", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE                  04|CHARGE                   01|CHARGE                  01|CHARGE                  08", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE                   05|CHARGE                   06", current_keyline, ignore.case = TRUE) |
           grepl("CHARGE                  05|CHARGE                  6|CHARGE TRACKING NUMBER|COUNTS 1|CHARGE                  8", current_keyline, ignore.case = TRUE) |
           grepl("ARRESTED OR RECEIVED|NEW ANUMBER|ARREST TRACKING #|DISPOSITION|CHARGE END OF PART|CHARGE DATE AND/OR CHARGE ORI|CHARGE IS DESIRED|CHARGE ORI FOR FILES|CHARGES OR ARRESTS|ARREST CASE NUMBER|ARRESTING AGENCY|ARREST #0[0-9]|ARREST/  [0-9]{4}/[0-9]{2}/[0-9]{2}|ARREST DATE|MRD|STATUTE|SEVERITY", current_keyline, ignore.case = TRUE) |
           current_keyline == "CHARGE" | current_keyline == "CHARGE LITERAL" | str_to_lower(str_sub(current_keyline, start = 1, end = 6)) == "arrest" | str_to_lower(str_sub(current_keyline, start = 1, end = 5)) == "court") {
                daca_charges$charge_type[i] <- "keyline_does_not_list_specific_charge"
        
                # note traffic_speed_moving_vehic_driving_license_insurance_dui was an old category i forgot to delete,
                # it flagged "VEH" and "INSUR", so is safe to collapse into traffic_speed_moving_vehic_driving_license_insurance
                
        } else if(grepl("traffic|speed|moving|LIC. SUSPEND|BEGINNER PERMIT|REGISTRATION SUSPENDED|RECEIVED A LIC|OPERATOR NEVER RECEIVED|NEVER RECE LISCE|WINDOW TINT|DRV-LIC|LANE UNSAFELY|VEH OPR|REAR WINDOW|LEAVE SCENE ACC|TAILLAMP|UNRESTRAND CHILD|UNSAFE TIRES|ROAD HAZARD|REMAIN AT SCENE|HWY|DRIVE WHILE SUSPENDED|LEAVING SCENE|VEH/PROP|APPLY FOR A TRANSFER OF OWNERSHIP|EXPIRED REGISTRATION|LANE CHANGE|w/o signal|LEAVE INJURY ACCIDENT|LIGHTS VIOL|OBST HWY|MTR VEH OWNER|VC VIOL|OBSCURING LETTERING OR DECAL|OBT DMV DOC|FAIL TO MAINTAIN CONTROL|BACK UP LIGHT|HWY LANDSCAPE|DUTY ON STRIKING FIXTURE|OVERTAKING ON RIGHT|FLASHING LIGHT|IMPROPER PASSING|OPER UNREGISTERED|PARKED VEH|FOLLOWING TO CLOSE|IMPROPER REGISTRATION|VEHCLE EQUIP|LEAVING SCENE|CHILD RESTRAINT DEVICE|YIELD SIGN|SIGNAL LIGHT|MORE PASS THAN BELTS|IMPROPER SIGNAL|INSPECTION/STICKER|AUTO WITHOUT OWNERS CONSENT|TAIL LIGHTS|DRV W/PRIV SUSP/REV|STAY IN ONE LANE|IGNITION INTERLOCK|FAIL DIM BEAMS|WEARING SEAT B|DRIV WITHOUT LIC|UNLAWF REG|FAIL TO MAINTAIN VEH|EXCESSIVE EXHAUST|MARKED LANES|FAIL STOP|USE CELL PH W DRIV|FAIL YIELD|DRIVE TOO SLOW|FAIL TO DRIVE RIGHT|UNSAFE LANE CHANGE|DRIVE SUSPENDED|OPER VEH|OPERATING NEVER RECIEVED|VEH MANSL|vehicular manslaughter|driving|SUSP REGISTRATION|UNATTENED VEH|UNATT VEH|driver|OPR VEH|AUXILIARY LAMPS|SCREECHING TIRES|VEH W/PLATE|REQ INSUR|head lamps|STOP/YIELD|DIVIDED ROAD|DAMAGE/UNATTND VEH|FICTITIOUS INSPECTION CERT|SCHOOL BUS|OPERATING AFTER REVOCATION|FAILED TO YIELD|FLS REG|LAMPS ACT|NO RESTRAINT|VEH TO DISPLAY FIC|FOLLOWING TOO CLOSE|MAINTAIN REQ IN|IMPROPER USE REGISTR|OPERATING W O|FAIL STOP VEH|UNLAWFUL TO DRIVE|ROADWAY|INSURANCE CARD|OPER MV|FALSE PROOF|PROVIDE CDL|IMPROPER EQUIPMENT|MUFFLER|OPERATION W/O|VALID REGISTRATION|LEAVING THE SCENE OF AN ACCIDENT|FAIL PROVE FIN RSP|license|STOP LAMPS|TURN SIGNAL|FAILURE TO YIELD|OPERATE UNINSUR|IGNITION INTERLOCK DEVICE TAMPERING|NO INSURANCE|PROOF OF INSUR|OPERATORS LIC|FAIL PROVE FIN RSP PO REQUEST|SEAT BELT|TAIL LAMP|625 ILCS 5-0/6-303|DL SUSPENDED|LIC SUSP|VIOLATE LIGHTS & LAMPS ACT|COUNTS OF DRIV|IMPROPER TURN|FAIL TO YIELD|FAIL TO STOP|FAIL TO SIGNAL|NO DL|UNSAFE TURN|TURN OR APPROACH|WNDSHLD|WINDSHLD|windshield|REGISTRATION PLATE|REGISTRATION EXP|PERMIT DISPLAY|GIVE SIGNAL|highway|racing|MOTOR VEH|LANE USAGE|W/O LIGHTS|without lights|STOP SIGN|DRIV LIC|625 ILCS 5-0/6-101|DRIV. LIC|motorcycle", 
                        current_keyline, ignore.case = TRUE) & !grepl("traffick", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "traffic_speed_moving_vehic_driving_license_insurance"
        
        } else if(grepl("DWI|DUI|>=0.08|DRIVE AFTER DRINKING|EXCESS BLOOD ALC|OPERATING - IMPAIRED|OPERATING WHILE|RIDE BIKE UNDER INFLUENCE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "DUI"
        
        } else if(grepl("veh", current_keyline, ignore.case = TRUE) & grepl("insur", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "traffic_speed_moving_vehic_driving_license_insurance"
        
        } else if(grepl("hit and run|HIT&RUN|hit & run|HIT RUN", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "hit_and_run"
        
        } else if(grepl("human traffick|SEX TRAFF", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "human_trafficking"
        
        } else if(grepl("homicide|murder", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "homicide_murder"
        
        } else if(grepl("manslaughter", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "manslaughter"
        
        } else if(grepl("SEX ASSLT|sex assault", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "sex_assault"
        
        } else if(grepl("SEX  ABUSE|SEX ABUSE|UNLAWFUL SEX|VICT INCAPABLE OF CONSENT|SEXUAL ABUSE|UNLAWFUL SEXUAL INTERCOUR|CRIMINAL SEXUAL CONDUCT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "sex_abuse_unlawful"
        
        } else if(grepl("OFFENSIVE TOUCHING", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "offensive_touching"
        
        } else if(grepl("CHILD PORN", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "child_porn"
        
        } else if(grepl("sodomy|SDMY", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "sodomy"
        
        } else if(grepl("INDECENT EXP|PUBLIC INDECENCY|PHOTOGRAPH UNSUSPECTING NUDE|INDECENT CONDUCT|INVASIVE VISUAL RECORDING", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "indecent_exposure_peeping"
        
        } else if(grepl("stat rape|statutory rape|RAPE OF A CHILD|RAPE OF CHILD|SEX W/MINOR|SEXUAL INTERCOURSE W/MNR|SEXUAL INTERCOURSE W/MINOR|SEX PENETRATION|INTERCOURSE W/MINOR|SEX W/MNR|CHILD MOLEST|COMMUNICATE W/MINOR-IMMORAL PURP|VICARIOUS SEX GRATIFICATION|SEXUAL CONDUCT WITH A MINOR|ANNOY/MOL VICT UNDER 18|SEX INTERCOURSE W/MNR|SEX WITH MINOR|INDECENCY W/A CHILD|INDECENCY WITH A CHILD SEXUAL CONTACT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "sexual_contact_with_minor"
        
        } else if(grepl("rape|SEXUAL PENETRATION W/FORCE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "rape"
                
        } else if(grepl("harassment|HARRASSMENT|VIOLATE ORDER PROTECTION", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "harassment_restrain_order_violation"
        
        } else if(grepl("kidnap", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "kidnap"
        
        } else if(grepl("LEWD OR LASCIV|LEWD LASCV|LASCIVIOUS|L&L|LEWD ACT|LEWDNESS|L&AMP;L W/CHILD|LEWD", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "lewd_lascivious_acts"
        
        } else if((grepl("assault|DRIVE BY SHOOTING|ASSLT|ADW/FORCE|A&B|ASLT|ADW WITH FORCE", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "assault"
        
        } else if((grepl("mayhem", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "mayhem"
        
        } else if(grepl("FIGHT|brawl", current_keyline, ignore.case = TRUE) & !(grepl("FIREFIGHTER", current_keyline, ignore.case = TRUE)) & 
                  !(grepl("DOGFIGHT", current_keyline, ignore.case = TRUE)) & !(grepl("COCKFIGHT", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "fighting"
        
        } else if(grepl("batt|counts of bat|720 ILCS 5-0/12-3-A-2|SEXUAL BATERY", current_keyline, ignore.case = TRUE) | 
                  (grepl("bat", current_keyline, ignore.case = TRUE) & grepl("spouse", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "battery"
        
        } else if(grepl("inf corp inj|CRPL INJ|INJURY UPON SPOU|CORP INJ|CORPORAL INJURY|INFLICT CORPORAL INJ|INFLICT CORP INJ|INT BODILY INJ", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "inflict_corporeal_injury"
        
        } else if(grepl("DOMESTIC VIOLENCE|DOM VIOL|DOM VIO|DOMESTC VIOL|DOMES VIOL|DOMESTIC VIOL|DOMESTIC ABUSE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "domestic_violence"
        
        } else if(grepl("dogfight", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "dog_fighting"
        
        } else if(grepl("CHILD ABUSE|CHILD IN NEED OF SERVICES|CHINS|ENTICING A CHILD|CHILD CRUELTY|CHILD TO BE ENDANGERED|ENDANGER LIFE/HEALTH CHILD|INFLICT INJ/ETC ON CHILD|CHILD ENDANGER|INJ TO A CHILD|INFLICT INJURY ETC UPON CHILD|ENDANGER CHILD|ENDANGERING THE WELFARE OF A CHILD|ENDANGERING A CHILD|ENDANGERMENT OF A CHILD|PUNISHMENT OF A CHILD|CRUEL TO CHLD|CRUELTY TO CHILD|ABANDON/ENDANGER CHILD", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "child_abuse"
        
        } else if(grepl("CARJACK|TAKING AUTO WITHOUT OWNERS CONSENT|MTR VEH W/O PERMISSION|VEH W O OWNER|TAKE VEH W/O OWNER CONSNT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "motor_vehicle_theft"
        
        } else if(grepl("ROBBERY", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "robbery"
        
        } else if(grepl("BURGL", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "burglary"
        
        } else if(grepl("EMBEZ", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "embezzlement"
        
        } else if(grepl("theft|shoplift|larceny|LARCNY|THFT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "theft_larceny"
        
        } else if(grepl("STOLEN|APPROPR LOST PROP|STOLN PROP|STOLEN PROP|WRONGFUL APPROP", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "stolen_property"
        
        } else if(grepl("RECKLESS ENDANGERMENT|RECKLESS CONDUCT|DEADLY CONDUCT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "reckless_endangerment_conduct"
        
        } else if(grepl("BREACH OF PEACE|disord|DISRD|DISTURBING THE PEACE|DISTURB THE PEACE|PUBLIC PEACE|NOISE|SOUND AMPLIFICATION SYSTEMS|PUBLIC ORDER|PUBLIC NUISANCE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "disturb_breach_of_peace_disorderly_noise"
        
        } else if(grepl("FALSE REPORT|FALSE RPT CRIM|FALSE INFORMATION|FALSE INFO|OBSTRUCT|FALSE STATEMENT|FALSE CLAIM|FABRICATING PHYSICAL EVIDENCE|FABRICATE PHYS EVID|FALSE EVIDENCE|CONCEAL EVIDENCE|MISUSE OF PASSPORT|destroy evidence|FALSE DOCUMENTS|FAL REPORT TO POLICE|TAMPERING WITH GOVT|REFUSE ID SELF|TAMPER W/GOVERNMENT RECORD|FAIL TO ID FUGITIVE|FAKE/ETC ID CARD|FALSE CRIME|FICTITIOUS/UNLAW ALTERD ID|FAIL TO IDENTIFY|FAIL TO GIVE AID OR INFORM|FALSE NAME|IDENTITY DECEPTION|TAMPERING WITH GOVT RECORD|FAKE ETC ID|GIVE FALSE|GIVING FALSE/FICITIOUS INFO|DECEPTIVE GOVT ID|FAIL TO IDENTIFY|FALSE ID|POSS DECEPTIVE GOVT ID", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "false_report_information_obstruct_false_id"
        
        } else if(grepl("MOB ACTION|LAWFUL ASSEMBLY|REFUSE TO DISPERSE|riot", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "riot_mob_unlawful_assembly"
        
        } else if(grepl("moral turp|MORALS AND DECENCY", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "moral_turpitude"
        
        } else if(grepl("DANG WPN|deadly wpn|DANGEROUS WPN|LOADED FIREAR|DANG WEAP|RIFLE|pistol|CARRY WPN|handgun|weapon|CRRY LOAD H/GUN|WPN|KNUCKL|STUN GUN|LEADED CANE|DEADWPN|CRIM POS WEAP|FIREARM|DAGGER|KNIFE|F/ARM|POSs? WPN", current_keyline, ignore.case = TRUE) & 
                  !(grepl("assault|asslt", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "weapon"
        
        } else if(grepl("marij|MARIHUANA|cannabis|MANUFACTURE/DELIVER THC|CNTL SUB|MAN/DEL CS|POSSESSION CDS|CONTRLD SU|CANNABI|POSS PARAPH|KETAMINE|PARAPHENELIA|AMPHETAMINE|POSSESS/CONTROLL MATTER|opium|MFG/SEL|POSS/PUR COKE BASE|CONTRLD SUBSTANCE|PARAPHERNA|POSS MARJ|POSSESSION OF THC|POSSESSION W/I TO DISTRIBUTE|POSSESSION WITH INTENT TO DISTRIBUTE|CONTROL SUB|cocaine|heroin|narcot|drug|NITROUS OX|TOLUENE|NARC/C/SUB|MINOR POSSESS|POSS CS|PARAPHERNALIA|CONTROLLD SUB|METHAMPHETAMINE|CONTROLLED SUB", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "drug"
        
        } else if(grepl("ST GANG|STREET GANG", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "gang"
                
        } else if(grepl("ORGANIZED CRIMINAL ACTIV|organized crim|org crime", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "organized_criminal_activity"
        
        } else if(grepl("INTIMIDATION WIT|intimidating wit|intimidate wit|DISUADE VIC/WIT|DETAIN WITNESS|HARASS WITNESS", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "intimidating_witness"
        
        } else if(grepl("intox|alco|MINOR IN POSSESSION|POSS ALCH|ALC CONSUMP|MINOR IN TAVERN|ALC BY MINOR|PUBLIC DRUNK|liq|beer|wine|open contain|ALC BEV|ILLEGAL CONSUMPTION|MINOR CONSUMPTION", current_keyline, ignore.case = TRUE) & !(grepl("driv", current_keyline, ignore.case = TRUE)) &
                  !(grepl("DWI|DUI|>=0.08|DRIVE AFTER DRINKING|EXCESS BLOOD ALC|OPERATING - IMPAIRED|OPERATING WHILE|RIDE BIKE UNDER INFLUENCE", current_keyline, ignore.case = TRUE))) {
                daca_charges$charge_type[i] <- "intox_alco_liq"
        
        } else if(grepl("vandal|GRAFFITI|SPRAY PAINT|VANDLISM|DESTRUCTION OF PROP|DAMAGE TO PROP|URINATE|DAMAGE PROPRTY|DAMAGE/ETC POWER LINES|DAMAGE TO RAILROAD PROPERTY|DEFACE|INJURY TO REAL PROPERTY|INJURY OF PERSONAL PROPERTY|DAMAGE STATE PROPERTY|LITTERING|MISCHIEF|DAMAGE PROPERTY|POSS DRILL/ETC|DAMAGE TO PROPERTY|DEST PROP|DESTROY PROP|CRIM MISCH", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "vandal"
        
        } else if(grepl("bail", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "bail"
        
        } else if(grepl("CONTRIBUTING TO THE DELINQUENCY OF A MINOR|DELINQUENCY OF A CHILD|DELINQUENCY OF MINOR|CONTRIBUTING TO A MINOR|DELINQ OF MINR|DELINQUENCY MINOR|DELINQUENCY OF A MINOR|DELINQUENCY/NEGL OF MINOR", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "contrib_delinquency_of_minor"
        
        } else if(grepl("fail to appear|FAILURE TO APPEAR|FAIL T/APPEAR|FTA", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "fail_to_appear"
        
        } else if(grepl("vagrant", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "vagrant"
        
        } else if(grepl("resisting|INTERFERENCE WITH ARRESTING OFFICER|RESIST ARREST|INTERF W/ DUTIES OF PUB SERVANT|INTERFER W/EMERGENCY|FUGITIVE|ESCAPE FROM CUSTODY|ELUDE PO|ELUDING INSPECTION|INTERFER W/PUBLIC DUTIES|ATTEMPT TO ELUDE|EVADING ARREST|FLIGHT-ESCAPE|FUGITIVE FROM JUSTICE|FLEEING POLICE|EVADING PEACE OFFICER|EVADING PO|EVADE PO|RESIST PEACE|RESIST PO|ELUDE A POLICE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "resisting_interference_evading"
        
        } else if(grepl("FAIL TO COMPLY|FL OBEY|FAIL OBEY|FAIL TO OBEY|NOT COMPLY|FAILING TO PROVIDE EVIDEN", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "fail_to_comply_obey"
        
        } else if(grepl("FRAUD|CORRUPT BUSINESS|MONEY LAUNDERING|FAIL WRITEN PROMIS|FALSE PRETENSES", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "fraud_false_pretense_corruption_money_laundering"
        
        } else if(grepl("forge|MISUSE CREDIT CARD|FICTITIOUS CHECK|FALSE CHECK|OBT CREDIT|BAD CHECK|POS/ETC BAD/ETC CHEKS|CMMT FORGRY|GET CREDIT/ETC OTHER S ID|OBTAIN CREDIT/ETC:USE OTHER'S ID|DEBIT CARD ABUSE|COUNTRFT|counterfeit", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "forge_counterfeit"
        
        } else if(grepl("stalk", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "stalking"
        
        } else if(grepl("imperson|PERSNATE|PERSONATE|IMPERSN|IMPOSTOR|IMPOSTER|FALSE PERSONATION", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "impersonate"
        
        } else if(grepl("PROB VIOL|parole|HOME DETENTION VIOL|CURFEW|COURT REMAND", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "probation_parole_violation_curfew_court_remand"
        
        } else if(grepl("trespass|TRESPAS|TRESPS|BREAKING OR ENTERING|TRES PROP|UNAUTH ENTRY POSTED LAND", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "trespass"
        
        } else if(grepl("loiter|LOITR", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "loitering"
        
        } else if(grepl("NONCOMM DWELLING|NONCOM DWELL|UNLAWFUL ENTRY|HOME INVASION", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "unlawful_entry_home_invasion_enter_noncomm_dwelling"
        
        } else if(grepl("unlawful restraint", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "unlawful_restraint"
        
        } else if(grepl("perjury", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "perjury"
        
        } else if(grepl("arson", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "arson"
        
        } else if(grepl("ACCOMPLICE AFTER THE FACT|ACCESSORY AFTER THE FACT|AIDING AN OFFENDER|HINDERING APPREHENSION|HARBORING RUNAWAY|ACCESSORY|CRIMINAL ASSISTANCE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "accessory_hindering_accomplice_after_fact"
        
        } else if(grepl("prostitut|pandering|PANDER PRSN F/PROST|BROTHEL", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "prostitution_pandering_brothel"
        
        } else if(grepl("deport|alien|overstay|ENTRY WITHOUT INSPECTION|immigrant|ILLEGAL ENTRTY|ENTRY W/O ADMISSION|ESTABLISH FALSE STATUS|REMOVAL PROCESSING|ENTRY W/O INSPECTION|IMMIGRATION|ILLEGAL ENTRY TO US|ILL ENTRY|illegal entry|ENTRY W/OUT INSPECTION|CLAIM TO US CITIZENSHIP|FALSE CITIZEN/ETC DOC|US CITIZENSHIP|ILLEGAL ENTRY INTO THE US|IMMIGRATION VIOL|FALSE CLAIM TO U.S. CITIZENSHIP|FALSE REPRESENTATION TO US CITIZENSHIP|IMMIGRATION WITHOUT IMMIGRANT|REMOVAL PROCEEDING|FALSE CITIZENSHIP|ILLEGAL ENTRY INTO THE UNITED STATES|CLAIM TO CITIZENSHIP|ILLEG ENT US", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "deport_alien_nonimmigrant_overstay_ewi_removal"
        
        } else if(grepl("JUVENILE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "undefined_juvenile"
        
        } else if(grepl("ORDINANCE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "undefined_ordinance"
        
        } else if(grepl("gambling", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "gambling"
        
        } else if(grepl("CRUELTY TO ANIMAL", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "cruelty_to_animals"
        
        } else if(grepl("ELECTRONIC COMMU DEVICE|DAMAGE WIRELES COM DEVICE", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "electronic_communication_device"
        
        } else if(grepl("solicit", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "solicitation"
        
        } else if(grepl("oral cop", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "oral_copulation"
        
        } else if(grepl("FALSE IMPRISON", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "false_imprisonment"
        
        } else if(grepl("CONSPIRACY|THRTN CRIME|THREATEN CRIME|ATT CRIME|ATT TO COMMIT CRIME|THREAT CRIME|TERRORISTIC THREAT|TERRORIST THREAT|THREATN|threaten|USE OF THREAT|threat", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "undefined_treat_attempted_crime_conspiracy"
        
        } else if(grepl("contempt|BENCH WARRANT|ISSUANCE OF WARRANT|WARRANT ARREST|COUNTS OF WARRANT|WARRANT", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "contempt_bench_warrant"
        
        } else if(grepl("CHARGE LITERAL", current_keyline, ignore.case = TRUE)) {
                daca_charges$charge_type[i] <- "undefined_charge_literal"
        }
}


write_csv(daca_charges, "daca_charges_part10.csv")


head(daca_charges)
unique(daca_charges$charge_type)
daca_charges %>% filter(!(is.na(charge_type)))
daca_charges %>% group_by(charge_type) %>% count() %>% arrange(desc(n)) %>% data.frame()
daca_charges %>% filter(is.na(charge_type)) %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame(.) %>% head(., 100)
daca_charges %>% filter(is.na(charge_type), court == 0) %>% group_by(keyline) %>% count() %>% arrange(desc(n)) %>% data.frame(.)
daca_charges %>% filter(is.na(charge_type), str_to_lower(str_sub(keyphrase, start = 1, end = 6)) == "arrest") %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% data.frame(.) %>% head(., 100)
daca_charges %>% filter(is.na(charge_type), str_to_lower(str_sub(keyphrase, start = 1, end = 5)) == "court") %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% data.frame(.) %>% head(., 100)


daca_charges %>% filter(is.na(charge_type), court != 1) %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% data.frame(.)

daca_charges %>% filter(charge_type == "keyphrase_does_not_list_specific_charge") %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% 
        data.frame(.) %>% head(., 50)
daca_charges %>% filter(grepl("MRD", keyphrase, ignore.case = TRUE) & mrd != 1)
daca_charges %>% filter(court == 1) %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% data.frame()

daca_charges %>% filter(grepl("CHARGE-III/720 ILCS 5-0/16A-3-A", keyline, ignore.case = TRUE))
daca_raw %>% filter(a_number == "A204402571") %>% filter(grepl("CHARGE-III/720 ILCS 5-0/16A-3-A", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204465173") %>% head(20)


###############################################


# test keyphrase_does_not_list_initial_charge
# find a_numbers with keyphrase_does_not_list_initial_charge, then ensure those also have at least one charge type != keyphrase_does_not_list_initial_charge
not_specific_charge_a_numbers <- daca_charges %>% filter(charge_type == "keyphrase_does_not_list_specific_charge") %>% distinct(anum) %>%
        pull(anum)
length(not_specific_charge_a_numbers)
length(unique(daca$anum))




#########################################################################


# inspect non_informative keyphrase formats to ensure we're getting actual charges

# "CHARGE                  1", 
daca_charges %>% filter(grepl("CHARGE                  1", keyphrase))
daca_raw %>% filter(a_number == "A204410876") %>% filter(grepl("CHARGE                  1", ident_report, ignore.case = TRUE)) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204410876")

# "CHARGE                  002", CHARGE OR DISPOSITION IS NEEDED, CHARGE DESCRIPTION, CHARGE NUMBER
daca_charges %>% filter(grepl("CHARGE                  002", keyphrase))
daca_raw %>% filter(a_number == "A214092001") %>% filter(grepl("CHARGE                  002", ident_report, ignore.case = TRUE)) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A214092001") %>% head(20)

# CHARGE INFORMATION
daca_charges %>% filter(grepl("CHARGE INFORMATION", keyphrase))
daca_raw %>% filter(a_number == "A204468998") %>% filter(grepl("CHARGE INFORMATION", ident_report, ignore.case = TRUE)) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204468998") %>% head(20)
daca %>% filter(anum == "A204468998") %>% head(20)
daca %>% filter(dismissed == 1)

# CHARGE TRACKING NUMBER
daca_charges %>% filter(grepl("CHARGE TRACKING NUMBER", keyphrase))
daca_raw %>% filter(a_number == "A204422871") %>% filter(grepl("CHARGE TRACKING NUMBER", ident_report, ignore.case = TRUE)) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204422871") %>% head(20)

# "ARRESTED OR RECEIVED"
daca_charges %>% filter(grepl("ARRESTED OR RECEIVED", keyphrase))
daca_raw %>% filter(a_number == "A204465173") %>% filter(grepl("ARRESTED OR RECEIVED", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204465173") %>% head(20)

# ARREST TRACKING
daca_charges %>% filter(grepl("ARREST TRACKING", keyphrase))
daca_raw %>% filter(a_number == "A204464661") %>% filter(grepl("ARREST TRACKING", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204464661") %>% head(20)

# ARREST DATE
daca_charges %>% filter(grepl("ARREST DATE", keyphrase))
daca_raw %>% filter(a_number == "A204422871") %>% filter(grepl("ARREST DATE", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204422871") %>% head(20)

# MRD
daca_charges %>% filter(grepl("MRD", keyphrase)) 
daca_raw %>% filter(a_number == "A204465173") %>% filter(grepl("ARRESTED OR RECEIVED", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204465173") %>% head(20)

# statute, severity
daca_charges %>% filter(grepl("STATUTE  INTERFERENCE WITH", keyphrase)) 
daca_raw %>% filter(a_number == "A204425325") %>% filter(grepl("STATUTE  INTERFERENCE WITH", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204425325") %>% head(20)

# CHARGE WITHOUT A PRE-TRIAL DIVERS
daca_charges %>% filter(grepl("CHARGE WITHOUT A PRE-TRIAL DIVERS", keyphrase)) 
daca_raw %>% filter(a_number == "A204477713") %>% filter(grepl("CHARGE WITHOUT A PRE-TRIAL DIVERS", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204477713") %>% head(20)

# court- ()
daca_raw %>% filter(grepl("COURT- ()", ident_report, ignore.case = TRUE))
daca_raw %>% filter(a_number == "A204465173") %>% filter(grepl("COURT- ()", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204465173")

# ARREST #01
daca_raw %>% filter(grepl("ARREST #01", ident_report, ignore.case = TRUE))
daca_raw %>% filter(a_number == "A204404050") %>% filter(grepl("ARREST #01", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204404050")


#######################################


# scratchpad

# check MRD
daca_charges %>% filter(grepl("MRD/DISMISSED/FURTHERANCE OF JUSTICE", keyphrase))
daca_raw %>% filter(a_number == "A204469511") %>% filter(grepl("MRD/DISMISSED/FURTHERANCE OF JUSTICE", ident_report, ignore.case = TRUE)) %>% do(cat(.$ident_report))
daca_charges %>% filter(anum == "A204469511") %>% head(20)

# check MRD
daca_charges %>% filter(mrd == 1)
daca_charges %>% filter(anum == "A204428310") %>% data.frame(.)
daca_raw %>% filter(a_number == "A204428310") %>% filter(grepl("MRD", ident_report, ignore.case = TRUE)) %>% slice(1) %>% do(cat(.$ident_report))

# check arrest
daca %>% filter(arrested == 1) %>% group_by(keyphrase) %>% count() %>% arrange(desc(n)) %>% data.frame(.)



###################################################################################
###################################################################################
###################################################################################



# disaggregate charge_type
daca_charges %>% filter(charge_type == "deport_alien_nonimmigrant_overstay_ewi_removal") %>% distinct(anum) %>% count() #23,212
immigration_a_numbers <- daca_charges %>% filter(charge_type == "deport_alien_nonimmigrant_overstay_ewi_removal") %>% distinct(anum) %>% 
        mutate(immigration_a_number_flag = 1)
dim(c3_elis2)
c3_elis2_immigration_merge <- c3_elis2 %>% left_join(., immigration_a_numbers, by = c("a_number" = "anum"))
dim(c3_elis2_immigration_merge)
c3_elis2_immigration_merge %>% filter(immigration_a_number_flag == 1) %>% distinct(a_number) %>% count() # 22242

daca_charges %>% filter(charge_type == "deport_alien_nonimmigrant_overstay_ewi_removal") %>% 
        group_by(keyline) %>% count() %>% ungroup() %>% arrange(desc(n))  %>% slice(100:200) %>% data.frame()

immigration_charges <- daca_charges %>% filter(charge_type == "deport_alien_nonimmigrant_overstay_ewi_removal") %>%
        mutate(immigration_charge = case_when(grepl("alien removal|deportable alien|deportation only|DEPORTATION|PENDING DEPORTATION|DEPORT. PROCESS|DEPORTATION PROCEEDING|DEPORTATION PROCESSING|REMOVAL PROCESSING|REMOVAL PROCEEDING", keyline, ignore.case = TRUE) ~ "alien_removal_deportation_processing_proceedings_deportable_alien",
                              grepl("entry without inspection|ENTRY W/O INSPECTION|ENTERED WITHOUT INSPECTION|ENTRY W/O ADMISSION|alien at improper|IMPROPER ENTRY OF ALIEN|illegal entry|ILL ENTRY|ILLEG ENT|IMMIGRATION WITHOUT IMMIGRANT VIS|ENTRY AFTER DEPORT|RE-ENTRY OF REMOVED ALIENS", keyline, ignore.case = TRUE) ~ "illegal_improper_entry_ewi_without_visa_after_deport",
                              grepl("overstay", keyline, ignore.case = TRUE) ~ "overstay",
                              grepl("harboring certain alien", keyline, ignore.case = TRUE) ~ "harboring_certain_aliens",
                              grepl("false citizen/etc doc|FALSE CITIZENSHIP|DOC TO ESTABLISH FALSE STATUS|IMMIGRANT NOT IN POSSESSION OF VALID|IMMIGRANT WITHOUT AN IMMIGRANT VISA", keyline, ignore.case = TRUE) ~ "false_citizen_document_immigrant_wo_visa",
                              grepl("transporting aliens|smuggling aliens|TRANSPORT AN ALIEN|ALIEN SMUGGLING|ALIEN SMUG", keyline, ignore.case = TRUE) ~ "transporting_smuggling_aliens",
                              grepl("alien inadmissability|ALIEN INADMISSIBILE|INADMISSIBLE ALIEN|NONIMMIGRANT OUT OF STATUS|ILLEGAL IMMIGRANT|ALIEN PRESENT IN THE UNITED STATES|INADM ALIEN|ALIEN INADMISSIBILITY|EXCLUDABLE ALIEN|INADMISSABLE ALIEN", keyline, ignore.case = TRUE) ~ "alien_present_inadmissability_excludable_nonimmigrant_out_of_status",
                              grepl("immigration violation|VIOLATION OF IMMIGRATION LAW|IMMIGRATION VIO|VIOL US IMMIGRATION LAW|VIOLATION OF US IMMIGRATION LAW", keyline, ignore.case = TRUE) ~ "immigration_law_violation",
                              grepl("INSPECTION BY IMMIGRATION OFFICER", keyline, ignore.case = TRUE) ~ "inspection_by_immigration_officer",
                              grepl("immigration hold|HOLD FOR IMMIGRATION", keyline, ignore.case = TRUE) ~ "immigration_hold",
                              grepl("CHARGE LITERAL  IMMIGRATION", keyline, ignore.case = TRUE) ~ "charge_literal_immigration",
                              TRUE ~ "NA"))
immigration_charges %>% group_by(immigration_charge) %>% count() %>% arrange(desc(n)) 

immigration_charges %>% filter(immigration_charge == "NA") %>% distinct(anum) %>% count() 
immigration_charges %>% filter(immigration_charge == "NA") %>% distinct(anum) %>% count() %>% 
        summarize(pct_NA = n / length(unique(immigration_charges$anum)))

immigration_charges %>% filter(immigration_charge == "NA") %>% group_by(keyline) %>% count() %>% 
        arrange(desc(n)) %>% data.frame() %>% head(100)
immigration_unable_to_extract <- immigration_charges %>% filter(immigration_charge == "NA") %>% group_by(keyline) %>% count() %>% 
        arrange(desc(n))
write_csv(immigration_unable_to_extract, "Data/immigration_unable_to_extract.csv")
                              
# create table
immigration_charges_pivot <- immigration_charges %>% select(anum, immigration_charge) %>% mutate(immigration_charge_flag = 1) %>% 
        distinct(anum, immigration_charge, immigration_charge_flag) %>% 
        spread(key = immigration_charge, value = immigration_charge_flag)
immigration_charges_pivot
glimpse(immigration_charges_pivot)
                 
immigration_charge_variables <- immigration_charges %>% distinct(immigration_charge) %>% 
        pull(immigration_charge)
immigration_charge_variables
             
immigration_charges_table <- immigration_charges_pivot %>%  
        select(anum, immigration_charge_variables) %>%
        gather(key = immigration_charge, value = value, c(immigration_charge_variables)) %>% 
        distinct(anum, immigration_charge, value) %>%
        filter(value == 1) %>% group_by(immigration_charge) %>% count() %>% arrange(desc(n))
immigration_charges_table

write_csv(immigration_charges_table, "Data/immigration_charges_table.csv")






