library(KeyboardSimulator)
library(tidyverse)

# https://github.com/ChiHangChen/KeyboardSimulator

# before running get_eoir_data_from_pcqs, need to uncheck CIS2 box and check doj-eoir box in pcqs
# chrome browser zoom set to 100% and set to right side of laptop screen

# setwd
setwd("X:/Asylum_EAD_study")

# load ead data
dir_ls("data")
# ead <- read_sas(data_file = "data/i765_asylum.sas7bdat")
ead <- read_excel(path = "data/765 variables (short rows).xlsx")

# load anumber_tbl
# anumber_tbl <- tibble(anumber = c("A075369917", "A076596366", "A088419752", "A073576134", "A099878453", "A088297558", "A086973636",
#                                   "A078740989", "A089160356", "A099936407"))

anumber_tbl <- ead %>% filter(!is.na(BEN_A_NUMBER)) %>% select(BEN_A_NUMBER) %>% slice(1:100) %>% rename(anumber = BEN_A_NUMBER)
anumber_tbl


####################################################################################################


# create get_eoir_data function
get_eoir_data_from_pcqs <- function(anumber) {
        
        # search for next anumber
        mouse.move(1150, 400)
        mouse.click()
        writeClipboard(anumber)
        keybd.press("ctrl+v")
        mouse.move(1420, 535)
        mouse.click()
        Sys.sleep(time = 2.5)
        
        # click on anumber record
        mouse.move(1080, 760)
        mouse.click()
        Sys.sleep(time = 5)
        
        # click on doj-eoir record
        # move further right for long names
        mouse.move(1475, 490)
        mouse.click()
        Sys.sleep(time = 3)
        
        # find and copy ij decision date
        mouse.move(1500, 500)
        mouse.click()
        keybd.press("ctrl+a")
        keybd.press("ctrl+c")
        Sys.sleep(time = 1)
        text <- readClipboard()
        
        # store text to return
        text_tbl <- text %>% enframe() %>% mutate(anumber = anumber)
        
        # navigate to home
        mouse.move(1500, 500)
        mouse.click()
        keybd.press("ctrl+a")
        keybd.press("ctrl+f")
        Sys.sleep(time = .5)
        keybd.press("ctrl+a")
        writeClipboard("Person Centric Query Service")
        keybd.press("ctrl+v")
        keybd.press("enter")
        mouse.move(1140, 95)
        mouse.click()
        Sys.sleep(time = 2)
        
        # return text
        return(text_tbl)
}


#####################################################################################################


# call get_eoir_data_from_pcqs()
tic()
output <- map_dfr(.x = anumber_tbl %>% pull(anumber), .f = ~ get_eoir_data_from_pcqs(anumber = .x))
toc()
output


########################


# test
anumber <- "A075369917"

# time tests
# 10 records in 142 sec, so 24 min per 100 records, or 40 hours per 10,000 records, or 250 records per hour
# average of 20-50k affirmative filings per year; 100k total from 2016-2018; 229k total from 2009-2018

# 100 records in 1425 sec, so 24 min per 100 records; 
# 77 of 100 correctly copied text, 16 of these 100 had "your search returned no results", so 6 of these 100 errored in some way
# full text for 100 records saved as 1.2 MB csv, so 100k records = 120,000 MB; and 120,000 MB / 1,000 = 120 GB
# full 2009-2018 229k records would be 274GB;
# due to size, will need to remove extraneous text before saving; should be manageable

# if i scale up to four simultaneous pcqs instances, then i get 24 min per 400 records, or 40 hours per 40,000 records, or 1000 records per hour

# if i ran 12 hours each weekday = 60 hours, plus 40 on weekends, that's 100 hours per week = 100,000 records, 
# so ~ 3 weeks for all 229k records; 1.5 if scott helps


##########################################################################################


# inspect 

# ij_decision_date
output %>% glimpse()

output %>% filter(str_detect(string = value, pattern = "IJ Decision Date")) %>% 
        mutate(ij_decision_date = str_replace(string = value, pattern = "IJ Decision Date\t", replacement = "")) 

output %>% filter(str_detect(string = value, pattern = "IJ Decision Date")) %>% 
        mutate(ij_decision_date = str_replace(string = value, pattern = "IJ Decision Date\t", replacement = "")) %>%
        distinct(anumber, value) %>% nrow()

output %>% filter(str_detect(string = value, pattern = "Your search returned no results")) %>% distinct(anumber) %>% nrow()

output %>% filter(str_detect(string = value, pattern = "IJ Decision\t"))

output %>% distinct(anumber) %>% nrow()

# save example output
write_csv(output, path = "data/pcqs_eoir_records_sample.csv")


###########################################################################################


# ij_decision_date
ij_decision_date <- output %>% filter(str_detect(string = value, pattern = "IJ Decision Date")) %>% 
        mutate(ij_decision_date = str_replace(string = value, pattern = "IJ Decision Date\t", replacement = ""))

# ij_decision
ij_decision <- output %>% filter(str_detect(string = value, pattern = "IJ Decision\t")) %>% 
        mutate(ij_decision = str_replace(string = value, pattern = "IJ Decision\t", replacement = ""))

# proceeding_received_date
proceeding_received_date <- output %>% filter(str_detect(string = value, pattern = "Proceeding Received Date\t")) %>% 
        mutate(proceeding_received_date = str_replace(string = value, pattern = "Proceeding Received Date\t", replacement = ""))

# custody_status
custody_status <- output %>% filter(str_detect(string = value, pattern = "Custody Status\t")) %>% 
        mutate(custody_status = str_replace(string = value, pattern = "Custody Status\t", replacement = ""))

# nta date

# in absentia








