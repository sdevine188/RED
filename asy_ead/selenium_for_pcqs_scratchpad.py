import os
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
import pandas as pd
import numpy as np
import math
from datetime import datetime



# https://www.youtube.com/watch?v=FFDDN1C1MEQ
# download drivers from https://www.seleniumhq.org/download/
# https://selenium-python.readthedocs.io/index.html
# http://allselenium.info/python-selenium-commands-cheat-sheet-frequently-used/
#https://seleniumhq.github.io/selenium/docs/api/py/webdriver/selenium.webdriver.common.keys.html
#https://stackoverflow.com/questions/10629815/how-to-switch-to-new-window-in-selenium-for-python

# set wd
os.chdir("X:/Asylum_EAD_study/data/EOIR")

# read in anumbers
eoir_anumbers = pd.read_csv("anumbers_to_pull_from_pcqs_eoir.csv")
eoir_anumbers.shape
eoir_anumbers.head()

# get batch of anumbers
batch_1 = eoir_anumbers.iloc[1:1000, : ]
batch_1.shape

batch_2 = eoir_anumbers.iloc[1000:281, : ] # batch_2 errored out after completing 280 anumbers
batch_2.shape

batch_3 = eoir_anumbers.iloc[10000:10121, : ] # batch_3 errored out after completing 120 anumbers
batch_3.shape

batch_4 = eoir_anumbers.iloc[13000:14000, : ] # batch_4 errored out after completing 60 anumbers
batch_4.shape

batch_5 = eoir_anumbers.iloc[16000:17000, : ] # batch_5 completed in full
batch_5.shape

batch_6 = eoir_anumbers.iloc[20000:20191, : ] # batch_6 errored out after completing 190 anumbers
batch_6.shape

batch_7 = eoir_anumbers.iloc[30000:30071, : ] # batch_7 errored out after completing 70 anumbers
batch_7.shape

batch_8 = eoir_anumbers.iloc[40000:41620, : ] # bath_8 errored out after completing 1620 anumbers
batch_8.shape

batch_9 = eoir_anumbers.iloc[50000:50111, : ] # bath_9 errored out after completing 110 anumbers
batch_9.shape

batch_10 = eoir_anumbers.iloc[60000:60461, : ] # bath_10 errored out after completing 460 anumbers
batch_10.shape

batch_11 = eoir_anumbers.iloc[70000:80000, : ] # batch_11 errored out due to vpn timing out, so it got 4595 complete records, and ran about 6000 total
batch_11.shape

batch_12 = eoir_anumbers.iloc[80000:90000, : ] 
batch_12.shape

# create and inspect current_anumber_batch
current_anumber_batch = batch_12.copy() 
current_anumber_batch.shape
current_anumber_batch.head()


############################

# start driver
driver = webdriver.Chrome("C:/users/sjdevine/Work Folders/Desktop/chromedriver.exe")

# set page load timeout
driver.set_page_load_timeout(60)

# set total_tab_count
total_tab_count = 10

# loop through creating multiple tabs and opening pcqs
# note that after all tabs are created, tab_1 will be the initial blank tab
for current_tab_number in list(range(0, total_tab_count)) :
        
        # open a new tab
        driver.execute_script("window.open()")
        
        # switch to new tab
        driver.switch_to.window(driver.window_handles[current_tab_number])

        # navigate to pcqs
        driver.get("https://esb2ui-ngdc.esb2.uscis.dhs.gov/PCQS508/Login.do;jsessionid=8EB648F862D46081241B4676050C3C8C")
        
        # pause
        time.sleep(3)
        
        # click login button
        driver.find_element_by_xpath("//a[@title = 'PCQS DHS Login']").click()
        
        # pause
        time.sleep(3)
        
        # for first tab, click DHS users button; subsequent tabs will automatically inherit this choice, skipping this page
        if(current_tab_number == 0):
                
                # press button to log in as All Other DHS Users
                driver.find_element_by_xpath("//span[contains(text(), 'All Other DHS Users')]").click()
                
                # pause
                time.sleep(3)
        
        # click to un-check CIS 2 checkbox that is checked by default when logging in to pcqs
        driver.find_element_by_xpath("//label[contains(text(), 'CIS 2')]").click()
         
        # click doj-eoir checkbox
        driver.find_element_by_xpath("//label[contains(text(), 'DOJ-EOIR')]").click()
 
        
#######################

        
# find initial blank tab, which appears to end up in a random order amidst the new tabs
for current_tab_number in list(range(0, total_tab_count + 1)) :
        
        # switch to current_tab
        driver.switch_to.window(driver.window_handles[current_tab_number])
        
        # get current_tab url
        current_url = driver.current_url
        
        # if current_tab is initial blank tab, then delete it
        if(current_url == "about:blank") :
                driver.close()
                break
        
        
##########################################################################################################################
##########################################################################################################################


# create check_for_search_results function, which uses try/catch to avoid error when "search returned no results" element is not found, 
# and have for loop skip to next if no search results are found
def check_for_search_results():
        try:
                return(driver.find_element_by_xpath("//a[@onclick = 'getPersonActivities(0);return false;']").text)
        
        except:
                return(check_for_search_results_after_pause())
                

##############################
                

# create check_for_search_results_after_pause
def check_for_search_results_after_pause():
        try:
                # pause
                time.sleep(3)
                
                return(driver.find_element_by_xpath("//a[@onclick = 'getPersonActivities(0);return false;']").text)
        
        except:
                print(current_anumber + " did not return search results", sep = "")
                return("search did not return results")
                
                
##############################################################################################################################################
                
                
# create check_for_doj_eoir_result                
def check_for_doj_eoir_result():
            
        try:        
                return(driver.find_element_by_xpath("//a[contains(text(), 'DOJ-EOIR')]").text)
                
        except:
                return(check_for_doj_eoir_result_after_pause())


################################
                

# create check_for_doj_eoir_result_after_pause                
def check_for_doj_eoir_result_after_pause():
            
        try:        
                # pause
                time.sleep(3)
                
                return(driver.find_element_by_xpath("//a[contains(text(), 'DOJ-EOIR')]").text)
                
        except:
                print(current_anumber + " did not have doj-eoir results", sep = "")
                return("no doj-eoir results")


###############################################################################################################################################
                
                
# create check_for_eoir_record function
def check_for_eoir_record():
        
        try:
                # check for presence of ij_decision_date field, indicating that eoir_record is loaded        
                return(driver.find_element_by_xpath("//tbody/tr[@title = 'IJ Decision Date']/td").text)
                
        except: 
                return("no doj eoir record loaded")
                

###############################################################################################################################################
                
                
# create convert_blank_to_na function to clean current_anumber_data
def convert_blank_to_na(value):
    if(value == ""):
        return(np.NaN)
    else:
        return(value)
                                
                                
###############################
                                

# create data df as placeholder
data = pd.DataFrame()

# create no_result_df to return when current_anumber has no search results
no_result_df = pd.DataFrame({"alien_full_name" : [np.nan], "alien_date_of_birth" : [np.nan], 
                                      "anumber" : [np.nan], "lead_anumber" : [np.nan], "nationality" : [np.nan], 
                                      "case_type" : [np.nan], "rider_relationship" : [np.nan], "rider_indicator" : [np.nan], 
                                      "base_city_address_city" : [np.nan], "charging_document_date" : [np.nan],
                                      "case_input_date" : [np.nan], "initial_hearing_date" : [np.nan],
                                      "proceeding_received_date" : [np.nan], "latest_hearing_date" : [np.nan], 
                                      "latest_hearing_calendar_type" : [np.nan], "custody_status" : [np.nan],
                                      "received_at_eoir_date" : [np.nan], "ij_decision" : [np.nan], "ij_decision_date" : [np.nan],
                                      "ij_other_completion_date" : [np.nan], "a212c_filed" : [np.nan], "a212c_decision" : [np.nan],
                                      "a245adj_filed" : [np.nan], "a245adj_decision" : [np.nan], "voluntary_departure_filed" : [np.nan],
                                      "voluntary_departure_decision" : [np.nan], "suspension_filed" : [np.nan], 
                                      "suspension_decision" : [np.nan], "ij_mtr_received_date" : [np.nan], 
                                      "ij_mtr_decision" : [np.nan], "ij_mtr_decision_date" : [np.nan], "appeal_filed" : [np.nan],
                                      "board_decision" : [np.nan], "board_decision_date" : [np.nan], "alien_address_street_line_1" : [np.nan],
                                      "alien_city_state_zip" : [np.nan], "alien_address_changed_date" : [np.nan],
                                      "alien_address_latest_changed_date" : [np.nan], "alien_phone_number" : [np.nan],
                                      "atty_rep_name" : [np.nan], "atty_rep_address" : [np.nan], "board_atty_rep_name" : [np.nan],
                                      "board_atty_rep_city_state_zip" : [np.nan]})

        
##############################       


# print start time
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Start time =", current_time)        
        
# loop through current_anumber_batch getting pcqs eoir data
for current_anumber_set_number in list(range(0, math.ceil(current_anumber_batch.shape[0] / total_tab_count))) :

        # print progress
        print("current_anumber_set: ", current_anumber_set_number, "; ", 
              round((current_anumber_set_number / max(list(range(0, math.ceil(current_anumber_batch.shape[0] / total_tab_count))))) * 100, 1), " pct completed", sep = "")
        
        # get current_anumber_set
        current_anumber_set = current_anumber_batch.iloc[(current_anumber_set_number * total_tab_count) : (current_anumber_set_number * total_tab_count) + total_tab_count, : ]
        
        
        ###########################
        
        
        # loop through tabs entering current_anumber into anumber search field
        for current_tab_number in list(range(0, total_tab_count)) :
        
                # switch to current_tab
                driver.switch_to.window(driver.window_handles[current_tab_number])
        
                # continue to next iteration if at the end of current_anumber_batch, and so current_tab_number exceeds current_anumber_set rows
                if((current_tab_number + 1) > current_anumber_set.shape[0]) :
                        continue
                
                # get current_anumber
                current_anumber = current_anumber_set.iloc[current_tab_number, :].anumber
                print(current_anumber + " in loop_1")
                
                # enter current_anumber into id field
                driver.find_element_by_xpath("//input[@name = 'idValue']").send_keys(current_anumber)
                
                # clich search button
                driver.find_element_by_xpath("//input[@value = 'Search']").click()
                
                # pause
#                time.sleep(1)
                
                # run check_for_search_results()
                results_status = check_for_search_results()
                print(current_anumber + " results_status: " + results_status)
                
                if(results_status == "search did not return results"):
                        print(current_anumber + "did not return resutls; skipping to next")
                        continue
                
                # click hyperlink to pull up current_anumber record
                driver.find_element_by_xpath("//a[@onclick = 'getPersonActivities(0);return false;']").click()
                
                # pause
#                time.sleep(1)
                
                # run check_for_doj_eoir_result
                doj_eoir_results_status = check_for_doj_eoir_result()
                
                if(doj_eoir_results_status == "no doj-eoir results"):
                        print(current_anumber + "has no doj-eoir results; skipping to next")
                        continue
                
                # click hyperlink to pull up doj-eoir record 
                driver.find_element_by_xpath("//a[contains(text(), 'DOJ-EOIR')]").click()
                

        ####################


        # loop through tabs extracting variables
        for current_tab_number in list(range(0, total_tab_count)) :
        
                # switch to current_tab
                driver.switch_to.window(driver.window_handles[current_tab_number])
                
                # continue to next iteration if at the end of current_anumber_batch, and so current_tab_number exceeds current_anumber_set rows
                if((current_tab_number + 1) > current_anumber_set.shape[0]) :
                        continue
                
                # get current_anumber
                current_anumber = current_anumber_set.iloc[current_tab_number, :].anumber
                
                # run check_for_eoir_record()()
                doj_eoir_record_load_status = check_for_eoir_record()
                
                # if current_tab does not have a doj_eoir record loaded, append no_result_df and continue
                if(doj_eoir_record_load_status == "no doj eoir record loaded"):
                        
                        print(current_anumber + "has no doj eoir record loaded; skipping to next")
                        
                        # create current_anumber_data using no_result_df to return all NaN values
                        current_anumber_data = no_result_df.assign(anumber = current_anumber,
                                                                   pcqs_eoir_results_found = 0,
                                                                   current_tab_number = current_tab_number,
                                                                   current_anumber_set = current_anumber_set_number)
                        
                        # apply convert_blank_to_na to all variables
                        current_anumber_data = current_anumber_data.apply(lambda x: pd.Series(list(x.values.flat)).apply(convert_blank_to_na))
                        
                        # append current_anumber_data to data
                        data = data.append(current_anumber_data)
                        
                        # skip to next tab
                        continue
                
                # create df for variables
                current_anumber_data = pd.DataFrame({"anumber" : [current_anumber]})
                
                
                #####################
                
                
                # extract variables
                alien_full_name = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Full Name']/td")
                current_anumber_data = current_anumber_data.assign(alien_full_name = alien_full_name.text)
                
                alien_date_of_birth = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Date Of Birth']/td")
                current_anumber_data = current_anumber_data.assign(alien_date_of_birth = alien_date_of_birth.text)
                
                anumber = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Number']/td")
                current_anumber_data = current_anumber_data.assign(anumber = anumber.text)
                
                lead_anumber = driver.find_element_by_xpath("//tbody/tr[@title = 'Lead Alien Number']/td")
                current_anumber_data = current_anumber_data.assign(lead_anumber = lead_anumber.text)
                
                nationality = driver.find_element_by_xpath("//tbody/tr[@title = 'Nationality']/td")
                current_anumber_data = current_anumber_data.assign(nationality = nationality.text)
                
                case_type = driver.find_element_by_xpath("//tbody/tr[@title = 'Case Type']/td")
                current_anumber_data = current_anumber_data.assign(case_type = case_type.text)
                
                rider_relationship = driver.find_element_by_xpath("//tbody/tr[@title = 'Rider Relationship']/td")
                current_anumber_data = current_anumber_data.assign(rider_relationship = rider_relationship.text)
                
                rider_indicator = driver.find_element_by_xpath("//tbody/tr[@title = 'Rider Indicator']/td")
                current_anumber_data = current_anumber_data.assign(rider_indicator = rider_indicator.text)
                
                base_city_address_city = driver.find_element_by_xpath("//tbody/tr[@title = 'Base City Address City']/td")
                current_anumber_data = current_anumber_data.assign(base_city_address_city = base_city_address_city.text)
                
                charging_document_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Charging Document Date']/td")
                current_anumber_data = current_anumber_data.assign(charging_document_date = charging_document_date.text)
                
                case_input_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Case Input Date']/td")
                current_anumber_data = current_anumber_data.assign(case_input_date = case_input_date.text)
                
                initial_hearing_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Initial Hearing Date']/td")
                current_anumber_data = current_anumber_data.assign(initial_hearing_date = initial_hearing_date.text)
                
                proceeding_received_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Proceeding Received Date']/td")
                current_anumber_data = current_anumber_data.assign(proceeding_received_date = proceeding_received_date.text)
                
                latest_hearing_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Latest Hearing Date']/td")
                current_anumber_data = current_anumber_data.assign(latest_hearing_date = latest_hearing_date.text)
                
                latest_hearing_calendar_type = driver.find_element_by_xpath("//tbody/tr[@title = 'Latest Hearing Calendar Type']/td")
                current_anumber_data = current_anumber_data.assign(latest_hearing_calendar_type = latest_hearing_calendar_type.text)
                
                custody_status = driver.find_element_by_xpath("//tbody/tr[@title = 'Custody Status']/td")
                current_anumber_data = current_anumber_data.assign(custody_status = custody_status.text)
                
                received_at_eoir_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Received at EOIR Date']/td")
                current_anumber_data = current_anumber_data.assign(received_at_eoir_date = received_at_eoir_date.text)
                
                ij_decision = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ Decision']/td")
                current_anumber_data = current_anumber_data.assign(ij_decision = ij_decision.text)
                
                ij_decision_date = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ Decision Date']/td")
                current_anumber_data = current_anumber_data.assign(ij_decision_date = ij_decision_date.text)
                
                ij_other_completion = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ Other Completion']/td")
                current_anumber_data = current_anumber_data.assign(ij_other_completion = ij_other_completion.text)
                
                a212c_filed = driver.find_element_by_xpath("//tbody/tr[@title = '212C Filed']/td")
                current_anumber_data = current_anumber_data.assign(a212c_filed = a212c_filed.text)
                
                a212c_decision = driver.find_element_by_xpath("//tbody/tr[@title = '212C Decision']/td")
                current_anumber_data = current_anumber_data.assign(a212c_decision = a212c_decision.text)
                
                a245adj_filed = driver.find_element_by_xpath("//tbody/tr[@title = '245Adj Filed']/td")
                current_anumber_data = current_anumber_data.assign(a245adj_filed = a245adj_filed.text)
                
                a245adj_decision = driver.find_element_by_xpath("//tbody/tr[@title = '245Adj Decision']/td")
                current_anumber_data = current_anumber_data.assign(a245adj_decision = a245adj_decision.text)
                
                voluntary_departure_filed = driver.find_element_by_xpath("//tbody/tr[@title = 'Voluntary Departure Filed']/td")
                current_anumber_data = current_anumber_data.assign(voluntary_departure_filed = voluntary_departure_filed.text)
                
                voluntary_departure_decision = driver.find_element_by_xpath("//tbody/tr[@title = 'Voluntary Departure Decision']/td")
                current_anumber_data = current_anumber_data.assign(voluntary_departure_decision = voluntary_departure_decision.text)
                
                suspension_filed = driver.find_element_by_xpath("//tbody/tr[@title = 'Suspension Filed']/td")
                current_anumber_data = current_anumber_data.assign(suspension_filed = suspension_filed.text)
                
                suspension_decision = driver.find_element_by_xpath("//tbody/tr[@title = 'Suspension Decision']/td")
                current_anumber_data = current_anumber_data.assign(suspension_decision = suspension_decision.text)
                
                ij_mtr_received_date = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ MTR Received Date']/td")
                current_anumber_data = current_anumber_data.assign(ij_mtr_received_date = ij_mtr_received_date.text)
                
                ij_mtr_decision = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ MTR Decision']/td")
                current_anumber_data = current_anumber_data.assign(ij_mtr_decision = ij_mtr_decision.text)
                
                ij_mtr_decision_date = driver.find_element_by_xpath("//tbody/tr[@title = 'IJ MTR Decision Date']/td")
                current_anumber_data = current_anumber_data.assign(ij_mtr_decision_date = ij_mtr_decision_date.text)
                
                appeal_filed = driver.find_element_by_xpath("//tbody/tr[@title = 'Appeal Filed']/td")
                current_anumber_data = current_anumber_data.assign(appeal_filed = appeal_filed.text)
                
                board_decision = driver.find_element_by_xpath("//tbody/tr[@title = 'Board Decision']/td")
                current_anumber_data = current_anumber_data.assign(board_decision = board_decision.text)
                
                board_decision_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Board Decision Date']/td")
                current_anumber_data = current_anumber_data.assign(board_decision_date = board_decision_date.text)
                
                alien_address_street_line_1 = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Address Street Line 1']/td")
                current_anumber_data = current_anumber_data.assign(alien_address_street_line_1 = alien_address_street_line_1.text)
                
                alien_city_state_zip = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien City-State-Zip']/td")
                current_anumber_data = current_anumber_data.assign(alien_city_state_zip = alien_city_state_zip.text)
                
                alien_address_changed_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Address Changed Date']/td")
                current_anumber_data = current_anumber_data.assign(alien_address_changed_date = alien_address_changed_date.text)
                
                alien_address_latest_changed_date = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Address Latest Changed Date']/td")
                current_anumber_data = current_anumber_data.assign(alien_address_latest_changed_date = alien_address_latest_changed_date.text)
                
                alien_phone_number = driver.find_element_by_xpath("//tbody/tr[@title = 'Alien Phone']/td")
                current_anumber_data = current_anumber_data.assign(alien_phone_number = alien_phone_number.text)
                
                atty_rep_name = driver.find_element_by_xpath("//tbody/tr[@title = 'Atty/Rep Name']/td")
                current_anumber_data = current_anumber_data.assign(atty_rep_name = atty_rep_name.text)
                
                atty_rep_address = driver.find_element_by_xpath("//tbody/tr[@title = 'Atty/Rep Address']/td")
                current_anumber_data = current_anumber_data.assign(atty_rep_address = atty_rep_address.text)
                
                board_atty_rep_name = driver.find_element_by_xpath("//tbody/tr[@title = 'Board Atty/Rep Name']/td")
                current_anumber_data = current_anumber_data.assign(board_atty_rep_name = board_atty_rep_name.text)
                
                board_atty_rep_city_state_zip = driver.find_element_by_xpath("//tbody/tr[@title = 'Board Atty/Rep City-State-Zip']/td")
                current_anumber_data = current_anumber_data.assign(board_atty_rep_city_state_zip = board_atty_rep_city_state_zip.text)
                
                current_anumber_data = current_anumber_data.assign(pcqs_eoir_results_found = 1,
                                                                   current_tab_number = current_tab_number,
                                                                   current_anumber_set = current_anumber_set_number)
                
                
                ############################
                
                
                # apply convert_blank_to_na to all variables
                current_anumber_data = current_anumber_data.apply(lambda x: pd.Series(list(x.values.flat)).apply(convert_blank_to_na))
                
                # append current_anumber_data to data
                data = data.append(current_anumber_data)
                
                # back out to the results screen
                driver.back()
                

        ######################################        

        
        # loop through tabs going back to home page
        for current_tab_number in list(range(0, total_tab_count)) :

                # switch to current_tab
                driver.switch_to.window(driver.window_handles[current_tab_number])
                
                # continue to next iteration if at the end of current_anumber_batch, and so current_tab_number exceeds current_anumber_set rows
                if((current_tab_number + 1) > current_anumber_set.shape[0]) :
                        continue
                
                # go back to home page
                driver.back()
                
                
        #######################################
                
        
        # loop through tabs clearing anumber search field
        for current_tab_number in list(range(0, total_tab_count)) :
                
                # switch to current_tab
                driver.switch_to.window(driver.window_handles[current_tab_number])
                
                # continue to next iteration if at the end of current_anumber_batch, and so current_tab_number exceeds current_anumber_set rows
                if((current_tab_number + 1) > current_anumber_set.shape[0]) :
                        continue
                
                # clear the anumber from the anumber search field
                driver.find_element_by_xpath("//input[@name = 'idValue']").clear()


# print finish time
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Finish time =", current_time)


##########################################################################################################################
 
        
# read in currently saved eoir_data_from_pcqs data 
eoir_data_from_pcqs = pd.read_csv("eoir_data_from_pcqs.csv")
print(eoir_data_from_pcqs.shape)
print(data.shape)

# if vpn closed, resulting in no pcqs access and lots of anumbers "not found", drop NA records
# data = data.query("alien_full_name == alien_full_name & received_at_eoir_date == received_at_eoir_date")

# append current data to eoir_data_from_pcqs
eoir_data_from_pcqs = eoir_data_from_pcqs.append(data)
print(eoir_data_from_pcqs.shape)

# save eoir_data_from_pcqs
eoir_data_from_pcqs.to_csv("eoir_data_from_pcqs.csv", index = False)


####################
               
                
# inspect data
data.shape
print(data.filter(["anumber", "alien_full_name", "pcqs_eoir_results_found"]).to_string())
print(data.query("alien_full_name != alien_full_name").filter(["anumber", "alien_full_name", "pcqs_eoir_results_found"]).to_string())
print(data.query("alien_full_name == alien_full_name").filter(["anumber", "alien_full_name", "pcqs_eoir_results_found"]).drop_duplicates().to_string())
data.query("alien_full_name != alien_full_name & received_at_eoir_date != received_at_eoir_date").\
        filter(["anumber", "alien_full_name", "pcqs_eoir_results_found"]).shape

print(data.query("ij_decision_date != ij_decision_date").
      query("pcqs_eoir_results_found == 1").filter(["anumber", "alien_full_name", "ij_decision_date", "pcqs_eoir_results_found"]).to_string())
print(data.query("appeal_filed == appeal_filed").filter(["anumber", "appeal_filed", "board_decision"]).to_string())
