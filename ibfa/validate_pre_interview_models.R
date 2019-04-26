# validate_pre_interview_models

# load pre_interview denied model output
setwd("H:/RED/IBFA/analysis")
getwd()
list.files("./output/pre_interview/full")

pre_interview_Case_OutcomeDenied <- read_csv("./output/pre_interview/full/treatment_pre_interview_full_table_Case_OutcomeDenied.csv")
glimpse(pre_interview_Case_OutcomeDenied)

# load full treatment file
# treatment <- read_csv("Analysis_20180316.csv")
# glimpse(treatment)

# load treatment_pre_interview_dummies
treatment_pre_interview_dummies <- read_csv("treatment_pre_interview_dummies_20180329.csv")
glimpse(treatment_pre_interview_dummies)


###############################################################


# set outcome_variable
outcome_variable <- "Case_OutcomeDenied"
outcome_variable_sym <- sym(outcome_variable)

# validate p_weighted
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>%
        data.frame()

# node 1
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(1)

treatment_pre_interview_dummies %>% mutate(node = case_when(type_ben_immig_proceed_groupREM > 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())
        # tabyl(node, Case_OutcomeDenied) %>% adorn_totals(c("row", "col")) %>% adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title("combined") 

# node 2
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(2) %>%
        data.frame()

treatment_pre_interview_dummies %>% mutate(node = case_when(type_ben_immig_proceed_groupREM < 1 & count_ben_marriages > 0 &
                                        BEN_COUNTRY_OF_BIRTHMEXIC < 1 & pet_get_lpr_via_marriageNO > 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 3
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% 
        slice(3) %>% data.frame()

treatment_pre_interview_dummies %>% mutate(node = case_when(type_ben_immig_proceed_groupREM < 1 & 
                                        count_ben_marriages > 0 & BEN_COUNTRY_OF_BIRTHMEXIC < 1 &
                                        pet_get_lpr_via_marriageNO < 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 4
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% 
        slice(4) %>% data.frame()

treatment_pre_interview_dummies %>% mutate(node = case_when(type_ben_immig_proceed_groupREM < 1 & 
                                        count_ben_marriages < 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 5
pre_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% 
        slice(5) %>% data.frame()

treatment_pre_interview_dummies %>% mutate(node = case_when(type_ben_immig_proceed_groupREM < 1 &
                                count_ben_marriages > 0 & BEN_COUNTRY_OF_BIRTHMEXIC > 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())




#########################################################################
######################################################################
##########################################################################


# load pre_interview denied model output
getwd()
list.files("./output/pre_interview/full")

pre_interview_SOF_Finding_SPFF <- read_csv("./output/pre_interview/full/treatment_pre_interview_full_table_SOF_Finding_SPFF.csv")
glimpse(pre_interview_SOF_Finding_SPFF)

# load full treatment file
# treatment <- read_csv("Analysis_20180316.csv")
# glimpse(treatment)

# load treatment_pre_interview_dummies
treatment_pre_interview_dummies <- read_csv("treatment_pre_interview_dummies_20180329.csv")
glimpse(treatment_pre_interview_dummies)


###############################################################


# set outcome_variable
outcome_variable <- "SOF_Finding_SPFF"
outcome_variable_sym <- sym(outcome_variable)

# validate p_weighted
pre_interview_SOF_Finding_SPFF %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>%
        data.frame()

# node 1
pre_interview_SOF_Finding_SPFF %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(1)

treatment_pre_interview_dummies %>% mutate(node = case_when(time_since_ben_divorceNA_value == 1 & type_ben_immig_proceed_groupREM == 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())
# tabyl(node, Case_OutcomeDenied) %>% adorn_totals(c("row", "col")) %>% adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title("combined") 

# node 2
pre_interview_SOF_Finding_SPFF %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(2)

treatment_pre_interview_dummies %>% mutate(node = case_when(time_since_ben_divorceNA_value == 0 & BEN_COUNTRY_OF_BIRTHMEXIC == 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 3
pre_interview_SOF_Finding_SPFF %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(3)

treatment_pre_interview_dummies %>% mutate(node = case_when(time_since_ben_divorceNA_value == 1 & type_ben_immig_proceed_groupREM == 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())



        