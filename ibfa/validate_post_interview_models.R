# validate_post_interview_models

# load post_interview denied model output
getwd()
list.files("./output/post_interview/full")

post_interview_Case_OutcomeDenied <- read_csv("./output/post_interview/full/treatment_post_interview_full_table_Case_OutcomeDenied.csv")
glimpse(post_interview_Case_OutcomeDenied)

# load full treatment file
treatment <-read_csv("Analysis_20180316.csv")
glimpse(treatment)

# load treatment_pre_interview_dummies
treatment_post_interview_dummies <- read_csv("treatment_post_interview_dummies_20180329.csv")
glimpse(treatment_post_interview_dummies)


###############################################################


# set outcome_variable
outcome_variable <- "Case_OutcomeDenied"
outcome_variable_sym <- sym(outcome_variable)

# validate p_weighted
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) 

# node 1
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(1)

treatment_post_interview_dummies %>% mutate(node = case_when(Live_TogetherNo > 0 & IssueNo > 0 & BEN_COUNTRY_OF_BIRTHHAITI > 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())
        # tabyl(node, Case_OutcomeDenied) %>% adorn_totals(c("row", "col")) %>% adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title("combined") 

# node 2
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(2)

treatment_post_interview_dummies %>% mutate(node = case_when(Live_TogetherNo > 0 & IssueNo < 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 3
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(3)

treatment_post_interview_dummies %>% mutate(node = case_when(Live_TogetherNo < 1 & IssueYes > 0 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 4
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(4)

treatment_post_interview_dummies %>% mutate(node = case_when(Live_TogetherNo > 0 & IssueNo > 0 & BEN_COUNTRY_OF_BIRTHHAITI < 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())

# node 5
post_interview_Case_OutcomeDenied %>% select(terminal_node, node_path, p_weighted, obs_in_node, outcome_positive_sum, pct_obs_in_node) %>% slice(5)

treatment_post_interview_dummies %>% mutate(node = case_when(Live_TogetherNo < 1 & IssueYes < 1 ~ 1, TRUE ~ 0)) %>%
        summarize(p_unweighted = sum(node[!!outcome_variable_sym == 1]) / sum(node), obs_in_node = sum(node), outcome_positive_sum = sum(node[!!outcome_variable_sym == 1]),
                  pct_obs_in_node = sum(node) / n())










        