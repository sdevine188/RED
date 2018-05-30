library(dplyr)
library(stringr)
library(readr)
library(janitor)
library(lubridate)
library(rlang)
library(readxl)

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")

# look at file names
list.files()

# read in pre_interview
control_pre_interview_dummies <- read_csv("output/pre_interview/full/control_pre_interview_full_w_terminal_nodes_Case_OutcomeDenied.csv")
dim(control_pre_interview_dummies)
glimpse(control_pre_interview_dummies)

# read in treatment_pre_interview_dummies
treatment_pre_interview_dummies <- read_csv("output/pre_interview/full/treatment_pre_interview_full_w_terminal_nodes_Case_OutcomeDenied.csv")
dim(treatment_pre_interview_dummies)
glimpse(treatment_pre_interview_dummies)

# add weights to treatment_pre_interviwe_dummies
treatment_pre_interview_dummies <- treatment_pre_interview_dummies %>% 
        mutate(weight = case_when(ben_country_of_birth_groupedMEXIC == 1 ~ ben_country_of_birth_groupedMEXIC * 1.24, 
                                                                                                 ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                                                                                                 ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1))
treatment_pre_interview_dummies %>% distinct(weight)


######################################################################


# compare treatment and control for each terminal node on pre-interview denied decision tree

# read in pre_interview denied decision tree
tree_output <- read_csv("./output/pre_interview/full/treatment_pre_interview_full_table_Case_OutcomeDenied.csv")
glimpse(tree_output)
tree_output %>% distinct(terminal_node)
tree_output %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                                        lower_limit_p_weighted, upper_limit_p_weighted,
                                                                        obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                                        unreliable_p_weighted, suppressed_p_weighted) %>% 
                                                                        data.frame()

# create placeholder df for comparison output
treatment_control_comparison_output <- tibble()

# get baseline denial rate in control
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

# read in pre_interview denied surrogate_output
# note there were no surrogate splits used
surrogate_output <- read_csv("./output/pre_interview/train/treatment_pre_interview_train_surrogate_output_Case_OutcomeDenied.csv")
glimpse(surrogate_output)
surrogate_output %>% data.frame()


################################################################


# terminal_node 6
# not significant difference btw treatment/control
terminal_node_value <- 6

# inspect terminal node
tree_output %>% filter(terminal_node == terminal_node_value) %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                      lower_limit_p_weighted, upper_limit_p_weighted,
                                                      obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                      unreliable_p_weighted, suppressed_p_weighted) %>% data.frame()

node_path <- tree_output %>% filter(terminal_node == terminal_node_value) %>% pull(node_path)
node_path

# get node_path_expr for tidy filtering
node_path_expr <- parse_expr(node_path)

# inspect NA values of splits for treatment
treatment_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

# inspect NA values of splits for control
control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% 
        select(count_ben_marriages, type_ben_immig_proceed_groupRemoval, terminal_node, ben_immig_proceedN, ben_immig_proceedY, type_ben_immig_proceed_groupNA_value,
               PET_STATENE, BEN_STATENE)

control_pre_interview_dummies %>% filter(count_ben_marriages < 0.0165) %>% count(type_ben_immig_proceed_groupRemoval)
control_pre_interview_dummies %>% filter(count_ben_marriages < 0.0165, is.na(type_ben_immig_proceed_groupRemoval)) %>%
        select(count_ben_marriages, type_ben_immig_proceed_groupRemoval, terminal_node)


#######


# calculate wilson interval for treatment
n1 <- treatment_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% 
        summarize(weighted_n = sum(weight)) %>% pull(weighted_n)
n1
x1 <- treatment_pre_interview_dummies %>% 
        filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        summarize(weighted_n_denied = sum(weight)) %>%
        pull(weighted_n_denied)
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1

# calculate wilson interval for control
n2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% nrow()
n2
x2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2

# use newcombe formula to get confidence interval for difference in treatment/control proportions
p_diff <- p1 - p2
p_diff

p_diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
p_diff_lower_limit 

p_diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
p_diff_upper_limit


#####################################################


# calculate whether control domain is significantly different than overall control average
# get wilson score bounds for control_complement
control_complement_n <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value) %>% nrow()
control_complement_n
control_complement_positive <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
control_complement_positive

control_complement_p <- control_complement_positive / control_complement_n
control_complement_p 

z <- 1.96

control_complement_upper_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) + 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_upper_limit

control_complement_lower_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) - 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_lower_limit


##############################################################


# calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
control_complement_diff <- p2 - control_complement_p
control_complement_diff

control_complement_diff_lower_limit <- (p2 - control_complement_p) - 
        sqrt((p2 - lower2)^2 + (control_complement_upper_limit - control_complement_p)^2)
control_complement_diff_lower_limit

control_complement_diff_upper_limit <- (p2 - control_complement_p) + 
        sqrt((upper2 - p2)^2 + (control_complement_p - control_complement_lower_limit)^2)
control_complement_diff_upper_limit


##################################################


# now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

control_p_diff_overall <- (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff 
control_p_diff_overall 

control_p_diff_overall_lower_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_lower_limit
control_p_diff_overall_lower_limit
control_p_diff_overall_upper_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_upper_limit
control_p_diff_overall_upper_limit


####################################################


# write output to treatment_control_comparison_output
node_comparison_output <- tibble(terminal_node = terminal_node_value, node_path = node_path, treatment_n = n1, treatment_positive = x1, treatment_p = p1,
                                 treatment_upper_limit = upper1, treatment_lower_limit = lower1, control_n = n2, control_positive = x2, control_p = p2,
                                 control_upper_limit = upper2, control_lower_limit = lower2,
                                 p_diff = p_diff, p_diff_lower_limit = p_diff_lower_limit, p_diff_upper_limit = p_diff_upper_limit,
                                 control_complement_n = control_complement_n, control_complement_p = control_complement_p,
                                 control_complement_lower_limit = control_complement_lower_limit, 
                                 control_complement_upper_limit = control_complement_upper_limit, 
                                 control_complement_diff = control_complement_diff,
                                 control_complement_diff_lower_limit = control_complement_diff_lower_limit,
                                 control_complement_diff_upper_limit = control_complement_diff_upper_limit,
                                 control_p_diff_overall = control_p_diff_overall, 
                                 control_p_diff_overall_lower_limit = control_p_diff_overall_lower_limit,
                                 control_p_diff_overall_upper_limit = control_p_diff_overall_upper_limit)

node_comparison_output <- node_comparison_output %>% mutate(p_diff_significant = case_when(p_diff_lower_limit > 0 & p_diff_upper_limit > 0 |
                p_diff_lower_limit < 0 & p_diff_upper_limit < 0 ~ 1, TRUE ~ 0),
    control_p_diff_overall_significant = case_when(control_p_diff_overall_lower_limit > 0 & 
                                        control_p_diff_overall_upper_limit > 0 |
                                          control_p_diff_overall_lower_limit < 0 & 
                                                control_p_diff_overall_upper_limit < 0 ~ 1, TRUE ~ 0))
node_comparison_output %>% data.frame()



# add coefficient of variation
node_comparison_output <- node_comparison_output %>%
        mutate(treatment_se = ((treatment_upper_limit - treatment_lower_limit) / (1.96 * 2)), 
               is_treatment_p_the_min = ifelse(treatment_p < (1 - treatment_p), 1, 0),
               treatment_coeff_of_variation = case_when(is_treatment_p_the_min == 1 ~ treatment_se / (treatment_p + .001),
                                              is_treatment_p_the_min == 0 ~ treatment_se / ((1 - treatment_p) + .001), TRUE ~ 0),
               unreliable_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation >= .3 &
                                                      treatment_coeff_of_variation <= .5, 1, 0),
               suppressed_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation > .5 | treatment_n < 10, 1, 0),
               
               control_se = ((control_upper_limit - control_lower_limit) / (1.96 * 2)), 
               is_control_p_the_min = ifelse(control_p < (1 - control_p), 1, 0),
               control_coeff_of_variation = case_when(is_control_p_the_min == 1 ~ control_se / (control_p + .001),
                                                        is_control_p_the_min == 0 ~ control_se / ((1 - control_p) + .001), TRUE ~ 0),
               unreliable_control_p = ifelse(control_n < 15 & control_coeff_of_variation >= .3 &
                                                       control_coeff_of_variation <= .5, 1, 0),
               suppressed_control_p = ifelse(control_n < 15 & control_coeff_of_variation > .5 | control_n < 10, 1, 0),
               
               suppressed_p_diff = ifelse(suppressed_treatment_p == 1 | suppressed_control_p == 1, 1, 0),
               unreliable_p_diff = case_when(suppressed_treatment_p == 1 | suppressed_control_p == 1 ~ 0, 
                                             unreliable_treatment_p == 1 | unreliable_control_p ~ 1, TRUE ~ 0))

node_comparison_output %>% data.frame()

treatment_control_comparison_output <- bind_rows(treatment_control_comparison_output, node_comparison_output)
treatment_control_comparison_output


###########################################################################
##########################################################################
#########################################################################


# terminal_node 6
# not significant difference btw treatment/control
terminal_node_value <- 4

# inspect terminal node
tree_output %>% filter(terminal_node == terminal_node_value) %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                                        lower_limit_p_weighted, upper_limit_p_weighted,
                                                                        obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                                        unreliable_p_weighted, suppressed_p_weighted) %>% data.frame()

node_path <- tree_output %>% filter(terminal_node == terminal_node_value) %>% pull(node_path)
node_path

# get node_path_expr for tidy filtering
node_path_expr <- parse_expr(node_path)

# inspect NA values of splits for treatment
treatment_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

# inspect NA values of splits for control
control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()


#######


# calculate wilson interval for treatment
n1 <- treatment_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% 
        summarize(weighted_n = sum(weight)) %>% pull(weighted_n)
n1
x1 <- treatment_pre_interview_dummies %>% 
        filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        summarize(weighted_n_denied = sum(weight)) %>%
        pull(weighted_n_denied)
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1

# calculate wilson interval for control
n2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% nrow()
n2
x2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2

# use newcombe formula to get confidence interval for difference in treatment/control proportions
p_diff <- p1 - p2
p_diff

p_diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
p_diff_lower_limit 

p_diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
p_diff_upper_limit


#####################################################


# calculate whether control domain is significantly different than overall control average
# get wilson score bounds for control_complement
control_complement_n <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value) %>% nrow()
control_complement_n
control_complement_positive <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
control_complement_positive

control_complement_p <- control_complement_positive / control_complement_n
control_complement_p 

z <- 1.96

control_complement_upper_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) + 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_upper_limit

control_complement_lower_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) - 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_lower_limit


##############################################################


# calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
control_complement_diff <- p2 - control_complement_p
control_complement_diff

control_complement_diff_lower_limit <- (p2 - control_complement_p) - 
        sqrt((p2 - lower2)^2 + (control_complement_upper_limit - control_complement_p)^2)
control_complement_diff_lower_limit

control_complement_diff_upper_limit <- (p2 - control_complement_p) + 
        sqrt((upper2 - p2)^2 + (control_complement_p - control_complement_lower_limit)^2)
control_complement_diff_upper_limit


##################################################


# now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

control_p_diff_overall <- (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff 
control_p_diff_overall 

control_p_diff_overall_lower_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_lower_limit
control_p_diff_overall_lower_limit
control_p_diff_overall_upper_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_upper_limit
control_p_diff_overall_upper_limit


####################################################


# write output to treatment_control_comparison_output
node_comparison_output <- tibble(terminal_node = terminal_node_value, node_path = node_path, treatment_n = n1, treatment_positive = x1, treatment_p = p1,
                                 treatment_upper_limit = upper1, treatment_lower_limit = lower1, control_n = n2, control_positive = x2, control_p = p2,
                                 control_upper_limit = upper2, control_lower_limit = lower2,
                                 p_diff = p_diff, p_diff_lower_limit = p_diff_lower_limit, p_diff_upper_limit = p_diff_upper_limit,
                                 control_complement_n = control_complement_n, control_complement_p = control_complement_p,
                                 control_complement_lower_limit = control_complement_lower_limit, 
                                 control_complement_upper_limit = control_complement_upper_limit, 
                                 control_complement_diff = control_complement_diff,
                                 control_complement_diff_lower_limit = control_complement_diff_lower_limit,
                                 control_complement_diff_upper_limit = control_complement_diff_upper_limit,
                                 control_p_diff_overall = control_p_diff_overall, 
                                 control_p_diff_overall_lower_limit = control_p_diff_overall_lower_limit,
                                 control_p_diff_overall_upper_limit = control_p_diff_overall_upper_limit)

node_comparison_output <- node_comparison_output %>% mutate(p_diff_significant = case_when(p_diff_lower_limit > 0 & p_diff_upper_limit > 0 |
                                                                                                   p_diff_lower_limit < 0 & p_diff_upper_limit < 0 ~ 1, TRUE ~ 0),
                                                            control_p_diff_overall_significant = case_when(control_p_diff_overall_lower_limit > 0 & 
                                                                                                                   control_p_diff_overall_upper_limit > 0 |
                                                                                                                   control_p_diff_overall_lower_limit < 0 & 
                                                                                                                   control_p_diff_overall_upper_limit < 0 ~ 1, TRUE ~ 0))
node_comparison_output %>% data.frame()



# add coefficient of variation
node_comparison_output <- node_comparison_output %>%
        mutate(treatment_se = ((treatment_upper_limit - treatment_lower_limit) / (1.96 * 2)), 
               is_treatment_p_the_min = ifelse(treatment_p < (1 - treatment_p), 1, 0),
               treatment_coeff_of_variation = case_when(is_treatment_p_the_min == 1 ~ treatment_se / (treatment_p + .001),
                                                        is_treatment_p_the_min == 0 ~ treatment_se / ((1 - treatment_p) + .001), TRUE ~ 0),
               unreliable_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation >= .3 &
                                                       treatment_coeff_of_variation <= .5, 1, 0),
               suppressed_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation > .5 | treatment_n < 10, 1, 0),
               
               control_se = ((control_upper_limit - control_lower_limit) / (1.96 * 2)), 
               is_control_p_the_min = ifelse(control_p < (1 - control_p), 1, 0),
               control_coeff_of_variation = case_when(is_control_p_the_min == 1 ~ control_se / (control_p + .001),
                                                      is_control_p_the_min == 0 ~ control_se / ((1 - control_p) + .001), TRUE ~ 0),
               unreliable_control_p = ifelse(control_n < 15 & control_coeff_of_variation >= .3 &
                                                     control_coeff_of_variation <= .5, 1, 0),
               suppressed_control_p = ifelse(control_n < 15 & control_coeff_of_variation > .5 | control_n < 10, 1, 0),
               
               suppressed_p_diff = ifelse(suppressed_treatment_p == 1 | suppressed_control_p == 1, 1, 0),
               unreliable_p_diff = case_when(suppressed_treatment_p == 1 | suppressed_control_p == 1 ~ 0, 
                                             unreliable_treatment_p == 1 | unreliable_control_p ~ 1, TRUE ~ 0))

node_comparison_output %>% data.frame()

treatment_control_comparison_output <- bind_rows(treatment_control_comparison_output, node_comparison_output)
treatment_control_comparison_output


###########################################################################
##########################################################################
#########################################################################


# terminal_node 10
# not significant difference btw treatment/control
terminal_node_value <- 10

# inspect terminal node
tree_output %>% filter(terminal_node == terminal_node_value) %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                                        lower_limit_p_weighted, upper_limit_p_weighted,
                                                                        obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                                        unreliable_p_weighted, suppressed_p_weighted) %>% data.frame()

node_path <- tree_output %>% filter(terminal_node == terminal_node_value) %>% pull(node_path)
node_path

# get node_path_expr for tidy filtering
node_path_expr <- parse_expr(node_path)

# inspect NA values of splits for treatment
treatment_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

# inspect NA values of splits for control
control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()


#######


# calculate wilson interval for treatment
n1 <- treatment_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% 
        summarize(weighted_n = sum(weight)) %>% pull(weighted_n)
n1
x1 <- treatment_pre_interview_dummies %>% 
        filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        summarize(weighted_n_denied = sum(weight)) %>%
        pull(weighted_n_denied)
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1

# calculate wilson interval for control
n2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% nrow()
n2
x2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2

# use newcombe formula to get confidence interval for difference in treatment/control proportions
p_diff <- p1 - p2
p_diff

p_diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
p_diff_lower_limit 

p_diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
p_diff_upper_limit


#####################################################


# calculate whether control domain is significantly different than overall control average
# get wilson score bounds for control_complement
control_complement_n <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value) %>% nrow()
control_complement_n
control_complement_positive <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
control_complement_positive

control_complement_p <- control_complement_positive / control_complement_n
control_complement_p 

z <- 1.96

control_complement_upper_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) + 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_upper_limit

control_complement_lower_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) - 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_lower_limit


##############################################################


# calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
control_complement_diff <- p2 - control_complement_p
control_complement_diff

control_complement_diff_lower_limit <- (p2 - control_complement_p) - 
        sqrt((p2 - lower2)^2 + (control_complement_upper_limit - control_complement_p)^2)
control_complement_diff_lower_limit

control_complement_diff_upper_limit <- (p2 - control_complement_p) + 
        sqrt((upper2 - p2)^2 + (control_complement_p - control_complement_lower_limit)^2)
control_complement_diff_upper_limit


##################################################


# now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

control_p_diff_overall <- (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff 
control_p_diff_overall 

control_p_diff_overall_lower_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_lower_limit
control_p_diff_overall_lower_limit
control_p_diff_overall_upper_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_upper_limit
control_p_diff_overall_upper_limit


####################################################


# write output to treatment_control_comparison_output
node_comparison_output <- tibble(terminal_node = terminal_node_value, node_path = node_path, treatment_n = n1, treatment_positive = x1, treatment_p = p1,
                                 treatment_upper_limit = upper1, treatment_lower_limit = lower1, control_n = n2, control_positive = x2, control_p = p2,
                                 control_upper_limit = upper2, control_lower_limit = lower2,
                                 p_diff = p_diff, p_diff_lower_limit = p_diff_lower_limit, p_diff_upper_limit = p_diff_upper_limit,
                                 control_complement_n = control_complement_n, control_complement_p = control_complement_p,
                                 control_complement_lower_limit = control_complement_lower_limit, 
                                 control_complement_upper_limit = control_complement_upper_limit, 
                                 control_complement_diff = control_complement_diff,
                                 control_complement_diff_lower_limit = control_complement_diff_lower_limit,
                                 control_complement_diff_upper_limit = control_complement_diff_upper_limit,
                                 control_p_diff_overall = control_p_diff_overall, 
                                 control_p_diff_overall_lower_limit = control_p_diff_overall_lower_limit,
                                 control_p_diff_overall_upper_limit = control_p_diff_overall_upper_limit)

node_comparison_output <- node_comparison_output %>% mutate(p_diff_significant = case_when(p_diff_lower_limit > 0 & p_diff_upper_limit > 0 |
                                                                                                   p_diff_lower_limit < 0 & p_diff_upper_limit < 0 ~ 1, TRUE ~ 0),
                                                            control_p_diff_overall_significant = case_when(control_p_diff_overall_lower_limit > 0 & 
                                                                                                                   control_p_diff_overall_upper_limit > 0 |
                                                                                                                   control_p_diff_overall_lower_limit < 0 & 
                                                                                                                   control_p_diff_overall_upper_limit < 0 ~ 1, TRUE ~ 0))
node_comparison_output %>% data.frame()



# add coefficient of variation
node_comparison_output <- node_comparison_output %>%
        mutate(treatment_se = ((treatment_upper_limit - treatment_lower_limit) / (1.96 * 2)), 
               is_treatment_p_the_min = ifelse(treatment_p < (1 - treatment_p), 1, 0),
               treatment_coeff_of_variation = case_when(is_treatment_p_the_min == 1 ~ treatment_se / (treatment_p + .001),
                                                        is_treatment_p_the_min == 0 ~ treatment_se / ((1 - treatment_p) + .001), TRUE ~ 0),
               unreliable_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation >= .3 &
                                                       treatment_coeff_of_variation <= .5, 1, 0),
               suppressed_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation > .5 | treatment_n < 10, 1, 0),
               
               control_se = ((control_upper_limit - control_lower_limit) / (1.96 * 2)), 
               is_control_p_the_min = ifelse(control_p < (1 - control_p), 1, 0),
               control_coeff_of_variation = case_when(is_control_p_the_min == 1 ~ control_se / (control_p + .001),
                                                      is_control_p_the_min == 0 ~ control_se / ((1 - control_p) + .001), TRUE ~ 0),
               unreliable_control_p = ifelse(control_n < 15 & control_coeff_of_variation >= .3 &
                                                     control_coeff_of_variation <= .5, 1, 0),
               suppressed_control_p = ifelse(control_n < 15 & control_coeff_of_variation > .5 | control_n < 10, 1, 0),
               
               suppressed_p_diff = ifelse(suppressed_treatment_p == 1 | suppressed_control_p == 1, 1, 0),
               unreliable_p_diff = case_when(suppressed_treatment_p == 1 | suppressed_control_p == 1 ~ 0, 
                                             unreliable_treatment_p == 1 | unreliable_control_p ~ 1, TRUE ~ 0))

node_comparison_output %>% data.frame()

treatment_control_comparison_output <- bind_rows(treatment_control_comparison_output, node_comparison_output)
treatment_control_comparison_output


###########################################################################
##########################################################################
#########################################################################


# terminal_node 7
# not significant difference btw treatment/control
terminal_node_value <- 7

# inspect terminal node
tree_output %>% filter(terminal_node == terminal_node_value) %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                                        lower_limit_p_weighted, upper_limit_p_weighted,
                                                                        obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                                        unreliable_p_weighted, suppressed_p_weighted) %>% data.frame()

node_path <- tree_output %>% filter(terminal_node == terminal_node_value) %>% pull(node_path)
node_path

# get node_path_expr for tidy filtering
node_path_expr <- parse_expr(node_path)

# inspect NA values of splits for treatment
treatment_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

# inspect NA values of splits for control
control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()


#######


# calculate wilson interval for treatment
n1 <- treatment_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% 
        summarize(weighted_n = sum(weight)) %>% pull(weighted_n)
n1
x1 <- treatment_pre_interview_dummies %>% 
        filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        summarize(weighted_n_denied = sum(weight)) %>%
        pull(weighted_n_denied)
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1

# calculate wilson interval for control
n2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% nrow()
n2
x2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2

# use newcombe formula to get confidence interval for difference in treatment/control proportions
p_diff <- p1 - p2
p_diff

p_diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
p_diff_lower_limit 

p_diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
p_diff_upper_limit


#####################################################


# calculate whether control domain is significantly different than overall control average
# get wilson score bounds for control_complement
control_complement_n <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value) %>% nrow()
control_complement_n
control_complement_positive <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
control_complement_positive

control_complement_p <- control_complement_positive / control_complement_n
control_complement_p 

z <- 1.96

control_complement_upper_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) + 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_upper_limit

control_complement_lower_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) - 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_lower_limit


##############################################################


# calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
control_complement_diff <- p2 - control_complement_p
control_complement_diff

control_complement_diff_lower_limit <- (p2 - control_complement_p) - 
        sqrt((p2 - lower2)^2 + (control_complement_upper_limit - control_complement_p)^2)
control_complement_diff_lower_limit

control_complement_diff_upper_limit <- (p2 - control_complement_p) + 
        sqrt((upper2 - p2)^2 + (control_complement_p - control_complement_lower_limit)^2)
control_complement_diff_upper_limit


##################################################


# now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

control_p_diff_overall <- (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff 
control_p_diff_overall 

control_p_diff_overall_lower_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_lower_limit
control_p_diff_overall_lower_limit
control_p_diff_overall_upper_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_upper_limit
control_p_diff_overall_upper_limit


####################################################


# write output to treatment_control_comparison_output
node_comparison_output <- tibble(terminal_node = terminal_node_value, node_path = node_path, treatment_n = n1, treatment_positive = x1, treatment_p = p1,
                                 treatment_upper_limit = upper1, treatment_lower_limit = lower1, control_n = n2, control_positive = x2, control_p = p2,
                                 control_upper_limit = upper2, control_lower_limit = lower2,
                                 p_diff = p_diff, p_diff_lower_limit = p_diff_lower_limit, p_diff_upper_limit = p_diff_upper_limit,
                                 control_complement_n = control_complement_n, control_complement_p = control_complement_p,
                                 control_complement_lower_limit = control_complement_lower_limit, 
                                 control_complement_upper_limit = control_complement_upper_limit, 
                                 control_complement_diff = control_complement_diff,
                                 control_complement_diff_lower_limit = control_complement_diff_lower_limit,
                                 control_complement_diff_upper_limit = control_complement_diff_upper_limit,
                                 control_p_diff_overall = control_p_diff_overall, 
                                 control_p_diff_overall_lower_limit = control_p_diff_overall_lower_limit,
                                 control_p_diff_overall_upper_limit = control_p_diff_overall_upper_limit)

node_comparison_output <- node_comparison_output %>% mutate(p_diff_significant = case_when(p_diff_lower_limit > 0 & p_diff_upper_limit > 0 |
                                                                                                   p_diff_lower_limit < 0 & p_diff_upper_limit < 0 ~ 1, TRUE ~ 0),
                                                            control_p_diff_overall_significant = case_when(control_p_diff_overall_lower_limit > 0 & 
                                                                                                                   control_p_diff_overall_upper_limit > 0 |
                                                                                                                   control_p_diff_overall_lower_limit < 0 & 
                                                                                                                   control_p_diff_overall_upper_limit < 0 ~ 1, TRUE ~ 0))
node_comparison_output %>% data.frame()



# add coefficient of variation
node_comparison_output <- node_comparison_output %>%
        mutate(treatment_se = ((treatment_upper_limit - treatment_lower_limit) / (1.96 * 2)), 
               is_treatment_p_the_min = ifelse(treatment_p < (1 - treatment_p), 1, 0),
               treatment_coeff_of_variation = case_when(is_treatment_p_the_min == 1 ~ treatment_se / (treatment_p + .001),
                                                        is_treatment_p_the_min == 0 ~ treatment_se / ((1 - treatment_p) + .001), TRUE ~ 0),
               unreliable_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation >= .3 &
                                                       treatment_coeff_of_variation <= .5, 1, 0),
               suppressed_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation > .5 | treatment_n < 10, 1, 0),
               
               control_se = ((control_upper_limit - control_lower_limit) / (1.96 * 2)), 
               is_control_p_the_min = ifelse(control_p < (1 - control_p), 1, 0),
               control_coeff_of_variation = case_when(is_control_p_the_min == 1 ~ control_se / (control_p + .001),
                                                      is_control_p_the_min == 0 ~ control_se / ((1 - control_p) + .001), TRUE ~ 0),
               unreliable_control_p = ifelse(control_n < 15 & control_coeff_of_variation >= .3 &
                                                     control_coeff_of_variation <= .5, 1, 0),
               suppressed_control_p = ifelse(control_n < 15 & control_coeff_of_variation > .5 | control_n < 10, 1, 0),
               
               suppressed_p_diff = ifelse(suppressed_treatment_p == 1 | suppressed_control_p == 1, 1, 0),
               unreliable_p_diff = case_when(suppressed_treatment_p == 1 | suppressed_control_p == 1 ~ 0, 
                                             unreliable_treatment_p == 1 | unreliable_control_p ~ 1, TRUE ~ 0))

node_comparison_output %>% data.frame()

treatment_control_comparison_output <- bind_rows(treatment_control_comparison_output, node_comparison_output)
treatment_control_comparison_output


###########################################################################
##########################################################################
#########################################################################


# terminal_node 11
# not significant difference btw treatment/control
terminal_node_value <- 11

# inspect terminal node
tree_output %>% filter(terminal_node == terminal_node_value) %>% select(terminal_node, node_path, terminal_node, node_path, p_weighted, 
                                                                        lower_limit_p_weighted, upper_limit_p_weighted,
                                                                        obs_in_node, outcome_positive_sum, pct_obs_in_node, validated_significant_test_terminal_node,
                                                                        unreliable_p_weighted, suppressed_p_weighted) %>% data.frame()

node_path <- tree_output %>% filter(terminal_node == terminal_node_value) %>% pull(node_path)
node_path

# get node_path_expr for tidy filtering
node_path_expr <- parse_expr(node_path)

# inspect NA values of splits for treatment
treatment_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()

# inspect NA values of splits for control
control_pre_interview_dummies %>% mutate(in_current_node_path = case_when(!!node_path_expr ~ 1, TRUE ~ 0)) %>% 
        filter(!(in_current_node_path == 1), terminal_node == terminal_node_value) %>% dim()


#######


# calculate wilson interval for treatment
n1 <- treatment_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% 
        summarize(weighted_n = sum(weight)) %>% pull(weighted_n)
n1
x1 <- treatment_pre_interview_dummies %>% 
        filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        summarize(weighted_n_denied = sum(weight)) %>%
        pull(weighted_n_denied)
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1

# calculate wilson interval for control
n2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value) %>% nrow()
n2
x2 <- control_pre_interview_dummies %>% filter(terminal_node == terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2

# use newcombe formula to get confidence interval for difference in treatment/control proportions
p_diff <- p1 - p2
p_diff

p_diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
p_diff_lower_limit 

p_diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
p_diff_upper_limit


#####################################################


# calculate whether control domain is significantly different than overall control average
# get wilson score bounds for control_complement
control_complement_n <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value) %>% nrow()
control_complement_n
control_complement_positive <- control_pre_interview_dummies %>% 
        filter(terminal_node != terminal_node_value, Case_OutcomeDenied == 1) %>% 
        nrow()
control_complement_positive

control_complement_p <- control_complement_positive / control_complement_n
control_complement_p 

z <- 1.96

control_complement_upper_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) + 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_upper_limit

control_complement_lower_limit <- (1 / (2 * (control_complement_n + z^2)) ) * 
        ( (2 * control_complement_n * control_complement_p + z^2) - 
                  (z * sqrt(4 * control_complement_n * control_complement_p * (1 - control_complement_p) + z^2) ) )
control_complement_lower_limit


##############################################################


# calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
control_complement_diff <- p2 - control_complement_p
control_complement_diff

control_complement_diff_lower_limit <- (p2 - control_complement_p) - 
        sqrt((p2 - lower2)^2 + (control_complement_upper_limit - control_complement_p)^2)
control_complement_diff_lower_limit

control_complement_diff_upper_limit <- (p2 - control_complement_p) + 
        sqrt((upper2 - p2)^2 + (control_complement_p - control_complement_lower_limit)^2)
control_complement_diff_upper_limit


##################################################


# now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
control_pre_interview_dummies %>% tabyl(Case_OutcomeDenied)

control_p_diff_overall <- (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff 
control_p_diff_overall 

control_p_diff_overall_lower_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_lower_limit
control_p_diff_overall_lower_limit
control_p_diff_overall_upper_limit = (1 - (n2 / (n2 + control_complement_n))) * control_complement_diff_upper_limit
control_p_diff_overall_upper_limit


####################################################


# write output to treatment_control_comparison_output
node_comparison_output <- tibble(terminal_node = terminal_node_value, node_path = node_path, treatment_n = n1, treatment_positive = x1, treatment_p = p1,
                                 treatment_upper_limit = upper1, treatment_lower_limit = lower1, control_n = n2, control_positive = x2, control_p = p2,
                                 control_upper_limit = upper2, control_lower_limit = lower2,
                                 p_diff = p_diff, p_diff_lower_limit = p_diff_lower_limit, p_diff_upper_limit = p_diff_upper_limit,
                                 control_complement_n = control_complement_n, control_complement_p = control_complement_p,
                                 control_complement_lower_limit = control_complement_lower_limit, 
                                 control_complement_upper_limit = control_complement_upper_limit, 
                                 control_complement_diff = control_complement_diff,
                                 control_complement_diff_lower_limit = control_complement_diff_lower_limit,
                                 control_complement_diff_upper_limit = control_complement_diff_upper_limit,
                                 control_p_diff_overall = control_p_diff_overall, 
                                 control_p_diff_overall_lower_limit = control_p_diff_overall_lower_limit,
                                 control_p_diff_overall_upper_limit = control_p_diff_overall_upper_limit)

node_comparison_output <- node_comparison_output %>% mutate(p_diff_significant = case_when(p_diff_lower_limit > 0 & p_diff_upper_limit > 0 |
                                                                                                   p_diff_lower_limit < 0 & p_diff_upper_limit < 0 ~ 1, TRUE ~ 0),
                                                            control_p_diff_overall_significant = case_when(control_p_diff_overall_lower_limit > 0 & 
                                                                                                                   control_p_diff_overall_upper_limit > 0 |
                                                                                                                   control_p_diff_overall_lower_limit < 0 & 
                                                                                                                   control_p_diff_overall_upper_limit < 0 ~ 1, TRUE ~ 0))
node_comparison_output %>% data.frame()



# add coefficient of variation
node_comparison_output <- node_comparison_output %>%
        mutate(treatment_se = ((treatment_upper_limit - treatment_lower_limit) / (1.96 * 2)), 
               is_treatment_p_the_min = ifelse(treatment_p < (1 - treatment_p), 1, 0),
               treatment_coeff_of_variation = case_when(is_treatment_p_the_min == 1 ~ treatment_se / (treatment_p + .001),
                                                        is_treatment_p_the_min == 0 ~ treatment_se / ((1 - treatment_p) + .001), TRUE ~ 0),
               unreliable_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation >= .3 &
                                                       treatment_coeff_of_variation <= .5, 1, 0),
               suppressed_treatment_p = ifelse(treatment_n < 15 & treatment_coeff_of_variation > .5 | treatment_n < 10, 1, 0),
               
               control_se = ((control_upper_limit - control_lower_limit) / (1.96 * 2)), 
               is_control_p_the_min = ifelse(control_p < (1 - control_p), 1, 0),
               control_coeff_of_variation = case_when(is_control_p_the_min == 1 ~ control_se / (control_p + .001),
                                                      is_control_p_the_min == 0 ~ control_se / ((1 - control_p) + .001), TRUE ~ 0),
               unreliable_control_p = ifelse(control_n < 15 & control_coeff_of_variation >= .3 &
                                                     control_coeff_of_variation <= .5, 1, 0),
               suppressed_control_p = ifelse(control_n < 15 & control_coeff_of_variation > .5 | control_n < 10, 1, 0),
               
               suppressed_p_diff = ifelse(suppressed_treatment_p == 1 | suppressed_control_p == 1, 1, 0),
               unreliable_p_diff = case_when(suppressed_treatment_p == 1 | suppressed_control_p == 1 ~ 0, 
                                             unreliable_treatment_p == 1 | unreliable_control_p ~ 1, TRUE ~ 0))

node_comparison_output %>% data.frame()

treatment_control_comparison_output <- bind_rows(treatment_control_comparison_output, node_comparison_output)
treatment_control_comparison_output


#########################################################################################################
#######################################################################################################
#########################################################################################################


# write treatment_control_comparison_output to file
treatment_control_comparison_output %>% data.frame()
write_csv(treatment_control_comparison_output, "output/pre_interview/pre_interview_treatment_sample_control_comparison_denied_output.csv")



