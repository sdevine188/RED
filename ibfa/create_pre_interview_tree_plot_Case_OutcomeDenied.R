library(data.tree)
library(treemap)
library(DiagrammeR)
library(rpart)
library(rpart.plot)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)
library(tibble)
library(tidyr)
library(tidyselect)
library(scales)
library(janitor)
library(readr)
library(forcats)
library(DiagrammeRsvg)
library(RColorBrewer)
library(magick)
library(ggplot2)
library(pdftools)

# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
# http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#graphviz-layouts
# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html
# http://rich-iannone.github.io/DiagrammeR/graphs.html
# https://www.graphviz.org/doc/info/attrs.html

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")


# load as_percent function
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("as_percent.R")
setwd(current_wd)


####################################################


# read model
model <- readRDS(file = "output/pre_interview/train/treatment_pre_interview_train_rpart_caret_model_Case_OutcomeDenied.rds")

# inspect model
model
rpart.plot(model, extra = 104, nn = TRUE)


################################################################


# get clean model output of stats for each node
model_output <- model$frame
model_output_part2 <- data.frame(model$frame$yval2)
glimpse(model_output)
glimpse(model_output_part2)
model_output <- cbind(model_output, model_output_part2)
glimpse(model_output)
model_output

# clean model output
# note that output dataframe has same stats for each node number as the rpart output object, 
# but the variable listed for each node number in the df is actually the variable listed for the subsequent node number on output object
# basically, if you just ignore the variable listed on the rpart output object, and refer only by node number, it's the same
# the output object is listing the variable that HAS been split on to get to current node, but
# the df is listing the variable that will be split on FROM that node

model_output <- model_output %>% select(-yval2, V1) %>%
        mutate(terminal_node = rownames(model_output), tree_depth = rpart:::tree.depth(as.numeric(terminal_node))) %>%
        rename(obs_in_node = n, var_to_be_split_next = var, misclassified_count = dev, predicted_class = yval,
               class_1_obs_in_node = V2, class_2_obs_in_node = V3, prob_class_1 = V4, 
               prob_class_2 = V5, pct_obs_in_node = nodeprob)
        

model_output
glimpse(model_output)


###############################################################


# call get_node_path function
current_wd <- getwd()
setwd("H:/R/rpart")
source("get_node_paths.R")
setwd(current_wd)

model_output_node_paths <- get_node_paths(model_fit = model, output_terminal_nodes = model_output)
model_output_node_paths

# add node paths to output_terminal nodes
model_output <- left_join(model_output, model_output_node_paths, by = "terminal_node")
model_output


##############################################################


# get next split at node
get_next_split_at_node <- function(node_path_tbl) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " & ")
        last_split_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% tail(1) %>% pull(end))
        last_split_locations <- map(.x = last_split_locations, .f = clean_last_split_locations) %>% tibble(last_split_locations = .) %>%
                unnest() %>% mutate(last_split_locations = as.numeric(last_split_locations),
                                    last_split_locations = case_when(last_split_locations != 0 ~ last_split_locations + 1,
                                                                     TRUE ~ last_split_locations))
        next_split_at_node <- str_sub(string = node_path_tbl$node_path, start = last_split_locations$last_split_locations)
        str_c("  ", next_split_at_node, "     ")
}

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_last_split_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# run get_next_split_at_node function
node_path_tbl <- model_output %>% select(node_path)
next_split_at_node <- get_next_split_at_node(node_path_tbl)
model_output <- model_output %>% mutate(next_split_at_node = next_split_at_node)
model_output %>% data.frame()


###############################################################


# create new_data
new_data <- read_csv("treatment_pre_interview_dummies_v2_20180508.csv")
glimpse(new_data)

# define outcome variable
outcome_variable <- "Case_OutcomeDenied"
positive_class_name <- "Denied"
negative_class_name <- "Not_Denied"
outcome_value_positive <- "Denied"
outcome_value_negative <- "Not_Denied"

# create outcome_variable_sym
outcome_variable_sym <- sym(outcome_variable)

# create outcome_variable_dummy_sym
outcome_variable_dummy <- str_c(outcome_variable, "_dummy")
outcome_variable_dummy_sym <- sym(outcome_variable_dummy)
outcome_variable_dummy_sym 

# prepare new_data
new_data %>% select(!!outcome_variable_sym) %>% glimpse()
new_data <- new_data %>% 
        mutate(!!outcome_variable_sym := case_when((!!outcome_variable_sym) == 1 ~ outcome_value_positive, TRUE ~ outcome_value_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_value_positive, outcome_value_negative)) %>%
        mutate(!!outcome_variable_dummy_sym := ifelse(!!outcome_variable_sym == outcome_value_positive, 1, 0))
new_data %>% select(!!outcome_variable_sym, !!outcome_variable_dummy_sym) 
new_data %>% select(!!outcome_variable_sym, !!outcome_variable_dummy_sym) %>% str()


#######################################################################


# predict nodes for new_data
current_wd <- getwd()
setwd("H:/R/rpart")
source("predict_nodes.R")
setwd(current_wd)

new_data_pred_node <- predict_nodes(object = model, newdata = new_data)

# add new_data_pred_node to new_data
new_data <- new_data %>% mutate(terminal_node = as.character(new_data_pred_node))
new_data


#########################################################################


# get node_paths for new_data terminal nodes
current_wd <- getwd()
setwd("H:/R/rpart")
source("get_node_paths.R")
setwd(current_wd)

new_data_node_paths <- get_node_paths(model_fit = model, output_terminal_nodes = model_output)
new_data_node_paths

# add node paths to output_terminal nodes
new_data <- left_join(new_data, new_data_node_paths, by = "terminal_node")
glimpse(new_data)

# check 
model_output %>% filter(var_to_be_split_next == "<leaf>") %>% distinct(terminal_node, node_path) %>% arrange(terminal_node)
new_data %>% distinct(terminal_node, node_path) %>% arrange(terminal_node)


##############################################################


# create tree_output

# inspect outcome_variable
new_data %>% tabyl(!!outcome_variable_sym)

# create tree_output
tree_output <- new_data %>% mutate(tree_depth = str_count(string = node_path, pattern = " & ") + 1) %>%
        group_by(terminal_node) %>% summarize(obs_in_node = n(), pct_obs_in_node = obs_in_node / nrow(new_data),
                        positive_class_obs_in_node = sum(!!outcome_variable_sym == outcome_value_positive),
                        negative_class_obs_in_node = sum(!!outcome_variable_sym == outcome_value_negative),
                        prob_positive_class = positive_class_obs_in_node / obs_in_node,
                        prob_negative_class = negative_class_obs_in_node / obs_in_node,
                        predicted_class = ifelse(prob_positive_class > prob_negative_class, 1, 0),
                        predicted_class_name = ifelse(prob_positive_class > prob_negative_class, positive_class_name, negative_class_name),
                        node_path = unique(node_path), 
                        tree_depth = unique(tree_depth))

tree_output %>% data.frame()


#################################################################


# create function add_hierarchy_to_tree_output, then run on tree_output
add_hierarchy_to_tree_output <- function(tree_output, final_terminal_node) {
        
        # find max tree depth to know how many placeholder columns you need to build pathString of hierarchy 
        max_tree_depth <- max(tree_output$tree_depth) + 1
        
        # find how many terminal nodes in tree
        terminal_node_count <- nrow(tree_output)
        
        # create placeholder variable names for the hierarchy
        node_level_var_names <- unlist(map(.x = 1:max_tree_depth, .f = ~ str_c("node_level_", .x)))
        
        # create placeholder variable vectors for the hierarchy
        node_level_vectors <- map(.x = 1:max_tree_depth, .f = ~ rep(NA, times = terminal_node_count))
        
        # create placeholder variables for the hierarchy
        create_tbl <- function(.x, .y, ...){
                node_level_var_name_sym <- sym(.x)
                tibble(!!node_level_var_name_sym := .y)
        }
        node_level_data <- map2(.x = node_level_var_names, .y = node_level_vectors, .f = create_tbl) %>% bind_cols(.)
        
        # save node_level variable names, and create syms for use in get_node_hierarchy function
        node_level_data_variable_names <- names(node_level_data)
        node_level_data_variable_names_syms <- syms(node_level_data_variable_names )
        
        # add node_level_data placholder to tree_output_terminal_nodes
        tree_output_terminal_nodes <- tree_output  
        tree_output_terminal_nodes <- bind_cols(tree_output_terminal_nodes, node_level_data)
        
        # create get_node_hierarchy function 
        grouping_var_sym <- sym("terminal_node")
        
        # test get_node_hierarchy
        # data <- tree_output_terminal_nodes %>% mutate(grouping_var = !!grouping_var_sym) %>% group_by(grouping_var) %>%
        #                 nest() %>% slice(1) %>% unnest()
        
        get_node_hierarchy <- function(grouping_var, data, ...) {
                
                # split node_path into constituent variables
                node_path_split <- str_trim(unlist(str_split(string = data$node_path[1], pattern = "&")))
                
                # remove spaces in each node_path
                node_hierarchy <- str_replace_all(string = node_path_split, pattern = " ", replace = "")
                
                # add root to beginning of node_hierarchy
                node_hierarchy <- c("root_node", node_hierarchy)
                
                # pad node_hierarchy with NA as needed
                count_of_NAs_needed_for_padding <- length(node_level_data_variable_names) - length(node_hierarchy)
                node_hierarchy <- c(node_hierarchy, rep(NA, times = count_of_NAs_needed_for_padding))
                
                # map over the node_hierarchy values and node_level_data_variable_names to return row for terminal node with hierarchy
                map2_dfr(.x = node_level_data_variable_names_syms, .y = node_hierarchy, 
                         .f = ~ data %>% mutate(!!.x := .y)) %>%
                        fill(vars_select(names(data), starts_with("node_level_")), .direction = "up") %>% 
                        fill(vars_select(names(data), starts_with("node_level_")), .direction = "down") %>% slice(1)
        }
        
        # map get_node_hieararchy function over all terminal nodes
        tree_output_w_hierarchy <- tree_output_terminal_nodes %>% mutate(grouping_var = !!grouping_var_sym) %>% group_by(grouping_var) %>%
                nest() %>% pmap_dfr(.l = ., .f = get_node_hierarchy)
        
        # add pathString to tree_output_w_hierarchy
        tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
                mutate_at(.vars = vars_select(names(tree_output_w_hierarchy), starts_with("node_level_")), .funs = funs(ifelse(is.na(.), "", .))) %>%
                mutate(pathString = str_c(!!!node_level_data_variable_names_syms, sep = "/")) %>%
                mutate(pathString = str_replace(string = pathString, pattern = regex("/+$"), replacement = ""))
        
        # return tree_output_w_hierarchy
        tree_output_w_hierarchy
        
}

# call function and save output
tree_output_w_hierarchy <- add_hierarchy_to_tree_output(tree_output = tree_output, final_terminal_node = TRUE)
tree_output_w_hierarchy %>% data.frame()
tree_output_w_hierarchy %>% select(terminal_node, node_path, starts_with("node_level_"), pathString) %>% data.frame()


###################################################################


# get tree_output for non-terminal_nodes
tree_output_w_hierarchy %>% data.frame()
model_output %>% data.frame()

# create variable for final_terminal_node
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(final_terminal_node = 1)
model_output <- model_output %>% mutate(final_terminal_node = ifelse(var_to_be_split_next == "<leaf>", 1, 0))

# test functions
# target_depth <- 1
# node_path_tbl <- model_output %>% filter(tree_depth == target_depth, final_terminal_node == 0)
# node_path_tbl

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_target_depth_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# create get_node_path_to_target_depth function to be called in get_non_terminal_node_paths function
get_node_path_to_target_depth <- function(node_path_tbl, target_depth) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " &")
        target_depth_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% slice(target_depth) %>% pull(end))
        target_depth_locations <- map(.x = target_depth_locations, .f = clean_target_depth_locations) %>% tibble(target_depth_locations = .) %>%
                unnest() %>% mutate(target_depth_locations = case_when(target_depth_locations != 0 ~ target_depth_locations - 2,
                                                                       TRUE ~ 0))
        node_path_to_target_depth <- str_sub(string = node_path_tbl$node_path, start = 1,
                                             end = ifelse(target_depth_locations$target_depth_locations != 0, 
                                                          target_depth_locations$target_depth_locations,
                                                          nchar(node_path_tbl$node_path)))
        node_path_to_target_depth
}

# create get_non_terminal_node_paths function
get_non_terminal_node_paths <- function(.x) {
        model_output %>% filter(tree_depth == .x, final_terminal_node == 0) %>%
                get_node_path_to_target_depth(node_path_tbl = ., target_depth = .x)
}

# call get_non_terminal_node_paths on model_output
non_terminal_node_paths <- map(.x = 1:(max(model_output$tree_depth)-1), .f = get_non_terminal_node_paths) %>%
        tibble(node_path = .) %>% unnest()
non_terminal_node_paths

# create get_non_terminal_node_stats function
get_non_terminal_node_output <- function(.x) {
        current_non_terminal_node_children <- tree_output_w_hierarchy %>% 
                filter(str_sub(string = node_path, start = 1, end = nchar(.x)) == .x)
        
        current_non_terminal_node <- tibble( 
                obs_in_node = sum(current_non_terminal_node_children$obs_in_node),
                pct_obs_in_node = obs_in_node / sum(tree_output_w_hierarchy$obs_in_node),
                positive_class_obs_in_node = sum(current_non_terminal_node_children$positive_class_obs_in_node),
                negative_class_obs_in_node = sum(current_non_terminal_node_children$negative_class_obs_in_node),
                prob_positive_class = positive_class_obs_in_node / obs_in_node,
                prob_negative_class = negative_class_obs_in_node / obs_in_node,
                predicted_class = ifelse(prob_positive_class > prob_negative_class, 1, 0),
                predicted_class_name = ifelse(prob_positive_class > prob_negative_class, 
                                              positive_class_name, negative_class_name), 
                node_path = .x, final_terminal_node = 0)
        
        current_non_terminal_node
}

# call get_non_terminal_node_output on non_terminal_node_paths$node_path
non_terminal_node_output <- map(.x = non_terminal_node_paths$node_path, .f = get_non_terminal_node_output) %>% 
        tibble(non_terminal_node_output = .) %>% unnest()
non_terminal_node_output %>% data.frame()

# add terminal_node and tree_depth to non_terminal_node_stats 
non_terminal_node_output <- model_output %>% filter(node_path %in% non_terminal_node_output$node_path) %>%
        select(terminal_node, tree_depth, node_path) %>%
        left_join(non_terminal_node_output, ., by = "node_path")
non_terminal_node_output %>% data.frame()

# call add_hierarchy_to_tree_output on non_terminal_node_output
non_terminal_node_output_w_hierarchy <- add_hierarchy_to_tree_output(tree_output = non_terminal_node_output,
                                                                     final_terminal_node = FALSE)
non_terminal_node_output_w_hierarchy %>% data.frame()

# ensure non_terminal_node_output_w_hierarchy has the same node_level variables
tree_output_w_hierarchy_node_level_variables <- tree_output_w_hierarchy %>% select(starts_with("node_level")) %>%
        names()
non_terminal_node_output_w_hierarchy_node_level_variables <- non_terminal_node_output_w_hierarchy %>%
        select(starts_with("node_level")) %>%
        names()

omitted_node_level_variable <- tree_output_w_hierarchy_node_level_variables[!(tree_output_w_hierarchy_node_level_variables %in% 
                                                       non_terminal_node_output_w_hierarchy_node_level_variables)]

# add any omitted_node_level_variable
if(length(omitted_node_level_variable) > 0) {
        omitted_node_level_variable_sym <- sym(omitted_node_level_variable)
        non_terminal_node_output_w_hierarchy <- non_terminal_node_output_w_hierarchy %>% 
                mutate(!!omitted_node_level_variable_sym := NA_character_)
}


# compare non_terminal_node_output_w_hierarchy and tree_output
dim(non_terminal_node_output_w_hierarchy)
dim(tree_output_w_hierarchy)

tree_output_w_hierarchy %>% data.frame()
non_terminal_node_output_w_hierarchy %>% data.frame()

tree_output_w_hierarchy %>% select(-c(!!!syms(names(non_terminal_node_output_w_hierarchy))))
non_terminal_node_output_w_hierarchy %>% select(-c(!!!syms(names(tree_output_w_hierarchy))))

# create root node
new_data %>% tabyl(!!outcome_variable)

root_terminal_node <- "1"
root_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_obs_in_node = sum(obs_in_node)) %>% pull(root_obs_in_node)
root_positive_class_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_positive_class_obs_in_node = sum(positive_class_obs_in_node)) %>%
        pull(root_positive_class_obs_in_node)
root_negative_class_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_negative_class_obs_in_node = sum(negative_class_obs_in_node)) %>%
        pull(root_negative_class_obs_in_node) 
root_prob_positive_class <- root_positive_class_obs_in_node / root_obs_in_node
root_prob_negative_class <- root_negative_class_obs_in_node / root_obs_in_node
root_predicted_class <- ifelse(root_prob_positive_class > root_prob_negative_class, 1, 0)
root_predicted_class_name <- ifelse(root_prob_positive_class > root_prob_negative_class, positive_class_name, negative_class_name)
root_node_path <- "root_node"
root_tree_depth <- 0
root_final_terminal_node <- 0
root_pathString <- "root_node"
root_node_level_1 <- "root_node"

root_node <- tibble(terminal_node = root_terminal_node, obs_in_node = root_obs_in_node,
                    pct_obs_in_node = 1.00, positive_class_obs_in_node = root_positive_class_obs_in_node,
                    negative_class_obs_in_node = root_negative_class_obs_in_node,
                    prob_positive_class = root_prob_positive_class, prob_negative_class = root_prob_negative_class,
                    predicted_class = root_predicted_class, predicted_class_name = root_predicted_class_name,
                    node_path = root_node_path, tree_depth = root_tree_depth, final_terminal_node = root_final_terminal_node,
                    pathString = root_pathString, node_level_1 = root_node_level_1)

root_node %>% data.frame()
dim(root_node)

# get node_level variables for root and add to root_node, except node_level_1 which is always going to be "root"
node_level_names <- tree_output_w_hierarchy %>% select(starts_with("node_level")) %>% select(-node_level_1) %>% names()
node_level_names_syms <- syms(node_level_names)
root_node_node_levels <- map_dfc(.x = node_level_names_syms, .f = ~ root_node %>% mutate(!!.x := NA_character_) %>% select(!!.x))

root_node <- bind_cols(root_node, root_node_node_levels)
root_node %>% data.frame()

# add root_node to non_terminal_node_output_w_hierarchy
root_node %>% data.frame()
dim(root_node)
dim(non_terminal_node_output_w_hierarchy)
non_terminal_node_output_w_hierarchy %>% select(-c(!!!syms(names(root_node))))
root_node %>% select(-c(!!!syms(names(non_terminal_node_output_w_hierarchy))))

non_terminal_node_output_w_hierarchy <- bind_rows(non_terminal_node_output_w_hierarchy, root_node)
non_terminal_node_output_w_hierarchy %>% data.frame()

# combine tree_output_w_hierarchy with non_terminal_node_output_w_hierarchy
tree_output_w_hierarchy <- bind_rows(tree_output_w_hierarchy, non_terminal_node_output_w_hierarchy)
tree_output_w_hierarchy %>% data.frame()


###################################################################


# get next split at node
get_split_leading_to_node <- function(node_path_tbl) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " & ")
        last_split_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% tail(1) %>% pull(end))
        last_split_locations <- map(.x = last_split_locations, .f = clean_last_split_locations) %>% tibble(last_split_locations = .) %>%
                unnest() %>% mutate(last_split_locations = as.numeric(last_split_locations),
                        last_split_locations = case_when(last_split_locations != 0 ~ last_split_locations + 1,
                                                                     TRUE ~ last_split_locations))
        split_leading_to_node <- str_sub(string = node_path_tbl$node_path, start = last_split_locations$last_split_locations)
        str_c("  ", split_leading_to_node, "     ")
}

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_last_split_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# run get_next_split_at_node function on tree_output_w_hierarchy
tree_output_w_hierarchy %>% data.frame()

node_path_tbl <- tree_output_w_hierarchy %>% select(node_path)
split_leading_to_node <- get_split_leading_to_node(node_path_tbl)
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(split_leading_to_node = split_leading_to_node)
tree_output_w_hierarchy %>% data.frame()


##############################################################################


# get node dummies for each individual observation 
# not necessary for plotting tree, just a useful function
tree_output_w_hierarchy %>% data.frame()
glimpse(new_data)


get_node_membership <- function(tree_output_w_hierarchy) {
        
        # get terminal nodes in tree
        terminal_node_list <- unique(tree_output_w_hierarchy$terminal_node)
        
        # create placeholder variable names for the hierarchy
        node_member_var_names <- unlist(map(.x = terminal_node_list, .f = ~ str_c("member_of_node_", .x)))
        
        # get final_terminal_nodes
        terminal_node_list <- tree_output_w_hierarchy %>% pull(terminal_node)
        
        # create placeholder variable vectors for the hierarchy
        node_level_vectors <- map(.x = node_member_var_names, .f = ~ rep(as.numeric(NA), times = length(terminal_node_list)))
        
        # create placeholder variables for the hierarchy
        create_tbl <- function(.x, .y, ...){
                node_level_var_name_sym <- sym(.x)
                tibble(!!node_level_var_name_sym := .y)
        }
        
        # run create_tbl
        node_member_data <- map2(.x = node_member_var_names, .y = node_level_vectors, .f = create_tbl) %>% bind_cols(.)
        
        # add terminal_nodes to node_member_data
        node_member_data <- node_member_data %>% mutate(terminal_node = terminal_node_list)
        
        # get pathString for final_terminal_nodes
        final_terminal_node_pathStrings <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>% pull(pathString)
        
        # create find_final_terminal_node_for_each_node function
        find_final_terminal_node_for_each_node <- function(.x) {
                terminal_node_tbl <- tree_output_w_hierarchy %>% filter(row_number() == .x)
                current_terminal_node <- terminal_node_tbl %>% pull(terminal_node)
                current_pathString <- terminal_node_tbl %>% pull(pathString)
                beginning_of_pathStrings <- str_sub(string = tree_output_w_hierarchy$pathString, start = 1, end = nchar(current_pathString))
                membership_terminal_nodes <- tree_output_w_hierarchy$terminal_node[beginning_of_pathStrings == current_pathString]
                map2_dfr(.x = membership_terminal_nodes, .y = current_terminal_node, .f = flag_node_membership) %>%
                fill(vars_select(names(node_member_data), starts_with("member_of_node")), .direction = "up") %>% 
                        fill(vars_select(names(node_member_data), starts_with("member_of_node")), .direction = "down") %>% slice(1)
        }
        
        # create flag_node_membership function
        flag_node_membership <- function(.x, .y) {
                current_terminal_node <- .y
                current_membership_terminal_node <- .x
                current_membership_terminal_node_var_name <- str_c("member_of_node_", current_membership_terminal_node)
                current_membership_terminal_node_var_name_sym <- sym(current_membership_terminal_node_var_name)
                current_node_member_data <- node_member_data %>% filter(terminal_node == current_terminal_node) %>%
                        mutate(!!current_membership_terminal_node_var_name_sym := 1)
                current_node_member_data
        }
        
        # call find_final_terminal_node_for_each_node function
        node_membership_tbl <- map_dfr(.x = 1:nrow(tree_output_w_hierarchy), .f = find_final_terminal_node_for_each_node)
        
        # pivot node_membership_tbl to get which nodes a given node traces up to, instead of which nodes a given node percolates down to
        node_membership_tbl <- node_membership_tbl %>% gather(key = percolates_down_to_node, value = value, -c(terminal_node)) %>% 
                mutate(percolates_down_to_node = str_replace(string = percolates_down_to_node, 
                                                              pattern = "member_of_node_", replacement = "")) %>%
                mutate(traces_up_to_node = str_c("traces_up_to_node_", terminal_node)) %>% 
                select(-terminal_node) %>% spread(key = traces_up_to_node, value = value) %>% 
                rename(terminal_node = percolates_down_to_node) %>%
                data.frame() %>% arrange(as.numeric(terminal_node))

        # join node_membership_tbl with new_data
        node_membership_tbl <- tree_output_w_hierarchy %>% select(terminal_node, final_terminal_node) %>%
                left_join(node_membership_tbl, ., by = "terminal_node")
}

node_membership_tbl <- get_node_membership(tree_output_w_hierarchy = tree_output_w_hierarchy)
node_membership_tbl %>% data.frame()


#############################################################################


# get all node statistics 
# because it needs to allow for weighting of individual observations, the stats must be calculated from individual obs instead of tree_output

# add node_membership_tbl to new_data
glimpse(new_data)
new_data <- new_data %>% left_join(., node_membership_tbl, by = "terminal_node")
new_data %>% distinct(terminal_node, final_terminal_node, traces_up_to_node_1, traces_up_to_node_2) %>% data.frame()

# add weights and add weighted_outcome var
new_data <- new_data %>% mutate(weight = case_when(ben_country_of_birth_groupedMEXIC == 1 ~ 1.24, 
                   ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                   ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1)) %>%
        mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight)

new_data %>% distinct(!!outcome_variable_sym, !!outcome_variable_dummy_sym, ben_country_of_birth_groupedMEXIC, BEN_SEXM, BEN_SEXF, weight, weighted_outcome)

# output new_data
file_name <- "output/pre_interview/full/treatment_pre_interview_full_w_tree_output_Case_OutcomeDenied"
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
file_name <- str_c(file_name, "_", current_date, ".csv")
file_name

# write_csv(new_data, path = file_name)

# get variable names for node dummies
node_var_names <- new_data %>% select(starts_with("traces_up_to_node")) %>% names(.)

# create get_all_node_stats function
get_all_node_stats <- function(.x, data) {
        
        # get current_data
        current_data <- data
        
        # assign current_node_var_name and current_node_var_name_sym
        current_node_var_name <- .x
        current_node_var_name_sym <- sym(current_node_var_name)
        
        # get current_terminal_node
        current_terminal_node <- str_replace(string = current_node_var_name, pattern = "traces_up_to_node_", replacement = "")
        
        
        ###################################################

        
        # get outcome_p_weighted
        current_node_outcome_stats <- current_data %>% filter(!!current_node_var_name_sym == 1) %>% 
                summarize(outcome_p_weighted = sum(weighted_outcome) / sum(weight), obs_in_node = n(),
                          outcome_denominator_weight_sum = sum(weight), outcome_numerator_weight_sum = sum(weighted_outcome), 
                          outcome_positive_sum = sum(!!outcome_variable_dummy_sym))

        # use wilson formula to get confidence interval for outcome_p_weighted
        n_weighted <- current_node_outcome_stats %>% pull(outcome_denominator_weight_sum)
        outcome_p_weighted <- current_node_outcome_stats %>% pull(outcome_p_weighted)
        z <- 1.96
        
        upper_limit_outcome_p_weighted_value <- (1 / (2 * (n_weighted   + z^2)) ) * ( (2 * n_weighted * outcome_p_weighted + z^2) + 
                                                                                      (z * sqrt(4 * n_weighted * outcome_p_weighted * (1 - outcome_p_weighted) + z^2) ) )
        
        lower_limit_outcome_p_weighted_value <- (1 / (2 * (n_weighted + z^2)) ) * ( (2 * n_weighted * outcome_p_weighted + z^2) - 
                                                                                    (z * sqrt(4 * n_weighted * outcome_p_weighted * (1 - outcome_p_weighted) + z^2) ) )
        
        # save upper/lower_limit_outcome_p_weighted
        current_node_outcome_stats <- current_node_outcome_stats %>% 
                mutate(upper_limit_outcome_p_weighted = upper_limit_outcome_p_weighted_value,
                       lower_limit_outcome_p_weighted = lower_limit_outcome_p_weighted_value)
        
        
        ###################################################
   
        
        # get share_p_weighted
        current_node_share_stats <- current_data %>% filter(!!current_node_var_name_sym == 1) %>% 
                summarize(share_p_weighted = sum(weight) / sum(current_data$weight), obs_in_node = n(),
                          share_denominator_weight_sum = sum(current_data$weight), share_numerator_weight_sum = sum(weight)) %>% 
                mutate(lower_limit_share_p_weighted = 0, upper_limit_share_p_weighted = 0)
        
        # use wilson formula to get confidence interval for share_p_weighted
        n_weighted <- current_node_share_stats %>% pull(share_denominator_weight_sum)
        share_p_weighted <- current_node_share_stats %>% pull(share_p_weighted)
        z <- 1.96
        
        upper_limit_share_p_weighted_value <- (1 / (2 * (n_weighted   + z^2)) ) * ( (2 * n_weighted * share_p_weighted + z^2) + 
                                                                                              (z * sqrt(4 * n_weighted * share_p_weighted * (1 - share_p_weighted) + z^2) ) )
        
        lower_limit_share_p_weighted_value <- (1 / (2 * (n_weighted + z^2)) ) * ( (2 * n_weighted * share_p_weighted + z^2) - 
                                                                                            (z * sqrt(4 * n_weighted * share_p_weighted * (1 - share_p_weighted) + z^2) ) )
        
        # save upper/lower_limit_share_p_weighted
        current_node_share_stats <- current_node_share_stats %>% 
                mutate(upper_limit_share_p_weighted = upper_limit_share_p_weighted_value,
                       lower_limit_share_p_weighted = lower_limit_share_p_weighted_value)
        
        
        #####################################################
        
        
        # combine current_node_outcome_stats and current_node_share_stats
        current_node_stats <- bind_cols(current_node_outcome_stats, current_node_share_stats) %>% 
                select(-obs_in_node1) %>% mutate(terminal_node = current_terminal_node) 
        
        current_node_stats
}

# call get_all_node_stats function
all_node_stats <- map_dfr(.x = node_var_names, .f = ~ get_all_node_stats(.x, data = new_data))
all_node_stats %>% data.frame()

# add all_node_stats to tree_output_w_hierarchy
tree_output_w_hierarchy <- all_node_stats %>% select(-obs_in_node) %>% left_join(tree_output_w_hierarchy, ., by = "terminal_node")
tree_output_w_hierarchy %>% data.frame()

# will wait to write tree_output_w_hierarchy to file until adding control_all_node_stats below


###############################################################################


# get all_node_stats for control group
control_data <- read_csv("output/pre_interview/full/control_pre_interview_full_w_terminal_nodes_Case_OutcomeDenied.csv")
glimpse(control_data)

# predict nodes for new_data
# current_wd <- getwd()
# setwd("H:/R/rpart")
# source("predict_nodes.R")
# setwd(current_wd)
# 
# control_data_pred_node <- predict_nodes(object = model, newdata = control_data)

# add control_data_pred_node to control_data
# control_data <- control_data %>% mutate(terminal_node = as.character(control_data_pred_node))
# control_data

# ensure terminal_node is a character so it can merge with node_membership_tbl
control_data <- control_data %>% mutate(terminal_node = as.character(terminal_node))

# convert outcome_variable to factor label, and create outcome_variable_dummy
# overwrite NA values for control Case_OutcomeDenied 
# these are the result of control variable "outcome" having value of "R" for rejected, which were converted to NA in cleaning
control_data %>% count(!!outcome_variable_sym)
control_data <- control_data %>% mutate(!!outcome_variable_sym := case_when(is.na(!!outcome_variable_sym) ~ outcome_value_negative,
        (!!outcome_variable_sym) == 1 ~ outcome_value_positive, TRUE ~ outcome_value_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_value_positive, outcome_value_negative)) %>%
        mutate(!!outcome_variable_dummy_sym := ifelse(!!outcome_variable_sym == outcome_value_positive, 1, 0))
control_data %>% count(!!outcome_variable_sym)
control_data %>% count(!!outcome_variable_dummy_sym)

# add weights to control
control_data <- control_data %>% mutate(weight = case_when(ben_country_of_birth_groupedMEXIC == 1 ~ 1, 
                                                   ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXM == 1 ~ 1, 
                                                   ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXF == 1 ~ 1, TRUE ~ 1)) %>%
        mutate(weight = case_when(is.na(weight) ~ 1, TRUE ~ weight)) %>%
        mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight)

control_data %>% distinct(ben_country_of_birth_groupedMEXIC, BEN_SEXM, BEN_SEXF, weight, weighted_outcome)

# add node_membership_tbl to control_data
glimpse(control_data)
control_data <- control_data %>% left_join(., node_membership_tbl, by = "terminal_node")
control_data %>% distinct(terminal_node, final_terminal_node, traces_up_to_node_1, traces_up_to_node_2) %>% data.frame()

# output control_data
file_name <- "output/pre_interview/full/control_pre_interview_full_w_tree_output_Case_OutcomeDenied"
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
file_name <- str_c(file_name, "_", current_date, ".csv")
file_name

# write_csv(control_data, path = file_name)

# get control_all_node_stats
control_all_node_stats <- map_dfr(.x = node_var_names, .f = ~ get_all_node_stats(.x, data = control_data))
control_all_node_stats %>% data.frame()

# rename control_all_node_stats variables
control_all_node_stats_variables <- names(control_all_node_stats)
control_all_node_stats_variables <- str_c("control_", control_all_node_stats_variables)
names(control_all_node_stats) <- control_all_node_stats_variables

# add all_node_stats to tree_output_w_hierarchy
tree_output_w_hierarchy <- control_all_node_stats %>% left_join(tree_output_w_hierarchy, ., by = c("terminal_node" = "control_terminal_node"))
tree_output_w_hierarchy %>% data.frame()

# write tree_output_w_hierarchy to file
file_name <- "output/pre_interview/full/control_and_pre_interview_full_tree_hierarchy_Case_OutcomeDenied"
current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
file_name <- str_c(file_name, "_", current_date, ".csv")
file_name

# write_csv(tree_output_w_hierarchy, path = file_name)
tree_output_w_hierarchy <- read_csv("output/pre_interview/full/control_and_pre_interview_full_tree_hierarchy_Case_OutcomeDenied_20180829.csv") 
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(terminal_node = as.character(terminal_node))
tree_output_w_hierarchy


##############################################################################


# reid's confidence intervals for IBFA are very slightly off what i get (based on his handling of NA values, and because for some variables)
# note his conf. int. for non-IBFA (control) are the same as what i get (for the terminal nodes)
# note though for nodes 2 and 3, which are also the subject of single-variable tables by reid, there is some difference
# (for control variables, it could because he used the universe instead of the control group, maybe slight coding differences for treatment??)
# manually add his IBFA denial rate conf. int. for use in tree plot
# note he did not provide his version of share of obs in node confidence intervals, so those are left as is

# inspect original denial conf. intervals
tree_output_w_hierarchy %>% data.frame()
tree_output_w_hierarchy %>% select(terminal_node, outcome_p_weighted, control_outcome_p_weighted, upper_limit_outcome_p_weighted, lower_limit_outcome_p_weighted)

# load edited denial rate conf. int
edited_denial_rate_conf_int <- read_csv("pre_interview_ibfa_denial_rate_confidence_intervals_edited_from_reid.csv")
edited_denial_rate_conf_int <- edited_denial_rate_conf_int %>% mutate(terminal_node = as.character(terminal_node))
edited_denial_rate_conf_int

# bind edited conf_int with tree_output_w_hierarchy for use in tree_plot, and format as conf int as percent
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% left_join(., edited_denial_rate_conf_int, by = "terminal_node")
glimpse(tree_output_w_hierarchy)
tree_output_w_hierarchy %>% select(lower_limit_outcome_p_weighted, lower_limit_outcome_p_weighted_edited, 
                                   upper_limit_outcome_p_weighted, upper_limit_outcome_p_weighted_edited)

# inspect difference in confidence intervals
tree_output_w_hierarchy %>% select(terminal_node, upper_limit_outcome_p_weighted, upper_limit_outcome_p_weighted_edited, 
                                    lower_limit_outcome_p_weighted, lower_limit_outcome_p_weighted_edited) %>% 
        mutate(diff_lower_limit = lower_limit_outcome_p_weighted - lower_limit_outcome_p_weighted_edited,
               diff_upper_limit = upper_limit_outcome_p_weighted - upper_limit_outcome_p_weighted_edited) %>%
        summarize(max_diff_lower_limit = max(diff_lower_limit), max_diff_upper_limit = max(diff_upper_limit))

################


# reid's overall control denial rate is different because it uses 401 control obs and 9300ish sample frame obs
# reid uses entire frame for domain calculations if the variable was available for the frame
# if not, then he used the 401 control obs
# my script uses only 401 control obs

# also though, reid's control stats are VERY slightly off mine, so i use his edited stats from table for terminal nodes, and mine for non-terminal nodes
edited_control_stats <- read_csv("pre_interview_control_denial_rate_and_confidence_intervals_edited_from_reid.csv")
edited_control_stats <- edited_control_stats %>% mutate(node = as.character(node))
edited_control_stats

# bind edited_overall_control_stats with tree_output_w_hierarchy
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% left_join(., edited_control_stats, by = c("terminal_node" = "node")) %>%
        mutate(control_outcome_p_weighted = case_when(!is.na(control_p_edited) ~ control_p_edited, TRUE ~ control_outcome_p_weighted),
               control_lower_limit_outcome_p_weighted = case_when(!is.na(control_p_lower_bound_edited) ~ control_p_lower_bound_edited, 
                                                                  TRUE ~ control_lower_limit_outcome_p_weighted),
               control_upper_limit_outcome_p_weighted = case_when(!is.na(control_p_upper_bound_edited) ~ control_p_upper_bound_edited, 
                                                                  TRUE ~ control_upper_limit_outcome_p_weighted))


################


# use newcombe formula to get conf. int for difference in proportions of treatment - control denial rate
# this needs to be done here because reid's calculations of ibfa denial rate are slightly off from mine due to his handling of NAs
# and ibfa treatment denial rate factors into newcombe stats, so i need to base on reid's edited ibfa denial rate stats

# get edited treatment stats
treatment_p_weighted <- tree_output_w_hierarchy %>% pull(outcome_p_weighted)
treatment_p_weighted_lower_bound <- tree_output_w_hierarchy %>% pull(lower_limit_outcome_p_weighted_edited)
treatment_p_weighted_upper_bound <- tree_output_w_hierarchy %>% pull(upper_limit_outcome_p_weighted_edited)

# get control stats
control_p_weighted <- tree_output_w_hierarchy %>% pull(control_outcome_p_weighted)
control_p_weighted_lower_bound <- tree_output_w_hierarchy %>% pull(control_lower_limit_outcome_p_weighted)
control_p_weighted_upper_bound <- tree_output_w_hierarchy %>% pull(control_upper_limit_outcome_p_weighted)

# use newcombe formula to get conf. int for difference in proportion of treatment and control denial rate
diff_btw_treatment_p_weighted_and_control_p_weighted <- treatment_p_weighted - control_p_weighted

diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound <- (treatment_p_weighted - control_p_weighted) - 
        sqrt((treatment_p_weighted - treatment_p_weighted_lower_bound)^2 + (control_p_weighted_upper_bound - control_p_weighted)^2)

diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound <- (treatment_p_weighted - control_p_weighted) +
        sqrt((treatment_p_weighted_upper_bound - treatment_p_weighted)^2 + (control_p_weighted - control_p_weighted_lower_bound)^2)

# update tree_output_w_hierarchy
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted = diff_btw_treatment_p_weighted_and_control_p_weighted,
               diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound = diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
               diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound = diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound)



###############


# ugh! even after computing newcombe conf. int for diff btw treatment and control, there are some VERY small differences btw my and reid's numbers
# for instance, the largest diff is for node 10, where he as 1.5% as lower bound of diff, and i have 1.1%; other diffs are like 0.1%
# also one diff in rate: 5.1 vs 5.2
# probably has to do with me using the rounded treatment conf. int.
# to correct, will just import reid's conf int directly from his table for the terminal nodes, and will keep my calculated version for non-terminal nodes
# can still use the suppressed and unreliable flags calculated in pre_interview_output_summary_stats

# also note that since reid's table has some suppressed conf. int. and denial rates for difference btw treatment/control
# i will manually add my calculated values to the csv of reid's edits 
edited_diff_btw_treatment_control_stats <- read_csv("pre_interview_diff_btw_treatment_control_edited_from_reid.csv")
edited_diff_btw_treatment_control_stats <- edited_diff_btw_treatment_control_stats %>% mutate(node = as.character(node))
edited_diff_btw_treatment_control_stats

# merge with tree_output_w_hierarchy
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% left_join(edited_diff_btw_treatment_control_stats, by = c("terminal_node" = "node")) %>% 
                mutate(diff_btw_treatment_p_weighted_and_control_p_weighted = case_when(!is.na(diff_btw_treatment_control_p_edited) ~
                diff_btw_treatment_control_p_edited, TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted),
                diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound = case_when(!is.na(diff_btw_treatment_control_p_lower_bound_edited) ~
                diff_btw_treatment_control_p_lower_bound_edited, TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound),
                diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound = case_when(!is.na(diff_btw_treatment_control_p_upper_bound_edited) ~
                diff_btw_treatment_control_p_upper_bound_edited, TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound))

# inspect
tree_output_w_hierarchy %>% select(terminal_node, diff_btw_treatment_p_weighted_and_control_p_weighted, 
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound)


##############################################################################


# load pre_interview_full_output_summary_stats (created with get_output_summary_stats_for_all_pre_interview_nodes.R)
# this significance, unreliable, and suppressed flags to add to tree
pre_interview_output_summary_stats <- read_csv("output/pre_interview/full/pre_interview_full_output_summary_stats.csv")
glimpse(pre_interview_output_summary_stats)

# note that there are no nodes marked unreliable solely due to failure to validate as significant on train/test
# node 6 did fail to validate on train/test, and it is significant overall, but it is flagged unreliable in any case due to n < 15 and coeff of var. > .3
# this means that no edits to reid's unreliable/suppresed flags (which don't account for failure to validate train/test) need to be made
pre_interview_output_summary_stats %>% select(node, node_train_test_validated_significant, treatment_p_diff_overall_significant, suppressed_treatment_p,
                                              unreliable_treatment_p, treatment_n, treatment_p_coeff_of_variation)

pre_interview_output_summary_stats %>% select(node, node_train_test_fraud_validated_significant, treatment_p_diff_overall_fraud_significant, suppressed_treatment_p_fraud,
                                              unreliable_treatment_p_fraud, treatment_n, treatment_p_fraud_coeff_of_variation) %>% data.frame()


##################


# get edited fraud_rate_conf_int since reid's version is slightly different from mine.  for non-terminal nodes where reid doesn't have intervals, I retain my conf. int
pre_interview_output_summary_stats %>% select(node, treatment_p_weighted_fraud, treatment_p_weighted_fraud_lower_bound, treatment_p_weighted_fraud_upper_bound)

# load edited denial rate conf. int
edited_fraud_rate_conf_int <- read_csv("pre_interview_ibfa_fraud_rate_confidence_intervals_edited_from_reid.csv")
edited_fraud_rate_conf_int <- edited_fraud_rate_conf_int %>% mutate(terminal_node = as.character(terminal_node))
edited_fraud_rate_conf_int

# add fraud_rate_conf_int to tree_output_w_hierarchy
glimpse(tree_output_w_hierarchy)
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% left_join(., edited_fraud_rate_conf_int, by = "terminal_node")
glimpse(tree_output_w_hierarchy)


#################


# merge pre_interview_output_summary_stats with tree_output_w_hierarchy
tree_output_w_hierarchy <- pre_interview_output_summary_stats %>% mutate(node = as.character(node)) %>%
        select(node, unreliable_treatment_share, suppressed_treatment_share, unreliable_control_share, suppressed_control_share,
               treatment_p_diff_overall_significant, suppressed_treatment_p, unreliable_treatment_p,
               treatment_p_weighted_fraud, # treatment_p_weighted_fraud_upper/lower_bound_edited are already added separately above
               treatment_p_diff_overall_fraud_significant, suppressed_treatment_p_fraud, unreliable_treatment_p_fraud,
                                              control_p_diff_overall_significant, suppressed_control_p, unreliable_control_p, 
               # no longer need to import these from pre_interview_output_summary_stats, 
               # since I calculated manually using reid's edited treatment conf. int's above
               #                                diff_btw_treatment_p_weighted_and_control_p_weighted, 
               # diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
               #                                diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound,
               unreliable_diff_btw_treatment_p_weighted_and_control_p_weighted,
               suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted, 
               suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
               suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound,
               negative_diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
               negative_diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound) %>% 
        left_join(tree_output_w_hierarchy, ., by = c("terminal_node" = "node"))


#################


# another edit: need to copy reid's rates for nodes 2 and 3, since they should look the same as reid's standalone tables
# he got slightly differentn rates due to differences in handling NAs, coding variables, and using control universe, etc

# load edited_rates_and_intervals_for_nodes_2_and_3
edited_rates_and_intervals_for_nodes_2_and_3 <- read_csv("pre_interview_nodes_2_and_3_all_rates_and_intervals_edited_from_reid.csv")
edited_rates_and_intervals_for_nodes_2_and_3 <- edited_rates_and_intervals_for_nodes_2_and_3 %>% mutate(terminal_node = as.character(terminal_node))
edited_rates_and_intervals_for_nodes_2_and_3 %>% glimpse()

# add edited_rates_and_intervals_for_nodes_2_and_3 to tree_output_w_hierarchy
glimpse(tree_output_w_hierarchy)
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
        mutate(treatment_p_weighted_fraud = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                filter(terminal_node == "2") %>% pull(treatment_p_weighted_fraud), 
                terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                        filter(terminal_node == "3") %>% pull(treatment_p_weighted_fraud),
                TRUE ~ treatment_p_weighted_fraud),
               upper_limit_outcome_p_weighted_fraud_edited = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                              filter(terminal_node == "2") %>% pull(upper_limit_outcome_p_weighted_fraud_edited), 
                                                      terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                              filter(terminal_node == "3") %>% pull(upper_limit_outcome_p_weighted_fraud_edited),
                                                      TRUE ~ upper_limit_outcome_p_weighted_fraud_edited),
               lower_limit_outcome_p_weighted_fraud_edited = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                               filter(terminal_node == "2") %>% pull(lower_limit_outcome_p_weighted_fraud_edited), 
                                       terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                               filter(terminal_node == "3") %>% pull(lower_limit_outcome_p_weighted_fraud_edited),
                                       TRUE ~ lower_limit_outcome_p_weighted_fraud_edited),
               outcome_p_weighted = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                               filter(terminal_node == "2") %>% pull(outcome_p_weighted), 
                                       terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                               filter(terminal_node == "3") %>% pull(outcome_p_weighted),
                                       TRUE ~ outcome_p_weighted),
               upper_limit_outcome_p_weighted_edited = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                      filter(terminal_node == "2") %>% pull(upper_limit_outcome_p_weighted_edited), 
                                              terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                      filter(terminal_node == "3") %>% pull(upper_limit_outcome_p_weighted_edited),
                                              TRUE ~ upper_limit_outcome_p_weighted_edited),
               lower_limit_outcome_p_weighted_edited = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                 filter(terminal_node == "2") %>% pull(lower_limit_outcome_p_weighted_edited), 
                                         terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                 filter(terminal_node == "3") %>% pull(lower_limit_outcome_p_weighted_edited),
                                         TRUE ~ lower_limit_outcome_p_weighted_edited),
               control_outcome_p_weighted = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                 filter(terminal_node == "2") %>% pull(control_outcome_p_weighted), 
                                         terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                                 filter(terminal_node == "3") %>% pull(control_outcome_p_weighted),
                                         TRUE ~ control_outcome_p_weighted),
               control_upper_limit_outcome_p_weighted = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                              filter(terminal_node == "2") %>% pull(control_upper_limit_outcome_p_weighted), 
                                      terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                              filter(terminal_node == "3") %>% pull(control_upper_limit_outcome_p_weighted),
                                      TRUE ~ control_upper_limit_outcome_p_weighted),
               control_lower_limit_outcome_p_weighted = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                         filter(terminal_node == "2") %>% pull(control_lower_limit_outcome_p_weighted), 
                                 terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                         filter(terminal_node == "3") %>% pull(control_lower_limit_outcome_p_weighted),
                                 TRUE ~ control_lower_limit_outcome_p_weighted),
               diff_btw_treatment_p_weighted_and_control_p_weighted = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                         filter(terminal_node == "2") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted), 
                                 terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                         filter(terminal_node == "3") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted),
                                 TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted),
               diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                        filter(terminal_node == "2") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound), 
                                terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                        filter(terminal_node == "3") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound),
                                TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound),
               diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound = case_when(terminal_node == "2" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                    filter(terminal_node == "2") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound), 
                            terminal_node == "3" ~ edited_rates_and_intervals_for_nodes_2_and_3 %>%
                                    filter(terminal_node == "3") %>% pull(diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound),
                            TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound))

# inspect edits to nodes 2 and 3
tree_output_w_hierarchy %>% filter(terminal_node %in% c("3", "2")) %>% 
        select(treatment_p_weighted_fraud, lower_limit_outcome_p_weighted_fraud_edited, upper_limit_outcome_p_weighted_fraud_edited,
                outcome_p_weighted, upper_limit_outcome_p_weighted_edited, lower_limit_outcome_p_weighted_edited,
               control_outcome_p_weighted, control_lower_limit_outcome_p_weighted, control_upper_limit_outcome_p_weighted,
               diff_btw_treatment_p_weighted_and_control_p_weighted, diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
               diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound) %>% data.frame()


#################


# incorporate significance, unreliable, and suppressed flags and create output values
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
        mutate(share_p_weighted_output = as_percent(share_p_weighted, 1), 
               lower_limit_share_p_weighted_output = as_percent(lower_limit_share_p_weighted, 1),
               upper_limit_share_p_weighted_output = as_percent(upper_limit_share_p_weighted, 1),
               control_share_p_weighted_output = as_percent(control_share_p_weighted, 1), 
               control_lower_limit_share_p_weighted_output = as_percent(control_lower_limit_share_p_weighted, 1),
               control_upper_limit_share_p_weighted_output = as_percent(control_upper_limit_share_p_weighted, 1)) %>%
        
        mutate(share_p_weighted_output = case_when(suppressed_treatment_share == 1 ~ "-", 
                                                   unreliable_treatment_share == 1 ~ str_c(share_p_weighted_output, "X"), 
                                                   TRUE ~ share_p_weighted_output),
               lower_limit_share_p_weighted_output = case_when(suppressed_treatment_share == 1 ~ "-", 
                                                                       TRUE ~ lower_limit_share_p_weighted_output),
               upper_limit_share_p_weighted_output = case_when(suppressed_treatment_share == 1 ~ "-", 
                                                                       TRUE ~ upper_limit_share_p_weighted_output),
               control_share_p_weighted_output = case_when(suppressed_control_share == 1 ~ "-", 
                                                   unreliable_control_share == 1 ~ str_c(control_share_p_weighted_output, "X"),
                                                   TRUE ~ control_share_p_weighted_output),
               control_lower_limit_share_p_weighted_output = case_when(suppressed_control_share == 1 ~ "-", 
                                                                       TRUE ~ control_lower_limit_share_p_weighted_output),
               control_upper_limit_share_p_weighted_output = case_when(suppressed_control_share == 1 ~ "-", 
                                                                       TRUE ~ control_upper_limit_share_p_weighted_output)) %>%
        
        mutate(outcome_p_weighted_output = case_when(treatment_p_diff_overall_significant == 1 ~ str_c(as_percent(outcome_p_weighted, 1), "*"), 
                                              TRUE ~ as_percent(outcome_p_weighted, 1))) %>%
        mutate(outcome_p_weighted_output = case_when(unreliable_treatment_p == 1 ~ str_c(outcome_p_weighted_output, "X"), 
                                              TRUE ~ outcome_p_weighted_output)) %>%
        mutate(outcome_p_weighted_output = case_when(suppressed_treatment_p == 1 ~ "-", 
                                              TRUE ~ outcome_p_weighted_output)) %>%
        mutate(lower_limit_outcome_p_weighted_edited_output = as_percent(lower_limit_outcome_p_weighted_edited, 1),
               upper_limit_outcome_p_weighted_edited_output = as_percent(upper_limit_outcome_p_weighted_edited, 1)) %>%
        mutate(lower_limit_outcome_p_weighted_edited_output = case_when(suppressed_treatment_p == 1 ~ "-", 
                                                                       TRUE ~ lower_limit_outcome_p_weighted_edited_output)) %>%
        mutate(upper_limit_outcome_p_weighted_edited_output = case_when(suppressed_treatment_p == 1 ~ "-", 
                                                                       TRUE ~ upper_limit_outcome_p_weighted_edited_output)) %>%
        
        mutate(treatment_p_weighted_fraud_output = case_when(treatment_p_diff_overall_fraud_significant == 1 ~ str_c(as_percent(treatment_p_weighted_fraud, 1), "*"), 
                                                           TRUE ~ as_percent(treatment_p_weighted_fraud, 1))) %>%
        mutate(treatment_p_weighted_fraud_output = case_when(unreliable_treatment_p_fraud == 1 ~ str_c(treatment_p_weighted_fraud_output, "X"), 
                                                           TRUE ~ treatment_p_weighted_fraud_output)) %>%
        mutate(treatment_p_weighted_fraud_output = case_when(suppressed_treatment_p_fraud == 1 ~ "-", 
                                                           TRUE ~ treatment_p_weighted_fraud_output)) %>%
        mutate(lower_limit_outcome_p_weighted_fraud_edited_output = as_percent(lower_limit_outcome_p_weighted_fraud_edited, 1),
               upper_limit_outcome_p_weighted_fraud_edited_output = as_percent(upper_limit_outcome_p_weighted_fraud_edited, 1)) %>%
        mutate(lower_limit_outcome_p_weighted_fraud_edited_output = case_when(suppressed_treatment_p_fraud == 1 ~ "-", 
                                                                         TRUE ~ lower_limit_outcome_p_weighted_fraud_edited_output)) %>%
        mutate(upper_limit_outcome_p_weighted_fraud_edited_output = case_when(suppressed_treatment_p_fraud == 1 ~ "-", 
                                                                         TRUE ~ upper_limit_outcome_p_weighted_fraud_edited_output)) %>%
        
        
        mutate(control_lower_limit_outcome_p_weighted_output = as_percent(control_lower_limit_outcome_p_weighted, 1),
                control_upper_limit_outcome_p_weighted_output = as_percent(control_upper_limit_outcome_p_weighted, 1)) %>%
        mutate(control_outcome_p_weighted_output = case_when(control_p_diff_overall_significant == 1 ~ str_c(as_percent(control_outcome_p_weighted, 1), "*"), 
                                                      TRUE ~ as_percent(control_outcome_p_weighted, 1))) %>%
        mutate(control_outcome_p_weighted_output = case_when(unreliable_control_p == 1 ~ str_c(control_outcome_p_weighted_output, "X"), 
                                                      TRUE ~ control_outcome_p_weighted_output)) %>%
        mutate(control_outcome_p_weighted_output = case_when(suppressed_control_p == 1 ~ "-", 
                                                      TRUE ~ control_outcome_p_weighted_output)) %>%
        mutate(control_lower_limit_outcome_p_weighted_output = case_when(suppressed_control_p == 1 ~ "-", 
                                                             TRUE ~ control_lower_limit_outcome_p_weighted_output)) %>%
        mutate(control_upper_limit_outcome_p_weighted_output = case_when(suppressed_control_p == 1 ~ "-", 
                                                                         TRUE ~ control_upper_limit_outcome_p_weighted_output)) %>%
        
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_output = case_when(
                unreliable_diff_btw_treatment_p_weighted_and_control_p_weighted == 1 ~ 
                                                        str_c(as_percent(diff_btw_treatment_p_weighted_and_control_p_weighted, 1), "X"), 
                                                TRUE ~ as_percent(diff_btw_treatment_p_weighted_and_control_p_weighted, 1))) %>%
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_output = case_when(
                suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted == 1 ~ "-", 
                                                        TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_output)) %>%
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output = 
                       case_when(negative_diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound == 1 ~ "--",
                                 TRUE ~ as_percent(diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound, 1))) %>%
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output = 
                       case_when(negative_diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound == 1 ~ "--",
                                 TRUE ~ as_percent(diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound, 1))) %>%
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output = case_when(
                suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound == 1 ~ "-",
                                 TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output)) %>%
        mutate(diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output = 
                       case_when(suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound == 1 ~ "-",
                                 TRUE ~ diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output))

# inspect
glimpse(tree_output_w_hierarchy)
tree_output_w_hierarchy %>% select(share_p_weighted, share_p_weighted_output, unreliable_treatment_share, suppressed_treatment_share,
                                   lower_limit_share_p_weighted, lower_limit_share_p_weighted_output,
                                   control_share_p_weighted, control_share_p_weighted_output, unreliable_control_share, suppressed_control_share,
                                   control_lower_limit_share_p_weighted, control_lower_limit_share_p_weighted_output,
                                   
                                   outcome_p_weighted, outcome_p_weighted_output, unreliable_treatment_p, suppressed_treatment_p,
                                    treatment_p_diff_overall_significant, 
                                   lower_limit_outcome_p_weighted_edited, lower_limit_outcome_p_weighted_edited_output,
                                   upper_limit_outcome_p_weighted_edited, upper_limit_outcome_p_weighted_edited_output,
                                   
                                   treatment_p_weighted_fraud, treatment_p_weighted_fraud_output, unreliable_treatment_p_fraud, suppressed_treatment_p_fraud,
                                   treatment_p_diff_overall_fraud_significant, 
                                   lower_limit_outcome_p_weighted_fraud_edited, lower_limit_outcome_p_weighted_fraud_edited_output,
                                   upper_limit_outcome_p_weighted_fraud_edited, upper_limit_outcome_p_weighted_fraud_edited_output,
                                   
                                   control_outcome_p_weighted, control_outcome_p_weighted_output, unreliable_control_p, suppressed_control_p,
                                   control_p_diff_overall_significant,
                                   control_lower_limit_outcome_p_weighted, control_lower_limit_outcome_p_weighted_output, 
                                   control_upper_limit_outcome_p_weighted, control_upper_limit_outcome_p_weighted_output,
                                   
                                   diff_btw_treatment_p_weighted_and_control_p_weighted, diff_btw_treatment_p_weighted_and_control_p_weighted_output,
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound, 
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output,
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound,
                                   diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output,
                                   unreliable_diff_btw_treatment_p_weighted_and_control_p_weighted, 
                                   suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted,
                                   suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound,
                                   suppressed_diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound) %>% data.frame()


################################################################################


# create node from tree_output_w_hierarchy
tree_output_w_hierarchy %>% data.frame()

tree_node <- as.Node(tree_output_w_hierarchy)
tree_node

tree_node$fieldsAll
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

plot(tree_node)


#####################################################################


# reorder tree_output_w_hierarchy to match tree_node order
# this allows for adding variables to from tree_output_w_hierarchy directly to tree_node
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

tree_output_w_hierarchy %>% select(terminal_node)
print(tree_node, "terminal_node")

# get tree_node_terminal_node_order
tree_node_terminal_node_order <- tibble(terminal_node = unname(tree_node$Get("terminal_node")))
tree_node_terminal_node_order

# reorder tree_output_w_hierarchy
tree_output_w_hierarchy <- left_join(tree_node_terminal_node_order, tree_output_w_hierarchy, by = "terminal_node")

tree_output_w_hierarchy %>% select(terminal_node)
print(tree_node, "terminal_node")


####################################################################


# add custom node fillcolor and fontcolor variables to tree_output_w_hierarchy

# inspect colors
# display.brewer.all()
# brewer.pal(n = 9, name = "Blues")

tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(custom_node_fill_color = ifelse(final_terminal_node == 1, "#08306B", "#9ECAE1"),
                                                              custom_node_font_color = ifelse(final_terminal_node == 1, "white", "black"))
tree_output_w_hierarchy %>% data.frame()


#####################


# add viridis node colors based on denial rate scale
show_col(viridis_pal()(100))
viridis_pal_percentages <- viridis_pal()(100)
viridis_pal_percentages

# normalize and round fraud rate between 0-100, get color from viridis_pal_percentages, and get associated font color
tree_output_w_hierarchy %>% select(terminal_node, treatment_p_weighted_fraud)
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
        mutate(treatment_p_weighted_fraud_normalized = round((treatment_p_weighted_fraud / max(treatment_p_weighted_fraud)) * 100),
               custom_node_fill_color = viridis_pal_percentages[treatment_p_weighted_fraud_normalized],
               custom_node_font_color = ifelse(treatment_p_weighted_fraud_normalized > 40, "black", "white")) 


# # normalize and round denial rate between 0-100, get color from viridis_pal_percentages, and get associated font color
# tree_output_w_hierarchy %>% select(terminal_node, outcome_p_weighted)
# tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
#         mutate(outcome_p_weighted_normalized = round((outcome_p_weighted / max(outcome_p_weighted)) * 100),
#                custom_node_fill_color = viridis_pal_percentages[outcome_p_weighted_normalized],
#                custom_node_font_color = ifelse(outcome_p_weighted_normalized > 40, "black", "white")) 

tree_output_w_hierarchy %>% select(treatment_p_weighted_fraud_normalized, custom_node_fill_color, custom_node_font_color)


########################################################################


# create custom_split_leading_to_node edge labels
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# create custom_next_split_at_node
# custom_split_leading_to_node <- c("root_node", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
      
custom_split_leading_to_node <- c("root_node", "Ben. was married before", "Ben. region of birth is Latin/South America", 
                                  "Time since present marriage is less than or equal to 7 months",
                                  "Time since present marriage is greater than 7 months",
                                  "Ben. region of birth is not Latin/South America", "Ben. was not married before",
                                  "Ben. is in removal proceedings", "Ben. is not in removal proceedings")
  
length(custom_split_leading_to_node)
nrow(tree_output_w_hierarchy)

# add leading and trailing spaces
custom_split_leading_to_node <- str_c("  ", custom_split_leading_to_node, "     ")

# add custom_next_split_at_node to tree_node
tree_node$Set(custom_split_leading_to_node = custom_split_leading_to_node)

# inspect
tree_node$fieldsAll
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")


###################################################################


# create custom_node_labels
# legacy code - not used in final version

# print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
#       "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
#       "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")
# 
# print(tree_node, "terminal_node")
# 
# tree_output_w_hierarchy %>% select(terminal_node)
# 
# 
# # create custom_node_labels
# # note than tree_node does not keep the same order as tree_output_w_hierarchy
# custom_node_labels <- c("Ben. was married before?",
#                         "Ben. region of birth is not Latin America?",
#                         "Time since present marriage is less than or equal to 7 months?",
#                         "Ben. was married before &\n ben. region of birth is Latin America &\n time since present marriage is less than or equal to 7 months",
#                         "Ben. was married before &\n ben. region of birth is Latin America &\n time since present marriage is greater than 7 months",
#                         "Ben. was married before &\n ben. region of birth is not Latin America",
#                         "Ben. is in removal proceedings?",
#                         "Ben. was not married before &\n ben. is in removal proceedings",
#                         "Ben. was not married before &\n ben. is not in removal proceedings")
#                         
# length(custom_node_labels)
# nrow(tree_output_w_hierarchy)
# 
# 
# # add leading and trailing spaces
# # custom_split_leading_to_node <- str_c("  ", custom_split_leading_to_node, "     ")
# 
# # add custom_next_split_at_node to tree_node
# tree_node$Set(custom_node_labels = custom_node_labels)
# 
# # inspect
# tree_node$fieldsAll
# print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
#       "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
#       "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")


#########################################################################


# set custom node style
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "upper_limit_outcome_p_weighted_edited", "share_p_weighted")

# get node label
GetNodeLabel <- function(node) {
        
        # str_c("Node ", node$terminal_node, "\n",
        #       node$predicted_class_name, "\n", 
        #       node$positive_class_obs_in_node, " / ", node$negative_class_obs_in_node, "\n",
        #       percent(node$prob_positive_class), " / ", percent(node$prob_negative_class), "\n",
        #       percent(node$pct_obs_in_node))
        
        # node$custom_node_labels
        
        
        ################################################################
        
        
        current_terminal_node <- unname(node$Get("terminal_node"))[1]
        
        if(current_terminal_node == 1) {
                str_c("Fraud rate", "\n", 
                      "IBFA: ", node$treatment_p_weighted_fraud_output,
                      " (", node$lower_limit_outcome_p_weighted_fraud_edited_output, 
                      " to ", node$upper_limit_outcome_p_weighted_fraud_edited_output, ")", "\n", "\n",
                        "Denial rate", "\n", 
                      "IBFA: ", node$outcome_p_weighted_output, 
                      # " (", node$lower_limit_outcome_p_weighted_output, 
                      # use edited confidence interval from reid
                      " (", node$lower_limit_outcome_p_weighted_edited_output, 
                      # "-", node$upper_limit_outcome_p_weighted_output, ")", "\n",
                      " to ", node$upper_limit_outcome_p_weighted_edited_output, ")", "\n",
                      "Non-IBFA: ", node$control_outcome_p_weighted_output, 
                      " (", node$control_lower_limit_outcome_p_weighted_output, 
                      " to ", node$control_upper_limit_outcome_p_weighted_output, ")", "\n",
                      "\n",
                      "Fraud not denied\n",
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_output, " (", 
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output, "-", 
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output, ")", "\n",
                      "\n",
                      "% of All Petitions", "\n",
                      "IBFA: 100%", "\n", "Non-IBFA: 100%")
        } else {
                str_c("Fraud rate", "\n", 
                      "IBFA: ", node$treatment_p_weighted_fraud_output,
                      " (", node$lower_limit_outcome_p_weighted_fraud_edited_output, 
                      " to ", node$upper_limit_outcome_p_weighted_fraud_edited_output, ")", "\n", "\n",
                        "Denial rate", "\n", 
                      "IBFA: ", node$outcome_p_weighted_output, 
                      # " (", node$lower_limit_outcome_p_weighted_output, 
                      " (", node$lower_limit_outcome_p_weighted_edited_output, 
                      # "-", node$upper_limit_outcome_p_weighted_output, ")", "\n",
                      " to ", node$upper_limit_outcome_p_weighted_edited_output, ")", "\n",
                      "Non-IBFA: ", node$control_outcome_p_weighted_output, 
                      " (", node$control_lower_limit_outcome_p_weighted_output, 
                      " to ", node$control_upper_limit_outcome_p_weighted_output, ")", "\n",
                      "\n",
                      "Fraud not denied\n",
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_output, " (", 
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_lower_bound_output, " to ", 
                      node$diff_btw_treatment_p_weighted_and_control_p_weighted_upper_bound_output, ")", "\n",
                      "\n",
                      "% of All Petitions", "\n",
                      "IBFA: ", node$share_p_weighted_output,
                      " (", node$lower_limit_share_p_weighted_output, 
                      " to ", node$upper_limit_share_p_weighted_output, ")", "\n",
                      "Non-IBFA: ", node$control_share_p_weighted_output,
                      " (", node$control_lower_limit_share_p_weighted_output, 
                      " to ", node$control_upper_limit_share_p_weighted_output, ")")
        }
        
        
}

# write get_node_color function
set_node_style <- function(x) {
        current_terminal_node <- unname(x$Get("terminal_node"))[1]
        # print(current_terminal_node)
        
        # set custom variables for current_node
        current_node_fill_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_fill_color)
        current_node_font_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_font_color)
        
        # run SetNodeStyle for current_node
        SetNodeStyle(node = x, style = "filled, rounded", fontname = 'helvetica', label = GetNodeLabel(x), 
                     shape = "box", fillcolor = current_node_fill_color, fontcolor = current_node_font_color, inherit = FALSE)
}


##################################################################



# set custom edge style
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# get/set edge label
GetEdgeLabel <- function(node) {
        # node$split_leading_to_node
        node$custom_split_leading_to_node
}

# write get_node_color function
set_edge_style <- function(x) {
        # current_terminal_node <- unname(x$Get("terminal_node"))[1]
        # print(current_terminal_node)
        
        # set custom variables for current_node
        # current_node_fill_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_fill_color)

        # run SetNodeStyle for current_node
        SetEdgeStyle(node = x, fontname = 'helvetica', label = GetEdgeLabel(x), inherit = FALSE)
}


#################################################################


# create custom tree plot
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# set graph style
SetGraphStyle(tree_node, rankdir = "TB")

# set node style
tree_node$Do(function(x) { set_node_style(x) })

# set edge style
tree_node$Do(function(x) { set_edge_style(x) })

# plot tree
plot(tree_node)

# save plot
tree_plot_title <- str_c("pre_interview_tree_plot_Case_OutcomeDenied")
tree_plot_title

filename <- str_c("output/pre_interview/train/", tree_plot_title, ".pdf")
filename <- str_c("output/pre_interview/train/", tree_plot_title, ".png")
filename

export_graph(graph = ToDiagrammeRGraph(tree_node), file_name = filename)


# try creating png another way
# ppi <- 300
# png("output/pre_interview/train/sample.png",  width=6*ppi, height=6*ppi, res=ppi)
# plot(tree_node)
# dev.off()
# 
# export_graph(graph = ToDiagrammeRGraph(tree_node), file_name = filename, width = 1000, height = 557)


#####################################################################


# add custom legend

# build a legend in ggplot and add overlay on pdf image of data.tree using magick

# get max and min denial rate
max_treatment_p_weighted_fraud <- tree_output_w_hierarchy %>% summarize(max_treatment_p_weighted_fraud = max(treatment_p_weighted_fraud))
# max_outcome_p_weighted <- tree_output_w_hierarchy %>% summarize(max_outcome_p_weighted = max(outcome_p_weighted))

# get color hex values
show_col(viridis_pal()(10))
show_col(viridis_pal()(100))
viridis_pal()(100)

# create data for plot
viridis_percentage_scale_tbl <- tibble(x = seq(from = 0, to = max_treatment_p_weighted_fraud$max_treatment_p_weighted_fraud, by = .01), 
                                       y = seq(from = 0, to = max_treatment_p_weighted_fraud$max_treatment_p_weighted_fraud, by = .01))

# viridis_percentage_scale_tbl <- tibble(x = seq(from = 0, to = max_outcome_p_weighted$max_outcome_p_weighted, by = .01), 
#                                        y = seq(from = 0, to = max_outcome_p_weighted$max_outcome_p_weighted, by = .01))

# create plot
plot_w_legend <- viridis_percentage_scale_tbl %>% ggplot(data = ., aes(x = x, y = y, color = x)) + geom_point() + 
        scale_color_viridis_c(labels = percent) + 
        guides(color = guide_colorbar(title = "IBFA fraud rate"))
plot_w_legend

# plot_w_legend <- viridis_percentage_scale_tbl %>% ggplot(data = ., aes(x = x, y = y, color = x)) + geom_point() + 
#         scale_color_viridis_c(labels = percent) + 
#         guides(color = guide_colorbar(title = "IBFA denial rate"))

# save plot as png
ggsave(filename = "pre_interview_plot_w_legend_png.png", plot = plot_w_legend, dpi = 300)
ggsave(filename = "pre_interview_plot_w_legend_pdf.pdf", plot = plot_w_legend, dpi = 300)


# load png into magick
plot_w_legend_image <- image_read(path = "pre_interview_plot_w_legend_png.png")
image_info(plot_w_legend_image)
plot_w_legend_image

# pdf loses the points plotted for some reason??
# plot_w_legend_bitmap <- pdf_render_page(pdf = "plot_w_legend_pdf.pdf", page = 1, dpi = 300)
# plot_w_legend_image <- image_read(plot_w_legend_bitmap)
# plot_w_legend_image

# snip out legend
# image_crop(image, "100x150+50+100"): crop out width:100px and height:150px starting +50px from the leftmost, 
# + 50px from topmost
# legend <- image_crop(image = plot_w_legend_image, "600x550+600+250")
# crop_geometry <- geometry_area(width = 600, height = 550, x = 600, y = 250)
# legend <- image_crop(image = plot_w_legend_image, geometry = geometry_area(width = 600, height = 550, x = 600, y = 250))
legend <- image_crop(image = plot_w_legend_image, geometry = geometry_area(width = 700, height = 550, x = 900, y = 550))
legend

# save legend
image_write(image = legend, path = "pre_interview_legend.png", format = 'png', density = '300x300')
image_write(image = legend, path = "pre_interview_legend.pdf", format = 'pdf', density = '300x300')

# read legend
legend_bitmap <- pdf_render_page(pdf = "pre_interview_legend.pdf", page = 1, dpi = 300)
legend <- image_read(legend_bitmap)
legend


#############


# create plot with footnote
plot_w_footnote <- viridis_percentage_scale_tbl %>% ggplot(data = ., aes(x = x, y = y, color = x)) + geom_point() + 
        scale_color_viridis_c(labels = percent) + 
        guides(color = guide_colorbar(title = "IBFA denial rate")) + 
        labs(caption = str_c("* Significantly higher or lower than the rate for All Petitions\n",
        "X Determined to be unreliable due to a high relative sampling error, or diagnostics ", 
        "indicated the significance of the subgroup may have been the result of overfitting,\n",
        "or the Fraud Not Denied confidence interval includes negative values\n",
        "- Estimate suppressed due to excessive sampling error or small sample size, ", 
        "or the Fraud Not Denied estimate is less than zero\n",
                             "-- Confidence interval bound is below zero")) +
        theme(plot.caption = element_text(hjust = 0))
plot_w_footnote

# save plot_w_footnote
ggsave(filename = "pre_interview_plot_w_footnote_png.png", plot = plot_w_footnote, dpi = 300,
       width = 15)
ggsave(filename = "pre_interview_plot_w_footnote_pdf.pdf", plot = plot_w_footnote, dpi = 300,
       width = 15)

# load png into magick
plot_w_footnote_image <- image_read(path = "pre_interview_plot_w_footnote_png.png")
plot_w_footnote_image
image_info(plot_w_footnote_image)

# pdf loses the points plotted for some reason??
# plot_w_footnote_bitmap <- pdf_render_page(pdf = "plot_w_footnote_pdf.pdf", page = 1, dpi = 300)
# plot_w_footnote_image <- image_read(plot_w_footnote_bitmap)
# plot_w_footnote_image

# snip out legend
# image_crop(image, "100x150+50+100"): crop out width:100px and height:150px starting +50px from the leftmost, 
# + 50px from topmost
# footnote <- image_crop(image = plot_w_footnote_image, "2000x500+1300+550")
# footnote <- image_crop(image = plot_w_footnote_image, "1800x500+100+800")
# footnote <- image_crop(image = plot_w_footnote_image, "4000x500+100+875")
footnote <- image_crop(image = plot_w_footnote_image, "4000x300+100+1475")
footnote 

# save footnote
image_write(image = footnote, path = "pre_interview_footnote.png", format = 'png', density = '300x300')
image_write(image = footnote, path = "pre_interview_footnote.pdf", format = 'pdf', density = '300x300')

# read legend
footnote_bitmap <- pdf_render_page(pdf = "pre_interview_footnote.pdf", page = 1, dpi = 300)
footnote <- image_read(footnote_bitmap)
footnote


#############


# load tree_plot as magick image

# read tree_plot pdf
tree_plot_pdf_bitmap <- pdf_render_page(pdf = "output/pre_interview/train/pre_interview_tree_plot_Case_OutcomeDenied.pdf",
                page = 1, dpi = 300)
tree_plot_pdf_image <- image_read(tree_plot_pdf_bitmap)
tree_plot_pdf_image
image_info(tree_plot_pdf_image)

# overlay legend on tree plot

# old crop before adding fraud not captured to tree
# tree_plot_w_legend_image <- image_composite(image = image_border(image = tree_plot_pdf_image, color = "white", geometry = "1000x1000"), 
#                 composite_image = image_resize(image = legend, geometry = "2000x2000"), offset = "+8200+2000") %>% 
#         image_crop(image = ., "8158x3471+1000+1000") 
# tree_plot_w_legend_image

# old layout of legend and crop - needed to be revised to accomodate footnote and less width
# tree_plot_w_legend_image <- image_composite(image = image_border(image = tree_plot_pdf_image, color = "white", geometry = "1000x1000"), 
#                                             composite_image = image_resize(image = legend, geometry = "2000x2000"), offset = "+8200+2500") %>% 
#         image_crop(image = ., "8158x4500+1000+1000") 
# tree_plot_w_legend_image

image_info(legend)

tree_plot_w_legend_image <- image_composite(image = image_border(image = tree_plot_pdf_image, 
                                                                 color = "white", geometry = "1000x1000"), 
        composite_image = image_resize(image = legend, geometry = "1100x1100"), offset = "+7100+1400") 
tree_plot_w_legend_image
image_info(tree_plot_w_legend_image)

# overlay footnote
tree_plot_w_legend_and_footnote_image <- image_composite(image = tree_plot_w_legend_image, 
        composite_image = image_resize(image = footnote, geometry = "8500x8500"), offset = "+1000+6550") %>% 
        image_crop(image = ., "7450x6100+1000+1000") 
tree_plot_w_legend_and_footnote_image

# save tree plot
# pdf plot is too big - like 13MB
# image_write(image = tree_plot_w_legend_and_footnote_image, format = "pdf", 
#             path = "output/pre_interview/train/pre_interview_tree_plot_w_legend_and_footnote_Case_OutcomeDenied.pdf",
#             density = "300x300")

image_write(image = tree_plot_w_legend_and_footnote_image, format = "png", 
            path = "output/pre_interview/train/pre_interview_tree_plot_w_legend_and_footnote_Case_OutcomeDenied_20190320.png",
            density = "300x300")







