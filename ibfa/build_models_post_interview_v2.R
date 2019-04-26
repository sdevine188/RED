library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(survey)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(rlang)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(partykit)
library(ROCR)
library(forcats)
library(plotROC)
library(ROSE)
library(DMwR)
library(janitor)
library(randomForest)
library(tibble)

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")

# load function
# source("fancyRpartPlot.R")

# load treatment_pre_interview_dummies
treatment_post_interview_dummies <- read_csv("treatment_post_interview_dummies_v2_20180508.csv")
glimpse(treatment_post_interview_dummies)
dim(treatment_post_interview_dummies)

# need to set new working directory to save wo_Issue plots/tables
setwd("H:/RED/IBFA/analysis/output/post_interview_wo_issue")


#####################################################


# define outcome variable
outcome_variable <- "Case_OutcomeDenied"
outcome_label_positive <- "Denied"
outcome_label_negative <- "Not_Denied"

# outcome_variable <- "SOF_Finding_SPFF"
# outcome_label_positive <- "Fraud"
# outcome_label_negative <- "Not_Fraud"

# outcome_variable <- "Suf_EvidenceYes"
# outcome_label_positive <- "Suf_Evid"
# outcome_label_negative <- "Not_Suf_Evid."

outcome_variable_sym <- sym(outcome_variable)

# inspect treatment_post_interview_dummies
treatment_post_interview_dummies %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))

# create training and test data
set.seed(100)
in_train <- createDataPartition(y = treatment_post_interview_dummies %>% pull(!!outcome_variable_sym), p = 0.7, list = FALSE)

length(in_train)
head(in_train)

# create test/train split
treatment_post_interview_train <- treatment_post_interview_dummies[in_train, ]
dim(treatment_post_interview_train)
# glimpse(treatment_post_interview_train)

treatment_post_interview_test <- treatment_post_interview_dummies[-in_train, ]
dim(treatment_post_interview_test)
# glimpse(treatment_post_interview_test)

# review for missing values
sum_na_values <- function(column) {
        sum(is.na(column))
}

treatment_post_interview_train %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count") %>% filter(na_count > 0)

treatment_post_interview_test %>% map_dfr(., .f = sum_na_values) %>% data.frame(.) %>%
        gather(key = "variable", value = "na_count") %>% filter(na_count > 0)


#########################################################


# convert outcome variable to factor using outcome label
# train data
treatment_post_interview_train %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_train <- treatment_post_interview_train %>% 
        mutate(!!outcome_variable_sym := case_when((!!outcome_variable_sym) == 1 ~ outcome_label_positive, TRUE ~ outcome_label_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))

treatment_post_interview_train %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_train %>% select(!!outcome_variable_sym) %>% str()


# test data
treatment_post_interview_test %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_test <- treatment_post_interview_test %>% 
        mutate(!!outcome_variable_sym := case_when((!!outcome_variable_sym) == 1 ~ outcome_label_positive, TRUE ~ outcome_label_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_test %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_test %>% select(!!outcome_variable_sym) %>% str()


# full data
treatment_post_interview_full <- treatment_post_interview_dummies

treatment_post_interview_full %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_full <- treatment_post_interview_full %>% 
        mutate(!!outcome_variable_sym := case_when((!!outcome_variable_sym) == 1 ~ outcome_label_positive, TRUE ~ outcome_label_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_full %>% select(!!outcome_variable_sym) %>% glimpse()
treatment_post_interview_full %>% select(!!outcome_variable_sym) %>% str()


########################################################


# save train/test receipt numbers 
treatment_post_interview_train_receipt_numbers <- treatment_post_interview_train %>% select(receipt_number)
treatment_post_interview_train_receipt_numbers
write_csv(treatment_post_interview_train_receipt_numbers, "output/post_interview/train/treatment_post_interview_train_receipt_numbers_20180508.csv")

treatment_post_interview_test_receipt_numbers <- treatment_post_interview_test %>% select(receipt_number)
treatment_post_interview_test_receipt_numbers
write_csv(treatment_post_interview_test_receipt_numbers, "output/post_interview/test/treatment_post_interview_test_receipt_numbers_20180508.csv")


##########################################################


# detach receipt_number from training, test, and full data; save receipt numbers for full data for linking predictions at the end
treatment_post_interview_train <- treatment_post_interview_train %>% select(-receipt_number)
treatment_post_interview_test <- treatment_post_interview_test %>% select(-receipt_number)

treatment_post_interview_full_receipt_numbers <- treatment_post_interview_full %>% select(receipt_number)
treatment_post_interview_full_receipt_numbers %>% head()
treatment_post_interview_full <- treatment_post_interview_full %>% select(-receipt_number)


################################################
####################################################
##################################################


# create formula for tree

# get independent variables names of treatment_post_interview_dummies for use in creating model formula (remove dependent vars)
post_interview_model_variables_w_dummies <- names(treatment_post_interview_full)
dependent_variable_index <- which(post_interview_model_variables_w_dummies %in% 
                                          c("Case_OutcomeApproved", "Case_OutcomeDenied", "Case_OutcomePending",
                                           "SOF_Finding_SPFF", "SOF_Finding_SPFNF", "SOF_Finding_SPInconclusive", "SOF_Finding_SPPending",
                                           "Suf_EvidenceYes", "Suf_EvidenceNo", "Suf_EvidenceNA_value"))
post_interview_model_variables_w_dummies <- post_interview_model_variables_w_dummies[-dependent_variable_index]
post_interview_model_variables_w_dummies

# build formula
names(treatment_post_interview_dummies)
post_interview_model_variables_w_dummies

outcome_variable_left_side_equation <- str_c(outcome_variable, " ~ ", sep = "")

post_interview_model_variables_formula <- as.formula(str_c(outcome_variable_left_side_equation, 
                                                                 str_c(post_interview_model_variables_w_dummies, collapse = " + ")))
post_interview_model_variables_formula


###################################################################


# over/under sample minority/majority classes

# get baseline for imbalanced original data
# treatment_post_interview_train %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))
treatment_post_interview_train %>% tabyl(!!outcome_variable_sym) %>% adorn_totals("row") %>% adorn_pct_formatting()

# get factor levels for original data
# will need to relevel factors of new data to match 
treatment_post_interview_train %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_test %>% select(!!outcome_variable_sym) %>% str()

# ROSE
set.seed(100)
treatment_post_interview_train_rose <- ROSE(formula = post_interview_model_variables_formula, data = treatment_post_interview_train, seed = 1,
                                            na.action = na.pass)$data
treatment_post_interview_train_rose %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))

# relevel ROSE factors
treatment_post_interview_train_rose %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_train_rose <- treatment_post_interview_train_rose %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_train_rose %>% select(!!outcome_variable_sym) %>% str()



# SMOTE
# note the data needs to be wrapped in as.data.frame because SMOTE chokes on tibbles
set.seed(100)
treatment_post_interview_train_smote <- SMOTE(form = post_interview_model_variables_formula, data = as.data.frame(treatment_post_interview_train))
treatment_post_interview_train_smote %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))

# relevel smote factors
treatment_post_interview_train_smote %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_train_smote <- treatment_post_interview_train_smote %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_train_smote %>% select(!!outcome_variable_sym) %>% str()



# oversampling
# parameter N means "over sample minority class until the combined total obs of both classes equals N"
# so if Denied = 27, Not Denied = 227, then setting N = 454 means it will oversample Denied until it gets 227, making 227 + 227 = 454 = N
n_value <- treatment_post_interview_train %>% tabyl(!!outcome_variable_sym) %>% arrange(desc(n)) %>% select(n) %>% slice(1) %>% pull(n) * 2
treatment_post_interview_train_over <- ovun.sample(formula = post_interview_model_variables_formula, data = treatment_post_interview_train, 
                                                   seed = 1, method = "over", N = n_value, na.action = na.pass)$data
treatment_post_interview_train_over %>% tabyl(!!outcome_variable_sym) %>% adorn_totals("row") %>% adorn_pct_formatting()

# relevel over factors
treatment_post_interview_train_over %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_train_over <- treatment_post_interview_train_over %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_train_over %>% select(!!outcome_variable_sym) %>% str()


# both under/oversampling
n_value <- treatment_post_interview_train %>% nrow()
treatment_post_interview_train_both <- ovun.sample(formula = post_interview_model_variables_formula, data = treatment_post_interview_train, 
                                                   seed = 1, method = "both", N = n_value, na.action = na.pass)$data
treatment_post_interview_train_both %>% tabyl(!!outcome_variable_sym) %>% adorn_totals("row") %>% adorn_pct_formatting()

# relevel both factors
treatment_post_interview_train_both %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_train_both <- treatment_post_interview_train_both %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
treatment_post_interview_train_both %>% select(!!outcome_variable_sym) %>% str()


######################################################


# baseline accuracy
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_train %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))
dim(treatment_post_interview_train)

treatment_post_interview_test %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))
dim(treatment_post_interview_test)

# crosstab tables
treatment_post_interview_dummies %>% tabyl(Case_OutcomeDenied)
treatment_post_interview_dummies %>% tabyl(SOF_Finding_SPFF)

treatment_post_interview_dummies %>% tabyl(Case_OutcomeDenied, SOF_Finding_SPFF, Suf_EvidenceYes) %>%
        adorn_percentages(denominator = "all") %>% adorn_totals(c("row", "col")) %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title()

treatment_post_interview_dummies %>% tabyl(Case_OutcomeDenied, SOF_Finding_SPFF) %>%
        adorn_percentages(denominator = "all") %>% adorn_totals(c("row", "col")) %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title()


##########################################################


# quickly inspect performance output of various training data using rpart

# compile training datasets to using in model 
train_data_tbl <- tibble(df = list(treatment_post_interview_train, treatment_post_interview_train_smote, treatment_post_interview_train_rose, 
                                   treatment_post_interview_train_over, treatment_post_interview_train_both), names = c("imbalanced", "smote", "rose", "over", "both"))
glimpse(train_data_tbl)

# get_tree_performance_rpart
source("get_performance_for_trees_rpart.R")
rpart_output <- get_performance_for_trees_rpart(train_data_tbl = train_data_tbl)
rpart_output


########################################################


# manually build models with rpart 

# build model without minbucket restriction
# treatment_post_interview_train_rpart_unrestricted <- rpart(formula = post_interview_model_variables_formula,
#                                               data = treatment_post_interview_train, method = "class",
#                                               minsplit = 2, cp = -1)
# 
# # inspect model
# treatment_post_interview_train_rpart_unrestricted
# rpart.plot(treatment_post_interview_train_rpart_unrestricted, tweak = 1.3)

# assign train_data
train_data <- treatment_post_interview_train
# train_data <- treatment_post_interview_train_smote
# train_data <- treatment_post_interview_train_rose
# train_data <- treatment_post_interview_train_over
# train_data <- treatment_post_interview_train_both

# build model using rpart
set.seed(100)
treatment_post_interview_train_rpart <- rpart(formula = post_interview_model_variables_formula,
                                                      data = train_data, method = "class",
                                                      minsplit = 2, minbucket = 10, cp = -1)

# inspect model
treatment_post_interview_train_rpart
# treatment_post_interview_train_rpart$splits
# treatment_post_interview_train_rpart$cptable

# plot tree
tree_plot_title <- str_c(outcome_variable, " - training - post_interview")
tree_plot_title
rpart.plot(treatment_post_interview_train_rpart, tweak = 1.3, extra = 104, nn = TRUE, main = tree_plot_title)
# prp(treatment_post_interview_train_rpart)
# fancyRpartPlot(treatment_post_interview_train_rpart, tweak = 1.5)

# get ROC on test set
treatment_post_interview_train_rpart_pred_prob <- predict(object = treatment_post_interview_train_rpart$finalModel, newdata = treatment_post_interview_test,
                                                                       type = "prob")
# since ROCR can't handle tidy evaluation, need to create separate vector with test set outcome
test_outcome_variable <- treatment_post_interview_test %>% pull(!!outcome_variable_sym)
treatment_post_interview_train_rpart_prediction_obj <- prediction(predictions = treatment_post_interview_train_rpart_pred_prob[ , 1], 
                                                labels = test_outcome_variable, label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_train_rpart_performance_obj <- performance(prediction.obj = treatment_post_interview_train_rpart_prediction_obj, measure = "tpr", x.measure = "fpr")
plot(treatment_post_interview_train_rpart_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)

# get AUC on test 
treatment_post_interview_train_rpart_auc <- performance(prediction.obj = treatment_post_interview_train_rpart_prediction_obj, measure = "auc")
treatment_post_interview_train_rpart_auc@y.values

# confusion matrix
treatment_post_interview_train_rpart_pred_class <- predict(object = treatment_post_interview_train_rpart$finalModel, newdata = treatment_post_interview_test, type = "class")
confusionMatrix(data = treatment_post_interview_train_rpart_pred_class, reference = test_outcome_variable, positive = outcome_label_positive)


##########################################################


# quickly inspect performance output of various training data using rpart

# compile training datasets to using in model 
# removed ROSE data because it was broke in some way - massive underperformance, weird values, etc
train_data_tbl <- tibble(df = list(treatment_post_interview_train, treatment_post_interview_train_smote, 
                                   treatment_post_interview_train_over, treatment_post_interview_train_both), 
                         names = c("imbalanced", "smote", "over", "both"))
glimpse(train_data_tbl)

# get_tree_performance_rpart
source("get_performance_for_trees_caret.R")
caret_output <- get_performance_for_trees_caret(train_data_tbl = train_data_tbl)
caret_output


###############################################################


# manually build caret model

# assign train_data
# train_data <- treatment_post_interview_train
train_data <- treatment_post_interview_train_smote
# train_data <- treatment_post_interview_train_rose
# train_data <- treatment_post_interview_train_over
# train_data <- treatment_post_interview_train_both


############################################################


# try randomForest for comparison
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# mtry <- sqrt(ncol(train_data))
tuneRF_x <- select(train_data, -c(Case_OutcomeDenied, Case_OutcomeApproved, SOF_Finding_SPFF, SOF_Finding_SPFNF, SOF_Finding_SPInconclusive,
                                 SOF_Finding_SPPending, Suf_EvidenceNA_value, Suf_EvidenceNo, Suf_EvidenceYes))
dim(tuneRF_x)
tuneRF_y <- pull(train_data, !!outcome_variable_sym)
glimpse(tuneRF_y)
length(tuneRF_y)
set.seed(100)
mtry_grid <- tuneRF(x = tuneRF_x, y = tuneRF_y, stepFactor = 1.5, improve = 1e-5, ntree = 500)
mtry <- data.frame(mtry_grid)
mtry <- mtry %>% filter(OOBError == min(OOBError)) %>% pull(mtry)
mtry
tunegrid <- expand.grid(.mtry = mtry)

set.seed(100)
treatment_post_interview_train_rf_caret <- train(form = post_interview_model_variables_formula, data = train_data, method = "rf",
                                                    trControl = train_control, tuneGrid = tunegrid, metric = "Accuracy", ntree = 2000)

treatment_post_interview_train_rf_caret
summary(treatment_post_interview_train_rf_caret)
treatment_post_interview_train_rf_caret$finalModel
train_data %>% tabyl(Case_OutcomeDenied)
treatment_post_interview_train_rf_caret$results

# get ROC on test set
treatment_post_interview_train_rf_caret_pred_prob <- predict(object = treatment_post_interview_train_rf_caret$finalModel, newdata = treatment_post_interview_test,
                                                                type = "prob")

# since ROCR can't handle tidy evaluation, need to create separate vector with test set outcome
test_outcome_variable <- treatment_post_interview_test %>% pull(!!outcome_variable_sym)
treatment_post_interview_train_rf_caret_prediction_obj <- prediction(predictions = treatment_post_interview_train_rf_caret_pred_prob[ , 1], 
                                                                        labels = test_outcome_variable, label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_train_rf_caret_performance_obj <- performance(prediction.obj = treatment_post_interview_train_rf_caret_prediction_obj, measure = "tpr", 
                                                                          x.measure = "fpr")
plot(treatment_post_interview_train_rf_caret_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)

# get AUC
treatment_post_interview_train_rf_caret_auc <- performance(prediction.obj = treatment_post_interview_train_rf_caret_prediction_obj, measure = "auc")
treatment_post_interview_train_rf_caret_auc@y.values
treatment_post_interview_train_rf_caret_auc@y.values[[1]]

# confusion matrix
treatment_post_interview_train_rf_caret_pred_class <- predict(object = treatment_post_interview_train_rf_caret$finalModel, newdata = treatment_post_interview_test,
                                                              type = "class")
confusionMatrix(data = treatment_post_interview_train_rf_caret_pred_class, reference = test_outcome_variable, positive = outcome_label_positive)


###########################################################


# use k-fold cross validation
# train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

# cp_grid <- expand.grid(.cp = seq(.01, .5, .01))
cp_grid <- expand.grid(.cp = seq(0, 0.5, 0.005))
# cp_grid

# can't get caret to implement minbucket or minsplit, so it keeps returning zero splits due to imbalanced data, w metric = accuracy
# though metric = ROC does split
# note that minbucket is misleading when using SMOTE data, since the # of obs per class is altered 
# https://stackoverflow.com/questions/36781755/how-to-specify-minbucket-in-caret-train-for?lq=1
# treatment_post_interview_train_rpart2 <- train(form = post_interview_model_variables_formula, data = train_data, 
#                                                method = "rpart", trControl = train_control, tuneGrid = cp_grid, metric = "ROC",
#                                                control = rpart.control(minbucket = 15))
# treatment_post_interview_train_rpart2$finalModel

# use default accuracry metric
# set.seed(100)
# treatment_post_interview_train_rpart_caret <- train(form = post_interview_model_variables_formula, data = train_data, method = "rpart",
#                                               trControl = train_control, tuneGrid = cp_grid)

# use ROC metric
set.seed(100)
treatment_post_interview_train_rpart_caret <- train(form = post_interview_model_variables_formula, data = train_data, method = "rpart",
                                                    trControl = train_control, tuneGrid = cp_grid, metric = "ROC", na.action = na.pass)

# inspect model
treatment_post_interview_train_rpart_caret
treatment_post_interview_train_rpart_caret$finalModel


attributes(treatment_post_interview_train_rpart_caret$finalModel)
treatment_post_interview_train_rpart_caret$finalModel$splits
summary(treatment_post_interview_train_rpart_caret$finalModel)

# inspect surrogate splits
cp_table <- as.tibble(treatment_post_interview_train_rpart_caret$finalModel$cptable) 
cp_value <- cp_table %>% filter(`rel error` == min(`rel error`)) %>% pull(CP)
cp_value
summary(treatment_post_interview_train_rpart_caret$finalModel, cp = cp_value)

# plot tree
tree_plot_title <- str_c(outcome_variable, " - post_interview - SMOTE data")
tree_plot_title

filename <- str_c("output/post_interview/train/tree_plot_", tree_plot_title, ".pdf")
filename

pdf(filename, width = 20, height = 8.5)
rpart.plot(treatment_post_interview_train_rpart_caret$finalModel, tweak = 1, extra = 104, nn = TRUE, main = tree_plot_title)
dev.off()


attributes(treatment_post_interview_train_rpart_caret)
attributes(treatment_post_interview_train_rpart_caret$finalModel)
treatment_post_interview_train_rpart_caret$finalModel$splits
treatment_post_interview_train_rpart_caret$finalModel$frame
treatment_post_interview_train_rpart_caret$finalModel$cptable
treatment_post_interview_train_rpart_caret$finalModel$tuneValue

treatment_post_interview_train_rpart_caret$bestTune
treatment_post_interview_train_rpart_caret$control
treatment_post_interview_train_rpart_caret$results
treatment_post_interview_train_rpart_caret$modelInfo
plot(treatment_post_interview_train_rpart_caret)
print(treatment_post_interview_train_rpart_caret)

# ggplot(treatment_post_interview_train_rpart_caret$results, aes(x = cp, y = Accuracy)) + geom_line() + geom_point()
ggplot(treatment_post_interview_train_rpart_caret$results, aes(x = cp, y = ROC)) + geom_line() + geom_point()

getTrainPerf(treatment_post_interview_train_rpart_caret)
getTrainPerf(treatment_post_interview_train_rpart_caret)$TrainAccuracy
getTrainPerf(treatment_post_interview_train_rpart_caret)$TrainKappa
treatment_post_interview_train_rpart_caret$resample
# mean(treatment_post_interview_train_rpart_caret$resample$Accuracy)
# mean(treatment_post_interview_train_rpart_caret$resample$Kappa)

# save cross validated performance metrics
cv_train_auc <- getTrainPerf(treatment_post_interview_train_rpart_caret)$TrainROC
cv_train_specificity <- getTrainPerf(treatment_post_interview_train_rpart_caret)$TrainSpec
cv_train_sensitivity <- getTrainPerf(treatment_post_interview_train_rpart_caret)$TrainSens

# get ROC on test set
treatment_post_interview_train_rpart_caret_pred_prob <- predict(object = treatment_post_interview_train_rpart_caret$finalModel, newdata = treatment_post_interview_test,
                                                          type = "prob")
# since ROCR can't handle tidy evaluation, need to create separate vector with test set outcome
test_outcome_variable <- treatment_post_interview_test %>% pull(!!outcome_variable_sym)
treatment_post_interview_train_rpart_caret_prediction_obj <- prediction(predictions = treatment_post_interview_train_rpart_caret_pred_prob[ , 1], 
                                        labels = test_outcome_variable, label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_train_rpart_caret_performance_obj <- performance(prediction.obj = treatment_post_interview_train_rpart_caret_prediction_obj, measure = "tpr", 
                                                                          x.measure = "fpr")
plot(treatment_post_interview_train_rpart_caret_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)

# get AUC
treatment_post_interview_train_rpart_caret_auc <- performance(prediction.obj = treatment_post_interview_train_rpart_caret_prediction_obj, measure = "auc")
treatment_post_interview_train_rpart_caret_auc@y.values
treatment_post_interview_train_rpart_caret_auc@y.values[[1]]

# confusion matrix
treatment_post_interview_train_rpart_caret_pred_class <- predict(object = treatment_post_interview_train_rpart_caret$finalModel, newdata = treatment_post_interview_test, 
                                                                 type = "class")
confusionMatrix(data = treatment_post_interview_train_rpart_caret_pred_class, reference = test_outcome_variable, positive = outcome_label_positive)

# get surrogate output
current_wd <- getwd()
setwd("H:/R/rpart")

source("get_surrogate_output.R")
surrogate_output <- get_surrogate_output(treatment_post_interview_train_rpart_caret$finalModel)
surrogate_output

setwd(current_wd)

# save surrogate output
surrogate_file_name <- str_c("output/post_interview/train/treatment_post_interview_train_surrogate_output_", outcome_variable, ".csv")
surrogate_file_name
write_csv(x = surrogate_output, path = surrogate_file_name)

# save model
model_file_name <- str_c("output/post_interview/train/treatment_post_interview_train_rpart_caret_model_", outcome_variable, ".rds")
model_file_name
saveRDS(object = treatment_post_interview_train_rpart_caret$finalModel, file = model_file_name)
model <- readRDS(file = "output/post_interview/train/treatment_post_interview_train_rpart_caret_model_Case_OutcomeDenied.rds")


############################################################


# get clean model output of stats for each node
treatment_post_interview_train_rpart_caret_output <- treatment_post_interview_train_rpart_caret$finalModel$frame
treatment_post_interview_train_rpart_caret_output_part2 <- data.frame(treatment_post_interview_train_rpart_caret$finalModel$frame$yval2)
glimpse(treatment_post_interview_train_rpart_caret_output)
glimpse(treatment_post_interview_train_rpart_caret_output_part2)
treatment_post_interview_train_rpart_caret_output <- cbind(treatment_post_interview_train_rpart_caret_output, 
                                                           treatment_post_interview_train_rpart_caret_output_part2)
glimpse(treatment_post_interview_train_rpart_caret_output)
treatment_post_interview_train_rpart_caret_output

# clean model output
# note that output dataframe has same stats for each node number as the rpart output object, 
# but the variable listed for each node number in the df is actually the variable listed for the subsequent node number on output object
# basically, if you just ignore the variable listed on the rpart output object, and refer only by node number, it's the same
# the output object is listing the variable that HAS been split on to get to current node, but
# the df is listing the variable that will be split on FROM that node

treatment_post_interview_train_rpart_caret_output <- treatment_post_interview_train_rpart_caret_output %>% select(-yval2, V1) %>%
        rename(obs_in_node = n, var_to_be_split_next = var, misclassified_count = dev, predicted_class = yval,
               class_1_obs_in_node = V2, class_2_obs_in_node = V3, prob_class_1 = V4, 
               prob_class_2 = V5, pct_obs_in_node = nodeprob) %>%
        mutate(terminal_node = rownames(treatment_post_interview_train_rpart_caret_output))

treatment_post_interview_train_rpart_caret_output
glimpse(treatment_post_interview_train_rpart_caret_output)

# filter output to just terminal nodes
treatment_post_interview_train_rpart_caret_output_terminal_nodes <- treatment_post_interview_train_rpart_caret_output %>% 
        filter(var_to_be_split_next == "<leaf>") %>% 
        arrange(desc(prob_class_2))

treatment_post_interview_train_rpart_caret_output_terminal_nodes


##########################################################


# get leaf node paths 
# ended up building custom function
# https://stackoverflow.com/questions/36086990/how-to-climb-the-tree-structure-of-rpart-object-using-path-in-order-to-purge-man
# https://stackoverflow.com/questions/13548266/define-all-functions-in-one-r-file-call-them-from-another-r-file-how-if-pos

# call get_node_path function
source("get_node_paths.R")

treatment_post_interview_train_rpart_caret_node_paths <- get_node_paths(model_fit = treatment_post_interview_train_rpart_caret$finalModel, 
                                        output_terminal_nodes = treatment_post_interview_train_rpart_caret_output_terminal_nodes)
treatment_post_interview_train_rpart_caret_node_paths

# add node paths to output_terminal nodes
treatment_post_interview_train_rpart_caret_output_terminal_nodes <- left_join(treatment_post_interview_train_rpart_caret_output_terminal_nodes,
                                                                                 treatment_post_interview_train_rpart_caret_node_paths,
                                                                                 by = c("terminal_node" = "terminal_node"))
treatment_post_interview_train_rpart_caret_output_terminal_nodes


##################################################################


# get predicted terminal nodes for train set - node # is same as "terminal_node" variable created from $frame output rownames
# note we are predicting terminal nodes for the original training set, not the SMOTE training set
source("predict_nodes.R")

treatment_post_interview_train_rpart_caret_pred_node <- predict_nodes(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                                        newdata = treatment_post_interview_train)
treatment_post_interview_train_rpart_caret_pred_node

# create updated train data including the terminal_node and weights, which is used in get_node_stats function
# note that this is using the original training data - not the SMOTE data
treatment_post_interview_train_updated <- treatment_post_interview_train %>% 
        mutate(terminal_node = treatment_post_interview_train_rpart_caret_pred_node,
                                        weight = case_when(BEN_COUNTRY_OF_BIRTHMEXIC == 1 ~ BEN_COUNTRY_OF_BIRTHMEXIC * 1.24, 
                                                           BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                                                           BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1))
unique(treatment_post_interview_train_updated$weight)
unique(treatment_post_interview_train_updated$terminal_node)
# names(treatment_post_interview_train_updated)


#####################################################################


# get node stats for training data
source("get_node_stats.R")

treatment_post_interview_train_node_stats <- get_node_stats(data_updated_w_terminal_nodes_and_weights = treatment_post_interview_train_updated,
                                           tree_output_terminal_nodes = treatment_post_interview_train_rpart_caret_output_terminal_nodes,
                                           outcome_variable = outcome_variable)
treatment_post_interview_train_node_stats %>% data.frame()


################################################################


# create train_model_node_order for use in getting node_stats for test and full datasets
train_model_node_order <- treatment_post_interview_train_node_stats %>% pull(terminal_node)
train_model_node_order


##############################################################


# create filtered list of high-probability and significant terminal nodes
treatment_post_interview_train_node_stats_significant_above <- treatment_post_interview_train_node_stats %>% 
        filter(p_diff_overall_significance == 1 & p_weighted > p_weighted_overall)
treatment_post_interview_train_node_stats_significant_above %>% data.frame(.)

treatment_post_interview_train_node_stats_significant_below <- treatment_post_interview_train_node_stats %>% 
        filter(p_diff_overall_significance == 1 & p_weighted < p_weighted_overall)
treatment_post_interview_train_node_stats_significant_below %>% data.frame(.)

# create filtered list of high-probability terminal nodes
treatment_post_interview_train_node_stats_above_avg <- treatment_post_interview_train_node_stats %>% 
        filter(p_weighted > p_weighted_overall)
treatment_post_interview_train_node_stats_above_avg %>% data.frame(.)

# create filtered list of low-probability terminal nodes
treatment_post_interview_train_node_stats_below_avg <- treatment_post_interview_train_node_stats %>% 
        filter(p_weighted < p_weighted_overall)
treatment_post_interview_train_node_stats_below_avg %>% data.frame(.)


#################################################################


# get roc curve for train data

# get pred_prob
post_interview_train_rpart_pred_prob <- predict(treatment_post_interview_train_rpart_caret$finalModel, 
                                               newdata = treatment_post_interview_train, type = "prob")
post_interview_train_rpart_pred_prob

treatment_post_interview_train_updated <- treatment_post_interview_train_updated %>% 
        mutate(pred_prob = post_interview_train_rpart_pred_prob[ , 1])

# # add p_weight to treatment_post_interview_train_updated
# treatment_post_interview_train_updated <- treatment_post_interview_train_node_stats %>% 
#         mutate(terminal_node = as.integer(terminal_node)) %>%
#         select(terminal_node, p_weighted) %>%
#         left_join(treatment_post_interview_train_updated, ., by = c("terminal_node" = "terminal_node"))
# 
# treatment_post_interview_train_updated %>% distinct(terminal_node, p_weighted) %>% arrange(desc(p_weighted))
# treatment_post_interview_train_node_stats %>% select(terminal_node, p_weighted) %>% arrange(desc(p_weighted))

# get outcome variable for use as label in prediction function
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_train_outcome_variable <- treatment_post_interview_train_updated %>% pull(!!outcome_variable_sym)

# get outcome variable dummy for use as label in geom_roc
outcome_variable_dummy <- str_c(outcome_variable, "_dummy", sep = "")
outcome_variable_dummy_sym <- sym(outcome_variable_dummy)
treatment_post_interview_train_updated <- treatment_post_interview_train_updated %>%
        mutate(!!outcome_variable_dummy_sym := case_when(!!outcome_variable_sym == outcome_label_positive ~ 1,
                                                 !!outcome_variable_sym == outcome_label_negative ~ 0))
treatment_post_interview_train_outcome_variable_dummy <- treatment_post_interview_train_updated %>% pull(!!outcome_variable_dummy_sym)

# create roc curve data
train_roc_curve_data <- data.frame(pred_prob = treatment_post_interview_train_updated$pred_prob,
                             outcome_variable = treatment_post_interview_train_outcome_variable,
                             outcome_variable_dummy = treatment_post_interview_train_outcome_variable_dummy)
train_roc_curve_data
glimpse(train_roc_curve_data)

# create roc curve
train_roc_curve <- ggplot(train_roc_curve_data, aes(d = outcome_variable_dummy, m = pred_prob)) + geom_roc() +
        ggtitle(str_c("ROC curve for training dataset - ", outcome_variable))
train_roc_curve

# get auc
# train_sorted_auc <- roc.curve(response = train_roc_curve_data$outcome_variable, predicted = train_roc_curve_data$p_weighted, plotit = FALSE)
# train_sorted_auc
# str(train_sorted_auc)
# train_sorted_auc$auc

# get performance using ROCR

treatment_post_interview_train_rpart_caret_prediction_obj <- prediction(predictions = train_roc_curve_data$pred_prob, 
                                                                        labels = train_roc_curve_data$outcome_variable, 
                                                                label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_train_rpart_caret_performance_obj <- performance(prediction.obj = treatment_post_interview_train_rpart_caret_prediction_obj, 
                                                                          measure = "tpr", 
                                                                          x.measure = "fpr")

# print roc curve
train_roc_curve_ROCR_filename <- str_c("output/post_interview/train/train_roc_curve_ROCR_", outcome_variable, " - post_interview", ".pdf")
train_roc_curve_ROCR_filename

pdf(train_roc_curve_ROCR_filename, width = 10, height = 10)
plot(treatment_post_interview_train_rpart_caret_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)
dev.off()


# get AUC
treatment_post_interview_train_rpart_caret_auc <- performance(prediction.obj = treatment_post_interview_train_rpart_caret_prediction_obj, 
                                                              measure = "auc")
treatment_post_interview_train_rpart_caret_auc@y.values
train_auc <- data.frame(auc = treatment_post_interview_train_rpart_caret_auc@y.values[[1]])


# confusion matrix
treatment_post_interview_train_rpart_caret_pred_class <- predict(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                                 newdata = treatment_post_interview_train, type = "class")
confusion_matrix <- confusionMatrix(data = treatment_post_interview_train_rpart_caret_pred_class, reference = train_roc_curve_data$outcome_variable, 
                positive = outcome_label_positive)
confusion_matrix

# confusion_matrix performance metrics
train_accuracy <- confusion_matrix$overall["Accuracy"]
train_kappa <- confusion_matrix$overall["Kappa"]
train_specificity <- confusion_matrix$byClass["Specificity"]
train_sensitivity <- confusion_matrix$byClass["Sensitivity"]
train_balanced_accuracy <- confusion_matrix$byClass["Balanced Accuracy"]
train_pos_pred_value <- confusion_matrix$byClass["Pos Pred Value"]
train_neg_pred_value <- confusion_matrix$byClass["Neg Pred Value"]

# verify confusion matrix table
treatment_post_interview_train_rpart_caret_pred_class %>% tabyl()
treatment_post_interview_train_updated %>% tabyl(!!outcome_variable_sym)
train_roc_curve_data %>% tabyl(outcome_variable)


# save chart
# sorted_train_roc_curve_filename <- str_c("output/post_interview/train/train_roc_curve_sorted_", outcome_variable, " - post_interview", ".pdf")
# sorted_train_roc_curve_filename
# 
# ggsave(filename = sorted_train_roc_curve_filename, plot = sorted_train_roc_curve, width = 17, height = 10, 
#        units = "in", dpi = 900)


#############################################################


# create roc curve manually
# need to order nodes by node_order_train, since that node order is what we would have used to assign obs to having positive outcomes
# so when evaluating what tpr we wouldve gotten if we classified node according to train model, we need to incrementally assign
# nodes as positive according to the training model ordering of high prob nodes
# it would be unfair to evalute the test set by seeing what tpr results from incrementally assigning the nodes as positive according to the
# test set node probabilities/order
# node_order_train <- treatment_post_interview_train_node_stats %>% pull(terminal_node)
# node_order_train
# 
# x <- treatment_post_interview_train_node_stats[order(match(treatment_post_interview_train_node_stats$terminal_node, node_order_train)), ]
# treatment_post_interview_train_node_stats_reordered <- x
# 
# treatment_post_interview_train_node_stats_reordered %>% select(terminal_node, p_weighted)
# treatment_post_interview_train_node_stats %>% select(terminal_node, p_weighted)



# # plot manual roc curve
treatment_post_interview_train_node_stats_roc <- rbind(treatment_post_interview_train_node_stats[1, ], 
                                                       treatment_post_interview_train_node_stats)
treatment_post_interview_train_node_stats_roc$cum_false_positive_rate_weighted[1] <- 0
treatment_post_interview_train_node_stats_roc$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_train_node_stats_roc$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
train_roc_curve <- ggplot(treatment_post_interview_train_node_stats_roc, aes(x = cum_false_positive_rate_weighted, y = cum_true_positive_rate_weighted)) +
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) + 
        ggtitle(str_c("ROC curve for training dataset - manual - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed")
train_roc_curve

# save chart
train_roc_curve_filename <- str_c("output/post_interview/train/train_roc_curve_manual_", outcome_variable, " - post_interview", ".pdf")
train_roc_curve_filename

ggsave(filename = train_roc_curve_filename, plot = train_roc_curve, width = 10, height = 10, 
       units = "in", dpi = 900)

# get roc auc
# treatment_post_interview_train_rpart_caret_roc_pred <- prediction(predictions = treatment_post_interview_train_updated$p_weighted,
#                                                                      labels = treatment_post_interview_train_outcome_variable)
# 
# treatment_post_interview_train_rpart_caret_roc_auc <- performance(treatment_post_interview_train_rpart_caret_roc_pred, measure = "auc")
# treatment_post_interview_train_rpart_caret_roc_auc@y.values


#######################################################################


# get lift chart for training data
# train_roc_curve_data_factor <- data.frame(p_weighted = treatment_post_interview_train_updated$p_weighted,
#                                    outcome_variable = factor(treatment_post_interview_train_outcome_variable))
# str(train_roc_curve_data_factor$outcome_variable)
# head(train_roc_curve_data_factor)

# need to relevel the factor so that first level is the positive class
# train_roc_curve_data_factor$outcome_variable <- fct_relevel(train_roc_curve_data_factor$outcome_variable, "1", "0")
# str(train_roc_curve_data_factor$outcome_variable)

train_lift_obj <- caret::lift(outcome_variable ~ pred_prob, data = train_roc_curve_data)
attributes(train_lift_obj)
train_lift_obj$data
train_lift_obj$pct

# create baseline
line1 <- data.frame(x1 = 0, x2 = 100, y1 = 0, y2 = 100)
line2 <- data.frame(x1 = 0, x2 = train_lift_obj$pct, y1 = 0, y2 = 100)
line3 <- data.frame(x1 = train_lift_obj$pct, x2 = 100, y1 = 100, y2 = 100)

# plot cumulative gains chart
train_gains_chart <- ggplot(train_lift_obj$data) + geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_line(aes(x = CumTestedPct, y = CumEventPct, col = liftModelVar)) + 
        ggtitle(str_c("Gains chart for training dataset - ", outcome_variable)) +
        ylab("True Positive Rate") + xlab("Rate of Positive Predictions")
train_gains_chart

# save chart
train_gains_chart_filename <- str_c("output/post_interview/train/train_gains_chart_", outcome_variable, " - post_interview", ".pdf")
train_gains_chart_filename

ggsave(filename = train_gains_chart_filename, plot = train_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


#################################################


# create cumulative gains chart manually
treatment_post_interview_train_node_stats_gains <- rbind(treatment_post_interview_train_node_stats[1, ], treatment_post_interview_train_node_stats)
treatment_post_interview_train_node_stats_gains$cum_pct_weighted_obs[1] <- 0
treatment_post_interview_train_node_stats_gains$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_train_node_stats_gains$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
line2 <- data.frame(x1 = 0, x2 = treatment_post_interview_train_node_stats_gains$p_weighted_overall[1], y1 = 0, y2 = 1)
line3 <- data.frame(x1 = treatment_post_interview_train_node_stats_gains$p_weighted_overall[1], x2 = 1, y1 = 1, y2 = 1)

train_gains_chart <- ggplot(treatment_post_interview_train_node_stats_gains, aes(x = cum_pct_weighted_obs, 
                                                                                 y = cum_true_positive_rate_weighted)) + 
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) +
        ggtitle(str_c("Gains chart for training dataset - manual - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed") + 
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted") +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted")
train_gains_chart

# save chart
train_gains_chart_filename <- str_c("output/post_interview/train/train_gains_chart_manual_", outcome_variable, " - post_interview", ".pdf")
train_gains_chart_filename

ggsave(filename = train_gains_chart_filename, plot = train_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


################################################################
#################################################################
#################################################################


# get predicted terminal nodes for test set - node # is same as "terminal_node" variable created from $frame output rownames
source("predict_nodes.R")

treatment_post_interview_test_rpart_pred_node <- predict_nodes(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                                       newdata = treatment_post_interview_test)
treatment_post_interview_test_rpart_pred_node

# create updated test data including the terminal_node and weights
treatment_post_interview_test_updated <- treatment_post_interview_test %>% 
        mutate(terminal_node = treatment_post_interview_test_rpart_pred_node,
                                          weight = case_when(BEN_COUNTRY_OF_BIRTHMEXIC == 1 ~ BEN_COUNTRY_OF_BIRTHMEXIC * 1.24, 
                                                             BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                                                             BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1))
unique(treatment_post_interview_test_updated$weight)
unique(treatment_post_interview_test_updated$terminal_node)

# create output for test set fit
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_test_outcome_variable <- treatment_post_interview_test %>% pull(!!outcome_variable_sym)
treatment_post_interview_test_rpart_output <- data.frame(treatment_post_interview_test_rpart_pred_node = treatment_post_interview_test_rpart_pred_node, 
                                        treatment_post_interview_test_outcome_variable = treatment_post_interview_test_outcome_variable)

treatment_post_interview_test_rpart_output <- treatment_post_interview_test_rpart_output %>% 
        rename(terminal_node = treatment_post_interview_test_rpart_pred_node, 
               actual_outcome = treatment_post_interview_test_outcome_variable) %>% 
        group_by(terminal_node) %>%
        summarize(obs_in_node = n(), class_2_obs_in_node = sum(actual_outcome == 1), prob_class_2 = class_2_obs_in_node / obs_in_node) %>%
        mutate(pct_obs_in_node = obs_in_node / sum(obs_in_node), terminal_node = as.character(terminal_node)) %>% arrange(desc(prob_class_2))

treatment_post_interview_test_rpart_output 


###################################################################


# add node_paths to rpart_output
treatment_post_interview_test_rpart_output <- left_join(treatment_post_interview_test_rpart_output, 
                                                                 treatment_post_interview_train_rpart_caret_node_paths,
                                                                 by = c("terminal_node" = "terminal_node"))

treatment_post_interview_test_rpart_output %>% data.frame()
       

####################################################################


# get node stats for test data
source("get_node_stats.R")

treatment_post_interview_test_node_stats <- get_node_stats(data_updated_w_terminal_nodes_and_weights = treatment_post_interview_test_updated,
                                                           tree_output_terminal_nodes = treatment_post_interview_test_rpart_output,
                                                           outcome_variable = outcome_variable, 
                                                          train_model_node_order = train_model_node_order)
treatment_post_interview_test_node_stats %>% data.frame()


####################################################################


# get roc curve for test data

# get predictions from train model on testing data
post_interview_test_rpart_pred_prob <- predict(treatment_post_interview_train_rpart_caret$finalModel, 
                                               newdata = treatment_post_interview_test, type = "prob")
post_interview_test_rpart_pred_prob

treatment_post_interview_test_updated <- treatment_post_interview_test_updated %>% 
        mutate(pred_prob = post_interview_test_rpart_pred_prob[ , 1])

# add p_weight to treatment_post_interview_test_updated
# treatment_post_interview_test_updated <- treatment_post_interview_test_node_stats %>% 
#         mutate(terminal_node = as.integer(terminal_node)) %>%
#         select(terminal_node, p_weighted) %>%
#         left_join(treatment_post_interview_test_updated, ., by = c("terminal_node" = "terminal_node"))
# 
# treatment_post_interview_test_updated %>% distinct(terminal_node, p_weighted) %>% arrange(desc(p_weighted))
# treatment_post_interview_test_node_stats %>% select(terminal_node, p_weighted) %>% arrange(desc(p_weighted))

# get outcome variable for use as label in prediction function
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_test_outcome_variable <- treatment_post_interview_test_updated %>% pull(!!outcome_variable_sym)

# get outcome variable dummy for use as label in geom_roc
outcome_variable_dummy <- str_c(outcome_variable, "_dummy", sep = "")
outcome_variable_dummy_sym <- sym(outcome_variable_dummy)
treatment_post_interview_test_updated <- treatment_post_interview_test_updated %>%
        mutate(!!outcome_variable_dummy_sym := case_when(!!outcome_variable_sym == outcome_label_positive ~ 1,
                                                         !!outcome_variable_sym == outcome_label_negative ~ 0))
treatment_post_interview_test_outcome_variable_dummy <- treatment_post_interview_test_updated %>% pull(!!outcome_variable_dummy_sym)

# create roc curve data
test_roc_curve_data <- data.frame(pred_prob = treatment_post_interview_test_updated$pred_prob,
                                   outcome_variable = treatment_post_interview_test_outcome_variable,
                                   outcome_variable_dummy = treatment_post_interview_test_outcome_variable_dummy)
test_roc_curve_data
glimpse(test_roc_curve_data)

# create roc curve
test_roc_curve <- ggplot(test_roc_curve_data, aes(d = outcome_variable_dummy, m = pred_prob)) + geom_roc() +
        ggtitle(str_c("ROC curve for testing dataset - ", outcome_variable))
test_roc_curve

# get auc
# test_sorted_auc <- roc.curve(response = test_roc_curve_data$outcome_variable, predicted = test_roc_curve_data$p_weighted, plotit = FALSE)
# test_sorted_auc
# str(test_sorted_auc)
# test_sorted_auc$auc

# get performance using ROCR

treatment_post_interview_test_rpart_caret_prediction_obj <- prediction(predictions = test_roc_curve_data$pred_prob, 
                                                                        labels = test_roc_curve_data$outcome_variable, 
                                                                        label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_test_rpart_caret_performance_obj <- performance(prediction.obj = treatment_post_interview_test_rpart_caret_prediction_obj, 
                                                                          measure = "tpr", 
                                                                          x.measure = "fpr")

# print roc curve
test_roc_curve_ROCR_filename <- str_c("output/post_interview/test/test_roc_curve_ROCR_", outcome_variable, " - post_interview", ".pdf")
test_roc_curve_ROCR_filename

pdf(test_roc_curve_ROCR_filename, width = 10, height = 10)
plot(treatment_post_interview_test_rpart_caret_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)
dev.off()


# get AUC
treatment_post_interview_test_rpart_caret_auc <- performance(prediction.obj = treatment_post_interview_test_rpart_caret_prediction_obj, 
                                                              measure = "auc")
treatment_post_interview_test_rpart_caret_auc@y.values
test_auc <- data.frame(auc = treatment_post_interview_test_rpart_caret_auc@y.values[[1]])


# confusion matrix
treatment_post_interview_test_rpart_caret_pred_class <- predict(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                                 newdata = treatment_post_interview_test, type = "class")
confusion_matrix <- confusionMatrix(data = treatment_post_interview_test_rpart_caret_pred_class, reference = test_roc_curve_data$outcome_variable, 
                positive = outcome_label_positive)
confusion_matrix

# confusion_matrix performance metrics
test_accuracy <- confusion_matrix$overall["Accuracy"]
test_kappa <- confusion_matrix$overall["Kappa"]
test_specificity <- confusion_matrix$byClass["Specificity"]
test_sensitivity <- confusion_matrix$byClass["Sensitivity"]
test_balanced_accuracy <- confusion_matrix$byClass["Balanced Accuracy"]
test_pos_pred_value <- confusion_matrix$byClass["Pos Pred Value"]
test_neg_pred_value <- confusion_matrix$byClass["Neg Pred Value"]

# verify confusion matrix table
treatment_post_interview_test_rpart_caret_pred_class %>% tabyl()
treatment_post_interview_test_updated %>% tabyl(!!outcome_variable_sym)
test_roc_curve_data %>% tabyl(outcome_variable)

# save chart
# sorted_test_roc_curve_filename <- str_c("output/post_interview/test/test_roc_curve_sorted_", outcome_variable, " - post_interview", ".pdf")
# sorted_test_roc_curve_filename
# 
# ggsave(filename = sorted_test_roc_curve_filename, plot = sorted_test_roc_curve, width = 17, height = 10, 
#        units = "in", dpi = 900)


########################################################################


# create roc curve manually
treatment_post_interview_test_node_stats_roc <- rbind(treatment_post_interview_test_node_stats[1, ], 
                                                      treatment_post_interview_test_node_stats)
treatment_post_interview_test_node_stats_roc$cum_false_positive_rate_weighted[1] <- 0
treatment_post_interview_test_node_stats_roc$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_test_node_stats_roc$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
test_roc_curve <- ggplot(treatment_post_interview_test_node_stats_roc, aes(x = cum_false_positive_rate_weighted, 
                                                                           y = cum_true_positive_rate_weighted)) +
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) + 
        ggtitle(str_c("ROC curve for testing dataset - manual - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed")
test_roc_curve

# save chart
test_roc_curve_filename <- str_c("output/post_interview/test/test_roc_curve_manual_", outcome_variable, " - post_interview", ".pdf")
test_roc_curve_filename

ggsave(filename = test_roc_curve_filename, plot = test_roc_curve, width = 10, height = 10, 
       units = "in", dpi = 900)


#######################################################################


# get lift chart for testing data

# test_roc_curve_data_factor <- data.frame(p_weighted = treatment_post_interview_test_updated$p_weighted,
#                                    outcome_variable = factor(treatment_post_interview_test_outcome_variable))
# str(test_roc_curve_data_factor$outcome_variable)
# head(test_roc_curve_data_factor)

# need to relevel the factor so that first level is the positive class
# test_roc_curve_data_factor$outcome_variable <- fct_relevel(test_roc_curve_data_factor$outcome_variable, "1", "0")
# str(test_roc_curve_data_factor$outcome_variable)

test_lift_obj <- caret::lift(outcome_variable ~ pred_prob, data = test_roc_curve_data)
attributes(test_lift_obj)
test_lift_obj$data
test_lift_obj$pct

# create baseline
line1 <- data.frame(x1 = 0, x2 = 100, y1 = 0, y2 = 100)
line2 <- data.frame(x1 = 0, x2 = test_lift_obj$pct, y1 = 0, y2 = 100)
line3 <- data.frame(x1 = test_lift_obj$pct, x2 = 100, y1 = 100, y2 = 100)

# plot cumulative gains chart
test_gains_chart <- ggplot(test_lift_obj$data) + geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_line(aes(x = CumTestedPct, y = CumEventPct, col = liftModelVar)) + ggtitle(str_c("Gains chart for testing dataset - ", outcome_variable)) +
        ylab("True Positive Rate") + xlab("Rate of Positive Predictions")
test_gains_chart

# save chart
test_gains_chart_filename <- str_c("output/post_interview/test/test_gains_chart_", outcome_variable, " - post_interview", ".pdf")
test_gains_chart_filename

ggsave(filename = test_gains_chart_filename, plot = test_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


#################################################


# create cumulative gains chart manually
treatment_post_interview_test_node_stats_gains <- rbind(treatment_post_interview_test_node_stats[1, ], treatment_post_interview_test_node_stats)
treatment_post_interview_test_node_stats_gains$cum_pct_weighted_obs[1] <- 0
treatment_post_interview_test_node_stats_gains$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_test_node_stats_gains$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
line2 <- data.frame(x1 = 0, x2 = treatment_post_interview_test_node_stats_gains$p_weighted_overall[1], y1 = 0, y2 = 1)
line3 <- data.frame(x1 = treatment_post_interview_test_node_stats_gains$p_weighted_overall[1], x2 = 1, y1 = 1, y2 = 1)

test_gains_chart <- ggplot(treatment_post_interview_test_node_stats_gains, aes(x = cum_pct_weighted_obs, y = cum_true_positive_rate_weighted)) + 
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) +
        ggtitle(str_c("Manual gains chart for testing dataset - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed") + 
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted") +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted")
test_gains_chart

# save chart
test_gains_chart_filename <- str_c("output/post_interview/test/test_gains_chart_manual_", outcome_variable, " - post_interview", ".pdf")
test_gains_chart_filename

ggsave(filename = test_gains_chart_filename, plot = test_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


################################################################


# get train p_weighted baseline to use in validation of above/below avg nodes
# treatment_post_interview_train %>% tabyl(!!outcome_variable_sym)
# train_p_weighted_baseline <- treatment_post_interview_train_node_stats %>% distinct(p_weighted_overall) %>% pull(p_weighted_overall)
# train_p_weighted_baseline


#############################################################


# select terminal_nodes which are signifantly above average or below average outcomes in both training and testing sets
validated_significant_test_terminal_nodes <- treatment_post_interview_test_node_stats %>% 
                filter(p_diff_overall_significance == 1,
                               terminal_node %in% treatment_post_interview_train_node_stats_significant_above$terminal_node &
                               p_weighted > p_weighted_overall |
                               terminal_node %in% treatment_post_interview_train_node_stats_significant_below$terminal_node &
                               p_weighted < p_weighted_overall) %>% 
        pull(terminal_node)
validated_significant_test_terminal_nodes

validated_above_below_avg_test_terminal_nodes <- treatment_post_interview_test_node_stats %>% 
        filter(p_weighted > p_weighted_overall &
                terminal_node %in% treatment_post_interview_train_node_stats_above_avg$terminal_node |
                        p_weighted < p_weighted_overall &
                        terminal_node %in% treatment_post_interview_train_node_stats_below_avg$terminal_node) %>% pull(terminal_node)
validated_above_below_avg_test_terminal_nodes

# validated significant terminal nodes in training model
validated_significant_train_terminal_node_output <- treatment_post_interview_train_node_stats %>% 
        filter(terminal_node %in% validated_significant_test_terminal_nodes)
validated_significant_train_terminal_node_output %>% data.frame(.)

# validated above_below_avg terminal nodes in training model
validated_above_below_avg_train_terminal_node_output <- treatment_post_interview_train_node_stats %>% 
        filter(terminal_node %in% validated_above_below_avg_test_terminal_nodes)
validated_above_below_avg_train_terminal_node_output %>% data.frame(.)

# validated significant terminal nodes in testing model
validated_significant_test_terminal_node_output <- treatment_post_interview_test_node_stats %>% 
        filter(terminal_node %in% validated_significant_test_terminal_nodes)
validated_significant_test_terminal_node_output %>% data.frame(.)

# validated significant terminal nodes in testing model
validated_above_below_avg_test_terminal_node_output <- treatment_post_interview_test_node_stats %>% 
        filter(terminal_node %in% validated_above_below_avg_test_terminal_nodes)
validated_above_below_avg_test_terminal_node_output %>% data.frame(.)



#####################################################
##########################################################
###########################################################


# baseline accuracy
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_full %>% group_by(!!outcome_variable_sym) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))
dim(treatment_post_interview_full)

# get predicted terminal nodes for full set - node # is same as "terminal_node" variable created from $frame output rownames
source("predict_nodes.R")

treatment_post_interview_full_rpart_pred_node <- predict_nodes(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                               newdata = treatment_post_interview_full)
treatment_post_interview_full_rpart_pred_node

# create updated full data including the terminal_node and weights
treatment_post_interview_full_updated <- treatment_post_interview_full %>% 
        mutate(terminal_node = treatment_post_interview_full_rpart_pred_node,
               weight = case_when(BEN_COUNTRY_OF_BIRTHMEXIC == 1 ~ BEN_COUNTRY_OF_BIRTHMEXIC * 1.24, 
                                  BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                                  BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1))
unique(treatment_post_interview_full_updated$weight)
unique(treatment_post_interview_full_updated$terminal_node)

# create output for full set fit
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_full_outcome_variable <- treatment_post_interview_full %>% pull(!!outcome_variable_sym)
treatment_post_interview_full_rpart_output <- data.frame(treatment_post_interview_full_rpart_pred_node = treatment_post_interview_full_rpart_pred_node, 
                                                         treatment_post_interview_full_outcome_variable = treatment_post_interview_full_outcome_variable)

treatment_post_interview_full_rpart_output <- treatment_post_interview_full_rpart_output %>% 
        rename(terminal_node = treatment_post_interview_full_rpart_pred_node, 
               actual_outcome = treatment_post_interview_full_outcome_variable) %>% 
        group_by(terminal_node) %>%
        summarize(obs_in_node = n(), class_2_obs_in_node = sum(actual_outcome == 1), prob_class_2 = class_2_obs_in_node / obs_in_node) %>%
        mutate(pct_obs_in_node = obs_in_node / sum(obs_in_node), terminal_node = as.character(terminal_node)) %>% arrange(desc(prob_class_2))

treatment_post_interview_full_rpart_output 


###################################################################


# add node_paths to rpart_output
treatment_post_interview_full_rpart_output <- left_join(treatment_post_interview_full_rpart_output, 
                                                        treatment_post_interview_train_rpart_caret_node_paths,
                                                        by = c("terminal_node" = "terminal_node"))

treatment_post_interview_full_rpart_output %>% data.frame()


####################################################################


# get node stats for full data
source("get_node_stats.R")

treatment_post_interview_full_node_stats <- get_node_stats(data_updated_w_terminal_nodes_and_weights = treatment_post_interview_full_updated,
                                                           tree_output_terminal_nodes = treatment_post_interview_full_rpart_output,
                                                           outcome_variable = outcome_variable, 
                                                           train_model_node_order = train_model_node_order)
treatment_post_interview_full_node_stats %>% data.frame()


####################################################################


# get roc curve for full data

# get pred_prob
post_interview_full_rpart_pred_prob <- predict(treatment_post_interview_train_rpart_caret$finalModel, 
                                               newdata = treatment_post_interview_full, type = "prob")
post_interview_full_rpart_pred_prob

treatment_post_interview_full_updated <- treatment_post_interview_full_updated %>% 
        mutate(pred_prob = post_interview_full_rpart_pred_prob[ , 1])


# add p_weight to treatment_post_interview_full_updated
# treatment_post_interview_full_updated <- treatment_post_interview_full_node_stats %>% 
#         mutate(terminal_node = as.integer(terminal_node)) %>%
#         select(terminal_node, p_weighted) %>%
#         left_join(treatment_post_interview_full_updated, ., by = c("terminal_node" = "terminal_node"))
# 
# treatment_post_interview_full_updated %>% distinct(terminal_node, p_weighted) %>% arrange(desc(p_weighted))
# treatment_post_interview_full_node_stats %>% select(terminal_node, p_weighted) %>% arrange(desc(p_weighted))

# get outcome variable for use as label in prediction function
outcome_variable_sym <- sym(outcome_variable)
treatment_post_interview_full_outcome_variable <- treatment_post_interview_full_updated %>% pull(!!outcome_variable_sym)

# get outcome variable dummy for use as label in geom_roc
outcome_variable_dummy <- str_c(outcome_variable, "_dummy", sep = "")
outcome_variable_dummy_sym <- sym(outcome_variable_dummy)
treatment_post_interview_full_updated <- treatment_post_interview_full_updated %>%
        mutate(!!outcome_variable_dummy_sym := case_when(!!outcome_variable_sym == outcome_label_positive ~ 1,
                                                         !!outcome_variable_sym == outcome_label_negative ~ 0))
treatment_post_interview_full_outcome_variable_dummy <- treatment_post_interview_full_updated %>% pull(!!outcome_variable_dummy_sym)

# create roc curve data
full_roc_curve_data <- data.frame(pred_prob = treatment_post_interview_full_updated$pred_prob,
                                  outcome_variable = treatment_post_interview_full_outcome_variable,
                                  outcome_variable_dummy = treatment_post_interview_full_outcome_variable_dummy)
full_roc_curve_data
glimpse(full_roc_curve_data)

# create roc curve
full_roc_curve <- ggplot(full_roc_curve_data, aes(d = outcome_variable_dummy, m = pred_prob)) + geom_roc() +
        ggtitle(str_c("ROC curve for fulling dataset - ", outcome_variable))
full_roc_curve

# get auc
# full_sorted_auc <- roc.curve(response = full_roc_curve_data$outcome_variable, predicted = full_roc_curve_data$p_weighted, plotit = FALSE)
# full_sorted_auc
# str(full_sorted_auc)
# full_sorted_auc$auc

# get performance using ROCR

treatment_post_interview_full_rpart_caret_prediction_obj <- prediction(predictions = full_roc_curve_data$pred_prob, 
                                                                       labels = full_roc_curve_data$outcome_variable, 
                                                                       label.ordering = c(outcome_label_negative, outcome_label_positive))
treatment_post_interview_full_rpart_caret_performance_obj <- performance(prediction.obj = treatment_post_interview_full_rpart_caret_prediction_obj, 
                                                                         measure = "tpr", 
                                                                         x.measure = "fpr")

# print roc curve
full_roc_curve_ROCR_filename <- str_c("output/post_interview/full/full_roc_curve_ROCR_", outcome_variable, " - post_interview", ".pdf")
full_roc_curve_ROCR_filename

pdf(full_roc_curve_ROCR_filename, width = 10, height = 10)
plot(treatment_post_interview_full_rpart_caret_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)
dev.off()


# get AUC
treatment_post_interview_full_rpart_caret_auc <- performance(prediction.obj = treatment_post_interview_full_rpart_caret_prediction_obj, 
                                                             measure = "auc")
treatment_post_interview_full_rpart_caret_auc@y.values
full_auc <- data.frame(auc = treatment_post_interview_full_rpart_caret_auc@y.values[[1]])

# confusion matrix
treatment_post_interview_full_rpart_caret_pred_class <- predict(object = treatment_post_interview_train_rpart_caret$finalModel, 
                                                                newdata = treatment_post_interview_full, type = "class")
confusion_matrix <- confusionMatrix(data = treatment_post_interview_full_rpart_caret_pred_class, reference = full_roc_curve_data$outcome_variable, 
                positive = outcome_label_positive)
confusion_matrix

# confusion_matrix performance metrics
full_accuracy <- confusion_matrix$overall["Accuracy"]
full_kappa <- confusion_matrix$overall["Kappa"]
full_specificity <- confusion_matrix$byClass["Specificity"]
full_sensitivity <- confusion_matrix$byClass["Sensitivity"]
full_balanced_accuracy <- confusion_matrix$byClass["Balanced Accuracy"]
full_pos_pred_value <- confusion_matrix$byClass["Pos Pred Value"]
full_neg_pred_value <- confusion_matrix$byClass["Neg Pred Value"]


# verify confusion matrix table
treatment_post_interview_full_rpart_caret_pred_class %>% tabyl()
treatment_post_interview_full_updated %>% tabyl(!!outcome_variable_sym)
full_roc_curve_data %>% tabyl(outcome_variable)

# save chart
# sorted_full_roc_curve_filename <- str_c("output/post_interview/full/full_roc_curve_sorted_", outcome_variable, " - post_interview", ".pdf")
# sorted_full_roc_curve_filename
# 
# ggsave(filename = sorted_full_roc_curve_filename, plot = sorted_full_roc_curve, width = 17, height = 10, 
#        units = "in", dpi = 900)


###########################################################


# create roc curve manually
# need to order nodes by node_order_full, since that node order is what we would have used to assign obs to having positive outcomes
# so when evaluating what tpr we wouldve gotten if we classified node according to full model, we need to incrementally assign
# nodes as positive according to the fulling model ordering of high prob nodes
# it would be unfair to evalute the full set by seeing what tpr results from incrementally assigning the nodes as positive according to the
# full set node probabilities/order
# node_order_full <- treatment_post_interview_full_node_stats %>% pull(terminal_node)
# node_order_full
# 
# x <- treatment_post_interview_full_node_stats[order(match(treatment_post_interview_full_node_stats$terminal_node, node_order_full)), ]
# treatment_post_interview_full_node_stats_reordered <- x
# 
# treatment_post_interview_full_node_stats_reordered %>% select(terminal_node, p_weighted)
# treatment_post_interview_full_node_stats %>% select(terminal_node, p_weighted)




# # plot manual roc curve
treatment_post_interview_full_node_stats_roc <- rbind(treatment_post_interview_full_node_stats[1, ], 
                                                      treatment_post_interview_full_node_stats)
treatment_post_interview_full_node_stats_roc$cum_false_positive_rate_weighted[1] <- 0
treatment_post_interview_full_node_stats_roc$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_full_node_stats_roc$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
full_roc_curve <- ggplot(treatment_post_interview_full_node_stats_roc, aes(x = cum_false_positive_rate_weighted, y = cum_true_positive_rate_weighted)) +
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) + 
        ggtitle(str_c("ROC curve for fulling dataset - manual - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed")
full_roc_curve

# save chart
full_roc_curve_filename <- str_c("output/post_interview/full/full_roc_curve_manual_", outcome_variable, " - post_interview", ".pdf")
full_roc_curve_filename

ggsave(filename = full_roc_curve_filename, plot = full_roc_curve, width = 10, height = 10, 
       units = "in", dpi = 900)

# get roc auc
# treatment_post_interview_full_rpart_caret_roc_pred <- prediction(predictions = treatment_post_interview_full_updated$p_weighted,
#                                                                      labels = treatment_post_interview_full_outcome_variable)
# 
# treatment_post_interview_full_rpart_caret_roc_auc <- performance(treatment_post_interview_full_rpart_caret_roc_pred, measure = "auc")
# treatment_post_interview_full_rpart_caret_roc_auc@y.values


#######################################################################


# get lift chart for full data
# full_roc_curve_data_factor <- data.frame(p_weighted = treatment_post_interview_full_updated$p_weighted,
#                                    outcome_variable = factor(treatment_post_interview_full_outcome_variable))
# str(full_roc_curve_data_factor$outcome_variable)
# head(full_roc_curve_data_factor)

# need to relevel the factor so that first level is the positive class
# full_roc_curve_data_factor$outcome_variable <- fct_relevel(full_roc_curve_data_factor$outcome_variable, "1", "0")
# str(full_roc_curve_data_factor$outcome_variable)

full_lift_obj <- caret::lift(outcome_variable ~ pred_prob, data = full_roc_curve_data)
attributes(full_lift_obj)
full_lift_obj$data
full_lift_obj$pct

# create baseline
line1 <- data.frame(x1 = 0, x2 = 100, y1 = 0, y2 = 100)
line2 <- data.frame(x1 = 0, x2 = full_lift_obj$pct, y1 = 0, y2 = 100)
line3 <- data.frame(x1 = full_lift_obj$pct, x2 = 100, y1 = 100, y2 = 100)

# plot cumulative gains chart
full_gains_chart <- ggplot(full_lift_obj$data) + geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .2, lty = 2) +
        geom_line(aes(x = CumTestedPct, y = CumEventPct, col = liftModelVar)) + ggtitle(str_c("Gains chart for full dataset - ", outcome_variable)) +
        ylab("True Positive Rate") + xlab("Rate of Positive Predictions")
full_gains_chart

# save chart
full_gains_chart_filename <- str_c("output/post_interview/full/full_gains_chart_", outcome_variable, " - post_interview", ".pdf")
full_gains_chart_filename

ggsave(filename = full_gains_chart_filename, plot = full_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


#################################################


# create cumulative gains chart manually
treatment_post_interview_full_node_stats_gains <- rbind(treatment_post_interview_full_node_stats[1, ], treatment_post_interview_full_node_stats)
treatment_post_interview_full_node_stats_gains$cum_pct_weighted_obs[1] <- 0
treatment_post_interview_full_node_stats_gains$cum_true_positive_rate_weighted[1] <- 0
treatment_post_interview_full_node_stats_gains$terminal_node[1] <- 0

line1 <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)
line2 <- data.frame(x1 = 0, x2 = treatment_post_interview_full_node_stats_gains$p_weighted_overall[1], y1 = 0, y2 = 1)
line3 <- data.frame(x1 = treatment_post_interview_full_node_stats_gains$p_weighted_overall[1], x2 = 1, y1 = 1, y2 = 1)

full_gains_chart <- ggplot(treatment_post_interview_full_node_stats_gains, aes(x = cum_pct_weighted_obs, y = cum_true_positive_rate_weighted)) + 
        geom_line() + geom_point() + geom_text(aes(label = terminal_node), position = position_nudge(x = .03, y = -.03)) +
        ggtitle(str_c("Gains chart for full dataset - manual - ", outcome_variable, " - post_interview")) + 
        geom_segment(data = line1, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dashed") + 
        geom_segment(data = line2, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted") +
        geom_segment(data = line3, aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 1, lty = "dotted")
full_gains_chart

# save chart
full_gains_chart_filename <- str_c("output/post_interview/full/full_gains_chart_manual_", outcome_variable, " - post_interview", ".pdf")
full_gains_chart_filename

ggsave(filename = full_gains_chart_filename, plot = full_gains_chart, width = 10, height = 10, 
       units = "in", dpi = 900)


#########################################################


# explore above average outcome nodes for full dataset

# get full p_weighted baseline
full_p_weighted_baseline <- treatment_post_interview_full_node_stats %>% distinct(p_weighted_overall) %>% pull(p_weighted_overall)
full_p_weighted_baseline


#############################################################


# create validated significant full terminal node output
validated_significant_full_terminal_node_output <- treatment_post_interview_full_node_stats %>% 
        filter(terminal_node %in% validated_significant_test_terminal_nodes)

# create validated above_below_avg full terminal node output
validated_above_below_avg_full_terminal_node_output <- treatment_post_interview_full_node_stats %>% 
        filter(terminal_node %in% validated_above_below_avg_test_terminal_nodes)

# inspect validated_significant_full_terminal_node_output
validated_significant_full_terminal_node_output %>% data.frame(.)

# compare with validated_significant_train_terminal_node_output
validated_significant_train_terminal_node_output %>% data.frame(.)

# compare with validated_significant_test_terminal_node_output
validated_significant_test_terminal_node_output %>% data.frame(.)


#############################################################


# prepare output tables 

# training data output table
treatment_post_interview_train_table <- treatment_post_interview_train_node_stats %>% 
        mutate(validated_significant_test_terminal_node = case_when(terminal_node %in% validated_significant_test_terminal_nodes ~ 1, 
                                                                    TRUE ~ 0),
               validated_above_below_avg_test_terminal_node = case_when(terminal_node %in% validated_above_below_avg_test_terminal_nodes ~ 1, 
                                                                    TRUE ~ 0),            
               output_type = "treatment_post_interview_train", outcome = !!outcome_variable,
               auc = train_auc$auc,
               accuracy = train_accuracy, balanced_accuracy = train_balanced_accuracy, kappa = train_kappa, sensitivity = train_sensitivity, 
               specificity = train_specificity, pos_pred_value = train_pos_pred_value, neg_pred_value = train_neg_pred_value,
               cv_train_auc = cv_train_auc, cv_train_sensitivity = cv_train_sensitivity, cv_train_specificity = cv_train_specificity) %>%
        # rename(unsorted_lift = lift, unsorted_lift_weighted = lift_weighted) %>%
        select(output_type, outcome, terminal_node, node_path, obs_in_node, outcome_positive_sum,
               pct_obs_in_node, cum_pct_obs, p_weighted, lower_limit_p_weighted, upper_limit_p_weighted, 
               validated_above_below_avg_test_terminal_node,
               validated_significant_test_terminal_node, p_weighted_overall, p_diff_overall, lower_limit_p_diff_overall,
                                                    upper_limit_p_diff_overall, p_diff_overall_significance,
               coeff_of_variation, unreliable_p_weighted, suppressed_p_weighted,
               cv_train_auc, cv_train_sensitivity, cv_train_specificity,
                                                    cum_true_positive_rate, cum_false_positive_rate, auc, lift,
               lift_weighted, accuracy, balanced_accuracy, kappa, sensitivity, specificity, pos_pred_value, neg_pred_value) %>% data.frame(.)
treatment_post_interview_train_table

# save training data output table
file_name_train <- str_c("output/post_interview/train/treatment_post_interview_train_table_", outcome_variable, ".csv")
file_name_train
write_csv(treatment_post_interview_train_table, path = file_name_train)


#########################################################


# test data output table
treatment_post_interview_test_table <- treatment_post_interview_test_node_stats %>% 
        mutate(validated_significant_test_terminal_node = case_when(terminal_node %in% validated_significant_test_terminal_nodes ~ 1, 
                                                                    TRUE ~ 0),
               validated_above_below_avg_test_terminal_node = case_when(terminal_node %in% validated_above_below_avg_test_terminal_nodes ~ 1, 
                                                                        TRUE ~ 0),   
               output_type = "treatment_post_interview_test", outcome = !!outcome_variable,
               auc = test_auc$auc,
               accuracy = test_accuracy, balanced_accuracy = test_balanced_accuracy, kappa = test_kappa, sensitivity = test_sensitivity, 
               specificity = test_specificity, pos_pred_value = test_pos_pred_value, neg_pred_value = test_neg_pred_value,
               cv_train_auc = cv_train_auc, cv_train_sensitivity = cv_train_sensitivity, cv_train_specificity = cv_train_specificity) %>%
        # rename(unsorted_lift = lift, unsorted_lift_weighted = lift_weighted) %>%
        select(output_type, outcome, terminal_node, node_path, obs_in_node, outcome_positive_sum,
               pct_obs_in_node, cum_pct_obs, p_weighted, lower_limit_p_weighted, upper_limit_p_weighted, 
               validated_above_below_avg_test_terminal_node,
               validated_significant_test_terminal_node, p_weighted_overall, p_diff_overall, lower_limit_p_diff_overall,
               upper_limit_p_diff_overall, p_diff_overall_significance, 
               coeff_of_variation, unreliable_p_weighted, suppressed_p_weighted,
               cv_train_auc, cv_train_sensitivity, cv_train_specificity, 
               cum_true_positive_rate, cum_false_positive_rate, auc, lift,
               lift_weighted, accuracy, balanced_accuracy, kappa, sensitivity, specificity, pos_pred_value, neg_pred_value) %>% data.frame(.)
treatment_post_interview_test_table

# save test data output table
file_name_test <- str_c("output/post_interview/test/treatment_post_interview_test_table_", outcome_variable, ".csv")
file_name_test
write_csv(treatment_post_interview_test_table, path = file_name_test)


######################################################


# full data output table
treatment_post_interview_full_table <- treatment_post_interview_full_node_stats %>% 
        mutate(validated_significant_test_terminal_node = case_when(terminal_node %in% validated_significant_test_terminal_nodes ~ 1, 
                                                                    TRUE ~ 0),
               validated_above_below_avg_test_terminal_node = case_when(terminal_node %in% validated_above_below_avg_test_terminal_nodes ~ 1, 
                                                                        TRUE ~ 0),   
               output_type = "treatment_post_interview_full", outcome = !!outcome_variable,
               auc = full_auc$auc,
               accuracy = full_accuracy, balanced_accuracy = full_balanced_accuracy, kappa = full_kappa, sensitivity = full_sensitivity, 
               specificity = full_specificity, pos_pred_value = full_pos_pred_value, neg_pred_value = full_neg_pred_value,
               cv_train_auc = cv_train_auc, cv_train_sensitivity = cv_train_sensitivity, cv_train_specificity = cv_train_specificity) %>%
        # rename(unsorted_lift = lift, unsorted_lift_weighted = lift_weighted) %>%
        select(output_type, outcome, terminal_node, node_path, obs_in_node, outcome_positive_sum,
               pct_obs_in_node, cum_pct_obs, p_weighted, lower_limit_p_weighted, upper_limit_p_weighted, 
               validated_above_below_avg_test_terminal_node,
               validated_significant_test_terminal_node, p_weighted_overall, p_diff_overall, lower_limit_p_diff_overall,
               upper_limit_p_diff_overall, p_diff_overall_significance, 
               coeff_of_variation, unreliable_p_weighted, suppressed_p_weighted,
               cv_train_auc, cv_train_sensitivity, cv_train_specificity, 
               cum_true_positive_rate, cum_false_positive_rate, auc, lift,
               lift_weighted, accuracy, balanced_accuracy, kappa, sensitivity, specificity, pos_pred_value, neg_pred_value) %>% data.frame(.)
treatment_post_interview_full_table

# save full data output table
file_name_full <- str_c("output/post_interview/full/treatment_post_interview_full_table_", outcome_variable, ".csv")
file_name_full
write_csv(treatment_post_interview_full_table, path = file_name_full)


############################################################
############################################################
###############################################################


# save predicted nodes for treatment group
dim(treatment_post_interview_full_updated)
dim(treatment_post_interview_full_receipt_numbers)

# confirm that treatment_post_interview_updated is still sorted same as treatment_post_interview when we dropped the receipt_numbers
treatment_post_interview_full_updated %>% select(time_since_present_marriage, time_since_cohabitation)
treatment_post_interview_full %>% select(time_since_present_marriage, time_since_cohabitation)
treatment_post_interview_dummies %>% select(time_since_present_marriage, time_since_cohabitation)

# add receipt_numbers back onto treatment_post_interview_full_updated
treatment_post_interview_full_updated <- treatment_post_interview_full_updated %>% mutate(receipt_number = treatment_post_interview_full_receipt_numbers$receipt_number)
treatment_post_interview_full_updated %>% select(receipt_number, terminal_node)

# merge node_paths to corresponding terminal_node
treatment_post_interview_train_rpart_caret_node_paths %>% mutate(terminal_node = as.integer(terminal_node)) %>% arrange(desc(terminal_node))
treatment_post_interview_full_updated <- treatment_post_interview_train_rpart_caret_node_paths %>% 
        mutate(terminal_node = as.integer(terminal_node)) %>%
        left_join(treatment_post_interview_full_updated, ., by = "terminal_node")

treatment_post_interview_full_updated %>% distinct(terminal_node, node_path) %>% arrange(desc(terminal_node))

# return outcome_variable to be a dummy instead of text, so that it matches control_pre_interview_dummies
treatment_post_interview_full_updated %>% count(!!outcome_variable_sym)
treatment_post_interview_full_updated %>% count(!!outcome_variable_dummy_sym)

treatment_post_interview_full_updated <- treatment_post_interview_full_updated %>% mutate(!!outcome_variable_sym := !!outcome_variable_dummy_sym)

treatment_post_interview_full_updated %>% count(!!outcome_variable_sym)

# write treatment_post_interview_full_updated to file
file_name <- str_c("output/post_interview/full/treatment_post_interview_full_w_terminal_nodes_", outcome_variable, ".csv")
file_name
write_csv(treatment_post_interview_full_updated, path = file_name)




















