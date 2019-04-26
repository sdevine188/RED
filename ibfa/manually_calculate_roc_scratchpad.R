
# inspect output of rose functions
glimpse(treatment_post_interview_train_both)
treatment_post_interview_train_both %>% select(!!outcome_variable_sym) %>% str()
treatment_post_interview_train_both %>% select(!!outcome_variable_sym) %>% head()

str(test_outcome_variable)


treatment_post_interview_train_rpart

# create original roc
treatment_post_interview_train_rpart_caret_pred_prob <- predict(object = treatment_post_interview_train_rpart_caret, newdata = treatment_post_interview_test,
                                                          type = "prob")
# since ROCR can't handle tidy evaluation, need to create separate vector with test set outcome
test_outcome_variable <- treatment_post_interview_test %>% pull(!!outcome_variable_sym)
treatment_post_interview_train_rpart_prediction_obj <- prediction(predictions = treatment_post_interview_train_rpart_pred_prob[ , 2], 
                                                                  labels = test_outcome_variable)
treatment_post_interview_train_rpart_performance_obj <- performance(prediction.obj = treatment_post_interview_train_rpart_prediction_obj, measure = "tpr", x.measure = "fpr")
plot(treatment_post_interview_train_rpart_performance_obj, colorize = TRUE, print.cutoffs.at = c(.1, .5, .7, .8, .9))
abline(a=0, b=1)





# manually create roc stats
treatment_post_interview_train_rpart__caret_pred_prob2 <- as_tibble(treatment_post_interview_train_rpart_caret_pred_prob) %>% mutate(row_id = row_number())
glimpse(treatment_post_interview_train_rpart_pred_prob2)

glimpse(as_tibble(test_outcome_variable))

test_output <- treatment_post_interview_train_rpart_caret_pred_prob2 %>% bind_cols(., as_tibble(test_outcome_variable)) %>% rename(reference = value)
glimpse(test_output)

test_output %>% distinct(`Suf. Evid.`) %>% arrange(desc(`Suf. Evid.`))
test_output %>% group_by(`Suf. Evid.`) %>% count() %>% arrange(desc(`Suf. Evid.`))




test_output %>% group_by(reference) %>% count()
total_count_positives <- test_output %>% group_by(reference) %>% count() %>% filter(reference == "Suf. Evid.") %>% pull(n)
total_count_positives

total_count_negatives <- test_output %>% group_by(reference) %>% count() %>% filter(reference == "Not Suf. Evid.") %>% pull(n)
total_count_negatives

summary_test_output <- test_output %>% group_by(`Suf. Evid.`, reference) %>% count() %>% arrange(desc(`Suf. Evid.`)) %>% ungroup() %>% 
        mutate(suf_evid = ifelse(reference == "Suf. Evid.", 1, 0), cumsum_n = cumsum(n), cumsum_suf_evid = cumsum(suf_evid),
               count_true_positives = case_when(reference == "Suf. Evid." ~ n, TRUE ~ as_integer(0)),
               cumsum_count_true_positives = cumsum(count_true_positives), cum_true_positive_rate = cumsum_count_true_positives / total_count_positives,
               count_false_positives = case_when(reference == "Not Suf. Evid." ~ n, TRUE ~ as_integer(0)),
               cumsum_count_false_positives = cumsum(count_false_positives),
               cum_false_positive_rate = cumsum_count_false_positives / total_count_negatives) %>% data.frame()
summary_test_output

summary_test_output %>% ggplot(., aes(x = cum_false_positive_rate, y = cum_true_positive_rate)) + geom_line()



treatment_post_interview_train_rpart_prediction_obj
