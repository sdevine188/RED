library(tidyverse)
library(tidytext)
library(extrafont)
library(officer)
library(devEMF)
library(RColorBrewer)
library(scales)

# load sentimentr library manually, if needed
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sentimentr")
source("load_sentimentr_library.R")
setwd(current_wd)

# load convert_terms_to_tbl()
source("convert_terms_to_tbl.R")

options(scipen=999)

# setwd
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/RED/uscis_hq_survey")

# read in survey responses
data <- read_csv("survey_text_responses.csv")

# inspect
data
data %>% glimpse()
data %>% count(question)
# note that question 4 has only NA response, question 3 had a single NA response but several non-NA responses
data %>% filter(is.na(response)) %>% distinct(question_number) %>% left_join(., data, by = "question_number")
data %>% filter(question_number == 4)

# since question 4 has no responses, will drop it, to avoid biasing tf-idf scores which rely on number of questions
data <- data %>% filter(question_number != 4)
data %>% count(question_number)


########################################################################################################################
########################################################################################################################
########################################################################################################################


# analyze tf-idf

# get unigram_tf_idf

# get question_terms
question_terms <- data %>% unnest_tokens(output = term, input = response) %>%
        count(question_number, term) %>% arrange(desc(n)) %>% rename(term_count_per_question = n)
question_terms

# get number_of_questions_containing_each_term
number_of_questions_containing_each_term <- question_terms %>% count(question_number, term) %>%
        group_by(term) %>% count() %>% ungroup() %>% arrange(desc(n)) %>% 
        rename(number_of_questions_containing_term = n)
number_of_questions_containing_each_term

# total_term_per_question
total_terms_per_question <- question_terms %>% 
        group_by(question_number) %>% 
        summarize(total_terms_per_question = sum(term_count_per_question))
total_terms_per_question

# get total_number_questions
total_number_questions <- nrow(total_terms_per_question)
total_number_questions

# get total_words_per_question
unigram_tf_idf <- question_terms %>% left_join(., total_terms_per_question, by = "question_number") %>%
        left_join(., number_of_questions_containing_each_term, by = "term") %>%
        bind_tf_idf(term = term, document = question_number, n = term_count_per_question) %>%
        mutate(manual_tf = term_count_per_question / total_terms_per_question, 
               manual_idf = log(total_number_questions / number_of_questions_containing_term),
               manual_tf_idf = manual_tf * manual_idf) %>%
        arrange(desc(tf_idf)) %>%
        left_join(., data %>% distinct(question, question_number), by = "question_number") %>%
        filter(!str_detect(string = question, regex(pattern = term, ignore_case = TRUE)))

# inspect
unigram_tf_idf
unigram_tf_idf %>% print(n = 50)
unigram_tf_idf %>% filter(question_number == 10) %>% print(n = 50)


###########################################################################################################################


unigram_tf_idf_chart_data <- unigram_tf_idf %>% filter(question_number == 10) %>% arrange(desc(tf_idf)) %>% slice(1:15)
unigram_tf_idf_chart_data


######################


# inspect range of term_count_per_question for the top tf-idf terms to be plotted
# result: it looks like bins of 1, 2, 3, 4, and 5 or above should work well
unigram_tf_idf %>% group_by(question_number) %>% arrange(desc(tf_idf)) %>% slice(1:15) %>%
        ungroup() %>% count(term_count_per_question)

# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)

# add fill_color_bin and fill_color
unigram_tf_idf_chart_data <- unigram_tf_idf_chart_data %>% mutate(fill_color_bin = case_when(term_count_per_question == 1 ~ "1",
                                                     term_count_per_question == 2 ~ "2",
                                                     term_count_per_question == 3 ~ "3",
                                                     term_count_per_question == 4 ~ "4",
                                                     term_count_per_question >= 5 ~ "5 or above"),
                          fill_color = case_when(term_count_per_question == 1 ~ color_palette %>% slice(3) %>% pull(hex),
                                                 term_count_per_question == 2 ~ color_palette %>% slice(4) %>% pull(hex),
                                                 term_count_per_question == 3 ~ color_palette %>% slice(6) %>% pull(hex),
                                                 term_count_per_question == 4 ~ color_palette %>% slice(8) %>% pull(hex),
                                                 term_count_per_question >= 5 ~ color_palette %>% slice(9) %>% pull(hex)))


# create fill_color_list for to pass to scale_color_manual
unigram_tf_idf_chart_fill_color_list <- unigram_tf_idf_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(unigram_tf_idf_chart_fill_color_list) <- unigram_tf_idf_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
unigram_tf_idf_chart_fill_color_list


###########################


unigram_tf_idf %>% ggplot(data = ., aes(x = tf_idf)) + geom_histogram()

# get limits
unigram_tf_idf_chart_min_limit <- 0
unigram_tf_idf_chart_max_limit <- unigram_tf_idf_chart_data %>% distinct(tf_idf) %>%
        filter(tf_idf == max(tf_idf)) %>% 
        select(tf_idf) %>% mutate(tf_idf = tf_idf * 1000) %>% 
        pull(tf_idf) %>% ceiling() / 1000
unigram_tf_idf_chart_limits <- c(unigram_tf_idf_chart_min_limit, unigram_tf_idf_chart_max_limit)

# get breaks, gridlines, and labels
unigram_tf_idf_chart_breaks <- round(seq(from = unigram_tf_idf_chart_limits[1], 
                                        to = unigram_tf_idf_chart_limits[2], by = .002), digits = 3)
unigram_tf_idf_chart_gridlines <- round(seq(from = unigram_tf_idf_chart_limits[1], 
                                           to = unigram_tf_idf_chart_limits[2], by = .002), digits = 3)
unigram_tf_idf_chart_labels <- round(seq(from = unigram_tf_idf_chart_limits[1], 
                                        to = unigram_tf_idf_chart_limits[2], by = .002), digits = 3)


###########################
        
# create unigram_tf_idf_chart
unigram_tf_idf_chart <- unigram_tf_idf_chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = term, .x = tf_idf, .desc = FALSE), y = tf_idf, fill = fill_color_bin)) + 
        geom_hline(yintercept = unigram_tf_idf_chart_breaks[-1], colour = "#B2B2B2") +
        geom_col(width = .7) + 
        scale_fill_manual(values = unigram_tf_idf_chart_fill_color_list) +
        scale_y_continuous(limits = unigram_tf_idf_chart_limits, breaks = unigram_tf_idf_chart_breaks) +
        labs(y = "Term prominence", x = NULL, title = "Term prominence (TF-IDF) and frequency", fill = "Term frequency") + coord_flip() +
        theme_bw() +
        theme(
              # plot.background = element_blank(), 
              text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.length.x.bottom = unit(.5,"cm"),
              # axis.text.y = element_blank(), axis.title.y = element_blank(),
              # axis.ticks.x = element_blank(),
              # axis.text.x = element_blank(),
              axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.line.x.bottom = element_line(color = "#000000"),
              axis.line.y.left = element_blank(),
              axis.title.x = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
              plot.title = element_text(size = 44, face = "bold", hjust = .5, family = "Calibri", color = "#000000"),
              legend.position = "bottom",
              # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 8, family = "Trebuchet MS"),
              legend.title = element_text(size = 36, family = "Calibri", face = "plain"),
              legend.text = element_text(size = 36, family = "Calibri", margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")),
              legend.text.align = 0,
              # legend.spacing.y = unit(5.5,"cm"),
              legend.key.size = unit(2, 'lines'),
              # panel.grid = element_blank(),
              # line = element_blank(),
              # rect = element_blank(),
              panel.grid.major = element_line(color = "transparent")
              )
unigram_tf_idf_chart

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 14, height = 14)
print(unigram_tf_idf_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/unigram_tf_idf_chart.docx")


######################################################################################################################################
######################################################################################################################################

        
# get bigram_tf-idf

# get question_terms
bigram_tf_idf <- data %>% unnest_tokens(output = term, input = response, token = "ngrams", n = 2, drop = FALSE) %>%
        count(question_number, term) %>% arrange(desc(n)) %>% rename(term_count_per_question = n) %>%
        bind_tf_idf(term = term, document = question_number, n = term_count_per_question) %>%
        left_join(., data %>% distinct(question, question_number), by = "question_number") %>%
        filter(!str_detect(string = question, regex(pattern = term, ignore_case = TRUE)))

# inspect
bigram_tf_idf
bigram_tf_idf %>% print(n = 50)
bigram_tf_idf %>% filter(question_number == 10) %>% print(n = 50)


###########################################################################################################################


bigram_tf_idf_chart_data <- bigram_tf_idf %>% filter(question_number == 10) %>% arrange(desc(tf_idf)) %>% slice(1:15)
bigram_tf_idf_chart_data


############################

# inspect range of term_count_per_question for the top tf-idf terms to be plotted
# result: it looks like bins of 1, 2, 3, 4, and 5 or above should work well
bigram_tf_idf %>% group_by(question_number) %>% arrange(desc(tf_idf)) %>% slice(1:15) %>%
        ungroup() %>% count(term_count_per_question)

# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)

# add fill_color_bin and fill_color
bigram_tf_idf_chart_data <- bigram_tf_idf_chart_data %>% mutate(fill_color_bin = case_when(term_count_per_question == 1 ~ "1",
                                                                       term_count_per_question == 2 ~ "2",
                                                                       term_count_per_question == 3 ~ "3",
                                                                       term_count_per_question == 4 ~ "4",
                                                                       term_count_per_question >= 5 ~ "5 or above"),
                                            fill_color = case_when(term_count_per_question == 1 ~ color_palette %>% slice(3) %>% pull(hex),
                                                                   term_count_per_question == 2 ~ color_palette %>% slice(4) %>% pull(hex),
                                                                   term_count_per_question == 3 ~ color_palette %>% slice(6) %>% pull(hex),
                                                                   term_count_per_question == 4 ~ color_palette %>% slice(8) %>% pull(hex),
                                                                   term_count_per_question >= 5 ~ color_palette %>% slice(9) %>% pull(hex)))


# create fill_color_list for to pass to scale_color_manual
bigram_tf_idf_chart_fill_color_list <- bigram_tf_idf_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(bigram_tf_idf_chart_fill_color_list) <- bigram_tf_idf_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
bigram_tf_idf_chart_fill_color_list


###########################


bigram_tf_idf %>% ggplot(data = ., aes(x = tf_idf)) + geom_histogram()

# get limits
bigram_tf_idf_chart_min_limit <- 0
bigram_tf_idf_chart_max_limit <- bigram_tf_idf_chart_data %>% distinct(tf_idf) %>%
        filter(tf_idf == max(tf_idf)) %>% 
        select(tf_idf) %>% mutate(tf_idf = tf_idf * 1000) %>% 
        pull(tf_idf) %>% ceiling() / 1000
bigram_tf_idf_chart_limits <- c(bigram_tf_idf_chart_min_limit, bigram_tf_idf_chart_max_limit)

# get breaks, gridlines, and labels
bigram_tf_idf_chart_breaks <- round(seq(from = bigram_tf_idf_chart_limits[1], 
                                                 to = bigram_tf_idf_chart_limits[2], by = .002), digits = 3)
bigram_tf_idf_chart_gridlines <- round(seq(from = bigram_tf_idf_chart_limits[1], 
                                                    to = bigram_tf_idf_chart_limits[2], by = .002), digits = 3)
bigram_tf_idf_chart_labels <- round(seq(from = bigram_tf_idf_chart_limits[1], 
                                                 to = bigram_tf_idf_chart_limits[2], by = .002), digits = 3)


###########################

# create bigram_tf_idf_chart
bigram_tf_idf_chart <- bigram_tf_idf_chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = term, .x = tf_idf, .desc = FALSE), y = tf_idf, fill = fill_color_bin)) + 
        geom_hline(yintercept = bigram_tf_idf_chart_breaks[-1], colour = "#B2B2B2") +
        geom_col(width = .7) + 
        scale_fill_manual(values = bigram_tf_idf_chart_fill_color_list) +
        scale_y_continuous(limits = bigram_tf_idf_chart_limits, breaks = bigram_tf_idf_chart_breaks) +
        labs(y = "Term prominence", x = NULL, title = "Term prominence (TF-IDF) and frequency", fill = "Term frequency") + coord_flip() +
        theme_bw() +
        theme(
                # plot.background = element_blank(), 
                text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.length.x.bottom = unit(.5,"cm"),
                # axis.text.y = element_blank(), axis.title.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
                plot.title = element_text(size = 44, face = "bold", hjust = .5, family = "Calibri", color = "#000000"),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 8, family = "Trebuchet MS"),
                legend.title = element_text(size = 36, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 36, family = "Calibri", margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0,
                # legend.spacing.y = unit(5.5,"cm"),
                legend.key.size = unit(2, 'lines'),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.grid.major = element_line(color = "transparent")
        )
bigram_tf_idf_chart

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 14, height = 14)
print(bigram_tf_idf_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/bigram_tf_idf_chart.docx")


######################################################################################################################################
######################################################################################################################################


# get trigram_tf-idf

# get question_terms
trigram_tf_idf <- data %>% unnest_tokens(output = term, input = response, token = "ngrams", n = 3, drop = FALSE) %>%
        count(question_number, term) %>% arrange(desc(n)) %>% rename(term_count_per_question = n) %>%
        bind_tf_idf(term = term, document = question_number, n = term_count_per_question) %>%
        left_join(., data %>% distinct(question, question_number), by = "question_number") %>%
        filter(!str_detect(string = question, regex(pattern = term, ignore_case = TRUE)))

# inspect
trigram_tf_idf
trigram_tf_idf %>% print(n = 50)
trigram_tf_idf %>% filter(question_number == 10) %>% print(n = 50)


###########################################################################################################################


# note the trigram chart is all terms with only one and two counts, so not really worth a chart


######################################################################################################################################
######################################################################################################################################


# get quadgram_tf-idf
# note the frequency of quadgrams drops off quite a bit

# get question_terms
quadgram_tf_idf <- data %>% unnest_tokens(output = term, input = response, token = "ngrams", n = 4, drop = FALSE) %>%
        count(question_number, term) %>% arrange(desc(n)) %>% rename(term_count_per_question = n) %>%
        bind_tf_idf(term = term, document = question_number, n = term_count_per_question)

# inspect
quadgram_tf_idf
quadgram_tf_idf %>% print(n = 50)
quadgram_tf_idf %>% filter(question_number == 10) %>% print(n = 50)


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


# analyse sentiment

# get data_sentiment
data_sentiment <- data %>% mutate(sentences = get_sentences(response)) %$% sentiment(text.var = sentences) %>% as_tibble()
data_sentiment

# get data_sentiment_terms
data_sentiment_terms <- data %>% mutate(sentences = get_sentences(response)) %$% extract_sentiment_terms(text.var = sentences) %>%
        as_tibble() %>% convert_terms_to_tbl()
data_sentiment_terms

# get_data_sentiment_tbl
data_sentiment_tbl <- data_sentiment_terms %>% 
        left_join(., data_sentiment %>% rename(sentence_sentiment_score = sentiment), by = c("element_id", "sentence_id")) %>%
        mutate(sentence_sentiment = case_when(sentence_sentiment_score < 0 ~ "negative", 
                                             sentence_sentiment_score == 0 ~ "neutral",
                                             sentence_sentiment_score > 0 ~ "positive"),
               term_sentiment_matches_sentence_sentiment_flag = case_when(sentiment == sentence_sentiment ~ 1, TRUE ~ 0))
data_sentiment_tbl

# get element_sentiment_score_avg
element_sentiment_score_avg <- data_sentiment_tbl %>% distinct(element_id, sentence_id, sentence_sentiment_score) %>%
        group_by(element_id) %>%
        mutate(element_sentiment_score_avg = average_downweighted_zero(sentence_sentiment_score),
               element_sentiment_score_sd = sd(sentence_sentiment_score)) %>%
        ungroup()
element_sentiment_score_avg

# add element_sentiment_score_avg to data_sentiment_tbl
data_sentiment_tbl <- data_sentiment_tbl %>% 
        left_join(., element_sentiment_score_avg %>% select(-sentence_sentiment_score), by = c("element_id", "sentence_id")) %>%
        mutate(element_sentiment = case_when(element_sentiment_score_avg < 0 ~ "negative",
                                             element_sentiment_score_avg == 0 ~ "neutral",
                                             element_sentiment_score_avg > 0 ~ "positive"))

# add question_number
data_sentiment_tbl <- data %>% mutate(element_id = row_number()) %>% left_join(data_sentiment_tbl, ., by = "element_id")

# inspect
data_sentiment_tbl 
data_sentiment_tbl %>% distinct(element_id) %>% nrow() # 193 element_id
data_sentiment_tbl %>% distinct(element_id, sentence_id) %>% nrow() # 384 sentences
data_sentiment_tbl %>% distinct(element_id, element_sentiment) %>% count(element_sentiment)
data_sentiment_tbl %>% filter(sentiment != "neutral", !is.na(term)) %>% print(n = 50)
data_sentiment_tbl %>% distinct(element_id, sentence_sentiment) # 291 distinct sentence_sentiments

# 50 elements have both positive and negative sentences
data_sentiment_tbl %>% distinct(element_id, sentence_sentiment) %>% filter(sentence_sentiment != "neutral") %>%
        count(element_id) %>% arrange(desc(n)) %>% filter(n > 1) # 50

# inspect that question_number has the same count of element_id/responses as in data
data_sentiment_tbl %>% distinct(question_number, element_id) %>% count(question_number)
data %>% count(question_number)

# inspect question 10, which has most responses
data_sentiment_tbl %>% filter(question_number == 10, element_sentiment_score_avg < 0, sentiment == "negative", !is.na(term)) %>% 
        select(element_id, sentence_id, term, sentiment, sentence_sentiment, element_sentiment_score_avg) %>% print(n = 50)
data_sentiment_tbl %>% filter(question_number == 10, element_sentiment_score_avg > 0, sentiment == "positive", !is.na(term)) %>% 
        select(element_id, sentence_id, term, sentiment, sentence_sentiment, element_sentiment_score_avg) %>% print(n = 50)
# look at max sentiment_score_avg
data_sentiment_tbl %>% filter(question_number == 10) %>% filter(element_sentiment_score_avg == max(element_sentiment_score_avg))
data_sentiment_tbl %>% filter(question_number == 10) %>% filter(element_sentiment_score_avg == max(element_sentiment_score_avg)) %>%
        slice(1) %>% pull(response)
# and min sentiment_score_avg
data_sentiment_tbl %>% filter(question_number == 10) %>% filter(element_sentiment_score_avg == min(element_sentiment_score_avg))
data_sentiment_tbl %>% filter(question_number == 10) %>% filter(element_sentiment_score_avg == min(element_sentiment_score_avg)) %>%
        slice(1) %>% pull(response)


######################################################################################################################################


# get sentiment_share_chart_data
sentiment_share_chart_data <- data_sentiment_tbl %>% filter(question_number == 10)


##################################


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add fill_color_bin and fill_color
sentiment_share_chart_data <- sentiment_share_chart_data %>% mutate(fill_color_bin = str_to_sentence(string = element_sentiment),
                                          fill_color = case_when(element_sentiment == "positive" ~ color_palette %>% slice(4) %>% pull(hex),
                                                                 element_sentiment == "neutral" ~ color_palette %>% slice(7) %>% pull(hex),
                                                                 element_sentiment == "negative" ~ color_palette %>% slice(9) %>% pull(hex)))


# create fill_color_list for to pass to scale_color_manual
sentiment_share_chart_fill_color_list <- sentiment_share_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(sentiment_share_chart_fill_color_list) <- sentiment_share_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
sentiment_share_chart_fill_color_list


##########################


# get breaks and limits for axis
sentiment_share_chart_breaks <- seq(from = 0, to = 1, by = .1)
sentiment_share_chart_limits <- c(0, 1)
sentiment_share_chart_gridlines <- seq(from = 0, to = 1, by = .2)


##########################


# get sentiment_share_chart
sentiment_share_chart <- sentiment_share_chart_data %>% 
        distinct(element_id, element_sentiment, fill_color_bin) %>% 
        mutate(element_sentiment = str_to_sentence(string = element_sentiment)) %>%
        count(element_sentiment, fill_color_bin) %>%
        mutate(pct = n / sum(n), response_count_label = str_c(n, " responses")) %>%
        ggplot(data = ., aes(x = element_sentiment, y = pct, label = response_count_label, fill = fill_color_bin)) + 
        geom_hline(yintercept = data_sentiment_tbl_gridlines, colour = "#B2B2B2") +
        geom_col(width = .7) +
        geom_text(nudge_y = 0.04, size = 10) +
        scale_fill_manual(values = sentiment_share_chart_fill_color_list) +
        scale_y_continuous(limits = sentiment_share_chart_limits, breaks = sentiment_share_chart_breaks,
                           labels = percent_format(accuracy = 1)) +
        labs(y = "Response share", x = "Sentiment", title = "Response sentiment", fill = NULL) +
        theme_bw() +
        theme(
                # plot.background = element_blank(), 
                text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.length.x.bottom = unit(.5,"cm"),
                # axis.text.y = element_blank(), 
                # axis.ticks.x = element_blank(),
                axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
                plot.title = element_text(size = 44, face = "bold", hjust = .5, family = "Calibri", color = "#000000"),
                legend.position = "none",
                # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 8, family = "Trebuchet MS"),
                # legend.title = element_text(size = 36, family = "Calibri", face = "plain"),
                # legend.text = element_text(size = 36, family = "Calibri", margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")),
                # legend.text.align = 0,
                # legend.spacing.y = unit(5.5,"cm"),
                # legend.key.size = unit(2, 'lines'),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.grid.major = element_line(color = "transparent")
        )

# inspect
sentiment_share_chart

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 14, height = 14)
print(sentiment_share_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/sentiment_share_chart.docx")


######################################################################################################################################


# get sentiment_distribution_chart_data
sentiment_distribution_chart_data <- data_sentiment_tbl %>% filter(question_number == 10)


########################


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add fill_color_bin and fill_color
sentiment_distribution_chart_data <- sentiment_distribution_chart_data %>% 
        mutate(fill_color_bin = case_when(element_sentiment_score_avg < 0 ~ "Negative",
                                          element_sentiment_score_avg == 0 ~ "Neutral",
                                          element_sentiment_score_avg > 0 ~ "Positive"),
                            fill_color = case_when(element_sentiment == "positive" ~ color_palette %>% slice(4) %>% pull(hex),
                                                   element_sentiment == "neutral" ~ color_palette %>% slice(7) %>% pull(hex),
                                                   element_sentiment == "negative" ~ color_palette %>% slice(9) %>% pull(hex)))


# create fill_color_list for to pass to scale_color_manual
sentiment_distribution_chart_fill_color_list <- sentiment_distribution_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(sentiment_distribution_chart_fill_color_list) <- sentiment_distribution_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
sentiment_distribution_chart_fill_color_list


########################


# get breaks and limits for axis
sentiment_distribution_chart_data %>% distinct(element_id, element_sentiment_score_avg) %>%
        ggplot(data = ., aes(x = element_sentiment_score_avg)) + geom_histogram()
sentiment_distribution_chart_data %>% summarize(max_element_sentiment_score_avg = max(element_sentiment_score_avg),
                              min_element_sentiment_score_avg = min(element_sentiment_score_avg))

# get limits
sentiment_distribution_chart_min_limit <- sentiment_distribution_chart_data %>% distinct(element_sentiment_score_avg) %>%
        filter(element_sentiment_score_avg == min(element_sentiment_score_avg)) %>% 
        select(element_sentiment_score_avg) %>% mutate(element_sentiment_score_avg = element_sentiment_score_avg * 10) %>% 
        pull(element_sentiment_score_avg) %>% floor() / 10
sentiment_distribution_chart_max_limit <- sentiment_distribution_chart_data %>% distinct(element_sentiment_score_avg) %>%
        filter(element_sentiment_score_avg == max(element_sentiment_score_avg)) %>% 
        select(element_sentiment_score_avg) %>% mutate(element_sentiment_score_avg = element_sentiment_score_avg * 10) %>% 
        pull(element_sentiment_score_avg) %>% ceiling() / 10
sentiment_distribution_chart_limits <- c(sentiment_distribution_chart_min_limit, sentiment_distribution_chart_max_limit)

# get breaks, gridlines, and labels
sentiment_distribution_chart_breaks <- if(length(round(seq(from = sentiment_distribution_chart_limits[1], 
                                                 to = sentiment_distribution_chart_limits[2], by = .1), digits = 1)) > 6) {
                                                         c(round(seq(from = 0, 
                                                                   to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                           round(seq(from = 0, 
                                                                     to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                                 } else {
                                                         c(round(seq(from = 0, 
                                                                     to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                           round(seq(from = 0, 
                                                                     to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                                 }
sentiment_distribution_chart_gridlines <- if(length(sentiment_distribution_chart_breaks) > 6) {
                                                c(round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                  round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                        } else {
                                                c(round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                  round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                           }     
sentiment_distribution_chart_labels <- if(length(sentiment_distribution_chart_breaks) > 6) {
                                                c(round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                  round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                        } else {
                                                c(round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[1], by = -.2), digits = 1),
                                                  round(seq(from = 0, 
                                                            to = sentiment_distribution_chart_limits[2], by = .2), digits = 1))
                                        }
            


######################


# create sentiment_distribution_chart
sentiment_distribution_chart <- sentiment_distribution_chart_data %>% 
        distinct(element_id, element_sentiment_score_avg, fill_color_bin) %>%
        mutate(x_axis_bin = "") %>%
        ggplot(data = ., aes(x = x_axis_bin, y = element_sentiment_score_avg, color = fill_color_bin)) + 
        geom_hline(yintercept = sentiment_distribution_chart_gridlines, colour = "#B2B2B2") +
        geom_hline(yintercept = 0.0, colour = "#B2B2B2", size = 6) +
        geom_jitter(shape = "circle", stroke = 10, width = 0.3) +
        scale_color_manual(values = sentiment_distribution_chart_fill_color_list) +
        scale_y_continuous(limits = sentiment_distribution_chart_limits, breaks = sentiment_distribution_chart_breaks,
                           labels = sentiment_distribution_chart_labels) +
        labs(y = "Sentiment score", x = "Responses", title = "Response sentiment score", color = NULL) +
        theme_bw() +
        theme(
                # plot.background = element_blank(), 
                text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.length.x.bottom = unit(.5,"cm"),
                # axis.text.y = element_blank(), 
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 0, r = 0, b = 5, l = 0)),
                plot.title = element_text(size = 44, face = "bold", hjust = .5, family = "Calibri", color = "#000000"),
                legend.position = "bottom",
                legend.title = element_text(size = 36, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 36, family = "Calibri", margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")),
                # legend.text.align = 0,
                # legend.spacing.y = unit(5.5,"cm"),
                # legend.key.size = unit(2, 'lines'),
                legend.key.width = unit(4, "lines"),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.grid.major = element_line(color = "transparent")
        )

# inspect
sentiment_distribution_chart

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 14, height = 14)
print(sentiment_distribution_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/sentiment_distribution_chart.docx")


######################################################################################################################################


# get sentiment_terms_chart_data
sentiment_terms_chart_data <- data_sentiment_tbl %>% 
        filter(question_number == 10, sentiment != "neutral", !is.na(term)) %>% 
        count(term, sentiment) %>% arrange(desc(n)) %>% rename(term_count = n) %>%
        slice(1:15) 
sentiment_terms_chart_data


####################


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)

# add fill_color_bin and fill_color
sentiment_terms_chart_data <- sentiment_terms_chart_data %>% 
        mutate(fill_color_bin = str_to_sentence(string = sentiment),
               fill_color = case_when(sentiment == "positive" ~ color_palette %>% slice(4) %>% pull(hex),
                                      sentiment == "neutral" ~ color_palette %>% slice(7) %>% pull(hex),
                                      sentiment == "negative" ~ color_palette %>% slice(9) %>% pull(hex)))


# create fill_color_list for to pass to scale_color_manual
sentiment_terms_chart_fill_color_list <- sentiment_terms_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(sentiment_terms_chart_fill_color_list) <- sentiment_terms_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
sentiment_terms_chart_fill_color_list

# # add fill_color_bin and fill_color
# sentiment_terms_chart_data <- sentiment_terms_chart_data %>% mutate(fill_color_bin = case_when(term_count == 1 ~ "1",
#                                                                      term_count == 2 ~ "2",
#                                                                      term_count == 3 ~ "3",
#                                                                      term_count == 4 ~ "4",
#                                                                      term_count >= 5 ~ "5 or above"),
#                                           fill_color = case_when(term_count == 1 ~ color_palette %>% slice(3) %>% pull(hex),
#                                                                  term_count == 2 ~ color_palette %>% slice(4) %>% pull(hex),
#                                                                  term_count == 3 ~ color_palette %>% slice(6) %>% pull(hex),
#                                                                  term_count == 4 ~ color_palette %>% slice(8) %>% pull(hex),
#                                                                  term_count >= 5 ~ color_palette %>% slice(9) %>% pull(hex)))
# 
# 
# # create fill_color_list for to pass to scale_color_manual
# sentiment_terms_chart_fill_color_list <- sentiment_terms_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
# names(sentiment_terms_chart_fill_color_list) <- sentiment_terms_chart_data %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
# sentiment_terms_chart_fill_color_list


####################


# get breaks and limits for axis
sentiment_terms_chart_breaks = seq(from = 0, to = sentiment_terms_chart_data %>% filter(term_count == max(term_count)) %>% pull(term_count), by = 1)
sentiment_terms_chart_limits = c(0, sentiment_terms_chart_data %>% filter(term_count == max(term_count)) %>% pull(term_count))
sentiment_terms_chart_gridlines = seq(from = 0, to = sentiment_terms_chart_data %>% filter(term_count == max(term_count)) %>% pull(term_count), by = 1)


####################


sentiment_terms_chart <- sentiment_terms_chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = term, .x = term_count, .desc = FALSE), y = term_count, fill = fill_color_bin)) + 
        geom_hline(yintercept = sentiment_terms_chart_gridlines[-1], colour = "#B2B2B2") +
        geom_col(width = .7) + 
        scale_fill_manual(values = sentiment_terms_chart_fill_color_list) +
        scale_y_continuous(limits = sentiment_terms_chart_limits, breaks = sentiment_terms_chart_breaks) +
        labs(y = "Term frequency", x = NULL, title = "Frequency of sentiment terms", fill = "Sentiment") + coord_flip() +
        theme_bw() +
        theme(
                # plot.background = element_blank(), 
                text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.length.x.bottom = unit(.5,"cm"),
                # axis.text.y = element_blank(), axis.title.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.text.x = element_blank(),
                axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 40, color = "#000000", margin = margin(t = 15, r = 0, b = 5, l = 0)),
                plot.title = element_text(size = 44, face = "bold", hjust = .5, family = "Calibri", color = "#000000"),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 8, family = "Trebuchet MS"),
                legend.title = element_text(size = 36, family = "Calibri"),
                legend.text = element_text(size = 36, family = "Calibri", margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0,
                # legend.spacing.y = unit(5.5,"cm"),
                legend.key.size = unit(2, 'lines'),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.grid.major = element_line(color = "transparent")
        )
sentiment_terms_chart

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 14, height = 14)
print(sentiment_terms_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/sentiment_terms_chart.docx")



######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


# analyze emotion

# get data_emotion
data_emotion <- data %>% mutate(sentences = get_sentences(response)) %$% emotion(text.var = sentences) %>% 
        as_tibble() %>% mutate(emotion_type = as.character(emotion_type)) %>% rename(emotion_rate = emotion) 
data_emotion

# get data_emotion_terms
data_emotion_terms <- data %>% mutate(sentences = get_sentences(response)) %$% extract_emotion_terms(text.var = sentences) %>%
        as_tibble() %>% convert_terms_to_tbl()
data_emotion_terms

 # add question_number
data_emotion <- data %>% mutate(element_id = row_number()) %>% left_join(data_emotion, ., by = "element_id")
data_emotion

# inspect
data_emotion %>% distinct(emotion_type)
data_emotion_terms %>% filter(!is.na(term)) %>% count(term) %>% arrange(desc(n)) %>% print(n = 20)
data_emotion_terms %>% filter(!is.na(term)) %>% count(emotion) %>% arrange(desc(n)) %>% print(n = 20)
data_emotion_terms %>% filter(!is.na(term), emotion == "trust") %>% count(term) %>% arrange(desc(n)) %>% print(n = 20)

# not too many emotions even looking at all questions
data_emotion %>% filter(emotion_count > 1) %>% group_by(emotion_type) %>% 
        summarize(emotion_count_sum = sum(emotion_count)) %>% arrange(desc(emotion_count_sum))
data_emotion %>% filter(emotion_count > 1) %>% group_by(emotion_type) %>% 
        summarize(emotion_count_sum = sum(emotion_count)) %>% arrange(desc(emotion_count_sum)) %>% summarize(sum = sum(emotion_count_sum)) # 134

# only 4 questions have >10 emotion_counts; question 10 has the most
data_emotion %>% filter(emotion_count > 1) %>% group_by(question_number, emotion_type) %>% 
        summarize(emotion_count_sum = sum(emotion_count)) %>% 
        ungroup() %>% group_by(question_number) %>%
        summarize(question_emotion_count_sum = sum(emotion_count_sum)) %>% ungroup()
data_emotion %>% filter(emotion_count > 1) %>% group_by(question_number, emotion_type) %>% 
        summarize(emotion_count_sum = sum(emotion_count)) %>% 
        ungroup() %>% group_by(question_number) %>%
        summarize(question_emotion_count_sum = sum(emotion_count_sum)) %>% ungroup() %>% summarize(sum = sum(question_emotion_count_sum)) # 134

#########################################################################################################################################









