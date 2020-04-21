library(tidyverse)
# library(sentimentr)

# load sentimentr library manually, if needed
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sentimentr")
source("load_sentimentr_library.R")
setwd(current_wd)

# create convert_terms_to_tbl()
convert_terms_to_tbl <- function(terms_df) {
        
        # handle sentiment terms
        if(sum(c("negative", "neutral", "positive") %in% names(terms_df)) > 0) {
                
                # get tidy_negative_col
                tidy_negative_col <- terms_df %>% select(element_id, sentence_id, negative) %>% unnest(negative) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "negative") %>% rename(term = negative)
                
                # get tidy_neutral_col
                tidy_neutral_col <- terms_df %>% select(element_id, sentence_id, neutral) %>% unnest(neutral) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "neutral") %>% rename(term = neutral)
                
                # get tidy_positive_col
                tidy_positive_col <- terms_df %>% select(element_id, sentence_id, positive) %>% unnest(positive) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "positive") %>% rename(term = positive)
                
                # combine tidy_negative/neutral/positive_col
                tidy_terms_tbl <- bind_rows(tidy_negative_col, tidy_neutral_col, tidy_positive_col) %>%
                        arrange(element_id, sentence_id, sentiment)
                
                return(tidy_terms_tbl)
        }
        
        # handle emotion terms
        if(sum(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust") %in% names(terms_df)) > 0) {
                
                # get tidy_anger_col
                tidy_anger_col <- terms_df %>% select(element_id, sentence_id, anger) %>% unnest(anger) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "anger") %>% rename(term = anger)
                
                # get tidy_anticipation_col
                tidy_anticipation_col <- terms_df %>% select(element_id, sentence_id, anticipation) %>% unnest(anticipation) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "anticipation") %>% rename(term = anticipation)
                
                # get tidy_disgust_col
                tidy_disgust_col <- terms_df %>% select(element_id, sentence_id, disgust) %>% unnest(disgust) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "disgust") %>% rename(term = disgust)
                
                # get tidy_fear_col
                tidy_fear_col <- terms_df %>% select(element_id, sentence_id, fear) %>% unnest(fear) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "fear") %>% rename(term = fear)
                
                # get tidy_joy_col
                tidy_joy_col <- terms_df %>% select(element_id, sentence_id, joy) %>% unnest(joy) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "joy") %>% rename(term = joy)
                
                # get tidy_sadness_col
                tidy_sadness_col <- terms_df %>% select(element_id, sentence_id, sadness) %>% unnest(sadness) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "sadness") %>% rename(term = sadness)
                
                # get tidy_surprise_col
                tidy_surprise_col <- terms_df %>% select(element_id, sentence_id, surprise) %>% unnest(surprise) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "surprise") %>% rename(term = surprise)
                
                # get tidy_trust_col
                tidy_trust_col <- terms_df %>% select(element_id, sentence_id, trust) %>% unnest(trust) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "trust") %>% rename(term = trust)
                
                # combine tidy_negative/neutral/positive_col
                tidy_terms_tbl <- bind_rows(tidy_anger_col, tidy_anticipation_col, tidy_disgust_col,
                                            tidy_fear_col, tidy_joy_col, tidy_sadness_col,
                                            tidy_surprise_col, tidy_trust_col) %>%
                        arrange(element_id, sentence_id, emotion)
                
                return(tidy_terms_tbl)
        }
}


##############################


# test

# create mytext
# mytext <- c(
#         "I am not afraid of you",
#         NA,
#         "",
#         "I love it [not really]", 
#         "I'm not angry with you", 
#         "I hate it when you lie to me.  It's so humiliating",
#         "I'm not happpy anymore.  It's time to end it",
#         "She's a darn good friend to me",
#         "I went to the terrible store",
#         "There is hate and love in each of us",
#         "I'm no longer angry!  I'm really experiencing peace but not true joy.",
#         
#         str_c("Out of the night that covers me, Black as the Pit from pole to", 
#               "pole, I thank whatever gods may be For my unconquerable soul.",
#               "In the fell clutch of circumstance I have not winced nor cried",
#               "aloud. Under the bludgeonings of chance My head is bloody, but unbowed.",
#               "Beyond this place of wrath and tears Looms but the Horror of the", 
#               "shade, And yet the menace of the years Finds, and shall find, me unafraid.",
#               "It matters not how strait the gate, How charged with punishments", 
#               "the scroll, I am the master of my fate: I am the captain of my soul."
#         )    
# )
# 
# # get mytext_sentiment_terms
# mytext_sentiment_terms <- mytext_tbl %$% extract_sentiment_terms(sentences)
# mytext_sentiment_terms 
# 
# # convert mytext_sentiment_terms to tbl
# mytext_sentiment_terms %>% convert_terms_to_tbl()

