library(tidyverse)
library(lubridate)
# library(janitor)
library(readxl)
# library(naniar)
library(testthat)
library(scales)
library(openxlsx)
library(UpSetR)
# library(fs)
library(extrafont)
library(officer)
library(devEMF)
library(RColorBrewer)
library(scales)
library(grid)


# setwd
setwd("X:/Asylum_EAD_study")

options(scipen=999)


# load validate_anumbers function
source("code/helper_scripts/validate_anumbers.R")

# load get_invalid_anumbers function
source("code/helper_scripts/get_invalid_anumbers.R")

# load as_percent function
source("code/helper_scripts/as_percent.R")

# load round_to_digits()
source("code/helper_scripts/round_to_digits.R")

# load add_group_index()
source("code/helper_scripts/add_group_index.R")

# load get_variation_functions()
source("code/helper_scripts/get_variation_functions.R")

# load fy()
source("code/helper_scripts/fy.R")

# load add_dummies()
source("code/helper_scripts/add_dummies.R")

# load add_linebreaks function
source("code/helper_scripts/add_linebreaks.R")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# read in joined data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# load ead_metrics
ead_metrics <- read_csv(file = "data/I-765/ead_metrics_20200903.csv")
ead_metrics %>% glimpse()


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# read in ead princ/dep data
ead <- read_csv("data/I-765/i765_pa_raw.csv")
ead_dep <- read_csv("data/I-765/i765_da_raw.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# note for blues color palette (n = 9)
# when 1 group: 7
# when 2 groups: 4, 9
# when 3 groups: 9, 6, 4
# when 4 groups: 9, 7, 5, 3
# when 5 groups: 9, 8, 7, 5, 3


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_outcome_rates_by_fy_cohort_chart ####


# create chart_data
chart_data <- data %>% add_dummies(outcome_bucket) %>% group_by(filing_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        select(filing_date_fy, grant_pct, denial_pct, referral_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "rate", values_to = "value") 

# inspect
chart_data


#///////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(rate == "denial_pct" ~ "Denials",
                                                               rate == "grant_pct" ~ "Grants",
                                                               rate == "referral_pct" ~ "Referrals"),
                                  color = case_when(rate == "denial_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                         rate == "grant_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                         rate == "referral_pct" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////


# create title

# title <- expression(atop(bold(paste("I-589 decisions as share of adjudicated"^"1", " cases,")),
#                          bold(paste("by I-589 FY filing cohort"))))
title <- "I-589 RAIO outcomes as a share of those adjudicated by RAIO,\nby I-589 FY filing cohort\n(for FY 2009-2018 filing cohorts)"

# create footnotes

# note that using expression for foonotes requires atop() because expression can't handle newlines, but atop can't be left justified, so use cowplot
# footnotes <- expression(atop(paste(""^"1", "Adjudicated cases are those referred, granted, or denied; cases administratively closed or pending are excluded."),
#                         paste("Source: USCIS Refugee, Asylum, & International Operations; USCIS Office of Policy & Strategy")))

# footnotes <- str_c(add_linebreaks(" Adjudicated cases are those referred, granted, or denied; cases administratively closed or pending are excluded."), 
#                    "\n",
#                    add_linebreaks("Source: USCIS RAIO; USCIS OP&S"), 
#                    sep = "")

footnotes <- c("Source: USCIS RAIO; USCIS OP&S")


#////////////////////////////////////////


# create raio_outcome_rates_by_fy_cohort_chart
raio_outcome_rates_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("Grants", "Referrals", "Denials")))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of\nadjudicated cases", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_outcome_rates_by_fy_cohort_chart


#////////////////////////////////////////


# draw superscript for footnotes
# raio_outcome_rates_by_fy_cohort_chart_formatted <- ggdraw(raio_outcome_rates_by_fy_cohort_chart) + 
#         draw_label(label = "1", x = .132, y = .276, size = 7, fontfamily = "Calibri", fontface = "plain", color = "#000000")
# 

#//////////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_outcome_rates_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/raio_outcome_rates_by_fy_cohort_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_outcome_rates_by_fy_chart ####

# note this chart was removed from the report, since the grant/referral rates by FY could be misleading, 
# since it omits cases filed prior to the fy 2009-2018 cohort, 
# also, since there is a very similar grant/referral rate chart based on the fy cohort, it makes sense for clarity/efficiency to keep to cohort chart only

# create chart_data
chart_data <- data %>% 
        add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit)) %>% 
        ungroup() %>%
        filter(!is.na(outcome_date_fy)) %>% 
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        select(outcome_date_fy, grant_pct, denial_pct, referral_pct) %>%
        pivot_longer(cols = -outcome_date_fy, names_to = "rate", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(rate == "denial_pct" ~ "Denials",
                                                          rate == "grant_pct" ~ "Grants",
                                                          rate == "referral_pct" ~ "Referrals"),
                                    color = case_when(rate == "denial_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      rate == "grant_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      rate == "referral_pct" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 RAIO outcomes as a share of those adjudicated by RAIO, by FY\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create raio_outcome_rates_by_fy_chart
raio_outcome_rates_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = outcome_date_fy, y = value, color = factor(color_bin, levels = c("Grants", "Referrals", "Denials")))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2),limits = c(2008, 2021)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "FY", y = "Share of\n adjudicated cases", 
             title = NULL,
                caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.17, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_outcome_rates_by_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_outcome_rates_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/raio_outcome_rates_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_filed_adj_pending_by_fy_chart ####

# get filings_by_fy
filings_by_fy <- data %>% 
        group_by(filing_date_fy) %>% count() %>% rename(fy = filing_date_fy, filing_count = n) %>% ungroup()
filings_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((filing_date_fy != outcome_date_fy) | is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(filing_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% filter(new_pending_flag == 1) %>%
        rename(fy = filing_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy

# get chart_data
chart_data <- data %>% 
        add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed)) %>% 
        ungroup() %>%
        filter(!is.na(outcome_date_fy), outcome_date_fy <= 2018) %>% 
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        rename(fy = outcome_date_fy) %>%
        left_join(., filings_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(cum_filing_count = cumsum(replace_na(filing_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_pending_count = cum_filing_count - cum_adj_count - cum_admin_closed_count) %>%
        select(-c(cum_filing_count, cum_adj_count, cum_admin_closed_count)) %>%
        select(fy, filing_count, adj_count, new_pending_count, cum_pending_count) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "filing_count" ~ "Filed",
                                                          var == "adj_count" ~ "Adjudicated",
                                                          var == "new_pending_count" ~ "Added to pending",
                                                          var == "cum_pending_count" ~ "Cumulative pending"),
                                    color = case_when(var == "filing_count" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "adj_count" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "new_pending_count" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "cum_pending_count" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 RAIO filings, adjudications, and pending cases, by FY\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create raio_filed_adj_pending_by_fy_chart
raio_filed_adj_pending_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = factor(color_bin, levels = c("Filed", "Adjudicated", "Added to pending", "Cumulative pending")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(labels = unit_format(unit = "", scale = .001)) +
        labs(x = "FY", y = "Case count (thousands)", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = .000016 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 8, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_filed_adj_pending_by_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_filed_adj_pending_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/raio_filed_adj_pending_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_outcome_rates_by_fy_cohort_chart ####


# get referral_received_by_filing_fy
referral_received_by_filing_fy <- data %>% group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_by_filing_fy

# get chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count) %>%
        select(fy, relief_granted_rate,
               removal_vol_dep_rate,
               terminated_rate) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "terminated_rate" ~ "Terminated",
                                                          var == "removal_vol_dep_rate" ~ "Removal / Voluntary departure",
                                                          var == "relief_granted_rate" ~ "Relief granted"),
                                    color = case_when(var == "terminated_rate" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "removal_vol_dep_rate" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "relief_granted_rate" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////


# create title
title <- "I-589 EOIR outcomes for as a share of those adjudicated by EOIR,\nby I-589 FY filing cohort\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- c("Source: DOJ EOIR; USCIS RAIO; USCIS OP&S")


#////////////////////////////////////////


# create eoir_outcome_rates_by_fy_cohort_chart
eoir_outcome_rates_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = factor(color_bin, levels = c("Relief granted", "Terminated", "Removal / Voluntary departure")))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of\nadjudicated cases", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_outcome_rates_by_fy_cohort_chart


#////////////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_outcome_rates_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/eoir_outcome_rates_by_fy_cohort_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_outcome_rates_by_fy_chart ####


# note this chart was removed from the report, like it's raio counterpart, since it was redundant with the outcomes by FY cohort version

# get referral_received_by_fy
referral_received_by_fy <- data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = eoir_case_received_date_fy)
referral_received_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((eoir_case_received_date_fy != eoir_outcome_date_fy) | is.na(eoir_outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(eoir_case_received_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% 
        filter(new_pending_flag == 1, eoir_case_received_date_fy >= 2009) %>%
        rename(fy = eoir_case_received_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy


# create chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>%
        rename(fy = eoir_outcome_date_fy) %>%
        left_join(., referral_received_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count) %>%
        select(fy, relief_granted_rate,
               removal_vol_dep_rate,
               terminated_rate) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "relief_granted_rate" ~ "Relief granted",
                                                          var == "removal_vol_dep_rate" ~ "Removal / Voluntary departure",
                                                          var == "terminated_rate" ~ "Terminated"),
                                    color = case_when(var == "relief_granted_rate" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "removal_vol_dep_rate" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "terminated_rate" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 EOIR outcomes as a share of those adjudicated by EOIR, by FY\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create eoir_outcome_rates_by_fy_chart
eoir_outcome_rates_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = factor(color_bin, levels = c("Relief granted", "Terminated", "Removal / Voluntary departure")))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2), limits = c(2008, 2021)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "FY", y = "Share of\n adjudicated cases", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.17, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_outcome_rates_by_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_outcome_rates_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/eoir_outcome_rates_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_referred_adj_pending_by_fy_chart ####

# get referral_received_by_fy
referral_received_by_fy <- data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = eoir_case_received_date_fy)
referral_received_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((eoir_case_received_date_fy != eoir_outcome_date_fy) | is.na(eoir_outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(eoir_case_received_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% 
        filter(new_pending_flag == 1, eoir_case_received_date_fy >= 2009) %>%
        rename(fy = eoir_case_received_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy


# get chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other)) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>%
        rename(fy = eoir_outcome_date_fy) %>%
        left_join(., referral_received_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               admin_closed_other_count = admin_closed_count + other_count,
               cum_referral_received_count = cumsum(replace_na(referral_received_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_other_count = cumsum(replace_na(other_count, replace = "0")),
               cum_pending_count = cum_referral_received_count - cum_adj_count - cum_admin_closed_count - cum_other_count) %>%
        select(fy, referral_received_count, adj_count, new_pending_count, cum_pending_count) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "referral_received_count" ~ "Referrals received",
                                                          var == "adj_count" ~ "Adjudicated",
                                                          var == "new_pending_count" ~ "Added to pending",
                                                          var == "cum_pending_count" ~ "Cumulative pending"),
                                    color = case_when(var == "referral_received_count" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "adj_count" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "new_pending_count" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "cum_pending_count" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 EOIR referrals received, adjudications, and pending cases, by FY\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; DOJ RAOI; USCIS OP&S"


#////////////////////////////////////


# create eoir_referred_adj_pending_by_fy_chart
eoir_referred_adj_pending_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, 
                             color = factor(color_bin, levels = c("Referrals received", "Adjudicated", "Added to pending", "Cumulative pending")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2), limits = c(2008, 2021)) +
        scale_y_continuous(labels = unit_format(unit = "", scale = .001)) +
        labs(x = "FY", y = "Case count (thousands)", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/16200, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 8, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_referred_adj_pending_by_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_referred_adj_pending_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/eoir_referred_adj_pending_by_fy_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% 
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag)) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               terminal_adj_pct = terminal_adj_count / filed_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, terminal_adj_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_relief_granted_pct" ~ "Overall relief granted rate",
                                                          var == "terminal_adj_pct" ~ "Terminally adjudicated rate"),
                                    color = case_when(var == "overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "terminal_adj_pct" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share within selected groups,\nby I-589 FY filing cohort"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "The overall relief granted rate is the share of terminally adjudicated cases that ",
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR. ",
                                        "The terminally adjudicated rate is the share of I-589 filings that have been completed by RAIO or EOIR."), 
                                  nchar_limit = 90),
                   "\n",
                   "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create overall_relief_granted_shares_by_fy_cohort_chart
overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("Terminally adjudicated rate", "Overall relief granted rate")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Rate", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/overall_relief_granted_shares_by_fy_cohort_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ewi_cor_1yr_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(vars(status_at_entry, outcome_bucket, eoir_outcome)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                ewi_count = sum(status_at_entry.EWI),
                referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                eoir_cancellation_applied_count = sum(eoir_cancellation_applied)) %>%
        ungroup() %>%
        mutate(ewi_pct = ewi_count / filed_count,
               referral_w_one_year_limit_pct = referral_w_one_year_limit_count / filed_count,
               eoir_cancellation_applied_pct = eoir_cancellation_applied_count / filed_count) %>% 
        select(filing_date_fy, ewi_pct, referral_w_one_year_limit_pct, eoir_cancellation_applied_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "ewi_pct" ~ "Entered without\ninspection",
                                                          var == "referral_w_one_year_limit_pct" ~ "Referred based on\n1-year filing deadline",
                                                          var == "eoir_cancellation_applied_pct" ~ "Applied for\ncancellation of removal"),
                                    color = case_when(var == "ewi_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "referral_w_one_year_limit_pct" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "eoir_cancellation_applied_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from selected groups,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create ewi_cor_1yr_filing_shares_by_fy_cohort_chart
ewi_cor_1yr_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\nfilings", "Entered without\ninspection", 
                                                                  "Referred based on\n1-year filing deadline", "Applied for\ncancellation of removal")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
ewi_cor_1yr_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ewi_cor_1yr_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/ewi_cor_1yr_filing_shares_by_fy_cohort_chart.docx")



#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(vars(status_at_entry, outcome_bucket, eoir_outcome)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  ewi_adj_count = sum(status_at_entry.EWI[terminal_adjudicated_flag == 1]),
                  ewi_overall_relief_granted_count = sum(status_at_entry.EWI[overall_relief_granted_flag == 1]),
                  referral_w_one_year_limit_adj_count = sum(outcome_bucket.referral_w_one_year_limit[terminal_adjudicated_flag == 1]),
                  referral_w_one_year_limit_overall_relief_granted_count = sum(outcome_bucket.referral_w_one_year_limit[overall_relief_granted_flag == 1]),
                  eoir_cancellation_applied_adj_count = sum(eoir_cancellation_applied[terminal_adjudicated_flag == 1]),
                  eoir_cancellation_applied_overall_relief_granted_count = sum(eoir_cancellation_applied[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               ewi_overall_relief_granted_pct = ewi_overall_relief_granted_count / ewi_adj_count,
               referral_w_one_year_limit_overall_relief_granted_pct = referral_w_one_year_limit_overall_relief_granted_count / 
                       referral_w_one_year_limit_adj_count,
               eoir_cancellation_applied_overall_relief_granted_pct = eoir_cancellation_applied_overall_relief_granted_count / 
                       eoir_cancellation_applied_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, ewi_overall_relief_granted_pct, 
               referral_w_one_year_limit_overall_relief_granted_pct, eoir_cancellation_applied_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_relief_granted_pct" ~ "All I-589 terminally\nadjudicated cases",
                                                          var == "ewi_overall_relief_granted_pct" ~ "Entered without\ninspection",
                                                          var == "referral_w_one_year_limit_overall_relief_granted_pct" ~ "Referred based on\n1-year filing deadline",
                                                          var == "eoir_cancellation_applied_overall_relief_granted_pct" ~ "Applied for\ncancellation of removal"),
                                    color = case_when(var == "overall_relief_granted_pct" ~ "#B2B2B2",
                                                      var == "ewi_overall_relief_granted_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "referral_w_one_year_limit_overall_relief_granted_pct" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "eoir_cancellation_applied_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share within selected groups,\nby I-589 FY filing cohort"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ",
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR."), 
                                  nchar_limit = 100),
      "\n",
      "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S",
      sep = "")


#////////////////////////////////////


# create ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart
ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589 terminally\nadjudicated cases", "Entered without\ninspection", 
                                                                  "Referred based on\n1-year filing deadline", "Applied for\ncancellation of removal")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted rate", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# income_group_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_income_count = sum(`income_group_bucket.Low income`),
                  lower_middle_income_count = sum(`income_group_bucket.Lower middle income`),
                  upper_middle_income_count = sum(`income_group_bucket.Upper middle income`),
                  high_income_count = sum(`income_group_bucket.High income`)) %>%
        ungroup() %>%
        mutate(low_income_pct = low_income_count / filed_count,
               lower_middle_income_pct = lower_middle_income_count / filed_count,
               upper_middle_income_pct = upper_middle_income_count / filed_count,
               high_income_pct = high_income_count / filed_count) %>% 
        select(filing_date_fy, low_income_pct, lower_middle_income_pct, upper_middle_income_pct, high_income_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_income_pct" ~ "Low\nincome",
                                                          var == "lower_middle_income_pct" ~ "Lower middle\nincome",
                                                          var == "upper_middle_income_pct" ~ "Upper middle\nincome",
                                                          var == "high_income_pct" ~ "High\nincome"),
                                    color = case_when(var == "low_income_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_income_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_income_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_income_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country income groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because income data was unavailable for some countries in some years.", 
                                  nchar_limit = 90),
                   "\n",
                   "Source: World Bank; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create income_group_filing_shares_by_fy_cohort_chart
income_group_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("Low\nincome", "Lower middle\nincome",
                                                                  "Upper middle\nincome", "High\nincome")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
income_group_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(income_group_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/income_group_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# income_group_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_income_adj_count = sum(`income_group_bucket.Low income`[terminal_adjudicated_flag == 1]),
                  low_income_overall_relief_granted_count = sum(`income_group_bucket.Low income`[overall_relief_granted_flag == 1]),
                  lower_middle_income_adj_count = sum(`income_group_bucket.Lower middle income`[terminal_adjudicated_flag == 1]),
                  lower_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Lower middle income`[overall_relief_granted_flag == 1]),
                  upper_middle_income_adj_count = sum(`income_group_bucket.Upper middle income`[terminal_adjudicated_flag == 1]),
                  upper_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Upper middle income`[overall_relief_granted_flag == 1]),
                  high_income_adj_count = sum(`income_group_bucket.High income`[terminal_adjudicated_flag == 1]),
                  high_income_overall_relief_granted_count = sum(`income_group_bucket.High income`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_income_overall_relief_granted_pct = low_income_overall_relief_granted_count / low_income_adj_count,
               lower_middle_income_overall_relief_granted_pct = lower_middle_income_overall_relief_granted_count / 
                       lower_middle_income_adj_count,
               upper_middle_income_overall_relief_granted_pct = upper_middle_income_overall_relief_granted_count / 
                       upper_middle_income_adj_count,
               high_income_overall_relief_granted_pct = high_income_overall_relief_granted_count / 
                       high_income_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_income_overall_relief_granted_pct, 
               lower_middle_income_overall_relief_granted_pct, upper_middle_income_overall_relief_granted_pct,
               high_income_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_relief_granted_pct" ~ "All I-589 terminally\nadjudicated cases",
                                                          var == "low_income_overall_relief_granted_pct" ~ "Low\nincome",
                                                          var == "lower_middle_income_overall_relief_granted_pct" ~ "Lower middle\nincome",
                                                          var == "upper_middle_income_overall_relief_granted_pct" ~ "Upper middle\nincome",
                                                          var == "high_income_overall_relief_granted_pct" ~ "High\nincome"),
                                    color = case_when(var == "overall_relief_granted_pct" ~ "#B2B2B2",
                                                      var == "low_income_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_income_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_income_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_income_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share\nwithin applicant country income groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ",
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminaly adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR."), 
                                  nchar_limit = 90),
                   "\n",
                   "Source: World Bank; DOJ EOIR; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create income_group_overall_relief_granted_shares_by_fy_cohort_chart
income_group_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589 terminally\nadjudicated cases", "Low\nincome", "Lower middle\nincome",
                                                                  "Upper middle\nincome", "High\nincome")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted rate", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
income_group_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(income_group_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/income_group_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# homicide_bucket_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_homicide_count = sum(`homicide_bucket.= 1`),
                  lower_middle_homicide_count = sum(`homicide_bucket.1.1 to 10`),
                  middle_homicide_count = sum(`homicide_bucket.10.1 to 20`),
                  upper_middle_homicide_count = sum(`homicide_bucket.20.1 to 40`),
                  high_homicide_count = sum(`homicide_bucket.> 40`)) %>%
        ungroup() %>%
        mutate(low_homicide_pct = low_homicide_count / filed_count,
               lower_middle_homicide_pct = lower_middle_homicide_count / filed_count,
               middle_homicide_pct = middle_homicide_count / filed_count,
               upper_middle_homicide_pct = upper_middle_homicide_count / filed_count,
               high_homicide_pct = high_homicide_count / filed_count) %>% 
        select(filing_date_fy, low_homicide_pct, lower_middle_homicide_pct, middle_homicide_pct, 
               upper_middle_homicide_pct, high_homicide_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_homicide_pct" ~ "\u2264 1",
                                                          var == "lower_middle_homicide_pct" ~ "1.1 to 10",
                                                          var == "middle_homicide_pct" ~ "10.1 to 20",
                                                          var == "upper_middle_homicide_pct" ~ "20.1 to 40",
                                                          var == "high_homicide_pct" ~ "> 40"),
                                    color = case_when(var == "low_homicide_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_homicide_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "middle_homicide_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "upper_middle_homicide_pct" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "high_homicide_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country homicide groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because homicide data was unavailable for some countries in some years.", 
                                  nchar_limit = 90),
                   "\n",
                   "Source: United Nations; USCIS RAIO; USCIS OP&S",
                   sep = "")

#////////////////////////////////////


# create homicide_bucket_filing_shares_by_fy_cohort_chart
homicide_bucket_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("\u2264 1", "1.1 to 10", "10.1 to 20", 
                                                                  "20.1 to 40", "> 40")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = NULL,
             caption = footnotes, color = "Homicides\nper 100k population") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", hjust = .5),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
homicide_bucket_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(homicide_bucket_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/homicide_bucket_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_homicide_adj_count = sum(`homicide_bucket.= 1`[terminal_adjudicated_flag == 1]),
                  low_homicide_overall_relief_granted_count = sum(`homicide_bucket.= 1`[overall_relief_granted_flag == 1]),
                  lower_middle_homicide_adj_count = sum(`homicide_bucket.1.1 to 10`[terminal_adjudicated_flag == 1]),
                  lower_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.1.1 to 10`[overall_relief_granted_flag == 1]),
                  middle_homicide_adj_count = sum(`homicide_bucket.10.1 to 20`[terminal_adjudicated_flag == 1]),
                  middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.10.1 to 20`[overall_relief_granted_flag == 1]),
                  upper_middle_homicide_adj_count = sum(`homicide_bucket.20.1 to 40`[terminal_adjudicated_flag == 1]),
                  upper_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.20.1 to 40`[overall_relief_granted_flag == 1]),
                  high_homicide_adj_count = sum(`homicide_bucket.> 40`[terminal_adjudicated_flag == 1]),
                  high_homicide_overall_relief_granted_count = sum(`homicide_bucket.> 40`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_homicide_overall_relief_granted_pct = low_homicide_overall_relief_granted_count / low_homicide_adj_count,
               lower_middle_homicide_overall_relief_granted_pct = lower_middle_homicide_overall_relief_granted_count / 
                       lower_middle_homicide_adj_count,
               middle_homicide_overall_relief_granted_pct = middle_homicide_overall_relief_granted_count / 
                       middle_homicide_adj_count,
               upper_middle_homicide_overall_relief_granted_pct = upper_middle_homicide_overall_relief_granted_count / 
                       upper_middle_homicide_adj_count,
               high_homicide_overall_relief_granted_pct = high_homicide_overall_relief_granted_count / 
                       high_homicide_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_homicide_overall_relief_granted_pct, 
               lower_middle_homicide_overall_relief_granted_pct, middle_homicide_overall_relief_granted_pct, 
               upper_middle_homicide_overall_relief_granted_pct, high_homicide_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_homicide_overall_relief_granted_pct" ~ "\u2264 1",
                                                          var == "lower_middle_homicide_overall_relief_granted_pct" ~ "1.1 to 10",
                                                          var == "middle_homicide_overall_relief_granted_pct" ~ "10.1 to 20",
                                                          var == "upper_middle_homicide_overall_relief_granted_pct" ~ "20.1 to 40",
                                                          var == "high_homicide_overall_relief_granted_pct" ~ "> 40",
                                                          var == "overall_relief_granted_pct" ~ "All I-589 terminally\nadjudicated cases"),
                                    color = case_when(var == "low_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "upper_middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "high_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "overall_relief_granted_pct" ~ "#B2B2B2"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share\nwithin applicant country homicide groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ",
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR."), 
                                  nchar_limit = 90),
                   "\n",
                   "Source: United Nations; DOJ EOIR; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart
homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589 terminally\nadjudicated cases", "\u2264 1", "1.1 to 10", "10.1 to 20", 
                                                                  "20.1 to 40", "> 40")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted rate", 
             title = NULL,
             caption = footnotes, color = "Homicides\nper 100k population") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", hjust = .5),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# prcl_bucket_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_prcl_count = sum(`prcl_bucket.< 25`),
                  lower_middle_prcl_count = sum(`prcl_bucket.25 to 49`),
                  upper_middle_prcl_count = sum(`prcl_bucket.50 to 74`),
                  high_prcl_count = sum(`prcl_bucket.75 to 100`)) %>%
        ungroup() %>%
        mutate(low_prcl_pct = low_prcl_count / filed_count,
               lower_middle_prcl_pct = lower_middle_prcl_count / filed_count,
               upper_middle_prcl_pct = upper_middle_prcl_count / filed_count,
               high_prcl_pct = high_prcl_count / filed_count) %>% 
        select(filing_date_fy, low_prcl_pct, lower_middle_prcl_pct, 
               upper_middle_prcl_pct, high_prcl_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_prcl_pct" ~ "< 25",
                                                          var == "lower_middle_prcl_pct" ~ "25 to 49",
                                                          var == "upper_middle_prcl_pct" ~ "50 to 74",
                                                          var == "high_prcl_pct" ~ "75 to 100"),
                                    color = case_when(var == "low_prcl_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_prcl_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_prcl_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_prcl_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country political rights / civil liberties groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Lower scores indicate less political rights / civil liberties. ",
                                        "Shares depicted do not sum to 100% because political rights / civil liberties data ",
                                        "was unavailable for some countries in some years."), nchar_limit = 90),
                   "\n",
                "Source: Freedom House; USCIS RAIO; USCIS OP&S",
                sep = "")


#////////////////////////////////////


# create prcl_bucket_filing_shares_by_fy_cohort_chart
prcl_bucket_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("< 25", "25 to 49", "50 to 74", 
                                                                  "75 to 100")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", hjust = .5),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
prcl_bucket_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(prcl_bucket_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/prcl_bucket_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_prcl_adj_count = sum(`prcl_bucket.< 25`[terminal_adjudicated_flag == 1]),
                  low_prcl_overall_relief_granted_count = sum(`prcl_bucket.< 25`[overall_relief_granted_flag == 1]),
                  lower_middle_prcl_adj_count = sum(`prcl_bucket.25 to 49`[terminal_adjudicated_flag == 1]),
                  lower_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.25 to 49`[overall_relief_granted_flag == 1]),
                  upper_middle_prcl_adj_count = sum(`prcl_bucket.50 to 74`[terminal_adjudicated_flag == 1]),
                  upper_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.50 to 74`[overall_relief_granted_flag == 1]),
                  high_prcl_adj_count = sum(`prcl_bucket.75 to 100`[terminal_adjudicated_flag == 1]),
                  high_prcl_overall_relief_granted_count = sum(`prcl_bucket.75 to 100`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_prcl_overall_relief_granted_pct = low_prcl_overall_relief_granted_count / low_prcl_adj_count,
               lower_middle_prcl_overall_relief_granted_pct = lower_middle_prcl_overall_relief_granted_count / 
                       lower_middle_prcl_adj_count,
               upper_middle_prcl_overall_relief_granted_pct = upper_middle_prcl_overall_relief_granted_count / 
                       upper_middle_prcl_adj_count,
               high_prcl_overall_relief_granted_pct = high_prcl_overall_relief_granted_count / 
                       high_prcl_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_prcl_overall_relief_granted_pct, 
               lower_middle_prcl_overall_relief_granted_pct, 
               upper_middle_prcl_overall_relief_granted_pct, high_prcl_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_prcl_overall_relief_granted_pct" ~ "< 25",
                                                          var == "lower_middle_prcl_overall_relief_granted_pct" ~ "25 to 49",
                                                          var == "upper_middle_prcl_overall_relief_granted_pct" ~ "50 to 74",
                                                          var == "high_prcl_overall_relief_granted_pct" ~ "75 to 100",
                                                          var == "overall_relief_granted_pct" ~ "All I-589\ncompleted cases"),
                                    color = case_when(var == "low_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "overall_relief_granted_pct" ~ "#B2B2B2"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share\nwithin applicant country political rights / civil liberties groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because political rights / civil liberties data was unavailable for some countries in some years.", nchar_limit = 110),
                   "\n",
                   "Source: Freedom House; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart
prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\ncompleted cases", "< 25", "25 to 49", "50 to 74", 
                                                                  "75 to 100")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted rate",
             title = NULL,
             caption = footnotes, color = "Total PR/CL\nscore:") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", hjust = .5),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# country_filing_shares_by_fy_cohort_chart ####

# get total_filings
total_filings <- data %>% count(filing_date_fy, name = "total_filings") 
total_filings

# get chart_data
chart_data <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:5) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>%
        summarize(filing_count = n()) %>%
        ungroup() %>%
        left_join(., total_filings, by = "filing_date_fy") %>%
        mutate(filing_share = filing_count / total_filings) %>%
        select(citizenship_output_country_name, filing_date_fy, filing_share) %>%
        rename(value = filing_share)

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = citizenship_output_country_name,
                                    color = case_when(citizenship_output_country_name == "China" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      citizenship_output_country_name == "Venezuela" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      citizenship_output_country_name == "Mexico" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      citizenship_output_country_name == "Guatemala" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      citizenship_output_country_name == "El Salvador" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings for top I-589 filing countries"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create country_filing_shares_by_fy_cohort_chart
country_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value,  
                             color = factor(color_bin, levels = c("El Salvador", "Guatemala", "Mexico", "Venezuela", "China")))) +
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0),
                                           angle = NULL, vjust = 1.2, hjust = 1.2),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/country_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# country_overall_relief_granted_shares_by_fy_cohort_chart ####
               
# get aggregate_overall_relief_granted_pct
aggregate_overall_relief_granted_pct <- data %>% group_by(filing_date_fy) %>%
        summarize(terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag)) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               citizenship_output_country_name = "All I-589 terminally\nadjudicated cases") %>%
        select(filing_date_fy, citizenship_output_country_name, overall_relief_granted_pct) %>%
        rename(var = citizenship_output_country_name, value = overall_relief_granted_pct)
        

# inspect
aggregate_overall_relief_granted_pct


#///////////////////////////////////////////////


# get chart_data
chart_data <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:5) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>%
        summarize(terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag)) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count) %>%
        select(filing_date_fy, citizenship_output_country_name, overall_relief_granted_pct) %>%
        rename(var = citizenship_output_country_name, value = overall_relief_granted_pct) %>%
        bind_rows(., aggregate_overall_relief_granted_pct)

# inspect
chart_data
chart_data %>% count(var)


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = var,
                                    color = case_when(var == "China" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "Venezuela" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "Mexico" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "Guatemala" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "El Salvador" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "All I-589 terminally\nadjudicated cases" ~ "#B2B2B2"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted rate for top I-589 filing countries, by I-589 FY filing cohort"

# create footnotes
footnotes <- str_c(add_linebreaks(str_c("Note: Cases considered to have had 'overall relief granted' are those with either a grant outcome from RAIO, or a ",
                                        "relief granted or cancellation of removal outcome from EOIR. ",
                                        "Overall relief granted rates are the share of terminally adjudicated cases in each category that ",
                                        "had overall relief granted from either RAIO or EOIR. ",
                                        "'Terminally adjudicated cases' are those with either a grant or denial outcome from RAIO, ",
                                        "or those with a relief granted, cancellation of removal, removal, voluntary departure, or termination outcome from EOIR."), 
                                  nchar_limit = 90),
                   "\n",
                   "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create country_overall_relief_granted_shares_by_fy_cohort_chart
country_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value,  
                             color = factor(color_bin, levels = c("All I-589 terminally\nadjudicated cases", "El Salvador", "Guatemala", "Mexico", "Venezuela", "China")))) +
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted rate", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/country_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create princ_and_dep_share_w_at_least_one_ead_by_cohort_chart ####


# get i589_princ_and_dep_count
i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum) %>%
        ungroup() %>%
        select(filing_date_fy, princ_i589_count, dep_i589_count)

# get princ_and_dep_w_at_least_one_approved_ead
princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_w_at_least_one_approved_ead = n_distinct(anumber[i589_princ_flag == 1]),
                  count_dep_w_at_least_one_approved_ead = n_distinct(anumber[i589_dep_flag == 1])) %>%
        ungroup() %>%
        left_join(i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_w_at_least_one_approved_ead = count_princ_w_at_least_one_approved_ead / princ_i589_count,
               overall_share_of_dep_w_at_least_one_approved_ead = count_dep_w_at_least_one_approved_ead / dep_i589_count)


#////////////////////////////


# inspect
princ_and_dep_w_at_least_one_approved_ead

# check that count_princ_w_at_least_one_approved_ead summary matches the raw count of princ anumbers in ead_metrics
expect_equal(object = princ_and_dep_w_at_least_one_approved_ead %>% summarize(princ_w_ead_sum = sum(count_princ_w_at_least_one_approved_ead)) %>%
                     pull(princ_w_ead_sum),
             expected = ead_metrics %>% filter(i589_princ_flag == 1) %>% distinct(anumber) %>% nrow())

# check that count_dep_w_at_least_one_approved_ead summary matches the raw count of dep anumbers in ead_metrics
expect_equal(object = princ_and_dep_w_at_least_one_approved_ead %>% summarize(dep_w_ead_sum = sum(count_dep_w_at_least_one_approved_ead)) %>%
                     pull(dep_w_ead_sum),
             expected = ead_metrics %>% filter(i589_dep_flag == 1) %>% distinct(anumber) %>% nrow())


#////////////////////////////////////////////


# get chart_data
chart_data <- princ_and_dep_w_at_least_one_approved_ead %>% 
        select(filing_date_fy, overall_share_of_princ_w_at_least_one_approved_ead, overall_share_of_dep_w_at_least_one_approved_ead) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data
chart_data %>% count(var)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_share_of_princ_w_at_least_one_approved_ead" ~ "I-589 principal\napplicants",
                                                          var == "overall_share_of_dep_w_at_least_one_approved_ead" ~ "I-589 dependent\napplicants"),
                                    color = case_when(var == "overall_share_of_princ_w_at_least_one_approved_ead" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "overall_share_of_dep_w_at_least_one_approved_ead" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create princ_and_dep_share_w_at_least_one_ead_by_cohort_chart
princ_and_dep_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("I-589 principal\napplicants", "I-589 dependent\napplicants")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
princ_and_dep_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(princ_and_dep_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/princ_and_dep_share_w_at_least_one_ead_by_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart ####

# get ewi_one_year_ref_cor_overall_total_i589_princ_and_dep_count
ewi_one_year_ref_cor_overall_total_i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get ewi_one_year_ref_cor_overall_share_of_princ_and_dep_w_at_least_one_approved_ead
ewi_one_year_ref_cor_overall_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(ewi_one_year_ref_cor_overall_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, overall_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
ewi_one_year_ref_cor_overall_share_of_princ_and_dep_w_at_least_one_approved_ead 


#////////////////////////////////////////////


# get ewi_total_i589_princ_and_dep_count
ewi_total_i589_princ_and_dep_count <- data %>% filter(ewi_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get ewi_share_of_princ_and_dep_w_at_least_one_approved_ead
ewi_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% filter(ewi_flag == 1) %>%
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(ewi_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(ewi_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, ewi_share_of_princ_and_dep_w_at_least_one_approved_ead)
        
# inspect
ewi_share_of_princ_and_dep_w_at_least_one_approved_ead 


#////////////////////////////////////////////


# get one_yr_ref_total_i589_princ_and_dep_count
one_yr_ref_total_i589_princ_and_dep_count <- data %>% filter(outcome_bucket == "referral_w_one_year_limit", eoir_received_flag == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead
one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        filter(outcome_bucket == "referral_w_one_year_limit", eoir_received_flag == 1) %>%
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(one_yr_ref_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead 


#////////////////////////////////////////////


# get cor_total_i589_princ_and_dep_count
cor_total_i589_princ_and_dep_count <- data %>% filter(eoir_cancellation_applied == 1) %>%
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get cor_share_of_princ_and_dep_w_at_least_one_approved_ead
cor_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% filter(eoir_cancellation_applied == 1) %>%
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(cor_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(cor_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, cor_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
cor_share_of_princ_and_dep_w_at_least_one_approved_ead 


#////////////////////////////////////////////


# get chart_data
chart_data <- ewi_one_year_ref_cor_overall_share_of_princ_and_dep_w_at_least_one_approved_ead %>%
        left_join(., ewi_share_of_princ_and_dep_w_at_least_one_approved_ead, by = "filing_date_fy") %>% 
        left_join(., one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead, by = "filing_date_fy") %>%
        left_join(., cor_share_of_princ_and_dep_w_at_least_one_approved_ead, by = "filing_date_fy") %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ "All I-589\nfilings",
                                                          var == "ewi_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ "Entered without\ninspection",
                                                          var == "one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ "Referred based on\n1-year filing deadline",
                                                          var == "cor_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ "Applied for\ncancellation of removal"),
                                    color = case_when(var == "overall_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ "#B2B2B2",
                                                      var == "ewi_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "one_yr_ref_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "cor_share_of_princ_and_dep_w_at_least_one_approved_ead" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart
ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                color = factor(color_bin, levels = c("All I-589\nfilings", "Entered without\ninspection",
                                                     "Referred based on\n1-year filing deadline", "Applied for\ncancellation of removal")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/ewi_one_year_ref_cor_share_w_at_least_one_ead_by_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create income_share_w_at_least_one_ead_by_cohort_chart ####


# get income_overall_total_i589_princ_and_dep_count
income_overall_total_i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get overall_share_of_princ_and_dep_w_at_least_one_approved_ead
income_overall_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(income_overall_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        mutate(var = "aggregate") %>%
        rename(value = overall_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
income_overall_share_of_princ_and_dep_w_at_least_one_approved_ead 


#///////////////////////////////////////////////////////////


# get princ_and_dep_i589_headcount_by_income
princ_and_dep_i589_headcount_by_income <- data %>% 
        group_by(filing_date_fy, income_group_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  princ_and_dep_i589_headcount = princ_i589_count + dep_i589_count) %>% 
        ungroup() %>% 
        select(filing_date_fy, income_group_bucket, princ_and_dep_i589_headcount) 

# inspect
princ_and_dep_i589_headcount_by_income


#////////////////////////////////////////////


# get chart_data
chart_data <- ead_metrics %>% 
        distinct(filing_date_fy, anumber, income_group_bucket) %>% 
        count(filing_date_fy, income_group_bucket, name = "princ_and_dep_ead_headcount") %>%
        left_join(princ_and_dep_i589_headcount_by_income, ., by = c("filing_date_fy", "income_group_bucket")) %>%
        mutate(princ_and_dep_share_w_at_least_one_approved_ead = princ_and_dep_ead_headcount / princ_and_dep_i589_headcount) %>%
        select(filing_date_fy, income_group_bucket, princ_and_dep_share_w_at_least_one_approved_ead) %>%
        rename(var = income_group_bucket, value = princ_and_dep_share_w_at_least_one_approved_ead) %>%
        bind_rows(., income_overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        filter(!(is.na(var)))

# inspect
chart_data
chart_data %>% count(var)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "aggregate" ~ "All I-589\nfilings",
                                                          var == "High income" ~ "High",
                                                          var == "Upper middle income" ~ "Upper\nmiddle",
                                                          var == "Lower middle income" ~ "Lower\nmiddle",
                                                          var == "Low income" ~ "Low"),
                                    color = case_when(var == "aggregate" ~ "#B2B2B2",
                                                      var == "High income" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "Upper middle income" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "Lower middle income" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "Low income" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: World Bank; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create income_share_w_at_least_one_ead_by_cohort_chart
income_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\nfilings", "Low", "Lower\nmiddle", "Upper\nmiddle", "High")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
income_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(income_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/income_share_w_at_least_one_ead_by_cohort_chart.docx")




#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create homicide_share_w_at_least_one_ead_by_cohort_chart ####


# get homicide_overall_total_i589_princ_and_dep_count
homicide_overall_total_i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get overall_share_of_princ_and_dep_w_at_least_one_approved_ead
homicide_overall_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(homicide_overall_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        mutate(var = "aggregate") %>%
        rename(value = overall_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
homicide_overall_share_of_princ_and_dep_w_at_least_one_approved_ead 


#///////////////////////////////////////////////////////////


# get princ_and_dep_i589_headcount_by_homicide
princ_and_dep_i589_headcount_by_homicide <- data %>% 
        group_by(filing_date_fy, homicide_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  princ_and_dep_i589_headcount = princ_i589_count + dep_i589_count) %>% 
        ungroup() %>% 
        select(filing_date_fy, homicide_bucket, princ_and_dep_i589_headcount) 

# inspect
princ_and_dep_i589_headcount_by_homicide


#////////////////////////////////////////////


# get chart_data
chart_data <- ead_metrics %>% 
        distinct(filing_date_fy, anumber, homicide_bucket) %>% 
        count(filing_date_fy, homicide_bucket, name = "princ_and_dep_ead_headcount") %>%
        left_join(princ_and_dep_i589_headcount_by_homicide, ., by = c("filing_date_fy", "homicide_bucket")) %>%
        mutate(princ_and_dep_share_w_at_least_one_approved_ead = princ_and_dep_ead_headcount / princ_and_dep_i589_headcount) %>%
        select(filing_date_fy, homicide_bucket, princ_and_dep_share_w_at_least_one_approved_ead) %>%
        rename(var = homicide_bucket, value = princ_and_dep_share_w_at_least_one_approved_ead) %>%
        bind_rows(., homicide_overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        filter(!(is.na(var)))

# inspect
chart_data
chart_data %>% count(var)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "aggregate" ~ "All I-589\nfilings",
                                                          TRUE ~ var),
                                    color = case_when(var == "aggregate" ~ "#B2B2B2",
                                                      var == "> 40" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "20.1 to 40" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "10.1 to 20" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "1.1 to 10" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "\u2264 1" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby country homicide "

# create footnotes
footnotes <- "Source: United Nations; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create homicide_share_w_at_least_one_ead_by_cohort_chart
homicide_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\nfilings", "\u2264 1", "1.1 to 10", "10.1 to 20", "20.1 to 40", "> 40")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "Homicides\nper 100k population") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", hjust = .5),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
homicide_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(homicide_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/homicide_share_w_at_least_one_ead_by_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create prcl_share_w_at_least_one_ead_by_cohort_chart ####


# get prcl_overall_total_i589_princ_and_dep_count
prcl_overall_total_i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get overall_share_of_princ_and_dep_w_at_least_one_approved_ead
prcl_overall_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(prcl_overall_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        mutate(var = "aggregate") %>%
        rename(value = overall_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
prcl_overall_share_of_princ_and_dep_w_at_least_one_approved_ead 


#///////////////////////////////////////////////////////////


# get princ_and_dep_i589_headcount_by_prcl
princ_and_dep_i589_headcount_by_prcl <- data %>% 
        group_by(filing_date_fy, prcl_bucket) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  princ_and_dep_i589_headcount = princ_i589_count + dep_i589_count) %>% 
        ungroup() %>% 
        select(filing_date_fy, prcl_bucket, princ_and_dep_i589_headcount) 

# inspect
princ_and_dep_i589_headcount_by_prcl


#////////////////////////////////////////////


# get chart_data
chart_data <- ead_metrics %>% 
        distinct(filing_date_fy, anumber, prcl_bucket) %>% 
        count(filing_date_fy, prcl_bucket, name = "princ_and_dep_ead_headcount") %>%
        left_join(princ_and_dep_i589_headcount_by_prcl, ., by = c("filing_date_fy", "prcl_bucket")) %>%
        mutate(princ_and_dep_share_w_at_least_one_approved_ead = princ_and_dep_ead_headcount / princ_and_dep_i589_headcount) %>%
        select(filing_date_fy, prcl_bucket, princ_and_dep_share_w_at_least_one_approved_ead) %>%
        rename(var = prcl_bucket, value = princ_and_dep_share_w_at_least_one_approved_ead) %>%
        bind_rows(., prcl_overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        filter(!(is.na(var))) 

# inspect
chart_data
chart_data %>% count(var)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "aggregate" ~ "All I-589\nfilings",
                                                          TRUE ~ var),
                                    color = case_when(var == "aggregate" ~ "#B2B2B2",
                                                      var == "75 to 100" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "50 to 74" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "25 to 49" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "< 25" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby country prcl "

# create footnotes
footnotes <- "Source: Freedom House; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create prcl_share_w_at_least_one_ead_by_cohort_chart
prcl_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\nfilings", "< 25", "25 to 49", "50 to 74", "75 to 100")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
prcl_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(prcl_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/prcl_share_w_at_least_one_ead_by_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_share_w_at_least_one_ead_by_cohort_chart ####


# get country_overall_total_i589_princ_and_dep_count
# note that i checked running this chart for just princ (excluding dep), but the lines barely shifted
# since the focus on eads is not princ-specific, it makes the most sense to include both princ and dep, 
# including child dep makes sense too since they are a sizable fraction of EADs, whether or not they're used more as IDs than actual work permits
# i could do 3 different variations of these charts focused on just princ, princ + dep spouse, princ + dep spouse + dep child, but it'd be little value added
country_overall_total_i589_princ_and_dep_count <- data %>% 
        group_by(filing_date_fy) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum, 
                  total_princ_and_dep_count = princ_i589_count + dep_i589_count) %>%
        ungroup() %>%
        select(filing_date_fy, total_princ_and_dep_count)

# get overall_share_of_princ_and_dep_w_at_least_one_approved_ead
country_overall_share_of_princ_and_dep_w_at_least_one_approved_ead <- ead_metrics %>% 
        group_by(filing_date_fy) %>%
        summarize(count_princ_and_dep_w_at_least_one_approved_ead = n_distinct(anumber)) %>%
        ungroup() %>%
        left_join(overall_total_i589_princ_and_dep_count, ., by = "filing_date_fy") %>%
        mutate(overall_share_of_princ_and_dep_w_at_least_one_approved_ead = count_princ_and_dep_w_at_least_one_approved_ead / total_princ_and_dep_count) %>%
        select(filing_date_fy, overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        mutate(var = "aggregate") %>%
        rename(value = overall_share_of_princ_and_dep_w_at_least_one_approved_ead)

# inspect
country_overall_share_of_princ_and_dep_w_at_least_one_approved_ead 


#///////////////////////////////////////////////////////////


# get princ_and_dep_i589_headcount_by_country
princ_and_dep_i589_headcount_by_country <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:5) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(filing_date_fy, citizenship_output_country_name) %>% 
        summarize(princ_i589_count = n(), dep_child_sum = sum(dep_child_count), dep_spouse_sum = sum(dep_spouse_count),
                  dep_i589_count = dep_child_sum + dep_spouse_sum,
                  princ_and_dep_i589_headcount = princ_i589_count + dep_i589_count) %>% 
        ungroup() %>% 
        select(filing_date_fy, citizenship_output_country_name, princ_and_dep_i589_headcount) 

# inspect
princ_and_dep_i589_headcount_by_country


#////////////////////////////////////////////


# get chart_data
chart_data <- ead_metrics %>% 
        distinct(filing_date_fy, anumber, citizenship_output_country_name) %>% 
        count(filing_date_fy, citizenship_output_country_name, name = "princ_and_dep_ead_headcount") %>%
        left_join(princ_and_dep_i589_headcount_by_country, ., by = c("filing_date_fy", "citizenship_output_country_name")) %>%
        mutate(princ_and_dep_share_w_at_least_one_approved_ead = princ_and_dep_ead_headcount / princ_and_dep_i589_headcount) %>%
        select(filing_date_fy, citizenship_output_country_name, princ_and_dep_share_w_at_least_one_approved_ead) %>%
        rename(var = citizenship_output_country_name, value = princ_and_dep_share_w_at_least_one_approved_ead) %>%
        bind_rows(., country_overall_share_of_princ_and_dep_w_at_least_one_approved_ead) %>%
        filter(!(is.na(var))) 

# inspect
chart_data
chart_data %>% count(var)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "aggregate" ~ "All I-589\nfilings",
                                                          TRUE ~ var),
                                    color = case_when(var == "aggregate" ~ "#B2B2B2",
                                                      var == "China" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "Venezuela" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "Mexico" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "Guatemala" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "El Salvador" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 principal applicants and dependents\nwho received at least one approved EAD,\nby country country "

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create country_share_w_at_least_one_ead_by_cohort_chart
country_share_w_at_least_one_ead_by_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("All I-589\nfilings", "El Salvador", "Guatemala", "Mexico", "Venezuela", "China")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share receiving\nat least one approved EAD", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_share_w_at_least_one_ead_by_cohort_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_share_w_at_least_one_ead_by_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/country_share_w_at_least_one_ead_by_cohort_chart.docx")


# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create country_filings_per_100k_pop_chart ####


# get country_populations
# note: inspections done in create_tables.R
country_populations <- read_csv("data/population/countries_and_country_income_groups/API_SP.POP.TOTL_DS2_en_csv_v2_1068829.csv", skip = 4) %>%
        rename(country_name = `Country Name`) %>%
        mutate(country_name = case_when(country_name == "Egypt, Arab Rep." ~ "Egypt",
                                        country_name == "Venezuela, RB" ~ "Venezuela", 
                                        TRUE ~ country_name)) %>%
        select(-c(`Country Code`, `Indicator Name`, `Indicator Code`, X65)) %>%
        pivot_longer(cols = -country_name, names_to = "mid_year", values_to = "country_population") %>%
        mutate(mid_year = as.numeric(mid_year))

# inspect
country_populations 


# ///////////////////////////////////////////


# create chart_data
chart_data <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:5) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name, filing_date_fy) %>% count() %>%
        ungroup() %>%
        left_join(., country_populations, by = c("citizenship_output_country_name" = "country_name", "filing_date_fy" = "mid_year")) %>%
        rename(filed_count = n) %>%
        mutate(filings_per_100k_pop = (filed_count / country_population) * 100000) %>%
        select(filing_date_fy, citizenship_output_country_name, filings_per_100k_pop) %>%
        rename(var = citizenship_output_country_name, value = filings_per_100k_pop)

# inspect
chart_data


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = var,
                                    color = case_when(var == "China" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "Venezuela" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "Mexico" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "Guatemala" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "El Salvador" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: United Nations; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create country_filings_per_100k_pop_chart
country_filings_per_100k_pop_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("El Salvador", "Guatemala", "Mexico", "Venezuela", "China")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = max(chart_data$value), by = 25)) +
        labs(x = "I-589 FY filing cohort", y = "I-589 filings per 100k population", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/36, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_filings_per_100k_pop_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_filings_per_100k_pop_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/country_filings_per_100k_pop_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create raio_eoir_processing_time_and_ead_years_by_cohort_fy_chart ####

# note that processing times can be easily misinterpreted in this case, 
# because there is a relatively large chunk of raio/eoir cases still unadjudicated; eg 54% of raio cases are adj for 2015; 28% for eoir in 2015
# so a line chart of median processing times can make it seem like there's been a steep drop, when it's just censored/omitted data
# for the table showing processing times, i was sure to add the pct of cases adj to give context, but that is hard to do in a line chart without clutter
# for this reason, i'll avoid adding this line chart unless specifically requested

# # get filing_to_terminal_decision_days_by_filing_fy_table
# filing_to_terminal_decision_days_by_filing_fy_table <- data %>% 
#         group_by(filing_date_fy) %>% 
#         summarize(filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
#         mutate(filing_to_terminal_decision_days_median = round(filing_to_terminal_decision_days_median, digits = 0)) 
# 
# # inspect
# filing_to_terminal_decision_days_by_filing_fy_table
# # definition of filing_to_terminal_decision_days
# # mutate(filing_to_terminal_decision_days = as.numeric(outcome_date - filing_date))
#               
#   
# #///////////////////////////////////////////
# 
# 
# # get eoir_received_to_terminal_decision_days_by_filing_fy_table
# eoir_received_to_terminal_decision_days_by_filing_fy_table <- data %>% group_by(filing_date_fy) %>% 
#         summarize(eoir_case_received_to_terminal_decision_days_median = median(eoir_case_received_to_terminal_decision_days, na.rm = TRUE))
# 
# # inspect
# eoir_received_to_terminal_decision_days_by_filing_fy_table
# # definition of eoir_case_received_to_terminal_decision_days below:
# # mutate(eoir_case_received_to_terminal_decision_days = case_when(!is.na(eoir_bia_decision_date) ~ as.numeric(eoir_bia_decision_date - eoir_case_received_date),
# #                                                                 !is.na(eoir_outcome_date) ~ as.numeric(eoir_outcome_date - eoir_case_received_date),
# #                                                                 TRUE ~ NA_real_),
# #        eoir_case_received_to_terminal_decision_days = case_when(eoir_case_received_to_terminal_decision_days < 0 ~ NA_real_,
# #                                                                 TRUE ~ eoir_case_received_to_terminal_decision_days))
# 
# 
# #///////////////////////////////////////
# 
# 
# # get filing_to_raio_or_eoir_max_terminal_decision_days_by_filing_fy_table
# filing_to_raio_or_eoir_max_terminal_decision_days_by_filing_fy_table <- data %>% 
#         mutate(raio_or_eoir_max_terminal_decision_days = case_when(!is.na(eoir_case_received_to_terminal_decision_days) ~ 
#                                                                            eoir_case_received_to_terminal_decision_days,
#                                                                    !is.na(filing_to_terminal_decision_days) ~ filing_to_terminal_decision_days,
#                                                                    TRUE ~ NA_real_)) %>%
#         group_by(filing_date_fy) %>%
#         summarize(filing_to_raio_or_eoir_max_terminal_decision_days_median = median(raio_or_eoir_max_terminal_decision_days, na.rm = TRUE))
# 
# # inspect
# filing_to_raio_or_eoir_max_terminal_decision_days_by_filing_fy_table
# data %>% filter(is.na(eoir_case_received_to_terminal_decision_days)) %>% count(eoir_outcome)
# data %>% filter(is.na(filing_to_terminal_decision_days)) %>% count(outcome_bucket)
# 
# 
# #///////////////////////////////////////
# 
# 
# # get chart_data
# chart_data <- filing_to_terminal_decision_days_by_filing_fy_table %>% 
#         left_join(., eoir_received_to_terminal_decision_days_by_filing_fy_table, by = "filing_date_fy") %>%
#         left_join(., filing_to_raio_or_eoir_max_terminal_decision_days_by_filing_fy_table, by = "filing_date_fy") %>%
#         pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value") %>%
#         mutate(value = value / 365)
# 
# # inspect
# chart_data
# chart_data %>% count(var)
# 
# 
# #///////////////////////////////////////
# 
# 
# # get color_palette
# display.brewer.pal(n = 9, name = "Blues")
# color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
# color_palette
# 
# # add color_bin and color
# chart_data <- chart_data %>% 
#         mutate(color_bin = case_when(var == "filing_to_terminal_decision_days_median" ~ "Median years from I-589 filing\nto RAIO completion",
#                                      var == "eoir_case_received_to_terminal_decision_days_median" ~ "Median years from referral received\nto EOIR completion",
#                                      var == "filing_to_raio_or_eoir_max_terminal_decision_days_median" ~ "Median years from I-589 filing\nto terminal completion"),
#                                     color = case_when(var == "filing_to_terminal_decision_days_median" ~ color_palette %>% slice(9) %>% pull(hex),
#                                                       var == "eoir_case_received_to_terminal_decision_days_median" ~ color_palette %>% slice(7) %>% pull(hex),
#                                                       var == "filing_to_raio_or_eoir_max_terminal_decision_days_median" ~ color_palette %>% slice(5) %>% pull(hex)))
# 
# # create color_list for to pass to scale_color_manual
# chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
# names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
# chart_data_color_list
# 
# 
# #///////////////////////////////////////
# 
# 
# # create title
# title <- "I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort"
# 
# # create footnotes
# footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"
# 
# 
# #////////////////////////////////////
# 
# 
# # create raio_eoir_processing_time_and_ead_years_by_cohort_fy_chart
# raio_eoir_processing_time_and_ead_years_by_cohort_fy_chart <- chart_data %>%
#         ggplot(data = ., aes(x = filing_date_fy, y = value, 
#                 color = factor(color_bin, levels = c("Median years from I-589 filing\nto RAIO completion", 
#                                                      "Median years from referral received\nto EOIR completion", 
#                                                      "Median years from I-589 filing\nto terminal completion")))) + 
#         geom_line(size = 2) + geom_point(size = 4) + 
#         scale_color_manual(values = chart_data_color_list) +
#         scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
#         scale_y_continuous(breaks = seq(from = 0, to = max(chart_data$value), by = .5)) +
#         labs(x = "I-589 FY filing cohort", y = "Years", 
#              title = NULL,
#              caption = footnotes, color = "") +
#         coord_fixed(ratio = 1/.5, clip = "off") +
#         theme_bw() +
#         theme(
#                 # plot.background = element_rect(fill = "blue"),
#                 plot.margin = unit(c(0, 10, 0, 0), "mm"),
#                 plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
#                                             color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
#                 # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
#                 panel.grid.minor = element_blank(),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.major.y = element_line(color = "#DDDDDD"),
#                 # panel.grid.major.y = element_line(color = "#000000"),
#                 panel.border = element_blank(),
#                 # panel.grid = element_blank(),
#                 # line = element_blank(),
#                 # rect = element_blank(),
#                 # axis.ticks.y = element_blank(),
#                 # axis.ticks.x = element_blank(),
#                 axis.ticks.length.y.left = unit(.2, "cm"),
#                 axis.ticks.length.x.bottom = unit(.2, "cm"),
#                 axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
#                 axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
#                 axis.line.x.bottom = element_line(color = "#000000"),
#                 axis.line.y.left = element_line(color = "#000000"),
#                 axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
#                 axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
#                 plot.title = NULL,
#                 legend.position = "bottom",
#                 # legend.key.size = unit(2, "mm"), 
#                 legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
#                 legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
#                 # legend.spacing.y = unit(5.5, "cm")
#                 # legend.key = element_rect(size = 5),
#                 # legend.key.size = unit(2, 'lines')
#         )
# 
# # inspect
# raio_eoir_processing_time_and_ead_years_by_cohort_fy_chart 


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create raio_outcomes_by_attorney_by_cohort_fy_chart ####

# get chart_data
# note inspections are done in create_tables.R
chart_data <- data %>% add_dummies(outcome_bucket) %>% group_by(attorney_flag) %>%
        summarize(filing_count = n(),
                  adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  filing_to_terminal_decision_days_median = median(filing_to_terminal_decision_days, na.rm = TRUE)) %>%
        ungroup() %>% mutate(filing_pct = filing_count / sum(filing_count), 
                             adj_pct = adj_count / filing_count,
                             grant_pct = grant_count / adj_count, 
                             denial_pct = denial_count / adj_count, 
                             referral_pct = referral_count / adj_count) %>%
        select(attorney_flag, filing_pct, adj_pct, grant_pct) %>% 
        pivot_longer(cols = -attorney_flag, names_to = "var", values_to = "value") %>%
        mutate(var = case_when(var == "filing_pct" ~ "Filed cases\nw/wo an attorney\nas a share of all filings",
                               var == "adj_pct" ~ "Adjudicated cases\nas a share of filings\nw/wo an attorney",
                               var == "grant_pct" ~ "Granted cases\nas a share of adjudications\nw/wo an attorney"))

# inspect
chart_data


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(attorney_flag == 0 ~ "Without attorney",
                                                          attorney_flag == 1 ~ "With attorney"),
                                    color = case_when(attorney_flag == 0 ~ color_palette %>% slice(9) %>% pull(hex),
                                                      attorney_flag == 1 ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create raio_outcomes_by_attorney_by_cohort_fy_chart
raio_outcomes_by_attorney_by_cohort_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = factor(var, levels = c("Filed cases\nw/wo an attorney\nas a share of all filings",
                                                        "Adjudicated cases\nas a share of filings\nw/wo an attorney",
                                                        "Granted cases\nas a share of adjudications\nw/wo an attorney")), y = value, 
                             fill = factor(color_bin, levels = c("With attorney", "Without attorney")))) + 
        geom_col(width = .75, position = position_dodge(width = .8)) + 
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = footnotes, fill = "") +
        coord_fixed(ratio = 1.25 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_outcomes_by_attorney_by_cohort_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_outcomes_by_attorney_by_cohort_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/raio_outcomes_by_attorney_by_cohort_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create eoir_outcomes_by_attorney_by_cohort_fy_chart ####

chart_data <- data %>% add_dummies(eoir_outcome) %>% group_by(eoir_ij_attorney_flag) %>%
        summarize(referral_received_count = sum(eoir_received_flag),
                  adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  appeal_count = sum(eoir_appeal_filed_flag),
                  absentia_count = sum(eoir_absentia, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_pct = relief_granted_count / adj_count,
               adj_pct = adj_count / referral_received_count,
               referral_received_share = referral_received_count / sum(referral_received_count),
               appeal_pct = appeal_count / adj_count,
               absentia_pct = absentia_count / adj_count) %>%
        select(eoir_ij_attorney_flag, referral_received_share,  adj_pct, 
               relief_granted_pct, absentia_pct, appeal_pct) %>%
        pivot_longer(cols = -eoir_ij_attorney_flag, names_to = "var", values_to = "value") %>%
        mutate(var = case_when(var == "referral_received_share" ~ "Referred cases\nw/wo an attorney\nas a share of\nall referrals",
                               var == "adj_pct" ~ "Adjudicated cases\nas a share of\nreferrals\nw/wo an attorney",
                               var == "relief_granted_pct" ~ "Cases with\nrelief granted\nas a share of\nadjudications\nw/wo an attorney",
                               var == "absentia_pct" ~ "Cases with\nin absentia rulings\nas a share of\nadjudications\nw/wo an attorney",
                               var == "appeal_pct" ~ "Appealed cases\nas a share of\nadjudications\nw/wo an attorney"))

# inspect
chart_data


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(eoir_ij_attorney_flag == 0 ~ "Without attorney",
                                                          eoir_ij_attorney_flag == 1 ~ "With attorney"),
                                    color = case_when(eoir_ij_attorney_flag == 0 ~ color_palette %>% slice(9) %>% pull(hex),
                                                      eoir_ij_attorney_flag == 1 ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create eoir_outcomes_by_attorney_by_cohort_fy_chart
eoir_outcomes_by_attorney_by_cohort_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = factor(var, levels = c("Referred cases\nw/wo an attorney\nas a share of\nall referrals",
                                                        "Adjudicated cases\nas a share of\nreferrals\nw/wo an attorney",
                                                        "Cases with\nrelief granted\nas a share of\nadjudications\nw/wo an attorney",
                                                        "Cases with\nin absentia rulings\nas a share of\nadjudications\nw/wo an attorney",
                                                        "Appealed cases\nas a share of\nadjudications\nw/wo an attorney")), y = value, 
                             fill = factor(color_bin, levels = c("With attorney", "Without attorney")))) + 
        geom_col(width = .75, position = position_dodge(width = .8)) + 
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = NULL, y = "Share", 
             title = NULL,
             caption = footnotes, fill = "") +
        coord_fixed(ratio = 2 / 1, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 11, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_outcomes_by_attorney_by_cohort_fy_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_outcomes_by_attorney_by_cohort_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/eoir_outcomes_by_attorney_by_cohort_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create state_i589_share_of_laborforce_by_fy_chart ####

# combine princ/dep to get eads_by_state
# note inspections done in create_tables.R
eads_by_state <-  ead_dep %>% select(dep_anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>% 
        rename(anumber = dep_anumber) %>% 
        filter(anumber %in% i589_dep$dep_anumber) %>%
        bind_rows(., ead %>% select(anumber, receipt_number, valid_from, valid_to, ead_len, ben_state, CURRENT_STATUS, part_2_1) %>%
                          filter(anumber %in% data$anumber)) %>%
        filter(CURRENT_STATUS == "Approved", part_2_1 %in% c("A", "C")) %>%
        mutate(ead_len_clean = case_when(ead_len < 365 ~ 365, 
                                             ead_len >= 365 & ead_len <= 547.5 ~ 365, 
                                             ead_len > 547.5 ~ 730, 
                                             TRUE ~ NA_real_),
               valid_from_year = year(valid_from), valid_from_month = month(valid_from),
               fy_eligibility_cutoff = case_when(valid_from_month >= 10 ~ ymd(str_c(valid_from_year + 1, "-07-02")),
                                                 valid_from_month < 10 ~ ymd(str_c(valid_from_year, "-07-02"))),
               eligibility_fy_1 = case_when(valid_from < fy_eligibility_cutoff ~ fy(valid_from),
                                            valid_from > fy_eligibility_cutoff ~ fy(valid_from) + 1),
               eligibility_fy_2 = case_when(ead_len_clean == 730 ~ eligibility_fy_1 + 1))

# inspect
eads_by_state


#////////////////////////////////


# add state_of_entry by parsing port_of_entry, and inspect
# note i manually fix some odd state_of_entry parsings, but only bother with top 10 filing states
data <- data %>% mutate(state_of_entry = str_replace(string = port_of_entry, pattern = regex(".*, "), replacement = ""),
                        state_of_entry = str_replace(string = state_of_entry, pattern = regex(" \\(.*"), replacement = ""),
                        state_of_entry = case_when(state_of_entry == "WASHINGTON DC" ~ "DC",
                                                   state_of_entry == "OTAY MESA CA" ~ "CA",
                                                   state_of_entry == "FL       ARUBA PRECLEARANCE" ~ "FL",
                                                   state_of_entry == "FL   PRECLEARANCE" ~ "FL",
                                                   state_of_entry == "PASO DEL NORTE,TX" ~ "TX",
                                                   state_of_entry == "WASHINGTON ,DC PRECLEARANCE" ~ "DC",
                                                   state_of_entry == "SEATTLE AIRPORT" ~ "WA",
                                                   state_of_entry == "OAKLAND CNTY INTL AIRPRT(USER FEE)" ~ "CA",
                                                   state_of_entry == "WA SEAPORT" ~ "WA",
                                                   state_of_entry == "FL SEAPLANE BASE" ~ "FL",
                                                   state_of_entry == "NY WHIRLPOOL BRDG" ~ "NY",
                                                   state_of_entry == "WA VICTORIA PRECLEAR" ~ "WA",
                                                   state_of_entry == "WORLD TRADE BRIDGE - LAREDO" ~ "TX", TRUE ~ state_of_entry)) 


#/////////////////////////////////


# get top_states_for_i589_filing
top_states_for_i589_filing <- data %>% count(state) %>% arrange(desc(n)) %>% rename(i589_filing_count = n) %>%
        mutate(i589_filing_count_annual_avg = i589_filing_count / 10,
               i589_filing_count_as_pct_of_all_i589_filing = i589_filing_count / sum(i589_filing_count)) %>% slice(1:5)

# add state_of_entry_count and state_of_entry_pct to top_states_for_i589_filing
top_states_for_i589_filing <- data %>% count(state_of_entry) %>% arrange(desc(n)) %>% rename(state_of_entry_count = n) %>% 
        mutate(state_of_entry_count_as_pct_of_all_state_of_entry = state_of_entry_count / sum(state_of_entry_count)) %>%
        left_join(top_states_for_i589_filing, ., by = c("state" = "state_of_entry")) %>%
        select(state, state_of_entry_count, state_of_entry_count_as_pct_of_all_state_of_entry, 
               i589_filing_count, i589_filing_count_annual_avg, i589_filing_count_as_pct_of_all_i589_filing)

# inspect
top_states_for_i589_filing


#///////////////////////////////


# create get_state_avg_laborforce_by_fy function
get_state_avg_laborforce_by_fy <- function(current_path) {
        
        # get state_abbreviations
        state_abbreviations <- tibble(state_name = state.name, state = state.abb) %>%
                bind_rows(., tibble(state_name = "Puerto Rico", state = "PR"), tibble(state_name = "District of Columbia", state = "DC"))
        
        # get current_state_name
        current_state_name <- read_excel(path = current_path, col_names = "state", range = "B8") %>% 
                pull(state)
        print(current_state_name)
        
        # get current_state_data
        current_state_data <- read_excel(path = current_path, range = "A12:n25")
        current_state_data
        
        # calculate average laborforce by fy
        current_state_data <- current_state_data %>% pivot_longer(cols = -Year, names_to = "month", values_to = "count") %>%
                mutate(month_numeric = case_when(month == "Jan" ~ 1, 
                                                 month == "Feb" ~ 2,
                                                 month == "Mar" ~ 3,
                                                 month == "Apr" ~ 4,
                                                 month == "May" ~ 5,
                                                 month == "Jun" ~ 6,
                                                 month == "Jul" ~ 7,
                                                 month == "Aug" ~ 8,
                                                 month == "Sep" ~ 9,
                                                 month == "Oct" ~ 10,
                                                 month == "Nov" ~ 11,
                                                 month == "Dec" ~ 12),
                       date = str_c(Year, "-", month_numeric, "-01"), 
                       date = ymd(date), fy = fy(date)) %>%
                filter(month != "Annual") %>%
                group_by(fy) %>% summarize(fy_avg = mean(count)) %>%
                ungroup() %>% filter(fy > 2008, fy <= 2019) %>%
                mutate(state_name = current_state_name) %>%
                left_join(., state_abbreviations, by = "state_name")
        
        # return current_state_data
        return(current_state_data)
}


#////////////////////////////////////////


# get laborforce data by mapping over laborforce files calling get_state_avg_laborforce_by_fy
laborforce <- map(.x = dir_ls("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead/data/population/us_states_laborforce"),
                  .f = ~ get_state_avg_laborforce_by_fy(current_path = .x)) %>% bind_rows() 

# inspect
laborforce


#/////////////////////////////////


# get chart_data
chart_data <- eads_by_state %>% 
        pivot_longer(cols = c(eligibility_fy_1, eligibility_fy_2), names_to = "eligibility_period", values_to = "eligibility_fy") %>%
        filter(!is.na(eligibility_fy), eligibility_fy <= 2019) %>% distinct(anumber, eligibility_fy, ben_state) %>%
        count(ben_state, eligibility_fy) %>% rename(anumbers_w_ead_eligibility = n) %>% 
        filter(ben_state %in% top_states_for_i589_filing$state) %>%
        left_join(., laborforce, by = c("ben_state" = "state", "eligibility_fy" = "fy")) %>%
        rename("fy_avg_laborforce" = "fy_avg") %>%
        mutate(eads_as_share_of_laborforce = anumbers_w_ead_eligibility / fy_avg_laborforce) %>%
        left_join(top_states_for_i589_filing %>% select(state), ., by = c("state" = "ben_state")) %>%
        select(state, eligibility_fy, eads_as_share_of_laborforce) %>%
        # mutate(eads_as_share_of_laborforce = case_when(eads_as_share_of_laborforce < .0001 ~ "< 0.01%",
        #                                                TRUE ~ as_percent(eads_as_share_of_laborforce, digits = 2))) %>%
        rename("FY" = eligibility_fy)

# inspect
chart_data
chart_data %>% count(state)


#////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = state,
                                    color = case_when(state == "CA" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      state == "NY" ~ color_palette %>% slice(8) %>% pull(hex), 
                                                      state == "FL" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      state == "TX" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      state == "NJ" ~ color_palette %>% slice(3) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 filings per 100k population across applicant countries, by I-589 FY filing cohort"

# create footnotes
footnotes <- "Source: Dept. of Labor - Bureau of Labor Statistics; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create state_i589_share_of_laborforce_by_fy_chart 
state_i589_share_of_laborforce_by_fy  <- chart_data %>%
        ggplot(data = ., aes(x = FY, y = eads_as_share_of_laborforce, 
                             color = factor(color_bin, levels = c("CA", "NY", "FL", "TX", "NJ")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = .02, by = .004), labels = percent_format(accuracy = .1), limits = c(-.005, .02)) +
        labs(x = "I-589 FY filing cohort", y = "Approved EADs\nas a share of state laborforce", 
             title = NULL,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 2.1/.01, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = NULL,
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), hjust = .5)
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
state_i589_share_of_laborforce_by_fy 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(state_i589_share_of_laborforce_by_fy )
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/state_i589_share_of_laborforce_by_fy .docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ewi_1yr_cor_upset_chart ####

# inspect counts
data %>% mutate(one_year_ref_flag = case_when(outcome_bucket == "referral_w_one_year_limit" ~ 1, TRUE ~ 0),
                cor_applied_flag = case_when(eoir_cancellation_applied == 1 ~ 1, TRUE ~ 0)) %>% 
        summarize(ewi_sum = sum(ewi_flag), ref_sum = sum(one_year_ref_flag), cor_sum = sum(cor_applied_flag))


#/////////////////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

data %>% mutate(one_year_ref_flag = case_when(outcome_bucket == "referral_w_one_year_limit" & eoir_received_flag == 1 ~ 1, TRUE ~ 0),
                cor_applied_flag = case_when(eoir_cancellation_applied == 1 ~ 1, TRUE ~ 0)) %>%
        select(ewi_flag, one_year_ref_flag, cor_applied_flag) %>% 
        rename("EWI" = ewi_flag, "Ref_one_yr" = one_year_ref_flag, "Applied_COR" = cor_applied_flag) %>%
        mutate(useless_integer_needed_for_upsetr_to_color_mainbar = sample(1:nrow(.))) %>%
        data.frame() %>% 
        upset(data = ., 
              order.by = "freq", 
              # mb.ratio = c(0.55, 0.45),
              point.size = 3.5, line.size = 1,
              sets.bar.color = color_palette %>% slice(7) %>% pull(hex),
              main.bar.color = "#000000",
              matrix.color = color_palette %>% slice(9) %>% pull(hex),
              sets.x.label = "Count of I-589\napplications in group",
              mainbar.y.label = "Count of I-589 applications in intersection",
              text.scale = c(1.5, 1.5, 1.5, 1.4, 1.5, 1.5),
              show.numbers = "no",
              set_size.angles = 15,
              queries = list(list(query = intersects, params = list("EWI"), color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("EWI", "Ref_one_yr"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("EWI", "Applied_COR"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("EWI", "Ref_one_yr", "Applied_COR"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("Ref_one_yr"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("Ref_one_yr", "Applied_COR"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T),
                             list(query = intersects, params = list("Applied_COR"),
                                  color = color_palette %>% slice(7) %>% pull(hex), active = T)
                             )
              )
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/charts/ewi_1yr_cor_upset.docx")

