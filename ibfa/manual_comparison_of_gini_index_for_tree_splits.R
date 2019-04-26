library(tidyverse)

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")

# load treatment_post_interview_dummies
treatment_post_interview_dummies <- read_csv("treatment_post_interview_dummies_v2_20180508.csv")
glimpse(treatment_post_interview_dummies)
dim(treatment_post_interview_dummies)

# get gini index score for ben_region_of_birth_Africa
treatment_post_interview_dummies %>% tabyl(ben_region_of_birthAfrica)
treatment_post_interview_dummies %>% filter(ben_region_of_birthAfrica == 0) %>% tabyl(Case_OutcomeDenied)
treatment_post_interview_dummies %>% filter(ben_region_of_birthAfrica == 1) %>% tabyl(Case_OutcomeDenied)
(.3928^2 + .607^2)*(.0784) + (.091^2 + .908^2)*(.9215)

# get gini index score for ben_prior_marriageY
treatment_post_interview_dummies %>% tabyl(ben_prior_marriageY)
treatment_post_interview_dummies %>% filter(ben_prior_marriageY == 1) %>% tabyl(Case_OutcomeDenied) 
treatment_post_interview_dummies %>% filter(ben_prior_marriageY == 0) %>% tabyl(Case_OutcomeDenied) 

(.238^2 + .7619^2)*(.235) + (.0769^2 + .923^2)*(.7647)

