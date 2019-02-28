## Load libraries.
library(tidyverse )
library(devtools)
library(readr)
library(ggplot2)
library(tidyr)
library(glue)
library(gt)

## Load data.
elections <- read_csv("ps_4_elections-poll-nc09-3.csv") %>% 
  filter(response%in%c("Dem","Rep","Und"))

############################################### Problem 1

## There were x respondents who supported the Dem candidate.

dem_support <- elections %>% 
  filter(response=="Dem") %>% 
  tally()

# Other method:
# elections %>% 
#   group_by(response) %>% 
#   summarize(support_count=n()) %>% 
#   filter(response=="Dem") %>% 
#   select(support_count)

## There were X more respondents who favored the Republican candidate than who were Undecided.

rep_support<- elections %>% 
  filter(response=="Dem") %>% 
  tally()

und_support<- elections %>% 
  filter(response=="Und") %>% 
  tally()

rep_support-und_support

## There are two gender variables (gender and gender_combined). There are X individuals for whom these variables have different values.
gender_combined_count <- elections %>% 
  group_by(gender_combined) %>% 
  summarize(count=n())
gender_count<- elections %>% 
  group_by(gender) %>% 
  summarize(count=n())
gender_difference <- gender_combined_count$count-gender_count$count
gender_difference[1]

## There are X respondents listed as “White” under race_eth who are not listed as “White” under file_race_black.
white_count_re <- elections %>% 
  filter(race_eth=="White") %>% 
  summarize(count=n())
white_count_frb<- elections %>% 
  filter(file_race_black=="White") %>% 
  summarize(count=n())
white_difference <- white_count_frb$count-white_count_re$count
white_difference[1]

## The first response of Rep came X minutes (rounded to the nearest minute) before the first response of Dem.
first_rep_response<-elections %>% 
  filter(response=="Rep") %>% 
  summarize(first_rep_response=min(timestamp))
first_dem_response<-elections %>% 
  filter(response=="Dem") %>% 
  summarize(first_dem_response=min(timestamp))
first_rep_response-first_dem_response


