# =====================================================================#
# File name: fuzzy-joins.R
# This is code to create: examples of fuzzy joins in R
# Authored by and feedback to: @mjfrigaard
# Last updated:  2020-10-26
# MIT License
# Version:
# =====================================================================#



# Problem --------------------------------------------------------------
# The more prevalent example is visit windows:
#   Visit 52 target = 15Jul2020 -> specific date is given
#   Visit 52 window = +/- 7 days -> window is given
# Lab sample A may be collected
#   Visit 52 target = 15Jul2020
#   Visit 52 actual = 16Jul2020
#   Visit 52 window = +/- 7 days
# Lab sample A may be collected anytime in the window.
# Lab sample A collection date = 20Jul2020
# The need is to join lab sample A to Visit 52.  Can't join on date because
# the dates don't match.
#
# Join 1: >= 19Mar2019 and <=21Mar2019
# Join 2: >= 28Mar2019 and <=30Mar2019

# packages -----------------------------------------------------------------
library(datapasta)
library(tidyverse)
library(fuzzyjoin)
library(janitor)
library(IRanges)

# Possible solutions ------------------------------------------------------
# the fuzzyjoin package has two functions for joining on inexact matches:
# fuzzyjoin::interval_left_join()
# fuzzyjoin::fuzzy_left_join()
# https://community.rstudio.com/t/tidy-way-to-range-join-tables-on-an-interval-of-dates/7881
# 


# create sample data.frame ------------------------------------------------
IntervalLabs <- tibble::tribble(
     ~SUBJECT,    ~LBDAT,     ~LBREFID,  ~LBTESTCD, ~LBSPID,                                 ~LBTEST,                         ~LBSCAT,
   "999999-1", "3/19/19", 290353092093,       "PT",    462L,                      "Prothrombin Time",                   "Coagulation",
   "999999-1", "3/19/19", 290353092093,      "INR",    461L,    "Prothrombin Intl. Normalized Ratio",                   "Coagulation",
   "999999-1", "3/20/19", 290353092093,     "APTT",    231L, "Activated Partial Thromboplastin Time",                   "Coagulation",
   "999999-1", "3/20/19", 290353092093,       "TT",    548L,                         "Thrombin Time",                   "Coagulation",
   "999999-1", "3/28/19", 290353092093, "FVIIIINE",    915L,       "Factor VIII Inhibitor Expedited",                              NA,
   "999999-1", "3/28/19", 290353092093, "AAV5ABSA",    811L,        "AAV5 Antibody Screening (ARUP)", "AAV5 Antibody Screening - Pre",
   "999999-1", "3/29/19", 290353092093,  "AAV5ABA",    813L,                  "AAV5 Antibody (ARUP)",           "AAV5 Antibody - Pre",
   "999999-1", "3/30/19", 290353092093, "AFVIIIAT",    785L,   "Anti-Factor VIII Antibody Titration",                              NA,
   "999999-1", "3/31/19", 290353092093, "FVIIIABS",    743L,     "Factor VIII Antibody Titer Screen",                              NA)

# lowercase names
IntervalLabs <- IntervalLabs %>% 
    janitor::clean_names(case = "snake")

# convert dates 
IntervalLabs <- IntervalLabs %>%
  dplyr::mutate(
    lbdat = lubridate::mdy(lbdat))

# create sample visit data ------------------------------------------------
IntervalVists <- tibble::tribble(
          ~SUBJECT, ~VISIT, ~TARGETDATE, ~UPPERDATE, ~LOWERDATE,
        "999999-1",    "A",   "3/19/19",         NA,         NA,
        "999999-1",    "B",   "3/28/19",         NA,         NA)

# lowercase names
IntervalVists <- IntervalVists %>% 
    janitor::clean_names(case = "snake")

# convert dates 
IntervalVists <- IntervalVists %>%
  dplyr::mutate(
    targetdate = lubridate::mdy(targetdate))


# create data window ------------------------------------------------------
# first we calculate a window of labs one day before and after the target date
# (which is pre-determined)

IntervalVists <- IntervalVists %>% 
    dplyr::mutate(
        upperdate = targetdate + 2,
        lowerdate = targetdate - 2)

# You can do it with the fuzzyjoin package, which implements 
# various not quite exact matching joins in dplyr syntax.
# 

# left join ---------------------------------------------------------------
# We want join on the first date window (19Mar2019 +/- 2days)
LeftJoinIntervalLabs <- fuzzy_left_join(x = IntervalVists, 
                 y = IntervalLabs,
  # list id vars to join on 
  by = c("subject" = "subject", 
         # both upper and lower dates are matched on lbdat
         "upperdate" = "lbdat", 
         "lowerdate" = "lbdat"),
  # sent as functions in list in same order as joining ids
  match_fun = list(`==`, `>=`, `<=`)) %>% 
    dplyr::select(
       subject = subject.x,
       lbdat,
       lowerdate,
       upperdate,
       lbtestcd, 
       lbtest)
LeftJoinIntervalLabs

FullJoinIntervalLabs <- fuzzy_full_join(x = IntervalVists, 
                 y = IntervalLabs,
  # list id vars to join on 
  by = c("subject" = "subject", 
         "upperdate" = "lbdat", 
         "lowerdate" = "lbdat"),
  # sent as functions in list in same order as joining ids
  match_fun = list(`==`, `>=`, `<=`)) %>% 
    dplyr::select(
       subject = subject.x,
       lbdat,
       lowerdate,
       upperdate,
       lbtestcd, 
       lbtest)

InnerJoinIntervalLabs <- fuzzy_inner_join(
    x = IntervalVists, 
    y = IntervalLabs,
  # list id vars to join on 
  by = c("subject" = "subject", 
         "upperdate" = "lbdat", 
         "lowerdate" = "lbdat"),
  # sent as functions in list in same order as joining ids
  match_fun = list(`==`, `>=`, `<=`)) %>% 
    dplyr::select(
       subject = subject.x,
       lbdat,
       lowerdate,
       upperdate,
       lbtestcd, 
       lbtest)



# WHY AREN'T THESE WORKING?? ----------------------------------------------

AntiJoinIntervalLabs <- fuzzy_anti_join(x = IntervalLabs, y = IntervalVists,
  # list id vars to join on 
  by = c("subject" = "subject", 
         "lbdat" = "upperdate", 
         "lbdat" = "lowerdate"),
  # sent as functions in list in same order as joining ids
  match_fun = list(`==`, `>=`, `<=`)) 

AntiJoinIntervalLabs2 <- fuzzy_anti_join(x = IntervalVists, y = IntervalLabs,
  # list id vars to join on 
  by = c("subject" = "subject",
         # upperdate greater than lbdat
         "upperdate" = "lbdat", 
         # lowerdate less than lbdat
         "lowerdate" = "lbdat"),
  # sent as functions in list in same order as joining ids
  match_fun = list(`==`, `>=`, `<=`)) 

# The extra argument, in the fuzzy_left_join() function, match_fun, allows
# you to define the matching criterion for each pair of columns as 
# a function. In this case, we want category == category, date >= start, and date <= end.



