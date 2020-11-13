# =====================================================================#
# File name: interval-joins.R
# This is code to create: examples of fuzzy joins in R
# Authored by and feedback to: @mjfrigaard
# Last updated:  2020-10-26
# MIT License
# Version:
# =====================================================================#

# packages -----------------------------------------------------------------
library(datapasta)
library(tidyverse)
library(fuzzyjoin)
library(janitor)
library(IRanges)



# Create sample data ------------------------------------------------------

TimeTable1 <- tibble(id1 = 1:3, 
                     start_times = c(1, 5, 10), 
                     end_times = c(3, 7, 15))
TimeTable1
# # A tibble: 3 x 3
#     id1 start_times end_times
#   <int>       <dbl>     <dbl>
# 1     1           1         3
# 2     2           5         7
# 3     3          10        15

TimeTable2 <- tibble(id2 = 1:3, 
                     start_times = c(2, 4, 16), 
                     end_times = c(4, 8, 20))
TimeTable2
# # A tibble: 3 x 3
#     id2 start_times end_times
#   <int>       <dbl>     <dbl>
# 1     1           2         4
# 2     2           4         8
# 3     3          16        20

# Allow them to be separated by a gap with a maximum:
# let TimeTable1 join with TimeTable2
interval_inner_join(x = TimeTable1, y = TimeTable2, maxgap = 1) %>% 
    # reorganize the columns
    dplyr::select(dplyr::starts_with(c("id", "start", "end")))

interval_inner_join(TimeTable1, TimeTable2, maxgap = 20) # everything joins each other

# Require that they overlap by more than a particular amount
interval_inner_join(TimeTable1, TimeTable2, minoverlap = 3) %>% 
        dplyr::select(dplyr::starts_with(c("id", "start", "end")))

# other types of joins:
interval_full_join(TimeTable1, TimeTable2)

# 
interval_left_join(TimeTable1, TimeTable2)
# Joining by: c("start_times", "end_times")
# # A tibble: 3 x 6
#     id1 start_times.x end_times.x   id2 start_times.y end_times.y
#   <int>         <dbl>       <dbl> <int>         <dbl>       <dbl>
# 1     1             1           3     1             2           4
# 2     2             5           7     2             4           8
# 3     3            10          15    NA            NA          NA

interval_right_join(TimeTable1, TimeTable2)

interval_semi_join(x = TimeTable1, y = TimeTable2)


# remove the unmatched rows
interval_anti_join(TimeTable1, TimeTable2)
# Joining by: c("start_times", "end_times")
# # A tibble: 1 x 3
#     id1 start_times end_times
#   <int>       <dbl>     <dbl>
# 1     3          10        15