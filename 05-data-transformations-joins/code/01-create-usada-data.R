#=====================================================================#
# This is code to create: 01-create-usada-data.R
# Authored by and feedback to: @mjfrigaard
# MIT License
# Version: 01
#=====================================================================#


# packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# read_html_USADA  --------------------------------------------------------

USADA_url <- "https://www.usada.org/testing/results/sanctions/"
USADA_extraction <- USADA_url %>%
     read_html() %>%
     html_nodes("table")

# USADA_extraction_str ----------------------------------------------------

# check the structure of the new USADA_extraction object
# USADA_extraction %>% str()



# USADA_extraction_class --------------------------------------------------
# USADA_extraction %>% class()



# UsadaRaw ----------------------------------------------------------------

UsadaRaw <- rvest::html_table(USADA_extraction[[1]])

# clean up ----
rm(USADA_url)
rm(USADA_extraction)

# create bad dates ----
UsadaRaw %>% 
    arrange(desc(`Sanction Announced`)) %>% 
    head(20) %>% 
    janitor::clean_names() -> UsadaBadDates

# lowercase ---------------------------------------------------------------

UsadaBadDates <- purrr::map_df(.x = UsadaBadDates, .f = stringr::str_to_lower)

UsadaBadDates <- UsadaBadDates %>% 
    dplyr::select(athlete, sanction_announced, everything()) 
    

# export ------------------------------------------------------------------

# write_csv(x = UsadaRaw, file = "docs/data/UsadaRaw.csv")
# write_csv(x = UsadaBadDates, file = "docs/data/UsadaBadDates.csv")
