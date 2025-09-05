# Script for aggregating cases identified with data issue and summarising the counts

# INSTRUCTIONS (HOW TO USE THIS R SCRIPT)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run the entire script
# Now highlight the entire page and click "Run" to execute the entire code

# An Excel file will be generated and stored in the Output sub-folder in this main project folder

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Version Control Notes
# **************************
# - Modified aggregate summary function to include filtering and removal of duplicate cases
# - Included Version Control Notes section

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



# load libraries
library(here)
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)


# Load in base tables ----
# load in condition base table to start the aggregate table (first time only)
condition_base_table <- readxl::read_xlsx(here("Data", 
                                       "Aggregate Condition Base Table.xlsx"))

# load in previous aggregate table as base to append new data to
agg_completed_base_table <- readxl::read_xlsx(here("Output", "Aggregate Table", 
                             "aggregate_table_FY2025_2026.xlsx"), 
                             sheet="Completed")

agg_inprogress_base_table <- readxl::read_xlsx(here("Output", "Aggregate Table", 
                                                    "aggregate_table_FY2025_2026.xlsx"), 
                                               sheet="InProgress")

agg_outbreak_base_table <- readxl::read_xlsx(here("Output", "Aggregate Table", 
                                                    "aggregate_table_FY2025_2026.xlsx"), 
                                               sheet="Outbreak")


# Summary tables ----
aggregate_summary <- function(df) {
  table = df %>% 
    # remove duplicate cases
    arrange(phess_id, data_check_date) %>% 
    group_by(phess_id) %>% 
    summarise(condition=first(condition), event_date=first(event_date), data_check_date=first(data_check_date), quarter=first(quarter)) %>% 
    ungroup() %>% 
    # count cases by condition and quarter
    group_by(condition, quarter) %>% 
    summarise(count=n()) %>% 
    right_join(., condition_base_table, by=c("condition" = "CONDITION")) %>% 
    filter(!is.na(count)) %>% 
    relocate(CONDITION_GROUP, .before=condition) %>% 
    pivot_wider(names_from=quarter, values_from=count) %>% 
    rename(CONDITION = condition) %>% 
    arrange(CONDITION_GROUP, CONDITION)
  return(table)
}

aggregate_completed_summary <- aggregate_summary(agg_completed_base_table)

aggregate_inprogress_summary <- aggregate_summary(agg_inprogress_base_table)

aggregate_outbreak_summary <- aggregate_summary(agg_outbreak_base_table)

# Write back tables to excel ----
# first excel file to be written in Oct (after the July-Sept quarter)

