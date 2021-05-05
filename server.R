library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  restrictionsTable = read.csv("data/restrictions_timeseries/restrictions_summary.csv")
  restrictionsTable <- restrictionsTable %>% 
    mutate(across("schools_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("pubs_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("shops_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("eating_places_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("stay_at_home", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("household_mixing_indoors_banned", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("wfh", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("rule_of_6_indoors", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("curfew", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("eat_out_to_help_out", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>% 
    rename(
    "schools closed" = schools_closed,
    "pubs closed" = pubs_closed,
    "shops closed" = shops_closed,
    "eating_places_closed" = eating_places_closed,
    "stay at home" = stay_at_home,
    "household mixing indoors banned" = household_mixing_indoors_banned,
    "work from home" = wfh,
    "rule o 6 indoors" = rule_of_6_indoors, 
    "eat out to help out" = eat_out_to_help_out
  )
    
  output$table = renderDataTable({restrictionsTable})
  
})
