library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)


shinyServer(function(input, output) {
  restrictionsTable = read.csv(url("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/03073b4a-2f5d-4a0a-be90-4fe3d9f609c9/restrictions_summary.csv"))
# ----------------- restrictions timetable formatting ------------------------------------------------------------------------
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
 # ----------------- activity processing -------------------------------------------------------------------------------------------
  activity = read.csv(url("https://data.london.gov.uk/download/google-mobility-by-borough/26d5821b-fcb6-4aae-af73-ee0596942d16/google_activity_by_London_Borough.csv"))
  names(activity) <-
    sub("_percent_change_from_baseline", "", names(activity)) # remove annoying postfix
  
  # transform data, so that is can be better facet_wraped; move columns describing industry branches into one attribute called "Description"
  # and its value in "value" column
  data_long <- activity %>%
    gather(Description, value, retail_and_recreation:residential)
  
  restrictions = read.csv(url("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/03073b4a-2f5d-4a0a-be90-4fe3d9f609c9/restrictions_summary.csv"))
  
  activity$date <- as.character(activity$date)
  restrictions$date <- as.character(restrictions$date)
  
  data_long2 <-
    left_join(data_long, restrictions, by = c("date" = "date"))
  data_long2 <- data_long2 %>% 
    fill(restriction:eat_out_to_help_out) 
  
  data_long2$restriction <- as.character(data_long2$restriction)
  data_long2$restriction <- replace_na(data_long2$restriction, "None")
  
  data_long2$source <- as.character(data_long2$source)
  data_long2$source <- replace_na(data_long2$source, "None")
  
  data_long2[, 8:17][is.na(data_long2[, 8:17])] <- 0
  
  data_long2 <- data_long2%>% 
    mutate(restriction = fct_relevel(
      restriction,
      "None",
      "Work from home advised",
      "Pubs and hospitality close",
      "Schools close",
      "Lockdown 1",
      "Stay alert",
      "Schools reopen",
      "Shops reopen",
      "Hospitality reopens",
      "Covid secure workplaces introduced",
      "Eat out to help out",
      "End of eat out to help out",
      "Rule of 6",
      "WFH encouraged again",
      "Curfew",
      "Tier 1",
      "Tier 2",
      "Lockdown 2",
      "Tier 3",
      "Tier 4",
      "Lockdown 3",
      "Roadmap out of lockdown: Step 1",
      "Roadmap out of lockdown: Step 2",
      "Roadmap out of lockdown: Step 3"
    )) 
  
  # target <- reactive({
  #   input$checkbox1
  # })
  # fig <- plot_ly(data = (data_long2 %>% filter(Description %in% target)), type = "box", x = ~restriction, y = ~value, color = ~Description)
  
  
 # --------------------- output rendering -------------------------------------------------------------------------------------------
  output$funnyBoxPlots = renderPlotly(plot_ly(data = (data_long2 %>% filter(Description %in% input$checkbox1)), type = "box", x = ~restriction, y = ~value, color = ~Description))
  output$table = renderDataTable({restrictionsTable})
  
})

