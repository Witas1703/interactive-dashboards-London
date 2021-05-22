library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)
library(readxl)
library(httr) # reading xlsx from URL
library(plotly)
library(ggridges)
library(waffle)

# Activity -------------------------------------------------------------------------------------------------------------------------------------
activity = read.csv(url("https://data.london.gov.uk/download/google-mobility-by-borough/26d5821b-fcb6-4aae-af73-ee0596942d16/google_activity_by_London_Borough.csv"))
names(activity) <-
  sub("_percent_change_from_baseline", "", names(activity)) # remove annoying postfix

# transform data, so that is can be better facet_wraped; move columns describing industry branches into one attribute called "Description"
# and its value in "value" column
data_long <- activity %>%
  gather(Description, value, retail_and_recreation:residential)

# XD
ggplot(activity, aes(date)) +
  geom_line(aes(y = retail_and_recreation), color = "blue") +
  scale_x_discrete(breaks = levels(activity$date)[c(T, rep(F, 9))]) +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 0.5,
    hjust = 1
  )) +
  labs(x = "", y = "change [%]", title = "retail and recration")

# Timeseries -----------------------------------------------------------------------------------------------------------------

restrictions = read.csv(url("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/03073b4a-2f5d-4a0a-be90-4fe3d9f609c9/restrictions_summary.csv"))

# already done in server.R
# restrictionsTable <- restrictions %>%
#   mutate(across("schools_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
#   mutate(across("pubs_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
#   mutate(across("shops_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
#   mutate(across("eating_places_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
#   mutate(across("stay_at_home", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
#   mutate(across("household_mixing_indoors_banned", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
#   mutate(across("wfh", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
#   mutate(across("rule_of_6_indoors", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
#   mutate(across("curfew", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
#   mutate(across("eat_out_to_help_out", ~factor(., levels=c(0,1), labels=c("no","yes"))))

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


# Assumption is that a user can choose a borough, for which he or she will see the change
# can be shown with the table of restrictions timeseries
unique(data_long2$Description)

target <- c("transit_stations", "residential")
plot_ly(data = (data_long2 %>% filter(area_name == "Westminster", Description %in% target, )), type = "box", x = ~restriction, y = ~value, color = ~Description)


# COVID -------------------------------------------------------------------------------------------------------------------

# cumulative_vaccines_url <- url("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=cumPeopleVaccinatedCompleteByVaccinationDate&format=csv")
# cumulative_vaccines <- read.csv(cumulative_vaccines_url, stringsAsFactors = FALSE)
# 
# daily_fully_vaccinated_url <- url("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newPeopleVaccinatedCompleteByVaccinationDate&format=csv")
# daily_fully_vaccinated <- read.csv(daily_fully_vaccinated_url, stringsAsFactors = FALSE)
# 
# vaccines <- bind_cols(daily_fully_vaccinated[4:5], cumulative_vaccines[5])
# names(vaccines) <- c("date", "newPeopleCompletlyVaccinated", "cumulativeCompleteVaccinations")
# 
# cases_url <- url("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDateAgeDemographics&format=csv")
# cases <- read.csv(cases_url, stringsAsFactors = FALSE)
# 
# cases <- cases[4:8]
# cases <- cases %>% filter(age != "unassigned" & age != "60+" & age != "00_59") 
# 
# for (i in 1:length(cases$age)) {
#   age = cases$age[i]
#   if(age == "00_04" | age == "05_09"){
#     cases$age[i] = "0-9"
#   } else if (age == "10_14" | age == "15_19"){
#     cases$age[i] = "10-19"
#   } else if (age == "20_24" | age == "25_29"){
#     cases$age[i] = "20-29"
#   } else if (age == "30_34" | age == "35_39"){
#     cases$age[i] = "30-39"
#   } else if (age == "40_44" | age == "45_49"){
#     cases$age[i] = "40-49"
#   } else if (age == "50_54" | age == "55_59"){
#     cases$age[i] = "50-59"
#   } else if (age == "60_64" | age == "65_69"){
#     cases$age[i] = "60-69"
#   } else {
#     cases$age[i] = "69+"
#   }
# }
# 
# combined <- left_join(cases, vaccines, by = 'date') %>% 
#   mutate_at(c(6:7), ~replace(., is.na(.), 0)) 
# 
# plt1 <- ggplot(combined, aes(fill = age, x = date, y = cases)) +
#   geom_bar(position = "stack", stat = "identity") 

  
# -----------------------------------------------------------------------------------------------------------------
url <- "https://data.london.gov.uk/download/coronavirus--covid-19--cases/50b79988-1c39-4283-b68e-a126afb6fcbf/nhse_weekly_vaccines_london_ltla.csv"
vaccines <- read.csv(url, stringsAsFactors = FALSE)  

vaccines <- vaccines %>% select(-one_of("ltla_code", "percent_vaccine"))

tmp <- vaccines %>% group_by(ltla_name) %>% summarise(last_date = max(end_date))

# filtering so that only latest result for each age group and borough are left
latestVaccines <- vaccines %>% left_join(tmp, by = "ltla_name") %>% filter(end_date >= last_date) %>% select(-one_of("start_date", "last_date", "end_date"))
# latestVaccines$percent <- latestVaccines$vaccines / latestVaccines$population

# w shiny po prostu będzie to wybierał użytkownik, tak to ja po prostu hard koduję, żeby łatwiej sprawdzać
helper <- latestVaccines %>% filter(ltla_name == "Camden" & age_band == "45-49") %>% select(-one_of("ltla_name", "age_band"))

waffle(c(`1st dose` = helper$vaccines[1] - helper$vaccines[2], `2nd dose` = helper$vaccines[2], `unvaccinated` = helper$population[1] - helper$vaccines[1])/100)

