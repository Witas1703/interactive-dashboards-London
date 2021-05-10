library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)
library(readxl)
library(httr) # reading xlsx from URL
library(plotly)
# Animals ------------------------------------------------------------------------------------------------------------------------------------
animals = read.csv(
  url("https://data.london.gov.uk/download/animal-rescue-incidents-attended-by-lfb/8a7d91c2-9aec-4bde-937a-3998f4717cd8/Animal%20Rescue%20incidents%20attended%20by%20LFB%20from%20Jan%202009.csv"),
  header = TRUE,
  na.strings = c("NULL")
)
animals = as.data.frame(sapply(animals, tolower)) # change all records into lowercase (there are same words written in CAPITAL and lowercase)
animals = select(animals, -c("IncidentNumber", "FinYear", "TypeOfIncident")) # drop useless columns
animals <-
  animals[!grepl("unknown ", animals$AnimalGroupParent),] # removing records with unknown animal type




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
data_long2 <- data_long2 %>% fill(restriction)
data_long2 <- data_long2 %>% mutate(
  restriction = fct_relevel(
    restriction,
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
  )
)


# TODO: change X axis so that it is more readable
# TODO: change labels
# TODO: change values to %, and show negative (?)
# Assumption is that a user can choose a borough, for which he or she will see the change
# of course, mouseover should show exact value and so on

# can be shown with the table of restrictions timeseries
p <- ggplot(
  data_long2 %>% filter(area_name == 'Westminster'),
  aes(x = restriction, y = value, fill = Description)) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7) +
  labs(title = "Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Description)
ggplotly(p)

plot_ly(data = (data_long2 %>% filter(area_name == "Westminster")), type = "bar", x = ~restriction, y = ~value, color = ~Description)

data_long2 %>% 
  filter(area_name == "Westminster") %>%
  group_by(Description) %>%
  do(p = plot_ly(., x = ~restriction, y = ~value, color = ~Description, type = "bar", colors = "Dark2")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)


# Crime -------------------------------------------------------------------------------------------------------------------------------
GET("https://data.london.gov.uk/download/use-of-force/9d266ef1-7376-4eec-bb0d-dfbd2b1a591e/MPS%20Use%20of%20Force%20-%20FY20-21.xlsx", write_disk(tf1 <- tempfile(fileext = ".xlsx")))
crime21 = read_excel(tf1, na = "NA")
GET("https://data.london.gov.uk/download/use-of-force/2aa0d839-add7-46c1-a168-e62d33323228/MPS%20Use%20of%20Force%20-%20FY19-20.xlsx", write_disk(tf2 <- tempfile(fileext = ".xlsx")))
crime20 = read_excel(tf2, na = "NA")
GET("https://data.london.gov.uk/download/use-of-force/727e768a-a8fe-4c06-bfa3-ac61930bfa78/MPS%20Use%20of%20Force%20-%20FY18-19.xlsx", write_disk(tf3 <- tempfile(fileext = ".xlsx")))
crime19 = read_excel(tf3, na = "NA")

crime21 <-crime21[-c(59:271)] # basically dropping plenty of rather worthless and uninteresting columns, please forgive me Lord Morzy
crime20 <- crime20[-c(59:271)]
crime19 <- crime19[-c(59:269)]

crime = bind_rows(crime21, crime20, crime19) # ah yes, the final database to visualize


names(crime) <-
  sub("Incident Location: ", "", names(crime)) # remove annoying prefixes
names(crime) <-
  sub("Impact Factor: ", "Cause ", names(crime)) # change prefix
names(crime) <-
  sub("Reason for Force: ", "Reason for force ", names(crime)) # change prefix
names(crime) <-
  gsub(" ", "_", names(crime)) # substitute " " with "_"
names(crime) <- tolower(names(crime))

# below code: transform crime dataframe so that it has column 'place' with values, i.e street/highway, then removes rows, which do not
# correspond to place. It can be done safely, as these variables are one-hot encoded (only one place for each intervention)
crime_long <- crime %>%
  gather(place, value, 'street/highway':other)
crime_long <- crime_long[!(crime_long$value == "No"), ]
crime <-
  select(crime_long,-c('value')) # ogólnie to nie trwa tak długo (może moje R miało jakiś problem za pierwszym razem XD),
# więc można to chyba zostawić - bo ta połączona csv nie mieści się do gita
crime <- select(crime,-c('threatenedwithweapon')) # too many NA's
# no ogólnie na razie nie umiem zmienić tylko kolumn Yes/No, także roboczo zmieniłem tylko część xD

crime %>%
  mutate(effective_1 = ifelse(effective_1 == "Yes", 1, 0)) %>%
  mutate(effective_2 = ifelse(effective_2 == "Yes", 1, 0)) %>%
  mutate(effective_3 = ifelse(effective_3 == "Yes", 1, 0)) %>%
  mutate(effective_4 = ifelse(effective_4 == "Yes", 1, 0)) %>%
  mutate(effective_5 = ifelse(effective_5 == "Yes", 1, 0)) %>% 
  mutate(assaultedbysubject = ifelse(assaultedbysubject == "Yes", 1, 0))

crime$incidentdate <- as.Date(crime$incidentdate)
crime$year <- format(crime$incidentdate, "%Y") 


ggplot(crime %>% filter(borough == "Bexley"), aes(y = mainduty, x = primaryconduct, color = assaultedbysubject)) +
  geom_count() + 
  theme_classic() + 
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(crime %>% filter(tactic_1 == "Non-compliant handcuffing"), aes(x = place, y = primaryconduct, color = effective_1)) +
  geom_jitter() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~year)























