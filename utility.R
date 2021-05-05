library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)

# Animals ------------------------------------------------------------------------------------------------------------------------------------
animals = read.csv("data/animal_rescue_incidents_LFB.csv")
animals = as.data.frame(sapply(animals, tolower)) # change all records into lowercase (there are same words written in CAPITAL and lowercase)

animals = select(animals,-c("IncidentNumber", "FinYear", "TypeOfIncident")) # drop useless columns

animals <-
  animals[!grepl("unknown ", animals$AnimalGroupParent), ] # removing records with unknown animal type

# tutaj znowu wybierana dzielnica/typ zwierzęcia ? w sumie losowy wykres z mojej strony
ggplot(
  animals %>% filter(Borough == "westminster"),
  aes(AnimalGroupParent, OriginofCall, color = AnimalGroupParent)
) +
  geom_count() +
  facet_wrap( ~ SpecialServiceTypeCategory) +
  theme_classic()

# Activity -------------------------------------------------------------------------------------------------------------------------------------
activity = read.csv("data/google_activity_by_London_Borough.csv")
names(activity) <-
  sub("_percent_change_from_baseline", "", names(activity)) # remove annoying postfix

# transform data, so that is can be better facet_wraped; move columns describing industry branches into one attribute called "Description"
# and its value in "value" column
data_long <- activity %>%
  gather(Description, value, retail_and_recreation:residential)

# w tym wykresie tak: użytkownik wybiera dzielnicę, pojawia się jeden z tych cudownych wykresików ze wszystkimi danymi
# (może zamiast geom_bar dać linie, nie wiem)
# no i tutaj myślałem, żeby na osi z datą zmienić milion dat na te dane kiedy było co wprowadzone,
# czyli np. zamiast 10.10.2020 dajemy tam Lockdown 1
# ggplot(
#   data_long %>% filter(area_name == 'Westminster'),
#   aes(x = date, y = value, fill = Description),
#   xlab = ""
# ) +
#   geom_bar(stat = "identity",
#            width = .5,
#            position = "dodge") +
#   labs(title = "Change") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(breaks = levels(data_long$date)[c(T, rep(F, 15))])


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

restrictions = read.csv("data/restrictions_timeseries/restrictions_summary.csv")
restrictions <- select(restrictions,-c("source"))


activity$date <- as.character(activity$date)
restrictions$date <- as.character(restrictions$date)
#df <- left_join(activity, restrictions, by = c("date" = "date"))
#df <- df %>% fill(restriction)



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

# timeseries + activities on one plot !!!!
# czy zrobić coś z osią Y, gdzie jest dużo napisów?
# tutaj w założeniu wybiera się dzielnicę
ggplot(
  data_long2 %>% filter(area_name == 'Westminster'),
  aes(x = value, y = restriction, fill = Description)
) +
  geom_density_ridges(alpha = 0.7) +
  labs(title = "Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Description) 
