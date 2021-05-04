library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# Animals ------------------------------------------------------------------------------------------------------------------------------------
animals = read.csv("data/animal_rescue_incidents_LFB.csv")
animals = as.data.frame(sapply(animals, tolower)) # change all records into lowercase (there are same words written in CAPITAL and lowercase)

animals = select(animals, -c("IncidentNumber", "FinYear", "TypeOfIncident")) # drop useless columns

animals <- animals[!grepl("unknown ", animals$AnimalGroupParent),] # removing records with unknown animal type

ggplot(animals, aes(AnimalGroupParent, Borough)) +
  geom_count()

# Activity -------------------------------------------------------------------------------------------------------------------------------------
activity = read.csv("data/google_activity_by_London_Borough.csv")
names(activity) <- sub("_percent_change_from_baseline", "", names(activity)) # remove annoying postfix 

# transform data, so that is can be better facet_wraped :)
data_long <- activity %>% 
  gather(Description, value, retail_and_recreation:residential) 

# w tym wykresie tak: użytkownik wybiera dzielnicę, pojawia się jeden z tych cudownych wykresików ze wszystkimi danymi 
# (może zamiast geom_bar dać linie, nie wiem)
ggplot(data_long , aes(x = date, y = value, fill = Description), xlab = "") + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  labs(title="As") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
  facet_wrap(~area_name)
