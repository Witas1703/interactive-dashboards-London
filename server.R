library(shiny)
library(dplyr)

shinyServer(function(input, output) {
  animalsRescue = read.csv("data/animal_rescue_incidents_LFB.csv")
  animalsRescue %>% select(- one_of("IncidentNumber"))
   output$animals = renderDataTable({animalsRescue})

})
