library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  animalsRescue = read.csv("data/animal_rescue_incidents_LFB.csv")
  output$animals = renderDataTable({animalsRescue})
  
})
