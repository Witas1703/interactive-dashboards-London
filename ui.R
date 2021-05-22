library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)
library(rgdal)
library(sp)
library(rgeos)
library(shinydashboard)
library(inlmisc)
library(shinyWidgets)
library(RColorBrewer)
library(rcartocolor)
library(reactable)
library(htmltools)

# https://fontawesome.com/icons?d=gallery&p=2&q=chart 
# TODO: go here and find cool chart icons instead of "th" below
dashboardPage(
  dashboardHeader(title = "London"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main page", tabName = "start", icon = icon("dashboard")),
      menuItem("Viz1", tabName = "1", icon = icon("th")),
      menuItem("Viz2", tabName = "2", icon = icon("th")),
      menuItem("Viz3", tabName = "3", icon = icon("th")),
      menuItem("Viz4", tabName = "4", icon = icon("th")),
      menuItem("Viz5", tabName = "5", icon = icon("th")),
      menuItem("Viz6", tabName = "6", icon = icon("th"))
    )
  ),
  dashboardBody(
  tabItems(
    tabItem(tabName = "start",
            h3("Add random text or something idk")),
    tabItem(tabName = "1", 
            h3("TODO: add title here"),
            selectInput("checkbox1", "Description",
                               choices = c("Residential" = "residential",
                                 "Retail and recreation" = "retail_and_recreation",
                                 "Gorcery and pharmacy" = "grocery_and_pharmacy",
                                 "Parks" = "parks",
                                 "Transit stations" = "transit_stations",
                                 "Workplaces" = "workplaces"), selected = "residental",
                        multiple = TRUE),
            plotlyOutput("funnyBoxPlots"),
            dataTableOutput("table")),
    tabItem(tabName = "2",
            h3("plz work"),
            fluidRow(
              box(width = 12,
                  sliderTextInput("month", 
                                  "Select date:",
                                  choices = c("2020-02" = "2020-02", "2020-03" = "2020-03",  
                                              "2020-04" = "2020-04", "2020-05" = "2020-05",
                                              "2020-06" = "2020-06", "2020-07" = "2020-07",
                                              "2020-08" = "2020-08", "2020-09" = "2020-09",
                                              "2020-10" = "2020-10", "2020-11" = "2020-11",
                                              "2020-12" = "2020-12", "2021-01" = "2021-01",
                                              "2021-02" = "2021-02", "2021-03" = "2021-03", 
                                              "2021-04" = "2021-04", "2021-05" = "2021-05"),
                                  selected = "2020-02",
                                  # choices = months,
                                  # selected = months[1],
                                  animate = T),
                  plotlyOutput("pie")
              )
            ),
            fluidRow(
              uiOutput("plot")
            )),
    tabItem(tabName = "3",
            h3("Add visualization")),
    tabItem(tabName = "4",
            h3("Add visualization")),
    tabItem(tabName = "5",
            h3("Add visualization")),
    tabItem(tabName = "6",
            h3("Add visualization"))
    )
  )
)




