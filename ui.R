library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggpubr)
library(forcats)
library(ggridges)
library(shinyWidgets)

dbHeader <- dashboardHeader(title = "London")
dbHeader$children[[2]]$children <-  tags$a(href='https://www.put.poznan.pl/',
                                           tags$img(src='PP_znak_konturowy_RGB.png',height='100%', float = "left"))

# https://fontawesome.com/icons?d=gallery&p=2&q=chart 
# TODO: go here and find cool chart icons instead of "th" below
dashboardPage(title = 'COVID in London', skin = "black",
  dbHeader,
  # dashboardHeader(title = "London"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main page", tabName = "start", icon = icon("dashboard")),
      menuItem("Change in activity", tabName = "1", icon = icon("head-side-mask")),
      menuItem("Cases & deaths", tabName = "2", icon = icon("chart-pie")),
      menuItem("Vaccinations", tabName = "3", icon = icon("syringe")),
      menuItem("Viz4", tabName = "4", icon = icon("menorah"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "start",
              mainPanel(
              h1("London during COVID-19"),
              h4("Anna Przybyłowska, 145447 & Witold Taisner, 145459"),
              p("We created a dashboard describing the pandemic's influence on London. \n The source code and data sources can be found in our ",
                a(href = "https://github.com/Witas1703/interactive-dashboards-London", "repository")),
              p("TODO: add more text, about section ? ")
                )
              ),
      tabItem(tabName = "1", 
              h3("% change in activities"),
              selectInput("checkbox1", "Description",
                          choices = c("Residential" = "residential",
                                      "Retail and recreation" = "retail_and_recreation",
                                      "Gorcery and pharmacy" = "grocery_and_pharmacy",
                                      "Parks" = "parks",
                                      "Transit stations" = "transit_stations",
                                      "Workplaces" = "workplaces"), selected = "residential",
                          multiple = TRUE),
              plotlyOutput("funnyBoxPlots"),
              dataTableOutput("table")),
      tabItem(tabName = "2",
              h3("Deaths & cases in the UK and London"),
              fluidRow(
                box(width = 12,
                    uiOutput("slider"),
                    plotlyOutput("pie")
                )
              ),
              fluidRow(
                uiOutput("plot")
              )),
      tabItem(tabName = "3",
              h3("Vaccination percantege per 1000 citizens"),
              fluidRow(
                box(width = 12,
                    uiOutput("selectBorough"),
                    uiOutput("selectAgeGroup")
                    )
                 ),
              fluidRow(
                plotOutput("wafflePlot")
              )
              ),
      tabItem(tabName = "4",
              h3("Add visualization"))
    )
  )
)
