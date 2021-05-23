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

# https://fontawesome.com/icons?d=gallery&p=2&q=chart 

dbHeader <- dashboardHeader(title = "London")
dbHeader$children[[2]]$children <-  tags$a(href='https://www.put.poznan.pl/',
                                           tags$img(src='PP_znak_konturowy_RGB.png',height='100%', float = "left"))


dashboardPage(title = 'COVID in London', skin = "black",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main page", tabName = "start", icon = icon("dashboard")),
      menuItem("Change in activity", tabName = "1", icon = icon("head-side-mask")),
      menuItem("Cases & deaths", tabName = "2", icon = icon("chart-pie")),
      menuItem("Vaccinations", tabName = "3", icon = icon("syringe")),
      menuItem("COVID-19 symptoms checker", tabName = "4", icon = icon("poll-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "start",
              mainPanel(
              h1("London during COVID-19"),
              h5("Anna Przybyłowska, 145447 & Witold Taisner, 145459"),
              p("We created a dashboard describing the pandemic's influence on London. The source code and data sources can be found in our ",
                a(href = "https://github.com/Witas1703/interactive-dashboards-London", "repository.")),
              hr(),
              h4("About"),
              p("The project itself is a part of our data visualization course at Poznań University of Technology. You can jump directly into universitiy's webpage by clicking the logo in the upper-right corner."),
              br(),
              p("We decided to pick a data, that allow the user to see how COVID-19 hit London, one of the biggest cities in the world. Using our interactive widgets, one can see changes & current trends in vaccinations, cases, deaths and so on."),
              br(),
              p("In addition, we decided to create a basic classification model, for a very rough COVID-19 symptoms checker. We used gini impurity measure to select splits when performing classification. If you're unfamiliar, please refer to ", 
                a(href = "https://www.gormanalysis.com/blog/magic-behind-constructing-a-decision-tree/", "this article.")),
              hr(),
              h4("General information"),
              p("Most of our visualizations are interactive. Try clicking, mousovering and dive deeper into the data!")
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
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                plotOutput("wafflePlot")
              ),
              fluidRow(
                box(width = 12,
                  textOutput("percents")
                )
              )
              ),
      tabItem(tabName = "4",
              h3("Add visualization"),
              fluidRow(
                box(width = 12,
                    radioButtons("gender", label = "Please specify your gender", choices = c("Male" = "male", "Female" = "female"), inline = TRUE),
                    radioButtons("age", label = "Are you over 60 years old?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE),
                    radioButtons("cough", label = "Do you cough?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE),
                    radioButtons("fever", label = "Do you have fever?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE),
                    radioButtons("soreThroat", label = "Do you experience sore throat?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE),
                    radioButtons("shortnessOfBreath", label = "Do you experience shortness of breath?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE),
                    radioButtons("headAche", label = "Do you experience head ache?", choices = c("Yes" = "Yes", "No" = "No"), inline = TRUE)
                    )
              
                      ),
              fluidRow(
                box(width = 12,
                  h4(textOutput("decision")),
                  hr(),
                  p(em("Please note, that above information is a mere prediction based on a computed model, in order to precisly determine your medical state, please contact medical services.")),
                  hr(),
                  p("Classification tree computed based on our data concerning COVID-19 symptoms, it allows us to rougly estimate, whether someone should test themselves."),
                  HTML("<p>The tree should be read as following:</p> <br>
                       <ul>
                        <li> first row contains decision in the leaf/node (negative/inconclusive/positive outcome of a potential test); </li>
                        <li> second row contains percentage of instances classified by a corresponding leaf/node: i.e 94% from negative, 1% from inconclusive and 4% from positive classes respectively;</li>
                        <li>last row contains percent of data classified by the leaf/node </li>
                       </ul>")
                ),
              ),
              fluidRow(
                plotOutput("decisionTree")
              )
              )
    )
  )
)
