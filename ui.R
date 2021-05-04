library(shiny)
library(shinydashboard)

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
            h3("test table"),
            dataTableOutput("animals")),
    tabItem(tabName = "2",
            h3("Add visualization")),
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




