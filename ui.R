library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "London"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main page", tabName = "1", icon = icon("dashboard")),
      menuItem("Table", tabName = "2", icon = icon("th")),
      menuItem("Viz2", tabName = "3", icon = icon("th"))
    )
  ),
  dashboardBody(
  tabItems(
    tabItem(tabName = "1",
            h2("trelemorele")),
    tabItem(tabName = "2", 
            h2('adam grzenda'),
            dataTableOutput("animals")),
    tabItem(tabName = "3", 
            h1("CZEŚĆ ANIA"))
    )
  )
)
