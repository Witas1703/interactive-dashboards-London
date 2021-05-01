library(shiny)

shinyUI(navbarPage("London",
    tabPanel("Viz1",
             mainPanel(
                dataTableOutput("animals")
                )
             ),
    tabPanel("Viz2")
    ))
