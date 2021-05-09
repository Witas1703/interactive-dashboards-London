library(rgdal)
library(sp)
library(rgeos)
library(plotly)
library(shinydashboard)
library(shiny)
library(inlmisc)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggpubr)


covid.london_england = read.csv(url("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumCasesBySpecimenDate&format=csv"))
covid.london_england = covid.london_england %>% as_tibble() %>% rename(area_name = areaName) %>% 
  rename(cases = cumCasesBySpecimenDate) %>% select(area_name, date, cases) 
covid.london_england = mutate(covid.london_england, month = substring(date,1,7)) %>% select(-date)
covid.london_england = covid.london_england %>% group_by(area_name, month) %>%
  summarize_each(funs(max))

covid.comparison = covid.london_england
covid.comparison$area_name[covid.comparison$area_name != "London"] = "Rest of England"
covid.comparison = covid.comparison %>% group_by(area_name, month) %>%
  summarize_each(funs(sum)) %>% filter(month != "2020-01")


covid.cases = read.csv(url("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv"))
covid.cases = mutate(covid.cases, month = substring(date,1,7)) %>% select(-date)
covid.cases = covid.cases %>% group_by(month, area_name, area_code) %>% 
  summarize_each(funs(sum, max)) %>% select(-c("total_cases_sum", "new_cases_max")) %>%
  rename(total_cases = total_cases_max) %>% rename(new_cases = new_cases_sum)


ldn = readOGR("data/covid/london", layer = "london")
#proj4string(ldn) = CRS("+init=epsg:27700")
#ldn.wgs84 = spTransform(ldn, CRS("+init=epsg:4326"))
tr1 = ldn@polygons[[33]]@Polygons[[1]]@coords 
tr2 = ldn@polygons[[21]]@Polygons[[1]]@coords 
new_c = rbind(tr2[1:23,],tr1[6:11,],tr1[1:3,],tr2[24:26,])
ldn@polygons[[33]]@Polygons[[1]]@coords = new_c
ldn@polygons[[21]]@Polygons[[1]]@coords = new_c

borough_and_id = ldn@data %>% as_tibble() %>% mutate(id = as.character(row_number()-1)) %>%
  select(c("id", "name")) %>% filter(name != "City of London") %>% rename(area_name = name)
borough_and_id[borough_and_id == "Hackney"] = "Hackney and City of London"

ldn_f = fortify(ldn) %>% filter (id != 32)


covid.cases = left_join(covid.cases, borough_and_id, by = "area_name")

ldn_f = left_join(ldn_f, covid.cases, by="id")

map = ggplot(filter(ldn_f, month == "2021-03"), 
             aes(long, lat, group = area_name, fill = total_cases)) +
  geom_polygon(color = "black") +
  coord_equal() + theme_transparent()
map

map  = ggplotly (map)

map


months = unique(ldn_f$month)

england = readOGR("data/covid/england", layer = "eng")
england_f = fortify(england)

region_and_id = england@data %>% as_tibble() %>% mutate(id = as.character(row_number()-1)) %>%
  select(c("id", "RGN20NM")) %>% rename(area_name = RGN20NM)

covid.eng = covid.london_england
covid.eng = left_join(covid.eng, region_and_id, by = "area_name")
england_f = left_join(england_f, covid.eng, by="id") %>% rename(total_cases = cases)

map = ggplot(filter(england_f, month == "2020-11"), 
             aes(long, lat, customdata = area_name, fill = total_cases, group = group)) +
  geom_polygon(color = "black") +
  coord_equal() + theme_transparent()
ggplotly(map)




ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12,
                    sliderTextInput("month", 
                                    "Select date:", 
                                    choices = months,
                                    selected = months[1],
                                    animate = T),
                    plotOutput("pie")
                )
              ),
              fluidRow(
                box(width = 12, 
                  sliderTextInput("date", 
                                  "Select date:", 
                                  choices = months,
                                  selected = months[1],
                                  animate = T),
                  plotlyOutput("map")
                )
              ),
              fluidRow(
                box(width = 12, 
                    sliderTextInput("date_eng", 
                                    "Select date:", 
                                    choices = months,
                                    selected = months[1],
                                    animate = T),
                    plotlyOutput("map_eng")
                )
              )
              
      )
  )))
  
down = min(ldn_f$total_cases)
up = max(ldn_f$total_cases)
down_eng = min(england_f$total_cases)
up_eng = max(england_f$total_cases)
server <- shinyServer(function(input, output) {
  
  output$map <- renderPlotly({
    
    data = filter(ldn_f, month == input$date)
    map = ggplot(data, 
                 aes(customdata = area_name, group = group, x = long, y = lat, fill = total_cases,
                 )) +
      geom_polygon(color = "black") +
      scale_fill_continuous(limits = c(down,up)) +
      coord_equal() + theme_transparent() 
    
    map = ggplotly(map)
    map
    
    
    
  })
  
  output$pie <- renderPlot({
    g = ggplot(filter(covid.comparison, month == input$month), aes(x=area_name, y=cases, fill=area_name)) +
      geom_bar(stat="identity", width=1, color="white") +
      theme_classic()
    g
    
  })
  
  output$map_eng <- renderPlotly({
    data = filter(england_f, month == input$date_eng)
    map = ggplot(data, 
                 aes(customdata = area_name, group = group, x = long, y = lat, fill = total_cases,
                 )) +
      geom_polygon(color = "black") +
      scale_fill_continuous(limits = c(down_eng,up_eng)) +
      coord_equal() + theme_transparent() 
    
    map = ggplotly(map)
    map
    
  })
  
  
  
  
  
})
shinyApp(ui = ui, server = server)