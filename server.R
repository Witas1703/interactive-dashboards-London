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
library(inlmisc)
library(RColorBrewer)
library(rcartocolor)
library(reactable)
library(htmltools)
library(shinyWidgets)
library(waffle)



shinyServer(function(input, output) {
  restrictionsTable = read.csv(url("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/03073b4a-2f5d-4a0a-be90-4fe3d9f609c9/restrictions_summary.csv"))
  # ----------------- restrictions timetable formatting ------------------------------------------------------------------------
  restrictionsTable <- restrictionsTable %>% 
    mutate(across("schools_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("pubs_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("shops_closed", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("eating_places_closed", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("stay_at_home", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("household_mixing_indoors_banned", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("wfh", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("rule_of_6_indoors", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>%
    mutate(across("curfew", ~factor(., levels=c(0,1), labels=c("no", "yes")))) %>%
    mutate(across("eat_out_to_help_out", ~factor(., levels=c(0,1), labels=c("no","yes")))) %>% 
    rename(
      "schools closed" = schools_closed,
      "pubs closed" = pubs_closed,
      "shops closed" = shops_closed,
      "eating_places_closed" = eating_places_closed,
      "stay at home" = stay_at_home,
      "household mixing indoors banned" = household_mixing_indoors_banned,
      "work from home" = wfh,
      "rule o 6 indoors" = rule_of_6_indoors, 
      "eat out to help out" = eat_out_to_help_out
    )
  # ----------------- activity processing -------------------------------------------------------------------------------------------
  activity = read.csv(url("https://data.london.gov.uk/download/google-mobility-by-borough/26d5821b-fcb6-4aae-af73-ee0596942d16/google_activity_by_London_Borough.csv"))
  names(activity) <-
    sub("_percent_change_from_baseline", "", names(activity)) # remove annoying postfix
  
  # transform data, so that is can be better facet_wraped; move columns describing industry branches into one attribute called "Description"
  # and its value in "value" column
  data_long <- activity %>%
    gather(Description, value, retail_and_recreation:residential)
  
  restrictions = read.csv(url("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/03073b4a-2f5d-4a0a-be90-4fe3d9f609c9/restrictions_summary.csv"))
  
  activity$date <- as.character(activity$date)
  restrictions$date <- as.character(restrictions$date)
  
  data_long2 <-
    left_join(data_long, restrictions, by = c("date" = "date"))
  data_long2 <- data_long2 %>% 
    fill(restriction:eat_out_to_help_out) 
  
  data_long2$restriction <- as.character(data_long2$restriction)
  data_long2$restriction <- replace_na(data_long2$restriction, "None")
  
  data_long2$source <- as.character(data_long2$source)
  data_long2$source <- replace_na(data_long2$source, "None")
  
  data_long2[, 8:17][is.na(data_long2[, 8:17])] <- 0
  
  data_long2 <- data_long2%>% 
    mutate(restriction = fct_relevel(
      restriction,
      "None",
      "Work from home advised",
      "Pubs and hospitality close",
      "Schools close",
      "Lockdown 1",
      "Stay alert",
      "Schools reopen",
      "Shops reopen",
      "Hospitality reopens",
      "Covid secure workplaces introduced",
      "Eat out to help out",
      "End of eat out to help out",
      "Rule of 6",
      "WFH encouraged again",
      "Curfew",
      "Tier 1",
      "Tier 2",
      "Lockdown 2",
      "Tier 3",
      "Tier 4",
      "Lockdown 3",
      "Roadmap out of lockdown: Step 1",
      "Roadmap out of lockdown: Step 2",
      "Roadmap out of lockdown: Step 3"
    )) 
  
  # -------------------------------- vaccinations ---------------------------------------------------------------------------------------------------------------------------------
  url <- "https://data.london.gov.uk/download/coronavirus--covid-19--cases/50b79988-1c39-4283-b68e-a126afb6fcbf/nhse_weekly_vaccines_london_ltla.csv"
  vaccines <- read.csv(url, stringsAsFactors = FALSE)  
  
  vaccines <- vaccines %>% select(-one_of("ltla_code", "percent_vaccine"))
  
  tmp <- vaccines %>% group_by(ltla_name) %>% summarise(last_date = max(end_date))
  
  # filtering so that only latest result for each age group and borough are left
  latestVaccines <- vaccines %>% left_join(tmp, by = "ltla_name") %>% filter(end_date >= last_date) %>% select(-one_of("start_date", "last_date", "end_date"))
  
  output$selectBorough <- renderUI({
    selectInput("boroughInput", "Choose borough: ",choices = unique(latestVaccines$ltla_name), selected = latestVaccines$ltla_name[1], multiple = FALSE)
  })
  
  output$selectAgeGroup <- renderUI({
    selectInput("ageInput", "Choose age band: ", choices = unique(latestVaccines$age_band), selected = "Under 40", multiple = FALSE)
  })
 
  
  # --------------------- output rendering -------------------------------------------------------------------------------------------
  output$funnyBoxPlots = renderPlotly(plot_ly(data = (data_long2 %>% filter(Description %in% input$checkbox1)), type = "box", x = ~restriction, y = ~value, color = ~Description))
  output$table = renderDataTable({restrictionsTable})
  output$wafflePlot = renderPlot({
    helper <- latestVaccines %>% filter(ltla_name == input$boroughInput & age_band == input$ageInput) %>% select(-one_of("ltla_name", "age_band"))
    waffle(c(`1st dose` = helper$vaccines[1] - helper$vaccines[2], `2nd dose` = helper$vaccines[2], `unvaccinated` = helper$population[1] - helper$vaccines[1])/(1000))
  })
  
  #-------------------- maps data -------------------------------------------------------------------------------------------------
  covid.london_england = read.csv(url("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumCasesBySpecimenDate&format=csv"))
  covid.london_england = covid.london_england %>% as_tibble() %>% rename(area_name = areaName) %>% 
    rename(cases = cumCasesBySpecimenDate) %>% select(area_name, date, cases) 
  covid.london_england = mutate(covid.london_england, month = substring(date,1,7)) %>% select(-date)
  covid.london_england = covid.london_england %>% group_by(area_name, month) %>%
    summarize_each(funs(max))
  
  covid.comparison = covid.london_england
  covid.comparison$area_name <- as.character(covid.comparison$area_name)
  covid.comparison$area_name[covid.comparison$area_name != "London"] = "Rest of England"
  covid.comparison = covid.comparison %>% group_by(area_name, month) %>%
    summarize_each(funs(sum)) %>% filter(month != "2020-01")
  
  
  covid.cases = read.csv(url("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv"))
  covid.cases = mutate(covid.cases, month = substring(date,1,7)) %>% select(-c("date", "new_cases"))
  covid.cases = covid.cases %>% group_by(month, area_name, area_code) %>% 
    summarize_each(funs(max)) 
  
  
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
  borough_and_id$area_name <- as.character(borough_and_id$area_name)
  borough_and_id$area_name[borough_and_id$area_name == "Hackney"] = "Hackney and City of London"
  
  ldn_f = fortify(ldn) %>% filter (id != 32)
  
  
  covid.cases = left_join(covid.cases, borough_and_id, by = "area_name")
  
  ldn_f = left_join(ldn_f, covid.cases, by="id")
  
  
  
  
  england = readOGR("data/covid/england", layer = "eng")
  england_f = fortify(england)
  
  region_and_id = england@data %>% as_tibble() %>% mutate(id = as.character(row_number()-1)) %>%
    select(c("id", "RGN20NM")) %>% rename(area_name = RGN20NM)
  
  covid.eng = covid.london_england
  covid.eng = left_join(covid.eng, region_and_id, by = "area_name")
  england_f = left_join(england_f, covid.eng, by="id") %>% rename(total_cases = cases)
  
  months = unique(ldn_f$month)[unique(ldn_f$month) %in% unique(england_f$month)]
  months = months[months %in% unique(covid.comparison$month)]
  
  down_ldn = min(ldn_f$total_cases)
  up_ldn = max(ldn_f$total_cases)
  down_eng = 0
  up_eng = max(england_f$total_cases)
  
  ldn_f = ldn_f  %>% spread(month,total_cases)
  england_f = england_f %>% spread(month, total_cases)
  covid.comparison = covid.comparison %>% spread(month, cases)
  
  ldn_f[is.na(ldn_f)] = 0
  england_f[is.na(england_f)] = 0
  covid.comparison[is.na(covid.comparison)] = 0
  
  england_f = england_f %>% filter(area_name != "London")
  
  covid.deaths = read.csv(url("https://data.london.gov.uk/download/coronavirus--covid-19--deaths/aa17aaa1-2b9e-4e60-a0ee-5d8bbac31486/nhse_total_deaths_by_region.csv"))
  covid.deaths = mutate(covid.deaths, month = substring(date,1,7))
  covid.deaths = covid.deaths %>% rename( area_name = nhs_england_region) %>%
    rename(`total deaths` = cumulative_deaths_total) %>% rename(`deaths with positive test` = cumulative_deaths_with_positive_test) %>%
    rename(`deaths without positive test` = cumulative_deaths_without_positive_test)
  covid.deaths = covid.deaths %>% select(c("area_name","month", "total deaths", "deaths with positive test","deaths without positive test"))
  covid.deaths = covid.deaths %>% group_by(area_name, month) %>%
    summarize_each(funs(max))
  covid.deaths$area_name[covid.deaths$area_name != "London"] = "Rest of England"
  covid.deaths = covid.deaths %>% group_by(area_name, month) %>%
    summarize_each(funs(sum))
  covid.deaths_london = covid.deaths %>% filter(area_name == "London") %>% ungroup() %>% select(-area_name)
  covid.deaths_rest = covid.deaths %>% filter(area_name =="Rest of England") %>% ungroup() %>% select(-area_name)
  data.deaths_london = covid.deaths_london %>% select(c("month", "total deaths"))
  data.deaths_rest = covid.deaths_rest %>% select(c("month", "total deaths"))
  
  #--------------------maps outputs----------------------------------------------------------------------------------------------
  
  output$slider <- renderUI({
    sliderTextInput("month", 
                    "Select date:", 
                    choices = months,
                    selected = months[1],
                    animate = T)
  })
  
  output$pie <- renderPlotly({
    g = plot_ly(covid.comparison, source = "pie", labels = ~area_name, 
                values = covid.comparison[[toString(input$month)]], type = "pie",
                textposition = 'inside',
                textinfo = 'label',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = c("#217a79","#9c3f5d"),
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = F)
    g %>% event_register("plotly_click")
    
    
  })
  
  output$plot <- renderUI({
    
    d = event_data("plotly_click", source = "pie")$pointNumber
    if(!is.null(d)){
      if(d == 0){
        output$map <- renderPlotly({
          map = ggplot(ldn_f, 
                       aes(customdata = area_name, group = group, x = long, y = lat, fill = .data[[input$date]],
                       )) +
            geom_polygon(color = "black") +
            scale_fill_gradientn(colors = colorRampPalette(carto_pal(name = "Emrld"))(50),
                                 limits = c(down_ldn,up_ldn)) +
            coord_equal() + theme_transparent()
          map = ggplotly(map)
          map
        })
        output$dt <- renderReactable({
          table = reactable(data.deaths_london, searchable = T, highlight = T,
                            showSortable = T, compact = T,
                            columns = list(
                              `total deaths` = colDef(details = function(index){
                                data = covid.deaths_london[covid.deaths_london$month == data.deaths_london$month[index],]
                                data = data %>% select(`deaths with positive test`, `deaths without positive test`)
                                reactable(data, outlined = T)
                              })
                            ))
          table
        })
      }
      else{
        output$map <- renderPlotly({
          map = ggplot(england_f, 
                       aes(customdata = area_name, group = group, x = long, y = lat, fill = .data[[input$date]],
                       )) +
            geom_polygon(color = "black") +
            scale_fill_gradientn(colors = colorRampPalette(carto_pal(name = "BurgYl"))(50),
                                 limits = c(down_eng,up_eng)) +
            coord_equal() + theme_transparent() 
          
          map = ggplotly(map)
          map
          
        })
        output$dt <- renderReactable({
          table = reactable(data.deaths_rest, searchable = T, highlight = T,
                            showSortable = T, compact = T,
                            columns = list(
                              `total deaths` = colDef(details = function(index){
                                data = covid.deaths_rest[covid.deaths_rest$month == data.deaths_rest$month[index],]
                                data = data %>% select(`deaths with positive test`, `deaths without positive test`)
                                reactable(data, outlined = T)
                              })
                            ))
          table
        })
      }
      box(width = 12, 
          sliderTextInput("date", 
                          "Select date:", 
                          choices = months,
                          selected = months[1],
                          animate = T),
          plotlyOutput("map"),
          reactableOutput("dt")
      )
      
    }
    
    
    
  })
  
})