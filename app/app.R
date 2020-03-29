rm(list = ls() )
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)
library(viridis)
library(shiny)
library(plotly)

##############
##############
rsconnect::setAccountInfo(name='janeli',
                          token='F0FEBC6B9F3AC0113FD7FE5FFBA61806',
                          secret='zPLZB09qn4YzvEob335f3Jj8Svf+KkWFmm5cwHMc')

##############
##############

cases <- 
  read_csv('cases-by-neighborhood-2020-03-27.csv') %>%
  filter(date>='2020-03-17')

neighborhood_spatial <- 
  read_sf('la-county-neighborhoods-current/') %>%
  select(name) %>%
  st_transform("+proj=longlat +datum=WGS84")


combined_df <- 
  neighborhood_spatial %>%
  left_join(cases, by=c('name'='neighborhood'))


##############
##############
ui <- fluidPage(
  titlePanel("COVID-19 LA update"),
  fluidRow(column(width = 4, wellPanel(selectInput(inputId = "Date",
                                                   label = "select a date:",
                                                   choices =sort(unique(combined_df$date), decreasing = TRUE)),
                                       h4("Overall Trend in LA County"),
                                       plotlyOutput("timeseries", height="225px"),
                                       h4("Past Trend in selected area"),
                                       plotlyOutput("specific_nbg", height="225px"))),
           column(width = 8, wellPanel(leafletOutput("map",  width="100%",height="600px")))))


##############
##############

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    filter_df<- combined_df %>% filter(date==max(date))
    pal <-colorNumeric(palette = "YlOrRd", filter_df$cases,na.color = "White")
    labels <- sprintf( "<strong>%s:</strong><br/>%g cases",filter_df$name, filter_df$cases) %>% lapply(htmltools::HTML)
    
    
    filter_df%>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(color = "black",weight=1,dashArray = 3, fillColor = ~pal(cases), fillOpacity = 0.8,
                highlight = highlightOptions( fillColor = "Cyan",fillOpacity = 0.8,bringToFront = TRUE),
                layerId = ~name,
                label = labels, 
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
      addLegend(position = "bottomright", pal = pal, values = filter_df$cases, title = "Number of cases", opacity = 0.8) 
  })
  
  
  observeEvent(input$Date,{
    
    filteredData <-  
      combined_df %>%
      filter(date==input$Date)
    
    pal <-colorNumeric(palette = "YlOrRd", filteredData$cases, na.color = "White")
    labels <- sprintf( "<strong>%s:</strong><br/>%g cases",filteredData$name, filteredData$cases) %>% lapply(htmltools::HTML)
    
    leafletProxy("map", data = filteredData) %>% 
      clearShapes() %>%
      clearControls() %>%
      addPolygons( color = "black",smoothFactor = 0.3,weight=1,dashArray = 3, fillColor = ~pal(cases), fillOpacity = 0.8,
        highlight = highlightOptions( fillColor = "Cyan",fillOpacity = 0.8,weight = 2, bringToFront = TRUE),
        layerId = ~name,
        label = labels, 
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
      addLegend(position = "bottomleft", pal = pal, 
                values = ~cases, title = paste0("Number of cases<br> on ", input$Date), opacity = 0.8) %>%
      clearPopups()
  })
  
  output$timeseries <- renderPlotly({
    combined_df %>%
      st_set_geometry(NULL) %>%
      group_by(date) %>%
      summarise(total_case=sum(na.omit(cases))) %>%
      ungroup() %>%
      plot_ly(x = ~date, y=~total_case, type='scatter', mode = 'lines+markers',
              text = ~date,
              hovertemplate = paste(
                "<b>%{text}</b><br><br>",
                "%{yaxis.title.text}: %{y:.0f}<br>",
                "<extra></extra>"),
              marker = list(size = 12,color="red"),
              line = list(shape = "linear", dash = "dot", width = 3, color= "red")) %>%
      layout(xaxis = list(title = 'date',showgrid = FALSE, autotick = F),
             yaxis = list(title = 'Total Cases',showgrid = FALSE))
  })
  
  
  
  
  
  id <-  eventReactive(input$map_shape_click, { 
    input$map_shape_click$id
  })
  
  output$specific_nbg <- renderPlotly({
    
    combined_df %>%
      filter(name==id()) %>%
      st_set_geometry(NULL) %>%
      plot_ly(x = ~date, y=~cases, type='scatter', mode = 'lines+markers',
              marker = list(size = 12,color="red"),
              line = list(shape = "linear", dash = "dot", width = 3, color= "red")) %>%
      layout(xaxis = list(title = 'date',showgrid = FALSE, autotick = F),
             yaxis = list(title = 'Total Cases',showgrid = FALSE))
  })
  
  
  }

shinyApp(ui = ui, server = server)

