rm(list = ls())
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)
library(viridis)
library(shiny)
library(plotly)
library(htmlwidgets)
##############
##############

load(file = 'data/case_data.rda')

##############
##############

empty_plot <- function(title = NULL) {
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title,
                        yref = "paper",
                        y = 0.5))
  return(p)
}
empty_plot("This community does not have data")


##############
##############
table_html <-
  '<table style="width:100%%">
<tr>
<th><span style="float:left"> %s </span><br/></th>
</tr>
<tr>
</tr>
<tr>
<td>Reported cases</td>
<td><span style="float:right"> %i </span><br/></td>
</tr>
<tr>
<td>Population</td>
<td><span style="float:right"> %i </span><br/></td>
</tr>
<tr>
<td>Cases per thousand</td>
<td><span style="float:right;padding:0px 0px 0px 10px"> %.2f </span><br/></td>
</tr>
</table>'

LA_county_link <-
  "http://www.publichealth.lacounty.gov/media/Coronavirus/"
latest_update_link <-
  "http://www.publichealth.lacounty.gov/phcommon/public/media/mediapubhpdetail.cfm?prid=2287"


about <-
  '<!DOCTYPE html>
<b>ABOUT:</b>
<br></br>
<body style="width:70%%"><P>
Cases of the novel coronavirus COVID-19 in Los Angeles County. Click on a community on the map to display number of cases there over time.<P>
Data on reported cases are taken from the <a href="http://www.publichealth.lacounty.gov/media/Coronavirus/">Los Angeles Department of Public Health</a>.
Many new cases are reported daily, absence of reported cases in an area does not mean there are no cases in that area. 
Reported cases are subject to change based on further investigations. 
Differences between areas in number of cases reported may reflect differences in access to testing rather than actual differences in coronavirus infection. 
"NA" indicates areas where no data area available. <P>
Population data and boundaries for each community are taken from the LA Countywide Statistical Areas dataset available online: <a href="https://egis3.lacounty.gov/dataportal/2017/11/02/board-approved-statistical-areas-communities-final-draft/">https://egis3.lacounty.gov/dataportal/2017/11/02/board-approved-statistical-areas-communities-final-draft/</a>.
Data and code to reproduce figures are available at <a href="https://github.com/akleinhesselink/LA_covid">https://github.com/akleinhesselink/LA_covid</a>. <P>
Map and figures by Andy Kleinhesselink and Jane Li.
<P>
From Los Angeles Department of Public Health: \"Always check with trusted sources for the latest accurate information about novel coronavirus:
<ul><li>Los Angeles County Department of Public Health <a href="http://publichealth.lacounty.gov/media/Coronavirus/"> http://publichealth.lacounty.gov/media/Coronavirus/</a>
<li>California Department of Public Health <a   href="https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx">https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx </a>
<li>Centers for Disease Control and Prevention (CDC) <a href="https://www.cdc.gov/coronavirus/2019-ncov/index.html">https://www.cdc.gov/coronavirus/2019-ncov/index.html</a>
Spanish<a href=" https://www.cdc.gov/coronavirus/2019-ncov/index-sp.html">  https://www.cdc.gov/coronavirus/2019-ncov/index-sp.html </a>
<li>World Health Organization <a href="https://www.who.int/health-topics/coronavirus">https://www.who.int/health-topics/coronavirus</a>
<li>LA County residents can also call 2-1-1 \"</ul>
<P><br>
<center>#####</center><br>
</td>
</body>
</html>'


latest_update <- max(basic_stats$date)
first_date <- min(basic_stats$date)

main_title <- "Reported COVID-19 Cases in Los Angeles County"
subtitle <- paste("Updated on", latest_update)
county_title <- "Los Angeles countywide totals"
community_title <- "Selected community (click on map)"
map_legend_title <- "Cases per thousand"

x_title <- "Date"
y_title <- "Cases"

x_range <-
  c(as.character(first_date - 1), as.character(latest_update + 1))

# library(classInt)
# my_breaks <- classInt::classIntervals(map_data$`Cases per thousand`, style = 'jenks')$brks
# my_breaks <- c( 0, 0.01, round( my_breaks, 1)[-1] )

rc2 <-
  colorRampPalette(colors = c("white", "red"), space = "Lab")(180)
pal_cases_per_thousand <-
  colorNumeric(
    palette = rc2,
    domain = map_data$`Cases per thousand`,
    na.color = 'lightgrey'
  )

# pal_cases <-colorNumeric(palette = rc2, domain = map_data$Cases, na.color = 'lightgrey')
# rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab")(length(my_breaks))
# pal_cases_per_thousand <-colorBin(palette=rc2, domain = map_data$`Cases per thousand`, bins = my_breaks,  na.color = 'lightgrey')
# pal_cases <-colorNumeric(palette = rc2, domain = map_data$Cases, na.color = 'lightgrey')

## --------------------------------------- #

ui <- fluidPage(
  tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel(h2(main_title)),
  titlePanel(h4(subtitle)),
  fluidRow(column(
    width = 5, wellPanel(
      selectInput("variable", "Select Statistic:", 
                  choices = unique( basic_stats$variable ), 
                  selected = "Total cases", multiple = F),
      h4(county_title),
      plotlyOutput("timeseries", height =
                     "255px"),
      h4(sprintf(community_title)),
      plotlyOutput("specific_nbg", height =
                     "255px")
    )
  ),
  column(width = 7, wellPanel(
    leafletOutput("map",  width = "100%", height = "600px")
  ))),
  fluidRow(column(width = 12, wellPanel(
    htmlOutput("about_text")
  )))
)

##############
##############

server <- function(input, output) {
  output$about_text <- renderText({
    about
  })
  
  output$map <- renderLeaflet({
    labels <- sprintf(
      table_html,
      map_data$label,
      as.integer(map_data$`Total cases`),
      as.integer(map_data$Population),
      as.numeric(map_data$`Cases per thousand`)
    ) %>% 
      lapply(htmltools::HTML)
    
    map_data %>%
      leaflet() %>%
      setView(-118.25, 34.2, zoom = 9) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "black",
        weight = 1,
        dashArray = 3,
        fillColor = ~ pal_cases_per_thousand(`Cases per thousand`),
        fillOpacity = 0.5,
        highlight = highlightOptions(
          fillColor = "Cyan",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        group = "Cases per thousand",
        layerId = ~ label,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "5px 10px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal_cases_per_thousand,
        na.label = "No Data",
        values = map_data$`Cases per thousand`,
        title = map_legend_title,
        opacity = 0.8
      )
  })
  
  # LA County Total
  output$timeseries <- renderPlotly({
  
  if( input$variable == 'Total cases' ){ 
    la_county %>%
      filter(variable == input$variable ) %>%
      plot_ly(
        x = ~ date,
        y = ~ value,
        type = 'scatter',
        mode = 'lines+markers',
        text = ~ date,
        hovertemplate = paste(
          "<b>%{text}</b><br><br>",
          "%{yaxis.title.text}: %{y:.0f}<br>",
          "<extra></extra>"),
        marker = list(size = 8, color = "red"),
        line = list(
          shape = "linear",
          dash = "dot",
          width = 3,
          color = "red"
        )
      ) %>%
      layout(
        xaxis = list(
          title = x_title,
          showgrid = FALSE,
          autotick = T,
          range = x_range
        ),
        yaxis = list(title = input$variable, showgrid = FALSE)
      ) %>%
      config(displayModeBar = F)
  }else if(  input$variable == 'New cases' ){ 
    la_county %>%
      filter(variable == input$variable ) %>%
      plot_ly(
        x = ~ date,
        y = ~ value,
        type = 'bar',
        text = ~ date,
        hovertemplate = paste(
          "<b>%{text}</b><br><br>",
          "%{yaxis.title.text}: %{y:.0f}<br>",
          "<extra></extra>"),
        marker = list(size = 8, color = "red")) %>%
        layout(
          xaxis = list(
            title = x_title,
            showgrid = FALSE,
            autotick = T,
            range = x_range
          ),
          yaxis = list(title = input$variable, showgrid = FALSE)
        ) %>%
        config(displayModeBar = F)
  }
  })
  
  id <-  eventReactive(input$map_shape_click, {
    input$map_shape_click$id
  })
  
  # Community Plot
  output$specific_nbg <- renderPlotly({
    selected_location <-
      basic_stats %>%
      filter(label == id()) %>%
      filter(variable == 'Total cases')
    
    any_cases = sum(selected_location$value, na.rm = T)

    if( !is.na(any_cases) & any_cases > 0 ){
      
      if( input$variable == 'Total cases' ){ 
        
      basic_stats %>%
        filter(label == id()) %>%
        filter(variable == input$variable) %>%
        plot_ly(
          x = ~ date,
          y = ~ value,
          type = 'scatter',
          mode = 'lines+markers',
          text = ~ date,
          hovertemplate = paste(
            "<b>%{text}</b><br><br>",
            "%{yaxis.title.text}: %{y:.0f}<br>",
            "<extra></extra>"
          ),
          marker = list(size = 8, color = "red"),
          line = list(
            shape = "linear",
            dash = "dot",
            width = 3,
            color = "red"
          )
        ) %>%
        layout(
          title = list(
            text = id(),
            x = 0.1,
            y = 0.9,
            xref = "paper",
            yref = 'paper'
          ),
          xaxis = list(
            title = x_title,
            showgrid = FALSE,
            autotick = T,
            range = x_range
          ),
          yaxis = list(
            title = input$variable,
            showgrid = FALSE,
            rangemode = "tozero"
          )
        ) %>%
        config(displayModeBar = F)
      }else if(  input$variable == 'New cases' ){ 
        basic_stats %>%
          filter(label == id()) %>%
          filter(variable == input$variable) %>%
          plot_ly(
            x = ~ date,
            y = ~ value,
            type = 'bar',
            text = ~ date,
            hovertemplate = paste(
              "<b>%{text}</b><br><br>",
              "%{yaxis.title.text}: %{y:.0f}<br>",
              "<extra></extra>"),
            marker = list(size = 8, color = "red")) %>%
          layout(
            title = list(
              text = id(),
              x = 0.1,
              y = 0.9,
              xref = "paper",
              yref = 'paper'
            ),
            xaxis = list(
              title = x_title,
              showgrid = FALSE,
              autotick = T,
              range = x_range
            ),
            yaxis = list(title = input$variable, showgrid = FALSE)
          ) %>%
          config(displayModeBar = F)
      }
    } else{
      empty_plot(sprintf("There are no data for %s", id()))
    }
  })
}

shinyApp(ui = ui, server = server)
