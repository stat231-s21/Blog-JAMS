library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(plotly)

###############
# import data #
###############

#https://plotly.com/r/dot-plots/#dot-and-dumbbell-plots

water_ur <- read.csv("Data Wrangling/water_urban_rural.csv")

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors to represent the choices given to the user

xform <- list(categoryorder = "array",
              categoryarray = c("urban", 
                                "rural", 
                                "total"))

############
#    ui    #
############

ui <- fluidPage(
  headerPanel('Water service level vs urban/rural disparity'),
  fluidRow(
    column(4, align="left", style='padding-left:50px; padding-top: 10px; padding-bottom: 0px; padding-right: 0px;',
           radioButtons(inputId = "type"
                        , label = "Choose water service type:"
                        , choices = c("Drinking water", "Sanitation")
                        , selected = "Drinking water")
    ),
    column(3, align="left", style='padding-left:0px; padding-top: 10px; padding-bottom: 0px; padding-right: 10px;',
           radioButtons(inputId = "level"
                        , label = "Choose water service level:"
                        , choices = c("Safely managed service", "At least basic")
                        , selected = "Safely managed service")
    ),
    column(5, align="left", style='padding-left:40px; padding-top: 0px; padding-bottom: 0px; padding-right: 0px;',
           sliderInput("year", "Year",
                       min = min(water_ur$year), max = max(water_ur$year),
                       value = min(water_ur$year), animate = TRUE, step = 1)
    )
  ),
  plotlyOutput('plot'),
  hr(),
)

############
# server   #
############
server <- function(input, output, session){
  yearData <- reactive({
    water_by_year <- water_ur %>%
      filter(year == input$year) %>%
      filter(service_type == toString(input$type)) %>%
      filter(service_level == toString(input$level)) 
  })
  
  output$plot <- renderPlotly({
    df <- yearData()
    df %>% 
      plot_ly() %>% 
      add_trace(x = ~ urban, y = ~ region, name = "Urban", type = 'scatter',
                mode = "markers", marker = list(color = "#3366cc", size = 25, opacity = 0.4),
                hoverinfo = 'text',
                fill = ~'',
                text = ~ paste('</br> Region:', region,
                               '</br> Urban Water Service Coverage(%):', urban)) %>% 
      add_trace(x = ~ rural, y = ~region, name = "Rural",type = 'scatter',
                mode = "markers", marker = list(color = "#109618", size = 25, opacity = 0.4),
                hoverinfo = 'text',
                fill = ~'',
                text = ~ paste('</br> Region:', region,
                               '</br> Rural Water Service Coverage(%):', rural)) %>% 
      add_trace(x = ~ total, y = ~region, name = "Total",type = 'scatter',
                mode = "markers", marker = list(color = "#ff9900", size = 25, opacity = 0.4),
                hoverinfo = 'text',
                fill = ~'',
                text = ~ paste('</br> Region:', region,
                               '</br> Total Water Service Coverage(%):', total)) %>% 
      layout(title = "World Water Service coverage by Residence Type",
             xaxis = list(title = "Water Service coverage (%)", range = c(0, 105)),
             yaxis = list(title = "Region"),
             margin = list(l = 200))
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

