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

colors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")

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
    column(4, offset = 7,
           sliderInput("year", "Year",
                       min = min(water_ur$year), max = max(water_ur$year),
                       value = min(water_ur$year), animate = TRUE, step = 1)
    ),
    column(6, offset = 1,
           radioButtons(inputId = "type"
                        , label = "Choose water service type:"
                        , choices = c("Sanitation","Drinking water")
                        , selected = "Sanitation"),
           radioButtons(inputId = "level"
                        , label = "Choose water service level:"
                        , choices = c("Basic service","Safely managed service")
                        , selected = "Basic service")
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
      filter(service_level == toString(input$level)) %>%
      arrange(residence_type)
  })
  
  output$plot <- renderPlotly({
    df <- yearData()
   # df$ylevel <- df[[input$level]]
    df %>% 
      highlight_key(~ region) %>%
      plot_ly(
        x = ~ residence_type, y = ~ coverage, color = ~ region, colors = colors,
        type = 'scatter', mode = 'markers', sizes = c(5, 30),
        marker = list(symbol = 'circle', opacity = 0.6, sizemode = 'diameter')
      #   df, color = I("gray80")) %>%
      # add_markers(x = ~ `Australia and New Zealand`, y = ~ residence_type, name = "Australia and New Zealand", 
      #         color = "pink") %>% 
      # add_trace(df, x = ~ `Central and Southern Asia`, y = ~ residence_type, name = "Central and Southern Asia", type = 'scatter',
      #           mode = "markers", marker = list(color = "blue")) %>%
      # layout(title = "Gender earnings disparity",
      # xaxis = list(title = "Annual Salary (in thousands)"),
      # margin = list(l = 100)
        ) %>% 
      highlight(on = "plotly_hover", off = "plotly_doubleclick", opacityDim = 0.5) %>%
      layout(yaxis = list(title = input$level),
             xaxis = xform,
             legend = list(orientation = 'v', y = -0.2, title=list(text='<b> Region </b>')))
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
