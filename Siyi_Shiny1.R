library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(plotly)

###############
# import data #
###############

water_income <- read.csv("Data Wrangling/water_income.csv")
water_income <- water_income %>%
  mutate(loggdp = log(gdp))
water_income$region <- as.factor(water_income$region)

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors to represent the choices given to the user

colors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")

service_values <- c("safe", "basic")
service_names <- c("People using safely managed drinking water services (% of population)", "People using at least basic drinking water services (% of population)")
names(service_values) <- service_names

############
#    ui    #
############

ui <- fluidPage(
  headerPanel('Water service level vs Income'),
  fluidRow(
    
    column(4, align="left", offset = 0.5, style='padding-left:50px; padding-top: 10px; padding-bottom: 0px; padding-right: 10px;',
           radioButtons(inputId = "level"
                        , label = "Choose water service level:"
                        , choices = service_values
                        , selected = "safe")
    ),
    column(6, align="left", style='padding-left:30px; padding-top: 10px; padding-bottom: 0px; padding-right: 10px;',
           sliderInput("year", "Year",
                       min = min(water_income$year), max = max(water_income$year),
                       value = min(water_income$year), animate = TRUE, step = 1)
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
    water_by_year <- water_income %>%
      filter(year == input$year) %>%
      arrange(region)
  })
  
  output$plot <- renderPlotly({
    df <- yearData()
    df$yy <- df[[input$level]]
    df %>% 
      highlight_key(~ region) %>%
      plot_ly(x = ~ loggdp, y = ~ yy, color = ~ region, colors = colors,
              type = 'scatter', mode = 'markers', size = ~ gini, sizes = c(5, 30),
              marker = list(symbol = 'circle', opacity = 0.4, sizemode = 'diameter'),
              hoverinfo = 'text',
              fill = ~'',
              text = ~ paste('</br> Country:', country,
                             '</br> Water Service Coverage(%):', yy,
                             '</br> GDP per capita($):', gdp,
                             '</br> Gini:', gini)) %>%
      layout(title = "World Water Service Coverage by GDP per capita",
             yaxis = list(title = "Water Service Coverage (%)", range = c(0, 115)),
             xaxis = list(title = "GDP per capita (log) ($)", range = c(min(water_income$loggdp)-0.2, max(water_income$loggdp)+0.1)),
             legend = list(orientation = 'h', y = -0.2, title=list(text='<b> Region </b>'))) %>%
      highlight(on = "plotly_hover", off = "plotly_doubleclick", opacityDim = 0.4)
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
