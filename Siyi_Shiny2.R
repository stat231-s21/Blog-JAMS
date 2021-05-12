library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(plotly)

###############
# import data #
###############

water_ur <- read.csv("Data Wrangling/water_urban_rural.csv")

water_w <- read.csv("Data Wrangling/water_world.csv")

############
#    ui    #
############

ui <- navbarPage(
  title = "Water Service Coverage and Urban-Rural Inequality",
  fluid = TRUE,
  tabPanel(title = "World",
           fluidRow(
             column(6, style = 'padding-left:70px; padding-top: 35px; padding-bottom: 0px; padding-right: 0px;',
                    h4("World Water Service Coverage by Residence Type 2000-2017")
             ),
             column(
               6,
               style = 'padding-left:80px; padding-top: 0px; padding-bottom: 0px; padding-right: 0px;',
               sliderInput(
                 "year2",
                 "Year",
                 min = min(water_ur$year),
                 max = max(water_ur$year),
                 value = min(water_ur$year),
                 animate = TRUE,
                 step = 1
               )
             ),
             column(6,
                    plotlyOutput('plot2')
             ),
             column(6,      
                    plotlyOutput('plot3'))
           )
  ),
  
  tabPanel(title = "SDG Region",
           fluidRow(
             column(
               4,
               align = "left",
               style = 'padding-left:50px; padding-top: 10px; padding-bottom: 0px; padding-right: 0px;',
               radioButtons(
                 inputId = "type1"
                 ,
                 label = "Choose water service type:"
                 ,
                 choices = c("Drinking water", "Sanitation")
                 ,
                 selected = "Drinking water"
               )
             ),
             column(
               3,
               align = "left",
               style = 'padding-left:0px; padding-top: 10px; padding-bottom: 0px; padding-right: 10px;',
               radioButtons(
                 inputId = "level1"
                 ,
                 label = "Choose water service level:"
                 ,
                 choices = c("Safely managed service", "At least basic")
                 ,
                 selected = "Safely managed service"
               )
             ),
             column(
               5,
               align = "left",
               style = 'padding-left:40px; padding-top: 0px; padding-bottom: 0px; padding-right: 0px;',
               sliderInput(
                 "year1",
                 "Year",
                 min = min(water_ur$year),
                 max = max(water_ur$year),
                 value = min(water_ur$year),
                 animate = TRUE,
                 step = 1
               )
             )
           ),
           plotlyOutput('plot1')
           # )
  )
)

############
# server   #
############
server <- function(input, output, session) {
  #TAB 1
  yearData1 <- reactive({
    water_by_year1 <- water_ur %>%
      filter(year == input$year1) %>%
      filter(service_type == toString(input$type1)) %>%
      filter(service_level == toString(input$level1))
  })
  
  output$plot1 <- renderPlotly({
    df1 <- yearData1()
    df1 %>%
      plot_ly() %>%
      add_trace(
        x = ~ urban,
        y = ~ region,
        name = "Urban",
        type = 'scatter',
        mode = "markers",
        marker = list(
          color = "#3366cc",
          size = 25,
          opacity = 0.4
        ),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Region:',
          region,
          '</br> Urban Water Service Coverage(%):',
          urban
        )
      ) %>%
      add_trace(
        x = ~ rural,
        y = ~ region,
        name = "Rural",
        type = 'scatter',
        mode = "markers",
        marker = list(
          color = "#109618",
          size = 25,
          opacity = 0.4
        ),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Region:',
          region,
          '</br> Rural Water Service Coverage(%):',
          rural
        )
      ) %>%
      add_trace(
        x = ~ total,
        y = ~ region,
        name = "Total",
        type = 'scatter',
        mode = "markers",
        marker = list(
          color = "#ff9900",
          size = 25,
          opacity = 0.4
        ),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Region:',
          region,
          '</br> Total Water Service Coverage(%):',
          total
        )
      ) %>%
      layout(
        title = "Region Water Service Coverage by Residence Type 2000-2017",
        xaxis = list(title = "Water Service coverage (%)", range = c(0, 105)),
        yaxis = list(title = "Region"),
        margin = list(l = 200),
        legend = list(title = list(text = '<b> Residence type </b>'))
      )
  })
  
  #TAB 2
  yearData2 <- reactive({
    water_by_year2 <- water_w %>%
      filter(year == input$year2) %>%
      filter(service_type == "Drinking water")
  })
  
  yearData3 <- reactive({
    water_by_year3 <- water_w %>%
      filter(year == input$year2) %>%
      filter(service_type == "Sanitation")
  })
  
  output$plot2 <- renderPlotly({
    p1 <- yearData2() %>%
      plot_ly() %>%
      add_trace(
        x = ~ residence_type,
        y = ~ safely_managed_service,
        name = "Safely Managed Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#003300", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Safely Managed Service Coverage(%):',
          safely_managed_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ basic_service,
        name = "Basic Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#009933", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Basic Service Coverage(%):',
          basic_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ limited_service,
        name = "Limited Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FFFF33", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Limited Service Coverage(%):',
          limited_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ unimproved,
        name = "Unimproved Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FFCC00", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Unimproved Service Coverage(%):',
          basic_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ surface_water,
        name = "Surface Water",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FF9900", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Surface Water Coverage(%):',
          surface_water
        )
      ) %>%
      layout(
        xaxis = list(title = "Residence Type"),
        yaxis = list(title = "Coverage by Drinking Water (%)", range = c(0, 105)),
        barmode = 'stack',
        legend = list(y = -0.4, orientation = 'h', title = list(text = '<b> Drinking Water </b>'))
      )
    p1
  })
  
  output$plot3 <- renderPlotly({
    p2 <- yearData3() %>%
      plot_ly() %>%
      add_trace(
        x = ~ residence_type,
        y = ~ safely_managed_service,
        name = "Safely Managed Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#0066CC", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Safely Managed Service Coverage(%):',
          safely_managed_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ basic_service,
        name = "Basic Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#3399FF", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Basic Service Coverage(%):',
          basic_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ limited_service,
        name = "Limited Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FFFF33", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Limited Service Coverage(%):',
          limited_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ unimproved,
        name = "Unimproved Service",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FFCC00", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Unimproved Service Coverage(%):',
          basic_service
        )
      ) %>%
      add_trace(
        x = ~ residence_type,
        y = ~ open_defecation,
        name = "Open Defecation",
        type = 'bar',
        mode = "markers",
        marker = list(color = "#FF9900", opacity = 0.4),
        hoverinfo = 'text',
        fill = ~ '',
        text = ~ paste(
          '</br> Residence Type:',
          residence_type,
          '</br> Open Defecation Coverage(%):',
          open_defecation
        )
      ) %>%
      layout(
        xaxis = list(title = "Residence Type"),
        yaxis = list(title = "Coverage by Sanitation (%)", range = c(0, 105)),
        barmode = 'stack',
        legend = list(y = -0.4, orientation = 'h', title = list(text = '<b> Sanitation </b>'))
      )
    p2
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
