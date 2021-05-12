# importing libraries
library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)

# importing data
school_water_data <- read_csv("Data Wrangling/school-water-access-regional.csv")

# defining choice values and labels for user inputs
region_choices <- c("Australia and New Zealand", "Central and Southern Asia", 
                                 "Eastern and South-Eastern Asia", "Europe and Northern America", 
                                 "Latin America and the Caribbean", "Northern Africa and Western Asia",
                                 "Oceania", "Sub-Saharan Africa")

# for bar chart and line graph                            
school_type_values <- c("primary","secondary","total")
school_type_names <- c("Primary Schools", "Secondary Schools", "All Schools")
names(school_type_values) <- school_type_names 

############
#    ui    #
############
ui <- navbarPage(
   
   
   title="Overview of Drinking Water Levels in Schools Worldwide",
   
   theme = shinytheme("flatly"), 
   

   # TAB 1: STACKED BAR CHART
   
   tabPanel(
      title = "Differences by SDG Region",
      sidebarLayout(
         sidebarPanel(
            # choose school type
            radioButtons(inputId = "stat_type_stacked_bar_chart", 
                         label = "School Type:",
                         choices = school_type_values, 
                         selected = "primary")
         ),
         mainPanel(
            plotOutput(outputId = "bar")
         )
      )
   ),
   
   # TAB 2: LINE GRAPH  
   
   tabPanel(
      title = "Basic Water Access in Schools Over Time",
   sidebarLayout(
      sidebarPanel(
        # choose region
        selectInput(inputId = "sdg_regions", 
                    label = "SDG Region:",
                    choices = region_choices, 
                    selected = region_choices,
                    multiple = TRUE),
        # choose school type
        radioButtons(inputId = "stat_type_line_graph", 
                    label = "Filter by School Type:",
                    choices = school_type_values, 
                    selected = "total")
      ),
      mainPanel(
         plotOutput("lineGraph")
      )
   )
   )
)



############
# server   #
############

server <- function(input, output) {
   
   # TAB 1: STACKED BAR CHART
   
   output$bar <- renderPlot({
      
      stacked_bars_data <- school_water_data %>%
         select(region, schoolType, year, coverage, serviceLevel) %>%
         filter(schoolType == input$stat_type_stacked_bar_chart) %>%
         filter(year == 2018)
      
      ggplot(data = stacked_bars_data, aes(x = region, y = coverage, 
                                           fill = factor(serviceLevel, c("Insufficient data", "No service", "Limited service", "Basic service")))) + 
         geom_bar(position = "fill", stat = "identity") +
         labs(
            title = paste("Distribution of Drinking Water Levels in Schools by SDG Region, 2018\n"),
            x = "\nSDG Region",
            y = "Percent",
            fill = "Drinking Water Level") +
         theme(text = element_text(size=13.5), axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
         scale_y_continuous(labels = scales::percent) +
         scale_fill_manual(values = c("grey", "orangered3","steelblue1", "steelblue4"))
   })
   
   # TAB 2: LINE GRAPH
   
   data_for_line_reactive <- reactive({
      data_for_line <- school_water_data %>%
         select(region, schoolType, year, coverage, serviceLevel) %>%
         filter(region %in% input$sdg_regions) %>% 
         filter(schoolType == input$stat_type_line_graph) %>%
         filter(serviceLevel == "Basic service") %>%
         filter(year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | 
                   year == 2016 | year == 2017 | year == 2018 | year == 2019)
   })
   
   output$lineGraph <- renderPlot({
      ggplot(data = data_for_line_reactive(), aes(x = year, y = coverage/100, color = region)) +
         geom_line(size = 1.2) + 
         labs(x = "Year", y = "Percent"
              , title = paste("Percent of Schools with a Basic Water Supply in SDG Regions, 2011-2019\n")) +
         scale_x_continuous(breaks=seq(2011, 2019, 1)) +
         scale_y_continuous(labels = percent, limits=c(.3,1)) + 
         theme(text = element_text(size=13.5), plot.title = element_text(face = "bold")) + 
         guides(col=guide_legend("Region"))
   })
}
   
   
   

####################
# call to shinyApp #
#################### 
shinyApp(ui = ui, server = server)
