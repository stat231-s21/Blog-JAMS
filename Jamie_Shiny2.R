# importing libraries
library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)
library(ggrepel)


# importing data
scatter_data <- read_csv("Data Wrangling/school-enrollment-and-water-access.csv")

# defining choice values and labels for user inputs
country_choices <- unique(scatter_data$country)

# for scatter plot
scatter_school_type_values <- c("primary","secondary")
scatter_school_type_names <- c("Primary Schools", "Secondary Schools")
names(scatter_school_type_values) <- scatter_school_type_names 

############
#    ui    #
############
ui <- navbarPage(
   
   title = "Effect of Basic Water Access in Schools on Enrollment Rates",
   
   theme = shinytheme("flatly"), 
   
# SCATTERPLOT (incorporating enrollment rates data)

      sidebarLayout(
      
      sidebarPanel(
         # choose school type
         radioButtons(inputId = "stat_type_scatter", 
                      label = "Filter by School Type:",
                      choices = scatter_school_type_values, 
                      selected = "primary")
      ,
      selectizeInput(inputId = "country_name"
                     , label = "Identify country(s) in the scatterplot:"
                     , choices = country_choices
                     , selected = NULL
                     , multiple = TRUE)
      ),
      mainPanel(
         plotOutput(outputId = "scatter")
      )
   )
)



############
# server   #
############

server <- function(input, output) {
   
   
   # SCATTERPLOT
   
   data_for_scatter_reactive <- reactive({
      data_for_scatter <- scatter_data %>%
         filter(schoolType == input$stat_type_scatter) 
   })
  
    output$scatter <- renderPlot({
         ggplot(data_for_scatter_reactive(), aes_string(x="coverage/100", y="grossSchoolEnrollmentRatio/100")) +
         geom_point(color = "#2c7fb8") +
         geom_smooth(method = "lm") +
         labs(x = "\nPercent of Schools in Country with Basic Water Supply", y = "School Enrollment Rates (Gross Ratios)\n"
              , title = "School Enrollment Rates (Gross Ratios) vs. Percent of Schools with Basic Water Supply\n") +
         geom_label_repel(data = filter(data_for_scatter_reactive(), country %in% input$country_name), aes(label = country), show.legend = FALSE)  + 
         theme(text = element_text(size=13.5), plot.title = element_text(face = "bold")) +
         scale_x_continuous(labels = scales::percent) + 
         scale_y_continuous(labels = scales::percent)  
 
}
)
}

####################
# call to shinyApp #
#################### 
shinyApp(ui = ui, server = server)
