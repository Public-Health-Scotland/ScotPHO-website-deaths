#Code to create chart of annual changes in life expectancy and healthy life expectancy by in weeks and years.

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)

#Preparing data - not needed unless new data coming through
# library(tidyr)
library(readr)

#annual_changes_le_hle <- read_csv("shiny_app/Data/OLD_annual_changes_in_le_and_hle_for_males_in_years_and_weeks.csv") #reading data for app

annual_changes_le_hle <- read_csv("shiny_app/Data/annual_changes_in_le_and_hle_for_males_and_females_in_years_and_weeks.csv")

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%", #Filters on top of page
                    h4("Chart 2. Annual changes in Life Expectancy and Healthy Life Expectancy in Scotland"),
                    div(style = "width: 40%; float: left;",
                        selectInput("measure", label = "Select a measure type",
                                    choices = c("Life expectancy", "Healthy life expectancy"), selected = "Life expectancy")
                    ),
                    div(style = "width: 40%; float: left;",
                        selectInput("difference", label = "Select difference from previous year", 
                                    choices = c("Years", "Weeks"), selected = "Years")
                    ),
                   
                  div(style = "width: 20%; float: left;",
                      selectInput("sex", label = "Select sex",
                                           choices = c("Male", "Female"), selected = "Male"))
                ),
                    
                
                div(style= "width:100%; float: left;", #Main panel
                    plotlyOutput("chart", width = "100%", height = "350px"),
                    p(div(style = "width: 25%; float: left;", #Footer
                          HTML("Source: <a href='https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy' target='_blank'>NRS</a>")),
                      div(style = "width: 25%; float: left;",
                          downloadLink('download_data', 'Download data'))
                          
                    )
                )
)

############################.
## Server ----
############################.
server <- function(input, output) {
  
  # Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'annual_changes_in_le_and_hle_data.csv', content = function(file) { 
      write.csv(annual_changes_le_hle, file, row.names=FALSE) })
  
  ############################.
  #Visualization
  output$chart <- renderPlotly({
    
      plot_ly() %>%
        layout(yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
               xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
               font = list(family = 'Arial, sans-serif')) %>% 
        config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
      
      
      #Data for Difference line
      data_difference <- annual_changes_le_hle  %>% 
      filter(Measure == input$measure & Difference == input$difference 
            & Sex == input$sex
             )
      
      #y axis title #y axis titles not working
      yaxistitle <- ifelse(input$measure == "Life Expectancy", "Life Expectancy", "Healthy Life Expectancy")
      
      plot <- plot_ly(data_difference, x=~Year, y = ~Value, 
                      type = "scatter", mode = 'lines', line = list(color = '#08519c'),
#                      name = unique(data_difference$Measure), 
                      width = 650, height = 350) %>% 
#        add_lines(data = data_scot, y = ~value, mode = 'lines', 
#                  name = "Scotland", line = list(color = '#000000')) %>%
        #Layout
        layout(annotations = list(), #It needs this because of a buggy behaviour
               yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
               xaxis = list(title = "Year",  fixedrange=TRUE, tickangle = 270),  
               font = list(family = 'Arial, sans-serif'), #font
               margin = list(pad = 4, t = 50), #margin-paddings
               hovermode = 'false'  # to get hover compare mode as default
               ) %>% 
        config(displayModeBar= T, displaylogo = F, editable =F) # taking out plotly logo and collaborate button
  
  }) 
  
} # end of server part

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END