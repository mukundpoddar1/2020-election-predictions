#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)

#subset data to just United States
dataset <- read.csv("merged_final_dataset.csv")

#create list of countries for drop-down selection
variable_choices <- setNames(names(dataset),names(dataset))

ui <- fluidPage(
        #second tab for scatter plot
            # Application title
            titlePanel("EDA"),
            
            # first row: interactive elements
            fluidRow(
                # first column: explanatory text
                column(3,
                       p("explore the relationship between 2 variables in the election dataset"),
                       br()
                ), # end of first column
                
                # 2nd column: dropdown menu to select variable 1
                column(3,
                       selectInput(inputId = "variable_1", label = "Choose a variable:",
                                   choices = variable_choices)
                ), # end of second column 
                # 3nd column: dropdown menu to select variable 2
                column(3,
                       selectInput(inputId = "variable_2", label = "Choose a variable:",
                                   choices = variable_choices)
                ), # end of third column
                
            ), # end of fluidRow
            
            # plot output and text output
            fluidRow(
                column(9,
                       plotOutput("scatterPlot")
                ), # end of column 1
            ) #end of fluidRow
) #end of fluidPage

server <- function(input, output) {
    #scatterplot plot for year vs life expectancy for the United States and another selected country
    output$scatterPlot <- renderPlot({
        # make a scatterplot for life expectancy against fertility rate, filtering for the year 2000 and coloring by continent
        ggplot(dataset, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point(aes(colour = stname.x))
    }) # end of renderPlot
}

# Run the application 
shinyApp(ui = ui, server = server)