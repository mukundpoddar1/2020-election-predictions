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
library(corrplot)

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
            ), #end of fluidRow
            
            fluidRow(
                # 1st column: input up to 10 predictors
                column(7,
                       selectizeInput(inputId = "corr", label = "Select up to 10 variables for correlation plot",
                                      choices = variable_choices,options=list(maxItems=10),
                       selected = "age_0_to_19_years_tot_female_2012.y")
                ), # end of first column
            ), # end of fluidRow
            
            fluidRow(
                column(9,
                       plotOutput("Corr_matrix")
                ),
            ),
            fluidRow(
                # another column, select variable to see distribution
                column(3,
                       selectInput(inputId = "hist_selection", label = "Choose a variable:",
                                   choices = variable_choices,
                                   selected = "age_0_to_19_years_tot_female_2012.y")
                ) #end of column
            ), # end of fluidRow 
            fluidRow(
                column(9,
                       plotOutput("hist")
                )
            )
) #end of fluidPage

server <- function(input, output) {
    #scatterplot plot for year vs life expectancy for the United States and another selected country
    output$scatterPlot <- renderPlot({
        # make a scatterplot for life expectancy against fertility rate, filtering for the year 2000 and coloring by continent
        ggplot(dataset, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point(aes(colour = stname.x))
    }) # end of renderPlot
    
    output$Corr_matrix <- renderPlot({
        corrplot(cor(select(dataset,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number")
    })
    
    output$hist <-renderPlot({
        ggplot(dataset, aes_string(input$hist_selection))+geom_histogram()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)